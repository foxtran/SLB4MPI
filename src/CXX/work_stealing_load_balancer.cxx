#include <SLB4MPI/work_stealing_load_balancer.hpp>

#include <algorithm>
#include <array>
#include <cmath>
#include <random>

SLB4MPI::WorkStealingLoadBalancer::WorkStealingLoadBalancer(const MPI_Comm communicator, const int64_t lower_bound, const int64_t upper_bound, const int64_t min_chunk_size, const int64_t max_chunk_size) :
  SLB4MPI::AbstractLoadBalancer::AbstractLoadBalancer(communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size) {
  int64_t n_tasks = (upper_bound - lower_bound + 1) / this->nranks;
  int64_t extra_tasks = (upper_bound - lower_bound + 1) % this->nranks;
  if (n_tasks < min_chunk_size) {
    n_tasks = min_chunk_size;
    extra_tasks = 0;
  }

  if (this->rank < extra_tasks) {
    this->lower_bound = this->rank * (n_tasks + 1) + lower_bound;
    this->upper_bound = (this->rank + 1) * (n_tasks + 1) + lower_bound - 1;
  } else {
    this->lower_bound = this->rank * n_tasks + extra_tasks + lower_bound;
    this->upper_bound = (this->rank + 1) * n_tasks + extra_tasks + lower_bound - 1;
  }

  this->lower_bound = std::min(this->lower_bound, upper_bound + 1);
  this->upper_bound = std::min(this->upper_bound, upper_bound);

  this->counter = this->lower_bound;

#ifdef SLB4MPI_WITH_MPI
  this->actual_rank = this->rank;
  this->done = false;
  this->num_active = this->nranks;

  MPI_Win_create(&(this->num_active), sizeof(int), sizeof(int), MPI_INFO_NULL, this->communicator, &(this->window_num_active));
  MPI_Win_create(&(this->actual_rank), sizeof(int), sizeof(int), MPI_INFO_NULL, this->communicator, &(this->window_actual_rank));
  MPI_Win_create(&(this->bounds), 2*sizeof(int64_t), sizeof(int64_t), MPI_INFO_NULL, this->communicator, &(this->window_bounds));
  MPI_Win_create(&(this->done), sizeof(bool), sizeof(bool), MPI_INFO_NULL, this->communicator, &(this->window_done));

  MPI_Barrier(this->communicator);
#endif
}

SLB4MPI::WorkStealingLoadBalancer::~WorkStealingLoadBalancer() {
#ifdef SLB4MPI_WITH_MPI
  MPI_Win_free(&(this->window_num_active));
  MPI_Win_free(&(this->window_actual_rank));
  MPI_Win_free(&(this->window_bounds));
  MPI_Win_free(&(this->window_done));
#endif
}

bool SLB4MPI::WorkStealingLoadBalancer::get_range(int64_t& lower_bound, int64_t& upper_bound) {

#ifndef SLB4MPI_WITH_MPI

  lower_bound = this->counter;
  upper_bound = std::min(lower_bound + this->max_chunk_size - 1, this->upper_bound);
  this->counter = upper_bound + 1;

  bool to_compute = false;
  if (lower_bound <= this->upper_bound) to_compute = true;

#else
  bool to_compute = false;

  if (this->actual_rank == this->rank) {
    to_compute = true;
    // select range for computing
    MPI_Win_lock(MPI_LOCK_EXCLUSIVE, this->rank, 0, this->window_bounds);
    lower_bound = this->lower_bound;
    upper_bound = std::min(lower_bound + this->max_chunk_size - 1, this->upper_bound);
    this->lower_bound = upper_bound + 1;
    MPI_Win_unlock(this->rank, this->window_bounds);
    // if it the last block, give that information in advance
    if (upper_bound + 1 > this->upper_bound && !this->done) {
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, this->root, 0, this->window_num_active);
      int minus_one = -1;
      MPI_Fetch_and_op(&minus_one, &(this->num_active), MPI_INT, this->root, 0, MPI_SUM, this->window_num_active);
      MPI_Win_unlock(this->root, this->window_num_active);
      this->actual_rank = (this->actual_rank + 1) % this->nranks;
      this->done = true;
    }
    if (lower_bound > upper_bound) to_compute = false;
  }

  // switch to other ranks
  int hop_count = 0, nohop_count = 0;
  while (!to_compute) {
    int compute_rank;
    bool done;
    std::array<int64_t, 2> bounds;
    // get rank which computes lb%actual_rank
    MPI_Win_lock(MPI_LOCK_SHARED, this->actual_rank, MPI_MODE_NOCHECK, this->window_actual_rank);
    MPI_Get(&compute_rank, 1, MPI_INT, this->actual_rank, 0, 1, MPI_INT, this->window_actual_rank);
    MPI_Win_unlock(this->actual_rank, this->window_actual_rank);
    // check that compute_rank computes itself
    if (this->actual_rank == compute_rank) {
      // check that there is something to steal
      MPI_Win_lock(MPI_LOCK_SHARED, this->actual_rank, MPI_MODE_NOCHECK, this->window_done);
      MPI_Get(&done, 1, MPI_CXX_BOOL, this->actual_rank, 0, 1, MPI_CXX_BOOL, this->window_done);
      MPI_Win_unlock(this->actual_rank, this->window_done);
      if (!done) {
        // try to steal min_chunk_size jobs from compute_rank
        MPI_Win_lock(MPI_LOCK_EXCLUSIVE, compute_rank, 0, this->window_bounds);
        MPI_Get(&bounds, 2, MPI_INT64_T, compute_rank, 0, 2, MPI_INT64_T, this->window_bounds);
        MPI_Win_flush(compute_rank, this->window_bounds);
        upper_bound = bounds[1];
        lower_bound = std::max(bounds[0], bounds[1] - this->min_chunk_size + 1);
        bounds[1] = lower_bound - 1;
        if (lower_bound <= upper_bound) to_compute = true;
        if (to_compute) {
          // update upper bound
          MPI_Accumulate(&bounds, 2, MPI_INT64_T, compute_rank, 0, 2, MPI_INT64_T, MPI_REPLACE, this->window_bounds);
        }
        MPI_Win_unlock(compute_rank, this->window_bounds);
        if (to_compute) return to_compute;
      }
    } else {
      // switch to lb%actual_rank of compute_rank
      this->actual_rank = compute_rank;
      if (this->actual_rank == this->rank) return false;
      // but... to avoid infinity cycle, sometimes we will check that there is some jobs
      hop_count = hop_count + 1;
      {
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_real_distribution<> unif(0.0, 1.0);
        float randval = unif(gen);
        // log(nranks) <- how often to check cond (each 1, 2, 3, ..)
        // 1 / log(nranks) <- probability
        if (randval > (1. / std::log(static_cast<float>(this->nranks))) && hop_count < 20) continue;
      }
    }
    // if could not steal job, check how many threads finished job
    MPI_Win_lock(MPI_LOCK_SHARED, this->root, 0, this->window_num_active);
    MPI_Get(&(this->num_active), 1, MPI_INT, this->root, 0, 1, MPI_INT, this->window_num_active);
    MPI_Win_unlock(this->root, this->window_num_active);
    if (this->num_active == 0) return false;
    this->actual_rank = (this->actual_rank + 1) % this->nranks;
    hop_count = 0;
    nohop_count = nohop_count + 1;
    if (nohop_count > this->nranks) return false;
  }
#endif
  return to_compute;
}
