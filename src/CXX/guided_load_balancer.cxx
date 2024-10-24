#include <SLB4MPI/guided_load_balancer.hpp>

#include <algorithm>

SLB4MPI::GuidedLoadBalancer::GuidedLoadBalancer(const MPI_Comm communicator, const int64_t lower_bound, const int64_t upper_bound, const int64_t min_chunk_size, const int64_t max_chunk_size) :
  SLB4MPI::AbstractLoadBalancer::AbstractLoadBalancer(communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size) {
  this->counter = this->lower_bound;

#ifdef SLB4MPI_WITH_MPI
  MPI_Win_create(&counter, sizeof(counter), sizeof(counter), MPI_INFO_NULL, this->communicator, &(this->window));
  MPI_Barrier(this->communicator);
#endif
}

SLB4MPI::GuidedLoadBalancer::~GuidedLoadBalancer() {
#ifdef SLB4MPI_WITH_MPI
  MPI_Win_free(&(this->window));
#endif
}

bool SLB4MPI::GuidedLoadBalancer::get_range(int64_t& lower_bound, int64_t& upper_bound) {
  int64_t associated_tasks = 0;
  int64_t root_counter = 0;

#ifdef SLB4MPI_WITH_MPI
  MPI_Win_lock(MPI_LOCK_EXCLUSIVE, this->root, 0, this->window);
  MPI_Get(&root_counter, 1, MPI_INT64_T, this->root, 0, 1, MPI_INT64_T, this->window);
  MPI_Win_flush(this->root, this->window);
  associated_tasks = std::min(std::max((this->upper_bound - root_counter) / this->nranks, this->min_chunk_size), this->max_chunk_size);
  MPI_Fetch_and_op(&associated_tasks, &lower_bound, MPI_INT64_T, this->root, 0, MPI_SUM, this->window);
  MPI_Win_unlock(this->root, this->window);
#else
  lower_bound = this->counter;
  associated_tasks = std::min(std::max(this->upper_bound - this->counter, this->min_chunk_size), this->max_chunk_size);
  this->counter += associated_tasks;
#endif

  upper_bound = std::min(lower_bound + associated_tasks - 1, this->upper_bound);

  bool to_compute = true;
  if (lower_bound > this->upper_bound) to_compute = false;
  return to_compute;
}
