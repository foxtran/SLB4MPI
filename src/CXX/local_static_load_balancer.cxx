#include <SLB4MPI/local_static_load_balancer.hpp>

#include <algorithm>

SLB4MPI::LocalStaticLoadBalancer::LocalStaticLoadBalancer(const MPI_Comm communicator, const int64_t lower_bound, const int64_t upper_bound, const int64_t min_chunk_size, const int64_t max_chunk_size) :
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
}

bool SLB4MPI::LocalStaticLoadBalancer::get_range(int64_t& lower_bound, int64_t& upper_bound) {
  lower_bound = this->counter;
  upper_bound = std::min(lower_bound + this->max_chunk_size - 1, this->upper_bound);
  this->counter = upper_bound + 1;

  bool to_compute = true;
  if (lower_bound > this->upper_bound) to_compute = false;
  return to_compute;
}
