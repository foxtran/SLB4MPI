#include <SLB4MPI/static_load_balancer.hpp>

#include <algorithm>

SLB4MPI::StaticLoadBalancer::StaticLoadBalancer(const MPI_Comm communicator, const int64_t lower_bound, const int64_t upper_bound, const int64_t min_chunk_size, const int64_t max_chunk_size) :
  SLB4MPI::AbstractLoadBalancer::AbstractLoadBalancer(communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size) {
  this->counter = this->lower_bound + this->rank * this->max_chunk_size;
}

bool SLB4MPI::StaticLoadBalancer::get_range(int64_t& lower_bound, int64_t& upper_bound) {
  lower_bound = this->counter;
  upper_bound = std::min(lower_bound + this->max_chunk_size - 1, this->upper_bound);
  this->counter += this->max_chunk_size * this->nranks;

  bool to_compute = true;
  if (lower_bound > this->upper_bound) to_compute = false;
  return to_compute;
}
