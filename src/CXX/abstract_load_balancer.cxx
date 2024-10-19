#include <SLB4MPI/abstract_load_balancer.hpp>

#include <cstdlib>
#include <algorithm>

/**
 *
 * @brief initialize default values of abstract load balancer
 *
 * @param[in]           communicator   - MPI communicator on which load balancer will be used
 * @param[in]           lower_bound    - lower bound of range
 * @param[in]           upper_bound    - upper bound of range, upper_bound >= lower_bound
 * @param[in, optional] min_chunk_size - minimal size of chank that can be associated with job, default: 1
 * @param[in, optional] max_chunk_size - maximal size of chank that can be associated with job, default: upper_bound - lower_bound + 1
 *
 */
SLB4MPI::AbstractLoadBalancer::AbstractLoadBalancer(const int communicator, const int64_t lower_bound, const int64_t upper_bound, const int64_t min_chunk_size, const int64_t max_chunk_size) {
  this->communicator = communicator;
  this->rank = 0;
  this->nranks = 1;
  this->root = 0;

#ifdef SLB4MPI_WITH_MPI
  MPI_Comm_rank(this->communicator, &(this->rank));
  MPI_Comm_size(this->communicator, &(this->nranks));
#endif

  this->lower_bound = lower_bound;
  this->upper_bound = upper_bound;

  this->min_chunk_size = min_chunk_size;
  this->max_chunk_size = max_chunk_size;
  if (this->max_chunk_size < 0) {
    this->max_chunk_size = std::max((this->upper_bound - this->lower_bound + 1) / this->nranks, this->min_chunk_size);
  }

  if (this->min_chunk_size <= 0) std::exit(1); // "Min chunk size must be greater than zero!"

  if (this->lower_bound > this->upper_bound) std::exit(1); // "Upper bound is less than lower bound!"
  if (this->min_chunk_size > this->max_chunk_size) std::exit(1); // "Max chunk size is less than min chunk size!"
}
