#pragma once

#ifdef SLB4MPI_ABSTRACT_LB
#  error "SLB4MPI_ABSTRACT_LB was previously defined"
#else
#  define SLB4MPI_ABSTRACT_LB
#endif

#ifdef SLB4MPI_WITH_MPI
#  include <mpi.h>
#else
#  define MPI_Comm int
#endif
#include <cstdint>

namespace SLB4MPI {
  class AbstractLoadBalancer {
    protected:
      MPI_Comm communicator;               ///< MPI communicator
      int rank;                            ///< Rank of the process
      int root;                            ///< Rank of the root process
      int nranks;                          ///< Number of processes (group size)
      int64_t bounds[2];                   ///< Bounds of range
      int64_t& lower_bound = bounds[0];    ///< Lower bound of range
      int64_t& upper_bound = bounds[1];    ///< Upper bound of range
      int64_t min_chunk_size;              ///< Minimal chunk size for job
      int64_t max_chunk_size;              ///< Maximal chunk size for job
    public:
      /**
       *
       * @brief initialize default values of abstract load balancer
       *
       * @param[in] communicator   - MPI communicator on which load balancer will be used
       * @param[in] lower_bound    - lower bound of range
       * @param[in] upper_bound    - upper bound of range, upper_bound >= lower_bound
       * @param[in] min_chunk_size - minimal size of chank that can be associated with job, default: 1
       * @param[in] max_chunk_size - maximal size of chank that can be associated with job, default: upper_bound - lower_bound + 1
       *
       */
      AbstractLoadBalancer(const MPI_Comm communicator, const int64_t lower_bound, const int64_t upper_bound, const int64_t min_chunk_size = 1, const int64_t max_chunk_size = -1);
      /**
       *
       * @brief d-tor of AbstractLoadBalancer
       *
       */
      virtual ~AbstractLoadBalancer() {};
      /**
       *
       * @brief get range to compute
       * @note `upper_bound` can be only less than or equal `AbstractLoadBalancer.upper_bound`.
       *       So, for last elements a batch with size less than `min_chunk_size` can be returned
       *
       * @param[out] lower_bound - lower bound of range to compute
       * @param[out] upper_bound - upper bound of range to compute
       * @return                 - true if there is something to compute
       *
       */
      virtual bool get_range(int64_t& lower_bound, int64_t& upper_bound) = 0;
  };
}
