#pragma once

#ifdef SLB4MPI_WORK_STEALING_LB
#  error "SLB4MPI_WORK_STEALING_LB was previously defined"
#else
#  define SLB4MPI_WORK_STEALING_LB
#endif

#include "abstract_load_balancer.hpp"

namespace SLB4MPI {
  class WorkStealingLoadBalancer final : public AbstractLoadBalancer {
    private:
      int64_t counter;
#ifdef SLB4MPI_WITH_MPI
      MPI_Win window_num_active;  ///< number of active threads
      MPI_Win window_bounds;      ///< lower and upper bounds of rank
      MPI_Win window_actual_rank; ///< actual rank to compute (for fast look up)
      MPI_Win window_done;        ///< status of thread
      int actual_rank;            ///< rank for computing
      int num_active;             ///< number of active ranks
      bool done;                  ///< does it compute all elements in its own rank? Implemented for non-blocking check with MPI_LOCK_SHARED
#endif
    public:
      /**
       *
       * @brief initialize values of work stealing load balancer
       *
       * @param[in]           communicator   - MPI communicator on which load balancer will be used
       * @param[in]           lower_bound    - lower bound of range
       * @param[in]           upper_bound    - upper bound of range, upper_bound >= lower_bound
       * @param[in, optional] min_chunk_size - minimal size of chank that can be associated with job, default: 1
       * @param[in, optional] max_chunk_size - maximal size of chank that can be associated with job, default: upper_bound - lower_bound + 1
       *
       */
      WorkStealingLoadBalancer(const MPI_Comm communicator, const int64_t lower_bound, const int64_t upper_bound, const int64_t min_chunk_size = 1, const int64_t max_chunk_size = -1);
      /**
       *
       * @brief d-tor of WorkStealingLoadBalancer
       *
       */
      virtual ~WorkStealingLoadBalancer();
      /**
       *
       * @brief get range to compute
       * @note upper_bound can be only less than or equal WorkStealingLoadBalancer.upper_bound
       *       so, for last elements a batch with size less than min_chunk_size can be returned
       *
       * @param[out] lower_bound - lower bound of range to compute
       * @param[out] upper_bound - upper bound of range to compute
       * @return                 - is there something to compute?
       *
       */
      virtual bool get_range(int64_t&, int64_t&);
  };
}
