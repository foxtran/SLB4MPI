#pragma once

#ifdef SLB4MPI_LOCAL_STATIC_LB
#  error "SLB4MPI_LOCAL_STATIC_LB was previously defined"
#else
#  define SLB4MPI_LOCAL_STATIC_LB
#endif

#include "abstract_load_balancer.hpp"

namespace SLB4MPI {
  class LocalStaticLoadBalancer final : public AbstractLoadBalancer {
    private:
      int64_t counter;
    public:
      /**
       *
       * @brief initialize values of local static load balancer
       *
       * @param[in]           communicator   - MPI communicator on which load balancer will be used
       * @param[in]           lower_bound    - lower bound of range
       * @param[in]           upper_bound    - upper bound of range, upper_bound >= lower_bound
       * @param[in, optional] min_chunk_size - minimal size of chank that can be associated with job, default: 1
       * @param[in, optional] max_chunk_size - maximal size of chank that can be associated with job, default: upper_bound - lower_bound + 1
       *
       */
      LocalStaticLoadBalancer(const MPI_Comm communicator, const int64_t lower_bound, const int64_t upper_bound, const int64_t min_chunk_size = 1, const int64_t max_chunk_size = -1);
      /**
       *
       * @brief d-tor of LocalStaticLoadBalancer
       *
       */
      virtual ~LocalStaticLoadBalancer() {};
      /**
       *
       * @brief get range to compute
       * @note upper_bound can be only less than or equal LocalStaticLoadBalancer.upper_bound
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
