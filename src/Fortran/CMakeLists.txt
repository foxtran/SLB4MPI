set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

set(SOURCES_F ${dir}/MPI_load_balancers.f90
              ${dir}/abstract_load_balancer.f90
              ${dir}/dynamic_load_balancer.f90
              ${dir}/guided_load_balancer.f90
              ${dir}/local_static_load_balancer.f90
              ${dir}/runtime_load_balancer.f90
              ${dir}/static_load_balancer.f90
              ${dir}/work_stealing_load_balancer.f90)

set(SOURCES_F ${SOURCES_F} PARENT_SCOPE)
