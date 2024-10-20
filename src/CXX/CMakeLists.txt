set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

set(SOURCES_CXX ${dir}/abstract_load_balancer.cxx
                ${dir}/static_load_balancer.cxx
                ${dir}/local_static_load_balancer.cxx
                ${dir}/dynamic_load_balancer.cxx
                ${dir}/guided_load_balancer.cxx)

set(SOURCES_CXX ${SOURCES_CXX} PARENT_SCOPE)
