cmake_minimum_required(VERSION 3.22)

project(MPI_load_balancer_example
  VERSION 0.0.1
  LANGUAGES CXX)

option(WITH_MPI  "Enable MPI"              OFF)
option(USE_LOCAL "Use local implmentation" OFF)

if(WITH_MPI)
  add_compile_definitions(WITH_MPI)
endif()

include(FetchContent)
FetchContent_Declare(SLB4MPI
    GIT_REPOSITORY https://github.com/foxtran/SLB4MPI.git
    GIT_TAG        origin/master
)
FetchContent_MakeAvailable(SLB4MPI)
FetchContent_GetProperties(SLB4MPI SOURCE_DIR SLB4MPI_SOURCE_DIR)

set(SLB4MPI_ENABLE_CXX ON)
set(SLB4MPI_WITH_MPI ${WITH_MPI})

if(USE_LOCAL)
  set(SLB4MPI_SOURCE_DIR ../../)
  add_subdirectory(${SLB4MPI_SOURCE_DIR} ${CMAKE_BINARY_DIR}/SLB4MPI-binary)
else()
  add_subdirectory(${SLB4MPI_SOURCE_DIR} ${SLB4MPI_SOURCE_DIR}-binary)
endif()

add_executable(SLB4MPI_cxx_example example.cpp)
target_link_libraries(SLB4MPI_cxx_example PUBLIC SLB4MPI::SLB4MPI_CXX)
