cmake_minimum_required(VERSION 3.22)

project(MPI_load_balancer_example
  VERSION 0.0.0
  LANGUAGES Fortran)

option(WITH_MPI       "Enable MPI"                               OFF)

if(WITH_MPI)
  add_compile_definitions(WITH_MPI)
endif()

include(FetchContent)
FetchContent_Declare(MPIlb
    GIT_REPOSITORY https://github.com/foxtran/MPI-lb.git
    GIT_TAG        origin/master
)
FetchContent_MakeAvailable(MPIlb)
FetchContent_GetProperties(MPIlb SOURCE_DIR MPIlb_SOURCE_DIR)
set(MPIlb_ENABLE_Fortran ON)
set(MPIlb_WITH_MPI ${WITH_MPI})
add_subdirectory(${MPIlb_SOURCE_DIR})

add_executable(MPIlb_f_example example.f90)
target_link_libraries(MPIlb_f_example PUBLIC MPIlb::MPIlb_f)
if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  target_compile_options(MPIlb_f_example PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-cpp>)
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "IntelLLVM")
  target_compile_options(MPIlb_f_example PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-cpp>)
else()
  message(FATAL_ERROR "${CMAKE_Fortran_COMPILER_ID} compiler is not supported yet!")
endif()
