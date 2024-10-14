cmake_minimum_required(VERSION 3.22)

project(MPI_load_balancer_example
  VERSION 0.0.0
  LANGUAGES Fortran)

option(WITH_MPI "Enable MPI" OFF)

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

set(SLB4MPI_ENABLE_Fortran ON)
set(SLB4MPI_WITH_MPI ${WITH_MPI})

add_subdirectory(${SLB4MPI_SOURCE_DIR} ${SLB4MPI_SOURCE_DIR}-binary)

add_executable(SLB4MPI_f_example example.f90)
target_link_libraries(SLB4MPI_f_example PUBLIC SLB4MPI::SLB4MPI_Fortran)
if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  target_compile_options(SLB4MPI_f_example PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-cpp>)
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "IntelLLVM")
  target_compile_options(SLB4MPI_f_example PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-cpp>)
else()
  message(FATAL_ERROR "${CMAKE_Fortran_COMPILER_ID} compiler is not supported yet!")
endif()
