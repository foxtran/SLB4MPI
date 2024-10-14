cmake_minimum_required(VERSION 3.22)

project(MPI_load_balancer
  VERSION 0.0.0)

option(MPIlb_ENABLE_Fortran "Enable Fortran-implemented load balancer" ON)
option(MPIlb_WITH_MPI       "Enable MPI"                               OFF)

if(MPIlb_ENABLE_Fortran)
  enable_language(Fortran)
endif()

if(MPIlb_WITH_MPI)
  find_package(MPI REQUIRED)
  if(MPIlb_ENABLE_Fortran)
    if(NOT MPI_Fortran_HAVE_F90_MODULE)
      message(FATAL_ERROR "F90 MPI module not found!")
    endif()
  endif()
  add_compile_definitions(WITH_MPI)
endif()

if(MPIlb_ENABLE_Fortran AND NOT TARGET MPIlb::MPIlb_f)
  set(SOURCES_F)
  add_subdirectory(src/Fortran)
  add_library(MPIlb_f OBJECT ${SOURCES_F})
  set_target_properties(MPIlb_f PROPERTIES INCLUDE_DIRECTORIES ${CMAKE_BINARY_DIR}/MPIlb/MPIlb_f/
                                           Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/MPIlb/MPIlb_f/
  )
  target_include_directories(MPIlb_f PUBLIC ${CMAKE_BINARY_DIR}/MPIlb/MPIlb_f/)
  if(MPIlb_WITH_MPI)
    target_link_libraries(MPIlb_f PUBLIC MPI::MPI_Fortran)
  endif()
  if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
    target_compile_options(MPIlb_f PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-cpp>)
    target_compile_options(MPIlb_f PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-ffree-line-length-none>)
    target_compile_options(MPIlb_f PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-fstrict-aliasing>)
  else()
    message(FATAL_ERROR "${CMAKE_Fortran_COMPILER_ID} compiler is not supported yet!")
  endif()
  add_library(MPIlb::MPIlb_f ALIAS MPIlb_f)
endif()
