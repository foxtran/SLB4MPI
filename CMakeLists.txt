cmake_minimum_required(VERSION 3.22)

project(SLB4MPI
  VERSION 0.0.1
  DESCRIPTION "Simple load balancers for MPI")

option(SLB4MPI_ENABLE_CXX     "Enable C++-implemented load balancer"     OFF)
option(SLB4MPI_ENABLE_Fortran "Enable Fortran-implemented load balancer" OFF)
option(SLB4MPI_WITH_MPI       "Enable MPI"                               OFF)

if(SLB4MPI_ENABLE_CXX)
  enable_language(CXX)
endif()

if(SLB4MPI_ENABLE_Fortran)
  enable_language(Fortran)
endif()

if(SLB4MPI_WITH_MPI)
  find_package(MPI REQUIRED)
  if(SLB4MPI_ENABLE_Fortran)
    if(NOT MPI_Fortran_HAVE_F90_MODULE)
      message(FATAL_ERROR "F90 MPI module not found!")
    endif()
  endif()
endif()

if(SLB4MPI_ENABLE_CXX AND NOT TARGET SLB4MPI::SLB4MPI_CXX)
  set(SOURCES_CXX)
  add_subdirectory(src/CXX)
  add_library(SLB4MPI__SLB4MPI_cxx OBJECT ${SOURCES_CXX})
  target_include_directories(SLB4MPI__SLB4MPI_cxx PUBLIC src/CXX/SLB4MPI)
  target_compile_features(SLB4MPI__SLB4MPI_cxx PRIVATE cxx_std_14)
  set_target_properties(SLB4MPI__SLB4MPI_cxx PROPERTIES CXX_STANDARD_REQUIRED ON
                                                        CXX_EXTENSIONS OFF
                                                        INTERFACE_INCLUDE_DIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR}/src/CXX/SLB4MPI
  )
  if(SLB4MPI_WITH_MPI)
    target_compile_definitions(SLB4MPI__SLB4MPI_cxx PRIVATE SLB4MPI_WITH_MPI)
    target_link_libraries(SLB4MPI__SLB4MPI_cxx PUBLIC MPI::MPI_CXX)
  endif()
  add_library(SLB4MPI::SLB4MPI_CXX ALIAS SLB4MPI__SLB4MPI_cxx)
endif()

if(SLB4MPI_ENABLE_Fortran AND NOT TARGET SLB4MPI::SLB4MPI_Fortran)
  set(SOURCES_F)
  add_subdirectory(src/Fortran)
  add_library(SLB4MPI__SLB4MPI_f OBJECT ${SOURCES_F})
  set_target_properties(SLB4MPI__SLB4MPI_f PROPERTIES INCLUDE_DIRECTORIES ${CMAKE_BINARY_DIR}/SLB4MPI/Fortran/
                                                      Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/SLB4MPI/Fortran/
  )
  target_include_directories(SLB4MPI__SLB4MPI_f PUBLIC ${CMAKE_BINARY_DIR}/SLB4MPI/Fortran/)
  if(SLB4MPI_WITH_MPI)
    target_compile_definitions(SLB4MPI__SLB4MPI_f PRIVATE SLB4MPI_WITH_MPI)
    target_link_libraries(SLB4MPI__SLB4MPI_f PUBLIC MPI::MPI_Fortran)
  endif()
  if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
    target_compile_options(SLB4MPI__SLB4MPI_f PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-cpp>)
    target_compile_options(SLB4MPI__SLB4MPI_f PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-ffree-line-length-none>)
  elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Flang")
    target_compile_options(SLB4MPI__SLB4MPI_f PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-cpp>)
  elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
    target_compile_options(SLB4MPI__SLB4MPI_f PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-cpp>)
  elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "IntelLLVM")
    target_compile_options(SLB4MPI__SLB4MPI_f PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-cpp>)
  elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "NVHPC")
    target_compile_options(SLB4MPI__SLB4MPI_f PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-Mpreprocess>)
  elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Cray")
    target_compile_options(SLB4MPI__SLB4MPI_f PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-eF>) # Enable preprocessor
  else()
    message(FATAL_ERROR "${CMAKE_Fortran_COMPILER_ID} compiler is not supported yet!")
  endif()
  add_library(SLB4MPI::SLB4MPI_Fortran ALIAS SLB4MPI__SLB4MPI_f)
endif()
