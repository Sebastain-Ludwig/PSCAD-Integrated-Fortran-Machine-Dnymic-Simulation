cmake_minimum_required(VERSION 3.5 FATAL_ERROR)
project(MADYCAL LANGUAGES Fortran)


set(CMAKE_Fortran_COMPLIER "/opt/intel/oneapi/compiler/2024.0/bin/ifort")

include(GNUInstallDirs)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY
${CMAKE_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY
${CMAKE_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY
${CMAKE_BINARY_DIR}/${CMAKE_INSTALL_BINDIR})

add_subdirectory(MATHBALIB)
add_subdirectory(NETSOLVERS)
add_subdirectory(POWERFLOW)

set(CMAKE_BUILD_TYPE Debug)

file(GLOB_RECURSE DIR_SRC_LIST "main.f90")
enable_testing()
add_subdirectory(Testing)

add_executable(MADYNCAL ${DIR_SRC_LIST})

add_dependencies(POWERFLOW NETSOLVERS MATHBALIB)
find_package(MKL CONFIG REQUIRED PATHS $ENV{MKLROOT})
message(STATUS "Imported oneMKL targets: ${MKL_IMPORTED_TARGETS}")
target_compile_options(MADYNCAL PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_COMPILE_OPTIONS>)
target_include_directories(MADYNCAL PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_INCLUDE_DIRECTORIES>)
link_directories("${CMAKE_CURRENT_SOURCE_DIR}/MKLLIB")
include_directories("${CMAKE_CURRENT_SOURCE_DIR}/MKLLIB")
target_link_libraries(
        MADYNCAL
        PUBLIC
        MATHBALIB
        POWERFLOW
        NETSOLVERS
        mkl_lapack95_ilp64
        MKL::MKL
)
