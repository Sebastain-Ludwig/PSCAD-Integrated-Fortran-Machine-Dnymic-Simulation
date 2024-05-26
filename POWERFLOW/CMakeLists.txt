add_library(POWERFLOW)
file(GLOB_RECURSE DIR_SRC_LIST "${CMAKE_CURRENT_SOURCE_DIR}/*.f90")

#set(OMP_LIBRARY "OMP_LIBRARY-FOUND")
#find_package(MKL CONFIG REQUIRED PATHS $ENV{MKLROOT})
#message(STATUS "Imported oneMKL targets: ${MKL_IMPORTED_TARGETS}")

#set(MKLPATH "$ENV{MKLROOT}/lib/intel64/")
#set(MKLINCLUDE "$ENV{MKLROOT}/include/")

#target_compile_options(NETSOLVERS PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_COMPILE_OPTIONS>)
#target_include_directories(NETSOLVERS PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_INCLUDE_DIRECTORIES>)
#find_package(MKL CONFIG REQUIRED PATHS $ENV{MKLROOT})

#include_directories("${CMAKE_CURRENT_SOURCE_DIR}/../MKLLIB")

target_sources(
        POWERFLOW
        PUBLIC
        ${DIR_SRC_LIST}
)
#target_link_libraries(
#        POWERFLOW
#        PUBLIC
#        MKL::MKL
#        mkl_lapack95_ilp64
#)
#link_directories("${CMAKE_CURRENT_SOURCE_DIR}/MKLLIB")