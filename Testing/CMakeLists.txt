add_executable(GENTESTS GEN_test.f90)
add_executable(BUSTESTS BUS_TEST.f90)
add_executable(YTESTS Y_TEST.f90)
add_executable(NLPFTESTS NLPF_TEST.f90)
add_executable(MTFATESTS MTFA_TEST.f90)
add_executable(PQPFTESTS PQPF_TEST.f90)

set(CMAKE_BUILD_TYPE Debug)

#find_package(MKL CONFIG REQUIRED PATHS $ENV{MKLROOT})
message(STATUS "Imported oneMKL targets: ${MKL_IMPORTED_TARGETS}")
target_compile_options(BUSTESTS PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_COMPILE_OPTIONS>)
target_include_directories(BUSTESTS PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_INCLUDE_DIRECTORIES>)
link_directories("${CMAKE_CURRENT_SOURCE_DIR}/../MKLLIB")
include_directories("${CMAKE_CURRENT_SOURCE_DIR}/../MKLLIB")

target_link_libraries(
        GENTESTS
        PUBLIC
        NETSOLVERS
        mkl_lapack95_ilp64
)

target_link_libraries(
        BUSTESTS
        PUBLIC
        NETSOLVERS
        MKL::MKL
        mkl_lapack95_ilp64
)

target_link_libraries(
        YTESTS
        PUBLIC
        NETSOLVERS
        MKL::MKL
        mkl_lapack95_ilp64
)

target_link_libraries(
        NLPFTESTS
        PUBLIC
        MATHBALIB
        POWERFLOW
        MKL::MKL
        mkl_lapack95_ilp64
)

target_link_libraries(
        MTFATESTS
        PUBLIC
        MATHBALIB
)

target_link_libraries(
        PQPFTESTS
        PUBLIC
        MATHBALIB
        POWERFLOW
)

add_test(
        NAME
        GENTESTS
        COMMAND
        $<TARGET_FILE:GENTESTS>
)
add_test(
        NAME
        BUSTESTS
        COMMAND
        $<TARGET_FILE:BUSTESTS>
)
add_test(
        NAME
        YTESTS
        COMMAND
        $<TARGET_FILE:YTESTS>
)

add_test(
        NAME
        NLPFTESTS
        COMMAND
        $<TARGET_FILE:NLPFTESTS>
)

add_test(
        NAME
        MTFATESTS
        COMMAND
        $<TARGET_FILE:MTFATESTS>
)

add_test(
        NAME
        PQPFTESTS
        COMMAND
        $<TARGET_FILE:PQPFTESTS>
)