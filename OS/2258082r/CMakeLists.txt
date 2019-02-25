cmake_minimum_required(VERSION 3.13)

project(driver)

set(CMAKE_C_FLAGS "-g -Wall -Wno-int-to-pointer-cast -Wno-unused-variable -Wno-deprecated-declarations -DMAX_ITERS=100 -DNTH=8 -DBUF_SZ=64 -DVERBOSE_MT=4 -DW1=6 -DW2=7")
set(CMAKE_C_COMPILER clang)
add_definitions(-DGPU)
find_package(OpenCL)
find_package(Threads)

add_executable(testbench testbench.c driver.c driver.h)
target_link_libraries(testbench OpenCL::OpenCL)
target_link_libraries(testbench Threads::Threads)
