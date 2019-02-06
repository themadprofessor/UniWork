cmake_minimum_required(VERSION 3.13)

project(driver)

set(CMAKE_C_FLAGS "-Wall -Wno-int-to-pointer-cast -Wno-unused-variable -Wno-deprecated-declarations -DMAX_ITERS=100 -DNTH=8 -DBUF_SZ=64 -DVERBOSE_MT=0")
set(CMAKE_C_COMPILER clang)
add_definitions(-DGPU)

add_executable(testbench testbench.c driver.c driver.h)