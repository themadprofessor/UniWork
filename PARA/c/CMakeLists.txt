cmake_minimum_required(VERSION 3.15)
project(c C)

find_package(OpenMP)

set(CMAKE_C_STANDARD 11)

add_executable(totient_range_orig TotientRange_orig.c)
add_executable(totient_range TotientRange.c)

if (OpenMP_C_FOUND)
    target_link_libraries(totient_range_orig PRIVATE OpenMP::OpenMP_C)
    target_link_libraries(totient_range PRIVATE OpenMP::OpenMP_C)
endif ()
