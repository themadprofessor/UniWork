cmake_minimum_required(VERSION 3.12)
project(depdiscover C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_FLAGS "-g -pthread -Wall -Werror -pedantic-errors")

add_executable(depdiscover dependencyDiscoverer.c)
add_library(tshtable tshtable.c)
add_library(tsllist tsllist.c)
target_link_libraries(depdiscover tshtable tsllist)
