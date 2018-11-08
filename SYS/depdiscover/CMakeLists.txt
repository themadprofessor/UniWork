cmake_minimum_required(VERSION 3.12)
project(depdiscover C)

set(CMAKE_C_STANDARD 99)

add_executable(depdiscover main.c)
add_library(tshtable tshtable.c)
add_library(tsllist tsllist.c)
