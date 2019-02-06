cmake_minimum_required(VERSION 3.13)
project(dnslookup C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_FLAGS "-W -Wall -Werror")

add_executable(dnslookup dnslookup.c)