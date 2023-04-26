
cmake_minimum_required(VERSION 3.2)

project(lps)


set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++20 -O0 -Werror")

set(CMAKE_INSTALL_PREFIX ${PROJECT_SOURCE_DIR}/.install/)


set(INCLUDE_DIR ${PROJECT_SOURCE_DIR}/include)

include_directories(${INCLUDE_DIR})

add_subdirectory(src)
add_subdirectory(tests)
