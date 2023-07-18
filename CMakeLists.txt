
cmake_minimum_required(VERSION 3.2)

project(lps)
find_package(Git)
execute_process(COMMAND
  "${GIT_EXECUTABLE}" describe --match=NeVeRmAtCh --always --abbrev=40 --dirty
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  OUTPUT_VARIABLE GIT_SHA1
  ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)

# the date of the commit
execute_process(COMMAND
  "${GIT_EXECUTABLE}" log -1 --format=%ad --date=local
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  OUTPUT_VARIABLE GIT_DATE
  ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)

# the subject of the commit
execute_process(COMMAND
  "${GIT_EXECUTABLE}" log -1 --format=%s
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  OUTPUT_VARIABLE GIT_COMMIT_SUBJECT
  ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)

# generate version.cc
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/src/version.cc.in" "${CMAKE_CURRENT_BINARY_DIR}/src/version.cc" @ONLY)


set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++20 -Werror")

set(CMAKE_INSTALL_PREFIX ${PROJECT_SOURCE_DIR}/.install/)


set(INCLUDE_DIR ${PROJECT_SOURCE_DIR}/include)

include_directories(${INCLUDE_DIR})


SET(FUNC_IMPL ${CMAKE_SOURCE_DIR}/src/parse_function/function_impl.cc)
SET(FUNC_DEF ${CMAKE_SOURCE_DIR}/include/parse_function/function.h)
SET(FUNC_KIND_DEF ${CMAKE_SOURCE_DIR}/include/parse_function/kinds.def)
SET(FUNC_DIAG_DEF ${CMAKE_SOURCE_DIR}/include/diag/kinds_parse.def)

add_subdirectory(src)
add_subdirectory(tests)
