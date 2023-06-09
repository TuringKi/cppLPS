
file(GLOB SRC  "./*.cc")
list(REMOVE_ITEM SRC "./main.cc")

SET(FUNC_IMPL ${CMAKE_SOURCE_DIR}/include/parse_function/function_impl.h)
SET(FUNC_DEF ${CMAKE_SOURCE_DIR}/include/parse_function/function.h)
SET(FUNC_KIND_DEF ${CMAKE_SOURCE_DIR}/include/parse_function/kinds.def)

add_custom_command(
  OUTPUT ${FUNC_IMPL} ${FUNC_DEF} ${FUNC_KIND_DEF}
  COMMAND python3  ${CMAKE_SOURCE_DIR}/gram/create_parse_functions.py
                   ${FUNC_IMPL}
                   ${FUNC_DEF}
                   ${FUNC_KIND_DEF}
  DEPENDS ${CMAKE_SOURCE_DIR}/gram/create_parse_functions.py
  VERBATIM)

list(APPEND SRC "${CMAKE_CURRENT_BINARY_DIR}/version.cc")

add_library(lps_ STATIC ${SRC} ${FUNC_DEF} ${FUNC_IMPL} ${FUNC_KIND_DEF})

install(TARGETS lps_ DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/)

add_executable(lps ./main.cc)
target_link_libraries(lps lps_)
install(TARGETS lps DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)
