
file(GLOB SRC  "./*.cc")
list(REMOVE_ITEM SRC "./main.cc")

add_custom_command(
  OUTPUT ${FUNC_IMPL} ${FUNC_DEF} ${FUNC_KIND_DEF} ${FUNC_DIAG_DEF}
  COMMAND python3  ${CMAKE_SOURCE_DIR}/gram/create_parse_functions.py
                   ${FUNC_IMPL}
                   ${FUNC_DEF}
                   ${FUNC_KIND_DEF}
                   ${FUNC_DIAG_DEF}
  DEPENDS ${CMAKE_SOURCE_DIR}/gram/create_parse_functions.py
          ${CMAKE_SOURCE_DIR}/gram/config.py
  VERBATIM)

list(APPEND SRC "${CMAKE_CURRENT_BINARY_DIR}/version.cc")

add_library(lps_ STATIC ${SRC} ${FUNC_DEF} ${FUNC_IMPL} ${FUNC_DIAG_DEF} ${FUNC_KIND_DEF})

install(TARGETS lps_ DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/)

add_executable(lps ./main.cc)
target_link_libraries(lps lps_)
install(TARGETS lps DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)
