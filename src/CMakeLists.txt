
add_subdirectory(basic)

file(GLOB SRC  "./*.cc")
list(APPEND SRC "${CMAKE_CURRENT_BINARY_DIR}/version.cc")


add_executable(lps ${SRC})
target_link_libraries(lps basic)
install(TARGETS lps DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)
