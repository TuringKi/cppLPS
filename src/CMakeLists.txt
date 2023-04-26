
add_subdirectory(basic)

file(GLOB SRC  "./*.cc")

add_executable(lps ${SRC})
target_link_libraries(lps basic)
install(TARGETS lps DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)
