

set(SRC main.cc parser.cc)

add_executable(lps ${SRC})

install(TARGETS lps DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)