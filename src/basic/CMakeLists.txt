file(GLOB SRC  "./*.cc")

add_library(basic STATIC ${SRC})

install(TARGETS basic DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/)
