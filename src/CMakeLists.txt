
file(GLOB SRC  "./*.cc")
list(REMOVE_ITEM SRC "./main.cc")

list(APPEND SRC "${CMAKE_CURRENT_BINARY_DIR}/version.cc")

add_library(lps_ STATIC ${SRC})

install(TARGETS lps_ DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/)

add_executable(lps ./main.cc)
target_link_libraries(lps lps_)
install(TARGETS lps DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)
