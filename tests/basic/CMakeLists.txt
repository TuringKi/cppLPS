add_executable(test_vec test_vec.cc)
target_link_libraries(test_vec basic)
enable_testing()

add_test(NAME test_vec COMMAND test_vec)
