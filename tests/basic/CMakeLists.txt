add_executable(test_vec test_vec.cc)
target_link_libraries(test_vec basic)

add_executable(test_str test_str.cc)
target_link_libraries(test_str basic)

enable_testing()

add_test(NAME test_vec COMMAND test_vec)
add_test(NAME test_str COMMAND test_str)
