add_executable(test_vec test_vec.cc)

add_executable(test_str test_str.cc)

add_executable(test_bitset test_bitset.cc)

enable_testing()

add_test(NAME test_vec COMMAND test_vec)
add_test(NAME test_str COMMAND test_str)
add_test(NAME test_bitset COMMAND test_bitset)
