add_executable(test_vec test_vec.cc)

add_executable(test_str test_str.cc)

add_executable(test_bitset test_bitset.cc)

add_executable(test_apn test_apn.cc)
target_link_libraries(test_apn  lps_)

enable_testing()

# add_test(NAME test_vec COMMAND test_vec)
# add_test(NAME test_str COMMAND test_str)
# add_test(NAME test_bitset COMMAND test_bitset)
add_test(NAME test_apn COMMAND test_apn)
