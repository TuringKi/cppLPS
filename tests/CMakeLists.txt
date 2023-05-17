add_subdirectory(basic)


add_executable(test_lexer test_lexer.cc)
target_link_libraries(test_lexer basic lps_)

enable_testing()

add_test(NAME test_lexer COMMAND test_lexer)
