add_subdirectory(basic)


add_executable(test_lexer test_lexer.cc)
target_link_libraries(test_lexer  lps_)

add_executable(test_parser test_parser.cc)
target_link_libraries(test_parser  lps_)


enable_testing()

# add_test(NAME test_lexer COMMAND test_lexer)
# add_test(NAME test_parser COMMAND test_parser)
