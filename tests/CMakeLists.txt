add_subdirectory(basic)


add_executable(test_lexer test_lexer.cc
        ${CMAKE_SOURCE_DIR}/src/diag.cc
        ${CMAKE_SOURCE_DIR}/src/tu.cc
        ${CMAKE_SOURCE_DIR}/src/basic.cc
        ${CMAKE_SOURCE_DIR}/src/lexer.cc
    )

add_executable(test_parser test_parser.cc)
target_link_libraries(test_parser  lps_)

add_executable(test_parse_function_output test_parse_function_output.cc)
target_link_libraries(test_parse_function_output lps_)

enable_testing()

add_test(NAME test_lexer COMMAND test_lexer)
add_test(NAME test_parser COMMAND test_parser)
add_test(NAME test_parse_function_output COMMAND test_parse_function_output)
