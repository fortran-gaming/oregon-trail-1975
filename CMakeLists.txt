cmake_minimum_required(VERSION 3.13)
project(OregonTrail LANGUAGES Fortran)
include(CTest)

add_executable(oregon_trail_1975 src/oregon-trail-1975.f)

add_test(NAME Fortran1975
COMMAND ${CMAKE_COMMAND} -Dexe:FILEPATH=$<TARGET_FILE:oregon_trail_1975> -Din_file:FILEPATH=${CMAKE_CURRENT_SOURCE_DIR}/tests/in75.txt -P ${CMAKE_CURRENT_SOURCE_DIR}/cmake/execute.cmake)

find_program(basic_exe NAMES bwbasic)

if(basic_exe)
  add_test(NAME Basic1975
  COMMAND ${CMAKE_COMMAND} -Dexe:FILEPATH=${basic_exe} -Din_file:FILEPATH=${CMAKE_CURRENT_SOURCE_DIR}/tests/test.txt -P ${CMAKE_CURRENT_SOURCE_DIR}/cmake/execute.cmake)
endif()
