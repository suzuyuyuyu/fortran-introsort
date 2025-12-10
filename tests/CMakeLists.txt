cmake_minimum_required(VERSION 3.14)

project(example_sort LANGUAGES Fortran)

if(NOT TARGET introsort)
    find_package(fortran_introsort REQUIRED)
endif()

add_executable(example_sort main.F90)

target_link_libraries(example_sort PRIVATE introsort)
