cmake_minimum_required(VERSION 3.10)

project(fortran_introsort LANGUAGES Fortran)

include(GNUInstallDirs)

set(SOURCES
    src/intro-sort.F90
)

add_library(introsort ${SOURCES})

set_target_properties(introsort PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules
)

target_include_directories(introsort
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
