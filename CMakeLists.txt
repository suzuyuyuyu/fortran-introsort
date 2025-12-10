cmake_minimum_required(VERSION 3.14)

project(fortran_introsort LANGUAGES Fortran)

include(GNUInstallDirs)

option(BUILD_EXAMPLES "Build example programs" OFF)

set(SOURCES
    src/intro-sort.F90
)

add_library(introsort ${SOURCES})

set_target_properties(introsort PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include
)

set_target_properties(introsort PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

target_include_directories(introsort
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

install(TARGETS introsort
    EXPORT introsortTargets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

set(INSTALL_MODULE_DIR ${CMAKE_INSTALL_INCLUDEDIR} CACHE PATH "Installation directory for Fortran module files")

install(DIRECTORY ${CMAKE_BINARY_DIR}/include/
    DESTINATION ${INSTALL_MODULE_DIR}
)


include(CMakePackageConfigHelpers)

write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/fortran_introsortConfigVersion.cmake"
    VERSION 0.1.0
    COMPATIBILITY AnyNewerVersion
)

install(EXPORT introsortTargets
    FILE fortran_introsortTargets.cmake
    NAMESPACE fortran_introsort::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fortran_introsort
)

file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/fortran_introsortConfig.cmake"
    "include(\"\${CMAKE_CURRENT_LIST_DIR}/fortran_introsortTargets.cmake\")\n"
    "if(NOT TARGET introsort)\n"
    "    add_library(introsort ALIAS fortran_introsort::introsort)\n"
    "endif()\n"
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/fortran_introsortConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/fortran_introsortConfigVersion.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fortran_introsort
)

if(BUILD_EXAMPLES)
    add_subdirectory(example)
endif()
