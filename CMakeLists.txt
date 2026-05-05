cmake_minimum_required(VERSION 3.14)

project(fortran_introsort VERSION 1.0.1 LANGUAGES Fortran)

if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set(CMAKE_INSTALL_PREFIX "." CACHE PATH "Default install prefix" FORCE)
endif()

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib CACHE PATH "Output directory for archives")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib CACHE PATH "Output directory for libraries")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin CACHE PATH "Output directory for runtime binaries")
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules CACHE PATH "Output directory for Fortran modules")

option(BUILD_EXAMPLES "Build example programs" OFF)

set(SOURCES
    src/intro-sort.F90
)

add_library(introsort ${SOURCES})

target_include_directories(introsort
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

install(TARGETS introsort
    EXPORT introsortTargets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

install(DIRECTORY ${CMAKE_BINARY_DIR}/modules/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

install(EXPORT introsortTargets
    FILE introsort_fTargets.cmake
    NAMESPACE introsort_f::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/introsort_f
)

configure_package_config_file(
    cmake/introsort_fConfig.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/introsort_fConfig.cmake
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/introsort_f
)

write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/introsort_fConfigVersion.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/introsort_fConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/introsort_fConfigVersion.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/introsort_f
)

if(BUILD_EXAMPLES)
    add_subdirectory(example)
endif()
