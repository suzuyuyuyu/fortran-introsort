<h1 align='center'> Fortran Intro Sort </h1>

A Fortran library that implements the introspective sort (introsort).

## Requirements

- Fortran Compiler (ifx, gfortran, etc.)
- CMake (>= 3.14)

## Build & Install

This project uses CMake for building. We provide CMake Presets for convenient configuration.

### Using CMake Presets (Recommended)

You can list available presets with:
```bash
cmake --list-presets
```

Common presets:
- `ifx-release`: Intel Fortran Compiler (ifx), Release build (-O3)
- `gfortran-release`: GNU Fortran Compiler (gfortran), Release build (-O3)
- `gfortran-debug`: GNU Fortran Compiler (gfortran), Debug build with checks

#### 1. Configure and Build

```bash
# Example: Using ifx (Release)
cmake --preset ifx-release
cmake --build --preset ifx-release
```

#### 2. Install (Optional)

```bash
# Install to system directories (requires sudo)
sudo cmake --install build/ifx-release
```

You can change the installation directory using `-DCMAKE_INSTALL_PREFIX` at the configuration step:

```bash
cmake --preset ifx-release -DCMAKE_INSTALL_PREFIX=./local_install
cmake --build --preset ifx-release
cmake --install build/ifx-release
```

Or override it at the install step:

```bash
cmake --install build/ifx-release --prefix ./local_install
```

You can also customize the subdirectory structure (e.g., `lib`, `include`) by setting standard CMake variables or the project-specific `INSTALL_MODULE_DIR`:

```bash
cmake --preset ifx-release \
    -DCMAKE_INSTALL_PREFIX=./local_install \
    -DCMAKE_INSTALL_LIBDIR=my_lib \
    -DINSTALL_MODULE_DIR=modules
```
This will install artifacts to:
- `./local_install/my_lib/` (Libraries)
- `./local_install/modules/` (Fortran Modules)

### Build Examples

To build the example programs, set `BUILD_EXAMPLES=ON`.

```bash
cmake --preset ifx-release -DBUILD_EXAMPLES=ON
cmake --build --preset ifx-release

# Run the example
./build/ifx-release/example/example_sort
```

## Usage in your code

### Fortran Code

```fortran
program main
    use intro_sort_mod
    implicit none
    ! ...
    
    ! Sort array
    call sort(array)
    
    ! Sort with options
    ! call sort(array, reverse=.true.)
end program
```

To avoid the conflict of subroutine name `sort` or others, you can rename as follows:
```fortran
program main
    use intro_sort_mod, only: introsort => sort
    implicit none
    ! ...
```

### Link and Compile
You can compile and link your Fortran code with the installed library as follows:
```bash
ifx -I<install-path>/include -L<install-path>/lib <src-files> -o <output-executables> -lintrosort
```

## Directory Structure (Build Artifacts)

After building, the artifacts are located in the build directory (e.g., `build/ifx-release/`):

- `include/`: Module files (`.mod`)
- `lib/`: Static libraries (`.a`)
- `example/`: Example executables

## License
This software is released under the [MIT License](LICENSE).
