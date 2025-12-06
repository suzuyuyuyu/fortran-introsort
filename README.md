<h1 align='center'> Fortran Quick Sort </h1>

A Fortran library that implements the quick sort.

## Usage
### 1. Clone this repository.

```bash
git clone https://github.com/suzuyuyuyu/fortran-quicksort.git
```

### 2. Compile the library.

```bash
cd fortran-quicksort
make
```
You can change the compiler, optimization flags, and so on in the `Makefile`.

| Flag | Default Value |
|--- | --- |
| `FC` | `ifx` |
| `FFLAGS` | `-O3` |
| `AR` | `llvm-ar rcs` |
| `LIB_OUT_DIR` | `lib` |
| `INC_OUT_DIR` | `include` |

e.g.,
```bash
make FC=gfortran FFLAGS='-O0 -g -Wall -Wextra'
```

### 3. Link the library and module in your Fortran code.

```fortran
program main
    use quick_sort_mod
    implicit none
    ⋮
    call sort(array)
    ! or
    call sort(array, pivot=2.0d0)
    ! or
    call sort(array, history, reverse=.true.)
    
    call reverse(array)
    
    ! After sorting
    corresponding_array = corresponding_array(history)
```

To find more details, see the sample code in the `examples` directory.
