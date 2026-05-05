#include "check.inc"
program main
  use, intrinsic :: iso_fortran_env
  use mod_introsort
  implicit none
  integer :: i, m, n
  integer, allocatable :: arr(:), twod_arr(:, :), hist(:)
  real(real64), allocatable :: corresp(:), twod_corresp(:, :)

  n = 100
  allocate(arr(n), corresp(n))
  allocate(hist(n), source=[(i, i=1,n)])

  call random_seed()
  block
    real(real64) :: arr_real(n)
    call random_number(arr_real)
    arr = int(arr_real * 100)
    call random_number(arr_real)
    corresp = dble(arr_real * 10)
  end block


  if (n <= 20) then
    write(output_unit,'("Unsorted array:")')
    write(output_unit,'(*(i5,1x))') arr
    write(output_unit,'(*(f5.1,1x))') corresp
  else
    block
      integer :: log_unit
      open(newunit=log_unit, file="log", status="replace", action="write")
      write(log_unit,'("Unsorted array:")')
      write(log_unit,'(*(i5,1x))') arr
      write(log_unit,'(*(i5,1x))') hist
      write(log_unit,'(*(f5.1,1x))') corresp
      close(log_unit)
    end block
  end if

  call sort(arr, hist)
  corresp = corresp(hist)

  if (n <= 20) then
    write(output_unit,'("Sorted array:")')
    write(output_unit,'(*(i5,1x))') arr
    write(output_unit,'(*(i5,1x))') hist
    write(output_unit,'(*(f5.1,1x))') corresp
  else
    block
      integer :: log_unit
      open(newunit=log_unit, file="log", status="old", action="write", position="append")
      write(log_unit,'("Sorted array:")')
      write(log_unit,'(*(i5,1x))') arr
      write(log_unit,'(*(i5,1x))') hist
      write(log_unit,'(*(f5.1,1x))') corresp
      write(log_unit,'(2x,*(l5,1x))') CHECK_STABLE(arr, hist)
      close(log_unit)
    end block
  end if
  write(output_unit,'("Check:")')
  write(output_unit,'("    is sorted: ",l1)') IS_SORTED(arr)
  write(output_unit,'("    is stable: ",l1)') IS_STABLE(arr, hist)

  deallocate(arr, corresp, hist)

  block
    integer :: permutation(10) = [(i, i=1,10)]
    n = 10
    allocate(arr(n), corresp(n))
    allocate(hist(n), source=[(i, i=1,n)])

    call random_seed()
    block
      real(real64) :: arr_real(n)
      call random_number(arr_real)
      arr = int(arr_real * 100)
      call random_number(arr_real)
      corresp = dble(arr_real * 10)
    end block


    if (n <= 20) then
      write(output_unit,'("Unsorted array:")')
      write(output_unit,'(*(i5,1x))') arr
      write(output_unit,'(*(f5.1,1x))') corresp
    else
      block
        integer :: log_unit
        open(newunit=log_unit, file="log", status="replace", action="write")
        write(log_unit,'("Unsorted array:")')
        write(log_unit,'(*(i5,1x))') arr
        write(log_unit,'(*(i5,1x))') hist
        write(log_unit,'(*(f5.1,1x))') corresp
        close(log_unit)
      end block
    end if

    call sort(arr(:), permutation)
    call apply_permutation(corresp, permutation)
    ! temp = corresp(3:6)
    ! corresp(3:6) = corresp(permutation)

    if (n <= 20) then
      write(output_unit,'("Sorted array:")')
      write(output_unit,'(*(i5,1x))') arr
      write(output_unit,'(*(i5,1x))') hist
      write(output_unit,'(*(f5.1,1x))') corresp
    else
      block
        integer :: log_unit
        open(newunit=log_unit, file="log", status="old", action="write", position="append")
        write(log_unit,'("Sorted array:")')
        write(log_unit,'(*(i5,1x))') arr
        write(log_unit,'(*(i5,1x))') hist
        write(log_unit,'(*(f5.1,1x))') corresp
        write(log_unit,'(2x,*(l5,1x))') CHECK_STABLE(arr, hist)
        close(log_unit)
      end block
    end if
    write(output_unit,'("Check:")')
    write(output_unit,'("    is sorted: ",l1)') IS_SORTED(arr)
    write(output_unit,'("    is stable: ",l1)') IS_STABLE(arr, hist)

    deallocate(arr, corresp, hist)
  end block

end program main
