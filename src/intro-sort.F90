! -----------------------------------------------------------------------------
!  Copyright (c) 2025 Yuta SUZUKI
!  Released under the MIT License
!  https://opensource.org/licenses/MIT
! -----------------------------------------------------------------------------
#ifndef DEFAULT_INT
#  define DEFAULT_INT int32
#endif
#ifndef DEFAULT_REAL
#  define DEFAULT_REAL real64
#endif

! #define IS_LARGE_ARRAY

module intro_sort_mod
  use, intrinsic :: iso_fortran_env
  implicit none
  integer(DEFAULT_INT), parameter :: short = int16, long = int32, llong = int64, sp = real32, dp = real64, qp = real128

  character(len=*), parameter :: SORTING_METHOD = 'Intro Sort'
  private
  public :: SORTING_METHOD

  interface sort
    module procedure introsort_int8
    module procedure introsort_int16
    module procedure introsort_int32
    module procedure introsort_int64
    module procedure introsort_real32
    module procedure introsort_real64
    module procedure introsort_real128
    module procedure introsort_int8_hist
    module procedure introsort_int16_hist
    module procedure introsort_int32_hist
    module procedure introsort_int64_hist
    module procedure introsort_real32_hist
    module procedure introsort_real64_hist
    module procedure introsort_real128_hist
  end interface sort

  interface reverse
    module procedure reverse_int8
    module procedure reverse_int16
    module procedure reverse_int32
    module procedure reverse_int64
    module procedure reverse_real32
    module procedure reverse_real64
    module procedure reverse_real128
    module procedure reverse_int8_hist
    module procedure reverse_int16_hist
    module procedure reverse_int32_hist
    module procedure reverse_int64_hist
    module procedure reverse_real32_hist
    module procedure reverse_real64_hist
    module procedure reverse_real128_hist
  end interface reverse
  public :: sort, reverse

  integer(DEFAULT_INT) :: max_depth

contains

  subroutine introsort_int8(arr, reverse)
    implicit none
    integer(int8), intent(inout) :: arr(:)

    logical, intent(in), optional :: reverse

    max_depth = 2 * ceiling(log(real(size(arr), kind=DEFAULT_REAL)) / log(2.0_real64))

    call introsort_recurs_int8(arr, 1, size(arr), 1)

    if (present(reverse)) then
      if (reverse) then
        call reverse_int8(arr)
      end if
    end if
  end subroutine introsort_int8

  recursive subroutine introsort_recurs_int8(arr, left, right, depth)
    implicit none
    integer(int8), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(in) :: depth

    integer(DEFAULT_INT) :: mid
    integer(DEFAULT_INT) :: pivot_idx

    if (right - left + 1 <= 16) then
      call bisort_int8(arr(left:right))
      return
    end if

    if (depth >= max_depth) then
      call hsort_int8(arr(left:right))
      return
    end if


    if (left < right) then
#     ifdef IS_LARGE_ARRAY
      pivot_idx = tukeys_ninther_pivot_int8(arr, left, right)
#     else
      pivot_idx = median_of_three_pivot_int8(arr, left, right)
#     endif

      mid = partition_int8(arr, left, right, arr(pivot_idx))

      call introsort_recurs_int8(arr, left, mid, depth + 1)
      call introsort_recurs_int8(arr, mid + 1, right, depth + 1)
    end if
  end subroutine introsort_recurs_int8

  subroutine introsort_int16(arr, reverse)
    implicit none
    integer(int16), intent(inout) :: arr(:)

    logical, intent(in), optional :: reverse

    max_depth = 2 * ceiling(log(real(size(arr), kind=DEFAULT_REAL)) / log(2.0_real64))

    call introsort_recurs_int16(arr, 1, size(arr), 1)

    if (present(reverse)) then
      if (reverse) then
        call reverse_int16(arr)
      end if
    end if
  end subroutine introsort_int16

  recursive subroutine introsort_recurs_int16(arr, left, right, depth)
    implicit none
    integer(int16), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(in) :: depth

    integer(DEFAULT_INT) :: mid
    integer(DEFAULT_INT) :: pivot_idx

    if (right - left + 1 <= 16) then
      call bisort_int16(arr(left:right))
      return
    end if

    if (depth >= max_depth) then
      call hsort_int16(arr(left:right))
      return
    end if


    if (left < right) then
#     ifdef IS_LARGE_ARRAY
      pivot_idx = tukeys_ninther_pivot_int16(arr, left, right)
#     else
      pivot_idx = median_of_three_pivot_int16(arr, left, right)
#     endif

      mid = partition_int16(arr, left, right, arr(pivot_idx))

      call introsort_recurs_int16(arr, left, mid, depth + 1)
      call introsort_recurs_int16(arr, mid + 1, right, depth + 1)
    end if
  end subroutine introsort_recurs_int16

  subroutine introsort_int32(arr, reverse)
    implicit none
    integer(int32), intent(inout) :: arr(:)

    logical, intent(in), optional :: reverse

    max_depth = 2 * ceiling(log(real(size(arr), kind=DEFAULT_REAL)) / log(2.0_real64))

    call introsort_recurs_int32(arr, 1, size(arr), 1)

    if (present(reverse)) then
      if (reverse) then
        call reverse_int32(arr)
      end if
    end if
  end subroutine introsort_int32

  recursive subroutine introsort_recurs_int32(arr, left, right, depth)
    implicit none
    integer(int32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(in) :: depth

    integer(DEFAULT_INT) :: mid
    integer(DEFAULT_INT) :: pivot_idx

    if (right - left + 1 <= 16) then
      call bisort_int32(arr(left:right))
      return
    end if

    if (depth >= max_depth) then
      call hsort_int32(arr(left:right))
      return
    end if


    if (left < right) then
#     ifdef IS_LARGE_ARRAY
      pivot_idx = tukeys_ninther_pivot_int32(arr, left, right)
#     else
      pivot_idx = median_of_three_pivot_int32(arr, left, right)
#     endif

      mid = partition_int32(arr, left, right, arr(pivot_idx))

      call introsort_recurs_int32(arr, left, mid, depth + 1)
      call introsort_recurs_int32(arr, mid + 1, right, depth + 1)
    end if
  end subroutine introsort_recurs_int32

  subroutine introsort_int64(arr, reverse)
    implicit none
    integer(int64), intent(inout) :: arr(:)

    logical, intent(in), optional :: reverse

    max_depth = 2 * ceiling(log(real(size(arr), kind=DEFAULT_REAL)) / log(2.0_real64))

    call introsort_recurs_int64(arr, 1, size(arr), 1)

    if (present(reverse)) then
      if (reverse) then
        call reverse_int64(arr)
      end if
    end if
  end subroutine introsort_int64

  recursive subroutine introsort_recurs_int64(arr, left, right, depth)
    implicit none
    integer(int64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(in) :: depth

    integer(DEFAULT_INT) :: mid
    integer(DEFAULT_INT) :: pivot_idx

    if (right - left + 1 <= 16) then
      call bisort_int64(arr(left:right))
      return
    end if

    if (depth >= max_depth) then
      call hsort_int64(arr(left:right))
      return
    end if


    if (left < right) then
#     ifdef IS_LARGE_ARRAY
      pivot_idx = tukeys_ninther_pivot_int64(arr, left, right)
#     else
      pivot_idx = median_of_three_pivot_int64(arr, left, right)
#     endif

      mid = partition_int64(arr, left, right, arr(pivot_idx))

      call introsort_recurs_int64(arr, left, mid, depth + 1)
      call introsort_recurs_int64(arr, mid + 1, right, depth + 1)
    end if
  end subroutine introsort_recurs_int64

  subroutine introsort_real32(arr, reverse)
    implicit none
    real(real32), intent(inout) :: arr(:)

    logical, intent(in), optional :: reverse

    max_depth = 2 * ceiling(log(real(size(arr), kind=DEFAULT_REAL)) / log(2.0_real64))

    call introsort_recurs_real32(arr, 1, size(arr), 1)

    if (present(reverse)) then
      if (reverse) then
        call reverse_real32(arr)
      end if
    end if
  end subroutine introsort_real32

  recursive subroutine introsort_recurs_real32(arr, left, right, depth)
    implicit none
    real(real32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(in) :: depth

    integer(DEFAULT_INT) :: mid
    integer(DEFAULT_INT) :: pivot_idx

    if (right - left + 1 <= 16) then
      call bisort_real32(arr(left:right))
      return
    end if

    if (depth >= max_depth) then
      call hsort_real32(arr(left:right))
      return
    end if


    if (left < right) then
#     ifdef IS_LARGE_ARRAY
      pivot_idx = tukeys_ninther_pivot_real32(arr, left, right)
#     else
      pivot_idx = median_of_three_pivot_real32(arr, left, right)
#     endif

      mid = partition_real32(arr, left, right, arr(pivot_idx))

      call introsort_recurs_real32(arr, left, mid, depth + 1)
      call introsort_recurs_real32(arr, mid + 1, right, depth + 1)
    end if
  end subroutine introsort_recurs_real32

  subroutine introsort_real64(arr, reverse)
    implicit none
    real(real64), intent(inout) :: arr(:)

    logical, intent(in), optional :: reverse

    max_depth = 2 * ceiling(log(real(size(arr), kind=DEFAULT_REAL)) / log(2.0_real64))

    call introsort_recurs_real64(arr, 1, size(arr), 1)

    if (present(reverse)) then
      if (reverse) then
        call reverse_real64(arr)
      end if
    end if
  end subroutine introsort_real64

  recursive subroutine introsort_recurs_real64(arr, left, right, depth)
    implicit none
    real(real64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(in) :: depth

    integer(DEFAULT_INT) :: mid
    integer(DEFAULT_INT) :: pivot_idx

    if (right - left + 1 <= 16) then
      call bisort_real64(arr(left:right))
      return
    end if

    if (depth >= max_depth) then
      call hsort_real64(arr(left:right))
      return
    end if


    if (left < right) then
#     ifdef IS_LARGE_ARRAY
      pivot_idx = tukeys_ninther_pivot_real64(arr, left, right)
#     else
      pivot_idx = median_of_three_pivot_real64(arr, left, right)
#     endif

      mid = partition_real64(arr, left, right, arr(pivot_idx))

      call introsort_recurs_real64(arr, left, mid, depth + 1)
      call introsort_recurs_real64(arr, mid + 1, right, depth + 1)
    end if
  end subroutine introsort_recurs_real64

  subroutine introsort_real128(arr, reverse)
    implicit none
    real(real128), intent(inout) :: arr(:)

    logical, intent(in), optional :: reverse

    max_depth = 2 * ceiling(log(real(size(arr), kind=DEFAULT_REAL)) / log(2.0_real64))

    call introsort_recurs_real128(arr, 1, size(arr), 1)

    if (present(reverse)) then
      if (reverse) then
        call reverse_real128(arr)
      end if
    end if
  end subroutine introsort_real128

  recursive subroutine introsort_recurs_real128(arr, left, right, depth)
    implicit none
    real(real128), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(in) :: depth

    integer(DEFAULT_INT) :: mid
    integer(DEFAULT_INT) :: pivot_idx

    if (right - left + 1 <= 16) then
      call bisort_real128(arr(left:right))
      return
    end if

    if (depth >= max_depth) then
      call hsort_real128(arr(left:right))
      return
    end if


    if (left < right) then
#     ifdef IS_LARGE_ARRAY
      pivot_idx = tukeys_ninther_pivot_real128(arr, left, right)
#     else
      pivot_idx = median_of_three_pivot_real128(arr, left, right)
#     endif

      mid = partition_real128(arr, left, right, arr(pivot_idx))

      call introsort_recurs_real128(arr, left, mid, depth + 1)
      call introsort_recurs_real128(arr, mid + 1, right, depth + 1)
    end if
  end subroutine introsort_recurs_real128

  subroutine introsort_int8_hist(arr, history, reverse)
    implicit none
    integer(int8), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    logical, intent(in), optional :: reverse

    max_depth = 2 * ceiling(log(real(size(arr), kind=DEFAULT_REAL)) / log(2.0_real64))

    call introsort_recurs_int8_hist(arr, 1, size(arr), 1, history)

    if (present(reverse)) then
      if (reverse) then
        call reverse_int8_hist(arr, history)
      end if
    end if
  end subroutine introsort_int8_hist

  recursive subroutine introsort_recurs_int8_hist(arr, left, right, depth, history)
    implicit none
    integer(int8), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(in) :: depth
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT) :: mid
    integer(DEFAULT_INT) :: pivot_idx

    if (right - left + 1 <= 16) then
      call bisort_int8_hist(arr(left:right), history)
      return
    end if

    if (depth >= max_depth) then
      call hsort_int8_hist(arr(left:right), history)
      return
    end if


    if (left < right) then
#     ifdef IS_LARGE_ARRAY
      pivot_idx = tukeys_ninther_pivot_int8(arr, left, right)
#     else
      pivot_idx = median_of_three_pivot_int8(arr, left, right)
#     endif

      mid = partition_int8_hist(arr, left, right, arr(pivot_idx), history)

      call introsort_recurs_int8_hist(arr, left, mid, depth + 1, history)
      call introsort_recurs_int8_hist(arr, mid + 1, right, depth + 1, history)
    end if
  end subroutine introsort_recurs_int8_hist

  subroutine introsort_int16_hist(arr, history, reverse)
    implicit none
    integer(int16), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    logical, intent(in), optional :: reverse

    max_depth = 2 * ceiling(log(real(size(arr), kind=DEFAULT_REAL)) / log(2.0_real64))

    call introsort_recurs_int16_hist(arr, 1, size(arr), 1, history)

    if (present(reverse)) then
      if (reverse) then
        call reverse_int16_hist(arr, history)
      end if
    end if
  end subroutine introsort_int16_hist

  recursive subroutine introsort_recurs_int16_hist(arr, left, right, depth, history)
    implicit none
    integer(int16), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(in) :: depth
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT) :: mid
    integer(DEFAULT_INT) :: pivot_idx

    if (right - left + 1 <= 16) then
      call bisort_int16_hist(arr(left:right), history)
      return
    end if

    if (depth >= max_depth) then
      call hsort_int16_hist(arr(left:right), history)
      return
    end if


    if (left < right) then
#     ifdef IS_LARGE_ARRAY
      pivot_idx = tukeys_ninther_pivot_int16(arr, left, right)
#     else
      pivot_idx = median_of_three_pivot_int16(arr, left, right)
#     endif

      mid = partition_int16_hist(arr, left, right, arr(pivot_idx), history)

      call introsort_recurs_int16_hist(arr, left, mid, depth + 1, history)
      call introsort_recurs_int16_hist(arr, mid + 1, right, depth + 1, history)
    end if
  end subroutine introsort_recurs_int16_hist

  subroutine introsort_int32_hist(arr, history, reverse)
    implicit none
    integer(int32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    logical, intent(in), optional :: reverse

    max_depth = 2 * ceiling(log(real(size(arr), kind=DEFAULT_REAL)) / log(2.0_real64))

    call introsort_recurs_int32_hist(arr, 1, size(arr), 1, history)

    if (present(reverse)) then
      if (reverse) then
        call reverse_int32_hist(arr, history)
      end if
    end if
  end subroutine introsort_int32_hist

  recursive subroutine introsort_recurs_int32_hist(arr, left, right, depth, history)
    implicit none
    integer(int32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(in) :: depth
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT) :: mid
    integer(DEFAULT_INT) :: pivot_idx

    if (right - left + 1 <= 16) then
      call bisort_int32_hist(arr(left:right), history)
      return
    end if

    if (depth >= max_depth) then
      call hsort_int32_hist(arr(left:right), history)
      return
    end if


    if (left < right) then
#     ifdef IS_LARGE_ARRAY
      pivot_idx = tukeys_ninther_pivot_int32(arr, left, right)
#     else
      pivot_idx = median_of_three_pivot_int32(arr, left, right)
#     endif

      mid = partition_int32_hist(arr, left, right, arr(pivot_idx), history)

      call introsort_recurs_int32_hist(arr, left, mid, depth + 1, history)
      call introsort_recurs_int32_hist(arr, mid + 1, right, depth + 1, history)
    end if
  end subroutine introsort_recurs_int32_hist

  subroutine introsort_int64_hist(arr, history, reverse)
    implicit none
    integer(int64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    logical, intent(in), optional :: reverse

    max_depth = 2 * ceiling(log(real(size(arr), kind=DEFAULT_REAL)) / log(2.0_real64))

    call introsort_recurs_int64_hist(arr, 1, size(arr), 1, history)

    if (present(reverse)) then
      if (reverse) then
        call reverse_int64_hist(arr, history)
      end if
    end if
  end subroutine introsort_int64_hist

  recursive subroutine introsort_recurs_int64_hist(arr, left, right, depth, history)
    implicit none
    integer(int64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(in) :: depth
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT) :: mid
    integer(DEFAULT_INT) :: pivot_idx

    if (right - left + 1 <= 16) then
      call bisort_int64_hist(arr(left:right), history)
      return
    end if

    if (depth >= max_depth) then
      call hsort_int64_hist(arr(left:right), history)
      return
    end if


    if (left < right) then
#     ifdef IS_LARGE_ARRAY
      pivot_idx = tukeys_ninther_pivot_int64(arr, left, right)
#     else
      pivot_idx = median_of_three_pivot_int64(arr, left, right)
#     endif

      mid = partition_int64_hist(arr, left, right, arr(pivot_idx), history)

      call introsort_recurs_int64_hist(arr, left, mid, depth + 1, history)
      call introsort_recurs_int64_hist(arr, mid + 1, right, depth + 1, history)
    end if
  end subroutine introsort_recurs_int64_hist

  subroutine introsort_real32_hist(arr, history, reverse)
    implicit none
    real(real32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    logical, intent(in), optional :: reverse

    max_depth = 2 * ceiling(log(real(size(arr), kind=DEFAULT_REAL)) / log(2.0_real64))

    call introsort_recurs_real32_hist(arr, 1, size(arr), 1, history)

    if (present(reverse)) then
      if (reverse) then
        call reverse_real32_hist(arr, history)
      end if
    end if
  end subroutine introsort_real32_hist

  recursive subroutine introsort_recurs_real32_hist(arr, left, right, depth, history)
    implicit none
    real(real32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(in) :: depth
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT) :: mid
    integer(DEFAULT_INT) :: pivot_idx

    if (right - left + 1 <= 16) then
      call bisort_real32_hist(arr(left:right), history)
      return
    end if

    if (depth >= max_depth) then
      call hsort_real32_hist(arr(left:right), history)
      return
    end if


    if (left < right) then
#     ifdef IS_LARGE_ARRAY
      pivot_idx = tukeys_ninther_pivot_real32(arr, left, right)
#     else
      pivot_idx = median_of_three_pivot_real32(arr, left, right)
#     endif

      mid = partition_real32_hist(arr, left, right, arr(pivot_idx), history)

      call introsort_recurs_real32_hist(arr, left, mid, depth + 1, history)
      call introsort_recurs_real32_hist(arr, mid + 1, right, depth + 1, history)
    end if
  end subroutine introsort_recurs_real32_hist

  subroutine introsort_real64_hist(arr, history, reverse)
    implicit none
    real(real64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    logical, intent(in), optional :: reverse

    max_depth = 2 * ceiling(log(real(size(arr), kind=DEFAULT_REAL)) / log(2.0_real64))

    call introsort_recurs_real64_hist(arr, 1, size(arr), 1, history)

    if (present(reverse)) then
      if (reverse) then
        call reverse_real64_hist(arr, history)
      end if
    end if
  end subroutine introsort_real64_hist

  recursive subroutine introsort_recurs_real64_hist(arr, left, right, depth, history)
    implicit none
    real(real64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(in) :: depth
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT) :: mid
    integer(DEFAULT_INT) :: pivot_idx

    if (right - left + 1 <= 16) then
      call bisort_real64_hist(arr(left:right), history)
      return
    end if

    if (depth >= max_depth) then
      call hsort_real64_hist(arr(left:right), history)
      return
    end if


    if (left < right) then
#     ifdef IS_LARGE_ARRAY
      pivot_idx = tukeys_ninther_pivot_real64(arr, left, right)
#     else
      pivot_idx = median_of_three_pivot_real64(arr, left, right)
#     endif

      mid = partition_real64_hist(arr, left, right, arr(pivot_idx), history)

      call introsort_recurs_real64_hist(arr, left, mid, depth + 1, history)
      call introsort_recurs_real64_hist(arr, mid + 1, right, depth + 1, history)
    end if
  end subroutine introsort_recurs_real64_hist

  subroutine introsort_real128_hist(arr, history, reverse)
    implicit none
    real(real128), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    logical, intent(in), optional :: reverse

    max_depth = 2 * ceiling(log(real(size(arr), kind=DEFAULT_REAL)) / log(2.0_real64))

    call introsort_recurs_real128_hist(arr, 1, size(arr), 1, history)

    if (present(reverse)) then
      if (reverse) then
        call reverse_real128_hist(arr, history)
      end if
    end if
  end subroutine introsort_real128_hist

  recursive subroutine introsort_recurs_real128_hist(arr, left, right, depth, history)
    implicit none
    real(real128), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(in) :: depth
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT) :: mid
    integer(DEFAULT_INT) :: pivot_idx

    if (right - left + 1 <= 16) then
      call bisort_real128_hist(arr(left:right), history)
      return
    end if

    if (depth >= max_depth) then
      call hsort_real128_hist(arr(left:right), history)
      return
    end if


    if (left < right) then
#     ifdef IS_LARGE_ARRAY
      pivot_idx = tukeys_ninther_pivot_real128(arr, left, right)
#     else
      pivot_idx = median_of_three_pivot_real128(arr, left, right)
#     endif

      mid = partition_real128_hist(arr, left, right, arr(pivot_idx), history)

      call introsort_recurs_real128_hist(arr, left, mid, depth + 1, history)
      call introsort_recurs_real128_hist(arr, mid + 1, right, depth + 1, history)
    end if
  end subroutine introsort_recurs_real128_hist


  ! Median-of-Three
  pure function median_of_three_pivot_int8(arr, left, right) result(pivot_idx)
    integer(int8), intent(in) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_idx, mid
    integer(int8) :: a, b, c

    mid = left + (right - left) / 2

    a = arr(left)
    b = arr(mid)
    c = arr(right)

    if (a <= b) then
      if (b <= c) then
        pivot_idx = mid
      else if (a <= c) then
        pivot_idx = right
      else
        pivot_idx = left
      end if
    else
      if (a <= c) then
        pivot_idx = left
      else if (b <= c) then
        pivot_idx = right
      else
        pivot_idx = mid
      end if
    end if

  end function median_of_three_pivot_int8

  ! Tukey's Ninther for the large arrays
  pure function tukeys_ninther_pivot_int8(arr, left, right) result(pivot_idx)
    integer(int8), intent(in) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_idx, n, third
    integer :: m1, m2, m3

    n = right - left + 1
    third = n / 3

    ! Divide the array into three parts and find the median of three in each part
    m1 = median_of_three_idx_int8(arr, left, left + third/2, left + third)
    m2 = median_of_three_idx_int8(arr, left + third, left + third + third/2, left + 2*third)
    m3 = median_of_three_idx_int8(arr, left + 2*third, left + 2*third + (n - 2*third)/2, right)

    ! Find the median of the three medians
    pivot_idx = median_of_three_idx_int8(arr, m1, m2, m3)

  end function tukeys_ninther_pivot_int8

  ! Helper function: returns the index of the median of three indices
  pure function median_of_three_idx_int8(arr, i, j, k) result(idx)
    integer(int8), intent(in) :: arr(:)
    integer, intent(in) :: i, j, k
    integer :: idx
    integer(int8) :: a, b, c

    a = arr(i)
    b = arr(j)
    c = arr(k)

    if (a <= b) then
      if (b <= c) then
        idx = j
      else if (a <= c) then
        idx = k
      else
        idx = i
      end if
    else
      if (a <= c) then
        idx = i
      else if (b <= c) then
        idx = k
      else
        idx = j
      end if
    end if
  end function median_of_three_idx_int8
  ! Median-of-Three
  pure function median_of_three_pivot_int16(arr, left, right) result(pivot_idx)
    integer(int16), intent(in) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_idx, mid
    integer(int16) :: a, b, c

    mid = left + (right - left) / 2

    a = arr(left)
    b = arr(mid)
    c = arr(right)

    if (a <= b) then
      if (b <= c) then
        pivot_idx = mid
      else if (a <= c) then
        pivot_idx = right
      else
        pivot_idx = left
      end if
    else
      if (a <= c) then
        pivot_idx = left
      else if (b <= c) then
        pivot_idx = right
      else
        pivot_idx = mid
      end if
    end if

  end function median_of_three_pivot_int16

  ! Tukey's Ninther for the large arrays
  pure function tukeys_ninther_pivot_int16(arr, left, right) result(pivot_idx)
    integer(int16), intent(in) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_idx, n, third
    integer :: m1, m2, m3

    n = right - left + 1
    third = n / 3

    ! Divide the array into three parts and find the median of three in each part
    m1 = median_of_three_idx_int16(arr, left, left + third/2, left + third)
    m2 = median_of_three_idx_int16(arr, left + third, left + third + third/2, left + 2*third)
    m3 = median_of_three_idx_int16(arr, left + 2*third, left + 2*third + (n - 2*third)/2, right)

    ! Find the median of the three medians
    pivot_idx = median_of_three_idx_int16(arr, m1, m2, m3)

  end function tukeys_ninther_pivot_int16

  ! Helper function: returns the index of the median of three indices
  pure function median_of_three_idx_int16(arr, i, j, k) result(idx)
    integer(int16), intent(in) :: arr(:)
    integer, intent(in) :: i, j, k
    integer :: idx
    integer(int16) :: a, b, c

    a = arr(i)
    b = arr(j)
    c = arr(k)

    if (a <= b) then
      if (b <= c) then
        idx = j
      else if (a <= c) then
        idx = k
      else
        idx = i
      end if
    else
      if (a <= c) then
        idx = i
      else if (b <= c) then
        idx = k
      else
        idx = j
      end if
    end if
  end function median_of_three_idx_int16
  ! Median-of-Three
  pure function median_of_three_pivot_int32(arr, left, right) result(pivot_idx)
    integer(int32), intent(in) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_idx, mid
    integer(int32) :: a, b, c

    mid = left + (right - left) / 2

    a = arr(left)
    b = arr(mid)
    c = arr(right)

    if (a <= b) then
      if (b <= c) then
        pivot_idx = mid
      else if (a <= c) then
        pivot_idx = right
      else
        pivot_idx = left
      end if
    else
      if (a <= c) then
        pivot_idx = left
      else if (b <= c) then
        pivot_idx = right
      else
        pivot_idx = mid
      end if
    end if

  end function median_of_three_pivot_int32

  ! Tukey's Ninther for the large arrays
  pure function tukeys_ninther_pivot_int32(arr, left, right) result(pivot_idx)
    integer(int32), intent(in) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_idx, n, third
    integer :: m1, m2, m3

    n = right - left + 1
    third = n / 3

    ! Divide the array into three parts and find the median of three in each part
    m1 = median_of_three_idx_int32(arr, left, left + third/2, left + third)
    m2 = median_of_three_idx_int32(arr, left + third, left + third + third/2, left + 2*third)
    m3 = median_of_three_idx_int32(arr, left + 2*third, left + 2*third + (n - 2*third)/2, right)

    ! Find the median of the three medians
    pivot_idx = median_of_three_idx_int32(arr, m1, m2, m3)

  end function tukeys_ninther_pivot_int32

  ! Helper function: returns the index of the median of three indices
  pure function median_of_three_idx_int32(arr, i, j, k) result(idx)
    integer(int32), intent(in) :: arr(:)
    integer, intent(in) :: i, j, k
    integer :: idx
    integer(int32) :: a, b, c

    a = arr(i)
    b = arr(j)
    c = arr(k)

    if (a <= b) then
      if (b <= c) then
        idx = j
      else if (a <= c) then
        idx = k
      else
        idx = i
      end if
    else
      if (a <= c) then
        idx = i
      else if (b <= c) then
        idx = k
      else
        idx = j
      end if
    end if
  end function median_of_three_idx_int32
  ! Median-of-Three
  pure function median_of_three_pivot_int64(arr, left, right) result(pivot_idx)
    integer(int64), intent(in) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_idx, mid
    integer(int64) :: a, b, c

    mid = left + (right - left) / 2

    a = arr(left)
    b = arr(mid)
    c = arr(right)

    if (a <= b) then
      if (b <= c) then
        pivot_idx = mid
      else if (a <= c) then
        pivot_idx = right
      else
        pivot_idx = left
      end if
    else
      if (a <= c) then
        pivot_idx = left
      else if (b <= c) then
        pivot_idx = right
      else
        pivot_idx = mid
      end if
    end if

  end function median_of_three_pivot_int64

  ! Tukey's Ninther for the large arrays
  pure function tukeys_ninther_pivot_int64(arr, left, right) result(pivot_idx)
    integer(int64), intent(in) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_idx, n, third
    integer :: m1, m2, m3

    n = right - left + 1
    third = n / 3

    ! Divide the array into three parts and find the median of three in each part
    m1 = median_of_three_idx_int64(arr, left, left + third/2, left + third)
    m2 = median_of_three_idx_int64(arr, left + third, left + third + third/2, left + 2*third)
    m3 = median_of_three_idx_int64(arr, left + 2*third, left + 2*third + (n - 2*third)/2, right)

    ! Find the median of the three medians
    pivot_idx = median_of_three_idx_int64(arr, m1, m2, m3)

  end function tukeys_ninther_pivot_int64

  ! Helper function: returns the index of the median of three indices
  pure function median_of_three_idx_int64(arr, i, j, k) result(idx)
    integer(int64), intent(in) :: arr(:)
    integer, intent(in) :: i, j, k
    integer :: idx
    integer(int64) :: a, b, c

    a = arr(i)
    b = arr(j)
    c = arr(k)

    if (a <= b) then
      if (b <= c) then
        idx = j
      else if (a <= c) then
        idx = k
      else
        idx = i
      end if
    else
      if (a <= c) then
        idx = i
      else if (b <= c) then
        idx = k
      else
        idx = j
      end if
    end if
  end function median_of_three_idx_int64
  ! Median-of-Three
  pure function median_of_three_pivot_real32(arr, left, right) result(pivot_idx)
    real(real32), intent(in) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_idx, mid
    real(real32) :: a, b, c

    mid = left + (right - left) / 2

    a = arr(left)
    b = arr(mid)
    c = arr(right)

    if (a <= b) then
      if (b <= c) then
        pivot_idx = mid
      else if (a <= c) then
        pivot_idx = right
      else
        pivot_idx = left
      end if
    else
      if (a <= c) then
        pivot_idx = left
      else if (b <= c) then
        pivot_idx = right
      else
        pivot_idx = mid
      end if
    end if

  end function median_of_three_pivot_real32

  ! Tukey's Ninther for the large arrays
  pure function tukeys_ninther_pivot_real32(arr, left, right) result(pivot_idx)
    real(real32), intent(in) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_idx, n, third
    integer :: m1, m2, m3

    n = right - left + 1
    third = n / 3

    ! Divide the array into three parts and find the median of three in each part
    m1 = median_of_three_idx_real32(arr, left, left + third/2, left + third)
    m2 = median_of_three_idx_real32(arr, left + third, left + third + third/2, left + 2*third)
    m3 = median_of_three_idx_real32(arr, left + 2*third, left + 2*third + (n - 2*third)/2, right)

    ! Find the median of the three medians
    pivot_idx = median_of_three_idx_real32(arr, m1, m2, m3)

  end function tukeys_ninther_pivot_real32

  ! Helper function: returns the index of the median of three indices
  pure function median_of_three_idx_real32(arr, i, j, k) result(idx)
    real(real32), intent(in) :: arr(:)
    integer, intent(in) :: i, j, k
    integer :: idx
    real(real32) :: a, b, c

    a = arr(i)
    b = arr(j)
    c = arr(k)

    if (a <= b) then
      if (b <= c) then
        idx = j
      else if (a <= c) then
        idx = k
      else
        idx = i
      end if
    else
      if (a <= c) then
        idx = i
      else if (b <= c) then
        idx = k
      else
        idx = j
      end if
    end if
  end function median_of_three_idx_real32
  ! Median-of-Three
  pure function median_of_three_pivot_real64(arr, left, right) result(pivot_idx)
    real(real64), intent(in) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_idx, mid
    real(real64) :: a, b, c

    mid = left + (right - left) / 2

    a = arr(left)
    b = arr(mid)
    c = arr(right)

    if (a <= b) then
      if (b <= c) then
        pivot_idx = mid
      else if (a <= c) then
        pivot_idx = right
      else
        pivot_idx = left
      end if
    else
      if (a <= c) then
        pivot_idx = left
      else if (b <= c) then
        pivot_idx = right
      else
        pivot_idx = mid
      end if
    end if

  end function median_of_three_pivot_real64

  ! Tukey's Ninther for the large arrays
  pure function tukeys_ninther_pivot_real64(arr, left, right) result(pivot_idx)
    real(real64), intent(in) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_idx, n, third
    integer :: m1, m2, m3

    n = right - left + 1
    third = n / 3

    ! Divide the array into three parts and find the median of three in each part
    m1 = median_of_three_idx_real64(arr, left, left + third/2, left + third)
    m2 = median_of_three_idx_real64(arr, left + third, left + third + third/2, left + 2*third)
    m3 = median_of_three_idx_real64(arr, left + 2*third, left + 2*third + (n - 2*third)/2, right)

    ! Find the median of the three medians
    pivot_idx = median_of_three_idx_real64(arr, m1, m2, m3)

  end function tukeys_ninther_pivot_real64

  ! Helper function: returns the index of the median of three indices
  pure function median_of_three_idx_real64(arr, i, j, k) result(idx)
    real(real64), intent(in) :: arr(:)
    integer, intent(in) :: i, j, k
    integer :: idx
    real(real64) :: a, b, c

    a = arr(i)
    b = arr(j)
    c = arr(k)

    if (a <= b) then
      if (b <= c) then
        idx = j
      else if (a <= c) then
        idx = k
      else
        idx = i
      end if
    else
      if (a <= c) then
        idx = i
      else if (b <= c) then
        idx = k
      else
        idx = j
      end if
    end if
  end function median_of_three_idx_real64
  ! Median-of-Three
  pure function median_of_three_pivot_real128(arr, left, right) result(pivot_idx)
    real(real128), intent(in) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_idx, mid
    real(real128) :: a, b, c

    mid = left + (right - left) / 2

    a = arr(left)
    b = arr(mid)
    c = arr(right)

    if (a <= b) then
      if (b <= c) then
        pivot_idx = mid
      else if (a <= c) then
        pivot_idx = right
      else
        pivot_idx = left
      end if
    else
      if (a <= c) then
        pivot_idx = left
      else if (b <= c) then
        pivot_idx = right
      else
        pivot_idx = mid
      end if
    end if

  end function median_of_three_pivot_real128

  ! Tukey's Ninther for the large arrays
  pure function tukeys_ninther_pivot_real128(arr, left, right) result(pivot_idx)
    real(real128), intent(in) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_idx, n, third
    integer :: m1, m2, m3

    n = right - left + 1
    third = n / 3

    ! Divide the array into three parts and find the median of three in each part
    m1 = median_of_three_idx_real128(arr, left, left + third/2, left + third)
    m2 = median_of_three_idx_real128(arr, left + third, left + third + third/2, left + 2*third)
    m3 = median_of_three_idx_real128(arr, left + 2*third, left + 2*third + (n - 2*third)/2, right)

    ! Find the median of the three medians
    pivot_idx = median_of_three_idx_real128(arr, m1, m2, m3)

  end function tukeys_ninther_pivot_real128

  ! Helper function: returns the index of the median of three indices
  pure function median_of_three_idx_real128(arr, i, j, k) result(idx)
    real(real128), intent(in) :: arr(:)
    integer, intent(in) :: i, j, k
    integer :: idx
    real(real128) :: a, b, c

    a = arr(i)
    b = arr(j)
    c = arr(k)

    if (a <= b) then
      if (b <= c) then
        idx = j
      else if (a <= c) then
        idx = k
      else
        idx = i
      end if
    else
      if (a <= c) then
        idx = i
      else if (b <= c) then
        idx = k
      else
        idx = j
      end if
    end if
  end function median_of_three_idx_real128


  function partition_int8(arr, l, r, pivot) result(new_pivot_idx)
    implicit none
    integer(int8), intent(inout) :: arr(:)

    integer(DEFAULT_INT), intent(in) :: l, r
    integer(int8), intent(in) :: pivot
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: new_pivot_idx

    i = l; j = r
    do while (.true.)
      do while (arr(i) < pivot)
        i = i + 1
      end do
      do while (arr(j) > pivot)
        j = j - 1
      end do
      if (i >= j) exit

      call swap_int8(arr, i, j)
      i = i + 1
      j = j - 1
    end do

    new_pivot_idx = j
  end function partition_int8
  function partition_int16(arr, l, r, pivot) result(new_pivot_idx)
    implicit none
    integer(int16), intent(inout) :: arr(:)

    integer(DEFAULT_INT), intent(in) :: l, r
    integer(int16), intent(in) :: pivot
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: new_pivot_idx

    i = l; j = r
    do while (.true.)
      do while (arr(i) < pivot)
        i = i + 1
      end do
      do while (arr(j) > pivot)
        j = j - 1
      end do
      if (i >= j) exit

      call swap_int16(arr, i, j)
      i = i + 1
      j = j - 1
    end do

    new_pivot_idx = j
  end function partition_int16
  function partition_int32(arr, l, r, pivot) result(new_pivot_idx)
    implicit none
    integer(int32), intent(inout) :: arr(:)

    integer(DEFAULT_INT), intent(in) :: l, r
    integer(int32), intent(in) :: pivot
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: new_pivot_idx

    i = l; j = r
    do while (.true.)
      do while (arr(i) < pivot)
        i = i + 1
      end do
      do while (arr(j) > pivot)
        j = j - 1
      end do
      if (i >= j) exit

      call swap_int32(arr, i, j)
      i = i + 1
      j = j - 1
    end do

    new_pivot_idx = j
  end function partition_int32
  function partition_int64(arr, l, r, pivot) result(new_pivot_idx)
    implicit none
    integer(int64), intent(inout) :: arr(:)

    integer(DEFAULT_INT), intent(in) :: l, r
    integer(int64), intent(in) :: pivot
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: new_pivot_idx

    i = l; j = r
    do while (.true.)
      do while (arr(i) < pivot)
        i = i + 1
      end do
      do while (arr(j) > pivot)
        j = j - 1
      end do
      if (i >= j) exit

      call swap_int64(arr, i, j)
      i = i + 1
      j = j - 1
    end do

    new_pivot_idx = j
  end function partition_int64
  function partition_real32(arr, l, r, pivot) result(new_pivot_idx)
    implicit none
    real(real32), intent(inout) :: arr(:)

    integer(DEFAULT_INT), intent(in) :: l, r
    real(real32), intent(in) :: pivot
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: new_pivot_idx

    i = l; j = r
    do while (.true.)
      do while (arr(i) < pivot)
        i = i + 1
      end do
      do while (arr(j) > pivot)
        j = j - 1
      end do
      if (i >= j) exit

      call swap_real32(arr, i, j)
      i = i + 1
      j = j - 1
    end do

    new_pivot_idx = j
  end function partition_real32
  function partition_real64(arr, l, r, pivot) result(new_pivot_idx)
    implicit none
    real(real64), intent(inout) :: arr(:)

    integer(DEFAULT_INT), intent(in) :: l, r
    real(real64), intent(in) :: pivot
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: new_pivot_idx

    i = l; j = r
    do while (.true.)
      do while (arr(i) < pivot)
        i = i + 1
      end do
      do while (arr(j) > pivot)
        j = j - 1
      end do
      if (i >= j) exit

      call swap_real64(arr, i, j)
      i = i + 1
      j = j - 1
    end do

    new_pivot_idx = j
  end function partition_real64
  function partition_real128(arr, l, r, pivot) result(new_pivot_idx)
    implicit none
    real(real128), intent(inout) :: arr(:)

    integer(DEFAULT_INT), intent(in) :: l, r
    real(real128), intent(in) :: pivot
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: new_pivot_idx

    i = l; j = r
    do while (.true.)
      do while (arr(i) < pivot)
        i = i + 1
      end do
      do while (arr(j) > pivot)
        j = j - 1
      end do
      if (i >= j) exit

      call swap_real128(arr, i, j)
      i = i + 1
      j = j - 1
    end do

    new_pivot_idx = j
  end function partition_real128
  function partition_int8_hist(arr, l, r, pivot, history) result(new_pivot_idx)
    implicit none
    integer(int8), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT), intent(in) :: l, r
    integer(int8), intent(in) :: pivot
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: new_pivot_idx

    i = l; j = r
    do while (.true.)
      do while (arr(i) < pivot)
        i = i + 1
      end do
      do while (arr(j) > pivot)
        j = j - 1
      end do
      if (i >= j) exit

      call swap_int8_hist(arr, i, j, history)
      i = i + 1
      j = j - 1
    end do

    new_pivot_idx = j
  end function partition_int8_hist
  function partition_int16_hist(arr, l, r, pivot, history) result(new_pivot_idx)
    implicit none
    integer(int16), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT), intent(in) :: l, r
    integer(int16), intent(in) :: pivot
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: new_pivot_idx

    i = l; j = r
    do while (.true.)
      do while (arr(i) < pivot)
        i = i + 1
      end do
      do while (arr(j) > pivot)
        j = j - 1
      end do
      if (i >= j) exit

      call swap_int16_hist(arr, i, j, history)
      i = i + 1
      j = j - 1
    end do

    new_pivot_idx = j
  end function partition_int16_hist
  function partition_int32_hist(arr, l, r, pivot, history) result(new_pivot_idx)
    implicit none
    integer(int32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT), intent(in) :: l, r
    integer(int32), intent(in) :: pivot
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: new_pivot_idx

    i = l; j = r
    do while (.true.)
      do while (arr(i) < pivot)
        i = i + 1
      end do
      do while (arr(j) > pivot)
        j = j - 1
      end do
      if (i >= j) exit

      call swap_int32_hist(arr, i, j, history)
      i = i + 1
      j = j - 1
    end do

    new_pivot_idx = j
  end function partition_int32_hist
  function partition_int64_hist(arr, l, r, pivot, history) result(new_pivot_idx)
    implicit none
    integer(int64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT), intent(in) :: l, r
    integer(int64), intent(in) :: pivot
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: new_pivot_idx

    i = l; j = r
    do while (.true.)
      do while (arr(i) < pivot)
        i = i + 1
      end do
      do while (arr(j) > pivot)
        j = j - 1
      end do
      if (i >= j) exit

      call swap_int64_hist(arr, i, j, history)
      i = i + 1
      j = j - 1
    end do

    new_pivot_idx = j
  end function partition_int64_hist
  function partition_real32_hist(arr, l, r, pivot, history) result(new_pivot_idx)
    implicit none
    real(real32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT), intent(in) :: l, r
    real(real32), intent(in) :: pivot
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: new_pivot_idx

    i = l; j = r
    do while (.true.)
      do while (arr(i) < pivot)
        i = i + 1
      end do
      do while (arr(j) > pivot)
        j = j - 1
      end do
      if (i >= j) exit

      call swap_real32_hist(arr, i, j, history)
      i = i + 1
      j = j - 1
    end do

    new_pivot_idx = j
  end function partition_real32_hist
  function partition_real64_hist(arr, l, r, pivot, history) result(new_pivot_idx)
    implicit none
    real(real64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT), intent(in) :: l, r
    real(real64), intent(in) :: pivot
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: new_pivot_idx

    i = l; j = r
    do while (.true.)
      do while (arr(i) < pivot)
        i = i + 1
      end do
      do while (arr(j) > pivot)
        j = j - 1
      end do
      if (i >= j) exit

      call swap_real64_hist(arr, i, j, history)
      i = i + 1
      j = j - 1
    end do

    new_pivot_idx = j
  end function partition_real64_hist
  function partition_real128_hist(arr, l, r, pivot, history) result(new_pivot_idx)
    implicit none
    real(real128), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT), intent(in) :: l, r
    real(real128), intent(in) :: pivot
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: new_pivot_idx

    i = l; j = r
    do while (.true.)
      do while (arr(i) < pivot)
        i = i + 1
      end do
      do while (arr(j) > pivot)
        j = j - 1
      end do
      if (i >= j) exit

      call swap_real128_hist(arr, i, j, history)
      i = i + 1
      j = j - 1
    end do

    new_pivot_idx = j
  end function partition_real128_hist


  subroutine swap_int8(arr, i, j)
    implicit none
    integer(int8), intent(inout) :: arr(:)

    integer, intent(in) :: i, j
    integer(int8) :: temp

    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  end subroutine swap_int8
  subroutine swap_int16(arr, i, j)
    implicit none
    integer(int16), intent(inout) :: arr(:)

    integer, intent(in) :: i, j
    integer(int16) :: temp

    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  end subroutine swap_int16
  subroutine swap_int32(arr, i, j)
    implicit none
    integer(int32), intent(inout) :: arr(:)

    integer, intent(in) :: i, j
    integer(int32) :: temp

    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  end subroutine swap_int32
  subroutine swap_int64(arr, i, j)
    implicit none
    integer(int64), intent(inout) :: arr(:)

    integer, intent(in) :: i, j
    integer(int64) :: temp

    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  end subroutine swap_int64
  subroutine swap_real32(arr, i, j)
    implicit none
    real(real32), intent(inout) :: arr(:)

    integer, intent(in) :: i, j
    real(real32) :: temp

    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  end subroutine swap_real32
  subroutine swap_real64(arr, i, j)
    implicit none
    real(real64), intent(inout) :: arr(:)

    integer, intent(in) :: i, j
    real(real64) :: temp

    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  end subroutine swap_real64
  subroutine swap_real128(arr, i, j)
    implicit none
    real(real128), intent(inout) :: arr(:)

    integer, intent(in) :: i, j
    real(real128) :: temp

    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  end subroutine swap_real128
  subroutine swap_int8_hist(arr, i, j, history)
    implicit none
    integer(int8), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer, intent(in) :: i, j
    integer(int8) :: temp
    integer(DEFAULT_INT) :: temp_hist
    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
    temp_hist = history(i)
    history(i) = history(j)
    history(j) = temp_hist
  end subroutine swap_int8_hist
  subroutine swap_int16_hist(arr, i, j, history)
    implicit none
    integer(int16), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer, intent(in) :: i, j
    integer(int16) :: temp
    integer(DEFAULT_INT) :: temp_hist
    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
    temp_hist = history(i)
    history(i) = history(j)
    history(j) = temp_hist
  end subroutine swap_int16_hist
  subroutine swap_int32_hist(arr, i, j, history)
    implicit none
    integer(int32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer, intent(in) :: i, j
    integer(int32) :: temp
    integer(DEFAULT_INT) :: temp_hist
    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
    temp_hist = history(i)
    history(i) = history(j)
    history(j) = temp_hist
  end subroutine swap_int32_hist
  subroutine swap_int64_hist(arr, i, j, history)
    implicit none
    integer(int64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer, intent(in) :: i, j
    integer(int64) :: temp
    integer(DEFAULT_INT) :: temp_hist
    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
    temp_hist = history(i)
    history(i) = history(j)
    history(j) = temp_hist
  end subroutine swap_int64_hist
  subroutine swap_real32_hist(arr, i, j, history)
    implicit none
    real(real32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer, intent(in) :: i, j
    real(real32) :: temp
    integer(DEFAULT_INT) :: temp_hist
    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
    temp_hist = history(i)
    history(i) = history(j)
    history(j) = temp_hist
  end subroutine swap_real32_hist
  subroutine swap_real64_hist(arr, i, j, history)
    implicit none
    real(real64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer, intent(in) :: i, j
    real(real64) :: temp
    integer(DEFAULT_INT) :: temp_hist
    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
    temp_hist = history(i)
    history(i) = history(j)
    history(j) = temp_hist
  end subroutine swap_real64_hist
  subroutine swap_real128_hist(arr, i, j, history)
    implicit none
    real(real128), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer, intent(in) :: i, j
    real(real128) :: temp
    integer(DEFAULT_INT) :: temp_hist
    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
    temp_hist = history(i)
    history(i) = history(j)
    history(j) = temp_hist
  end subroutine swap_real128_hist


  subroutine bisort_int8(arr)
    implicit none
    integer(int8), intent(inout) :: arr(:)
    integer(DEFAULT_INT) :: l, r

    integer(int8) :: key
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: low, high, mid, loc

    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l + 1, r
      key = arr(i)
      low = l
      high = i - 1

      do while (low <= high)
        mid = low + (high - low) / 2
        if (key < arr(mid)) then
          high = mid - 1
        else
          low = mid + 1
        end if
      end do
      loc = low

      do j = i - 1, loc, -1
        arr(j + 1) = arr(j)

      end do
      arr(loc) = key

    end do
  end subroutine bisort_int8
  subroutine bisort_int16(arr)
    implicit none
    integer(int16), intent(inout) :: arr(:)
    integer(DEFAULT_INT) :: l, r

    integer(int16) :: key
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: low, high, mid, loc

    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l + 1, r
      key = arr(i)
      low = l
      high = i - 1

      do while (low <= high)
        mid = low + (high - low) / 2
        if (key < arr(mid)) then
          high = mid - 1
        else
          low = mid + 1
        end if
      end do
      loc = low

      do j = i - 1, loc, -1
        arr(j + 1) = arr(j)

      end do
      arr(loc) = key

    end do
  end subroutine bisort_int16
  subroutine bisort_int32(arr)
    implicit none
    integer(int32), intent(inout) :: arr(:)
    integer(DEFAULT_INT) :: l, r

    integer(int32) :: key
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: low, high, mid, loc

    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l + 1, r
      key = arr(i)
      low = l
      high = i - 1

      do while (low <= high)
        mid = low + (high - low) / 2
        if (key < arr(mid)) then
          high = mid - 1
        else
          low = mid + 1
        end if
      end do
      loc = low

      do j = i - 1, loc, -1
        arr(j + 1) = arr(j)

      end do
      arr(loc) = key

    end do
  end subroutine bisort_int32
  subroutine bisort_int64(arr)
    implicit none
    integer(int64), intent(inout) :: arr(:)
    integer(DEFAULT_INT) :: l, r

    integer(int64) :: key
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: low, high, mid, loc

    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l + 1, r
      key = arr(i)
      low = l
      high = i - 1

      do while (low <= high)
        mid = low + (high - low) / 2
        if (key < arr(mid)) then
          high = mid - 1
        else
          low = mid + 1
        end if
      end do
      loc = low

      do j = i - 1, loc, -1
        arr(j + 1) = arr(j)

      end do
      arr(loc) = key

    end do
  end subroutine bisort_int64
  subroutine bisort_real32(arr)
    implicit none
    real(real32), intent(inout) :: arr(:)
    integer(DEFAULT_INT) :: l, r

    real(real32) :: key
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: low, high, mid, loc

    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l + 1, r
      key = arr(i)
      low = l
      high = i - 1

      do while (low <= high)
        mid = low + (high - low) / 2
        if (key < arr(mid)) then
          high = mid - 1
        else
          low = mid + 1
        end if
      end do
      loc = low

      do j = i - 1, loc, -1
        arr(j + 1) = arr(j)

      end do
      arr(loc) = key

    end do
  end subroutine bisort_real32
  subroutine bisort_real64(arr)
    implicit none
    real(real64), intent(inout) :: arr(:)
    integer(DEFAULT_INT) :: l, r

    real(real64) :: key
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: low, high, mid, loc

    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l + 1, r
      key = arr(i)
      low = l
      high = i - 1

      do while (low <= high)
        mid = low + (high - low) / 2
        if (key < arr(mid)) then
          high = mid - 1
        else
          low = mid + 1
        end if
      end do
      loc = low

      do j = i - 1, loc, -1
        arr(j + 1) = arr(j)

      end do
      arr(loc) = key

    end do
  end subroutine bisort_real64
  subroutine bisort_real128(arr)
    implicit none
    real(real128), intent(inout) :: arr(:)
    integer(DEFAULT_INT) :: l, r

    real(real128) :: key
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: low, high, mid, loc

    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l + 1, r
      key = arr(i)
      low = l
      high = i - 1

      do while (low <= high)
        mid = low + (high - low) / 2
        if (key < arr(mid)) then
          high = mid - 1
        else
          low = mid + 1
        end if
      end do
      loc = low

      do j = i - 1, loc, -1
        arr(j + 1) = arr(j)

      end do
      arr(loc) = key

    end do
  end subroutine bisort_real128
  subroutine bisort_int8_hist(arr, history)
    implicit none
    integer(int8), intent(inout) :: arr(:)
    integer(DEFAULT_INT) :: l, r
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(int8) :: key
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: low, high, mid, loc

    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l + 1, r
      key = arr(i)
      low = l
      high = i - 1

      do while (low <= high)
        mid = low + (high - low) / 2
        if (key < arr(mid)) then
          high = mid - 1
        else
          low = mid + 1
        end if
      end do
      loc = low

      do j = i - 1, loc, -1
        arr(j + 1) = arr(j)
        history(j + 1) = history(j)
      end do
      arr(loc) = key
      history(loc) = i
    end do
  end subroutine bisort_int8_hist
  subroutine bisort_int16_hist(arr, history)
    implicit none
    integer(int16), intent(inout) :: arr(:)
    integer(DEFAULT_INT) :: l, r
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(int16) :: key
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: low, high, mid, loc

    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l + 1, r
      key = arr(i)
      low = l
      high = i - 1

      do while (low <= high)
        mid = low + (high - low) / 2
        if (key < arr(mid)) then
          high = mid - 1
        else
          low = mid + 1
        end if
      end do
      loc = low

      do j = i - 1, loc, -1
        arr(j + 1) = arr(j)
        history(j + 1) = history(j)
      end do
      arr(loc) = key
      history(loc) = i
    end do
  end subroutine bisort_int16_hist
  subroutine bisort_int32_hist(arr, history)
    implicit none
    integer(int32), intent(inout) :: arr(:)
    integer(DEFAULT_INT) :: l, r
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(int32) :: key
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: low, high, mid, loc

    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l + 1, r
      key = arr(i)
      low = l
      high = i - 1

      do while (low <= high)
        mid = low + (high - low) / 2
        if (key < arr(mid)) then
          high = mid - 1
        else
          low = mid + 1
        end if
      end do
      loc = low

      do j = i - 1, loc, -1
        arr(j + 1) = arr(j)
        history(j + 1) = history(j)
      end do
      arr(loc) = key
      history(loc) = i
    end do
  end subroutine bisort_int32_hist
  subroutine bisort_int64_hist(arr, history)
    implicit none
    integer(int64), intent(inout) :: arr(:)
    integer(DEFAULT_INT) :: l, r
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(int64) :: key
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: low, high, mid, loc

    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l + 1, r
      key = arr(i)
      low = l
      high = i - 1

      do while (low <= high)
        mid = low + (high - low) / 2
        if (key < arr(mid)) then
          high = mid - 1
        else
          low = mid + 1
        end if
      end do
      loc = low

      do j = i - 1, loc, -1
        arr(j + 1) = arr(j)
        history(j + 1) = history(j)
      end do
      arr(loc) = key
      history(loc) = i
    end do
  end subroutine bisort_int64_hist
  subroutine bisort_real32_hist(arr, history)
    implicit none
    real(real32), intent(inout) :: arr(:)
    integer(DEFAULT_INT) :: l, r
    integer(DEFAULT_INT), intent(inout) :: history(:)
    real(real32) :: key
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: low, high, mid, loc

    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l + 1, r
      key = arr(i)
      low = l
      high = i - 1

      do while (low <= high)
        mid = low + (high - low) / 2
        if (key < arr(mid)) then
          high = mid - 1
        else
          low = mid + 1
        end if
      end do
      loc = low

      do j = i - 1, loc, -1
        arr(j + 1) = arr(j)
        history(j + 1) = history(j)
      end do
      arr(loc) = key
      history(loc) = i
    end do
  end subroutine bisort_real32_hist
  subroutine bisort_real64_hist(arr, history)
    implicit none
    real(real64), intent(inout) :: arr(:)
    integer(DEFAULT_INT) :: l, r
    integer(DEFAULT_INT), intent(inout) :: history(:)
    real(real64) :: key
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: low, high, mid, loc

    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l + 1, r
      key = arr(i)
      low = l
      high = i - 1

      do while (low <= high)
        mid = low + (high - low) / 2
        if (key < arr(mid)) then
          high = mid - 1
        else
          low = mid + 1
        end if
      end do
      loc = low

      do j = i - 1, loc, -1
        arr(j + 1) = arr(j)
        history(j + 1) = history(j)
      end do
      arr(loc) = key
      history(loc) = i
    end do
  end subroutine bisort_real64_hist
  subroutine bisort_real128_hist(arr, history)
    implicit none
    real(real128), intent(inout) :: arr(:)
    integer(DEFAULT_INT) :: l, r
    integer(DEFAULT_INT), intent(inout) :: history(:)
    real(real128) :: key
    integer(DEFAULT_INT) :: i, j
    integer(DEFAULT_INT) :: low, high, mid, loc

    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l + 1, r
      key = arr(i)
      low = l
      high = i - 1

      do while (low <= high)
        mid = low + (high - low) / 2
        if (key < arr(mid)) then
          high = mid - 1
        else
          low = mid + 1
        end if
      end do
      loc = low

      do j = i - 1, loc, -1
        arr(j + 1) = arr(j)
        history(j + 1) = history(j)
      end do
      arr(loc) = key
      history(loc) = i
    end do
  end subroutine bisort_real128_hist


  subroutine hsort_int8(arr, reverse)
    implicit none
    integer(int8), intent(inout) :: arr(:)

    logical, intent(in), optional :: reverse

    call hsort_recurs_int8(arr, 1, size(arr))

    if (present(reverse)) then
      if (reverse) then
        call reverse_int8(arr)
      end if
    end if
  end subroutine hsort_int8

  recursive subroutine hsort_recurs_int8(arr, left, right)
    implicit none
    integer(int8), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right

    integer(DEFAULT_INT) :: i, n

    n = right - left + 1
    if (n <= 1) return

    ! Build heap (rearrange array)
    do i = n / 2, 1, -1
      call sift_down(i, n)
    end do

    ! One by one extract an element from heap
    do i = n, 2, -1
      ! Move current root to end
      call swap_int8(arr, left, left + i - 1)

      ! call max heapify on the reduced heap
      call sift_down(1, i - 1)
    end do

  contains
    subroutine sift_down(start_node, end_node)
      integer(DEFAULT_INT), intent(in) :: start_node, end_node
      integer(DEFAULT_INT) :: root, child, swap_idx

      root = start_node

      do while (2 * root <= end_node)
        child = 2 * root
        swap_idx = root

        if (arr(left + swap_idx - 1) < arr(left + child - 1)) then
          swap_idx = child
        end if

        if (child + 1 <= end_node) then
          if (arr(left + swap_idx - 1) < arr(left + child)) then
            swap_idx = child + 1
          end if
        end if

        if (swap_idx == root) then
          return
        else
          call swap_int8(arr, left + root - 1, left + swap_idx - 1)
          root = swap_idx
        end if
      end do
    end subroutine sift_down

  end subroutine hsort_recurs_int8

  subroutine hsort_int16(arr, reverse)
    implicit none
    integer(int16), intent(inout) :: arr(:)

    logical, intent(in), optional :: reverse

    call hsort_recurs_int16(arr, 1, size(arr))

    if (present(reverse)) then
      if (reverse) then
        call reverse_int16(arr)
      end if
    end if
  end subroutine hsort_int16

  recursive subroutine hsort_recurs_int16(arr, left, right)
    implicit none
    integer(int16), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right

    integer(DEFAULT_INT) :: i, n

    n = right - left + 1
    if (n <= 1) return

    ! Build heap (rearrange array)
    do i = n / 2, 1, -1
      call sift_down(i, n)
    end do

    ! One by one extract an element from heap
    do i = n, 2, -1
      ! Move current root to end
      call swap_int16(arr, left, left + i - 1)

      ! call max heapify on the reduced heap
      call sift_down(1, i - 1)
    end do

  contains
    subroutine sift_down(start_node, end_node)
      integer(DEFAULT_INT), intent(in) :: start_node, end_node
      integer(DEFAULT_INT) :: root, child, swap_idx

      root = start_node

      do while (2 * root <= end_node)
        child = 2 * root
        swap_idx = root

        if (arr(left + swap_idx - 1) < arr(left + child - 1)) then
          swap_idx = child
        end if

        if (child + 1 <= end_node) then
          if (arr(left + swap_idx - 1) < arr(left + child)) then
            swap_idx = child + 1
          end if
        end if

        if (swap_idx == root) then
          return
        else
          call swap_int16(arr, left + root - 1, left + swap_idx - 1)
          root = swap_idx
        end if
      end do
    end subroutine sift_down

  end subroutine hsort_recurs_int16

  subroutine hsort_int32(arr, reverse)
    implicit none
    integer(int32), intent(inout) :: arr(:)

    logical, intent(in), optional :: reverse

    call hsort_recurs_int32(arr, 1, size(arr))

    if (present(reverse)) then
      if (reverse) then
        call reverse_int32(arr)
      end if
    end if
  end subroutine hsort_int32

  recursive subroutine hsort_recurs_int32(arr, left, right)
    implicit none
    integer(int32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right

    integer(DEFAULT_INT) :: i, n

    n = right - left + 1
    if (n <= 1) return

    ! Build heap (rearrange array)
    do i = n / 2, 1, -1
      call sift_down(i, n)
    end do

    ! One by one extract an element from heap
    do i = n, 2, -1
      ! Move current root to end
      call swap_int32(arr, left, left + i - 1)

      ! call max heapify on the reduced heap
      call sift_down(1, i - 1)
    end do

  contains
    subroutine sift_down(start_node, end_node)
      integer(DEFAULT_INT), intent(in) :: start_node, end_node
      integer(DEFAULT_INT) :: root, child, swap_idx

      root = start_node

      do while (2 * root <= end_node)
        child = 2 * root
        swap_idx = root

        if (arr(left + swap_idx - 1) < arr(left + child - 1)) then
          swap_idx = child
        end if

        if (child + 1 <= end_node) then
          if (arr(left + swap_idx - 1) < arr(left + child)) then
            swap_idx = child + 1
          end if
        end if

        if (swap_idx == root) then
          return
        else
          call swap_int32(arr, left + root - 1, left + swap_idx - 1)
          root = swap_idx
        end if
      end do
    end subroutine sift_down

  end subroutine hsort_recurs_int32

  subroutine hsort_int64(arr, reverse)
    implicit none
    integer(int64), intent(inout) :: arr(:)

    logical, intent(in), optional :: reverse

    call hsort_recurs_int64(arr, 1, size(arr))

    if (present(reverse)) then
      if (reverse) then
        call reverse_int64(arr)
      end if
    end if
  end subroutine hsort_int64

  recursive subroutine hsort_recurs_int64(arr, left, right)
    implicit none
    integer(int64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right

    integer(DEFAULT_INT) :: i, n

    n = right - left + 1
    if (n <= 1) return

    ! Build heap (rearrange array)
    do i = n / 2, 1, -1
      call sift_down(i, n)
    end do

    ! One by one extract an element from heap
    do i = n, 2, -1
      ! Move current root to end
      call swap_int64(arr, left, left + i - 1)

      ! call max heapify on the reduced heap
      call sift_down(1, i - 1)
    end do

  contains
    subroutine sift_down(start_node, end_node)
      integer(DEFAULT_INT), intent(in) :: start_node, end_node
      integer(DEFAULT_INT) :: root, child, swap_idx

      root = start_node

      do while (2 * root <= end_node)
        child = 2 * root
        swap_idx = root

        if (arr(left + swap_idx - 1) < arr(left + child - 1)) then
          swap_idx = child
        end if

        if (child + 1 <= end_node) then
          if (arr(left + swap_idx - 1) < arr(left + child)) then
            swap_idx = child + 1
          end if
        end if

        if (swap_idx == root) then
          return
        else
          call swap_int64(arr, left + root - 1, left + swap_idx - 1)
          root = swap_idx
        end if
      end do
    end subroutine sift_down

  end subroutine hsort_recurs_int64

  subroutine hsort_real32(arr, reverse)
    implicit none
    real(real32), intent(inout) :: arr(:)

    logical, intent(in), optional :: reverse

    call hsort_recurs_real32(arr, 1, size(arr))

    if (present(reverse)) then
      if (reverse) then
        call reverse_real32(arr)
      end if
    end if
  end subroutine hsort_real32

  recursive subroutine hsort_recurs_real32(arr, left, right)
    implicit none
    real(real32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right

    integer(DEFAULT_INT) :: i, n

    n = right - left + 1
    if (n <= 1) return

    ! Build heap (rearrange array)
    do i = n / 2, 1, -1
      call sift_down(i, n)
    end do

    ! One by one extract an element from heap
    do i = n, 2, -1
      ! Move current root to end
      call swap_real32(arr, left, left + i - 1)

      ! call max heapify on the reduced heap
      call sift_down(1, i - 1)
    end do

  contains
    subroutine sift_down(start_node, end_node)
      integer(DEFAULT_INT), intent(in) :: start_node, end_node
      integer(DEFAULT_INT) :: root, child, swap_idx

      root = start_node

      do while (2 * root <= end_node)
        child = 2 * root
        swap_idx = root

        if (arr(left + swap_idx - 1) < arr(left + child - 1)) then
          swap_idx = child
        end if

        if (child + 1 <= end_node) then
          if (arr(left + swap_idx - 1) < arr(left + child)) then
            swap_idx = child + 1
          end if
        end if

        if (swap_idx == root) then
          return
        else
          call swap_real32(arr, left + root - 1, left + swap_idx - 1)
          root = swap_idx
        end if
      end do
    end subroutine sift_down

  end subroutine hsort_recurs_real32

  subroutine hsort_real64(arr, reverse)
    implicit none
    real(real64), intent(inout) :: arr(:)

    logical, intent(in), optional :: reverse

    call hsort_recurs_real64(arr, 1, size(arr))

    if (present(reverse)) then
      if (reverse) then
        call reverse_real64(arr)
      end if
    end if
  end subroutine hsort_real64

  recursive subroutine hsort_recurs_real64(arr, left, right)
    implicit none
    real(real64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right

    integer(DEFAULT_INT) :: i, n

    n = right - left + 1
    if (n <= 1) return

    ! Build heap (rearrange array)
    do i = n / 2, 1, -1
      call sift_down(i, n)
    end do

    ! One by one extract an element from heap
    do i = n, 2, -1
      ! Move current root to end
      call swap_real64(arr, left, left + i - 1)

      ! call max heapify on the reduced heap
      call sift_down(1, i - 1)
    end do

  contains
    subroutine sift_down(start_node, end_node)
      integer(DEFAULT_INT), intent(in) :: start_node, end_node
      integer(DEFAULT_INT) :: root, child, swap_idx

      root = start_node

      do while (2 * root <= end_node)
        child = 2 * root
        swap_idx = root

        if (arr(left + swap_idx - 1) < arr(left + child - 1)) then
          swap_idx = child
        end if

        if (child + 1 <= end_node) then
          if (arr(left + swap_idx - 1) < arr(left + child)) then
            swap_idx = child + 1
          end if
        end if

        if (swap_idx == root) then
          return
        else
          call swap_real64(arr, left + root - 1, left + swap_idx - 1)
          root = swap_idx
        end if
      end do
    end subroutine sift_down

  end subroutine hsort_recurs_real64

  subroutine hsort_real128(arr, reverse)
    implicit none
    real(real128), intent(inout) :: arr(:)

    logical, intent(in), optional :: reverse

    call hsort_recurs_real128(arr, 1, size(arr))

    if (present(reverse)) then
      if (reverse) then
        call reverse_real128(arr)
      end if
    end if
  end subroutine hsort_real128

  recursive subroutine hsort_recurs_real128(arr, left, right)
    implicit none
    real(real128), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right

    integer(DEFAULT_INT) :: i, n

    n = right - left + 1
    if (n <= 1) return

    ! Build heap (rearrange array)
    do i = n / 2, 1, -1
      call sift_down(i, n)
    end do

    ! One by one extract an element from heap
    do i = n, 2, -1
      ! Move current root to end
      call swap_real128(arr, left, left + i - 1)

      ! call max heapify on the reduced heap
      call sift_down(1, i - 1)
    end do

  contains
    subroutine sift_down(start_node, end_node)
      integer(DEFAULT_INT), intent(in) :: start_node, end_node
      integer(DEFAULT_INT) :: root, child, swap_idx

      root = start_node

      do while (2 * root <= end_node)
        child = 2 * root
        swap_idx = root

        if (arr(left + swap_idx - 1) < arr(left + child - 1)) then
          swap_idx = child
        end if

        if (child + 1 <= end_node) then
          if (arr(left + swap_idx - 1) < arr(left + child)) then
            swap_idx = child + 1
          end if
        end if

        if (swap_idx == root) then
          return
        else
          call swap_real128(arr, left + root - 1, left + swap_idx - 1)
          root = swap_idx
        end if
      end do
    end subroutine sift_down

  end subroutine hsort_recurs_real128

  subroutine hsort_int8_hist(arr, history, reverse)
    implicit none
    integer(int8), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    logical, intent(in), optional :: reverse

    call hsort_recurs_int8_hist(arr, 1, size(arr), history)

    if (present(reverse)) then
      if (reverse) then
        call reverse_int8_hist(arr, history)
      end if
    end if
  end subroutine hsort_int8_hist

  recursive subroutine hsort_recurs_int8_hist(arr, left, right, history)
    implicit none
    integer(int8), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT) :: i, n

    n = right - left + 1
    if (n <= 1) return

    ! Build heap (rearrange array)
    do i = n / 2, 1, -1
      call sift_down(i, n)
    end do

    ! One by one extract an element from heap
    do i = n, 2, -1
      ! Move current root to end
      call swap_int8_hist(arr, left, left + i - 1, history)

      ! call max heapify on the reduced heap
      call sift_down(1, i - 1)
    end do

  contains
    subroutine sift_down(start_node, end_node)
      integer(DEFAULT_INT), intent(in) :: start_node, end_node
      integer(DEFAULT_INT) :: root, child, swap_idx

      root = start_node

      do while (2 * root <= end_node)
        child = 2 * root
        swap_idx = root

        if (arr(left + swap_idx - 1) < arr(left + child - 1)) then
          swap_idx = child
        end if

        if (child + 1 <= end_node) then
          if (arr(left + swap_idx - 1) < arr(left + child)) then
            swap_idx = child + 1
          end if
        end if

        if (swap_idx == root) then
          return
        else
          call swap_int8_hist(arr, left + root - 1, left + swap_idx - 1, history)
          root = swap_idx
        end if
      end do
    end subroutine sift_down

  end subroutine hsort_recurs_int8_hist

  subroutine hsort_int16_hist(arr, history, reverse)
    implicit none
    integer(int16), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    logical, intent(in), optional :: reverse

    call hsort_recurs_int16_hist(arr, 1, size(arr), history)

    if (present(reverse)) then
      if (reverse) then
        call reverse_int16_hist(arr, history)
      end if
    end if
  end subroutine hsort_int16_hist

  recursive subroutine hsort_recurs_int16_hist(arr, left, right, history)
    implicit none
    integer(int16), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT) :: i, n

    n = right - left + 1
    if (n <= 1) return

    ! Build heap (rearrange array)
    do i = n / 2, 1, -1
      call sift_down(i, n)
    end do

    ! One by one extract an element from heap
    do i = n, 2, -1
      ! Move current root to end
      call swap_int16_hist(arr, left, left + i - 1, history)

      ! call max heapify on the reduced heap
      call sift_down(1, i - 1)
    end do

  contains
    subroutine sift_down(start_node, end_node)
      integer(DEFAULT_INT), intent(in) :: start_node, end_node
      integer(DEFAULT_INT) :: root, child, swap_idx

      root = start_node

      do while (2 * root <= end_node)
        child = 2 * root
        swap_idx = root

        if (arr(left + swap_idx - 1) < arr(left + child - 1)) then
          swap_idx = child
        end if

        if (child + 1 <= end_node) then
          if (arr(left + swap_idx - 1) < arr(left + child)) then
            swap_idx = child + 1
          end if
        end if

        if (swap_idx == root) then
          return
        else
          call swap_int16_hist(arr, left + root - 1, left + swap_idx - 1, history)
          root = swap_idx
        end if
      end do
    end subroutine sift_down

  end subroutine hsort_recurs_int16_hist

  subroutine hsort_int32_hist(arr, history, reverse)
    implicit none
    integer(int32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    logical, intent(in), optional :: reverse

    call hsort_recurs_int32_hist(arr, 1, size(arr), history)

    if (present(reverse)) then
      if (reverse) then
        call reverse_int32_hist(arr, history)
      end if
    end if
  end subroutine hsort_int32_hist

  recursive subroutine hsort_recurs_int32_hist(arr, left, right, history)
    implicit none
    integer(int32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT) :: i, n

    n = right - left + 1
    if (n <= 1) return

    ! Build heap (rearrange array)
    do i = n / 2, 1, -1
      call sift_down(i, n)
    end do

    ! One by one extract an element from heap
    do i = n, 2, -1
      ! Move current root to end
      call swap_int32_hist(arr, left, left + i - 1, history)

      ! call max heapify on the reduced heap
      call sift_down(1, i - 1)
    end do

  contains
    subroutine sift_down(start_node, end_node)
      integer(DEFAULT_INT), intent(in) :: start_node, end_node
      integer(DEFAULT_INT) :: root, child, swap_idx

      root = start_node

      do while (2 * root <= end_node)
        child = 2 * root
        swap_idx = root

        if (arr(left + swap_idx - 1) < arr(left + child - 1)) then
          swap_idx = child
        end if

        if (child + 1 <= end_node) then
          if (arr(left + swap_idx - 1) < arr(left + child)) then
            swap_idx = child + 1
          end if
        end if

        if (swap_idx == root) then
          return
        else
          call swap_int32_hist(arr, left + root - 1, left + swap_idx - 1, history)
          root = swap_idx
        end if
      end do
    end subroutine sift_down

  end subroutine hsort_recurs_int32_hist

  subroutine hsort_int64_hist(arr, history, reverse)
    implicit none
    integer(int64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    logical, intent(in), optional :: reverse

    call hsort_recurs_int64_hist(arr, 1, size(arr), history)

    if (present(reverse)) then
      if (reverse) then
        call reverse_int64_hist(arr, history)
      end if
    end if
  end subroutine hsort_int64_hist

  recursive subroutine hsort_recurs_int64_hist(arr, left, right, history)
    implicit none
    integer(int64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT) :: i, n

    n = right - left + 1
    if (n <= 1) return

    ! Build heap (rearrange array)
    do i = n / 2, 1, -1
      call sift_down(i, n)
    end do

    ! One by one extract an element from heap
    do i = n, 2, -1
      ! Move current root to end
      call swap_int64_hist(arr, left, left + i - 1, history)

      ! call max heapify on the reduced heap
      call sift_down(1, i - 1)
    end do

  contains
    subroutine sift_down(start_node, end_node)
      integer(DEFAULT_INT), intent(in) :: start_node, end_node
      integer(DEFAULT_INT) :: root, child, swap_idx

      root = start_node

      do while (2 * root <= end_node)
        child = 2 * root
        swap_idx = root

        if (arr(left + swap_idx - 1) < arr(left + child - 1)) then
          swap_idx = child
        end if

        if (child + 1 <= end_node) then
          if (arr(left + swap_idx - 1) < arr(left + child)) then
            swap_idx = child + 1
          end if
        end if

        if (swap_idx == root) then
          return
        else
          call swap_int64_hist(arr, left + root - 1, left + swap_idx - 1, history)
          root = swap_idx
        end if
      end do
    end subroutine sift_down

  end subroutine hsort_recurs_int64_hist

  subroutine hsort_real32_hist(arr, history, reverse)
    implicit none
    real(real32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    logical, intent(in), optional :: reverse

    call hsort_recurs_real32_hist(arr, 1, size(arr), history)

    if (present(reverse)) then
      if (reverse) then
        call reverse_real32_hist(arr, history)
      end if
    end if
  end subroutine hsort_real32_hist

  recursive subroutine hsort_recurs_real32_hist(arr, left, right, history)
    implicit none
    real(real32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT) :: i, n

    n = right - left + 1
    if (n <= 1) return

    ! Build heap (rearrange array)
    do i = n / 2, 1, -1
      call sift_down(i, n)
    end do

    ! One by one extract an element from heap
    do i = n, 2, -1
      ! Move current root to end
      call swap_real32_hist(arr, left, left + i - 1, history)

      ! call max heapify on the reduced heap
      call sift_down(1, i - 1)
    end do

  contains
    subroutine sift_down(start_node, end_node)
      integer(DEFAULT_INT), intent(in) :: start_node, end_node
      integer(DEFAULT_INT) :: root, child, swap_idx

      root = start_node

      do while (2 * root <= end_node)
        child = 2 * root
        swap_idx = root

        if (arr(left + swap_idx - 1) < arr(left + child - 1)) then
          swap_idx = child
        end if

        if (child + 1 <= end_node) then
          if (arr(left + swap_idx - 1) < arr(left + child)) then
            swap_idx = child + 1
          end if
        end if

        if (swap_idx == root) then
          return
        else
          call swap_real32_hist(arr, left + root - 1, left + swap_idx - 1, history)
          root = swap_idx
        end if
      end do
    end subroutine sift_down

  end subroutine hsort_recurs_real32_hist

  subroutine hsort_real64_hist(arr, history, reverse)
    implicit none
    real(real64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    logical, intent(in), optional :: reverse

    call hsort_recurs_real64_hist(arr, 1, size(arr), history)

    if (present(reverse)) then
      if (reverse) then
        call reverse_real64_hist(arr, history)
      end if
    end if
  end subroutine hsort_real64_hist

  recursive subroutine hsort_recurs_real64_hist(arr, left, right, history)
    implicit none
    real(real64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT) :: i, n

    n = right - left + 1
    if (n <= 1) return

    ! Build heap (rearrange array)
    do i = n / 2, 1, -1
      call sift_down(i, n)
    end do

    ! One by one extract an element from heap
    do i = n, 2, -1
      ! Move current root to end
      call swap_real64_hist(arr, left, left + i - 1, history)

      ! call max heapify on the reduced heap
      call sift_down(1, i - 1)
    end do

  contains
    subroutine sift_down(start_node, end_node)
      integer(DEFAULT_INT), intent(in) :: start_node, end_node
      integer(DEFAULT_INT) :: root, child, swap_idx

      root = start_node

      do while (2 * root <= end_node)
        child = 2 * root
        swap_idx = root

        if (arr(left + swap_idx - 1) < arr(left + child - 1)) then
          swap_idx = child
        end if

        if (child + 1 <= end_node) then
          if (arr(left + swap_idx - 1) < arr(left + child)) then
            swap_idx = child + 1
          end if
        end if

        if (swap_idx == root) then
          return
        else
          call swap_real64_hist(arr, left + root - 1, left + swap_idx - 1, history)
          root = swap_idx
        end if
      end do
    end subroutine sift_down

  end subroutine hsort_recurs_real64_hist

  subroutine hsort_real128_hist(arr, history, reverse)
    implicit none
    real(real128), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    logical, intent(in), optional :: reverse

    call hsort_recurs_real128_hist(arr, 1, size(arr), history)

    if (present(reverse)) then
      if (reverse) then
        call reverse_real128_hist(arr, history)
      end if
    end if
  end subroutine hsort_real128_hist

  recursive subroutine hsort_recurs_real128_hist(arr, left, right, history)
    implicit none
    real(real128), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(in) :: left, right
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer(DEFAULT_INT) :: i, n

    n = right - left + 1
    if (n <= 1) return

    ! Build heap (rearrange array)
    do i = n / 2, 1, -1
      call sift_down(i, n)
    end do

    ! One by one extract an element from heap
    do i = n, 2, -1
      ! Move current root to end
      call swap_real128_hist(arr, left, left + i - 1, history)

      ! call max heapify on the reduced heap
      call sift_down(1, i - 1)
    end do

  contains
    subroutine sift_down(start_node, end_node)
      integer(DEFAULT_INT), intent(in) :: start_node, end_node
      integer(DEFAULT_INT) :: root, child, swap_idx

      root = start_node

      do while (2 * root <= end_node)
        child = 2 * root
        swap_idx = root

        if (arr(left + swap_idx - 1) < arr(left + child - 1)) then
          swap_idx = child
        end if

        if (child + 1 <= end_node) then
          if (arr(left + swap_idx - 1) < arr(left + child)) then
            swap_idx = child + 1
          end if
        end if

        if (swap_idx == root) then
          return
        else
          call swap_real128_hist(arr, left + root - 1, left + swap_idx - 1, history)
          root = swap_idx
        end if
      end do
    end subroutine sift_down

  end subroutine hsort_recurs_real128_hist



  subroutine reverse_int8(arr)
    implicit none
    integer(int8), intent(inout) :: arr(:)

    integer :: l, r
    integer(int8) :: temp

    integer :: i
    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l, (r + l) / 2
      temp = arr(i)
      arr(i) = arr(r + l - i)
      arr(r + l - i) = temp
    end do
  end subroutine reverse_int8
  subroutine reverse_int16(arr)
    implicit none
    integer(int16), intent(inout) :: arr(:)

    integer :: l, r
    integer(int16) :: temp

    integer :: i
    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l, (r + l) / 2
      temp = arr(i)
      arr(i) = arr(r + l - i)
      arr(r + l - i) = temp
    end do
  end subroutine reverse_int16
  subroutine reverse_int32(arr)
    implicit none
    integer(int32), intent(inout) :: arr(:)

    integer :: l, r
    integer(int32) :: temp

    integer :: i
    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l, (r + l) / 2
      temp = arr(i)
      arr(i) = arr(r + l - i)
      arr(r + l - i) = temp
    end do
  end subroutine reverse_int32
  subroutine reverse_int64(arr)
    implicit none
    integer(int64), intent(inout) :: arr(:)

    integer :: l, r
    integer(int64) :: temp

    integer :: i
    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l, (r + l) / 2
      temp = arr(i)
      arr(i) = arr(r + l - i)
      arr(r + l - i) = temp
    end do
  end subroutine reverse_int64
  subroutine reverse_real32(arr)
    implicit none
    real(real32), intent(inout) :: arr(:)

    integer :: l, r
    real(real32) :: temp

    integer :: i
    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l, (r + l) / 2
      temp = arr(i)
      arr(i) = arr(r + l - i)
      arr(r + l - i) = temp
    end do
  end subroutine reverse_real32
  subroutine reverse_real64(arr)
    implicit none
    real(real64), intent(inout) :: arr(:)

    integer :: l, r
    real(real64) :: temp

    integer :: i
    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l, (r + l) / 2
      temp = arr(i)
      arr(i) = arr(r + l - i)
      arr(r + l - i) = temp
    end do
  end subroutine reverse_real64
  subroutine reverse_real128(arr)
    implicit none
    real(real128), intent(inout) :: arr(:)

    integer :: l, r
    real(real128) :: temp

    integer :: i
    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l, (r + l) / 2
      temp = arr(i)
      arr(i) = arr(r + l - i)
      arr(r + l - i) = temp
    end do
  end subroutine reverse_real128
  subroutine reverse_int8_hist(arr, history)
    implicit none
    integer(int8), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer :: l, r
    integer(int8) :: temp
    integer(DEFAULT_INT) :: temp_hist
    integer :: i
    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l, (r + l) / 2
      temp = arr(i)
      arr(i) = arr(r + l - i)
      arr(r + l - i) = temp
      temp_hist = history(i)
      history(i) = history(r + l - i)
      history(r + l - i) = temp_hist
    end do
  end subroutine reverse_int8_hist
  subroutine reverse_int16_hist(arr, history)
    implicit none
    integer(int16), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer :: l, r
    integer(int16) :: temp
    integer(DEFAULT_INT) :: temp_hist
    integer :: i
    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l, (r + l) / 2
      temp = arr(i)
      arr(i) = arr(r + l - i)
      arr(r + l - i) = temp
      temp_hist = history(i)
      history(i) = history(r + l - i)
      history(r + l - i) = temp_hist
    end do
  end subroutine reverse_int16_hist
  subroutine reverse_int32_hist(arr, history)
    implicit none
    integer(int32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer :: l, r
    integer(int32) :: temp
    integer(DEFAULT_INT) :: temp_hist
    integer :: i
    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l, (r + l) / 2
      temp = arr(i)
      arr(i) = arr(r + l - i)
      arr(r + l - i) = temp
      temp_hist = history(i)
      history(i) = history(r + l - i)
      history(r + l - i) = temp_hist
    end do
  end subroutine reverse_int32_hist
  subroutine reverse_int64_hist(arr, history)
    implicit none
    integer(int64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer :: l, r
    integer(int64) :: temp
    integer(DEFAULT_INT) :: temp_hist
    integer :: i
    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l, (r + l) / 2
      temp = arr(i)
      arr(i) = arr(r + l - i)
      arr(r + l - i) = temp
      temp_hist = history(i)
      history(i) = history(r + l - i)
      history(r + l - i) = temp_hist
    end do
  end subroutine reverse_int64_hist
  subroutine reverse_real32_hist(arr, history)
    implicit none
    real(real32), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer :: l, r
    real(real32) :: temp
    integer(DEFAULT_INT) :: temp_hist
    integer :: i
    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l, (r + l) / 2
      temp = arr(i)
      arr(i) = arr(r + l - i)
      arr(r + l - i) = temp
      temp_hist = history(i)
      history(i) = history(r + l - i)
      history(r + l - i) = temp_hist
    end do
  end subroutine reverse_real32_hist
  subroutine reverse_real64_hist(arr, history)
    implicit none
    real(real64), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer :: l, r
    real(real64) :: temp
    integer(DEFAULT_INT) :: temp_hist
    integer :: i
    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l, (r + l) / 2
      temp = arr(i)
      arr(i) = arr(r + l - i)
      arr(r + l - i) = temp
      temp_hist = history(i)
      history(i) = history(r + l - i)
      history(r + l - i) = temp_hist
    end do
  end subroutine reverse_real64_hist
  subroutine reverse_real128_hist(arr, history)
    implicit none
    real(real128), intent(inout) :: arr(:)
    integer(DEFAULT_INT), intent(inout) :: history(:)
    integer :: l, r
    real(real128) :: temp
    integer(DEFAULT_INT) :: temp_hist
    integer :: i
    l = lbound(arr, 1); r = ubound(arr, 1)
    do i = l, (r + l) / 2
      temp = arr(i)
      arr(i) = arr(r + l - i)
      arr(r + l - i) = temp
      temp_hist = history(i)
      history(i) = history(r + l - i)
      history(r + l - i) = temp_hist
    end do
  end subroutine reverse_real128_hist
end module intro_sort_mod
