program problem_3

use iso_fortran_env
use aoc_utilities

implicit none

character(len=:),allocatable :: line
integer :: iunit, n_lines, i, j, k, n_cols, jstart, jend
logical :: adjacent, tmp
integer(int64) :: isum, gear_ratio
character(len=1),dimension(:,:),allocatable :: array
integer(int64),dimension(:),allocatable :: ivals

! read file:
open(newunit=iunit, file='inputs/day3.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
line = read_line(iunit); n_cols = len(line)
rewind(iunit)
allocate(array(0:n_lines+1, 0:n_cols+1))
array = '.'  ! pad around with blanks to simplify logic below
do i = 1, n_lines
    line = read_line(iunit)
    do j = 1, n_cols
        array(i,j) = line(j:j)
    end do
end do
close(iunit)

!----------------- part 1 -----------------

isum = 0
do i = 1, n_lines
    ! is this character adjacent to a symbol?
    jend = 0
    do j = 1, n_cols
        if (is_not_number(array(i,j))) cycle
        if (j<=jend) cycle   ! skip the cols that we already have
        jend = 0
        adjacent =  is_symbol(array(i-1,j)  ) .or. &
                    is_symbol(array(i-1,j-1)) .or. &
                    is_symbol(array(i-1,j+1)) .or. &
                    is_symbol(array(i,j-1)  ) .or. &
                    is_symbol(array(i,j+1)  ) .or. &
                    is_symbol(array(i+1,j)  ) .or. &
                    is_symbol(array(i+1,j-1)) .or. &
                    is_symbol(array(i+1,j+1))
        if (adjacent) isum = isum + get_number(i,j) ! note: this sets jend
    end do
end do
write(*,*) '3a: sum :', isum

!----------------- part 2 -----------------

! now look for '*' and find adjacent numbers ....
isum = 0
do i = 1, n_lines
    do j = 1, n_cols
        if (array(i,j) == '*') then
            ! look for 2 adjacent numbers.
            if (allocated(ivals)) deallocate(ivals); allocate(ivals(0))
            ! above
            tmp = check(i-1,j) ! if only one on top
            if (.not. tmp) then
                tmp = check(i-1,j-1)
                tmp = check(i-1,j+1)
            end if
            tmp = check(i,j-1)    ! left and right
            tmp = check(i,j+1)
            ! below
            tmp = check(i+1,j) ! if only one below
            if (.not. tmp) then
                tmp = check(i+1,j-1)
                tmp = check(i+1,j+1)
            end if
            if (size(ivals) == 2) isum = isum + product(ivals) ! sum gear ratio
        end if
    end do

end do
write(*,*) '3b: result :', isum

contains

    logical function check(i,j)
        !! if the char is part of a number, then get it and append to ivals
        integer,intent(in) :: i,j
        check = is_number(array(i,j))
        if (check) ivals = [ivals, get_number(i,j)]
    end function check

    logical function is_symbol(c)
        character(len=1),intent(in) :: c
        is_symbol = is_not_number(c) .and. c /= '.'
    end function is_symbol

    integer(int64) function get_number(i,j)
        !! get the full number contining the character at i,j
        integer,intent(in) :: i,j
        jstart = j
        jend = j
        do
            if (array(i,jstart-1)=='.' .or. is_symbol(array(i,jstart-1))) exit
            jstart = jstart - 1
        end do
        do
            if (array(i,jend+1)=='.' .or. is_symbol(array(i,jend+1))) exit
            jend = jend + 1
        end do
        get_number = int(array(i, jstart:jend))
    end function get_number

end program problem_3