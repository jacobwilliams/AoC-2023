program problem_3

use iso_fortran_env
use aoc_utilities

implicit none

integer :: n_lines, i, j, n_cols, jstart, jend
logical :: adjacent, tmp
integer(int64) :: isum
character(len=1),dimension(:,:),allocatable :: array
integer(int64),dimension(:),allocatable :: ivals

call clk%tic()

! read file:
! pad around with blanks to simplify logic below
array = read_file_to_char_array('inputs/day3.txt', border = '.')
n_lines = size(array,1)
n_cols  = size(array,2)

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

call clk%toc()

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