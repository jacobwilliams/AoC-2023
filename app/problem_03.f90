program problem_3

use iso_fortran_env
use aoc_utilities

implicit none

character(len=:),allocatable :: line
integer :: iunit, n_lines, i, j, k, n_cols, jstart, jend, icount
logical :: adjacent
integer(int64) :: isum, gear_ratio
character(len=1),dimension(:,:),allocatable :: array
integer(int64),dimension(2) :: ivals

character(len=*),parameter :: digits = '0123456789'

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
            icount = 0
            ! above
            if (is_number(array(i-1,j))) then
                ! only one on top:
                icount = icount + 1; if (icount>2) cycle
                ivals(icount) = get_number(i-1,j)
            else
                if (is_number(array(i-1,j-1))) then
                    icount = icount + 1; if (icount>2) cycle
                    ivals(icount) = get_number(i-1,j-1)
                end if
                if (is_number(array(i-1,j+1))) then
                    icount = icount + 1; if (icount>2) cycle
                    ivals(icount) = get_number(i-1,j+1)
                end if
            end if
            ! left and right
            if (is_number(array(i,j-1))) then
                icount = icount + 1; if (icount>2) cycle
                ivals(icount) = get_number(i,j-1)
            end if
            if (is_number(array(i,j+1))) then
                icount = icount + 1; if (icount>2) cycle
                ivals(icount) = get_number(i,j+1)
            end if
            ! below
            if (is_number(array(i+1,j))) then
                ! only one below
                icount = icount + 1; if (icount>2) cycle
                ivals(icount) = get_number(i+1,j)
            else
                if (is_number(array(i+1,j-1))) then
                    icount = icount + 1; if (icount>2) cycle
                    ivals(icount) = get_number(i+1,j-1)
                end if
                if (is_number(array(i+1,j+1))) then
                    icount = icount + 1; if (icount>2) cycle
                    ivals(icount) = get_number(i+1,j+1)
                end if
            end if
            if (icount == 2) then
                gear_ratio = product(ivals)
                isum = isum + gear_ratio
            end if

        end if
    end do

end do
write(*,*) '3b: result :', isum

contains

    logical function is_number(c)
    character(len=1),intent(in) :: c
    is_number = scan(c,digits)==1
    end function is_number

    logical function is_not_number(c)
    character(len=1),intent(in) :: c
    is_not_number = .not. is_number(c)
    end function is_not_number

    logical function is_symbol(c)
    character(len=1),intent(in) :: c
    is_symbol = is_not_number(c) .and. c /= '.'
    end function is_symbol

    integer(int64) function get_number(i,j)
        integer,intent(in) :: i,j
        character(len=:),allocatable :: str
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
        str = ''
        do k = jstart, jend
            str = str//array(i,k)
        end do
        get_number = int(str)
    end function get_number

end program problem_3