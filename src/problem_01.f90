program problem_1

use iso_fortran_env
use aoc_utilities

implicit none

integer :: iunit
logical :: status_ok
character(len=:),allocatable :: line
integer :: n_lines
integer :: i, j, k, n, isum, ifirst, ilast, idxfirst, idxlast

character(len=*),dimension(9),parameter :: ichars = [ &
    'one  ', &
    'two  ', &
    'three', &
    'four ', &
    'five ', &
    'six  ', &
    'seven', &
    'eight', &
    'nine ']

call clk%tic()

isum = 0
open(newunit=iunit, file='inputs/day1.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
do i = 1, n_lines

    ! remove all the chars before the first and after the last number
    line = read_line(iunit,status_ok)
    n = len(line)
    do j = 1, n
        if (line(j:j)>='1' .and. line(j:j)<='9') exit ! done
        line(j:j) = ' '
    end do

    do j = n, 1, -1
        if (line(j:j)>='1' .and. line(j:j)<='9') exit ! done
        line(j:j) = ' '
    end do

    line = trim(adjustl(line))
    n = len(line)
    isum  = isum + 10*int(line(1:1)) + int(line(n:n))

end do
write(*,*) '1a: sum:', isum
close(iunit)

! -------- part 2 -----------

open(newunit=iunit, file='inputs/day1.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
isum = 0
do i = 1, n_lines

    line = read_line(iunit,status_ok)
    n = len(line)

    ! keep track of the index of the first and last number (digit or string)
    ifirst = huge(1)
    idxfirst = huge(1)
    do j = 1, size(ichars)
        k = min( index(line,trim(ichars(j))), index(line,trim(ichars(j)),back=.true.) )  ! accounts for duplicates
        if (k>0 .and. k<=idxfirst) then
            ifirst = j
            idxfirst = k
        end if
    end do
    do j = 1, n
        if (line(j:j)>='1' .and. line(j:j)<='9') then
            if (j <= idxfirst) then
                ifirst = int(line(j:j))
                idxfirst = j
            end if
        end if
    end do

    ilast = 0
    idxlast = 0
    do j = 1, size(ichars)
        k = max( index(line,trim(ichars(j))), index(line,trim(ichars(j)), back=.true.) )  ! accounts for duplicates
        if (k>0 .and. k>=idxlast) then
            ilast = j
            idxlast = k
        end if
    end do
    do j = 1, n
        if (line(j:j)>='1' .and. line(j:j)<='9') then
            if (j >= idxlast) then
                ilast = int(line(j:j))
                idxlast = j
            end if
        end if
    end do

    isum  = isum + 10*ifirst + ilast

end do
write(*,*) '1b: sum:', isum

call clk%toc('1')

end program problem_1