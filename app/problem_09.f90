program problem_9

use iso_fortran_env, ip => int64 ! use long ints
use aoc_utilities

implicit none

integer :: i, iunit, n_lines
integer(ip) :: isum

call clk%tic()

! read the data file:
! open(newunit=iunit, file='inputs/day9_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day9.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
isum = sum([(extrapolate(parse(read_line(iunit))), i = 1, n_lines)])
close(iunit)
write(*,*) '9a: sum: ', isum

open(newunit=iunit, file='inputs/day9.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
isum = sum([(extrapolate(reverse(parse(read_line(iunit)))), i = 1, n_lines)])
close(iunit)
write(*,*) '9b: sum: ', isum

call clk%toc('9')

contains

    pure function extrapolate(ivals) result(iextrap)
    !! extrapolate the next value in the sequence using
    !! a difference table. Straightfoward implemention:
    !! create the full table, and then evaluate it.
    integer(ip),dimension(:),intent(in) :: ivals
    integer(ip) :: iextrap
    integer :: i, n

    ! types for a difference table:
    type :: iline
        !! a line in a difference table
        integer(ip),dimension(:),allocatable :: vals
    end type iline
    type(iline),dimension(:),allocatable :: diff_table

    ! create the difference table:
    diff_table = [iline(ivals)]; n = 1
    do
        n = n + 1
        diff_table = [diff_table, iline(diff(diff_table(n-1)%vals))] ! next line is diff of previous line
        if (all(diff_table(n)%vals==0)) exit
    end do

    ! extrapolate
    iextrap = 0
    do i = n-1, 1, -1
        associate( ilast => diff_table(i)%vals(size(diff_table(i)%vals)) )
            iextrap = iextrap + ilast
        end associate
    end do

    end function extrapolate

    pure function diff(ivals) result(idiff)
        !! difference vector
        integer(ip),dimension(:),intent(in) :: ivals
        integer(ip),dimension(:),allocatable :: idiff
        integer :: i !! counter
        idiff = [(ivals(i+1) - ivals(i), i = 1, size(ivals)-1)]
    end function diff

end program problem_9