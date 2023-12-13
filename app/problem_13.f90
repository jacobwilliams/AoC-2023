program problem_13

use iso_fortran_env, ip => int64, wp => real64
use aoc_utilities

implicit none

integer :: iunit, iline, n_lines, ival, isum
type(string),dimension(:),allocatable :: lines
character(len=:),allocatable :: line

integer,parameter :: ASH = 0  !! .
integer,parameter :: ROCK = 1 !! #

call clk%tic()

! open(newunit=iunit, file='inputs/day13_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day13.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
isum = 0
allocate(lines(0))
do iline = 1, n_lines
    line = read_line(iunit)
    !write(*,*) line
    if (line == '' .or. iline == n_lines) then
        ! process this puzzle
        ival = go(lines)
        isum = isum + ival
        if (allocated(lines)) then
            deallocate(lines); allocate(lines(0))
        end if
    else
        ! accumulate this line in the current puzzle
        lines = [lines, string(line)]
    end if
end do
write(*,*) '13a: ',isum

call clk%toc('13')

contains

    integer function go(lines)
    type(string),dimension(:),intent(in) :: lines

    integer,dimension(:,:),allocatable :: puzzle
    integer :: nrows, ncols, i, j, ileft, iabove
    logical :: mirror

    nrows = size(lines)
    ncols = len(lines(1)%str) ! assumed to all be the same size

    ! convert the input to a numeric matrix:
    allocate(puzzle(nrows, ncols))
    do i = 1, nrows
        puzzle(i,:) = str_to_int_array_with_mapping(lines(i)%str, ['.','#'], [ASH,ROCK])
        !write(*,'(*(I1))') puzzle(i,:)
    end do

    ! process this puzzle:

    ! add up the number of columns to the left of each vertical line of reflection;
    ! to that, also add 100 multiplied by the number of rows above each horizontal
    ! line of reflection

    ! rows:
    ileft = 0
    do i = 1, ncols-1
        mirror = .true.
        do j = 1, min(i, ncols-i)
            if (any( puzzle(:,i-j+1) /= puzzle(:,i+j)) ) then ! no mirror on this line
                mirror = .false.
                exit
            end if
        end do
        if (mirror) then
            ileft = i
            exit
        end if
    end do

    ! rows:
    iabove = 0
    if (ileft==0) then
        do i = 1, nrows-1
            mirror = .true.
            do j = 1, min(i, nrows-i)
                !write(*,*) '1'
                if (any( puzzle(i-j+1,:) /= puzzle(i+j,:)) ) then ! no mirror on this line
                    mirror = .false.
                    exit
                end if
            end do
            if (mirror) then
                iabove = i
                exit
            end if
        end do
    end if

    go = ileft + 100*iabove

    end function go

end program problem_13