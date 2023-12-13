program problem_13

use iso_fortran_env, ip => int64, wp => real64
use aoc_utilities

implicit none

integer :: i, iunit, iline, n_lines, ival, isum, ival2, isum2, &
           m, n, nrows, ncols, ileft, iabove, ileft2, iabove2
type(string),dimension(:),allocatable :: lines
integer,dimension(:,:),allocatable:: puzzle, puzzle2
character(len=:),allocatable :: line

integer,parameter :: ASH = 0  !! .
integer,parameter :: ROCK = 1 !! #

call clk%tic()

! open(newunit=iunit, file='inputs/day13_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day13.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
isum = 0
isum2 = 0
allocate(lines(0))
do iline = 1, n_lines
    line = read_line(iunit)
    if (line == '' .or. iline == n_lines) then
        ! process this puzzle
        ! convert the puzzle to a numeric matrix:
        if (allocated(puzzle)) deallocate(puzzle)
        nrows = size(lines)
        ncols = len(lines(1)%str)
        allocate(puzzle(nrows,ncols))
        do i = 1, nrows
            puzzle(i,:) = str_to_int_array_with_mapping(lines(i)%str, ['.','#'], [ASH,ROCK])
        end do

        ! part 1
        call go(puzzle, ileft, iabove)
        ival = ileft + 100*iabove
        isum = isum + ival

        ! part 2
        ! permute each with a smudge and find the different non-zero value
        main : do m = 1, nrows
            do n = 1, ncols
                puzzle2 = puzzle
                if (puzzle(m,n)==ASH) then
                    puzzle2(m,n) = ROCK
                else
                    puzzle2(m,n) = ASH
                end if

                ! don't consider the one from part a (since that one still may be valid)
                call go(puzzle2, ileft2, iabove2, ileft_skip=ileft, iabove_skip=iabove)

                if ((ileft2/=ileft .or. iabove2/=iabove) .and. (ileft2/=0 .or. iabove2/=0)) then
                    ival = ileft2 + 100*iabove2
                    exit main
                end if
            end do
        end do main
        isum2 = isum2 + ival

        if (allocated(lines)) then
            deallocate(lines); allocate(lines(0))
        end if

    else
        ! accumulate this line in the current puzzle
        lines = [lines, string(line)]
    end if
end do

write(*,*) '13a: ',isum
write(*,*) '13b: ',isum2

call clk%toc('13')

contains

    subroutine go(puzzle, ileft, iabove, ileft_skip, iabove_skip)
    integer,dimension(:,:),intent(in) :: puzzle
    integer,intent(out) :: ileft, iabove
    integer,intent(in),optional :: ileft_skip, iabove_skip

    integer :: nrows, ncols, i, j
    logical :: mirror

    nrows = size(puzzle,1)
    ncols = size(puzzle,2)

    ! add up the number of columns to the left of each vertical line of reflection;
    ! to that, also add 100 multiplied by the number of rows above each horizontal
    ! line of reflection

    ! rows:
    ileft = 0
    do i = 1, ncols-1
        if (present(ileft_skip)) then
            if (i==ileft_skip) cycle
        end if
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
            if (present(iabove_skip)) then
                if (i==iabove_skip) cycle
            end if
            mirror = .true.
            do j = 1, min(i, nrows-i)
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

    end subroutine go

end program problem_13