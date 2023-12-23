program problem_23

use iso_fortran_env, only: ip => int64 ! use long ints
use aoc_utilities

implicit none

integer(ip) :: nrows, ncols
character(len=1),dimension(:,:),allocatable :: array
logical,dimension(:,:),allocatable :: visited
integer(ip),dimension(1) :: iloc
integer(ip) :: istart, iend, max_dist

call clk%tic()

! read the data file:
! array = read_file_to_char_array('inputs/day23_test.txt') ! pad
array = read_file_to_char_array('inputs/day23.txt')
nrows = size(array,1)
ncols = size(array,2)
allocate(visited(nrows, ncols))
iloc = findloc(array(1,:), '.');     istart = iloc(1) ! get start and end columns
iloc = findloc(array(nrows,:), '.'); iend   = iloc(1) !

max_dist = 0 ! global: max distance so far
visited = .false.
call traverse(1_ip,istart,0_ip,visited)

write(*,*) '23a: ', max_dist

call clk%toc('23')

contains

    recursive subroutine traverse(i,j,idist,visited)
        integer(ip),intent(in) :: i,j !! current position
        integer(ip),intent(in) :: idist !! current distance (number of steps)
        logical,dimension(:,:),intent(in) :: visited !! elements visited (not counting this one)

        logical,dimension(:,:),allocatable :: tmp_visited

        if (i<1 .or. i>nrows .or. j<1 .or. j>ncols) return
        if (array(i,j)=='#') return! can't continue from here
        if (visited(i,j)) return

        ! make a copy and mark this one as visited [not super efficient]
        tmp_visited = visited; tmp_visited(i,j) = .true.

        ! paths (.), forest (#), and steep slopes (^, >, v, and <).
        select case (array(i,j))
        case ('.') ! path
            if (i==nrows .and. j==iend) then ! reached the destination
                if (idist>max_dist) max_dist = idist ! best so far
            else
                ! continue in any direction
                call traverse(i+1,j,idist+1,tmp_visited)
                call traverse(i-1,j,idist+1,tmp_visited)
                call traverse(i,j+1,idist+1,tmp_visited)
                call traverse(i,j-1,idist+1,tmp_visited)
            end if
        case('^'); call traverse(i-1,j,idist+1,tmp_visited)
        case('v'); call traverse(i+1,j,idist+1,tmp_visited)
        case('>'); call traverse(i,j+1,idist+1,tmp_visited)
        case('<'); call traverse(i,j-1,idist+1,tmp_visited)
        end select
    end subroutine traverse

end program problem_23