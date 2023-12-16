program problem_16

use iso_fortran_env, only: ip => int64 ! use long ints
use aoc_utilities

implicit none

integer :: i, j, nrows, ncols
character(len=1),dimension(:,:),allocatable :: array
logical,dimension(:,:,:),allocatable :: visited ! top,bottom,left,right

integer,parameter :: TOP = 1
integer,parameter :: BOTTOM = 2
integer,parameter :: LEFT = 3
integer,parameter :: RIGHT = 4

call clk%tic()

! read the data file:

! array = read_file_to_char_array('inputs/day16_test.txt') ! pad with wall around the array
array = read_file_to_char_array('inputs/day16.txt')
nrows = size(array,1)
ncols = size(array,2)

! track if we have already done this case, to avoid unnecessary duplication
allocate(visited(4, nrows, ncols))

visited = .false.
call go(1,1,LEFT)
write(*,*) '16a: ', count(visited(1,:,:) .or. &
                          visited(2,:,:) .or. &
                          visited(3,:,:) .or. &
                          visited(4,:,:))

call clk%toc('16')



contains

    recursive subroutine go(i,j,direction_from)
        integer,intent(in) :: i, j !! grid coordinate
        integer,intent(in) :: direction_from !! direction coming from (TOP,BOTTOM,LEFT,RIGHT

        integer :: k

        if (i>nrows .or. i<1 .or. j>ncols .or. j<1) return ! off the board
        if (visited(direction_from, i, j)) return ! this case has already been done

        visited(direction_from, i, j) = .true. ! mark this one as visited in this direction

        associate (c => array(i,j))
            select case (c)
            case('.') ! continue in same direction
                k = 1
                do
                    ! loop until we hit something else
                    ! (to avoid unnecessary recursion)
                    select case (direction_from)
                    case(TOP)  ! V
                        if ((i+k)>nrows) return
                        if (array(i+k,j)/='.') then
                            call go(i+k,j,direction_from)
                            return
                        end if
                        visited(direction_from, i+k,j) = .true.
                    case(BOTTOM)  ! ^
                        if ((i-k)<1) return
                        if (array(i-k,j)/='.') then
                            call go(i-k,j,direction_from)
                            return
                        end if
                        visited(direction_from, i-k,j) = .true.
                    case(LEFT)  ! -->
                        if ((j+k)>ncols) return
                        if (array(i,j+k)/='.') then
                            call go(i,j+k,direction_from)
                            return
                        end if
                        visited(direction_from, i,j+k) = .true.
                    case(RIGHT)  ! <--
                        if ((j+k)<1) return
                        if (array(i,j-k)/='.') then
                            call go(i,j-k,direction_from)
                            return
                        end if
                        visited(direction_from, i,j-k) = .true.
                    end select
                    k = k + 1
                end do
            case('-')
                select case (direction_from)
                case(TOP,BOTTOM)  ! V, ^
                    call go(i,j+1,LEFT) ! split
                    call go(i,j-1,RIGHT)
                case(LEFT)  ! -->
                    call go(i,j+1,LEFT)
                case(RIGHT)  ! <--
                    call go(i,j-1,RIGHT)
               end select
            case('|')
                select case (direction_from)
                case(TOP)  ! V
                    call go(i+1,j,TOP)
                case(BOTTOM)  ! ^
                    call go(i-1,j,BOTTOM)
                case(LEFT,RIGHT)  ! -->, <--
                    call go(i+1,j,TOP)
                    call go(i-1,j,BOTTOM)
                end select
            case('/')
                select case (direction_from)
                case(TOP)  ! V
                    call go(i,j-1,RIGHT)
                case(BOTTOM)  ! ^
                    call go(i,j+1,LEFT)
                case(LEFT)  ! -->
                    call go(i-1,j,BOTTOM)
                case(RIGHT)  ! <--
                    call go(i+1,j,TOP)
                end select
            case('\')
                select case (direction_from)
                case(TOP)  ! V
                    call go(i,j+1,LEFT)
                case(BOTTOM)  ! ^
                    call go(i,j-1,RIGHT)
                case(LEFT)  ! -->
                    call go(i+1,j,TOP)
                case(RIGHT)  ! <--
                    call go(i-1,j,BOTTOM)
                end select
            end select
        end associate

    end subroutine go

end program problem_16