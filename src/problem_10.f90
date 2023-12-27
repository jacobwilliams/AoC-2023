program problem_10

use iso_fortran_env
use aoc_utilities

implicit none

integer :: i, j, nrows, ncols, imove, l , m
logical,dimension(:,:),allocatable :: icounts
character(len=1),dimension(:,:),allocatable :: array
integer,dimension(:,:),allocatable :: distance, distance_reverse
logical,dimension(:,:),allocatable :: visited
real(wp),dimension(:),allocatable :: x, y !! path cooidinates
integer,dimension(2) :: Sij !! i,j of the S char in array

call clk%tic()

array = read_file_to_char_array('inputs/day10.txt', '.') ! pad with . to simplify edge logic
nrows = size(array,1)
ncols = size(array,2)

allocate(distance(nrows,ncols)); distance = -1
allocate(distance_reverse(nrows,ncols)); distance_reverse = -1
allocate(visited(nrows,ncols))

! start at the S coordinate:
Sij = findloc(array, 'S')
x = [Sij(1)] ! to store the path for part b
y = [Sij(2)]

! traverse the maze:
visited = .false.; visited(Sij(1), Sij(2)) = .true.; distance(Sij(1), Sij(2)) = 0
do imove = 1, 4
    call move(Sij(1), Sij(2), imove, distance, .true.)
end do

! traverse the maze again in reverse:
visited = .false.; visited(Sij(1), Sij(2)) = .true.; distance_reverse(Sij(1), Sij(2)) = 0
do imove = 4, 1, -1
    call move(Sij(1), Sij(2), imove, distance_reverse, .false.)
end do
! where they match is the distance furthest away from the start
write(*,*) '10a: ', pack(distance, mask = (distance==distance_reverse .and. distance>0))

! for part b, use locpt to test all the points
! allow openmp to be used here to do each row in parallel
allocate(icounts(nrows,ncols)); icounts = .false.
!$OMP PARALLEL DO SHARED(icounts,x,y) PRIVATE(i,j,l,m)
do i = 2, nrows-1 ! we can skip the padding
    do j = 2, ncols-1
        if (any(i==x .and. j==y)) cycle ! skip if on path
        call locpt (real(i,wp), real(j,wp), x, y, size(x), l, m)
        if (l==1) icounts(i,j) = .true. ! if (i,j) is inside the polygonal path
    end do
end do
!$OMP END PARALLEL DO
write(*,*) '10b: ', count(icounts)

call clk%toc('10')

contains

    recursive subroutine move(i,j,direction,distance,save_path)
        integer,intent(in) :: i,j,direction
        integer,dimension(:,:),intent(inout) :: distance
        logical,intent(in) :: save_path !! to save the path coordinates

        integer :: inew, jnew, imove
        logical :: valid_move

        select case(direction)
        case(1); inew = i-1; jnew = j   ! north
        case(2); inew = i+1; jnew = j   ! south
        case(3); inew = i;   jnew = j+1 ! east
        case(4); inew = i;   jnew = j-1 ! west
        end select
        if (visited(inew,jnew) .or. array(inew,jnew)=='.') return

        ! can we move in this direction?
        valid_move = .false.
        associate (current_pipe     => array(i,j),      &
                   current_distance => distance(i,j),   &
                   move_to          => array(inew,jnew) )
            select case (current_pipe)
            case('S')
                ! don't know what the first pip is, so have to try them all
                select case(direction)
                case(1); valid_move = index(pipe_info(move_to),'S')>0  ! north
                case(2); valid_move = index(pipe_info(move_to),'N')>0  ! south
                case(3); valid_move = index(pipe_info(move_to),'W')>0 ! east
                case(4); valid_move = index(pipe_info(move_to),'E')>0 ! west
                end select
            case('|')
                select case(direction)
                case(1); valid_move = index(pipe_info(move_to),'S')>0  ! north
                case(2); valid_move = index(pipe_info(move_to),'N')>0  ! south
                end select
            case('-')
                select case(direction)
                case(3); valid_move = index(pipe_info(move_to),'W')>0 ! east
                case(4); valid_move = index(pipe_info(move_to),'E')>0 ! west
                end select
            case('L')
                select case(direction)
                case(1);  valid_move = index(pipe_info(move_to),'S')>0 ! north
                case(3);  valid_move = index(pipe_info(move_to),'W')>0 ! east
                end select
            case('J')
                select case(direction)
                case(1);  valid_move = index(pipe_info(move_to),'S')>0 ! north
                case(4);  valid_move = index(pipe_info(move_to),'E')>0! west
                end select
            case('7')
                select case(direction)
                case(2);  valid_move = index(pipe_info(move_to),'N')>0 ! south
                case(4);  valid_move = index(pipe_info(move_to),'E')>0 ! west
                end select
            case('F')
                select case(direction)
                case(2);  valid_move = index(pipe_info(move_to),'N')>0 ! south
                case(3);  valid_move = index(pipe_info(move_to),'W')>0 ! east
                end select
            end select

            if (valid_move) then
                distance(inew,jnew) = current_distance + 1
                visited(inew,jnew) = .true.
                do imove = 1, 4
                    call move(inew,jnew,imove,distance,save_path)
                end do
                if (save_path) then
                    x = [x, real(inew,wp)] ! save cordinates of point on the path
                    y = [y, real(jnew,wp)]
                end if
            end if

        end associate

    end subroutine move

    pure character(len=2) function pipe_info(p)
        character(len=1),intent(in) :: p
        select case (p)
        case('|'); pipe_info = 'NS'   ! | is a vertical pipe connecting north and south.
        case('-'); pipe_info = 'EW'   ! - is a horizontal pipe connecting east and west.
        case('L'); pipe_info = 'NE'   ! L is a 90-degree bend connecting north and east.
        case('J'); pipe_info = 'NW'   ! J is a 90-degree bend connecting north and west.
        case('7'); pipe_info = 'SW'   ! 7 is a 90-degree bend connecting south and west.
        case('F'); pipe_info = 'SE'   ! F is a 90-degree bend connecting south and east.
        end select
    end function pipe_info

end program problem_10