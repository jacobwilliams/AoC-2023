program problem_10

use iso_fortran_env, ip => int64, wp => real64
use aoc_utilities

implicit none

integer :: i, j, nrows, ncols, istart, jstart, imove, l , m
integer(ip) :: icount
character(len=1),dimension(:,:),allocatable :: array
integer,dimension(:,:),allocatable :: distance, distance_reverse
logical,dimension(:,:),allocatable :: visited
real(wp),dimension(:),allocatable :: x, y, x0, y0

call clk%tic()

array = read_file_to_char_array('inputs/day10.txt', '.') ! pad with . to simplify edge logic
nrows = size(array,1)
ncols = size(array,2)

allocate(distance(nrows,ncols)); distance = -1
allocate(distance_reverse(nrows,ncols)); distance_reverse = -1
allocate(visited(nrows,ncols))

! start at the S coordinate:
main: do istart = 1, nrows
    do jstart = 1, ncols
        if (array(istart,jstart)=='S') exit main
    end do
end do main
visited = .false.
visited(istart, jstart) = .true.
distance(istart, jstart) = 0
x = [istart] ! to store the path
y = [jstart]

! traverse the maze:
do imove = 1, 4
    call move(istart, jstart, imove, distance)
end do
x0 = [x,real(istart,wp)] ! save the path for part b
y0 = [y,real(jstart,wp)]

! do it in reverse!
visited = .false.
visited(istart, jstart) = .true.
distance_reverse(istart, jstart) = 0
! traverse the maze:
do imove = 4, 1, -1
    call move(istart, jstart, imove, distance_reverse)
end do
write(*,*) '10a: ', maxval(distance+distance_reverse)/2 ! this is the distance furthest away from the start

! for part b, use locpt to test all the points !
icount = 0
do i = 2, nrows-1 ! we can skip the padding
    do j = 2, ncols-1
        if (any(i==x .and. j==y)) cycle ! skip if on path
        call locpt (real(i,wp), real(j,wp), x0, y0, size(x0), l, m)
        if (l==1) icount = icount + 1 ! if (x0,y0) is inside the polygonal path
    end do
end do
write(*,*) '10b: ', icount

call clk%toc('10')

contains

    recursive subroutine move(i,j,direction,distance)
        integer,intent(in) :: i,j,direction
        integer,dimension(:,:),intent(inout) :: distance

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
                    call move(inew,jnew,imove,distance)
                end do
                x = [x, real(inew,wp)] ! save cordinates of point on the path
                y = [y, real(jnew,wp)]
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


! the following routine is from the Fortran Astrodynamics Toolkit

!*****************************************************************************************
!>
!  given a polygonal line connecting the vertices (x(i),y(i))
!  (i = 1,...,n) taken in this order. it is assumed that the
!  polygonal path is a loop, where (x(n),y(n)) = (x(1),y(1))
!  or there is an arc from (x(n),y(n)) to (x(1),y(1)).
!
!  (x0,y0) is an arbitrary point and l and m are variables.
!  l and m are assigned the following values:
!
!     l = -1   if (x0,y0) is outside the polygonal path
!     l =  0   if (x0,y0) lies on the polygonal path
!     l =  1   if (x0,y0) is inside the polygonal path
!
!  m = 0 if (x0,y0) is on or outside the path. if (x0,y0)
!  is inside the path then m is the winding number of the
!  path around the point (x0,y0).
!
!# History
!  * Original version from the NSWC Library
!  * Modified by J. Williams : 08/04/2012 : refactored to modern Fortran

    pure subroutine locpt (x0, y0, x, y, n, l, m)

    implicit none

    !arguments:
    integer,intent(in)               :: n
    real(wp),intent(in)              :: x0
    real(wp),intent(in)              :: y0
    real(wp),dimension(n),intent(in) :: x
    real(wp),dimension(n),intent(in) :: y
    integer,intent(out)              :: l
    integer,intent(out)              :: m

    !constants:
    real(wp),parameter :: eps = epsilon(1.0_wp)
    real(wp),parameter :: pi  = atan2(0.0_wp, -1.0_wp)
    real(wp),parameter :: pi2 = 2.0_wp*pi
    real(wp),parameter :: tol = 4.0_wp*eps*pi

    !local variables:
    integer  :: i,n0
    real(wp) :: u,v,theta1,sum,theta,angle,thetai

    n0 = n
    if (x(1) == x(n) .and. y(1) == y(n)) n0 = n - 1
    l = -1
    m = 0
    u = x(1) - x0
    v = y(1) - y0

    if (u == 0.0_wp .and. v == 0.0_wp) then

        l = 0  !(x0, y0) is on the boundary of the path

    else

        if (n0 >= 2) then

            theta1 = atan2(v, u)
            sum = 0.0_wp
            theta = theta1

            do i = 2,n0

                u = x(i) - x0
                v = y(i) - y0

                if (u == 0.0_wp .and. v == 0.0_wp) then
                    l = 0  !(x0, y0) is on the boundary of the path
                    exit
                end if

                thetai = atan2(v, u)
                angle = abs(thetai - theta)
                if (abs(angle - pi) < tol) then
                    l = 0  !(x0, y0) is on the boundary of the path
                    exit
                end if

                if (angle > pi) angle = angle - pi2
                if (theta > thetai) angle = -angle
                sum = sum + angle
                theta = thetai

            end do

            if (l/=0) then

                angle = abs(theta1 - theta)
                if (abs(angle - pi) < tol) then

                    l = 0  !(x0, y0) is on the boundary of the path

                else

                    if (angle > pi) angle = angle - pi2
                    if (theta > theta1) angle = -angle
                    sum = sum + angle    !sum = 2*pi*m where m is the winding number
                    m = abs(sum)/pi2 + 0.2_wp
                    if (m /= 0) then
                        l = 1
                        if (sum < 0.0_wp) m = -m
                    end if

                end if

            end if

        end if

    end if

    end subroutine locpt
!*****************************************************************************************

end program problem_10