program problem_18

    use aoc_utilities
    use iso_fortran_env, only: wp => real64

    implicit none

    integer :: iunit, iline, n_lines, n
    character(len=:),allocatable :: line
    type(string),dimension(:),allocatable :: vals
    character(len=1) :: direction
    integer,dimension(2) :: iloc, iloc2
    integer :: xmin,xmax,ymin,ymax,ival, isteps
    real(wp),dimension(:),allocatable :: xvec,yvec

    call clk%tic()

    ! open(newunit=iunit, file='inputs/day18_test.txt', status='OLD')
    open(newunit=iunit, file='inputs/day18.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)

    ! once just to get the map size
    xmin = huge(1)
    xmax = 0
    ymin = huge(1)
    ymax = 0
    iloc = [0,0] ! where to start
    isteps = 0
    xvec = [0]; yvec = [0]
    do iline = 1, n_lines
        line = read_line(iunit)
        vals = split(line, ' ') ! direction, number, (hex)
        direction = vals(1)%str
        ival = int(vals(2)%str)
        iloc2 = destination(iloc, direction, ival)
        isteps = isteps + ival
        xmin = merge(iloc2(1), xmin, iloc2(1)<xmin)
        xmax = merge(iloc2(1), xmax, iloc2(1)>xmax)
        ymin = merge(iloc2(2), ymin, iloc2(2)<ymin)
        ymax = merge(iloc2(2), ymax, iloc2(2)>ymax)
        xvec = [xvec, real(iloc2(1),wp)] ! save the points
        yvec = [yvec, real(iloc2(2),wp)] !
        iloc = iloc2
    end do
    close(iunit)

    ! area + 1/2 the perimeter + 1
    n = size(xvec)
    write(*,*) '18a:', int(abs(parea(yvec, xvec, n)) + real(isteps,wp)/2 + 1)

    call clk%toc('18')

    contains

    pure function destination(initial, direction, steps) result(final)
        integer,dimension(2),intent(in) :: initial
        character(len=1),intent(in) :: direction
        integer,intent(in) :: steps
        integer,dimension(2) :: final !! x,y of desination point
        select case (direction)
        case('R'); final = [initial(1), initial(2)+steps]
        case('L'); final = [initial(1), initial(2)-steps]
        case('D'); final = [initial(1)+steps, initial(2)]
        case('U'); final = [initial(1)-steps, initial(2)]
        end select
    end function destination

end program problem_18
