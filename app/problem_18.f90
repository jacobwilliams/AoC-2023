program problem_18

    use aoc_utilities
    use iso_fortran_env, only: wp => real64

    implicit none

    integer :: iunit, iline, n_lines, i, j
    character(len=:),allocatable :: line
    integer,dimension(:,:),allocatable :: map
    type(string),dimension(:),allocatable :: vals
    character(len=1) :: direction
    integer,dimension(2) :: iloc, iloc2
    integer :: xmin,xmax,ymin,ymax,ival, icount
    real(wp),dimension(:),allocatable :: xvec,yvec,x0,y0
    integer :: l,m,n

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
    do iline = 1, n_lines
        line = read_line(iunit)
        vals = split(line, ' ') ! direction, number, (hex)
        direction = vals(1)%str
        ival = int(vals(2)%str)
        iloc2 = destination(iloc, direction, ival)
        xmin = merge(iloc2(1), xmin, iloc2(1)<xmin)
        xmax = merge(iloc2(1), xmax, iloc2(1)>xmax)
        ymin = merge(iloc2(2), ymin, iloc2(2)<ymin)
        ymax = merge(iloc2(2), ymax, iloc2(2)>ymax)
        iloc = iloc2
    end do
    rewind(iunit)
    !allocate(map(xmin:xmax, ymin:ymax)); map = 0
    xvec = [0]; yvec = [0]

    ! now fill it in:
    iloc = [0,0] ! where to start
    do iline = 1, n_lines
        line = read_line(iunit)
        vals = split(line, ' ') ! direction, number, (hex)
        direction = vals(1)%str
        ival = int(vals(2)%str)
        iloc2 = destination(iloc, direction, ival)
        xvec = [xvec, real(iloc2(1),wp)] ! save the points
        yvec = [yvec, real(iloc2(2),wp)] !
        ! select case (direction)
        ! case('R'); map(iloc(1),          iloc(2):iloc2(2)) = 1
        ! case('L'); map(iloc(1),          iloc2(2):iloc(2)) = 1
        ! case('D'); map(iloc(1):iloc2(1), iloc(2))          = 1
        ! case('U'); map(iloc2(1):iloc(1), iloc(2))          = 1
        ! case default; error stop 'error'
        ! end select
        iloc = iloc2
    end do
    rewind(iunit)

    ! rezero
    xvec = xvec - minval(xvec) + 1
    yvec = yvec - minval(yvec) + 1

    ! print*, 'xvec = ', xvec
    ! print*, 'yvec = ', yvec
    icount = 0
    n = size(xvec)
    do i = 1, int(maxval(xvec))
        do j = 1, int(maxval(yvec))
            call locpt (real(i,wp), real(j,wp), xvec, yvec, n, l, m)
            if (l /= -1) icount = icount + 1
            !print*, i,j,l
        end do
    end do

    write(*,*) '18a: ', icount

    ! do iline = lbound(map,1), ubound(map,1)
    !     write(*,'(*(i1))') map(iline,:)
    ! end do

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
