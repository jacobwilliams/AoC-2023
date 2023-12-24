program problem_18

    use aoc_utilities
    use iso_fortran_env, only: wp => real64, ip => int64

    implicit none

    integer(ip) :: iresult

    call clk%tic()

    call go(.false., iresult); write(*,*) '18a:', iresult
    call go(.true., iresult);  write(*,*) '18b:', iresult

    call clk%toc('18')

    contains

    subroutine go(partb, iresult)

        logical,intent(in) :: partb !! if solving part b
        integer(ip),intent(out) :: iresult

        integer :: iunit, iline, n_lines, n
        character(len=:),allocatable :: line, hex
        type(string),dimension(:),allocatable :: vals
        character(len=1) :: direction
        integer(ip),dimension(2) :: iloc, iloc2
        integer(ip) :: ival, isteps
        real(wp),dimension(:),allocatable :: xvec,yvec

        character(len=1),dimension(0:3) :: direction_str = ['R', 'D', 'L', 'U'] ! directions for part b : 0,1,2,3

        ! open(newunit=iunit, file='inputs/day18_test.txt', status='OLD')
        open(newunit=iunit, file='inputs/day18.txt', status='OLD')
        n_lines = number_of_lines_in_file(iunit)

        iloc = [0,0] ! where to start
        isteps = 0
        xvec = [0]; yvec = [0]
        do iline = 1, n_lines
            line = read_line(iunit)
            vals = split(line, ' ') ! direction, number, (hex)
            if (partb) then
                ! values encoded in hex
                hex       = vals(3)%str
                ival      = hex2int(hex(3:7))
                direction = direction_str(int(hex(8:8)))
            else
                direction = vals(1)%str
                ival = int(vals(2)%str)
            end if
            isteps = isteps + ival ! keep track of the total perimeter size
            iloc2 = destination(iloc, direction, ival) ! destination oint
            xvec = [xvec, real(iloc2(1),wp)] ! save the points of the polygon
            yvec = [yvec, real(iloc2(2),wp)] !
            iloc = iloc2 ! update for next move
        end do
        close(iunit)

        ! the area including the boundary = area + 1/2 the perimeter + 1
        ! using NSWC parea function!
        n = size(xvec)
        iresult = int(abs(parea(yvec, xvec, n)) + real(isteps,wp)/2_ip + 1_ip, ip)

    end subroutine go

    pure function destination(initial, direction, steps) result(final)
        !! get destination point
        integer(ip),dimension(2),intent(in) :: initial
        character(len=1),intent(in) :: direction
        integer(ip),intent(in) :: steps
        integer(ip),dimension(2) :: final !! x,y of desination point
        select case (direction)
        case('R'); final = [initial(1), initial(2)+steps]
        case('L'); final = [initial(1), initial(2)-steps]
        case('D'); final = [initial(1)+steps, initial(2)]
        case('U'); final = [initial(1)-steps, initial(2)]
        end select
    end function destination

end program problem_18
