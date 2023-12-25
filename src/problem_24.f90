program problem_24

use iso_fortran_env, only: wp => real64, ip => int64 ! use long ints
use aoc_utilities

implicit none

integer :: iunit, n_lines, i, j
character(len=:),allocatable :: line
type(string),dimension(:),allocatable :: vals, rvals, vvals
integer(ip),dimension(:),allocatable :: xvec, yvec, zvec, vxvec, vyvec, vzvec
integer(ip) :: n_intersections
real(wp),dimension(2) :: tvec
real(wp),dimension(2) :: xy_inter
integer(ip) :: min_val, max_val

call clk%tic()

! read the data:
! open(newunit=iunit, file='inputs/day24_test.txt', status='OLD'); min_val=7_ip; max_val = 27_ip
open(newunit=iunit, file='inputs/day24.txt', status='OLD'); min_val=200000000000000.0_wp; max_val = 400000000000000.0_wp
n_lines = number_of_lines_in_file(iunit)
allocate(xvec(0), yvec(0), zvec(0), vxvec(0), vyvec(0), vzvec(0))
do i = 1, n_lines
    line = read_line(iunit)
    vals = split(line, '@')
    rvals = split(trim(vals(1)%str), ',')
    xvec  = [xvec, rvals(1)%to_int_64()]
    yvec  = [yvec, rvals(2)%to_int_64()]
    zvec  = [zvec, rvals(3)%to_int_64()]
    vvals = split(trim(vals(2)%str), ',')
    vxvec = [vxvec, vvals(1)%to_int_64()]
    vyvec = [vyvec, vvals(2)%to_int_64()]
    vzvec = [vzvec, vvals(3)%to_int_64()]
end do
close(iunit)

n_intersections = 0
do i = 1, n_lines
    do j = 1, i-1
        associate ( x  => xvec(i),    y => yvec(i),  &
                    dx => vxvec(i),  dy => vyvec(i), &
                    u  => xvec(j),    v => yvec(j),  &
                    du => vxvec(j),  dv => vyvec(j)  )
            if (dy*du /= dv*dx) then
                tvec = [(dv*(x-u) - du*(y-v)) / (dy*du - dx*dv), &
                        (dy*(u-x) - dx*(v-y)) / (dv*dx - du*dy) ]
                xy_inter = [x+tvec(1)*dx, y+tvec(1)*dy]
                if (all(tvec>0.0_wp) .and. &
                    all([xy_inter>=min_val .and. xy_inter<=max_val])) then
                    n_intersections = n_intersections + 1
                end if
            end if
        end associate
    end do
end do

write(*,*) '21a : ', n_intersections

call clk%toc('24')

end program problem_24
