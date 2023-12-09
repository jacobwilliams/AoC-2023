program problem_6

use iso_fortran_env, only: ip => int64 ! use long ints
use aoc_utilities

implicit none

! just copy and paste the inputs here:

! a
integer(ip),dimension(*),parameter :: time_a      = [ 60  ,   94 ,    78 ,    82]
integer(ip),dimension(*),parameter :: distance_a  = [475  , 2138 ,  1015 ,  1650]

! b
integer(ip),dimension(*),parameter :: time_b      = [ 60947882_ip]
integer(ip),dimension(*),parameter :: distance_b  = [475213810151650_ip]

call clk%tic()

print*, '6a: ways to win:', go(time_a, distance_a)
print*, '6b: ways to win:', go(time_b, distance_b)

call clk%toc()

contains

function go(time, distance) result(iproduct)
    integer(ip),dimension(:),intent(in) :: time
    integer(ip),dimension(:),intent(in) :: distance
    integer(ip) :: iproduct
    integer(ip) :: i, ihold, iways, idistance
    iproduct = 1
    do i = 1, size(time)
        iways = 0
        do ihold = 1, time(i) ! time to hold the button
            idistance = ihold * (time(i)-ihold)
            if (idistance > distance(i)) iways = iways + 1
        end do
        iproduct = iproduct * iways
    end do
end function go

end program problem_6