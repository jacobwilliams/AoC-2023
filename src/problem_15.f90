program problem_15

use iso_fortran_env, only: ip => int64 ! use long ints
use aoc_utilities

implicit none

integer :: i, iunit
type(string),dimension(:),allocatable :: vals
character(len=:),allocatable :: line
integer(ip),dimension(:),allocatable :: ivals

call clk%tic()

! read the data file:
! open(newunit=iunit, file='inputs/day15_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day15.txt', status='OLD')
line = read_line(iunit)
vals = split(line,',')
allocate(ivals(size(vals))); ivals = 0

do i = 1, size(vals)
    associate(s => vals(i)%str)
        ivals(i) = hash(s)
    end associate
end do
write(*,*) '15a: ', sum(ivals)

contains
    pure elemental integer(ip) function hash(s)
        character(len=*),intent(in) :: s
        integer :: i
        ! Determine the ASCII code for the current character of the string.
        ! Increase the current value by the ASCII code you just determined.
        ! Set the current value to itself multiplied by 17.
        ! Set the current value to the remainder of dividing itself by 256.
        hash = 0
        do i = 1, len(s)
            hash = modulo(17_ip*( hash + iachar(s(i:i))), 256_ip)
        end do
    end function hash

end program problem_15