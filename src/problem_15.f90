program problem_15

use iso_fortran_env, only: ip => int64 ! use long ints
use aoc_utilities

implicit none

integer :: i, iunit
type(string),dimension(:),allocatable :: vals
integer(ip),dimension(:),allocatable :: ivals

call clk%tic()

! read the data file:
! open(newunit=iunit, file='inputs/day15_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day15.txt', status='OLD')
vals = split(read_line(iunit),',')
allocate(ivals(size(vals)))

do i = 1, size(vals)
    ivals(i) = hash(vals(i)%str)
end do
write(*,*) '15a: ', sum(ivals)

call clk%toc('15')

contains

    pure elemental integer(ip) function hash(s)
        !! Determine the ASCII code for the current character of the string.
        character(len=*),intent(in) :: s
        integer :: i
        hash = 0
        do i = 1, len(s)
            hash = modulo(17_ip*( hash + iachar(s(i:i))), 256_ip)
        end do
    end function hash

end program problem_15