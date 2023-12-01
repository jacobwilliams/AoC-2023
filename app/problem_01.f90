program problem_1

use iso_fortran_env
use aoc_utilities

implicit none

! integer,dimension(:),allocatable :: iarray
! integer :: iunit, istat, ical, n_elves
! character(len=10) :: str
! integer :: current_count

! open(newunit=iunit, file='inputs/day1.txt', status='OLD')
! allocate(iarray(0))
! current_count = 0
! do
!     read(iunit,'(A)', iostat=istat) str
!     if (istat==iostat_end .or. str=='') then
!         iarray = [iarray, current_count]
!         current_count = 0
!         if (istat==iostat_end) exit
!     end if
!     read(str, '(I10)') ical
!     current_count = current_count + ical
! end do

! n_elves = size(iarray)
! call sort_ascending(iarray)

! write(*,*) '1a: max cals:         ', iarray(n_elves)
! write(*,*) '1b: sum of top three: ', sum(iarray(n_elves-2:n_elves))

end program problem_1