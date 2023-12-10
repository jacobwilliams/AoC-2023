program problem_4

use iso_fortran_env
use aoc_utilities

implicit none

integer :: iunit, n_lines, i, points, j, iwin
type(string),dimension(:),allocatable :: vals, vals2
integer,dimension(:),allocatable :: iwinning, ihave, n_matches
character(len=:),allocatable :: line
integer,dimension(:,:),allocatable :: card_matrix

call clk%tic()

! open(newunit=iunit, file='inputs/day4_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day4.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)

allocate(n_matches(n_lines))              ! for part 2
allocate(card_matrix(0:n_lines, n_lines)) ! for part 2

! Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
points = 0
do i = 1, n_lines
    line = read_line(iunit)
    vals  = split(line,': ')
    vals2 = split(vals(2), ' | ')
    iwinning = parse_ints(vals2(1)%str) ! winning list
    ihave    = parse_ints(vals2(2)%str) ! ones you have
    ! count the number of ones you have in the winning set:
    iwin = 0
    do j = 1, size(ihave)
        iwin = iwin + count(ihave(j) == iwinning)
    end do
    n_matches(i) = iwin
    points = points + 2**(iwin-1)
end do
close(iunit)
write(*,*) '4a : points : ', points

card_matrix = 0
card_matrix(0,:) = 1 ! start with one card each
do i = 1, n_lines
    associate (n => n_matches(i))
        if (n>0) card_matrix(i, i+1:i+n) = sum(card_matrix(:,i))
    end associate
end do
write(*,*) '4b : scratchcards :', sum(card_matrix)

call clk%toc('4')

end program problem_4