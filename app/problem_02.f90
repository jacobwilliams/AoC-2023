program problem_2

use iso_fortran_env
use aoc_utilities

implicit none

integer :: iunit
logical :: status_ok, game_possible
character(len=:),allocatable :: line, color
integer :: n_lines, id, j, k, ipossible, inum, min_red, min_green, min_blue, power
type(string),dimension(:),allocatable :: vals, trys, cubes, num_color

integer,parameter :: n_red   = 12
integer,parameter :: n_green = 13
integer,parameter :: n_blue  = 14

!example: Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
open(newunit=iunit, file='inputs/day2.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit) ! will parse the line with successive splits
ipossible = 0
power = 0
main: do id = 1, n_lines
    game_possible = .true.
    min_red = 0; min_green = 0; min_blue = 0
    line = read_line(iunit,status_ok)
    vals = split(line,': ')
    trys = split(vals(2), '; ')
    do j = 1, size(trys)
        cubes = split(trys(j), ', ')
        do k = 1, size(cubes)
            num_color = split(cubes(k), ' ')
            inum = int(num_color(1)%str)
            color = num_color(2)%str
            select case (color)
            case('red')
                if (inum>n_red) game_possible = .false.
                if (inum>min_red) min_red = inum
            case('green')
                if (inum>n_green) game_possible = .false.
                if (inum>min_green) min_green = inum
            case('blue')
                if (inum>n_blue) game_possible = .false.
                if (inum>min_blue) min_blue = inum
            end select
        end do
    end do
    power = power + (min_red * min_green * min_blue)
    if (game_possible) ipossible = ipossible + id

end do main
write(*,*) '2a: result   :', ipossible
write(*,*) '2b: power sum:', power
close(iunit)

end program problem_2