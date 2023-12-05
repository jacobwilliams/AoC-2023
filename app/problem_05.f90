program problem_5

use iso_fortran_env, only: ip => int64 ! use long ints
use aoc_utilities

implicit none

integer :: i, iunit, n_lines
character(len=:),allocatable :: line
type(string),dimension(:),allocatable :: vals
integer(ip),dimension(:),allocatable :: seeds_list
integer(ip),dimension(:),allocatable :: nums
integer(ip) :: ilocation_min, ilocation, iseed
integer(ip),dimension(:),allocatable :: ilocation_min_parallel

type :: mapping
    ! save the source and destination start and end values
    integer(ip),dimension(:),allocatable :: dest_start, dest_end, src_start, src_end
end type mapping
integer,parameter :: NSTAGES = 7
type(mapping),dimension(NSTAGES) :: mappings ! seed_to_soil, soil_to_fertilizer, fertilizer_to_water, water_to_light,
                                             ! light_to_temperature, temperature_to_humidity, humidity_to_location
integer :: parsing_state ! index in mappings (1 to NSTAGES)

do i = 1, NSTAGES
    allocate(mappings(i)%dest_start(0),&
             mappings(i)%dest_end(0),&
             mappings(i)%src_start(0),&
             mappings(i)%src_end(0))
end do

! open(newunit=iunit, file='inputs/day5_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day5.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
do i = 1, n_lines
    line = read_line(iunit)
    if (line=='') cycle ! blank line
    if (startswith(line, 'seeds:')) then; seeds_list = parse_ints64(line(7:))
    elseif (startswith(line, 'seed-to-soil map:'))            then; parsing_state = 1
    elseif (startswith(line, 'soil-to-fertilizer map:'))      then; parsing_state = 2
    elseif (startswith(line, 'fertilizer-to-water map:'))     then; parsing_state = 3
    elseif (startswith(line, 'water-to-light map:'))          then; parsing_state = 4
    elseif (startswith(line, 'light-to-temperature map:'))    then; parsing_state = 5
    elseif (startswith(line, 'temperature-to-humidity map:')) then; parsing_state = 6
    elseif (startswith(line, 'humidity-to-location map:'))    then; parsing_state = 7
    else
        ! parse the numbers:
        nums = parse_ints64(line)  ! destination range start, source range start, range length
        call populate(nums, mappings(parsing_state))
    end if
end do
close(iunit)

! ------------ part 1 -----------------

ilocation_min = huge(1)
do i = 1, size(seeds_list)
    ilocation = traverse(seeds_list(i))
    if (ilocation < ilocation_min) ilocation_min = ilocation
end do
print*, '5a: ', ilocation_min

! ------------ part 2 -----------------

!  so it doesn't run in the CI !!
! if (.true.) then
!     print*, '5b: ', 11611182   ! answer produced by the code below !
!     stop
! end if

allocate(ilocation_min_parallel(size(seeds_list)/2))
ilocation_min_parallel = huge(1)
!$OMP PARALLEL DO SHARED(ilocation_min_parallel) PRIVATE(i,iseed,ilocation)
do i = 1, size(seeds_list), 2
    do iseed = seeds_list(i), seeds_list(i)+seeds_list(i+1)-1
        ilocation = traverse(iseed)
        if (ilocation < ilocation_min_parallel((i+1)/2)) ilocation_min_parallel((i+1)/2) = ilocation
    end do
end do
!$OMP END PARALLEL DO
print*, '5b: ', minval(ilocation_min_parallel)

contains

    subroutine populate(nums, m)
        integer(ip),dimension(3),intent(in) :: nums  ! the three numbers from the line
        type(mapping),intent(inout) :: m
        associate( dest => nums(1), source => nums(2), range => nums(3) )
            m%dest_start = [m%dest_start,   dest]
            m%dest_end   = [m%dest_end,     dest+range]
            m%src_start  = [m%src_start,    source]
            m%src_end    = [m%src_end,      source+range]
        end associate
    end subroutine populate

    pure function map(isource, m) result(idest)
        integer(ip),intent(in) :: isource
        type(mapping),intent(in) :: m
        integer(ip) :: idest
        integer :: i
        do i = 1, size(m%src_start)
            ! locate isource in the source start:end range
            if (isource>=m%src_start(i) .and. isource<=m%src_end(i)) then ! found it, map to dest
                idest = m%dest_start(i) + (isource-m%src_start(i))
                return
            end if
        end do
        idest = isource ! if not found in any of the sets
    end function map

    pure function traverse(iseed) result(ilocation)
        integer(ip),intent(in) :: iseed
        integer(ip) :: ilocation
        integer :: i
        ilocation = iseed
        do i = 1, NSTAGES
            ilocation = map(ilocation,mappings(i))
        end do
    end function traverse

end program problem_5