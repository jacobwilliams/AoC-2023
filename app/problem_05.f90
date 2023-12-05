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
parsing_state = 0
do i = 1, n_lines
    line = read_line(iunit)
    if (line=='') cycle ! blank line
    if (startswith(line, 'seeds:')) then; seeds_list = parse_ints64(line(7:))
    else if (index(line, 'map:')>0) then; parsing_state = parsing_state + 1 ! one of the 7 stages
    else
        ! parse the numbers for the given state:
        nums = parse_ints64(line)  ! destination range start, source range start, range length
        call populate(nums, mappings(parsing_state))
    end if
end do
close(iunit)

! ------------ part 1 -----------------

ilocation_min = huge(1)
do i = 1, size(seeds_list)
    ilocation = traverse(seeds_list(i),.false.)
    if (ilocation < ilocation_min) ilocation_min = ilocation
end do
print*, '5a: ', ilocation_min

! ------------ part 2 -----------------

!  so it doesn't run in the CI !!
if (.false.) then
    ! brute force, openMP version. just run the part a algorithm for all the seeds.
    ! takes a minute or so on my computer.
    allocate(ilocation_min_parallel(size(seeds_list)/2))
    ilocation_min_parallel = huge(1)
    !$OMP PARALLEL DO SHARED(ilocation_min_parallel) PRIVATE(i,iseed,ilocation)
    do i = 1, size(seeds_list), 2
        do iseed = seeds_list(i), seeds_list(i)+seeds_list(i+1)-1
            ilocation = traverse(iseed, .false.)
            if (ilocation < ilocation_min_parallel((i+1)/2)) ilocation_min_parallel((i+1)/2) = ilocation
        end do
    end do
    !$OMP END PARALLEL DO
    print*, '5b: ', minval(ilocation_min_parallel)
end if

! ------------ part 2 -----------------

! Alternate version, go backwards from the location to the seed
! and see if it is contained in the seed set.
! this one is pretty fast (< 1 sec)
do ilocation = 1, maxval(mappings(7)%dest_end) ! up to the max ilocation value
    iseed = traverse(ilocation, reverse=.true.) ! from ilocation to iseed
    if (in_seed_list(iseed)) exit ! found the min
end do
print*, '5b: ', ilocation

contains

    logical function in_seed_list(iseed)
        ! for part b, is the seed in the initial list
        integer(ip),intent(in) :: iseed
        integer :: i
        do i = 1, size(seeds_list), 2
            if (iseed>=seeds_list(i) .and. iseed<=seeds_list(i)+seeds_list(i+1)-1) then
                in_seed_list = .true.
                return
            end if
        end do
        in_seed_list = .false.
    end function in_seed_list

    subroutine populate(nums, m)
        integer(ip),dimension(3),intent(in) :: nums  ! the three numbers from the line
        type(mapping),intent(inout) :: m
        associate( dest => nums(1), source => nums(2), range => nums(3) )
            m%dest_start = [m%dest_start, dest]
            m%dest_end   = [m%dest_end,   dest+range]
            m%src_start  = [m%src_start,  source]
            m%src_end    = [m%src_end,    source+range]
        end associate
    end subroutine populate

    pure function map(ival, m, reverse) result(idest)
        integer(ip),intent(in) :: ival
        type(mapping),intent(in) :: m
        logical,intent(in) :: reverse ! if reversed, go from: dest -> src
        integer(ip) :: idest
        integer :: i
        if (reverse) then
            do i = 1, size(m%src_start)
                ! locate ival (dest) in the dest start:end range
                if (ival>=m%dest_start(i) .and. ival<=m%dest_end(i)) then ! found it, map to dest
                    idest = m%src_start(i) + (ival-m%dest_start(i)) ! this is the resultant isource
                    return
                end if
            end do
        else
            do i = 1, size(m%src_start)
                ! locate ival (source) in the source start:end range
                if (ival>=m%src_start(i) .and. ival<=m%src_end(i)) then ! found it, map to dest
                    idest = m%dest_start(i) + (ival-m%src_start(i))
                    return
                end if
            end do
        end if
        idest = ival ! if not found in any of the sets
    end function map

    pure function traverse(iseed, reverse) result(ilocation)
        integer(ip),intent(in) :: iseed
        logical,intent(in) :: reverse !! if reverse, then ilocation -> iseed
        integer(ip) :: ilocation
        integer :: i
        ilocation = iseed ! initialize
        if (reverse) then
            do i = NSTAGES, 1, -1
                ilocation = map(ilocation,mappings(i),reverse) ! this is really iseed
            end do
        else
            do i = 1, NSTAGES
                ilocation = map(ilocation,mappings(i),reverse)
            end do
        end if
    end function traverse

end program problem_5