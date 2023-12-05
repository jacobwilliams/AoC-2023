program problem_5

use iso_fortran_env, only: ip => int64 ! use long ints
use aoc_utilities

implicit none

integer :: i, iunit, n_lines
character(len=:),allocatable :: line
type(string),dimension(:),allocatable :: vals
integer(ip),dimension(:),allocatable :: seeds_list
integer(ip),dimension(:),allocatable :: nums
integer(ip) ::  isoil, &
                ifertilizer, &
                iwater, &
                ilight, &
                itemperature, &
                ihumidity, &
                ilocation
integer(ip) :: ilocation_min

! store data as arrays:
integer(ip),dimension(:),allocatable :: seed_to_soil_seed_start, seed_to_soil_seed_end
integer(ip),dimension(:),allocatable :: seed_to_soil_soil_start, seed_to_soil_soil_end
integer(ip),dimension(:),allocatable :: soil_to_fertilizer_soil_start, soil_to_fertilizer_soil_end
integer(ip),dimension(:),allocatable :: soil_to_fertilizer_fertilizer_start, soil_to_fertilizer_fertilizer_end
integer(ip),dimension(:),allocatable :: fertilizer_to_water_fertilizer_start, fertilizer_to_water_fertilizer_end
integer(ip),dimension(:),allocatable :: fertilizer_to_water_water_start, fertilizer_to_water_water_end
integer(ip),dimension(:),allocatable :: water_to_light_water_start, water_to_light_water_end
integer(ip),dimension(:),allocatable :: water_to_light_light_start, water_to_light_light_end
integer(ip),dimension(:),allocatable :: light_to_temperature_light_start, light_to_temperature_light_end
integer(ip),dimension(:),allocatable :: light_to_temperature_temperature_start, light_to_temperature_temperature_end
integer(ip),dimension(:),allocatable :: temperature_to_humidity_temperature_start, temperature_to_humidity_temperature_end
integer(ip),dimension(:),allocatable :: temperature_to_humidity_humidity_start, temperature_to_humidity_humidity_end
integer(ip),dimension(:),allocatable :: humidity_to_location_humidity_start, humidity_to_location_humidity_end
integer(ip),dimension(:),allocatable :: humidity_to_location_location_start, humidity_to_location_location_end

integer :: parsing_state !! 1 = parsing seed_to_soil, &
                         !! 2 = parsing soil_to_fertilizer, &
                         !! 3 = parsing fertilizer_to_water, &
                         !! 4 = parsing water_to_light, &
                         !! 5 = parsing light_to_temperature, &
                         !! 6 = parsing temperature_to_humidity, &
                         !! 7 = parsing humidity_to_location

! read in the data into the arrays:
allocate(   seed_to_soil_seed_start(0),                   seed_to_soil_seed_end(0),                   &
            seed_to_soil_soil_start(0),                   seed_to_soil_soil_end(0),                   &
            soil_to_fertilizer_soil_start(0),             soil_to_fertilizer_soil_end(0),             &
            soil_to_fertilizer_fertilizer_start(0),       soil_to_fertilizer_fertilizer_end(0),       &
            fertilizer_to_water_fertilizer_start(0),      fertilizer_to_water_fertilizer_end(0),      &
            fertilizer_to_water_water_start(0),           fertilizer_to_water_water_end(0),           &
            water_to_light_water_start(0),                water_to_light_water_end(0),                &
            water_to_light_light_start(0),                water_to_light_light_end(0),                &
            light_to_temperature_light_start(0),          light_to_temperature_light_end(0),          &
            light_to_temperature_temperature_start(0),    light_to_temperature_temperature_end(0),    &
            temperature_to_humidity_temperature_start(0), temperature_to_humidity_temperature_end(0), &
            temperature_to_humidity_humidity_start(0),    temperature_to_humidity_humidity_end(0),    &
            humidity_to_location_humidity_start(0),       humidity_to_location_humidity_end(0),       &
            humidity_to_location_location_start(0),       humidity_to_location_location_end(0) )
! open(newunit=iunit, file='inputs/day5_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day5.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
parsing_state = 0
ilocation_min = huge(1)
do i = 1, n_lines
    line = read_line(iunit)
    !write(*,'(a)') 'line: '//line
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
        select case (parsing_state)
        case(1); call populate(nums(1), nums(2), nums(3), &
                                seed_to_soil_soil_start, seed_to_soil_soil_end, &
                                seed_to_soil_seed_start, seed_to_soil_seed_end)
        case(2); call populate(nums(1), nums(2), nums(3), &
                                soil_to_fertilizer_fertilizer_start, soil_to_fertilizer_fertilizer_end, &
                                soil_to_fertilizer_soil_start, soil_to_fertilizer_soil_end)
        case(3); call populate(nums(1), nums(2), nums(3), &
                                fertilizer_to_water_water_start, fertilizer_to_water_water_end, &
                                fertilizer_to_water_fertilizer_start, fertilizer_to_water_fertilizer_end)
        case(4); call populate(nums(1), nums(2), nums(3), &
                                water_to_light_light_start, water_to_light_light_end, &
                                water_to_light_water_start, water_to_light_water_end)
        case(5); call populate(nums(1), nums(2), nums(3), &
                                light_to_temperature_temperature_start, light_to_temperature_temperature_end, &
                                light_to_temperature_light_start, light_to_temperature_light_end)
        case(6); call populate(nums(1), nums(2), nums(3), &
                                temperature_to_humidity_humidity_start, temperature_to_humidity_humidity_end, &
                                temperature_to_humidity_temperature_start, temperature_to_humidity_temperature_end)
        case(7); call populate(nums(1), nums(2), nums(3), &
                                humidity_to_location_location_start, humidity_to_location_location_end, &
                                humidity_to_location_humidity_start, humidity_to_location_humidity_end)
        case default; error stop 'invalid parsing state'
        end select
    end if
end do
close(iunit)
! print*, 'seed_to_soil_soil_start: ', seed_to_soil_soil_start
! print*, 'seed_to_soil_soil_end: ', seed_to_soil_soil_end

! now, process the data:

do i = 1, size(seeds_list)

    write(*,*) 'for seed: ', seeds_list(i)

    isoil        = map(seeds_list(i), seed_to_soil_soil_start,                seed_to_soil_seed_start,                   seed_to_soil_seed_end)
    ifertilizer  = map(isoil,         soil_to_fertilizer_fertilizer_start,    soil_to_fertilizer_soil_start,             soil_to_fertilizer_soil_end)
    iwater       = map(ifertilizer,   fertilizer_to_water_water_start,        fertilizer_to_water_fertilizer_start,      fertilizer_to_water_fertilizer_end)
    ilight       = map(iwater,        water_to_light_light_start,             water_to_light_water_start,                water_to_light_water_end)
    itemperature = map(ilight,        light_to_temperature_temperature_start, light_to_temperature_light_start,          light_to_temperature_light_end)
    ihumidity    = map(itemperature,  temperature_to_humidity_humidity_start, temperature_to_humidity_temperature_start, temperature_to_humidity_temperature_end)
    ilocation    = map(ihumidity,     humidity_to_location_location_start,    humidity_to_location_humidity_start,       humidity_to_location_humidity_end)

    ! write(*,*)  seeds_list(i), &
    !             isoil, &
    !             ifertilizer, &
    !             iwater, &
    !             ilight, &
    !             itemperature, &
    !             ihumidity, &
    !             ilocation

    !print*, 'ilocation: ', ilocation
    if (ilocation < ilocation_min) ilocation_min = ilocation

end do
print*, '5a: ', ilocation_min




contains

    subroutine populate(dest, source, range, dest_start, dest_end, source_start, source_end)
        integer(ip),intent(in) :: dest, source, range  ! the three numbers from the line
        integer(ip),dimension(:),allocatable,intent(inout) :: dest_start, dest_end, source_start, source_end
        dest_start   = [dest_start,   dest]
        dest_end     = [dest_end,     dest+range]
        source_start = [source_start, source]
        source_end   = [source_end,   source+range]
    end subroutine populate

    function map(isource, dest_start, source_start, source_end) result(idest)
        integer(ip),intent(in) :: isource
        integer(ip),dimension(:),intent(in) :: dest_start, source_start, source_end ! all the same size
        integer(ip) :: idest
        integer :: i
        idest = isource ! if not found in any of the sets
        do i = 1, size(source_start)
            ! locate isource in the source start:end range
            if (isource>=source_start(i) .and. isource<=source_end(i)) then
                ! found it, map to dest

                !-- example --
                !
                ! src       dest
                ! 2 3 4  -> 10 11 12
                !
                ! ival=2 ===> idest=10
                ! ival=3 ===> idest=11
                !write(*,*) '   ', source_start(i), '<=', isource, '<=', source_end(i)

                idest = dest_start(i) + (isource-source_start(i))
                return
            end if
        end do

    end function map

end program problem_5