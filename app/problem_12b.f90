program problem_12b

!! completely reworked solution from part a
!! this one starts with the int list and checks it against the pattern.
!! it also employs a function cache to speed it up.

use iso_fortran_env, ip => int64, wp => real64
use aoc_utilities
use aoc_cache_module

implicit none

! note: these are negative because we are appending
! them to the pattern for the cache
integer(ip),parameter :: POINT    = -1 ! .
integer(ip),parameter :: NUMBER   = -2 ! #
integer(ip),parameter :: QUESTION = -3 ! ?

! some global variables
integer :: iunit, n_lines, iline
character(len=:),allocatable :: line, pattern
type(string),dimension(:),allocatable :: vals
integer(ip),dimension(:),allocatable :: ints,ipattern
integer(ip) :: isum, ival
type(function_cache) :: cache !! to cache the [[go]] function values

call clk%tic()

call cache%initialize(isize=1000000,chunk_size=1000)

! open(newunit=iunit, file='inputs/day12_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day12.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
isum = 0
do iline = 1, n_lines
    line = read_line(iunit)
    vals = split(line,' ')
    ints = parse_ints(vals(2)%str)  ! integer list
    pattern = vals(1)%str           ! the pattern
    ! will convert the pattern to an array of numbers:
    ipattern = str_to_int64_array_with_mapping(pattern,['.','#','?'],&
                                                       [POINT,NUMBER,QUESTION])
    ! expand the input
    ipattern = [ipattern, QUESTION, ipattern, QUESTION, &
                ipattern, QUESTION, ipattern, QUESTION, &
                ipattern]
    ints = [ints, ints, ints, ints, ints]
    ival = go(ipattern, ints)
    isum = isum + ival
end do
write(*,*) '12b: ', isum

call clk%toc('12b')

contains

    recursive function go(ipattern, ints) result(ival)
        integer(ip),dimension(:),intent(in) :: ipattern
        integer(ip),dimension(:),intent(in) :: ints
        integer(ip) :: ival

        integer(ip) :: idx
        logical :: found

        ! first check the cache:
        call cache%get([ipattern,ints],idx,ival,found)
        if (.not. found) then
            if (size(ints)==0) then ! no more ints
                ival = merge(0, 1, any(ipattern==NUMBER)) ! if any more numbers, invalid
            else if (size(ipattern)==0) then
                ival = 0  ! too many ints
            else
                ! process next element in pattern
                select case (ipattern(1))
                case(NUMBER);    ival = ipound(ipattern, ints)
                case (POINT);    ival = ipoint(ipattern, ints)
                case (QUESTION); ival = ipoint(ipattern, ints) + ipound(ipattern, ints)
                end select
            end if
            ! cache this function call:
            call cache%put(idx,[ipattern,ints],ival)

        end if

    end function go

    recursive integer(ip) function ipound(ipattern, ints)
        integer(ip),dimension(:),intent(in) :: ipattern !! first char is a #
        integer(ip),dimension(:),intent(in) :: ints

        integer(ip),dimension(:),allocatable :: this_group

        ! check for the number of # that correspond to the first group
        if (size(ipattern)>=ints(1)) then
            this_group = ipattern(1:ints(1))
            where (this_group==QUESTION) this_group = NUMBER ! replace ? with #
            if (any(this_group/=NUMBER)) then
                ! can't fit all the #'s so not valid
                ipound = 0
                return
            else
                ! so far so good
            end if
        else
            ! not enough room to hold all the #'s
            ipound = 0
            return
        end if

        if (size(ipattern) == ints(1)) then
            ! if this is the last group, then we are done
            ipound = merge(1, 0, size(ints)==1)
        else
            ! the next character after this number must be a ? or .
            if (size(ipattern)>=ints(1)+1) then
                if (any(ipattern(ints(1)+1)==[QUESTION,POINT])) then
                    block
                        integer(ip),dimension(:),allocatable :: ipattern_tmp, ints_tmp ! to handle edge cases
                        ! skip it and process the next group
                        if (size(ipattern)>=ints(1)+2) then
                            ipattern_tmp = ipattern(ints(1)+2:)
                        else
                            allocate(ipattern_tmp(0))
                        end if
                        if (size(ints)>=2) then
                            ints_tmp = ints(2:)
                        else
                            allocate(ints_tmp(0))
                        end if
                        ipound = go(ipattern_tmp, ints_tmp)
                        return
                    end block
                end if
            end if

            ! not valid at this point
            ipound = 0
        end if

        end function ipound

        recursive integer(ip) function ipoint(ipattern,ints)
            integer(ip),dimension(:),intent(in) :: ipattern !! first char is a .
            integer(ip),dimension(:),intent(in) :: ints
            if (size(ipattern)<=1) then
                ipoint = go([integer(ip) ::], ints) ! done, pass in empty array
            else
                ipoint = go(ipattern(2:), ints) ! skip it and go to next one
            end if
        end function ipoint

end program problem_12b