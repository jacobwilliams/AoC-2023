program problem_12

use iso_fortran_env, ip => int64, wp => real64
use aoc_utilities

implicit none

integer,parameter :: POINT    = 0
integer,parameter :: NUMBER   = 1
integer,parameter :: QUESTION = 2

! some global variables
integer,dimension(:),allocatable :: a,ints,ipattern,ipattern_tmp
integer(ip) :: n_valid
integer(ip) :: isum
integer :: iline

call clk%tic()

call go(.false., isum); write(*,*) '12a: ', isum
! call go(.true., isum); write(*,*) '12b: ', isum

call clk%toc('12')

contains

subroutine go(expand,isum)
    logical,intent(in) :: expand
    integer(ip),intent(out) :: isum

    integer :: iunit, n_lines, n_unknowns
    integer(ip) :: n_perms
    character(len=:),allocatable :: line, pattern
    type(string),dimension(:),allocatable :: vals

    ! open(newunit=iunit, file='inputs/day12_test.txt', status='OLD')
    open(newunit=iunit, file='inputs/day12.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)
    isum = 0
    do iline = 1, n_lines
        line = read_line(iunit)
        vals = split(line,' ')
        ints = parse_ints(vals(2)%str)     ! integer list  1,1,3
        pattern = vals(1)%str              ! the pattern   #.#.###

        ! will convert the pattern to an array of numbers:
        ipattern = str_to_int_array_with_mapping(pattern,['.','#','?'],&
                                                        [POINT,NUMBER,QUESTION])  ! 1010111

        if (expand) then
            ! brute force it
            ipattern = [ipattern, QUESTION, ipattern, QUESTION, &
                        ipattern, QUESTION, ipattern, QUESTION, &
                        ipattern]
            ints = [ints, ints, ints, ints, ints]
        end if

        n_unknowns = count(ipattern==2)
        n_valid = 0 ! number of valid permutations
        n_perms = 2 ** n_unknowns ! number of permutations
        ipattern_tmp = ipattern

        ! recursively test all the permutations
        if (allocated(a)) deallocate(a)
        allocate(a(n_unknowns))
        call test(1, n_unknowns)

        isum = isum + n_valid

       ! write(*,*) iline, 'n_valid = ', n_valid

    end do

    end subroutine go

    recursive subroutine test (i, n)
    !! each ? can be either a . or a #
    !! check pattern to match the int list

    integer, intent(in) :: i, n
    integer :: ix
    integer,dimension(*),parameter :: icoeffs = [POINT,NUMBER] !! set of coefficients ['.', '#']

    ! what we are not doing here is accounting for permutations
    ! that we know do not match, because the begin with a sequence
    ! that doesn't match. those need to be skipped...

    if (i > n) then
        ! so we have an array of 0s and 1s -> replace the 2s in the ipattern with these
        !     ipattern: 2220111  -> ???.###
        !            a: 011      -> .##
        !       result: 0110111  -> .##.###
        !write(*,'(a,1x,i5,1x, *(I1))') 'test:', iline, ipattern_tmp
        ipattern_tmp = unpack(a, mask=ipattern==QUESTION, field=ipattern)
        if (match(ipattern_tmp, ints)) n_valid = n_valid + 1
    else
        do ix = 1, 2
            a(i) = icoeffs(ix)
            call test(i+1, n)
        end do
    end if
    end subroutine test

    logical function match(ipattern, ints)
        !! returns true if the pattern is valid for the int list.
        integer,dimension(:),intent(in) :: ipattern
        integer,dimension(:),intent(in) :: ints

        integer :: i, iacc, int_checked
        integer,dimension(1) :: ifirst, iend
        logical :: accumulating

        ! .##..###... -> 2,3

        ! start and end indices (ignoring leading and trailing spaces)
        ifirst = findloc(ipattern,1)
        iend   = findloc(ipattern,1,back=.true.)
        if (ifirst(1)==0 .or. iend(1)==0) then ! all blank
            match = .false.; return
        end if

        ! step through the pattern and stop once we find it invalid
        accumulating = .true.
        iacc = 0
        int_checked = 0 ! the count of ints that have been checked
        match = .true. ! initialize
        do i = ifirst(1), iend(1)
            select case(ipattern(i))
            case(POINT)
                if (accumulating) then
                    int_checked = int_checked + 1 ! check the next one
                    if (int_checked>size(ints)) then
                        ! too many ints
                        match = .false.
                        return
                    else if (ints(int_checked)/=iacc) then
                        ! doesn't match
                        match = .false.
                        return
                    end if
                    accumulating = .false.
                    iacc = 0
                end if
            case(NUMBER)
                if (accumulating) then
                    iacc = iacc + 1
                else
                    ! start of a new number
                    accumulating = .true.
                    iacc = 1
                end if
                if (i==iend(1)) then ! last number
                    int_checked = int_checked + 1 ! check the next one
                    if (int_checked/=size(ints)) then
                        ! not enough ints
                        match = .false.
                        return
                    else if (ints(int_checked)/=iacc) then
                        ! doesn't match
                        match = .false.
                        return
                    end if
                end if
            end select
        end do

    end function match

end program problem_12