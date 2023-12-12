program problem_12

use iso_fortran_env, ip => int64, wp => real64
use aoc_utilities

implicit none

integer :: iunit, n_lines, i, n_unknowns, j, n_valid, n_perms, isum
character(len=:),allocatable :: line, pattern, tmp1, tmp2
type(string),dimension(:),allocatable :: vals
integer,dimension(:),allocatable :: ints

integer,dimension(:),allocatable :: a

call clk%tic()

open(newunit=iunit, file='inputs/day12_test.txt', status='OLD')
! open(newunit=iunit, file='inputs/day12.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
isum = 0
do i = 1, n_lines
    line = read_line(iunit)
    vals = split(line,' ')

    ! !...part b ..... simple way....probably won't work...
    ! write(*,*) 'vals(1)%str = ' // vals(1)%str
    ! write(*,*) 'vals(2)%str = ' // vals(2)%str
    ! tmp1 = vals(1)%str
    ! tmp2 = vals(2)%str
    ! do j = 1 , 4
    !     vals(1)%str = vals(1)%str//'?'//tmp1
    !     vals(2)%str = vals(2)%str//','//tmp2
    ! end do
    ! write(*,*) 'vals(1)%str = ' // vals(1)%str
    ! write(*,*) 'vals(2)%str = ' // vals(2)%str
    ! !stop

    !......


    ints = parse_ints(vals(2)%str)     ! integer list  1,1,3
    pattern = vals(1)%str              ! the pattern   #.#.###
    call replace(pattern, '.', ' ')
    pattern = trim(adjustl(pattern)) ! trim leading the trailing spaces

    !write(*,*) 'vals(2)%str = ', vals(2)%str
    !ints = int(split(vals(2)%str, ','))

    ! print *, 'line : '//line
    ! print *, 'pattern : '//pattern
    ! write(*,*) 'ints = ', ints

    n_unknowns = 0
    do j = 1, len(line)
        if (line(j:j)=='?') n_unknowns = n_unknowns + 1
    end do

    n_valid = 0 ! number of valid permutations

    ! each ? can be either a . or a #
    ! check pattern to match the int list

    n_perms = 2 ** n_unknowns ! number of permutations
    !write(*,*) 'n_unknowns = ', n_unknowns
    !write(*,*) 'n_perms = ', n_perms

    if (allocated(a)) deallocate(a)
    allocate(a(n_unknowns))
    call test(1, n_unknowns)

    !write(*,*) 'nvalid = ', n_valid

    isum = isum + n_valid

end do
write(*,*) '12a: ', isum

! write(*,*) '12b: '

call clk%toc('12')

contains

    recursive subroutine test (i, n)
    integer, intent(in) :: i, n
    integer :: ix, m, k
    integer,dimension(*),parameter :: icoeffs = [0,1] !! set of coefficients
    character(len=:),allocatable :: tmp

    if (i > n) then
        ! so we have an array of 0s and 1s
        ! convert them to .s and #s
        k = 0
        tmp = pattern
        do m = 1, len(tmp)
            if (tmp(m:m)=='?') then
                k = k + 1
                if (a(k)==0) then
                    tmp(m:m) = ' '
                else
                    tmp(m:m) = '#'
                end if
            end if
        end do
        tmp = trim(adjustl(tmp))
        ! write(*,*) 'perm: ', a
        ! write(*,*) 'try:  '//tmp
        ! write(*,*) 'tmp:  ->'//tmp//'<-'
        ! write(*,*) 'ints: ',ints
        if (match(tmp, ints)) then
           ! write(*,*) 'MATCH!'
            n_valid = n_valid + 1
        end if
    else
        do ix = 1,size(icoeffs)
            a(i) = icoeffs(ix)
            call test(i+1, n)
        end do
    end if
    end subroutine test

    subroutine replace(str, c1, c2)
        !! replace all c1 in str with c2
        character(Len=*),intent(inout) :: str
        character(len=1),intent(in) :: c1, c2
        integer :: i
        do i = 1, len(str)
            if (str(i:i)==c1) str(i:i) = c2
        end do
    end subroutine replace

    logical function match(line, ints)
        character(Len=:),allocatable,intent(inout) :: line
        integer,dimension(:),intent(in) :: ints
        type(string),dimension(:),allocatable :: vals
        integer :: i
        integer,dimension(:),allocatable :: itmp

        ! replaces any consecutive spaces with one space
        do
            if (index(line, '  ')>0) then
                call replace_alloc(line,'  ',' ')
            else
                exit
            end if
        end do
        vals = split(line,' ')
        match = size(vals) == size(ints)
        if (match) then
            allocate(itmp(size(vals)))
            do i = 1, size(vals)
                itmp(i) = len(vals(i)%str)   ! # # ###
            end do
            match = all(itmp == ints)
        end if

    end function match

    subroutine replace_alloc(s,s1,s2)
        !! replace s1 in s with s2. s is allocatable and will be resized if necessary.

        character(len=:),allocatable,intent(inout) :: s
        character(len=*),intent(in) :: s1
        character(len=*),intent(in) :: s2

        character(len=:),allocatable :: s_out, s_tmp
        integer :: i,ilen,il1,il2
        integer :: iloc

        s_out = s
        il1   = len(s1)
        il2   = len(s2)
        i     = 1
        do
            ilen = len(s_out)
            iloc = index(s_out(i:ilen),s1)
            if (iloc<1) exit   ! done
            iloc = iloc + i-1
                                s_tmp = ''
            if (iloc>1)         s_tmp = s_out(1:iloc-1)
                                s_tmp = s_tmp//s2
            if (iloc+il1<=ilen) s_tmp = s_tmp//s_out(iloc+il1:ilen)
            s_out = s_tmp
            i = iloc + il2
        end do
        s = s_out

    end subroutine replace_alloc

end program problem_12