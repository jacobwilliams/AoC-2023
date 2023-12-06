module aoc_utilities

    use iso_fortran_env

    implicit none

    private

    integer,parameter :: chunk_size = 100 !! for dynamic allocations
    character(len=*),parameter :: digits = '0123456789'

    type,public :: string
        character(len=:),allocatable :: str
        contains
        procedure,public :: to_int => string_to_int
    end type string

    public :: read_file_to_integer_array, &
              read_file_to_integer64_array, &
              read_file_to_char_array
    public :: number_of_lines_in_file
    public :: sort_ascending,sort_ascending_64
    public :: read_line
    public :: parse_ints, parse_ints64

    interface split
        procedure :: split1, split2
    end interface split
    public :: split

    interface int
        procedure :: string_to_int, char_to_int, char_array_to_int, char_to_int64
    end interface int
    public :: int

    interface unique
        procedure :: unique32, unique64
    end interface unique
    public :: unique

    interface startswith
        !! test if a string starts with a specified substring
        procedure :: startswith_cc, startswith_ss, startswith_sc, startswith_cs
    end interface startswith
    public :: startswith

    public :: swap

    public :: is_number, is_not_number

contains

!****************************************************************
    pure function char_to_int(str) result(i)
    !! basic string to integer routine
    implicit none
    character(len=*),intent(in) :: str
    integer :: i
    read(str,*) i
    end function char_to_int
    pure function string_to_int(me) result(i)
    !! basic string to integer routine
    implicit none
    class(string),intent(in) :: me
    integer :: i
    i = char_to_int(me%str)
    end function string_to_int
    pure function char_to_int64(str, kind) result(i)
    !! basic string to integer(ip) routine. hacky hack just so we can overload as int()
    implicit none
    character(len=*),intent(in) :: str
    integer,intent(in) :: kind
    integer(int64) :: i
    if (kind/=int64) error stop 'error'
    read(str,*) i
    end function char_to_int64
    pure function char_array_to_int(str_array) result(i)
    !! character array to integer routine
    implicit none
    character(len=1),dimension(:),intent(in) :: str_array !! example ['1','3'] --> 13
    integer :: i
    character(len=:),allocatable :: str
    integer :: k
    str = ''
    do k = 1, size(str_array)
        str = str//str_array(k)
    end do
    i = char_to_int(str)
    end function char_array_to_int

    function read_file_to_char_array(filename, border) result(array)
        !! read a file into a 2d character array.
        character(len=*),intent(in) :: filename
        character(len=1),intent(in),optional :: border !! if true, extra border is added with this char
        character(len=1),dimension(:,:),allocatable :: array

        integer :: i, j, iunit, n_lines, n_cols
        character(len=:),allocatable :: line

        open(newunit=iunit, file=filename, status='OLD')
        n_lines = number_of_lines_in_file(iunit)
        line = read_line(iunit); n_cols = len(line)
        rewind(iunit)

        if (present(border)) then
            allocate(array(0:n_lines+1, 0:n_cols+1)) ! padding with border
            array = border
        else
            allocate(array(n_lines, n_cols))
        end if

        do i = 1, n_lines
            line = read_line(iunit)
            do j = 1, n_cols
                array(i,j) = line(j:j)
            end do
        end do
        close(iunit)

    end function read_file_to_char_array

!****************************************************************
    function read_file_to_integer_array(filename) result(iarray)

    character(len=*),intent(in) :: filename
    integer,dimension(:),allocatable :: iarray

    integer :: i, iunit, n_lines, istat

    open(newunit=iunit, file=filename, iostat=istat)
    if (istat /= 0) error stop ' error reading file'

    n_lines = number_of_lines_in_file(iunit)
    allocate(iarray(n_lines))
    do i = 1, n_lines
        read(iunit, '(I10)') iarray(i)
    end do

    close(iunit)

    end function read_file_to_integer_array
!****************************************************************

!****************************************************************
    function read_file_to_integer64_array(filename) result(iarray)

    character(len=*),intent(in) :: filename
    integer(int64),dimension(:),allocatable :: iarray

    integer :: i, iunit, n_lines, istat

    open(newunit=iunit, file=filename, iostat=istat)
    if (istat /= 0) error stop ' error reading file'

    n_lines = number_of_lines_in_file(iunit)
    allocate(iarray(n_lines))
    do i = 1, n_lines
        read(iunit, *) iarray(i)
    end do

    close(iunit)

    end function read_file_to_integer64_array
!****************************************************************

!****************************************************************
    function number_of_lines_in_file(iunit) result(n_lines)

    implicit none

    integer,intent(in)  :: iunit   !! the file unit number
                                    !! (assumed to be open)
    integer :: n_lines   !! the number of lines in the file

    character(len=1) :: tmp
    integer :: istat

    rewind(iunit)
    n_lines = 0
    do
        read(iunit,fmt='(A1)',iostat=istat) tmp
        if (is_iostat_end(istat)) exit
        n_lines = n_lines + 1
    end do
    rewind(iunit)

    end function number_of_lines_in_file
!****************************************************************

!*******************************************************************************
!>
!  Sorts an integer array `ivec` in increasing order.
!  Uses a basic recursive quicksort
!  (with insertion sort for partitions with \(\le\) 20 elements).

    subroutine sort_ascending(ivec)

    implicit none

    integer,dimension(:),intent(inout) :: ivec

    integer,parameter :: max_size_for_insertion_sort = 20 !! max size for using insertion sort.

    call quicksort(1,size(ivec))

    contains

        recursive subroutine quicksort(ilow,ihigh)

        !! Sort the array

        implicit none

        integer,intent(in) :: ilow
        integer,intent(in) :: ihigh

        integer :: ipivot !! pivot element
        integer :: i      !! counter
        integer :: j      !! counter

        if ( ihigh-ilow<=max_size_for_insertion_sort .and. ihigh>ilow ) then

            ! do insertion sort:
            do i = ilow + 1,ihigh
                do j = i,ilow + 1,-1
                    if ( ivec(j) < ivec(j-1) ) then
                        call swap(ivec(j),ivec(j-1))
                    else
                        exit
                    end if
                end do
            end do

        elseif ( ihigh-ilow>max_size_for_insertion_sort ) then

            ! do the normal quicksort:
            call partition(ilow,ihigh,ipivot)
            call quicksort(ilow,ipivot - 1)
            call quicksort(ipivot + 1,ihigh)

        end if

        end subroutine quicksort

        subroutine partition(ilow,ihigh,ipivot)

        !! Partition the array, based on the
        !! lexical ivecing comparison.

        implicit none

        integer,intent(in)  :: ilow
        integer,intent(in)  :: ihigh
        integer,intent(out) :: ipivot

        integer :: i,ip

        call swap(ivec(ilow),ivec((ilow+ihigh)/2))
        ip = ilow
        do i = ilow + 1, ihigh
            if ( ivec(i) < ivec(ilow) ) then
                ip = ip + 1
                call swap(ivec(ip),ivec(i))
            end if
        end do
        call swap(ivec(ilow),ivec(ip))
        ipivot = ip

        end subroutine partition

    end subroutine sort_ascending
!*******************************************************************************


!*******************************************************************************
!>
    subroutine sort_ascending_64(ivec)

    implicit none

    integer(int64),dimension(:),intent(inout) :: ivec

    integer(int64),parameter :: max_size_for_insertion_sort = 20 !! max size for using insertion sort.

    call quicksort(1_int64,size(ivec,kind=int64))

    contains

        recursive subroutine quicksort(ilow,ihigh)

        !! Sort the array

        implicit none

        integer(int64),intent(in) :: ilow
        integer(int64),intent(in) :: ihigh

        integer(int64) :: ipivot !! pivot element
        integer(int64) :: i      !! counter
        integer(int64) :: j      !! counter

        if ( ihigh-ilow<=max_size_for_insertion_sort .and. ihigh>ilow ) then

            ! do insertion sort:
            do i = ilow + 1,ihigh
                do j = i,ilow + 1,-1
                    if ( ivec(j) < ivec(j-1) ) then
                        call swap64(ivec(j),ivec(j-1))
                    else
                        exit
                    end if
                end do
            end do

        elseif ( ihigh-ilow>max_size_for_insertion_sort ) then

            ! do the normal quicksort:
            call partition(ilow,ihigh,ipivot)
            call quicksort(ilow,ipivot - 1)
            call quicksort(ipivot + 1,ihigh)

        end if

        end subroutine quicksort

        subroutine partition(ilow,ihigh,ipivot)

        !! Partition the array, based on the
        !! lexical ivecing comparison.

        implicit none

        integer(int64),intent(in)  :: ilow
        integer(int64),intent(in)  :: ihigh
        integer(int64),intent(out) :: ipivot

        integer(int64) :: i,ip

        call swap64(ivec(ilow),ivec((ilow+ihigh)/2))
        ip = ilow
        do i = ilow + 1, ihigh
            if ( ivec(i) < ivec(ilow) ) then
                ip = ip + 1
                call swap64(ivec(ip),ivec(i))
            end if
        end do
        call swap64(ivec(ilow),ivec(ip))
        ipivot = ip

        end subroutine partition

    end subroutine sort_ascending_64
!*******************************************************************************
!*******************************************************************************
!>
!  Swap two integer values.

    pure elemental subroutine swap(i1,i2)

    implicit none

    integer,intent(inout) :: i1
    integer,intent(inout) :: i2

    integer :: tmp

    tmp = i1
    i1  = i2
    i2  = tmp

    end subroutine swap
!*******************************************************************************
!*******************************************************************************
!>
!  Swap two integer values.

    pure elemental subroutine swap64(i1,i2)

    implicit none

    integer(int64),intent(inout) :: i1
    integer(int64),intent(inout) :: i2

    integer(int64) :: tmp

    tmp = i1
    i1  = i2
    i2  = tmp

    end subroutine swap64
!*******************************************************************************

!*******************************************************************************

    pure function split2(s,token) result(vals)

    implicit none

    type(string),intent(in)  :: s
    character(len=*),intent(in)  :: token
    type(string),dimension(:),allocatable:: vals

    if (allocated(s%str)) then
        vals = split1(s%str,token)
    else
        error stop 'error: string not allocated'
    end if

    end function split2
!*******************************************************************************

!*****************************************************************************************
!>
!  Split a character string using a token.
!  This routine is inspired by the Python split function.
!
!### Example
!````Fortran
!   character(len=:),allocatable :: s
!   type(string),dimension(:),allocatable :: vals
!   s = '1,2,3,4,5'
!   call split(s,',',vals)
!````

    pure function split1(str,token) result(vals)

    implicit none

    character(len=*),intent(in)  :: str
    character(len=*),intent(in)  :: token
    type(string),dimension(:),allocatable :: vals

    integer :: i          !! counter
    integer :: len_str    !! significant length of `str`
    integer :: len_token  !! length of the token
    integer :: n_tokens   !! number of tokens
    integer :: i1         !! index
    integer :: i2         !! index
    integer :: j          !! counters
    integer,dimension(:),allocatable :: itokens !! start indices of the
                                                !! token locations in `str`

    len_token = len(token)  ! length of the token
    n_tokens  = 0           ! initialize the token counter
    j         = 0           ! index to start looking for the next token

    ! first, count the number of times the token
    ! appears in the string, and get the token indices.
    !
    ! Examples:
    !  ',         '    --> 1
    !  '1234,67,90'    --> 5,8
    !  '123,      '    --> 4

    ! length of the string
    if (token == ' ') then
        ! in this case, we can't ignore trailing space
        len_str = len(str)
    else
        ! safe to ignore trailing space when looking for tokens
        len_str = len_trim(str)
    end if

    j = 1
    n_tokens = 0
    do
        if (j>len_str) exit      ! end of string, finished
        i = index(str(j:),token) ! index of next token in remaining string
        if (i<=0) exit           ! no more tokens found
        call expand_vector(itokens,n_tokens,i+j-1)  ! save the token location
        j = j + i + (len_token - 1)
    end do
    call expand_vector(itokens,n_tokens,finished=.true.)  ! resize the vector

    allocate(vals(n_tokens+1))

    if (n_tokens>0) then

        len_str = len(str)

        i1 = 1
        i2 = itokens(1)-1
        if (i2>=i1) then
            vals(1)%str = str(i1:i2)
        else
            vals(1)%str = ''  !the first character is a token
        end if

        !      1 2 3
        !    'a,b,c,d'

        do i=2,n_tokens
            i1 = itokens(i-1)+len_token
            i2 = itokens(i)-1
            if (i2>=i1) then
                vals(i)%str = str(i1:i2)
            else
                vals(i)%str = ''  !empty element (e.g., 'abc,,def')
            end if
        end do

        i1 = itokens(n_tokens) + len_token
        i2 = len_str
        if (itokens(n_tokens)+len_token<=len_str) then
            vals(n_tokens+1)%str = str(i1:i2)
        else
            vals(n_tokens+1)%str = ''  !the last character was a token
        end if

    else
        !no tokens present, so just return the original string:
        vals(1)%str = str
    end if

    end function split1
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add elements to the integer vector in chunks.

    pure subroutine expand_vector(vec,n,val,finished)

    implicit none

    integer,dimension(:),allocatable,intent(inout) :: vec
    integer,intent(inout)       :: n           !! counter for last element added to `vec`.
                                               !! must be initialized to `size(vec)`
                                               !! (or 0 if not allocated) before first call
    integer,intent(in),optional :: val         !! the value to add to `vec`
    logical,intent(in),optional :: finished    !! set to true to return `vec`
                                               !! as its correct size (`n`)

    integer,dimension(:),allocatable :: tmp  !! temporary array

    if (present(val)) then
        if (allocated(vec)) then
            if (n==size(vec)) then
                ! have to add another chunk:
                allocate(tmp(size(vec)+chunk_size))
                tmp(1:size(vec)) = vec
                call move_alloc(tmp,vec)
            end if
            n = n + 1
        else
            ! the first element:
            allocate(vec(chunk_size))
            n = 1
        end if
        vec(n) = val
    end if

    if (present(finished)) then
        if (finished) then
            ! set vec to actual size (n):
            if (allocated(tmp)) deallocate(tmp)
            allocate(tmp(n))
            tmp = vec(1:n)
            call move_alloc(tmp,vec)
        end if
    end if

    end subroutine expand_vector
!*******************************************************************************

!*****************************************************************************************
!>
!  Reads the next line from a file.

    function read_line(iunit,status_ok) result(line)

    implicit none

    integer,intent(in) :: iunit
    character(len=:),allocatable :: line
    logical,intent(out),optional :: status_ok !! true if no problems

    integer :: nread  !! character count specifier for read statement
    integer :: istat  !! file read io status flag
    character(len=chunk_size) :: buffer !! the file read buffer

    nread  = 0
    buffer = ''
    line   = ''
    if (present(status_ok)) status_ok = .true.

    do
        ! read in the next block of text from the line:
        read(iunit,fmt='(A)',advance='NO',size=nread,iostat=istat) buffer
        if (IS_IOSTAT_END(istat) .or. IS_IOSTAT_EOR(istat)) then
            ! add the last block of text before the end of record
            if (nread>0) line = line//buffer(1:nread)
            exit
        else if (istat==0) then ! all the characters were read
            line = line//buffer  ! add this block of text to the string
        else  ! some kind of error
            if (present(status_ok)) then
                status_ok = .false.
                exit
            else
                error stop 'Read error'
            end if
        end if
    end do

    end function read_line
!*****************************************************************************************

!*****************************************************************************************
function unique32(vec) result(vec_unique)
! Return only the unique values from vec.

implicit none

integer,dimension(:),intent(in) :: vec
integer,dimension(:),allocatable :: vec_unique

integer :: i,num
logical,dimension(size(vec)) :: mask

mask = .false.

do i=1,size(vec)

    !count the number of occurrences of this element:
    num = count( vec(i)==vec )

    if (num==1) then
        !there is only one, flag it:
        mask(i) = .true.
    else
        !flag this value only if it hasn't already been flagged:
        if (.not. any(vec(i)==vec .and. mask) ) mask(i) = .true.
    end if

end do

!return only flagged elements:
allocate( vec_unique(count(mask)) )
vec_unique = pack( vec, mask )

!if you also need it sorted, then do so.
! For example, with slatec routine:
!call ISORT (vec_unique, [0], size(vec_unique), 1)

end function unique32
!*****************************************************************************************

!*****************************************************************************************
function unique64(vec) result(vec_unique)
! Return only the unique values from vec.

implicit none

integer(int64),dimension(:),intent(in) :: vec
integer(int64),dimension(:),allocatable :: vec_unique

integer(int64) :: i,num
logical,dimension(size(vec)) :: mask

mask = .false.

do i=1,size(vec)

    !count the number of occurrences of this element:
    num = count( vec(i)==vec )

    if (num==1) then
        !there is only one, flag it:
        mask(i) = .true.
    else
        !flag this value only if it hasn't already been flagged:
        if (.not. any(vec(i)==vec .and. mask) ) mask(i) = .true.
    end if

end do

!return only flagged elements:
allocate( vec_unique(count(mask)) )
vec_unique = pack( vec, mask )

!if you also need it sorted, then do so.
! For example, with slatec routine:
!call ISORT (vec_unique, [0], size(vec_unique), 1)

end function unique64
!*****************************************************************************************

function parse_ints(line) result(ints)
    ! parse positive ints from a string that also includes text
    character(len=*),intent(in) :: line
    integer,dimension(:),allocatable :: ints ! array of integers
    integer :: i, j, n
    integer :: istart
    character(len=*),parameter :: tokens = '0123456789'

    n = len(line)
    istart = 0
    allocate(ints(0))

    do i = 1, n
        if (index(tokens,line(i:i))>0) then
            if (istart==0) istart = i
        else
            if (istart/=0) ints = [ints, int(line(istart:i-1))] ! get previous int
            istart = 0
        end if
    end do
    if (istart/=0) ints = [ints, int(line(istart:n))] ! get last int

end function parse_ints
!*****************************************************************************************

function parse_ints64(line) result(ints)
    ! parse positive ints from a string that also includes text
    character(len=*),intent(in) :: line
    integer(int64),dimension(:),allocatable :: ints ! array of integers
    integer(int64) :: i, j, n
    integer(int64) :: istart
    character(len=*),parameter :: tokens = '0123456789'

    n = len(line)
    istart = 0
    allocate(ints(0))

    do i = 1, n
        if (index(tokens,line(i:i))>0) then
            if (istart==0) istart = i
        else
            if (istart/=0) ints = [ints, int(line(istart:i-1), kind=int64)] ! get previous int
            istart = 0
        end if
    end do
    if (istart/=0) ints = [ints, int(line(istart:n), kind=int64)] ! get last int

end function parse_ints64
!*****************************************************************************************

!*****************************************************************************************
! starts with function for strings
    pure logical function startswith_cc(str, substring)
        character(len=*),intent(in) :: str, substring
        startswith_cc = index(str, substring) == 1
    end function startswith_cc
    pure logical function startswith_ss(str, substring)
        type(string),intent(in) :: str, substring
        startswith_ss = startswith(str%str, substring%str)
    end function startswith_ss
    pure logical function startswith_sc(str, substring)
        type(string),intent(in) :: str
        character(len=*),intent(in) :: substring
        startswith_sc = startswith(str%str, substring)
    end function startswith_sc
    pure logical function startswith_cs(str, substring)
        character(len=*),intent(in) :: str
        type(string),intent(in) :: substring
        startswith_cs = startswith(str, substring%str)
    end function startswith_cs
!*****************************************************************************************

    logical function is_number(c)
    character(len=1),intent(in) :: c
    is_number = c >= '0' .and. c <= '9'
    end function is_number

    logical function is_not_number(c)
    character(len=1),intent(in) :: c
    is_not_number = .not. is_number(c)
    end function is_not_number



end module aoc_utilities