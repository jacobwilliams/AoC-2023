!*******************************************************************************
!> author: Jacob Williams
!
!  For caching function evaluations.
!
!  This is based on the cache module from `NumDiff`.
!  It has been modified to cache integers (`int64`) instead of reals.

    module aoc_cache_module

    use iso_fortran_env, only: ip => int64, wp => real64

    implicit none

    private

    type :: fx
        !! an [x,f(x)] cached pair.
        !! x is a vector and f is a scalar value.
        private
        integer(ip),dimension(:),allocatable :: x    !! vector of input values
        integer(ip) :: f    !! scalar output function
    end type fx

    type,public :: function_cache
        !! a vector function cache.
        private
        type(fx),dimension(:),allocatable :: c  !! the cache of `f(x)`
        integer :: chunk_size = 100 !! for resizing vectors
                                    !! in the [[unique]] function
    contains
        private
        procedure,public :: initialize => initialize_cache
        procedure,public :: get        => get_from_cache
        procedure,public :: put        => put_in_cache
        procedure,public :: destroy    => destroy_cache
    end type function_cache

    contains
!*******************************************************************************

!*******************************************************************************
!>
!  Initialize the cache. Must be called first before use.

    subroutine initialize_cache(me,isize,chunk_size)

    implicit none

    class(function_cache),intent(inout) :: me
    integer,intent(in) :: isize !! the size of the hash table
    integer,intent(in),optional :: chunk_size  !! chunk size to speed up reallocation
                                               !! of arrays. A good value is a guess for
                                               !! the actual number of elements of `f` that
                                               !! will be saved per value of `x` [default is 100]

    call me%destroy()

    allocate(me%c(0:isize-1))

    if (present(chunk_size)) then
        me%chunk_size = chunk_size
    else
        me%chunk_size = 100
    end if

    end subroutine initialize_cache
!*******************************************************************************

!*******************************************************************************
!>
!  Check if the `x` vector is in the cache, if so return `f`.
!  Note that only some of the elements may be present, so it will return
!  the ones there are there, and indicate which ones were found.

    subroutine get_from_cache(me,x,i,f,found)

    implicit none

    class(function_cache),intent(inout)      :: me
    integer(ip),dimension(:),intent(in)      :: x      !! independant variable vector
    integer(ip),intent(out)                  :: i      !! index in the hash table
    integer(ip),intent(out)                  :: f      !! `f(x)` from the cache (if it was found)
    logical,intent(out)                      :: found  !! if `x` was found in the cache

    integer :: j !! counter

    ! initialize:
    found = .false.

    if (allocated(me%c)) then

        ! get index in the hash table:
        i = mod( abs(vector_djb_hash(x)), int(size(me%c),ip) )

        ! check the table:
        if (allocated(me%c(i)%x)) then
            if (size(me%c(i)%x)==size(x)) then
                if (all(me%c(i)%x==x)) then
                    found = .true.
                    f = me%c(i)%f
                end if
            end if
        end if

    else
        error stop 'Error: the cache has not been initialized.'
    end if

    end subroutine get_from_cache
!*******************************************************************************

!*******************************************************************************
!>
!  Put a value into the cache.

    subroutine put_in_cache(me,i,x,f)

    implicit none

    class(function_cache),intent(inout) :: me
    integer(ip),intent(in)              :: i    !! index in the hash table
    integer(ip),dimension(:),intent(in) :: x    !! independant variable vector
    integer(ip),intent(in)              :: f    !! function

    integer(ip),parameter :: null = huge(1) !! an unusual value to initialize arrays

    if (allocated(me%c)) then
        if (i<=size(me%c)) then
            ! add to the cache
            me%c(i)%x = x
            me%c(i)%f = f
        else
            error stop 'Error: invalid index in hash table.'
        end if
    else
        error stop 'Error: the cache has not been initialized.'
    end if

    end subroutine put_in_cache
!*******************************************************************************

!*******************************************************************************
!>
!  Destroy a cache.

    subroutine destroy_cache(me)

    implicit none

    class(function_cache),intent(out) :: me

    end subroutine destroy_cache
!*******************************************************************************

!*******************************************************************************
!>
!  DJB hash algorithm for a `integer(ip)` vector.
!
!### See also
!  * J. Shahbazian, Fortran hashing algorithm, July 6, 2013
!   [Fortran Dev](https://fortrandev.wordpress.com/2013/07/06/fortran-hashing-algorithm/)

    pure function vector_djb_hash(r) result(hash)

    integer(ip),dimension(:),intent(in) :: r     !! the vector
    integer(ip)                         :: hash  !! the hash value

    integer :: i !! counter

    hash = 5381_ip

    do i=1,size(r)
        hash = ishft(hash,5_ip) + hash + r(i)
    end do

    end function vector_djb_hash
!*******************************************************************************

!*******************************************************************************
    end module aoc_cache_module
!*******************************************************************************
