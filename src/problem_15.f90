!*********************************************************************************
!>
!  A module for problem 15, part 2.

module problem_15_module
    use iso_fortran_env, only: ip => int64 ! use long ints
    use aoc_utilities

    implicit none

    type :: lens
        character(len=:),allocatable :: label
        integer(ip) :: focal_length = 0
    end type lens

    type :: box
        type(lens),dimension(:),allocatable :: lenses !! the lenses in the box
        contains
        procedure :: add_lens
        procedure :: remove_lens
    end type box

    contains

    subroutine add_lens(me,l) ! =
        class(box),intent(inout) :: me
        type(lens),intent(in) :: l !! lense to add
        integer :: i !! counter
        if (allocated(me%lenses)) then
            do i = 1, size(me%lenses)
                if (me%lenses(i)%label == l%label) then
                    me%lenses(i) = l  ! replace with this one
                    return
                end if
            end do
            me%lenses = [me%lenses, l]  ! if not found, add to end
        else
            me%lenses = [l]  ! first one in the box
        end if
    end subroutine add_lens

    subroutine remove_lens(me,label) ! -
        !! remove the lens with the label (if present)
        class(box),intent(inout) :: me
        character(len=*),intent(in) :: label
        integer :: i
        type(lens),dimension(:),allocatable :: tmp
        if (allocated(me%lenses)) then
            allocate(tmp(0))
            do i = 1, size(me%lenses)
                if (me%lenses(i)%label/=label) tmp = [tmp, me%lenses(i)]
            end do
            call move_alloc(tmp, me%lenses)
        end if
    end subroutine remove_lens

end module problem_15_module
!*********************************************************************************

program problem_15

use iso_fortran_env, only: ip => int64 ! use long ints
use aoc_utilities
use problem_15_module

implicit none

integer :: i, iunit, j
type(string),dimension(:),allocatable :: vals, vals2
integer(ip),dimension(:),allocatable :: ivals

type(box),dimension(0:255) :: boxes ! for part 2
integer(ip) :: ibox ! 0-255
character(len=:),allocatable :: label
integer(ip) :: ival, focusing_power, lens_focusing_power

call clk%tic()

! read the data file:
! open(newunit=iunit, file='inputs/day15_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day15.txt', status='OLD')
vals = split(read_line(iunit),',')
close(iunit)

allocate(ivals(size(vals)))
do i = 1, size(vals)
    ! part 1:
    ivals(i) = hash(vals(i)%str)
    ! part 2:
    if (index(vals(i)%str,'=')>0) then
        vals2 = split(vals(i)%str, '=')
        label = vals2(1)%str
        ibox = hash(label)
        ival = int(vals2(2)%str) ! focal length
        call boxes(ibox)%add_lens(lens(label,ival))
    else if (index(vals(i)%str,'-')>0) then
        vals2 = split(vals(i)%str, '-')
        label = vals2(1)%str
        ibox = hash(label)
        call boxes(ibox)%remove_lens(label)
    end if
end do
write(*,*) '15a: ', sum(ivals)

focusing_power = 0
do i = 0, 255 ! all the boxes
    if (allocated(boxes(i)%lenses)) then
        do j = 1, size(boxes(i)%lenses) ! all the lenses in the box
            lens_focusing_power = (i+1)*(j)*(boxes(i)%lenses(j)%focal_length)
            focusing_power = focusing_power + lens_focusing_power
        end do
    end if
end do
write(*,*) '15b: ', focusing_power

call clk%toc('15')

contains

    pure elemental integer(ip) function hash(s)
        !! Determine the ASCII code for the current character of the string.
        character(len=*),intent(in) :: s
        integer :: i
        hash = 0
        do i = 1, len(s)
            hash = modulo(17_ip*( hash + iachar(s(i:i))), 256_ip)
        end do
    end function hash

end program problem_15