program problem_14

use iso_fortran_env, only: ip => int64 ! use long ints
use aoc_utilities
use aoc_cache_module

implicit none

integer(ip) :: i, nrows, ncols, iload, icycle
character(len=1),dimension(:,:),allocatable :: array
type(function_cache) :: cache !! to cache the [[go]] function values
logical :: found
integer(ip) :: idx
integer(ip),dimension(:),allocatable :: iarray

call clk%tic()

! read the data file:
! array = read_file_to_char_array('inputs/day14_test.txt', border='#') ! pad
array = read_file_to_char_array('inputs/day14.txt', border='#')

call move_up(array)
nrows = size(array,1)
ncols = size(array,2)
! get load:
iload = 0
do i = 2, nrows-1
    iload = iload + count(array(i,:)=='O')*(nrows-i)
end do
write(*,*) '14a: ', iload

call cache%initialize(isize=10000,chunk_size=1000)

! to solve:
!   * run until cache is hit - find cycle where that happend (144)
!   * run for 2 * number of cycles where cache was hit - examine pattern
!   * the number of moves before cycle starts (122)
!   * the lenght of each cycle (21)
!
! 122 + 21*x = 1000000000 => (1000000000-122)/21 = 47619041.809523806 ==> 21*.809523806 = 17

!... code up the above... TODO

!do icycle = 1, 1000000000
do icycle = 1, 17+122   !<-----see above!!
    iarray = reshape(ichar(array,ip),[nrows*ncols])

    !call cache%get(iarray,idx,iload,found)
    found = .false.
    if (found) then
        !write(*,*) 'icycle,iload = ', icycle, iload
        stop
        !cycle
    else

        call move_up(array) ! north
        call rotate(array)
        call move_up(array) ! west
        call rotate(array)
        call move_up(array) ! south
        call rotate(array)
        call move_up(array) ! east
        call rotate(array)

        iload = 0
        do i = 2, nrows-1
            iload = iload + count(array(i,:)=='O')*(nrows-i)
        end do

       !call cache%put(idx,iarray,iload)   !<-----
    end if
    !write(*,*) 'icycle,iload = ', icycle, iload
end do
iload = 0
do i = 2, nrows-1
    iload = iload + count(array(i,:)=='O')*(nrows-i)
end do
write(*,*) '14b: ', iload

call clk%toc('14')

contains

    subroutine rotate(array)
        character(len=1),dimension(:,:),intent(inout) :: array ! always a square matrix
        integer :: n
        n = size(array,1)
        array = transpose(array)
        do i = 1, n/2 ! now reverse the columns
            call swap(array(:,i), array(:,n-i+1))
        end do
    end subroutine rotate

    subroutine move_up(array)
        character(len=1),dimension(:,:),intent(inout) :: array
        ! BUBBLE SORT BABY
        ! move all the O's up
        integer :: i, j, nrows, ncols
        logical :: done
        nrows = size(array,1)
        ncols = size(array,2)
        do
            done = .true.
            do i = 2, nrows-1
                do j = 2, ncols-1
                    if (array(i,j)=='O' .and. array(i-1,j)=='.') then ! can move this one up
                        array(i-1,j) = 'O'
                        array(i,j) = '.'
                        done = .false.
                    end if
                end do
            end do
            if (done) exit
        end do
    end subroutine move_up

end program problem_14