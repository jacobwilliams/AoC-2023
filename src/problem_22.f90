program problem_22

use iso_fortran_env, only: wp => real64, ip => int64 ! use long ints
use aoc_utilities

implicit none

integer :: iunit, n_lines
integer(ip) :: i, j
character(len=:),allocatable :: line
type(string),dimension(:),allocatable :: start_end, starts, ends
integer(ip),dimension(:,:),allocatable :: istart_array
integer(ip),dimension(:,:),allocatable :: iend_array
integer(ip),dimension(:,:,:),allocatable :: array
integer(ip),dimension(:),allocatable :: ipieces_above, ipieces_below
integer(ip) :: ok_to_disintegrate
logical :: moved, tmp_moved, ok_tmp

call clk%tic()

! read the data:
! open(newunit=iunit, file='inputs/day22_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day22.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
allocate(istart_array(n_lines,3))
allocate(iend_array(n_lines,3))
do i = 1, n_lines
    line = read_line(iunit); start_end = split(line, '~')
    starts = split(start_end(1)%str, ','); ends = split(start_end(2)%str, ',')
    istart_array(i,:) = [(int(starts(j)%str), j = 1, 3)]
    iend_array(i,:)   = [(int(ends(j)%str), j = 1, 3)]
end do
close(iunit)
! create the array to hold all the pieces:
allocate(array(0:maxval([istart_array(:,1), iend_array(:,1)]),&
               0:maxval([istart_array(:,2), iend_array(:,2)]),&
               0:maxval([istart_array(:,3), iend_array(:,3)])))
call update_array()

!continue forward in time until all the bricks settle.
do
    moved = .false.
    do i = 1, n_lines
        call move_piece_down(i, tmp_moved)
        if (tmp_moved) moved = .true. ! at least one was moved
    end do
    if (.not. moved) exit ! done moving all the pieces
end do

!find the ones we can remove without any other falling:
!  to do that, look to see if any above it are not otherwise held up by another piece
ok_to_disintegrate = 0
do i = 1, n_lines
    ipieces_above = get_pieces_above(i)
    if (size(ipieces_above)==0) then
        ! nothing above, it so ok to remove
        ok_to_disintegrate = ok_to_disintegrate + 1
    else
        ok_tmp = .true.
        do j = 1, size(ipieces_above) ! check all the pieces above. if ALL have other supporters then OK to remove
            ipieces_below = get_pieces_below(ipieces_above(j))
            if (all(ipieces_below==i)) then ! only supported by i , so can't remove i
                ok_tmp = .false.
                exit
            end if
        end do
        if (ok_tmp) ok_to_disintegrate = ok_to_disintegrate + 1
    end if
end do
write(*,*) '22a: ', ok_to_disintegrate

call clk%toc('22')

contains

    subroutine update_array()
        !! populate the array using the start/end indices
        integer :: i
        array = 0
        do i = 1, n_lines
            array(istart_array(i,1):iend_array(i,1),&
                  istart_array(i,2):iend_array(i,2),&
                  istart_array(i,3):iend_array(i,3)) = i
        end do
    end subroutine update_array

    function get_pieces_above(i) result(ipieces)
        !! get set of pieces above piece i
        integer(ip),intent(in) :: i
        integer(ip),dimension(:),allocatable :: ipieces
        integer(ip) :: x,y,z !! counter
        allocate(ipieces(0))
        do x = istart_array(i,1), iend_array(i,1)
            do y = istart_array(i,2), iend_array(i,2)
                do z = istart_array(i,3), iend_array(i,3)
                    if (z+1<=ubound(array,3)) ipieces = [ipieces,array(x,y,z+1)] ! above this one
                end do
            end do
        end do
        ipieces = unique(pack(ipieces, ipieces/=i .and. ipieces/=0)) ! exclude the ones in this piece
    end function get_pieces_above

    function get_pieces_below(i) result(ipieces)
        !! get set of pieces below piece i
        integer(ip),intent(in) :: i
        integer(ip),dimension(:),allocatable :: ipieces
        integer(ip) :: x,y,z !! counter
        allocate(ipieces(0))
        do x = istart_array(i,1), iend_array(i,1)
            do y = istart_array(i,2), iend_array(i,2)
                do z = istart_array(i,3), iend_array(i,3)
                    if (z-1>0) ipieces = [ipieces,array(x,y,z-1)] ! below this one
                end do
            end do
        end do
        ipieces = unique(pack(ipieces, ipieces/=i .and. ipieces/=0)) ! exclude the ones in this piece
    end function get_pieces_below

    subroutine move_piece_down(i, moved)
        !! move a piece down (fall one square) if it can be moved
        integer(ip),intent(in) :: i !! piece number
        logical,intent(out) :: moved !! if it was actually movec
        moved = .false.
        if (size(get_pieces_below(i))==0) then ! can only move if nothing below
            if (istart_array(i,3)>1 .and. iend_array(i,3)>1) then
                istart_array(i,3) = istart_array(i,3) - 1
                iend_array(i,3)   = iend_array(i,3) - 1
                moved = .true.
                call update_array()
            end if
        end if
    end subroutine move_piece_down

end program problem_22