program problem_22

use iso_fortran_env
use aoc_utilities
use aoc_cache_module

implicit none

integer :: iunit, n_lines
integer(ip) :: i, j, k
character(len=:),allocatable :: line
type(string),dimension(:),allocatable :: start_end, starts, ends
integer(ip),dimension(:,:),allocatable :: istart_array
integer(ip),dimension(:,:),allocatable :: iend_array
integer(ip),dimension(:,:,:),allocatable :: array
integer(ip),dimension(:),allocatable :: ipieces_above, ipieces_below
integer(ip) :: ok_to_disintegrate, isum, isum_total
logical :: ok_tmp
type(function_cache) :: above_cache, below_cache

call clk%tic()

!find the ones we can remove without any other falling:
!  to do that, look to see if any above it are not otherwise held up by another piece
call initialize()
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

! for part 2, we need to find all the nodes in the tree
! that are only supportd by ones below that in the tree
!
!  777  44444
! 66111222  9
!88 333333333   <--- deleting 3 will cause 1, 2, 4, 9 to fall, but not 6,7,8

! just use a cache to save the ones above/below a given piece
! since there is a lot of duplication in that calculation
call above_cache%initialize(isize=1000,chunk_size=1000)
call below_cache%initialize(isize=1000,chunk_size=1000)

call initialize()
isum_total = 0
do i = 1, n_lines ! try remove piece i
    ipieces_above = get_all_pieces_above(i)
    isum = size(ipieces_above) ! start out assuming they will all fall
    if (isum>0) then
        main : do j = 1, size(ipieces_above)
            ! for all the ones, see if there are any below that are not in the above set.
            ! is so, then it will not fall.
            ipieces_below = get_all_pieces_below(ipieces_above(j), iskip=i)
            ipieces_below = pack(ipieces_below, ipieces_below/=i) ! remove i if present (the one being removed)
            do k = 1, size(ipieces_below)
                if (.not. any(ipieces_below(k)==[ipieces_above])) then
                    isum = isum - 1 ! this one will not fall, so remove it from total
                    cycle main
                end if
            end do
        end do main
    end if
    isum_total = isum_total + isum
end do
write(*,*) '22b: ', isum_total

call clk%toc('22')

contains

    subroutine initialize()
        !! read the data
        ! open(newunit=iunit, file='inputs/day22_test.txt', status='OLD')
        open(newunit=iunit, file='inputs/day22.txt', status='OLD')
        n_lines = number_of_lines_in_file(iunit)
        if (allocated(istart_array)) deallocate(istart_array); allocate(istart_array(n_lines,3))
        if (allocated(iend_array)) deallocate(iend_array); allocate(iend_array(n_lines,3))
        do i = 1, n_lines
            line = read_line(iunit); start_end = split(line, '~')
            starts = split(start_end(1)%str, ',')
            ends   = split(start_end(2)%str, ',')
            istart_array(i,:) = [(int(starts(j)%str), j = 1, 3)]
            iend_array(i,:)   = [(int(ends(j)%str),   j = 1, 3)]
        end do
        close(iunit)
        ! create the array to hold all the pieces:
        if (allocated(array)) deallocate(array)
        allocate(array(0:maxval([istart_array(:,1), iend_array(:,1)]),&
                       0:maxval([istart_array(:,2), iend_array(:,2)]),&
                       0:maxval([istart_array(:,3), iend_array(:,3)])))
        call update_array()  ! set array values from the start/end arrays
        call drop() ! continue forward in time until all the bricks settle.
    end subroutine initialize

    subroutine drop()
        !! continue forward in time until all the bricks settle.
        logical :: moved, tmp_moved
        integer(ip) :: i
        do
            moved = .false.
            do i = 1, n_lines
                call move_piece_down(i, tmp_moved)
                if (tmp_moved) moved = .true. ! at least one was moved
            end do
            if (.not. moved) exit ! done moving all the pieces
        end do
    end subroutine drop

    recursive function get_all_pieces_above(i) result(ipieces)
        !! reursively get a list of all pieces above piece i
        integer(ip),intent(in) :: i
        integer(ip),dimension(:),allocatable :: ipieces
        integer :: j !! counter
        logical :: found
        integer(ip) :: idx
        call above_cache%get([i],idx,ipieces,found)
        if (.not. found) then
            allocate(ipieces(0))
            associate(tmp => get_pieces_above(i))
                if (size(tmp)>0) then
                    ipieces = [ipieces, tmp] ! ones directly above
                    do j = 1, size(tmp) ! go up the tree
                        ipieces = [ipieces, get_all_pieces_above(tmp(j))]
                    end do
                    ipieces = unique(ipieces)
                end if
            end associate
           call above_cache%put(idx,[i],ipieces)
        end if
    end function get_all_pieces_above

    recursive function get_all_pieces_below(i, iskip) result(ipieces)
        !! reursively get a list of all pieces below piece i
        integer(ip),intent(in) :: i
        integer(ip),intent(in) :: iskip !! skip this one and it's children
        integer(ip),dimension(:),allocatable :: ipieces
        integer :: j !! counter
        logical :: found
        integer(ip) :: idx
        integer(ip),dimension(:),allocatable :: tmp
        call below_cache%get([i,iskip],idx,ipieces,found)
        if (.not. found) then
            allocate(ipieces(0))
            if (i==iskip) return
            tmp = get_pieces_below(i)
            if (size(tmp)>0) then
                tmp = pack(tmp, tmp/=iskip) ! remove the skipped one
                ipieces = [ipieces, tmp] ! ones directly below
                do j = 1, size(tmp) ! go down the tree
                    ipieces = [ipieces, get_all_pieces_below(tmp(j),iskip=iskip)]
                end do
                ipieces = unique(ipieces)
            end if
           call below_cache%put(idx,[i,iskip],ipieces)
        end if
    end function get_all_pieces_below

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
        !! get set of pieces directly above piece i
        integer(ip),intent(in) :: i
        integer(ip),dimension(:),allocatable :: ipieces
        integer(ip) :: x,y,z !! counter
        allocate(ipieces(0))
        do x = istart_array(i,1), iend_array(i,1)
            do y = istart_array(i,2), iend_array(i,2)
                do z = istart_array(i,3), iend_array(i,3)
                    if (z+1<=ubound(array,3)) then
                        if (array(x,y,z+1)/=0) ipieces = [ipieces,array(x,y,z+1)] ! above this one
                    end if
                end do
            end do
        end do
        ipieces = unique(pack(ipieces, ipieces/=i)) ! exclude the ones in this piece
    end function get_pieces_above

    function get_pieces_below(i) result(ipieces)
        !! get set of pieces directly below piece i
        integer(ip),intent(in) :: i
        integer(ip),dimension(:),allocatable :: ipieces
        integer(ip) :: x,y,z !! counter
        allocate(ipieces(0))
        do x = istart_array(i,1), iend_array(i,1)
            do y = istart_array(i,2), iend_array(i,2)
                do z = istart_array(i,3), iend_array(i,3)
                    if (z-1>0) then ! 0 is the floor
                        if (array(x,y,z-1)/=0) ipieces = [ipieces,array(x,y,z-1)] ! below this one
                    end if
                end do
            end do
        end do
        ipieces = unique(pack(ipieces, ipieces/=i)) ! exclude the ones in this piece
    end function get_pieces_below

    subroutine move_piece_down(i, moved)
        !! move a piece down (fall one square) if it can be moved
        integer(ip),intent(in) :: i !! piece number
        logical,intent(out) :: moved !! if it was actually movec
        moved = .false.
        if (size(get_pieces_below(i))==0) then ! can only move if nothing below
            if (istart_array(i,3)>1 .and. iend_array(i,3)>1) then
                istart_array(i,3) = istart_array(i,3) - 1
                iend_array(i,3)   = iend_array(i,3)   - 1
                moved = .true.
                call update_array()
            end if
        end if
    end subroutine move_piece_down

end program problem_22