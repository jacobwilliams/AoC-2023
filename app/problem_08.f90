program problem_8

use iso_fortran_env
use aoc_utilities

implicit none

integer :: iunit, n_lines
character(len=:),allocatable :: line, instructions
type(string),dimension(:),allocatable :: vals
integer(ip) :: i, j, idx, idx_zzz, imoves, ias, izs
integer(ip),dimension(:),allocatable :: instructions_ints
integer(ip),dimension(:),allocatable :: idx_vec, idx_zzz_vec
integer(ip),dimension(:),allocatable :: imoves_vec

type :: node
    character(len=3) :: name = ''
    character(len=3),dimension(2) :: lr
    integer,dimension(2) :: lr_idx ! the indices in the nodes array of the l,r nodes
end type node
type(node),dimension(:),allocatable :: nodes

call clk%tic()

! read the data file:
! open(newunit=iunit, file='inputs/day8_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day8.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
allocate(nodes(n_lines-2))
instructions = read_line(iunit)
allocate(instructions_ints(len(instructions)))
do i = 1, len(instructions)
    select case (instructions(i:i))   ! make an int array for easier access
    case('L'); instructions_ints(i) = 1
    case('R'); instructions_ints(i) = 2
    end select
end do
do i = 0, n_lines-1
    line = read_line(iunit)
    if (line == '') cycle
    read(line,"(A3,1X,1X,1X,1X,A3,1X,1X,A3,1X)") nodes(i)%name, nodes(i)%lr(1), nodes(i)%lr(2)
end do
close(iunit)
! compute all the lr indices:
do i = 1, size(nodes)
    nodes(i)%lr_idx(1) = find_node(nodes(i)%lr(1))
    nodes(i)%lr_idx(2) = find_node(nodes(i)%lr(2))
end do

!********************************************

imoves = moves(find_node('AAA'), find_node('ZZZ'))
write(*,*) '8a : number of moves:', imoves

!********************************************

! get indices of the ones that end in A and Z:
allocate(idx_vec(0), idx_zzz_vec(0))
do i = 1, size(nodes)
    if (nodes(i)%name(3:3)=='A') idx_vec     = [idx_vec, i]
    if (nodes(i)%name(3:3)=='Z') idx_zzz_vec = [idx_zzz_vec, i]
end do
if (size(idx_vec)/=size(idx_zzz_vec)) error stop 'error: they need to be the same size?'
allocate(imoves_vec(size(idx_vec)))
! for EACH start, find the number of moves that it takes to get to ONE of the ends.
do i = 1, size(idx_vec)
    imoves_vec(i) = moves_any_z(idx_vec(i), idx_zzz_vec)
end do
! don't know why this works ¯\_(ツ)_/¯
! I think it's a feature of the specific data given
! (it's a repeating cycle once it gets to the end)
write(*,*) '8b : number of moves: ', lcm(lcm(lcm(lcm(lcm(imoves_vec(1),&
                                             imoves_vec(2)),&
                                             imoves_vec(3)),&
                                             imoves_vec(4)),&
                                             imoves_vec(5)),&
                                             imoves_vec(6))

! !
! ! brute force. run all the moves in parallel until they are all done.
! !
! i = 0
! imoves = 0
! do
!     if (all_in_set(idx_vec, idx_zzz_vec)) exit
!     imoves = imoves + 1 ! another move
!     i = i + 1
!     if (i>len(instructions)) i = 1
!     do concurrent (j = 1 : size(idx_vec))
!         idx_vec(j) = nodes(idx_vec(j))%lr_idx(instructions_ints(i))
!     end do
! end do
! write(*,*) '8b : number of moves:', imoves

call clk%toc('8')

contains

    pure function moves(istart, iend) result(imoves)

        integer(ip),intent(in) :: istart, iend !! indices for start and end nodes
        integer(ip) :: imoves

        integer :: i
        integer(ip) :: idx

        i = 0
        imoves = 0
        idx = istart
        do
            !write(*,*) nodes(idx)%name
            if (idx == iend) exit
            ! if (idx == istart) write(*,*) 'back to start'
            imoves = imoves + 1 ! another move
            i = i + 1
            if (i>len(instructions)) i = 1
            idx = nodes(idx)%lr_idx(instructions_ints(i))
        end do

    end function moves

    pure function moves_any_z(istart, iend) result(imoves)

        integer(ip),intent(in) :: istart !! indices for start and end nodes
        integer(ip),dimension(:),intent(in) :: iend
        integer(ip) :: imoves

        integer :: i
        integer(ip) :: idx

        i = 0
        imoves = 0
        idx = istart
        do
            !write(*,*) nodes(idx)%name
            if (any(idx == iend)) exit
            ! if (idx == istart) write(*,*) 'back to start'
            imoves = imoves + 1 ! another move
            i = i + 1
            if (i>len(instructions)) i = 1
            idx = nodes(idx)%lr_idx(instructions_ints(i))
        end do

    end function moves_any_z


    pure logical function all_in_set(ivals, iset)
    !! returns true if all the elements of ivals are in the set iset
    integer(ip),dimension(:),intent(in) :: ivals, iset
    integer :: i
    all_in_set = .true.
    do i = 1, size(ivals)
        if (.not. any(ivals(i)==iset)) then
            all_in_set = .false.
            exit
        end if
    end do
    end function all_in_set

    pure integer(ip) function find_node(name)
        character(len=*),intent(in) :: name
        integer(ip) :: i
        do i = 1, size(nodes)
            if (name == nodes(i)%name) then
                find_node = i
                return
            end if
        end do
        error stop 'could not find node'
    end function find_node

end program problem_8