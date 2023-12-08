program problem_8

use iso_fortran_env, only: ip => int64 ! use long ints
use aoc_utilities

implicit none

integer :: iunit, n_lines
character(len=:),allocatable :: line, instructions
type(string),dimension(:),allocatable :: vals
integer(ip) :: i, j, idx, idx_zzz, imoves, ias, izs
integer(ip),dimension(:),allocatable :: instructions_ints

integer(ip),dimension(:),allocatable :: idx_vec, idx_zzz_vec

type :: node
    character(len=3) :: name = ''
    character(len=3),dimension(2) :: lr
    integer,dimension(2) :: lr_idx ! the indices in the nodes array of the l,r nodes
end type node
type(node),dimension(:),allocatable :: nodes

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
    read(line,'(A3,1X,"=",1X,"(",A3,",",1X,A3,")")') nodes(i)%name, nodes(i)%lr(1), nodes(i)%lr(2)
end do
close(iunit)
! compute all the lr indices:
do i = 1, size(nodes)
    nodes(i)%lr_idx(1) = find_node(nodes(i)%lr(1))
    nodes(i)%lr_idx(2) = find_node(nodes(i)%lr(2))
end do

!********************************************
i = 0
imoves = 0
idx = find_node('AAA')
idx_zzz = find_node('ZZZ')
do
    !write(*,*) nodes(idx)%name
    if (idx == idx_zzz) exit
    imoves = imoves + 1 ! another move
    i = i + 1
    if (i>len(instructions)) i = 1
    idx = nodes(idx)%lr_idx(instructions_ints(i))
end do
write(*,*) '8a : number of moves:', imoves

!********************************************

stop ! not finished


!
! brute force... not going to work !!!
!
! get indices of the ones that end in A and Z:
allocate(idx_vec(0), idx_zzz_vec(0))
do i = 1, size(nodes)
    if (nodes(i)%name(3:3)=='A') idx_vec     = [idx_vec, i]
    if (nodes(i)%name(3:3)=='Z') idx_zzz_vec = [idx_zzz_vec, i]
end do
if (size(idx_vec)/=size(idx_zzz_vec)) error stop 'error: they need to be the same size?'
i = 0
imoves = 0
do
    if (all_in_set(idx_vec, idx_zzz_vec)) exit
    imoves = imoves + 1 ! another move
    i = i + 1
    if (i>len(instructions)) i = 1
    do concurrent (j = 1 : size(idx_vec))
        idx_vec(j) = nodes(idx_vec(j))%lr_idx(instructions_ints(i))
    end do
end do
write(*,*) '8b : number of moves:', imoves


contains

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