program problem_25

use aoc_utilities
use dag_module

implicit none

integer :: iunit, n_lines, i, j, n_nodes, inode, inode2
character(len=:),allocatable :: line
type(string),dimension(:),allocatable :: vals, vals2
character(len=3) :: node1, node2
character(len=3),dimension(:),allocatable :: nodes
integer,dimension(:),allocatable :: inodedep, icounts

character(len=*),parameter :: filetype = 'pdf'
type(dag) :: d

type :: node_t
    !character(len=3) :: name = ''
    integer,dimension(:),allocatable :: connections !! the ones connected to this one
end type node_t
type(node_t),dimension(:),allocatable :: graph !! index is the node number
logical,dimension(:),allocatable :: visited

call clk%tic()

! read the data:
! open(newunit=iunit, file='inputs/day25_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day25.txt', status='OLD')
allocate(nodes(0))
n_lines = number_of_lines_in_file(iunit)
do i = 1, n_lines
    line = read_line(iunit)
    ! for starters, get a list of all the nodes
    vals = split(line,': ')
    node1 = vals(1)%str
    if (.not. any(nodes==node1)) nodes = [nodes,node1]
    vals2 = split(vals(2)%str, ' ')
    do j = 1, size(vals2)
        node2 = vals2(j)%str
        if (.not. any(nodes==node2)) nodes = [nodes,node2]
    end do
end do
rewind(iunit)
n_nodes = size(nodes)

! create a dag:
call d%set_vertices(n_nodes)
allocate(graph(n_nodes)) !...
do i = 1, n_nodes
    allocate(graph(i)%connections(0)) ! initialize
end do
do i = 1, n_lines
    line = read_line(iunit)
    ! for starters, get a list of all the nodes
    vals = split(line,': ')
    node1 = vals(1)%str
    inode = node_index(node1)
    !inode = findloc(nodes,node1)
    if (.not. any(nodes==node1)) nodes = [nodes,node1]
    vals2 = split(vals(2)%str, ' ')
    if (allocated(inodedep)) deallocate(inodedep)
    allocate(inodedep(0))
    do j = 1, size(vals2)
        node2 = vals2(j)%str
        inode2 = node_index(node2)

        !--------------------------------------------
        ! prune the ones by inspection by looking at the graph, opening in Inkscape,
        ! coloring the 3 lines and finding the nodes that connect them
        if ( (node1=='ljm' .and. node2=='sfd') .or. (node2=='ljm' .and. node1=='sfd') ) cycle
        if ( (node1=='gst' .and. node2=='rph') .or. (node2=='gst' .and. node1=='rph') ) cycle
        if ( (node1=='jkn' .and. node2=='cfn') .or. (node2=='jkn' .and. node1=='cfn') ) cycle
        !--------------------------------------------

        ! accumulate the graph:
        ! connection between inode <--> inodedep
        graph(inode)%connections  = [graph(inode)%connections, inode2]
        graph(inode2)%connections = [graph(inode2)%connections, inode]

        ! for the plot:
        inodedep = [inodedep, inode2]
    end do
    call d%set_edges(inode, inodedep)
end do

! define some styles for the GraphViz output:
do i = 1, n_nodes
    call d%set_vertex_info(i, label = nodes(i))
end do

! generate the GraphViz output:
call d%save_digraph('problem25.dot',rankdir='TB',dpi=300)
call d%destroy()
call execute_command_line('dot -Tpdf -o problem25.pdf problem25.dot')
    ! add to dot file:  !todo add this to daglib ...
    ! mindist=10
    ! ranksep=20

! count the ones connected to each node and get the unique two we need
allocate(visited(n_nodes))
allocate(icounts(0))
do i = 1, n_lines
    visited = .false.
    call traverse(i)
    icounts = unique([icounts, count(visited)])
    write(*,*) i, icounts
end do
write(*,*) '25a:', product(icounts)

call clk%toc('25')

contains

    pure integer function node_index(node)
        !! find the node number for this name
        character(len=*),intent(in) :: node
        integer,dimension(1) :: idx
        idx = findloc(nodes,node)
        node_index = idx(1)
    end function node_index

    recursive subroutine traverse(i)
        !! travere the graph and visit all the connected nodes
        integer,intent(in) :: i
        integer :: j
        if (visited(i)) return
        visited(i) = .true.
        do j = 1, size(graph(i)%connections)
            call traverse(graph(i)%connections(j))
        end do
    end subroutine traverse

end program problem_25