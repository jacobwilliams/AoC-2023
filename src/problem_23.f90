program problem_23

!! alternate one... collapse the maze into a graph

! Process:
!   1. locate the start and end nodes (top and bottom)
!   2. find all the cells that have 3 or 4 adjacent cells:
!
!      1    2    3    4
!     ###  #.#  ###  #.#
!-->  #.#  #.#  ...  ...
!     #.#  #.#  #.#  #.#
!
!     those are the nodes, and the paths between them are the edges
!   3. compute the edges between each node combination and construct the graph
!   4. DFS all paths to find the longest.

use iso_fortran_env, only: ip => int64 ! use long ints
use aoc_utilities

implicit none

type :: node
    integer(ip),dimension(:),allocatable :: inext !! the node connected to this one
    integer(ip),dimension(:),allocatable :: idist !! distance to inext nodes
end type node
type(node),dimension(:),allocatable :: nodes !! the list of nodes
logical :: slopes !! a=true, b=false
logical,dimension(:,:),allocatable :: visited
logical,dimension(:),allocatable :: nodes_visited
integer(ip),dimension(:),allocatable :: inodes, jnodes !! node coordinates
integer(ip) :: max_dist, total_nodes, nrows, ncols
character(len=1),dimension(:,:),allocatable :: array

call clk%tic()

! call go('23a', 'inputs/day23_test.txt', .true. )
! call go('23b', 'inputs/day23_test.txt', .false.)

call go('23a', 'inputs/day23.txt', .true. )
call go('23b', 'inputs/day23.txt', .false.)

call clk%toc('23')

contains

    subroutine go(case, filename, parta)
        !! solve the case
        character(len=*),intent(in) :: case !! case name for printing
        character(len=*),intent(in) :: filename !! input file to read
        logical,intent(in) :: parta !! if this is part a, then consider the slopes

        integer(ip),dimension(1) :: iloc
        integer(ip) :: istart, iend, idist, i, j

        ! initialize:
        max_dist = 0
        slopes = parta
        if (allocated(nodes))         deallocate(nodes)
        if (allocated(visited))       deallocate(visited)
        if (allocated(nodes_visited)) deallocate(nodes_visited)
        if (allocated(inodes))        deallocate(inodes)
        if (allocated(jnodes))        deallocate(jnodes)

        ! read the data file:
        array = read_file_to_char_array(filename)
        nrows = size(array,1)
        ncols = size(array,2)
        iloc = findloc(array(1,:), '.');     istart = iloc(1) ! get start and end columns
        iloc = findloc(array(nrows,:), '.'); iend   = iloc(1) !

        ! identify the coordinates of all the nodes:
        inodes = [1] ! start node
        jnodes = [istart]
        do i = 1, nrows
            do j = 1, ncols
                if (count_adjacent(i,j)>=3) then
                    inodes = [inodes, i]
                    jnodes = [jnodes, j]
                end if
            end do
        end do
        inodes = [inodes,nrows] ! end node
        jnodes = [jnodes,iend]
        total_nodes = size(inodes)

        ! for each node, find the other nodes they are
        ! connected to and the distances between them (the edges)
        allocate(nodes(total_nodes))
        allocate(visited(nrows, ncols))
        do i = 1, total_nodes
            visited = .false.
            idist = 0
            call build_graph(i,inodes(i),jnodes(i),idist,visited)
        end do

        ! start at first, and find the longest that gets to the last.
        ! recursively traverse the graph.
        visited = .false.
        allocate(nodes_visited(total_nodes)); nodes_visited = .false.
        call traverse(1_ip, 0_ip, nodes_visited)

        write(*,*) case, max_dist ! result for this case

    end subroutine go

    recursive subroutine build_graph(node_num,i,j,idist,visited)
        integer(ip),intent(in) :: node_num !! current node number
        integer(ip),intent(in) :: i,j   !! current position
        integer(ip),intent(in) :: idist !! current distance (number of steps)
        logical,dimension(:,:),intent(in) :: visited !! elements visited in this path (not counting this one)

        logical,dimension(:,:),allocatable :: tmp_visited
        integer(ip) :: child_node_num

        if (i<1 .or. i>nrows .or. j<1 .or. j>ncols) return
        if (visited(i,j)) return
        if (array(i,j)=='#') return ! can't continue from here

        ! go until we hit another node
        child_node_num = node_number(i,j)
        if (child_node_num>0 .and. child_node_num/=node_num) then
            ! we have reached another node
            call add_edge(node_num, child_node_num, idist)
        else
            ! continue processing this edge
            tmp_visited = visited     !make a copy and mark this one
            tmp_visited(i,j) = .true. ! we are here now
            associate (a => get_cell(i,j)) ! paths (.), forest (#), and steep slopes (^, >, v, and <).
                select case (a)
                case ('.') ! path
                    call build_graph(node_num,i-1,j  ,idist+1,tmp_visited)
                    call build_graph(node_num,i+1,j  ,idist+1,tmp_visited)
                    call build_graph(node_num,i,  j+1,idist+1,tmp_visited)
                    call build_graph(node_num,i,  j-1,idist+1,tmp_visited)
                ! these don't have a choice, must go in these directions:
                case('^'); call build_graph(node_num, i-1,j,  idist+1, tmp_visited)
                case('v'); call build_graph(node_num, i+1,j,  idist+1, tmp_visited)
                case('>'); call build_graph(node_num, i,  j+1,idist+1, tmp_visited)
                case('<'); call build_graph(node_num, i,  j-1,idist+1, tmp_visited)
                end select
            end associate
        end if
    end subroutine build_graph

    recursive subroutine traverse(node_num, idist, nodes_visited)
        !! traverse the graph until we get to the end and check the max distance
        integer(ip),intent(in) :: node_num !! current node
        integer(ip),intent(in) :: idist !! distance to get here
        logical,dimension(:),intent(in) :: nodes_visited

        logical,dimension(:),allocatable :: tmp_nodes_visited
        integer :: i

        if (nodes_visited(node_num)) return ! already visited this node
        if (node_num==total_nodes) then ! reached the destination
            if (idist>max_dist) max_dist = idist  ! best so far
        else
            ! are their child nodes?
            if (allocated(nodes(node_num)%inext)) then
                tmp_nodes_visited = nodes_visited
                tmp_nodes_visited(node_num) = .true. ! mark this node
                do i = 1, size(nodes(node_num)%inext)
                    call traverse(nodes(node_num)%inext(i), idist + nodes(node_num)%idist(i), tmp_nodes_visited)
                end do
            end if
        end if

    end subroutine traverse

    pure logical function is_node(i,j)
        !! returns true if a node is at these coordinates
        integer(ip),intent(in) :: i,j
        is_node = any(inodes==i .and. jnodes==j)
    end function is_node

    subroutine add_edge(inode, ichild, idist)
        !! add an edge to this node (path to another node with the specified distance)
        integer(ip),intent(in) :: inode, ichild, idist
        if (.not. allocated(nodes(inode)%inext)) then
            allocate(nodes(inode)%inext(0))
            allocate(nodes(inode)%idist(0))
        end if
        nodes(inode)%inext = [nodes(inode)%inext, ichild]
        nodes(inode)%idist = [nodes(inode)%idist, idist]
    end subroutine add_edge

    pure integer(ip) function node_number(i,j)
        !! returns the index of these coordinates in
        !! the list of nodes (0 if it is not a node)
        integer(ip),intent(in) :: i,j
        integer(ip),dimension(1) :: iloc
        iloc = findloc(inodes==i .and. jnodes==j, .true.)
        node_number = iloc(1)
    end function node_number

    pure function count_adjacent(i,j) result(icount)
        !! count the number of adjacent cells not a tree
        integer(ip),intent(in) :: i,j
        integer(ip) :: icount
        icount = count([not_tree(i-1,j  ),&
                        not_tree(i+1,j  ),&
                        not_tree(i,  j+1),&
                        not_tree(i,  j-1)])
    end function count_adjacent

    pure function get_cell(i,j) result(a)
        integer(ip),intent(in) :: i,j !! coordinates
        character(len=1) :: a
        if (slopes) then
            a = array(i,j)
        else
            a = '.' ! ignore the slopes for part b
        end if
    end function get_cell

    pure logical function not_tree(i,j)
        !! returns true if the cell isn't a tree
        integer(ip),intent(in) :: i,j !! coordinates
        not_tree = array(i,j) /= '#'
    end function not_tree

end program problem_23