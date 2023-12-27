program problem_17

    use aoc_utilities
    use iso_fortran_env

    !! Based on AOC 2021 Day 15: https://github.com/jacobwilliams/AoC-2021/blob/master/test/problem_15.f90
    !! see: https://en.wikipedia.org/wiki/Dijkstra's_algorithm#Pseudocode
    !!
    !! only difference is the constraint that it can move at most three blocks in a single direction

    implicit none

    integer,parameter :: NSTATE = 4 !! number of state dimensions [i,j,direction,moves_in_that_direction]
    integer,parameter :: N_DIRECTIONS = 4 ! 1,2,3,4
    integer(ip),parameter :: UP     = 1  ! direction traveling
    integer(ip),parameter :: DOWN   = -1
    integer(ip),parameter :: LEFT   = 2
    integer(ip),parameter :: RIGHT  = -2
    integer,parameter :: MAX_MOVES_IN_DIR  = 3

    ! queue for the states and distances... rather than constructing the 2d array...
    ! just create this as we go...
    type :: item
        integer(ip),dimension(NSTATE) :: state = 0 !! state: [i, j, direction, and # of moves]
        integer(ip) :: dist    = huge(1) !! distance for this one
        logical     :: visited = .false. !! if this one is done
        integer(ip) :: iprev   = -1      !! index in the queue of the previous one
    end type item
    type(item),dimension(:),allocatable :: queue !! will be expanding

    integer(ip),dimension(1) :: iloc
    integer(ip) :: idx
    integer :: i, j, n_rows, n_cols
    integer,dimension(:,:),allocatable :: map !! heat map

    ! for the CI ! need a faster version ...
    write(*,*) 'it works but takes a few minutes...'
    stop

    call clk%tic()

    ! map = read_file_to_int_array('inputs/day17_test.txt')
    map = read_file_to_int_array('inputs/day17.txt')
    n_rows = size(map,1)
    n_cols = size(map,2)

    call add_to_queue([1_ip,1_ip,0_ip,0_ip], idx) ! start
    queue(idx)%dist = 0

    do
        ! find min distance so far for ones not visited
        iloc = minloc(queue%dist, mask=.not.queue%visited)
        idx  = iloc(1) ! index in the queue
        queue(idx)%visited = .true.
       !write(*,*) 'visited ', queue(idx)%state(1), queue(idx)%state(2), n_rows, n_cols
        if (queue(idx)%state(1) == n_rows .and. queue(idx)%state(2) == n_cols) exit ! destination: we are done
        ! process ajacent ones:
        call check(idx, UP   )
        call check(idx, DOWN )
        call check(idx, LEFT )
        call check(idx, RIGHT)
    end do

    write(*,*) '15a: ', queue(idx)%dist

    ! ...draw the path
    ! block
    !     character(len=1),dimension(:,:),allocatable :: char_map
    !     char_map = int_array_to_char_array(map)
    !     do
    !         char_map(queue(idx)%state(1),queue(idx)%state(2)) = '#'
    !         if (queue(idx)%state(1)==1) then
    !             write(*,*) queue(idx)%state
    !         end if
    !         if (queue(idx)%state(1)==1 .and. queue(idx)%state(2)==1) exit
    !         idx = queue(idx)%iprev
    !     end do
    !     write(*,*) ' path: '
    !     do i = 1, n_rows
    !         write(*,'(*(A1))') char_map(i,:)
    !     end do
    ! end block

    call clk%toc('17')

    contains

    subroutine check(idx, idirection)
        implicit none
        integer(ip),intent(in) :: idx ! current item in the queue to process
        integer(ip),intent(in) :: idirection !! direction to move from current

        integer(ip) :: alt
        integer(ip),dimension(2) :: v ! neighbor to move to
        integer(ip) :: imovesv, idxv, distv
        integer(ip),dimension(NSTATE) :: istate
        logical :: in_queue

        associate ( i         => queue(idx)%state(1), &
                    j         => queue(idx)%state(2), &
                    direction => queue(idx)%state(3), &
                    n_moves   => queue(idx)%state(4))

            if (direction==-idirection) return ! can't reverse

            ! the one to go to:
            if (direction==idirection) then
                if (n_moves==MAX_MOVES_IN_DIR) return ! can't move in this direction anymore
                imovesv = n_moves + 1
            else
                imovesv = 1 ! reset on change of direction
            end if
            select case(idirection)
            case(UP);   v = [i-1, j  ]
            case(DOWN); v = [i+1, j  ]
            case(LEFT); v = [i,   j-1]
            case(RIGHT);v = [i,   j+1]
            end select
            if (v(1)<1 .or. v(2)<1 .or. v(1)>n_rows .or. v(2)>n_cols) return ! can't go off the board

            istate = [v(1), v(2), idirection, imovesv] ! new state
            idxv = index_in_queue(istate) ! is it already in the queue
            in_queue = idxv>0 ! if this one is already part of another path
            if (in_queue) then
                if (queue(idxv)%visited) return ! already visited this one
                distv = queue(idxv)%dist ! distance in the queue
            else
                !call add_to_queue(istate, idxv) ! add this to the queue for processing ! always add it ??
                distv = huge(1_ip) ! not processed yet, so huge number
            end if

            !alt = queue(idx)%dist + map(queue(idxv)%state(1), queue(idxv)%state(2))
            alt = queue(idx)%dist + map(v(1), v(2)) ! new distance
            if (alt < distv) then ! the new one is better, so replace values in the queue
                if (.not. in_queue) call add_to_queue(istate, idxv) ! only add it if we accept it ????
                ! accept this step
                queue(idxv)%dist  = alt ! new best dist
                queue(idxv)%iprev = idx ! previous
            end if

        end associate

    end subroutine check

    ! .. need a faster queue...

    function index_in_queue(state) result(idx)
        integer(ip),dimension(NSTATE),intent(in) :: state
        integer(ip) :: idx !! index in the queue. -1 if not present
        integer :: i
        idx = -1
        if (.not. allocated(queue)) error stop 'error: queue not allocated'
        do i = 1, size(queue)
            if (all(state == queue(i)%state)) then
                idx = i
                return
            end if
        end do
    end function index_in_queue

    subroutine add_to_queue(state, idx)
        integer(ip),dimension(NSTATE),intent(in) :: state
        integer(ip),intent(out) :: idx !! index of the element in the queue
        if (allocated(queue)) then
            queue = [queue, item(state = state)]
            idx = size(queue)
        else
            queue = [item(state = state)]
            idx = 1
        end if
    end subroutine add_to_queue

end program problem_17