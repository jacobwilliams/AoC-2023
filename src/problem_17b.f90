program problem_17
    !! part b

    use aoc_utilities
    use iso_fortran_env, ip => int64
    use aoc_cache_module

    implicit none

    integer,parameter :: NSTATE = 4 !! number of state dimensions [i,j,direction,moves_in_that_direction]
    integer,parameter :: N_DIRECTIONS = 4 ! 1,2,3,4
    integer(ip),parameter :: UP     = 1  ! direction traveling
    integer(ip),parameter :: DOWN   = -1
    integer(ip),parameter :: LEFT   = 2
    integer(ip),parameter :: RIGHT  = -2
    integer,parameter :: MAX_MOVES_IN_DIR  = 10  !3

    type(function_cache) :: cache !! to cache the queue access ... to try to speed things up !!!

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
    integer(ip) :: queue_size

    ! for the CI ! need a faster version ...
    write(*,*) 'it works but takes a few minutes...'
    stop

    call clk%tic()

    call cache%initialize(isize=10000,chunk_size=1000)

    ! map = read_file_to_int_array('inputs/day17_test.txt')
    map = read_file_to_int_array('inputs/day17.txt')
    n_rows = size(map,1)
    n_cols = size(map,2)

    queue_size = 0
    call add_to_queue([1_ip,1_ip,0_ip,0_ip], idx) ! start
    queue(idx)%dist = 0

    do
        ! find min distance so far for ones not visited
        iloc = minloc(queue(1:queue_size)%dist, mask=.not.queue(1:queue_size)%visited)
        idx  = iloc(1) ! index in the queue
        queue(idx)%visited = .true.
        if (queue(idx)%state(1) == n_rows .and. queue(idx)%state(2) == n_cols) exit ! destination: we are done
        ! process ajacent ones:
        call check(idx, UP   )
        call check(idx, DOWN )
        call check(idx, LEFT )
        call check(idx, RIGHT)
    end do

    write(*,*) '17b: ', queue(idx)%dist

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
                if (n_moves/=0 .and. n_moves<4) return ! must move at last 4 before a turn!
                imovesv = 1 ! reset on change of direction
            end if

            select case(idirection)
            case(UP);   v = [i-1, j  ]
            case(DOWN); v = [i+1, j  ]
            case(LEFT); v = [i,   j-1]
            case(RIGHT);v = [i,   j+1]
            end select
            if (v(1)<1 .or. v(2)<1 .or. v(1)>n_rows .or. v(2)>n_cols) return ! can't go off the board
            if (v(1)==n_rows .and. v(2)==n_cols .and. imovesv<4) return ! require at least 4 to end

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
        integer(ip) :: i
        logical :: found
        integer(ip) :: cache_idx,cache_ival

        call cache%get(state,cache_idx,cache_ival,found)
        if (found) then
            idx = cache_ival
        else
            idx = -1
            if (.not. allocated(queue)) error stop 'error: queue not allocated'
            do i = 1, queue_size
                if (all(state == queue(i)%state)) then
                    idx = i
                    exit
                end if
            end do
            if (idx /= -1) call cache%put(cache_idx,state,idx)
        end if
    end function index_in_queue

    subroutine add_to_queue(state, idx)
        integer(ip),dimension(NSTATE),intent(in) :: state
        integer(ip),intent(out) :: idx !! index of the element in the queue
        if (allocated(queue)) then
            call expand_queue(queue,queue_size,item(state = state))
            idx = queue_size
        else
            queue = [item(state = state)]
            queue_size = 1
            idx = 1
        end if
    end subroutine add_to_queue

!****************************************************************
!>
!  Add elements to the integer vector in chunks.

    pure subroutine expand_queue(vec,n,val,finished)

    integer(ip),parameter :: chunk_size = 1000

    type(item),dimension(:),allocatable,intent(inout) :: vec
    integer(ip),intent(inout)       :: n           !! counter for last element added to `vec`.
                                               !! must be initialized to `size(vec)`
                                               !! (or 0 if not allocated) before first call
    type(item),intent(in),optional :: val         !! the value to add to `vec`
    logical,intent(in),optional :: finished    !! set to true to return `vec`
                                               !! as its correct size (`n`)

    type(item),dimension(:),allocatable :: tmp  !! temporary array

    if (present(val)) then
        if (allocated(vec)) then
            if (n==size(vec)) then
                ! have to add another chunk:
                allocate(tmp(size(vec)+chunk_size))
                tmp(1:size(vec)) = vec
                call move_alloc(tmp,vec)
            end if
            n = n + 1
        else
            ! the first element:
            allocate(vec(chunk_size))
            n = 1
        end if
        vec(n) = val
    end if

    if (present(finished)) then
        if (finished) then
            ! set vec to actual size (n):
            if (allocated(tmp)) deallocate(tmp)
            allocate(tmp(n))
            tmp = vec(1:n)
            call move_alloc(tmp,vec)
        end if
    end if

    end subroutine expand_queue
!****************************************************************

end program problem_17