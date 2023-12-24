program problem_21

    use aoc_utilities
    use iso_fortran_env, only: wp => real64, ip => int64

    implicit none

    character(len=1),dimension(:,:),allocatable :: array
    integer :: nrows, ncols, i, j
    integer,dimension(2) :: iloc
    integer,dimension(:,:),allocatable :: icount

    call clk%tic()

    ! array = read_file_to_char_array('inputs/day21_test.txt', border='#')
    array = read_file_to_char_array('inputs/day21.txt', border='.')
    nrows = size(array,1)
    ncols = size(array,2)
    allocate(icount(nrows, ncols)); icount = 0

    iloc = findloc(array, 'S') ! find starting point
    i = iloc(1)
    j = iloc(2)
    array(i,j) = 'O' ! starting here

    do i = 1, 64
        call step(array)
    end do
    write(*,*) '21a: ', count(array == 'O')

    call clk%toc('21')

    contains

    recursive subroutine step(array)
        !! take all valid steps from any # elements in array
        character(len=1),dimension(:,:),intent(inout),allocatable :: array
        integer :: i,j
        logical :: steped
        character(len=1),dimension(:,:),allocatable :: tmp
        tmp = array
        do i = 1, nrows
            do j = 1, ncols
                if (array(i,j)=='O') then
                    steped = .false.
                    ! try a step in each direction:
                    if (array(i-1,j)=='.') then
                        tmp(i-1,j)='O'; steped = .true.
                    end if
                    if (array(i+1,j)=='.') then
                        tmp(i+1,j)='O'; steped = .true.
                    end if
                    if (array(i,j-1)=='.') then
                        tmp(i,j-1)='O'; steped = .true.
                    end if
                    if (array(i,j+1)=='.') then
                        tmp(i,j+1)='O'; steped = .true.
                    end if
                    if (steped) tmp(i,j) = '.'
                end if
            end do
        end do
        call move_alloc(tmp,array)
    end subroutine step

end program problem_21