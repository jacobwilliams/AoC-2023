program problem_11

use iso_fortran_env, ip => int64, wp => real64
use aoc_utilities

implicit none

call clk%tic()

write(*,*) '11a: ', go(2_ip)
write(*,*) '11b: ', go(1000000_ip)

call clk%toc('11')

contains

integer(ip) function go(expansion_factor)
    integer(ip),intent(in) :: expansion_factor

    integer :: i, j, nrows, ncols, n_galaxies, n, k
    character(len=1),dimension(:,:),allocatable :: array
    integer(ip),dimension(:,:),allocatable :: idists
    integer(ip),dimension(:),allocatable :: igal, jgal
    integer(ip),dimension(:,:),allocatable :: ix,iy

    ! array = read_file_to_char_array('inputs/day11_test.txt')
    array = read_file_to_char_array('inputs/day11.txt')
    nrows = size(array,1)
    ncols = size(array,2)
    n_galaxies = count(array=='#')

    ! track the real indices after expansion
    allocate(ix(nrows, ncols), iy(nrows, ncols))
    do i = 1, nrows                                ! probably could use spread here ...
        ix(i,:) = i
    end do
    do j = 1, ncols
        iy(:,j) = j
    end do

    allocate(idists(n_galaxies, n_galaxies), &
             igal(n_galaxies), jgal(n_galaxies) ); idists = 0

    ! expand the array
    do i = 1, nrows ! add rows if necessary
        if (all(array(i,:)=='.')) ix(i:,:) = ix(i:,:) + expansion_factor-1 ! new row
    end do
    do j = 1, ncols ! add cols if necessary
        if (all(array(:,j)=='.')) iy(:,j:) = iy(:,j:) + expansion_factor-1 ! new column
    end do

    ! get the indices of the galaxies:
    n = 0
    do i = 1, nrows
        do j = 1, ncols
            if (array(i,j)=='#') then
                n = n + 1
                igal(n) = i; jgal(n) = j
            end if
        end do
    end do

    ! now compute all the distances:
    do i = 1, n_galaxies
        do j = 1, n_galaxies
            if (j<=i) cycle ! only need the lower diagonal
            idists(i,j) = dist(ix(igal(i),jgal(i)),iy(igal(i),jgal(i)),&
                               ix(igal(j),jgal(j)),iy(igal(j),jgal(j)))
        end do
    end do

    go = sum(idists)

end function go

pure integer function dist(x1,y1,x2,y2) ! Manhattan distance
    integer(ip),intent(in) :: x1,y1,x2,y2
    dist = abs(x1 - x2) + abs(y1 - y2)
end function dist

end program problem_11