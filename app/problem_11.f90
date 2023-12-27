program problem_11

use iso_fortran_env
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
    nrows = size(array,1); ncols = size(array,2)
    n_galaxies = count(array=='#')

    ! track the real indices after expansion
    ix = transpose(spread([(i, i=1,ncols)], 1, nrows)) ! row indices
    iy =           spread([(i, i=1,nrows)], 1, ncols)  ! col indices
    igal = pack(ix, array=='#') ! indices of galaxies
    jgal = pack(iy, array=='#')

    ! expand the array, keeping track of the new row indices
    do i = 1, nrows ! add rows if necessary
        if (all(array(i,:)=='.')) ix(i:,:) = ix(i:,:) + expansion_factor-1 ! new row
    end do
    do j = 1, ncols ! add cols if necessary
        if (all(array(:,j)=='.')) iy(:,j:) = iy(:,j:) + expansion_factor-1 ! new column
    end do

    ! now compute all the distances:
    allocate(idists(n_galaxies, n_galaxies)); idists = 0
    do i = 1, n_galaxies
        do j = 1, n_galaxies
            if (j<=i) cycle ! only need the lower diagonal
            idists(i,j) = manhatten_distance(ix(igal(i),jgal(i)),iy(igal(i),jgal(i)),&
                                             ix(igal(j),jgal(j)),iy(igal(j),jgal(j)))
        end do
    end do

    go = sum(idists)

end function go

end program problem_11