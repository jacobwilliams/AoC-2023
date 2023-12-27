program problem_24

use aoc_utilities
use nlesolver_module, only: nlesolver_type
use numerical_differentiation_module, only: numdiff_type

implicit none

integer :: iunit, n_lines, i, j
character(len=:),allocatable :: line
type(string),dimension(:),allocatable :: vals, rvals, vvals
integer(ip),dimension(:),allocatable :: xvec, yvec, zvec, vxvec, vyvec, vzvec
integer(ip) :: n_intersections
real(wp),dimension(2) :: tvec
real(wp),dimension(2) :: xy_inter
integer(ip) :: min_val, max_val

type(nlesolver_type) :: solver
type(numdiff_type) :: numdiff
integer :: istat !! Integer status code.
character(len=:),allocatable :: message  !! Text status message
real(wp),dimension(6) :: x, xscale

call clk%tic()

! read the data:
! open(newunit=iunit, file='inputs/day24_test.txt', status='OLD'); min_val=7_ip; max_val = 27_ip
open(newunit=iunit, file='inputs/day24.txt', status='OLD'); min_val=200000000000000.0_wp; max_val = 400000000000000.0_wp
n_lines = number_of_lines_in_file(iunit)
allocate(xvec(0), yvec(0), zvec(0), vxvec(0), vyvec(0), vzvec(0))
do i = 1, n_lines
    line = read_line(iunit)
    vals = split(line, '@')
    rvals = split(trim(vals(1)%str), ',')
    xvec  = [xvec, rvals(1)%to_int_64()]
    yvec  = [yvec, rvals(2)%to_int_64()]
    zvec  = [zvec, rvals(3)%to_int_64()]
    vvals = split(trim(vals(2)%str), ',')
    vxvec = [vxvec, vvals(1)%to_int_64()]
    vyvec = [vyvec, vvals(2)%to_int_64()]
    vzvec = [vzvec, vvals(3)%to_int_64()]
end do
close(iunit)

n_intersections = 0
do i = 1, n_lines
    do j = 1, i-1
        associate ( x  => xvec(i),    y => yvec(i),  &
                    dx => vxvec(i),  dy => vyvec(i), &
                    u  => xvec(j),    v => yvec(j),  &
                    du => vxvec(j),  dv => vyvec(j)  )
            if (dy*du /= dv*dx) then
                tvec = [(dv*(x-u) - du*(y-v)) / (dy*du - dx*dv), &
                        (dy*(u-x) - dx*(v-y)) / (dv*dx - du*dy) ]
                xy_inter = [x+tvec(1)*dx, y+tvec(1)*dy]
                if (all(tvec>0.0_wp) .and. &
                    all([xy_inter>=min_val .and. xy_inter<=max_val])) then
                    n_intersections = n_intersections + 1
                end if
            end if
        end associate
    end do
end do
write(*,*) '21a : ', n_intersections

! for part 2, use trick from reddit. only have to consider three points.
! will use a numerical nonlinear equation solver to solve the 6x6 system.
!
! for this, requires compiling with quad precision: -DREAL128

! initialize the finite diff library:
call numdiff%initialize(6,6,&
                        -huge(1.0_wp)*[1,1,1,1,1,1],&
                        huge(1.0_wp)*[1,1,1,1,1,1],&
                        perturb_mode=1,&  !3
                        dpert = 1.0e-10_wp*[1,1,1,1,1,1],&
                        problem_func=my_func,&
                        sparsity_mode=1,& ! dense
                        class=3) ! central diffs

! initialize the solver:
! try lsqr mode, since that works for quad precision
call solver%initialize( n = 6, &
                        m = 6, &
                        max_iter = 10000, &
                        tol = epsilon(1.0_wp),&
                        tolx = epsilon(1.0_wp),&
                        func = func, &
                        grad_sparse = grad_sparse, &
                        step_mode = 4, n_intervals=5, &
                        sparsity_mode = 2, & ! lsqr   -- this one works in quad precision
                        irow = [1,2,3,4,5,6,&
                                1,2,3,4,5,6,&
                                1,2,3,4,5,6,&
                                1,2,3,4,5,6,&
                                1,2,3,4,5,6,&
                                1,2,3,4,5,6], & ! dense
                        icol = [1,1,1,1,1,1,&
                                2,2,2,2,2,2,&
                                3,3,3,3,3,3,&
                                4,4,4,4,4,4,&
                                5,5,5,5,5,5,&
                                6,6,6,6,6,6], & ! dense
                        verbose = .false.)

call solver%status(istat, message)
if (istat /= 0) then
    write(*,'(I3,1X,A)') istat, message
    error stop
end if

x = [1.0_wp, 1.0_wp,1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp] ! just some guess
xscale = [ 1.0e10_wp,&
           1.0e10_wp,&
           1.0e10_wp,&
           100.0_wp,&
           100.0_wp,&
           100.0_wp ] ! x vector scales

x = x / xscale ! scale
call solver%solve(x)
x = x*xscale ! unscale

!call solver%status(istat, message)
! write(*,*) ''
! write(*,'(I3,1X,A)') istat, message
write(*,*) 'solution: ', x
! write(*,*) ''

write(*,*) '24b: ', sum(nint(x(1:3), kind=ip))

call clk%toc('24')

contains

    subroutine func(me,x,f)
        !! compute the function
        class(nlesolver_type),intent(inout) :: me
        real(wp),dimension(:),intent(in)    :: x
        real(wp),dimension(:),intent(out)   :: f
        integer,parameter :: p1 = 2, p2 = 3, p3 = 4  ! these points seem to work with the settings i have used for the solver
        associate( p0 => x(1:3)*xscale(1:3), v0 => x(4:6)*xscale(4:6), & ! unload/unscale rock position and velocity
                   p1 => [xvec(p1),  yvec(p1),  zvec(p1)], &   ! pick three hailstones
                   p2 => [xvec(p2),  yvec(p2),  zvec(p2)], &
                   p3 => [xvec(p3),  yvec(p3),  zvec(p3)], &
                   v1 => [vxvec(p1), vyvec(p1), vzvec(p1)], &
                   v2 => [vxvec(p2), vyvec(p2), vzvec(p2)], &
                   v3 => [vxvec(p3), vyvec(p3), vzvec(p3)] )
            f = [(cross(real(p0,wp), real(v1-v2,wp)) + cross(real(v0,wp), real(p2-p1,wp))) - &
                 (cross(real(p1,wp), real(v1,wp))    - cross(real(p2,wp), real(v2,wp))),&
                 (cross(real(p0,wp), real(v1-v3,wp)) + cross(real(v0,wp), real(p3-p1,wp))) - &
                 (cross(real(p1,wp), real(v1,wp))    - cross(real(p3,wp), real(v3,wp)))]
           !write(*,*) ''
           !write(*,'(A,1x,*(F40.20,1x))') 'x=',x*xscale
           !write(*,'(A,1x,*(F40.20,1x))') 'f=',f
        end associate
    end subroutine func

    subroutine grad(me,x,g)
        !! compute the gradient of the function (Jacobian):
        !! use finite differences
        class(nlesolver_type),intent(inout) :: me
        real(wp),dimension(:),intent(in)    :: x
        real(wp),dimension(:,:),intent(out) :: g
        real(wp),dimension(:,:),allocatable :: jac !! the jacobian matrix
        call numdiff%compute_jacobian_dense(x,jac)
        g = jac
    end subroutine grad
    subroutine grad_sparse(me,x,g)
        !! compute the gradient of the function (Jacobian) - sparse mode for lsqr
        !! use finite differences
        class(nlesolver_type),intent(inout) :: me
        real(wp),dimension(:),intent(in)    :: x
        real(wp),dimension(:),intent(out)   :: g
        real(wp),dimension(:),allocatable :: jac !! the jacobian matrix
        call numdiff%compute_jacobian(x,jac)
        g = jac
    end subroutine grad_sparse

    subroutine my_func(me,x,f,funcs_to_compute)
        !! Problem function for numdiff
        class(numdiff_type),intent(inout) :: me
        real(wp),dimension(:),intent(in)  :: x
        real(wp),dimension(:),intent(out) :: f
        integer,dimension(:),intent(in)   :: funcs_to_compute
        call func(solver,x,f)
    end subroutine my_func

end program problem_24
