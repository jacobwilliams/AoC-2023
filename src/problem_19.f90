program problem_19

    use aoc_utilities
    use iso_fortran_env

    implicit none

    integer :: iunit, n_lines, iline, i
    character(len=:),allocatable :: line !, name
    type(string),dimension(:),allocatable :: vals
    logical :: accepted
    integer(ip) :: total_accepted

    type :: part
        integer(ip),dimension(4) :: xmas  ! [x,m,a,s]
    end type part
    type(part),dimension(:), allocatable :: parts

    type :: rule
        logical :: accept = .false.
        logical :: reject = .false.
        integer :: operator = 0 ! 1:<, 2:>
        integer :: operator_arg = 0 ! 1:x, 2:m, 3:a, 4:s
        integer :: operator_val = 0 ! value to compare to
        character(len=:),allocatable :: goto ! where to next (workflow name)
    end type rule

    type :: workflow
        character(len=:),allocatable :: name
        type(rule),dimension(:),allocatable :: rules
    end type workflow
    type(workflow),dimension(:), allocatable :: workflows
    type(workflow),allocatable :: w

    call clk%tic()

    ! open(newunit=iunit, file='inputs/day19_test.txt', status='OLD')
    open(newunit=iunit, file='inputs/day19.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)
    allocate(workflows(0))
    do iline = 1, n_lines
        line = read_line(iunit)
        if (line == '') then !now parse part ratings
            allocate(parts(0))
        end if

        if (allocated(parts)) then
            ! parsing parts
            if (line=='') cycle
            ! {x=787,m=2655,a=1222,s=2876}
            vals = split(line,',')
            parts = [parts, part(xmas=[int(vals(1)%str(4:)), &
                                       int(vals(2)%str(3:)), &
                                       int(vals(3)%str(3:)), &
                                       int(vals(4)%str(3:len(vals(4)%str)-1))])]
        else
            ! parsing workflows : qqz{s>2770:qs,m<1801:hdj,R}
            if (allocated(w)) deallocate(w); allocate(w) ! blank one
            vals = split(line, '{')
            w%name = vals(1)%str  ! workflow name
            !write(*,*) '--->'//w%name//'<-----'
            vals(2)%str = vals(2)%str(1:len(vals(2)%str)-1) ! remove last bracket
            vals = split(vals(2)%str, ',') ! workflow rules
            ! add each rule to the workflow
            allocate(w%rules(0))
            do i = 1, size(vals)
                w%rules = [w%rules, parse_rule(vals(i)%str)]
            end do
            workflows = [workflows, w]
        end if

    end do

    total_accepted = 0
    do i = 1, size(parts)
        accepted = process(parts(i))
        !write(*,*) parts(i), accepted
        if (accepted) then
            total_accepted = total_accepted + rating(parts(i))
        end if
    end do

    write(*,*) '19a: ', total_accepted

    call clk%toc('19')

    contains

        pure integer(ip) function rating(p)
            type(part),intent(in) :: p
            rating = sum(p%xmas)
        end function rating

        function process(p) result(accepted)
            !! process this part through all the workflows.
            type(part),intent(in) :: p
            logical :: accepted

            integer :: i, irule

            i = workflow_name_to_index('in')     ! start at the first workflow
            irule = 1 ! first rule
            do
                associate( r => workflows(i)%rules(irule) )
                    ! check if we are done
                    if (r%accept) then; accepted = .true.; return; end if
                    if (r%reject) then; accepted = .false.;return; end if
                    if (r%operator>0) then
                        ! process the operator
                        select case (r%operator)
                        case(1) ! <
                            if (p%xmas(r%operator_arg) < r%operator_val) then
                                ! check if done
                                if (r%goto=='A') then; accepted = .true.; return; end if
                                if (r%goto=='R') then; accepted = .false.; return; end if
                                ! go to next workflow:
                                i = workflow_name_to_index(r%goto)
                                !write(*,*) 'goto '//r%goto
                                irule = 1 ! start at first rule of new workflow
                             else
                                irule = irule + 1 ! next rule
                             end if
                        case(2) ! >
                            if (p%xmas(r%operator_arg) > r%operator_val) then
                                ! check if done
                                if (r%goto=='A') then; accepted = .true.; return; end if
                                if (r%goto=='R') then; accepted = .false.; return; end if
                                ! go to next workflow:
                                i = workflow_name_to_index(r%goto)
                                irule = 1 ! start at first rule of new workflow
                            else
                                irule = irule + 1 ! next rule
                            end if
                        end select
                    else
                        ! goto another workflow
                        i = workflow_name_to_index(r%goto)
                        irule = 1 ! start at first rule of new workflow
                    end if
                end associate
            end do

        end function process

        function workflow_name_to_index(name) result(idx)
            !! get the index of this workflow in the array
            character(len=*),intent(in) :: name
            integer :: idx
            do idx = 1, size(workflows)
                if (name == workflows(idx)%name) return ! found it
            end do
            if (idx>size(workflows)) error stop 'workflow not found: '//name
        end function workflow_name_to_index

        function parse_rule(s) result(r)
            character(len=*),intent(in) :: s
            type(rule) :: r

            type(string),dimension(:),allocatable :: v
            character(len=*),parameter :: xmas = 'xmas' ! 1,2,3,4

            if (s=='A') then
                r%accept = .true.
            else if (s=='R') then
                r%reject = .true.
            else
                if (index(s,'>')>0) then
                    v = split(s(3:), ':') ! example: a<2006:qkq
                    r%operator_arg = index(xmas,s(1:1))
                    r%operator = 2
                    r%operator_val = int(v(1)%str)
                    r%goto = v(2)%str ! can be a workflow or A or R
                else if (index(s,'<')>0) then
                    v = split(s(3:), ':')
                    r%operator_arg = index(xmas,s(1:1))
                    r%operator = 1
                    r%operator_val = int(v(1)%str)
                    r%goto = v(2)%str ! can be a workflow or A or R
                else
                    r%goto = s  ! it's a workflow name
                end if
            end if
        end function parse_rule

end program problem_19
