program problem_19b

    !! reformulated to operate on ranges of the x,m,a,s ratings

    use aoc_utilities
    use iso_fortran_env, only: wp => real64, ip => int64

    implicit none

    integer :: iunit, n_lines, iline, i
    character(len=:),allocatable :: line !, name
    type(string),dimension(:),allocatable :: vals
    logical :: accepted
    integer(ip) :: total_accepted

    type :: interval
        integer(ip),dimension(2) :: v !! [start, end] of the interval
    end type interval

    type :: part
        type(interval),dimension(4) :: xmas  ! [x,m,a,s] ranges
    end type part
    type(part) :: parts

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
        if (line == '') exit ! done - dont need the parts list for part b
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
    end do

    parts = part(xmas=[interval([1,4000]), &
                       interval([1,4000]), &
                       interval([1,4000]),&
                       interval([1,4000])])

    i = workflow_name_to_index('in')     ! start at the first workflow

    total_accepted = process(i,1,parts)

    write(*,*) '19b: ', total_accepted

    call clk%toc('19b')

    contains

        recursive function process(iworkflow,irule,p) result(iaccepted)
            !! process this part range starting with irule

            integer,intent(in) :: iworkflow
            integer,intent(in) :: irule
            type(part),intent(in) :: p
            integer(ip) :: iaccepted

            integer(ip),dimension(:),allocatable :: itmp, itmp_t, itmp_f
            integer(ip) :: i
            type(part) :: pt, pf

            iaccepted = 0

            associate( r => workflows(iworkflow)%rules(irule) )

                ! check if we are done
                if (r%accept) then
                    iaccepted = get_accepted(p)
                    return
                else if (r%reject) then
                    iaccepted = 0
                    return
                end if

                ! process recursively, by splitting up the intervals when necessary

                if (r%operator>0) then
                    ! process the operator
                    select case (r%operator)
                    case(1) ! <

                        ! always create the two sets and process the two cases below
                        ! [note that some sets maybe empty]
                        !
                        ! example:
                        ![1,200]   < 10   ==> [1,9]    TRUE
                        !                     [10,200] FALSE

                        itmp = [(i, i=p%xmas(r%operator_arg)%v(1),p%xmas(r%operator_arg)%v(2))]
                        itmp_t = pack(itmp, mask = itmp<r%operator_val)  !true
                        itmp_f = pack(itmp, mask = itmp>=r%operator_val) !false

                        if (size(itmp_t)>0) then  ! some are true
                            pt = p
                            pt%xmas(r%operator_arg)%v = [itmp_t(1), itmp_t(size(itmp_t))]
                            if (r%goto=='A') then
                                iaccepted = iaccepted + get_accepted(pt)
                            else if (r%goto/='R') then
                                ! go to next workflow & start at first rule of new workflow
                                iaccepted = iaccepted + process(workflow_name_to_index(r%goto),1,pt)
                            end if
                        end if
                        if (size(itmp_f)>0) then  ! some are false
                            pf = p
                            pf%xmas(r%operator_arg)%v = [itmp_f(1), itmp_f(size(itmp_f))]
                            iaccepted = iaccepted + process(iworkflow,irule+1,pf) ! next rule
                        end if

                    case(2) ! >

                        itmp = [(i, i=p%xmas(r%operator_arg)%v(1),p%xmas(r%operator_arg)%v(2))]
                        itmp_t = pack(itmp, mask = itmp>r%operator_val)  !true
                        itmp_f = pack(itmp, mask = itmp<=r%operator_val) !false

                        if (size(itmp_t)>0) then  ! some are true
                            pt = p
                            pt%xmas(r%operator_arg)%v = [itmp_t(1), itmp_t(size(itmp_t))]
                            if (r%goto=='A') then
                                iaccepted = iaccepted + get_accepted(pt)
                            else if (r%goto/='R') then
                                ! go to next workflow & start at first rule of new workflow
                                iaccepted = iaccepted + process(workflow_name_to_index(r%goto),1,pt)
                            end if
                        end if
                        if (size(itmp_f)>0) then  ! some are false
                            pf = p
                            pf%xmas(r%operator_arg)%v = [itmp_f(1), itmp_f(size(itmp_f))]
                            iaccepted = iaccepted + process(iworkflow,irule+1,pf) ! next rule
                        end if

                    end select

                else
                    ! goto another workflow & start at first rule of new workflow
                    iaccepted = iaccepted + process(workflow_name_to_index(r%goto),1,p)
                end if
            end associate
            !end do

        end function process

        integer(ip) function get_accepted(p)
            !! count of all the parts in the set
            type(part),intent(in) :: p
            get_accepted =  (1 + (p%xmas(1)%v(2) - p%xmas(1)%v(1)))* &
                            (1 + (p%xmas(2)%v(2) - p%xmas(2)%v(1)))* &
                            (1 + (p%xmas(3)%v(2) - p%xmas(3)%v(1)))* &
                            (1 + (p%xmas(4)%v(2) - p%xmas(4)%v(1)))
        end function get_accepted

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

end program problem_19b
