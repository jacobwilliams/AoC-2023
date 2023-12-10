program problem_7

use iso_fortran_env, only: ip => int64 ! use long ints
use aoc_utilities

implicit none

integer :: i, iunit, n_lines, icase
character(len=:),allocatable :: line
type(string),dimension(:),allocatable :: vals
logical :: done

type :: hand
    character(len=1),dimension(5) :: cards !! the cards in a hand
    integer(ip) :: bid = 0 ! the bit for the hand
    integer :: type = 0 ! the hand type
end type hand
type(hand),dimension(:),allocatable :: hands !! array of hands

character(len=1),dimension(2),parameter :: cases = ['a','b']
character(len=1),dimension(*),parameter :: cards = ['A', 'K', 'Q', 'J', 'T', &
                                                    '9', '8', '7', '6', '5', &
                                                    '4', '3', '2']
character(len=1),dimension(*),parameter :: cards_with_joker = ['A', 'K', 'Q', 'T', &
                                                               '9', '8', '7', '6', '5', &
                                                               '4', '3', '2', 'J']

integer,parameter :: FIVE_OF_A_KIND  = 1 ! the hand types
integer,parameter :: FOUR_OF_A_KIND  = 2
integer,parameter :: FULL_HOUSE      = 3
integer,parameter :: THREE_OF_A_KIND = 4
integer,parameter :: TWO_PAIR        = 5
integer,parameter :: ONE_PAIR        = 6
integer,parameter :: HIGH_CARD       = 7

call clk%tic()

! read the data file:
! open(newunit=iunit, file='inputs/day7_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day7.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
allocate(hands(n_lines))
do i = 1, n_lines
    line = read_line(iunit); vals = split(line,' ')
    hands(i)%cards = str_to_array(vals(1)%str)
    hands(i)%bid   = int(vals(2))
end do
close(iunit)

do icase = 1, 2  ! first time normally, second time processing the jokers
    ! sort the list from worst to best to determinte the rank of each hand
    hands%type = 0 ! reinitialize since they need to be recomputed
    do ! bubble sort !
        done = .true.
        do i = 1, n_lines-1
            if (beats(hands(i), hands(i+1), icase==2)) then
                call swap_hands(hands(i), hands(i+1)); done = .false.
            end if
        end do
        if (done) exit
    end do
    write(*,*) '7'//cases(icase)//':', sum( hands%bid * [(i, i = 1, n_lines)])
end do

call clk%toc('7')

contains

    integer function hand_type(me, with_jokers)
        !! returns the type of hand
        class(hand),intent(in) :: me
        logical,intent(in) :: with_jokers !! if considering jokers
        integer,dimension(5) :: i
        integer,dimension(:),allocatable :: u
        integer :: n_jokers, n_unique
        character(len=1),dimension(5) :: h

        h = me%cards
        i = ichar(h)  ! convert to code
        u = unique(i) ! unique elements
        n_unique = size(u)

        if (with_jokers .and. any(h=='J')) then

            ! have to pick best hand for all jokers
            ! just do it by inspection
            n_jokers = count(h=='J')
            select case (n_jokers)
            case(5)
                hand_type = FIVE_OF_A_KIND !jjjjj
            case(4)
                hand_type = FIVE_OF_A_KIND !jjjja
            case(3)
                ! can always turn to 4 or 5 of a kind
                if (n_unique==2) then !jjjaa
                    hand_type = FIVE_OF_A_KIND
                else ! jjjab
                    hand_type = FOUR_OF_A_KIND
                end if
            case(2)
                if (n_unique==2) then !jjaaa
                    hand_type = FIVE_OF_A_KIND
                else if (n_unique==3) then !jjaab
                    hand_type = FOUR_OF_A_KIND
                else !jjabc
                    hand_type = THREE_OF_A_KIND
                end if
            case(1)
                if (n_unique==2) then !jaaaa
                    hand_type = FIVE_OF_A_KIND
                else if (n_unique==3) then
                    if ( any([count(i==u(1))==3, &
                              count(i==u(2))==3 , &
                              count(i==u(3))==3]) ) then !jaaab
                        hand_type = FOUR_OF_A_KIND
                    elseif ( (count(i==u(1))==2 .and. count(i==u(2))==2) .or. &
                             (count(i==u(1))==2 .and. count(i==u(3))==2) .or. &
                             (count(i==u(2))==2 .and. count(i==u(2))==2) ) then !jaabb
                        hand_type = FULL_HOUSE
                    end if
                else if (n_unique==4) then !jaabc
                    hand_type = THREE_OF_A_KIND
                else if (n_unique==5) then !jabcd
                    hand_type = ONE_PAIR
                end if
            end select

        else

            !Every hand is exactly one type. From strongest to weakest, they are:
            select case (n_unique)
            case(1) ! aaaaa
                hand_type = FIVE_OF_A_KIND
            case(2) ! aaaab, aaabb
                if ((count(i==u(1))==4 .and. count(i==u(2))==1) .or. &
                    (count(i==u(1))==1 .and. count(i==u(2))==4) ) then
                    hand_type = FOUR_OF_A_KIND
                else if ((count(i==u(1))==3 .and. count(i==u(2))==2) .or. &
                        (count(i==u(1))==2 .and. count(i==u(2))==3) ) then
                    hand_type = FULL_HOUSE
                end if
            case(3) ! aaabc, aabcc
                if ( any([count(i==u(1))==3, count(i==u(2))==3 , count(i==u(3))==3]) ) then
                    hand_type = THREE_OF_A_KIND
                else if ( any([ count(i==u(1))==2 .and. count(i==u(2))==2,  &
                                count(i==u(1))==2 .and. count(i==u(3))==2, &
                                count(i==u(2))==2 .and. count(i==u(3))==2]) ) then
                    hand_type = TWO_PAIR
                end if
            case(4) !aabcd
                hand_type = ONE_PAIR
            case(5) !abcde
                hand_type = HIGH_CARD ! High card, where all cards' labels are distinct: 23456
            end select

        end if
    end function hand_type

    logical function beats(hand1, hand2, with_jokers)
        !! return true if hand1 beats hand2 (has a higher score)
        class(hand),intent(inout) :: hand1, hand2
        logical,intent(in) :: with_jokers !! if considering jokers
        integer :: i

        associate(h1 => hand1%cards, h2 => hand2%cards)
            ! recompute type if it hasn't already been computed
            if (hand1%type==0) hand1%type = hand_type(hand1, with_jokers)
            if (hand2%type==0) hand2%type = hand_type(hand2, with_jokers)
            if (hand1%type==hand2%type) then
                ! lower index is stronger
                do i = 1, 5
                    if (h1(i)/=h2(i)) then
                        beats = index_in_cards(h1(i),with_jokers) < &
                                index_in_cards(h2(i),with_jokers) ! lower is stronger
                        return
                    end if
                end do
            else
                ! one hand beat the other
                beats = hand1%type < hand2%type  ! lower score is better (1-7)
            end if
        end associate
    end function beats

    integer function index_in_cards(c,with_jokers)
        character(len=1),intent(in) :: c
        logical,intent(in) :: with_jokers !! if considering jokers
        integer :: i
        do i = 1, size(cards_with_joker)
            if (with_jokers) then
                if (c == cards_with_joker(i)) then
                    index_in_cards = i
                    return
                end if
            else
                if (c == cards(i)) then
                    index_in_cards = i
                    return
                end if
            end if
        end do
        error stop 'card not found'
    end function index_in_cards

    pure elemental subroutine swap_hands(i1,i2)
    !! swap function for hand type
    type(hand),intent(inout) :: i1
    type(hand),intent(inout) :: i2
    type(hand) :: tmp
    tmp = i1
    i1  = i2
    i2  = tmp
    end subroutine swap_hands

end program problem_7