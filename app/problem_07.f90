program problem_7

use iso_fortran_env, only: ip => int64 ! use long ints
use aoc_utilities

implicit none

integer :: i, iunit, n_lines, itmp
character(len=:),allocatable :: line, tmp
type(string),dimension(:),allocatable :: vals, hands
integer(ip),dimension(:),allocatable :: bids
logical :: done

character(len=1),dimension(*),parameter :: cards = ['A', 'K', 'Q', 'J', 'T', &
                                                    '9', '8', '7', '6', '5', &
                                                    '4', '3', '2']

! open(newunit=iunit, file='inputs/day7_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day7.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
allocate(hands(n_lines))
allocate(bids(n_lines))

do i = 1, n_lines
    line = read_line(iunit)
    !if (line=='') cycle ! blank line
    vals  = split(line,' ')  ! hands, bid
    hands(i)%str = vals(1)%str
    bids(i) = int(vals(2))
    !print*, hands(i)%str, bids(i)
end do
close(iunit)

! sort the list from worst to best to determinte the rank of each hand

! bubble sort!!!
do
    done = .true.
    do i = 1, n_lines-1
        if (beats(hands(i)%str, hands(i+1)%str)) then
           ! write(*,*) i, 'beats', i+1
            ! swap them!
            tmp            = hands(i)%str
            hands(i)%str   = hands(i+1)%str
            hands(i+1)%str = tmp
            itmp      = bids(i)
            bids(i)   = bids(i+1)
            bids(i+1) = itmp
            done = .false.
        end if
    end do
    if (done) exit
end do

do i = 1, size(bids)
    write(*,*) hands(i)%str
end do

write(*,*) '7a:', sum( bids * [(i, i = 1, size(bids))])

contains

    integer function hand_type(h)
        character(len=1),dimension(:) :: h
        integer,dimension(size(h)) :: i
        integer,dimension(:),allocatable :: j

        i = ichar(h) ! convert to code
        j = unique(i) ! unique elements
        !write(*,*) ' unique: ', j

        !Every hand is exactly one type. From strongest to weakest, they are:

        if (size(j)==1) then

            hand_type = 1 ! Five of a kind, where all five cards have the same label: AAAAA

        else if ( size(j)==2 .and. &
                  ((count(i==j(1))==4 .and. count(i==j(2))==1) .or. &
                   (count(i==j(1))==1 .and. count(i==j(2))==4) ) ) then
            hand_type = 2 ! Four of a kind, where four cards have the same label and one card has a different label: AA8AA

        else if ( size(j)==2 .and. &
            ((count(i==j(1))==3 .and. count(i==j(2))==2) .or. &
             (count(i==j(1))==2 .and. count(i==j(2))==3) ) ) then
            hand_type = 3 ! Full house, where three cards have the same label, and the remaining two cards share a different label: 23332

        else if ( size(j)==3 .and. &
            (count(i==j(1))==3 .or. count(i==j(2))==3 .or. count(i==j(3))==3) ) then
            hand_type = 4 ! Three of a kind, where three cards have the same label, and the remaining two cards
                          ! are each different from any other card in the hand: TTT98

        else if ( size(j)==3 .and. &
                 ((count(i==j(1))==2 .and. count(i==j(2))==2) .or. &
                  (count(i==j(1))==2 .and. count(i==j(3))==2) .or. &
                  (count(i==j(2))==2 .and. count(i==j(3))==2) ) ) then
            hand_type = 5 ! Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432

        else if (size(j)==4) then
            hand_type = 6 ! One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4

        else
            hand_type = 7 ! High card, where all cards' labels are distinct: 23456
        end if

    end function hand_type

    logical function beats(hand1, hand2)
        !! return true if hand1 beats hand2 (has a higher score)
        character(len=*),intent(in) :: hand1, hand2

        integer :: i, hand_type_1, hand_type_2, i1, i2, j
        character(len=1),dimension(len(hand1)) :: h1, h2

        ! transfer to array:
        do i = 1, len(hand1)
            h1(i) = hand1(i:i)
            h2(i) = hand2(i:i)
        end do

        hand_type_1 = hand_type(h1)
        hand_type_2 = hand_type(h2)

        !write(*,*) hand_type_1,hand_type_1

        if (hand_type_1==hand_type_2) then
            ! lower index is stronger
            do i = 1, size(h1)
                if (h1(i)/=h2(i)) then
                    beats = index_in_cards(h1(i)) < index_in_cards(h2(i)) ! lower is stronger
                    return
                end if
            end do
        else
            ! one hand beat the other
            beats = hand_type_1 < hand_type_2  ! lower score is better (1-7)
        end if

    end function beats

    integer function index_in_cards(c)
        character(len=1),intent(in) :: c
        integer :: i
        do i = 1, size(cards)
            if (c == cards(i)) then
                index_in_cards = i
                return
            end if
        end do
        error stop 'card not found'
    end function index_in_cards



end program problem_7