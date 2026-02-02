! Oregon Trail 1978 - Fortran 2018 Translation
! Original BASIC program from Creative Computing, July-August 1978
! Translated to Fortran by Grok Code Fast 1
! untested, unverified

module oregon_trail_module

    use, intrinsic :: iso_fortran_env, only : stdin => input_unit
    implicit none

    ! Game state variables
    integer :: a        ! Amount spent on animals (oxen)
    integer :: b        ! Amount spent on ammunition (bullets)
    real :: b1          ! Actual response time for shooting
    real :: b3          ! Clock time at start of shooting input
    integer :: c        ! Amount spent on clothing
    integer :: c1       ! Flag for insufficient clothing in cold weather
    character(len=3) :: c_str  ! Yes/No response
    integer :: d1       ! Counter in generating events
    integer :: d3       ! Turn number for setting date
    integer :: d9       ! Choice of shooting expertise level
    integer :: e        ! Choice of eating
    integer :: f        ! Amount spent on food
    integer :: f1       ! Flag for clearing South Pass
    integer :: f2       ! Flag for clearing Blue Mountains
    real :: f9          ! Fraction of 2 weeks traveled on final turn
    integer :: k8       ! Flag for injury
    integer :: l1       ! Flag for blizzard
    integer :: m        ! Total mileage whole trip
    integer :: m1       ! Amount spent on miscellaneous supplies
    integer :: m2       ! Total mileage up through previous turn
    integer :: m9       ! Flag for clearing South Pass in setting mileage
    integer :: p        ! Amount spent on items at fort
    real :: r1          ! Random number in choosing events
    integer :: s4       ! Flag for illness
    integer :: s5       ! Hostility of riders factor
    integer :: s6       ! Shooting word selector
    character(len=4), dimension(4) :: s_str = ["BANG", "BLAM", "POW ", "WHAM"]  ! Shooting words
    integer :: t        ! Cash left over after initial purchases
    integer :: t1       ! Choice of tactics when attacked
    integer :: x        ! Choice of action for each turn
    integer :: x1       ! Flag for fort option

    ! Constants
    integer, parameter :: TOTAL_DISTANCE = 2040
    integer, parameter :: INITIAL_CASH = 700
    integer, parameter :: INITIAL_CASH_TOTAL = 900
    integer, parameter :: WAGON_COST = 200

contains

    ! Get random number (equivalent to RND(-1) in BASIC)
    function rnd() result(r)
        real :: r
        call random_number(r)
    end function rnd

    ! Get clock time in seconds (approximation for CLK(0))
    real function clk()
        integer :: count, rate
        call system_clock(count=count, count_rate=rate)
        clk = real(count) / real(rate)
    end function clk

end module oregon_trail_module

program oregon_trail_1978
    use oregon_trail_module
    implicit none

    integer :: ierr

    call random_init(.false., .false.)

    ! Initialize variables
    x1 = -1
    k8 = 0
    s4 = 0
    f1 = 0
    f2 = 0
    m9 = 0
    d3 = 0
    m = 0

    ! Start the game
    call print_instructions()
    call initial_purchases()
    call game_loop()

contains

    subroutine print_instructions()
        print *, "DO YOU NEED INSTRUCTIONS (YES/NO)"
        read(stdin, '(A3)', iostat=ierr) c_str
        if (is_iostat_end(ierr)) stop 'goodbye'

        if (c_str /= "NO") then
            print *, ""
            print *, ""
            print *, "***INSTRUCTIONS***"
            print *, "THIS PROGRAM SIMULATES A TRIP OVER THE OREGON TRAIL FROM"
            print *, "INDEPENDENCE, MISSOURI TO OREGON CITY, OREGON IN 1847."
            print *, "YOUR FAMILY OF FIVE WILL COVER THE 2040 MILE OREGON TRAIL"
            print *, "IN 5-6 MONTHS --- IF YOU MAKE IT ALIVE."
            print *, ""
            print *, "YOU HAD SAVED $900 TO SPEND FOR THE TRIP, AND YOU'VE JUST"
            print *, "   PAID $200 FOR A WAGON."
            print *, "YOU WILL NEED TO SPEND THE REST OF YOUR MONEY ON THE"
            print *, "   FOLLOWING ITEMS:"
            print *, ""
            print *, "     OXEN - YOU CAN SPEND $200-$300 ON YOUR TEAM"
            print *, "            THE MORE YOU SPEND, THE FASTER YOU'LL GO"
            print *, "            BECAUSE YOU'LL HAVE BETTER ANIMALS"
            print *, ""
            print *, "     FOOD - THE MORE YOU HAVE, THE LESS CHANCE THERE"
            print *, "            IS OF GETTING SICK"
            print *, ""
            print *, "AMMUNITION - $1 BUYS A BELT OF 50 BULLETS"
            print *, "            YOU WILL NEED BULLETS FOR ATTACKS BY ANIMALS"
            print *, "            AND BANDITS, AND FOR HUNTING FOOD"
            print *, ""
            print *, "CLOTHING - THIS IS ESPECIALLY IMPORTANT FOR THE COLD"
            print *, "            WEATHER YOU WILL ENCOUNTER WHEN CROSSING"
            print *, "            THE MOUNTAINS"
            print *, ""
            print *, "MISCELLANEOUS SUPPLIES - THIS INCLUDES MEDICINE AND"
            print *, "            OTHER THINGS YOU WILL NEED FOR SICKNESS"
            print *, "            AND EMERGENCY REPAIRS"
            print *, ""
            print *, ""
            print *, "YOU CAN SPEND ALL YOUR MONEY BEFORE YOU START YOUR TRIP -"
            print *, "OR YOU CAN SAVE SOME OF YOUR CASH TO SPEND AT FORTS ALONG"
            print *, "THE WAY WHEN YOU RUN LOW. HOWEVER, ITEMS COST MORE AT"
            print *, "THE FORTS. YOU CAN ALSO GO HUNTING ALONG THE WAY TO GET"
            print *, "MORE FOOD."
            print *, 'WHENEVER YOU HAVE TO USE YOUR TRUSTY RIFLE ALONG THE WAY,'
            print *, 'YOU WILL BE TOLD TO TYPE IN A WORD (ONE THAT SOUNDS LIKE A'
            print *, 'GUN SHOT). THE FASTER YOU TYPE IN THAT WORD AND HIT THE'
            print *, '"RETURN" KEY, THE BETTER LUCK YOU''LL HAVE WITH YOUR GUN.'
            print *, ""
            print *, "AT EACH TURN, ALL ITEMS ARE SHOWN IN DOLLAR AMOUNTS"
            print *, "EXCEPT BULLETS"
            print *, "WHEN ASKED TO ENTER MONEY AMOUNTS, DON'T USE A ""$""."
            print *, ""
            print *, "GOOD LUCK!!!"
        end if
        print *, ""
    end subroutine print_instructions

    subroutine initial_purchases()
        logical :: valid_purchase

        valid_purchase = .false.
        do while (.not. valid_purchase)
            print *, "HOW GOOD A SHOT ARE YOU WITH YOUR RIFLE?"
            print *, "  (1) ACE MARKSMAN,  (2) GOOD SHOT,  (3) FAIR TO MIDDLIN'"
            print *, "         (4) NEED MORE PRACTICE,  (5) SHAKY KNEES"
            print *, "ENTER ONE OF THE ABOVE -- THE BETTER YOU CLAIM YOU ARE, THE"
            print *, "FASTER YOU'LL HAVE TO BE WITH YOUR GUN TO BE SUCCESSFUL."
            read (stdin, '(i1)', iostat=ierr) d9
            if (is_iostat_end(ierr)) stop 'goodbye'
            if (d9 > 5) d9 = 0

            print *, ""
            print *, ""
            print *, "HOW MUCH DO YOU WANT TO SPEND ON YOUR OXEN TEAM"
            read (stdin, '(i3)', iostat=ierr) a
            if (is_iostat_end(ierr)) stop 'goodbye'
            do while (a < 200 .or. a > 300)
                if (a < 200) then
                    print *, "NOT ENOUGH"
                else
                    print *, "TOO MUCH"
                end if
                print *, "HOW MUCH DO YOU WANT TO SPEND ON YOUR OXEN TEAM"
                read (stdin, '(i3)', iostat=ierr) a
                if (is_iostat_end(ierr)) stop 'goodbye'
            end do

            print *, "HOW MUCH DO YOU WANT TO SPEND ON FOOD"
            read (stdin, '(i3)', iostat=ierr) f
            if (is_iostat_end(ierr)) stop 'goodbye'
            do while (f < 0)
                print *, "IMPOSSIBLE"
                print *, "HOW MUCH DO YOU WANT TO SPEND ON FOOD"
                read (stdin, '(i3)', iostat=ierr) f
                if (is_iostat_end(ierr)) stop 'goodbye'
            end do

            print *, "HOW MUCH DO YOU WANT TO SPEND ON AMMUNITION"
            read (stdin, '(i3)', iostat=ierr) b
            if (is_iostat_end(ierr)) stop 'goodbye'
            do while (b < 0)
                print *, "IMPOSSIBLE"
                print *, "HOW MUCH DO YOU WANT TO SPEND ON AMMUNITION"
                read (stdin, '(i3)', iostat=ierr) b
                if (is_iostat_end(ierr)) stop 'goodbye'
            end do

            print *, "HOW MUCH DO YOU WANT TO SPEND ON CLOTHING"
            read (stdin, '(i3)', iostat=ierr) c
            if (is_iostat_end(ierr)) stop 'goodbye'
            do while (c < 0)
                print *, "IMPOSSIBLE"
                print *, "HOW MUCH DO YOU WANT TO SPEND ON CLOTHING"
                read (stdin, '(i3)', iostat=ierr) c
                if (is_iostat_end(ierr)) stop 'goodbye'
            end do

            print *, "HOW MUCH DO YOU WANT TO SPEND ON MISCELLANEOUS SUPPLIES"
            read (stdin, '(i3)', iostat=ierr) m1
            if (is_iostat_end(ierr)) stop 'goodbye'
            do while (m1 < 0)
                print *, "IMPOSSIBLE"
                print *, "HOW MUCH DO YOU WANT TO SPEND ON MISCELLANEOUS SUPPLIES"
                read (stdin, '(i3)', iostat=ierr) m1
                if (is_iostat_end(ierr)) stop 'goodbye'
            end do

            t = INITIAL_CASH - a - f - b - c - m1
            if (t >= 0) then
                valid_purchase = .true.
            else
                print *, "YOU OVERSPENT--YOU ONLY HAD $700 TO SPEND.  BUY AGAIN"
            end if
        end do

        b = 50 * b
        print *, "AFTER ALL YOUR PURCHASES, YOU NOW HAVE ", t, " DOLLARS LEFT"
        print *, ""
        print *, "MONDAY MARCH 29 1847"
        print *, ""
    end subroutine initial_purchases

    subroutine game_loop()
        do while (m < TOTAL_DISTANCE)
            call turn_logic()
        end do
        call final_turn()
    end subroutine game_loop

    subroutine turn_logic()

        d3 = d3 + 1
        print *, ""
        call print_date()
        print *, ""

        ! Doctor's bill
        if (s4 == 1 .or. k8 == 1) then
            t = t - 20
            if (t < 0) then
                call die_no_doctor()
            end if
            print *, "DOCTOR'S BILL IS $20"
            k8 = 0
            s4 = 0
        end if

        ! Print mileage
        if (m9 == 1) then
            print *, "TOTAL MILEAGE IS 950"
        else
            print *, "TOTAL MILEAGE IS", m
        end if
        print *, "FOOD","BULLETS","CLOTHING","MISC. SUPP.","CASH"
        print *, f, b, c, m1, t

        ! Action choice
        if (x1 == -1) then
            print *, "DO YOU WANT TO (1) HUNT, OR (2) CONTINUE"
            read(stdin, '(i1)', iostat=ierr) x
            if (ierr /= 0) x = 3
            if (is_iostat_end(ierr)) stop 'goodbye'
            if (x == 1) then
                x = 2
            else
                x = 3
            end if
        else
            print *, "DO YOU WANT TO (1) STOP AT THE NEXT FORT, (2) HUNT, "
            print *, "OR (3) CONTINUE"
            read(stdin, '(i1)', iostat=ierr) x
            if (ierr /= 0) x = 3
            if (is_iostat_end(ierr)) stop 'goodbye'
            if (x < 1 .or. x > 3) x = 3
        end if

        select case (x)
        case (1)
            call fort_stop()
        case (2)
            call hunting()
        case (3)
            ! Continue
        end select

        ! Eating
        if (f < 13) call die_starvation()

        print *, "DO YOU WANT TO EAT (1) POORLY  (2) MODERATELY"
        print *, "OR (3) WELL"
        read(stdin, '(i1)', iostat=ierr) e
        if (ierr /= 0) e = 2
        if (is_iostat_end(ierr)) stop 'goodbye'

        if (e < 1 .or. e > 3) e = 2  ! Default to moderately

        f = f - 8 - 5 * e
        if (f < 0) then
            f = f + 8 + 5 * e
            print *, "YOU CAN'T EAT THAT WELL"
            ! Loop back, but for simplicity, set to poorly
            e = 1
            f = f - 8 - 5 * e
        end if

        ! Travel distance
        m = m + 200 + (a - 220) / 5 + 10 * rnd()
        l1 = 0
        c1 = 0

        ! Events
        call riders_attack()
        if (m >= TOTAL_DISTANCE) return
        call random_events()
        if (m >= TOTAL_DISTANCE) return
        call mountains()
    end subroutine turn_logic

    subroutine print_date()
        integer :: month, day
        if (d3 > 10) then
            if (d3 - 10 <= 9) then
                select case (d3 - 10)
                case (1)
                    print *, "APRIL 12 "
                case (2)
                    print *, "APRIL 26 "
                case (3)
                    print *, "MAY 10 "
                case (4)
                    print *, "MAY 24 "
                case (5)
                    print *, "JUNE 7 "
                case (6)
                    print *, "JUNE 21 "
                case (7)
                    print *, "JULY 5 "
                case (8)
                    print *, "JULY 19 "
                case (9)
                    print *, "AUGUST 2 "
                end select
            else
                select case (d3 - 10)
                case (10)
                    print *, "AUGUST 16 "
                case (11)
                    print *, "AUGUST 31 "
                case (12)
                    print *, "SEPTEMBER 13 "
                case (13)
                    print *, "SEPTEMBER 27 "
                case (14)
                    print *, "OCTOBER 11 "
                case (15)
                    print *, "OCTOBER 25 "
                case (16)
                    print *, "NOVEMBER 8 "
                case (17)
                    print *, "NOVEMBER 22 "
                case (18)
                    print *, "DECEMBER 6 "
                case (19)
                    print *, "DECEMBER 20 "
                case default
                    print *, "YOU HAVE BEEN ON THE TRAIL TOO LONG  ------"
                    print *, "YOUR FAMILY DIES IN THE FIRST BLIZZARD OF WINTER"
                    call die()
                end select
            end if
        else
            select case (d3)
            case (1)
                print *, "APRIL 12 "
            case (2)
                print *, "APRIL 26 "
            case (3)
                print *, "MAY 10 "
            case (4)
                print *, "MAY 24 "
            case (5)
                print *, "JUNE 7 "
            case (6)
                print *, "JUNE 21 "
            case (7)
                print *, "JULY 5 "
            case (8)
                print *, "JULY 19 "
            case (9)
                print *, "AUGUST 2 "
            case (10)
                print *, "AUGUST 16 "
            end select
        end if
        print *, "1847"
    end subroutine print_date

    subroutine fort_stop()
        print *, "ENTER WHAT YOU WISH TO SPEND ON THE FOLLOWING"
        print *, "FOOD"
        call spend_at_fort(f, 2.0/3.0)
        print *, "AMMUNITION"
        call spend_at_fort(b, 2.0/3.0 * 50)
        print *, "CLOTHING"
        call spend_at_fort(c, 2.0/3.0)
        print *, "MISCELLANEOUS SUPPLIES"
        call spend_at_fort(m1, 2.0/3.0)
        m = m - 45
    end subroutine fort_stop

    subroutine spend_at_fort(item, multiplier)
        integer, intent(inout) :: item
        real, intent(in) :: multiplier
        read (stdin, '(i4)', iostat=ierr) p
        if (is_iostat_end(ierr)) stop 'goodbye'
        if (p < 0) then
            p = 0
        else
            t = t - p
            if (t < 0) then
                print *, "YOU DON'T HAVE THAT MUCH--KEEP YOUR SPENDING DOWN"
                print *, "YOU MISS YOUR CHANCE TO SPEND ON THAT ITEM"
                t = t + p
                p = 0
            else
                item = item + int(multiplier * p)
            end if
        end if
    end subroutine spend_at_fort

    subroutine hunting()
        if (b <= 39) then
            print *, "TOUGH---YOU NEED MORE BULLETS TO GO HUNTING"
            return
        end if
        m = m - 45
        call shooting_subroutine()
        if (b1 <= 1) then
            print *, "RIGHT BETWEEN THE EYES---YOU GOT A BIG ONE!!!!"
            print *, "FULL BELLIES TONIGHT!"
            f = f + 52 + rnd() * 6
            b = b - 10 - int(rnd() * 4)
        else
            if (100 * rnd() < 13 * b1) then
                print *, "YOU MISSED---AND YOUR DINNER GOT AWAY....."
            else
                f = f + 48 - 2 * b1
                print *, "NICE SHOT--RIGHT ON TARGET--GOOD EATIN' TONIGHT!!"
                b = b - 10 - 3 * b1
            end if
        end if
    end subroutine hunting

    subroutine riders_attack()
        if (rnd() * 10 * ((m/100.0 - 4)**2 + 72) / ((m/100.0 - 4)**2 + 12) > 1) return

        print *, "RIDERS AHEAD.  THEY "
        s5 = 0
        if (rnd() < 0.8) then
            print *, "DON'T "
            s5 = 1
        end if
        print *, "LOOK HOSTILE"
        print *, "TACTICS"
        print *, "(1) RUN  (2) ATTACK  (3) CONTINUE  (4) CIRCLE WAGONS"
        read (stdin, '(i1)', iostat=ierr) t1
        if (is_iostat_end(ierr)) stop 'goodbye'
        if (t1 < 1 .or. t1 > 4) then
            t1 = 3  ! Default to continue
        end if

        if (s5 == 1) then
            ! Hostile
            if (t1 > 1) then
                if (t1 > 2) then
                    if (rnd() > 0.8) then
                        b = b - 150
                        m1 = m1 - 15
                    end if
                else
                    call shooting_subroutine()
                    b = b - b1 * 40 - 80
                    if (b1 > 1) then
                        if (b1 <= 4) then
                            print *, "KINDA SLOW WITH YOUR COLT .45"
                        else
                            print *, "LOUSY SHOT---YOU GOT KNIFED"
                            k8 = 1
                        end if
                    else
                        print *, "NICE SHOOTING---YOU DROVE THEM OFF"
                    end if
                end if
            else
                m = m + 20
                m1 = m1 - 15
                b = b - 150
                a = a - 40
            end if
            print *, "RIDERS WERE HOSTILE--CHECK FOR LOSSES"
        else
            ! Friendly
            if (t1 > 1) then
                if (t1 > 2) then
                    m = m - 25
                    call shooting_subroutine()
                    b = b - b1 * 30 - 80
                    if (b1 <= 1) then
                        print *, "QUICKEST DRAW OUTSIDE OF DODGE CITY!!!"
                        print *, "YOU GOT 'EM!"
                    else
                        if (b1 <= 4) then
                            print *, "KINDA SLOW WITH YOUR COLT .45"
                        else
                            print *, "LOUSY SHOT---YOU GOT KNIFED"
                            k8 = 1
                        end if
                    end if
                else
                    m = m - 5
                    b = b - 100
                end if
            else
                m = m + 15
                a = a - 10
            end if
            print *, "RIDERS WERE FRIENDLY, BUT CHECK FOR POSSIBLE LOSSES"
        end if

        if (b < 0) then
            print *, "YOU RAN OUT OF BULLETS AND GOT MASSACRED BY THE RIDERS"
            call die()
        end if
    end subroutine riders_attack

    subroutine random_events()
        real, dimension(15) :: event_probs = [6.0, 11.0, 13.0, 15.0, 17.0, 22.0, 32.0, 35.0, 37.0, 42.0, 44.0, 54.0, 64.0, 69.0, 95.0]
        integer :: event_index

        r1 = 100 * rnd()
        d1 = 0
        do while (d1 < 16)
            d1 = d1 + 1
            if (r1 > event_probs(d1)) cycle
            exit
        end do

        if (d1 > 15) d1 = 16

        select case (d1)
        case (1)
            print *, "WAGON BREAKS DOWN--LOSE TIME AND SUPPLIES FIXING IT"
            m = m - 15 - 5 * rnd()
            m1 = m1 - 8
        case (2)
            print *, "OX INJURES LEG---SLOWS YOU DOWN REST OF TRIP"
            m = m - 25
            a = a - 20
        case (3)
            print *, "BAD LUCK---YOUR DAUGHTER BROKE HER ARM"
            print *, "YOU HAD TO STOP AND USE SUPPLIES TO MAKE A SLING"
            m = m - 5 - 4 * rnd()
            m1 = m1 - 2 - 3 * rnd()
        case (4)
            print *, "OX WANDERS OFF---SPEND TIME LOOKING FOR IT"
            m = m - 17
        case (5)
            print *, "YOUR SON GETS LOST---SPEND HALF THE DAY LOOKING FOR HIM"
            m = m - 10
        case (6)
            print *, "UNSAFE WATER--LOSE TIME LOOKING FOR CLEAN SPRING"
            m = m - 10 * rnd() - 2
        case (7)
            if (m > 950) then
                call cold_weather()
                return
            end if
            print *, "HEAVY RAINS---TIME AND SUPPLIES LOST"
            f = f - 10
            b = b - 500
            m1 = m1 - 15
            m = m - 10 * rnd() - 5
        case (8)
            print *, "BANDITS ATTACK"
            call shooting_subroutine()
            b = b - 20 * b1
            if (b < 0) then
                print *, "YOU RAN OUT OF BULLETS---THEY GET LOTS OF CASH"
                t = t / 3
            else
                print *, "YOU GOT SHOT IN THE LEG AND THEY TOOK ONE OF YOUR OXEN"
                k8 = 1
                m1 = m1 - 5
                a = a - 20
            end if
        case (9)
            print *, "THERE WAS A FIRE IN YOUR WAGON--FOOD AND SUPPLIES DAMAGE"
            f = f - 40
            b = b - 400
            m1 = m1 - rnd() * 8 - 3
            m = m - 15
        case (10)
            print *, "LOSE YOUR WAY IN HEAVY FOG---TIME IS LOST"
            m = m - 10 - 5 * rnd()
        case (11)
            print *, "YOU KILLED A POISONOUS SNAKE AFTER IT BIT YOU"
            b = b - 10
            m1 = m1 - 5
            if (m1 < 0) then
                print *, "YOU DIE OF SNAKEBITE SINCE YOU HAVE NO MEDICINE"
                call die()
            end if
        case (12)
            print *, "WAGON GETS SWAMPED FORDING RIVER--LOSE FOOD AND CLOTHES"
            f = f - 30
            c = c - 20
            m = m - 20 - 20 * rnd()
        case (13)
            print *, "WILD ANIMALS ATTACK!"
            if (b > 39) then
                call shooting_subroutine()
                if (b1 > 2) then
                    print *, "SLOW ON THE DRAW---THEY GOT AT YOUR FOOD AND CLOTHES"
                    b = b - 20 * b1
                    c = c - b1 * 4
                    f = f - b1 * 8
                else
                    print *, "NICE SHOOTIN' PARTNER---THEY DIDN'T GET MUCH"
                end if
            else
                print *, "YOU WERE TOO LOW ON BULLETS--"
                print *, "THE WOLVES OVERPOWERED YOU"
                k8 = 1
                call die_injury()
            end if
        case (14)
            print *, "HAIL STORM---SUPPLIES DAMAGED"
            m = m - 5 - rnd() * 10
            b = b - 200
            m1 = m1 - 4 - rnd() * 3
        case (15)
            if (e == 1) then
                call illness()
                return
            end if
            if (e == 3) then
                if (rnd() < 0.5) then
                    call illness()
                    return
                end if
            else
                if (rnd() < 0.25) then
                    call illness()
                    return
                end if
            end if
        case (16)
            print *, "HELPFUL INDIANS SHOW YOU WHERE TO FIND MORE FOOD"
            f = f + 14
        end select
    end subroutine random_events

    subroutine mountains()
        if (m <= 950) return

        if (rnd() * 10 > ((9 - ((m/100.0 - 15)**2 + 72)) / ((m/100.0 - 15)**2 + 12))) return

        print *, "RUGGED MOUNTAINS"
        if (rnd() > 0.1) then
            if (rnd() > 0.11) then
                print *, "THE GOING GETS SLOW"
                m = m - 45 - rnd() / 0.02
            else
                print *, "WAGON DAMAGED!---LOSE TIME AND SUPPLIES"
                m1 = m1 - 5
                b = b - 200
                m = m - 20 - 30 * rnd()
            end if
        else
            print *, "YOU GOT LOST---LOSE VALUABLE TIME TRYING TO FIND TRAIL!"
            m = m - 60
        end if

        if (f1 == 1) then
            if (rnd() < 0.8) then
                call blizzard()
                return
            end if
            print *, "YOU MADE IT SAFELY THROUGH SOUTH PASS--NO SNOW"
        end if

        if (m < 1700) then
            if (f2 == 1) then
                if (rnd() < 0.7) then
                    call blizzard()
                    return
                end if
            end if
        end if

        if (m > 950) then
            m9 = 1
        end if
    end subroutine mountains

    subroutine blizzard()
        print *, "BLIZZARD IN MOUNTAIN PASS--TIME AND SUPPLIES LOST"
        l1 = 1
        f = f - 25
        m1 = m1 - 10
        b = b - 300
        m = m - 30 - 40 * rnd()
        if (c < 18 + 2 * rnd()) then
            call cold_weather()
        end if
    end subroutine blizzard

    subroutine cold_weather()
        print *, "COLD WEATHER---BRRRRRRR!---YOU "
        if (c > 22 + 4 * rnd()) then
            print *, "DON'T "
        end if
        print *, "HAVE ENOUGH CLOTHING TO KEEP YOU WARM"
        if (c1 == 0) return
        call illness()
    end subroutine cold_weather

    subroutine illness()
        if (100 * rnd() < 10 + 35 * (e - 1)) then
            print *, "SERIOUS ILLNESS---"
            print *, "YOU MUST STOP FOR MEDICAL ATTENTION"
            m1 = m1 - 10
            s4 = 1
        else
            if (100 * rnd() < 100 - (40 / 4**(e - 1))) then
                print *, "BAD ILLNESS---MEDICINE USED"
                m = m - 5
                m1 = m1 - 5
            else
                print *, "MILD ILLNESS---MEDICINE USED"
                m = m - 5
                m1 = m1 - 2
            end if
        end if

        if (m1 < 0) then
            print *, "YOU RAN OUT OF MEDICAL SUPPLIES"
            call die_injury()
        end if

        if (l1 == 1) then
            ! Back to mountains logic
        end if
    end subroutine illness

    subroutine shooting_subroutine()
        character(len=4) :: input_word
        s6 = int(rnd() * 4) + 1
        print *, "TYPE ", trim(s_str(s6))
        b3 = clk()
        read(stdin, '(a4)', iostat=ierr) input_word
        if (is_iostat_end(ierr)) stop 'goodbye'
        b1 = clk()
        b1 = ((b1 - b3) * 3600) - (d9 - 1)
        if (b1 < 0) b1 = 0
        if (input_word /= s_str(s6)) b1 = 9
    end subroutine shooting_subroutine

    subroutine die_starvation()
        print *, "YOU RAN OUT OF FOOD AND STARVED TO DEATH"
        call die()
    end subroutine die_starvation

    subroutine die_no_doctor()
        t = 0
        print *, "YOU CAN'T AFFORD A DOCTOR"
        call die_injury()
    end subroutine die_no_doctor

    subroutine die_injury()
        print *, "YOU DIED OF "
        if (k8 == 1) then
            print *, "INJURIES"
        else
            print *, "PNEUMONIA"
        end if
        call die()
    end subroutine die_injury

    subroutine die()
        print *, ""
        print *, "DUE TO YOUR UNFORTUNATE SITUATION, THERE ARE A FEW"
        print *, "FORMALITIES WE MUST GO THROUGH"
        print *, ""
        print *, "WOULD YOU LIKE A MINISTER?"
        read (stdin, '(a3)', iostat=ierr) c_str
        if (is_iostat_end(ierr)) stop 'goodbye'
        print *, "WOULD YOU LIKE A FANCY FUNERAL?"
        read (stdin, '(a3)', iostat=ierr) c_str
        if (is_iostat_end(ierr)) stop 'goodbye'
        print *, "WOULD YOU LIKE US TO INFORM YOUR NEXT OF KIN?"
        read (stdin, '(a3)', iostat=ierr) c_str
        if (is_iostat_end(ierr)) stop 'goodbye'
        if (c_str == "YES") then
            print *, "THAT WILL BE $4.50 FOR THE TELEGRAPH CHARGE."
            print *, ""
        else
            print *, "BUT YOUR AUNT SADIE IN ST. LOUIS IS REALLY WORRIED ABOUT YOU"
            print *, ""
        end if
        print *, "WE THANK YOU FOR THIS INFORMATION AND WE ARE SORRY YOU"
        print *, "DIDN'T MAKE IT TO THE GREAT TERRITORY OF OREGON"
        print *, "BETTER LUCK NEXT TIME"
        print *, ""
        print *, ""
        print *, "                              SINCERELY"
        print *, ""
        print *, "                    THE OREGON CITY CHAMBER OF COMMERCE"
        stop
    end subroutine die

    subroutine final_turn()
        f9 = (TOTAL_DISTANCE - m2) / real(m - m2)
        f = f + (1 - f9) * (8 + 5 * e)
        print *, ""

        print *, "YOU FINALLY ARRIVED AT OREGON CITY"
        print *, "AFTER 2040 LONG MILES---HOORAY!!!!!"
        print *, "A REAL PIONEER!"
        print *, ""

        f9 = int(f9 * 14)
        d3 = d3 * 14 + f9
        f9 = f9 + 1
        if (f9 < 8) f9 = f9 - 7

        ! Print final date
        if (d3 > 124) then
            d3 = d3 - 93
            print *, "JULY ", d3, " 1847"
        else if (d3 > 155) then
            d3 = d3 - 124
            print *, "AUGUST ", d3, " 1847"
        else if (d3 > 185) then
            d3 = d3 - 155
            print *, "SEPTEMBER ", d3, " 1847"
        else if (d3 > 216) then
            d3 = d3 - 185
            print *, "OCTOBER ", d3, " 1847"
        else if (d3 > 246) then
            d3 = d3 - 216
            print *, "NOVEMBER ", d3, " 1847"
        else
            d3 = d3 - 246
            print *, "DECEMBER ", d3, " 1847"
        end if

        print *, ""
        print *, "FOOD","BULLETS","CLOTHING","MISC. SUPP.","CASH"
        if (b < 0) b = 0
        if (c < 0) c = 0
        if (m1 < 0) m1 = 0
        if (t < 0) t = 0
        if (f < 0) f = 0
        print '(i0,1x,i0,1x,i0,1x,i0,1x,i0)', f, b, c, m1, t
        print *, ""
        print *, "PRESIDENT JAMES K. POLK SENDS YOU HIS"
        print *, "HEARTIEST CONGRATULATIONS"
        print *, ""
        print *, "AND WISHES YOU A PROSPEROUS LIFE AHEAD"
        print *, ""
        print *, "AT YOUR NEW HOME"
        stop
    end subroutine final_turn

end program oregon_trail_1978
