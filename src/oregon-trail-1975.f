C     ******************************************************************
C
C     THE OREGON TRAIL IN FORTRAN 77
C
C     ******************************************************************
C
C     THIS PROGRAM IS A PORT OF THE 1978 VERSION OF "THE OREGON TRAIL"
C     TO ANSI FORTRAN 77, ORIGINALLY WRITTEN IN THE HP TIME-SHARED BASIC
C     BY DON RAWITSCH, BILL HEINEMANN, AND PAUL DILLENBERGER IN 1971.
C
C     AN ADDITIONAL INTEGER FUNCTION TIME() THAT RETURNS THE CURRENT
C     TIME AS A TIMESTAMP IN SECONDS IS REQUIRED AND MUST BE LINKED
C     WITH THE EXECUTABLE, IF NOT PROVIDED BY THE FORTRAN COMPILER
C     ALREADY.
C
C     ******************************************************************
C     AUTHOR:  PHILIPP ENGEL
C     DATE:    2021-04-12
C     VERSION: 1.0
C     LICENCE: ISC
C     ******************************************************************
      PROGRAM OREGON
      EXTERNAL INSTRU, PLAY
      INTEGER  TIME
      LOGICAL  ASK
      CALL random_init(.false., .false.)
      PRINT *,'DO YOU NEED INSTRUCTIONS? (Y/N)'
      IF (ASK()) CALL INSTRU()
      CALL PLAY()
      END program
C     ******************************************************************
      FUNCTION ASK()
C
C     READS USER INPUT AND RETURNS .TRUE. IF INPUT STARTS WITH "Y" AND
C     .FALSE. IF WITH "N".
C
      LOGICAL     ASK
      CHARACTER*3 A
      ASK = .FALSE.
   10 CONTINUE
      READ (*, 100) A
  100 FORMAT (A)
      IF (A(1:1) .NE. 'N' .AND. A(1:1) .NE. 'n' .AND.
     &  A(1:1) .NE. 'Y' .AND. A(1:1) .NE. 'y') THEN
        PRINT *,'PARDON?'
        GOTO 10
      END IF
      IF (A(1:1) .EQ. 'Y' .OR. A(1:1) .EQ. 'y') ASK = .TRUE.
      END
C     ******************************************************************
      FUNCTION INPUT(MIN, MAX)
C
C     READS INTEGER VALUE WITH GIVEN MINIUM AND MAXIMUM FROM USER INPUT.
C
      INTEGER MIN, MAX
      INTEGER INPUT, N
   10 CONTINUE
      READ (*, 100, IOSTAT=N) INPUT
  100 FORMAT (I5)
      IF (N .NE. 0) THEN
        PRINT *,'INVALID.'
        GOTO 10
      END IF
      IF (INPUT .LT. MIN ) THEN
        PRINT *,'TOO LOW.'
        GOTO 10
      ELSE IF (INPUT .GT. MAX) THEN
        PRINT *,'TOO HIGH.'
        GOTO 10
      END IF
      END
C     ******************************************************************
      FUNCTION LEVEL()
C
C     RETURNS THE MARKSMANSHIP LEVEL (1 TO 5).
C
      INTEGER LEVEL
      INTEGER INPUT
      CHARACTER*1 Q
      Q = CHAR(39)
      PRINT *,'HOW GOOD A SHOT ARE YOU WITH YOUR RIFLE?'
      PRINT *,' '
      PRINT *,'  (1) ACE MARKSMAN, (2) GOOD SHOT, (3) FAIR TO MIDDLIN',Q
      PRINT *,'        (4) NEED MORE PRACTICE, (5) SHAKY KNEES'
      PRINT *,' '
      PRINT *,'ENTER ONE OF THE ABOVE -- THE BETTER YOU CLAIM YOU ARE,'
      PRINT *,'THE FASTER YOU',Q,'LL HAVE TO BE WITH YOUR GUN TO BE'
      PRINT *,'SUCCESSFUL.'
      LEVEL = INPUT(1, 5)
      END
C     ******************************************************************
      real FUNCTION RAND()
      call random_number(rand)
      END function rand
C     ******************************************************************
      FUNCTION SHOOT(ILEVL)
C
C     SHOOT STUFF BY LETTING THE PLAYER ENTER INSTANCES OF ONOMATOPOEIA.
C
      EXTERNAL    UPPER
      INTEGER     SHOOT
      REAL        RAND
      INTEGER     ILEVL
      INTEGER     TIME
      CHARACTER*4 A, S(4)
      INTEGER     R, T1, T2
      DATA S /'BANG','BLAM','POW','WHAM'/
      R = INT(RAND() * 4 + 1)
      PRINT *,'TYPE: ',S(R)
      T1 = TIME()
      READ (*, 100) A
  100 FORMAT (A)
      T2 = TIME()
      SHOOT = (ABS(T2 - T1) * 2) - ILEVL - 1
      CALL UPPER(A)
      IF (A .NE. S(R)) SHOOT = 9
      END
C     ******************************************************************
      SUBROUTINE ARRIVE(ITMIL, ILMIL, ICTRN, ICASH, IAMMU, ICLTH, IFOOD,
     &                  IMISC, IEATS)
C
C     FINAL TURN.
C
      INTEGER      ITMIL, ILMIL, ICTRN, ICASH
      INTEGER      IAMMU, ICLTH, IFOOD, IMISC, IEATS
      CHARACTER*10 WKDSTR(7)
      INTEGER      IFINL
      REAL         FFRAC
      COMMON /WDAYS/ WKDSTR
      FFRAC = (2040.0 - ILMIL) / (ITMIL - ILMIL)
      IFOOD = IFOOD + (1 - INT(FFRAC)) * (8 + 5 * IEATS)
      PRINT *,' '
      PRINT *,'YOU FINALLY ARRIVED AT OREGON CITY'
      PRINT *,'AFTER 2040 LONG MILES -- HOORAY!!'
      PRINT *,'A REAL PIONEER!'
      PRINT *,' '
      IFINL = INT(FFRAC * 14)
      ICTRN = ICTRN * 14 + IFINL
      IFINL = IFINL + 1
      IF (IFINL .GT. 7) IFINL = IFINL - 7
      IF (ICTRN .LE. 124) THEN
        ICTRN = ICTRN - 93
        PRINT *,WKDSTR(IFINL),' JULY ',ICTRN,' 1847'
      ELSE IF (ICTRN .LE. 155) THEN
        ICTRN = ICTRN - 124
        PRINT *,WKDSTR(IFINL),' AUGUST ',ICTRN,' 1847'
      ELSE IF (ICTRN .LE. 185) THEN
        ICTRN = ICTRN - 155
        PRINT *,WKDSTR(IFINL),' SEPTEMBER ',ICTRN,' 1847'
      ELSE IF (ICTRN .LE. 216) THEN
        ICTRN = ICTRN - 185
        PRINT *,WKDSTR(IFINL),' OCTOBER ',ICTRN,' 1847'
      ELSE IF (ICTRN .LE. 246) THEN
        ICTRN = ICTRN - 216
        PRINT *,WKDSTR(IFINL),' NOVEMBER ',ICTRN,' 1847'
      ELSE
        ICTRN = ICTRN - 246
        PRINT *,WKDSTR(IFINL),' DECEMBER ',ICTRN,' 1847'
      END IF
      PRINT *,'--------------------------------------------------------'
      PRINT *,'FOOD BULLETS CLOTHING MISC. SUPP. CASH'
      PRINT 100, IFOOD, IAMMU, ICLTH, IMISC, ICASH
  100 FORMAT (I5, ' ', I7, ' ', I8, ' ', I11, ' ', I4)
      PRINT *,' '
      PRINT *,'         PRESIDENT JAMES K. POLK SENDS YOU HIS'
      PRINT *,'               HEARTIEST CONGRATULATIONS'
      PRINT *,' '
      PRINT *,'         AND WISHES YOU A PROSPEROUS LIFE AHEAD'
      PRINT *,'                    AT YOUR NEW HOME'
      PRINT *,' '
      STOP
      END
C     ******************************************************************
      SUBROUTINE BLIZZ(ITMIL, IAMMU, ICLTH, IFOOD, IMISC, IFILL, IFINJ,
     &                 IEATS)
C
C     BLIZZARD IN MOUNTAIN PASS.
C
      EXTERNAL SICK
      REAL     RAND
      INTEGER  ITMIL, IAMMU, ICLTH, IFOOD, IMISC, IFILL, IFINJ, IEATS
      PRINT *,'BLIZZARD IN MOUNTAIN PASS -- TIME AND SUPPLIES LOST.'
      IFOOD = IFOOD - 25
      IMISC = IMISC - 10
      IAMMU = IAMMU - 300
      ITMIL = ITMIL - 30 - INT(40 * RAND())
      IF (ICLTH .LT. 18 + INT(2 * RAND())) THEN
        CALL SICK(IEATS, ITMIL, IMISC, IFILL, IFINJ)
      END IF
      END
C     ******************************************************************
      SUBROUTINE DIE()
C
C     READS FINAL INFORMATION AND STOPS GAME.
C
      LOGICAL     ASK
      CHARACTER*1 Q
      LOGICAL     L
      Q = CHAR(39)
      PRINT *,' '
      PRINT *,'DUE TO YOUR UNFORTUNATE SITUATION, THERE ARE A FEW'
      PRINT *,'FORMALITIES WE MUST GO THROUGH:'
      PRINT *,' '
      PRINT *,'WOULD YOU LIKE A MINISTER? (Y/N)'
      L = ASK()
      PRINT *,'WOULD YOU LIKE A FANCY FUNERAL? (Y/N)'
      L = ASK()
      PRINT *,'WOULD YOU LIKE US TO INFORM YOUR NEXT OF KIN? (Y/N)'
      IF (ASK()) THEN
        PRINT *,'THAT WILL BE $4.50 FOR THE TELEGRAPH CHARGE.'
      ELSE
        PRINT *,'BUT YOUR AUNT SADIE IN ST. LOUIS IS REALLY WORRIED'
        PRINT *,'ABOUT YOU.'
      END IF
      PRINT *,' '
      PRINT *,'WE THANK YOU FOR THIS INFORMATION AND WE ARE SORRY YOU'
      PRINT *,'DIDN',Q,'T MAKE IT TO THE GREAT TERRITORY OF OREGON.'
      PRINT *,'BETTER LUCK NEXT TIME.'
      PRINT *,' '
      PRINT *,'                              SINCERLY,'
      PRINT *,'                 THE OREGON CITY CHAMBER OF COMMERCE'
      PRINT *,' '
      STOP
      END
C     ******************************************************************
      SUBROUTINE DOCTOR(ICASH, IFILL, IFINJ)
C
C     VISIT OL' DOC BLANCHARD.
C
      INTEGER ICASH, IFILL, IFINJ
      CHARACTER*1 Q
      Q = CHAR(39)
      ICASH = ICASH - 20
      IF (ICASH .LT. 0) THEN
        PRINT *,'YOU CAN',Q,'T AFFORD A DOCTOR.'
        IF (IFINJ .EQ. 1) THEN
          PRINT *,'YOU DIED OF INJURIES.'
        ELSE
          PRINT *,'YOU DIED OF PNEUMONIA.'
        END IF
        CALL DIE()
      END IF
      PRINT *,'DOCTOR',Q,'S BILL IS $20.'
      IFILL = 0
      IFINJ = 0
      END
C     ******************************************************************
      SUBROUTINE EAT(IFOOD, IEATS)
C
C     LETS THE PLAYER DECIDE HOW TO EAT.
C
      INTEGER     IFOOD, IEATS
      INTEGER     IAMOU, INPUT
      CHARACTER*1 Q
      Q = CHAR(39)
   10 CONTINUE
      PRINT *,'DO YOU WANT TO EAT'
      PRINT *,'(1) POORLY (2) MODERATELY (3) WELL'
      IEATS = INPUT(1, 3)
      IAMOU = 8 + 5 * IEATS
      IF (IFOOD - IAMOU .LT. 0) THEN
        PRINT *,'YOU CAN',Q,'T EAT THAT WELL.'
        GOTO 10
      END IF
      IFOOD = IFOOD - IAMOU
      END
C     ******************************************************************
      SUBROUTINE FORT(ICASH, IFOOD, IAMMU, ICLTH, IMISC)
C
C     YE OLDE FORT SHOPPE.
C
      INTEGER INPUT
      INTEGER ICASH, IFOOD, IAMMU, ICLTH, IMISC
      INTEGER ISPND
      CHARACTER*1 Q
      Q = CHAR(39)
      IF (ICASH .LE. 0) THEN
        PRINT *,'YOU DON',Q,'T HAVE ANY MONEY TO SPEND.'
        RETURN
      END IF
      PRINT *,'ENTER WHAT YOU WISH TO SPEND ON THE FOLLOWING:'
      PRINT *,'FOOD'
      ISPND = INPUT(0, ICASH)
      ICASH = ICASH - ISPND
      IFOOD = IFOOD + INT(2. / 3. * ISPND)
      PRINT *,'AMMUNITION'
      ISPND = INPUT(0, ICASH)
      ICASH = ICASH - ISPND
      IAMMU = IAMMU + INT(2. / 3. * ISPND * 50)
      PRINT *,'CLOTHING'
      ISPND = INPUT(0, ICASH)
      ICASH = ICASH - ISPND
      ICLTH = ICLTH + INT(2. / 3. * ISPND)
      PRINT *,'MISCELLANEOUS SUPPLIES'
      ISPND = INPUT(0, ICASH)
      ICASH = ICASH - ISPND
      IMISC = IMISC + INT(2. / 3. * ISPND)
      END
C     ******************************************************************
      SUBROUTINE HUNT(IAMMU, ILEVL, IFOOD)
C
C     HUNTING POOR WILDLIFE.
C
      INTEGER SHOOT
      REAL    RAND
      INTEGER IAMMU, ILEVL, IFOOD
      INTEGER IBANGT
      CHARACTER*1 Q
      Q = CHAR(39)
      IF (IAMMU .LT. 39) THEN
        PRINT *,'TOUGH -- YOU NEED MORE BULLETS TO GO HUNTING.'
        RETURN
      END IF
      IBANGT = SHOOT(ILEVL)
      IF (IBANGT .LE. 1) THEN
        PRINT *,'RIGHT BETWEEN THE EYES -- YOU GOT A BIG ONE!!'
        PRINT *,'FULL BELLIES TONIGHT!'
        IFOOD = IFOOD + 52 + INT(RAND() * 6)
        IAMMU = IAMMU - 10 - INT(RAND() * 4)
      ELSE IF (100 * RAND() .LT. 13 * IBANGT) THEN
        PRINT *,'YOU MISSED -- AND YOUR DINNER GOT AWAY ...'
      ELSE
        PRINT *,'NICE SHOT -- RIGHT ON TARGET -- GOOD EATIN',Q,
     &          ' TONIGHT!!'
        IFOOD = IFOOD + 48 - 2 * IBANGT
        IAMMU = IAMMU - 10 - 3 * IBANGT
      END IF
      END
C     ******************************************************************
      SUBROUTINE INSTRU()
C
C     OUTPUTS THE GAME INSTRUCTIONS.
C
      CHARACTER*1 Q
      Q = CHAR(39)
  100 FORMAT (A)
      PRINT *,'THIS PROGRAM SIMULATES A TRIP OVER THE OREGON TRAIL FROM'
      PRINT *,'INDEPENDENCE, MISSOURI TO OREGON CITY, OREGON IN 1847.'
      PRINT *,'YOUR FAMILY OF FIVE WILL COVER THE 2040 MILE OREGON'
      PRINT *,'TRAIL IN 5-6 MONTHS -- IF YOU MAKE IT ALIVE.'
      PRINT *,' '
      PRINT *,'YOU HAD SAVED $900 TO SPEND FOR THE TRIP, AND YOU',Q,
     &        'VE JUST'
      PRINT *,'PAID $200 FOR A WAGON.'
      PRINT *,' '
      PRINT *,'YOU WILL NEED TO SPEND THE REST OF YOUR MONEY ON THE'
      PRINT *,'FOLLOWING ITEMS:'
      PRINT *,' '
      PRINT *,'PRESS [RETURN] KEY.'
      READ (*, 100)
      PRINT *,'  OXEN - YOU CAN SPEND $200-$300 ON YOUR TEAM. THE MORE'
      PRINT *,'         YOU SPEND, THE FASTER YOU',Q,'LL GO BECAUSE'
      PRINT *,'         YOU',Q,'LL HAVE BETTER ANIMALS.'
      PRINT *,' '
      PRINT *,'  FOOD - THE MORE YOU HAVE, THE LESS CHANCE THERE IS OF'
      PRINT *,'         GETTING SICK.'
      PRINT *,' '
      PRINT *,'  AMMUNITION - $1 BUYS A BELT OF 50 BULLETS. YOU WILL'
      PRINT *,'         NEED BULLETS FOR ATTACKS BY ANIMALS AND'
      PRINT *,'         BANDITS, AND FOR HUNTING FOOD.'
      PRINT *,' '
      PRINT *,'  CLOTHING - THIS IS ESPECIALLY IMPORTANT FOR THE COLD'
      PRINT *,'         WEATHER YOU WILL ENCOUNTER WHEN CROSSING THE'
      PRINT *,'         MOUNTAINS.'
      PRINT *,' '
      PRINT *,'  MISCELLANEOUS SUPPLIES - THIS INCLUDES MEDICINE AND'
      PRINT *,'         OTHER THINGS YOU WILL NEED FOR SICKNESS AND'
      PRINT *,'         EMERGENCY REPAIRS.'
      PRINT *,' '
      PRINT *,'PRESS [RETURN] KEY.'
      READ (*, 100)
      PRINT *,'YOU CAN SPEND ALL YOUR MONEY BEFORE YOU START YOUR'
      PRINT *,'TRIP -- OR YOU CAN SAVE SOME OF YOUR CASH TO SPEND AT'
      PRINT *,'FORTS ALONG THE WAY WHEN YOU RUN LOW. HOWEVER, ITEMS'
      PRINT *,'COST MORE AT THE FORTS. YOU CAN ALSO GO HUNTING ALONG'
      PRINT *,'THE WAY TO GET MORE FOOD.'
      PRINT *,' '
      PRINT *,'WHENEVER YOU HAVE TO USE YOUR TRUSTY RIFLE ALONG THE '
      PRINT *,'WAY, YOU WILL BE TOLD TO TYPE IN A WORD (ONE THAT SOUNDS'
      PRINT *,'LIKE A GUN SHOT). THE FASTER YOU TYPE IN THAT WORD AND'
      PRINT *,'THE [RETURN] KEY, THE BETTER LUCK YOU',Q,'LL HAVE WITH'
      PRINT *,'YOUR GUN.'
      PRINT *,' '
      PRINT *,'AT EACH TURN, ALL ITEMS ARE SHOWN IN DOLLAR AMOUNTS'
      PRINT *,'EXCEPT BULLETS.'
      PRINT *,' '
      PRINT *,'WHEN ASKED TO ENTER MONEY AMOUNTS, DON',Q,'T USE A $.'
      PRINT *,' '
      PRINT *,'GOOD LUCK!!'
      PRINT *,' '
      PRINT *,'PRESS [RETURN] KEY.'
      READ (*, 100)
      END
C     ******************************************************************
      SUBROUTINE PLAY()
C
C     INITIAL ROUTINE. GAME STARTS HERE.
C
      EXTERNAL ARRIVE, BLIZZ, DIE, DOCTOR, EAT, FORT, HUNT, RIDERS
      EXTERNAL SHOP, SICK
      INTEGER  INPUT, LEVEL, SHOOT
      REAL     RAND
      INTEGER  I, IBANGT, IEVENT(15), ISELEC
      REAL     R
      CHARACTER*17 DATSTR(20)
      CHARACTER*1  Q
      COMMON /EVNTS/ IEVENT
      COMMON /STATE/ IAMMU, IANIM, ICLTH, IFOOD, IMISC, IEVTC, ICTRN,
     &               ILEVL, IEATS, IF950, IFPAS, IFMOU, IFFRT, IFILL,
     &               IFINJ, ITMIL, ILMIL, ICASH
      COMMON /DATES/ DATSTR
      Q = CHAR(39)
      ILEVL = LEVEL()
      CALL SHOP(ICASH, IANIM, IFOOD, IAMMU, ICLTH, IMISC)
      DO 10 I = 1, 20
      ICASH = MAX(0, ICASH)
      IAMMU = MAX(0, IAMMU)
      IANIM = MAX(0, IANIM)
      ICLTH = MAX(0, ICLTH)
      IFOOD = MAX(0, IFOOD)
      IMISC = MAX(0, IMISC)
      IF (ITMIL .GE. 2040) THEN
        CALL ARRIVE(ITMIL, ILMIL, ICTRN, ICASH, IAMMU, ICLTH, IFOOD,
     &              IMISC,IEATS)
      END IF
      ICTRN = ICTRN + 1
      PRINT *,' '
      PRINT *,'MONDAY, ',DATSTR(I)
      PRINT *,'--------------------------------------------------------'
      ILMIL = ITMIL
      IF (IFILL .EQ. 1 .OR. IFINJ .EQ. 1) THEN
        CALL DOCTOR(ICASH, IFILL, IFINJ)
      END IF
      IF (IF950 .EQ. 1) THEN
        IF950 = 0
        PRINT *,'TOTAL MILEAGE IS ',950
      ELSE
        PRINT *,'TOTAL MILEAGE IS',ITMIL
      END IF
      IF (IFOOD .LE. 13) THEN
        PRINT *,'YOU',Q,'D BETTER DO SOME HUNTING OR BUY FOOD AND ',
     &          'SOON!!'
      END IF
      PRINT *,' '
      PRINT *,'FOOD BULLETS CLOTHING MISC. SUPP. CASH'
      PRINT 100, IFOOD, IAMMU, ICLTH, IMISC, ICASH
  100 FORMAT (I5, ' ', I7, ' ', I8, ' ', I11, ' ', I4)
      PRINT *,' '
      IF (IFFRT .EQ. -1) THEN
        PRINT *,'DO YOU WANT TO'
        PRINT *,'(1) STOP AT THE NEXT FORT (2) HUNT (3) CONTINUE'
        ISELEC = INPUT(1, 3)
        IF (ISELEC .EQ. 1) THEN
          CALL FORT(ICASH, IFOOD, IAMMU, ICLTH, IMISC)
          ITMIL = ITMIL - 45
        ELSE IF (ISELEC .EQ. 2) THEN
          CALL HUNT(IAMMU, ILEVL, IFOOD)
          ITMIL = ITMIL - 45
        END IF
      ELSE
        PRINT *,'DO YOU WANT TO'
        PRINT *,'(1) HUNT (2) CONTINUE'
        ISELEC = INPUT(1, 2)
        IF (ISELEC .EQ. 1) THEN
          CALL HUNT(IAMMU, ILEVL, IFOOD)
          ITMIL = ITMIL - 45
        END IF
      END IF
      IF (IFOOD .GE. 13) THEN
        CALL EAT(IFOOD, IEATS)
      ELSE
        PRINT *,'YOU RAN OUT OF FOOD AND STARVED TO DEATH.'
        CALL DIE()
      END IF
      ITMIL = ITMIL + 200 + INT((IANIM - 220) / 5 + 10 * RAND())
      R = ((ITMIL/100 - 4)**2 + 72) / ((ITMIL/100 - 4)**2 + 12) - 1
      IF (RAND() * 10 .LE. R) THEN
        CALL RIDERS(ILEVL, ITMIL, IANIM, IAMMU, IMISC, IFINJ)
      END IF
      IEVTC = 0
      R = 100 * RAND()
   20 IEVTC = IEVTC + 1
      IF (IEVTC .LT. 16 .AND. R .GT. IEVENT(IEVTC)) GOTO 20
      IF (IEVTC .EQ. 1) THEN
        PRINT *,'WAGON BREAKS DOWN -- LOSE TIME AND SUPPLIES FIXING IT.'
        ITMIL = ITMIL - 15 - INT(5 * RAND())
        IMISC = IMISC - 8
      ELSE IF (IEVTC .EQ. 2) THEN
        PRINT *,'OX INJURES LEG -- SLOWS YOU DOWN REST OF TRIP.'
        ITMIL = ITMIL - 25
        IANIM = IANIM - 20
      ELSE IF (IEVTC .EQ. 3) THEN
        PRINT *,'BAD LUCK -- YOUR DAUGHTER BROKE HER ARM.'
        PRINT *,'YOU HAD TO STOP AND USE SUPPLIES TO MAKE A SLING.'
        ITMIL = ITMIL - 5 - INT(4 * RAND())
        IMISC = IMISC - 2 - INT(3 * RAND())
      ELSE IF (IEVTC .EQ. 4) THEN
        PRINT *,'OX WANDERS OFF -- SPEND TIME LOOKING FOR IT.'
        ITMIL = ITMIL - 17
      ELSE IF (IEVTC .EQ. 5) THEN
        PRINT *,'YOUR SON GETS LOST -- SPEND HALF THE DAY LOOKING FOR ',
     &          'HIM.'
        ITMIL = ITMIL - 10
      ELSE IF (IEVTC .EQ. 6) THEN
        PRINT *,'UNSAFE WATER -- LOSE TIME LOOKING FOR CLEAN SPRING.'
        ITMIL = ITMIL - INT(10 * RAND()) - 2
      ELSE IF (IEVTC .EQ. 7) THEN
        IF (ITMIL .GT. 950) THEN
          PRINT *,'COLD WEATHER -- BRRRRRRR!'
          IF (ICLTH .GT. 22 + 4 * RAND()) THEN
            PRINT *,'YOU HAVE ENOUGH CLOTHING TO KEEP YOU WARM.'
          ELSE
            PRINT *,'YOU DON',Q,'T HAVE ENOUGH CLOTHING TO KEEP YOU ',
     &              'WARM.'
            CALL SICK(IEATS, ITMIL, IMISC, IFILL, IFINJ)
          END IF
        ELSE
          PRINT *,'HEAVY RAINS -- TIME AND SUPPLIES LOST.'
          IFOOD = IFOOD - 10
          IAMMU = IAMMU - 500
          IMISC = IMISC - 15
          ITMIL = ITMIL - INT(10 * RAND()) - 5
        END IF
      ELSE IF (IEVTC .EQ. 8) THEN
        PRINT *,'BANDITS ATTACK.'
        IBANGT = SHOOT(ILEVL)
        IAMMU = IAMMU - 20 * IBANGT
        IF (IAMMU .LT. 0) THEN
          PRINT *,'YOU RAN OUT OF BULLETS -- THEY GET LOTS OF CASH.'
          ICASH = ICASH / 3
        END IF
        IF (IAMMU .LT. 0 .OR. IBANGT .GT. 1) THEN
          PRINT *,'YOU GOT SHOT IN THE LEG AND THEY TOOK ONE OF YOUR ',
     &            'OXEN.'
          IFINJ = 1
          PRINT *,'BETTER HAVE A DOC LOOK AT YOUR WOUND.'
          IMISC = IMISC - 5
          IANIM = IANIM - 20
        ELSE IF (IBANGT .LE. 1) THEN
          PRINT *,'QUICKEST DRAW OUTSIDE OF DODGE CITY!!'
          PRINT *,'YOU GOT ',Q,'EM!'
        END IF
      ELSE IF (IEVTC .EQ. 9) THEN
        PRINT *,'THERE WAS A FIRE IN YOUR WAGON -- FOOD AND SUPPLIES ',
     &          'DAMAGE!'
        IFOOD = IFOOD - 40
        IAMMU = IAMMU - 400
        IMISC = IMISC - INT(RAND() * 8) - 3
        ITMIL = ITMIL - 15
      ELSE IF (IEVTC .EQ. 10) THEN
        PRINT *,'LOSE YOUR WAY IN HEAVY FOG -- TIME IS LOST.'
        ITMIL = ITMIL - 10 - INT(5 * RAND())
      ELSE IF (IEVTC .EQ. 11) THEN
        PRINT *,'YOU KILLED A POISONOUS SNAKE AFTER IT BIT YOU.'
        IAMMU = IAMMU - 10
        IMISC = IMISC - 5
        IF (IMISC .LT. 0) THEN
          PRINT *,'YOU DIE OF SNAKEBITE SINCE YOU HAVE NO MEDICINE.'
          CALL DIE()
        END IF
      ELSE IF (IEVTC .EQ. 12) THEN
        PRINT *,'WAGON GETS SWAMPED FORDING RIVER -- LOSE FOOD AND ',
     &          'CLOTHES.'
        IFOOD = IFOOD - 30
        ICLTH = ICLTH - 20
        ITMIL = ITMIL - 20 - INT(20 * RAND())
      ELSE IF (IEVTC .EQ. 13) THEN
        PRINT *,'WILD ANIMALS ATTACK!!'
        IF (IAMMU .LE. 39) THEN
          PRINT *,'YOU WERE TOO LOW ON BULLETS -- THE WOLVES ',
     &            'OVERPOWERED YOU.'
          PRINT *,'YOU DIED OF INJURIES.'
          CALL DIE()
        END IF
        IBANGT = SHOOT(ILEVL)
        IF (IBANGT .GT. 2) THEN
          PRINT *,'SLOW ON THE DRAW -- THEY GOT AT YOUR FOOD AND ',
     &            'CLOTHES.'
        ELSE
          PRINT *,'NICE SHOOTIN',Q,' PARDNER -- THEY DIDN',Q,'T ',
     &            'GET MUCH.'
        END IF
        IAMMU = IAMMU - 20 * IBANGT
        ICLTH = ICLTH - IBANGT * 4
        IFOOD = IFOOD - IBANGT * 8
      ELSE IF (IEVTC .EQ. 14) THEN
        PRINT *,'HAIL STORM -- SUPPLIES DAMAGED.'
        ITMIL = ITMIL - 5 - INT(RAND() * 3)
        IAMMU = IAMMU - 200
        IMISC = IMISC - 4 - INT(RAND() * 3)
      ELSE IF (IEVTC .EQ. 15) THEN
        IF ((IEATS .EQ. 1) .OR.
     &      (IEATS .EQ. 2 .AND. RAND() .GT. 0.25) .OR.
     &      (IEATS .EQ. 3 .AND. RAND() .LT. 0.5)) THEN
          CALL SICK(IEATS, ITMIL, IMISC, IFILL, IFINJ)
        END IF
      ELSE IF (IEVTC .EQ. 16) THEN
        PRINT *,'HELPFUL INDIANS SHOW YOU WERE TO FIND MORE FOOD.'
        IFOOD = IFOOD + 14
      END IF
      IF (ITMIL .GT. 950) THEN
        R = 9 - ((ITMIL /100 - 15)**2 / ((ITMIL/100 - 15)**2 + 12))
        IF (RAND() * 10 .LE. R) THEN
          PRINT *,'RUGGED MOUNTAINS.'
          IF (RAND() .LE. 0.1) THEN
            PRINT *,'YOU GOT LOST -- LOSE VALUABLE TIME TRYING TO ',
     &              'FIND TRAIL!'
            ITMIL = ITMIL - 60
          ELSE IF (RAND() .LE. 0.11) THEN
            PRINT *,'WAGON DAMAGED -- LOSE TIME AND SUPPLIES.'
            IMISC = IMISC - 5
            IAMMU = IAMMU - 200
            ITMIL = ITMIL - 20 - INT(30 * RAND())
          ELSE
            PRINT *,'THE GOING GETS SLOW.'
            ITMIL = ITMIL - 45 - INT(RAND() / 0.02)
          END IF
        END IF
        IF (IFPAS .NE. 1) THEN
          IFPAS = 1
          IF950 = 1
          IF (RAND() .GE. 0.8) THEN
            PRINT *,'YOU MADE IT SAFELY THROUGH SOUTH PASS -- NO SNOW.'
          ELSE
            CALL BLIZZ(ITMIL, IAMMU, ICLTH, IFOOD, IMISC, IFILL, IFINJ,
     &                 IEATS)
          END IF
        END IF
      END IF
      IF (ITMIL .GE. 1700 .AND. IFMOU .NE. 1) THEN
        IFMOU = 1
        IF (RAND() .GE. 0.7) THEN
          CALL BLIZZ(ITMIL, IAMMU, ICLTH, IFOOD, IMISC, IFILL, IFINJ,
     &               IEATS)
        END IF
      END IF
      IFFRT = -1 * IFFRT
   10 CONTINUE
      PRINT *,'YOU HAVE BEEN ON THE TRAIL TOO LONG ...'
      PRINT *,'YOUR FAMILY DIES IN THE FIRST BLIZZARD OF WINTER.'
      CALL DIE()
      END
C     ******************************************************************
      SUBROUTINE RIDERS(ILEVL, ITMIL, IANIM, IAMMU, IMISC, IFINJ)
C
C     RIDERS ATTACK (OR NOT).
C
      INTEGER     INPUT, SHOOT
      REAL        RAND
      INTEGER     ILEVL, ITMIL, IANIM, IAMMU, IMISC, IFINJ
      CHARACTER*1 Q
      INTEGER     IBANGT, IHORF, ISELEC
      Q = CHAR(39)
      IHORF = 0
      IF (RAND() .LT. .8) THEN
        PRINT *,'RIDERS AHEAD. THEY LOOK HOSTILE.'
      ELSE
        PRINT *,'RIDERS AHEAD. THEY DON',Q,'T LOOK HOSTILE.'
        IHORF = 1
      END IF
      PRINT *,'TACTICS'
      PRINT *,'(1) RUN  (2) ATTACK  (3) CONTINUE  (4) CIRCLE WAGONS'
      IF (RAND() .LE. .2) IHORF = 1 - IHORF
      ISELEC = INPUT(1, 4)
      IF (ISELEC .EQ. 1) THEN
        IF (IHORF .EQ. 1) THEN
          ITMIL = ITMIL + 15
          IANIM = IANIM - 10
        ELSE
          ITMIL = ITMIL + 20
          IMISC = IMISC - 15
          IAMMU = IAMMU - 150
          IANIM = IANIM - 40
        END IF
      ELSE IF (ISELEC .EQ. 2 .OR. ISELEC .EQ. 4) THEN
        IBANGT = SHOOT(ILEVL)
        IF (ISELEC .EQ. 2) THEN
          IAMMU = IAMMU - IBANGT * 40 - 80
        ELSE
          IAMMU = IAMMU - IBANGT * 30 - 80
          ITMIL = ITMIL - 25
        END IF
        IF (IBANGT .LE. 1) THEN
          PRINT *,'NICE SHOOTING -- YOU DROVE THEM OFF.'
        ELSE IF (IBANGT .GT. 1 .AND. IBANGT .LE. 4) THEN
          PRINT *,'KINDA SLOW WITH YOUR COLT .45.'
        ELSE IF (IBANGT .GT. 5) THEN
          PRINT *,'LOUSY SHOT -- YOU GOT KNIFED.'
          PRINT *,'YOU HAVE TO SEE OL',Q,' DOC BLANCHARD.'
          IFINJ = 1
        END IF
      ELSE IF (ISELEC .EQ. 3) THEN
        IF (RAND() .GT. .8) THEN
          PRINT *,'THEY DID NOT ATTACK.'
          RETURN
        END IF
        IAMMU = IAMMU - 150
        IMISC = IMISC - 15
      END IF
      IF (IHORF .EQ. 0) THEN
        PRINT *,'RIDERS WERE HOSTILE -- CHECK FOR LOSSES.'
        IF (IAMMU .LT. 0) THEN
          PRINT *,'YOU RAN OUT OF BULLETS AND GOT MASSACRED BY ',
     &            'THE RIDERS.'
          CALL DIE()
        END IF
      ELSE
        PRINT *,'RIDERS WERE FRIENDLY, BUT CHECK FOR POSSIBLE LOSSES.'
      END IF
      END
C     ******************************************************************
      SUBROUTINE SHOP(ICASH, IANIM, IFOOD, IAMMU, ICLTH, IMISC)
C
C     SHOP VISIT IN MISSOURI. THE PLAYER HAS TO BUY AT LEAST OXEN FOR
C     $200 TO $300.
C
      INTEGER ICASH, IANIM, IFOOD, IAMMU, ICLTH, IMISC
      INTEGER INPUT
      PRINT *,'YOU HAVE ',ICASH,' DOLLARS LEFT.'
      PRINT *,'HOW MUCH DO YOU WANT TO SPEND ON YOUR OXEN TEAM?'
      IANIM = INPUT(200, 300)
      ICASH = ICASH - IANIM
      PRINT *,'YOU NOW HAVE ',ICASH,' DOLLARS LEFT.'
      PRINT *,'HOW MUCH DO YOU WANT TO SPEND ON FOOD?'
      IFOOD = INPUT(0, ICASH)
      ICASH = ICASH - IFOOD
      PRINT *,'YOU NOW HAVE ',ICASH,' DOLLARS LEFT.'
      PRINT *,'HOW MUCH DO YOU WANT TO SPEND ON AMMUNITION?'
      IAMMU = INPUT(0, ICASH)
      ICASH = ICASH - IAMMU
      IAMMU = IAMMU * 50
      PRINT *,'YOU NOW HAVE ',ICASH,' DOLLARS LEFT.'
      PRINT *,'HOW MUCH DO YOU WANT TO SPEND ON CLOTHING?'
      ICLTH = INPUT(0, ICASH)
      ICASH = ICASH - ICLTH
      PRINT *,'YOU NOW HAVE ',ICASH,' DOLLARS LEFT.'
      PRINT *,'HOW MUCH DO YOU WANT TO SPEND ON MISCELLANEOUS'
      PRINT *,'SUPPLIES?'
      IMISC = INPUT(0, ICASH)
      ICASH = ICASH - IMISC
      PRINT *,'AFTER ALL YOUR PURCHASES, YOU NOW HAVE ',ICASH,
     &        ' DOLLARS LEFT.'
      END
C     ******************************************************************
      SUBROUTINE SICK(IEATS, ITMIL, IMISC, IFILL, IFINJ)
C
C     ILLNESS EVENTS.
C
      REAL    RAND
      INTEGER IEATS, ITMIL, IMISC, IFILL, IFINJ
      IF (100 * RAND() .LT. 10 + 35 * (IEATS - 1)) THEN
        PRINT *,'MILD ILLNESS -- MEDICINE USED.'
        ITMIL = ITMIL - 5
        IMISC = IMISC - 2
      ELSE IF (100 * RAND() .LT. 100 - (40 / 4**(IEATS - 1))) THEN
        PRINT *,'BAD ILLNESS -- MEDICINE USED.'
        ITMIL = ITMIL - 5
        IMISC = IMISC - 5
      ELSE
        PRINT *,'SERIOUS ILLNESS -- YOU MUST STOP FOR MEDICAL ',
     &          'ATTENTION.'
        ITMIL = ITMIL - 10
        IFILL = 1
      END IF
      IF (IMISC .LT. 0) THEN
        PRINT *,'YOU RAN OUT OF MEDICAL SUPPLIES.'
        IF (IFINJ .EQ. 1) THEN
          PRINT *,'YOU DIED OF INJURIES.'
        ELSE
          PRINT *,'YOU DIED OF PNEUMONIA.'
        END IF
        CALL DIE()
      END IF
      END

C     ******************************************************************
      SUBROUTINE UPPER(STR)
C
C     CONVERTS A STRING TO UPPER CASE.
C
      INTEGER A, Z
      PARAMETER (A=97, Z=122)
      CHARACTER*(*) STR
      INTEGER       ICHR
      INTEGER       I
      DO 10 I = 1, LEN(STR)
        ICHR = ICHAR(STR(I:I))
        IF (ICHR .GE. A .AND. ICHR .LE. Z) STR(I:I) = CHAR(ICHR - 32)
   10 CONTINUE
      END
C     ******************************************************************
      BLOCK DATA
C
C     COMMON VARIABLES:
C
C     JSEED - PRNG SEED VALUE.
C     IFRST - FIRST RANDOM VALUE.
C     EVNTS - ARRAY OF EVENT PROBABILITIES.
C     DATES - ARRAY OF DATE STRINGS.
C     WDAYS - ARRAY OF WEEKDAY STRINGS.
C     IAMMU - AMOUNT SPENT ON AMMUNITION.
C     IANIM - AMOUNT SPENT ON ANIMALS.
C     ICLTH - AMOUNT SPENT ON CLOTHING.
C     IFOOD - AMOUNT SPENT ON FOOD.
C     IMISC - AMOUNT SPENT ON MISCELLANEAOUS SUPPLIES.
C     IEVTC - COUNTER IN GENERATING EVENTS.
C     ICTRN - TURN NUMBER FOR SETTING DATE.
C     ILEVL - CHOICE OF SHOOTING EXPERTISE LEVEL.
C     IEATS - CHOICE OF EATING.
C     IF950 - FLAG FOR CLEARING SOUTH PASS IN SETTING MILEAGE.
C     IFPAS - FLAG FOR CLEARING SOUTH PASS.
C     IFMOU - FLAG FOR CLEARING BLUE MOUNTAINS.
C     IFFRT - FLAG FOR FORT OPTION.
C     IFILL - FLAG FOR ILLNESS
C     IFINJ - FLAG FOR INJURY.
C     ITMIL - TOTAL MILEAGE WHOLE TRIP.
C     ILMIL - TOTAL MILEAGE UP THROUGH PREVIOUS TURN.
C     ICASH - CASH LEFT AFTER INITIAL PURCHASE.
C
      CHARACTER*17 DATSTR(20)
      CHARACTER*10 WKDSTR(7)
      INTEGER      IEVENT(15)
      COMMON /SEED/  JSEED, IFRST
      COMMON /EVNTS/ IEVENT
      COMMON /DATES/ DATSTR
      COMMON /WDAYS/ WKDSTR
      COMMON /STATE/ IAMMU, IANIM, ICLTH, IFOOD, IMISC, IEVTC, ICTRN,
     &               ILEVL, IEATS, IF950, IFPAS, IFMOU, IFFRT, IFILL,
     &               IFINJ, ITMIL, ILMIL, ICASH
      DATA JSEED, IFRST /123456789,0/
      DATA ICTRN, ITMIL /0,0/
      DATA IEVENT /6,11,13,15,17,22,32,35,37,42,44,54,64,69,95/
      DATA IFFRT, IFILL, IFINJ, IF950, IFPAS, IFMOU /-1,0,0,0,0,0/
      DATA ICASH  /700/
      DATA WKDSTR /'MONDAY,','TUESDAY,','WEDNESDAY,','THURSDAY,',
     &  'FRIDAY,','SATURDAY,','SUNDAY,'/
      DATA DATSTR /'MARCH 29 1847','APRIL 12 1847','APRIL 26 1847',
     &  'MAY 10 1847','MAY 24 1847','JUNE 7 1847','JUNE 21 1847',
     &  'JULY 5 1847','JULY 19 1847','AUGUST 2 1847','AUGUST 16 1847',
     &  'AUGUST 31 1847','SEPTEMBER 13 1847','SEPTEMBER 27 1847',
     &  'OCTOBER 11 1847','OCTOBER 25 1847','NOVEMBER 8 1847',
     &  'NOVEMBER 22 1847','DECEMBER 6 1847','DECEMBER 20 1847'/
      END
