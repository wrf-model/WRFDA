
   program geth_newdate 

      IMPLICIT NONE

      !  From old date ('YYYY-MM-DD HH:MM:SS.ffff') and
      !  delta-time, compute the new date.

      !  on entry     -  odate  -  the old hdate.
      !                  idt    -  the change in time

      !  on exit      -  ndate  -  the new hdate.

      INTEGER            :: idt
      CHARACTER (LEN=13)  :: ndate
      CHARACTER (LEN=13)  :: odate


      !  Local Variables
      INTEGER :: nlen, olen
      INTEGER :: yrnew, monew, dynew, hrnew, minew, scnew, frnew
      INTEGER :: yrold, moold, dyold, hrold, miold, scold, frold
      INTEGER :: mday(12), nday, nhour, nmin, nsec, nfrac, i, ifrc
      LOGICAL :: opass
      CHARACTER (LEN=10) :: hfrc
      CHARACTER (LEN=1) :: sp
      INTEGER, EXTERNAL :: nfeb  ! in the same module now
      integer, external :: iargc
      logical :: lmore
      character(len=120) :: harg
      integer :: numarg

      ! get args from command

      numarg = iargc()

      i = 1

      do while ( i <= numarg)
        call getarg(i, harg)
        if(i == 1) odate = harg
        if(i == 2) read(harg,'(i5.5)') idt
        i = i + 1
      enddo

      !  Assign the number of days in a months

      mday( 1) = 31
      mday( 2) = 28
      mday( 3) = 31
      mday( 4) = 30
      mday( 5) = 31
      mday( 6) = 30
      mday( 7) = 31
      mday( 8) = 31
      mday( 9) = 30
      mday(10) = 31
      mday(11) = 30
      mday(12) = 31

      !  Break down old hdate into parts

      hrold = 0
      miold = 0
      scold = 0
      frold = 0
      olen = LEN(odate)
      IF (olen.GE.11) THEN
         sp = odate(11:11)
      else
         sp = ' '
      END IF

      !  Use internal READ statements to convert the CHARACTER string
      !  date into INTEGER components.

      READ(odate(1:4),  '(I4)') yrold
      READ(odate(6:7),  '(I2)') moold
      READ(odate(9:10), '(I2)') dyold
      READ(odate(12:13),'(I2)') hrold

      !  Set the number of days in February for that year.

      mday(2) = nfeb(yrold)

      !  Check that ODATE makes sense.

      opass = .TRUE.

      !  Check that the month of ODATE makes sense.

      IF ((moold.GT.12).or.(moold.LT.1)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Month of ODATE = ', moold
         opass = .FALSE.
      END IF

      !  Check that the day of ODATE makes sense.

      IF ((dyold.GT.mday(moold)).or.(dyold.LT.1)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Day of ODATE = ', dyold
         opass = .FALSE.
      END IF

      !  Check that the hour of ODATE makes sense.

      IF ((hrold.GT.23).or.(hrold.LT.0)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Hour of ODATE = ', hrold
         opass = .FALSE.
      END IF

      !  Check that the minute of ODATE makes sense.

      IF ((miold.GT.59).or.(miold.LT.0)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Minute of ODATE = ', miold
         opass = .FALSE.
      END IF

      !  Check that the second of ODATE makes sense.

      IF ((scold.GT.59).or.(scold.LT.0)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold
         opass = .FALSE.
      END IF

      !  Check that the fractional part  of ODATE makes sense.


      IF (.not.opass) THEN
         WRITE(*,*) 'GETH_NEWDATE: Crazy ODATE: ', odate(1:olen), olen
         STOP 'odate_3'
      END IF

      !  Date Checks are completed.  Continue.


      !  Compute the number of days, hours, minutes, and seconds in idt

      IF (olen.eq.13) THEN !idt should be in hours
         ifrc = 1
         nday   = ABS(idt)/24 ! Integer number of days in delta-time
         nhour  = MOD(ABS(idt),24)
         nmin   = 0
         nsec   = 0
         nfrac  = 0
      ELSE IF (olen.eq.10) THEN !idt should be in days
         ifrc = 1
         nday   = ABS(idt)/24 ! Integer number of days in delta-time
         nhour  = 0
         nmin   = 0
         nsec   = 0
         nfrac  = 0
      ELSE
         WRITE(*,'(''GETH_NEWDATE: Strange length for ODATE: '', i3)') &
              olen
         WRITE(*,*) odate(1:olen)
         STOP 'odate_4'
      END IF

      IF (idt.GE.0) THEN

         frnew = frold + nfrac
         IF (frnew.GE.ifrc) THEN
            frnew = frnew - ifrc
            nsec = nsec + 1
         END IF

         scnew = scold + nsec
         IF (scnew .GE. 60) THEN
            scnew = scnew - 60
            nmin  = nmin + 1
         END IF

         minew = miold + nmin
         IF (minew .GE. 60) THEN
            minew = minew - 60
            nhour  = nhour + 1
         END IF

         hrnew = hrold + nhour
         IF (hrnew .GE. 24) THEN
            hrnew = hrnew - 24
            nday  = nday + 1
         END IF

         dynew = dyold
         monew = moold
         yrnew = yrold
         DO i = 1, nday
            dynew = dynew + 1
            IF (dynew.GT.mday(monew)) THEN
               dynew = dynew - mday(monew)
               monew = monew + 1
               IF (monew .GT. 12) THEN
                  monew = 1
                  yrnew = yrnew + 1
                  ! If the year changes, recompute the number of days in February
                  mday(2) = nfeb(yrnew)
               END IF
            END IF
         END DO

      ELSE IF (idt.LT.0) THEN

         frnew = frold - nfrac
         IF (frnew .LT. 0) THEN
            frnew = frnew + ifrc
            nsec = nsec - 1
         END IF

         scnew = scold - nsec
         IF (scnew .LT. 00) THEN
            scnew = scnew + 60
            nmin  = nmin + 1
         END IF
         minew = miold - nmin
         IF (minew .LT. 00) THEN
            minew = minew + 60
            nhour  = nhour + 1
         END IF

         hrnew = hrold - nhour
         IF (hrnew .LT. 00) THEN
            hrnew = hrnew + 24
            nday  = nday + 1
         END IF

         dynew = dyold
         monew = moold
         yrnew = yrold
         DO i = 1, nday
            dynew = dynew - 1
            IF (dynew.eq.0) THEN
               monew = monew - 1
               IF (monew.eq.0) THEN
                  monew = 12
                  yrnew = yrnew - 1
                  ! If the year changes, recompute the number of days in February
                  mday(2) = nfeb(yrnew)
               END IF
               dynew = mday(monew)
            END IF
         END DO
      END IF

      !  Now construct the new mdate

      nlen = LEN(ndate)

      IF (nlen.eq.13) THEN
         WRITE(ndate,13) yrnew, monew, dynew, hrnew
      13   format(I4,'-',I2.2,'-',I2.2,'_',I2.2)

      ELSE IF (nlen.eq.10) THEN
         WRITE(ndate,10) yrnew, monew, dynew
      10   format(I4,'-',I2.2,'-',I2.2)

      END IF

      IF (olen.GE.11) ndate(11:11) = sp

      write (*,*) ndate

   END 

   FUNCTION nfeb ( year ) RESULT (num_days)

      ! Compute the number of days in February for the given year

      IMPLICIT NONE

      INTEGER :: year
      INTEGER :: num_days

      num_days = 28 ! By default, February has 28 days ...
      IF (MOD(year,4).eq.0) THEN
         num_days = 29  ! But every four years, it has 29 days ...
         IF (MOD(year,100).eq.0) THEN
            num_days = 28  ! Except every 100 years, when it has 28 days ...
            IF (MOD(year,400).eq.0) THEN
               num_days = 29  ! Except every 400 years, when it has 29 days.
            END IF
         END IF
      END IF

   END FUNCTION nfeb

