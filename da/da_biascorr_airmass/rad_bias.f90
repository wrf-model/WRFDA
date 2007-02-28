  MODULE rad_bias

! PRINT_BIAS, MASK

  IMPLICIT NONE

!  INTEGER, PARAMETER :: LONG = SELECTED_REAL_KIND(9,99)
  INTEGER, PARAMETER :: LONG = SELECTED_REAL_KIND(15,37)

  INTEGER, PARAMETER :: JPNX=8
  INTEGER, PARAMETER :: JPXEIG=10

  INTEGER, PARAMETER :: JPNY=20
  INTEGER, PARAMETER :: JPCHAN=20
  INTEGER, PARAMETER :: JPSCAN=90

  INTEGER, PARAMETER :: JBAND=18
  INTEGER, PARAMETER :: BOFF = JBAND/2 + 1
  REAL,    PARAMETER :: BDIV = 180.0/JBAND + 0.0001
  INTEGER, PARAMETER :: AVBAND=2

  REAL(KIND=LONG), PARAMETER :: PI = 3.141592654
  REAL(KIND=LONG), PARAMETER :: VMAX = 350.0, VMIN =  50.0, VDMAX = 20.0
  CHARACTER(LEN=10), PARAMETER :: labsel(10) = &
                                    (/' WRONG SAT','  PATH 2+3','      LAND', &
                                      '  ROGUE TB',' ROGUE DTB','BAD Win CH', &
                                      '  OUTLIERS','    MASKED','          ', &
                                      '      GOOD'/)

   TYPE BIAS
    INTEGER :: nchan     ! number of channels
    INTEGER :: npred     ! number of predictors
    INTEGER :: platform_id,satellite_id,sensor_id
    INTEGER :: year, month, day, hour, min, sec
    INTEGER :: scanline,scanpos
    INTEGER :: landmask
    INTEGER :: surf_flag
    INTEGER, pointer :: qc_flag(:) ! 1/0:good/bad
    INTEGER, pointer :: cloud_flag(:) ! 1/0:no-cloud/cloud
    REAL    :: elevation,lat,lon,ps, t2m, q2m, tsk, clwp
    REAL, pointer  :: tb(:), omb(:), bias(:)
    REAL, pointer  :: pred(:)
   END TYPE BIAS

  CONTAINS

    SUBROUTINE PRINT_BIAS(rads)

    IMPLICIT NONE

    TYPE(BIAS), INTENT(IN) :: rads

    INTEGER :: lev

    WRITE(6,*) ' '

    WRITE(6,'(a20,3i4)') 'Instrument Triplet :', rads%platform_id, &
                           rads%satellite_id,rads%sensor_id
    WRITE(6,'(a20,6i5)') 'DATE :', rads%year, &
       rads%month,rads%day,rads%hour,rads%min,rads%sec

    WRITE(6,'(a20,2f10.3)') 'Latitude Longitude :', rads%lat,rads%lon

    WRITE(6,'(a20,2i5)') 'Scan Line/Position :', rads%scanline,rads%scanpos
    WRITE(6,'(a20,i3,f10.2)') 'Land/Sea Elevation :', rads%landmask, rads%elevation

    WRITE(6,'(a20,4f10.2)') 'Ps Ts T2m Q2m :', rads%ps, rads%tsk, rads%t2m, rads%q2m

    WRITE(6,'(a20,i5)') 'Number of Channels :', rads%nchan
    DO lev=1, rads%nchan
      WRITE(6,'(10x,i3,2f10.2)') lev, rads%tb(lev), rads%omb(lev)
    ENDDO

    WRITE(6,'(a60)') ' predictors : 1000-300/200-50mb thickness T_skin TPW'
    WRITE(6,'(4f10.2)')  rads%pred(1:rads%npred)

    RETURN
    END SUBROUTINE PRINT_BIAS

  SUBROUTINE MASK(PLAT,PLONG,LMASK)

! ROUTINE TO ACCESS RADIOSONDE MASK:
! RETURNS LMASK=.TRUE. IF PLAT,PLONG CLOSE TO RADIOSONDE
! LOCATION.  DISTANCE DEFINED IN CREATION OF MASK.

  IMPLICIT NONE

  REAL,    INTENT(INOUT)  :: PLAT, PLONG
  LOGICAL, INTENT(OUT) :: LMASK

  INTEGER :: IMASK(360,181) = 0
  INTEGER :: IERR, ICL, ILEN
! INTEGER :: CRAYOPEN, CRAYREAD, CRAYCLOSE
  INTEGER :: IUMASK = 39, LOOP = 0 

  INTEGER, SAVE :: INIT = 0 
  INTEGER :: IX, IY

  CHARACTER(LEN=1) :: C(600000)

  IF (INIT == 0) THEN

    INIT = 1
    open(169,FILE='mask_asc')
    print * ,' open radiosonde mask file mask_asc '
    read(169,99)imask
    99 FORMAT (1X,90I1)
    close(169)

  ENDIF

  LOOP = LOOP + 1
  IF (PLONG < 0.0) PLONG = PLONG + 360.0
  IX = MOD(NINT(PLONG),360) + 1
  IY = NINT(90.0-PLAT) + 1
  IF (IMASK(IX,IY) == 1) THEN
    LMASK = .TRUE.
  ELSE
    LMASK = .FALSE.
  ENDIF

! IF (LOOP.LE.100) WRITE(6,*) LOOP,PLAT,PLONG,IX,IY,IMASK(IX,IY),LMASK

  RETURN
  END SUBROUTINE MASK

    LOGICAL FUNCTION USE_CHAN(j,land,path,ICLR,ICLD,LNDCH)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: j, land, path
    INTEGER, INTENT(IN) :: ICLR(JPNY), ICLD(JPNY), LNDCH(JPNY)

    INTEGER :: Lmask, Cmask

    SELECT CASE(land)
    CASE(0)            ! over land
      Lmask = LNDCH(J) ! 0:not use; 1: use
    CASE(1)            ! over sea
      Lmask = 1
    CASE DEFAULT
      Lmask = 0
    END SELECT

    SELECT CASE(path)
    CASE(0)            ! clear sky
      Cmask = ICLR(J)  ! 0:not use; 1: use
    CASE(1)            ! cloudy sky
      Cmask = ICLD(J)  ! 0:not use; 1: use
    CASE DEFAULT
      Cmask = 0
    END SELECT

    IF (Lmask*Cmask == 0) THEN
      USE_CHAN = .FALSE.
    ELSE
      USE_CHAN = .TRUE.
    ENDIF

    RETURN
    END FUNCTION USE_CHAN

    SUBROUTINE GET_SCORR(JPCHAN,SCORR,LAT,vmnrlb,JSCAN)

    IMPLICIT NONE

!   Assume JBAND, JPCHAN, JPSCAN, BDIV and BOFF are inherited from main program.
!   Will need to be explicitly defined for 1DVAR, presumably #include in future.

    INTEGER,         INTENT(IN)  :: JPCHAN
    REAL(KIND=LONG), INTENT(OUT) :: SCORR(JPCHAN)
    REAL,            INTENT(INOUT)  :: LAT
    REAL(KIND=LONG), INTENT(INOUT)  :: vmnrlb(JPCHAN,JPSCAN,JBAND)
    INTEGER,         INTENT(IN)  :: JSCAN

    INTEGER :: sband,i
    REAL    :: BLAT

    sband = FLOOR(LAT/BDIV) + BOFF
    BLAT = FLOOR(LAT/BDIV)*BDIV

    IF (LAT >= BLAT+BDIV/2) THEN

      IF (sband < JBAND) THEN
        SCORR(1:JPCHAN) = (LAT -   (BLAT+BDIV/2)) * vmnrlb(1:JPCHAN,JSCAN,sband+1) / BDIV &
                       + ((BLAT+3*BDIV/2) - LAT) * vmnrlb(1:JPCHAN,JSCAN,sband  ) / BDIV
      ELSE
        SCORR(1:JPCHAN) = vmnrlb(1:JPCHAN,JSCAN,sband)
      ENDIF

    ELSEIF (LAT < BLAT+BDIV/2) THEN

      IF (sband > 1) THEN
        SCORR(1:JPCHAN) = (LAT - (BLAT-BDIV/2)) * vmnrlb(1:JPCHAN,JSCAN,sband  ) / BDIV &
                       + ((BLAT+BDIV/2) - LAT) * vmnrlb(1:JPCHAN,JSCAN,sband-1) / BDIV
      ELSE
        SCORR(1:JPCHAN) = vmnrlb(1:JPCHAN,JSCAN,sband)
      ENDIF

    ENDIF

    END SUBROUTINE GET_SCORR

    SUBROUTINE QC_AMSUA(tovs)

    TYPE(bias), intent(inout) :: tovs

    integer  :: j, jv

    tovs%qc_flag(:) = 1
!--------------------------------------------------------
! 2.1 extrem values
!--------------------------------------------------------
    DO j=1, tovs%nchan    ! Reject silly values
        jv = j
        IF ((tovs%tb(jv) > VMAX) .OR. (tovs%tb(jv) < VMIN) ) then
          tovs%qc_flag(jv) = -1
        ENDIF
    ENDDO

!------------------------------------
! 2.2 departure extrem values test
!------------------------------------
    DO j=1, tovs%nchan
        jv = j
        IF (ABS(tovs%omb(jv)) > VDMAX) THEN
           tovs%qc_flag(jv) = -1
        ENDIF
    ENDDO

!------------------------------------
! 2.3 window channel
!------------------------------------
        IF (tovs%surf_flag > 0) THEN   ! not over sea
           tovs%qc_flag(1:3) = -1
           tovs%qc_flag(15)  = -1
        ENDIF

    END SUBROUTINE QC_AMSUA

    SUBROUTINE QC_AMSUB(tovs)

    TYPE(bias), intent(inout) :: tovs
    integer :: j, jv
 
    tovs%qc_flag(:) = 1
!--------------------------------------------------------
! 2.1 extrem values
!--------------------------------------------------------
    DO j=1, tovs%nchan    ! Reject silly values
        jv = j
        IF ((tovs%tb(jv) > VMAX) .OR. (tovs%tb(jv) < VMIN) ) then
          tovs%qc_flag(jv) = -1
        ENDIF
    ENDDO

!------------------------------------
! 2.2 departure extrem values test
!------------------------------------
    DO j=1, tovs%nchan
        jv = j
        IF (ABS(tovs%omb(jv)) > VDMAX) THEN
           tovs%qc_flag(jv) = -1
        ENDIF
    ENDDO

!------------------------------------
! 2.3 window channel
!------------------------------------
        IF (tovs%surf_flag > 0) THEN   ! not over sea
           tovs%qc_flag(1:2) = -1
        ENDIF

    END SUBROUTINE QC_AMSUB

    subroutine da_read_biasprep(radbias,biasprep_unit,ierr)
      TYPE(bias), INTENT(INOUT)  :: radbias
      integer, intent(in)        :: biasprep_unit
      integer, intent(out)       :: ierr
      read(UNIT=biasprep_unit,END=990,ERR=995) radbias%nchan,radbias%npred
      read(UNIT=biasprep_unit,END=990,ERR=995) radbias%platform_id , &
                                radbias%satellite_id, &
                                radbias%sensor_id,    &
                                radbias%year,radbias%month,&
                                radbias%day, radbias%hour, &
                                radbias%min, radbias%sec,  &
                                radbias%scanline, &
                                radbias%scanpos,  &
                                radbias%landmask, &
                                radbias%elevation,&
                                radbias%lat,radbias%lon, &
                                radbias%ps,radbias%t2m, &
                                radbias%q2m,radbias%tsk, &
                                radbias%tb(1:radbias%nchan), &
                                radbias%omb(1:radbias%nchan), &
                                radbias%bias(1:radbias%nchan), &
                                radbias%pred(1:radbias%npred), &
                                radbias%qc_flag(1:radbias%nchan), &
                                radbias%cloud_flag(1:radbias%nchan), &
                                radbias%surf_flag, radbias%clwp

    ierr = 0
    RETURN

    990 CONTINUE
    ierr = 1
    RETURN

    995 CONTINUE
    ierr = 2
    RETURN

    end subroutine da_read_biasprep

    subroutine da_write_biasprep(radbias,biasprep_unit)
      TYPE(bias), INTENT(IN)  :: radbias
      integer,    INTENT(IN)  :: biasprep_unit

      write(UNIT=biasprep_unit) radbias%nchan,radbias%npred
      write(UNIT=biasprep_unit) radbias%platform_id , &
                                radbias%satellite_id, &
                                radbias%sensor_id,    &
                                radbias%year,radbias%month,&
                                radbias%day, radbias%hour, &
                                radbias%min, radbias%sec,  &
                                radbias%scanline, &
                                radbias%scanpos,  &
                                radbias%landmask, &
                                radbias%elevation,&
                                radbias%lat,radbias%lon, &
                                radbias%ps,radbias%t2m, &
                                radbias%q2m,radbias%tsk, &
                                radbias%tb(1:radbias%nchan), &
                                radbias%omb(1:radbias%nchan), &
                                radbias%bias(1:radbias%nchan), &
                                radbias%pred(1:radbias%npred), &
                                radbias%qc_flag(1:radbias%nchan), &
                                radbias%cloud_flag(1:radbias%nchan), &
                                radbias%surf_flag, radbias%clwp
    end subroutine da_write_biasprep

  END MODULE rad_bias
