  PROGRAM da_airmass_bias

  USE RAD_BIAS

! PURPOSE.
! --------
! To calculate spatially varying bias correction COEFFICIENTS
! for TOVS using model bias predictors.

! EXTERNALS.
! ----------
! REGRESS_ONE

! REFERENCE.
! ----------
! Harris and Kelly 1998.

  IMPLICIT NONE

  TYPE(BIAS) :: tovs

  REAL(KIND=LONG) :: vmean_dep(JPCHAN), vmean_abs(JPCHAN)
  REAL(KIND=LONG) :: vec_dep(JPCHAN), vec_abs(JPCHAN)
  REAL(KIND=LONG) :: vstd_dep(JPCHAN), vstd_abs(JPCHAN)
  REAL(KIND=LONG) :: vmn(JPCHAN), vstd(JPCHAN)

  REAL :: pred(JPNX)

  REAL(KIND=LONG) :: xbar(JPNY,JPNX), ybar(JPNY)
  REAL(KIND=LONG) :: xcov(JPNY,JPNX,JPNX), ycov(JPNY), xycov(JPNY,JPNX)

  REAL(KIND=LONG) :: coef(JPNY,JPNX), coef0(JPNY)
  REAL(KIND=LONG) :: reserr(JPNY), rescov(JPNY,JPNY)
  REAL(KIND=LONG) :: xvec(JPNY,JPNX,JPNX)
  REAL(KIND=LONG) :: xreser(JPCHAN), xcoef0(JPCHAN), xcoef(JPCHAN,JPNX)

  REAL(KIND=LONG) :: vmnbd(JPNY,6), vstdbd(JPNY,6)
  REAL(KIND=LONG) :: vmnbdl(JPNY,6,0:1), vstdbdl(JPNY,6,0:1)
  REAL(KIND=LONG) :: vmnb0(JPNY,6), vstdb0(JPNY,6)
  REAL(KIND=LONG) :: vmnbl0(JPNY,6,0:1), vstdbl0(JPNY,6,0:1)
  REAL(KIND=LONG) :: dvmnbd(JPNY,6), dvstdb(JPNY,6)

  REAL(KIND=LONG) :: vmnrl(JPCHAN,JPSCAN) = 0.0
  REAL(KIND=LONG) :: vmnrlb(JPCHAN,JPSCAN,JBAND) = 0.0

  REAL(KIND=LONG) :: SCORR(JPCHAN)

  INTEGER :: nsel(10)
  INTEGER :: nobs(JPCHAN)
  INTEGER :: nobsy(JPNY),lchan

  INTEGER :: nband(6), nbandl(6,0:1), n_band_ch(6,JPNY,0:1) = 0
  INTEGER :: nband0(JPNY,6), nbandd(JPNY,6)
  INTEGER :: nbandl0(JPNY,6,0:1), nbanddl(JPNY,6,0:1)

  LOGICAL :: LMASK

  LOGICAL :: lscan = .FALSE., global = .FALSE.

  INTEGER :: IR, ibin, I, iband, II, IV, ib, ierr
  INTEGER :: JS, J, JJ, JCOUNT, JBOX, JMINI, JSCAN, jv, IIV, JJV, sband
  INTEGER :: ssel(5,3)
  INTEGER :: lsel(5,3)

  REAL(KIND=LONG) :: xcorr(JPCHAN)
  REAL(KIND=LONG) :: coef_sat, coef_year, coef_month, coef_day, coef_time
  INTEGER :: CDATE = 1998010105                           ! Current Date

  INTEGER :: icount = 0
  INTEGER :: platform_id,satellite_id,sensor_id          ! Namelist
  INTEGER :: KSCAN = JPSCAN
  LOGICAL :: check_limb=.false., check_mask=.false.
  REAL    :: FAC = 3.0      ! Number of SD' for QC


  NAMELIST /INPUTS/ platform_id,satellite_id,sensor_id, &
                    global, lscan, kscan, check_limb, check_mask, &
                    FAC,CDATE

!------------------------------------------------------------------------------
!        1.   SETUP.
!             -----
print * ,' '
print * ,' start program da_airmass_bias '
print * ,' '

  READ(5,INPUTS,END=100)
  100 CONTINUE
  WRITE(6,INPUTS)

  IF (lscan) OPEN(112,FORM='UNFORMATTED') ! Scan Biases with lat bands
  OPEN(UNIT=10,FORM='UNFORMATTED')        ! Input data from SELECT
  OPEN(11,FORM='UNFORMATTED')             ! Output 'core' array file

! Read scan biases

  IF (lscan) THEN 
     WRITE (6,112) (JS,JS=1,KSCAN)
     112 FORMAT(/1X,'SCAN DEPENDENT BIASES APPLIED'/1X,3X,18I7/(4X,18I7))
  ELSE
     WRITE (6,*) 'NO SCAN CORRECTION APPLIED'
  ENDIF

!----------------------------------------------------------------------
!        2.   READ IN DATA, Q.C., CALC MEANS AND VARIANCES.
!             ---- -- ----- ----- ---- ----- --- ---------

  nobs(:)  = 0
  nsel(:)  = 0
  ssel(:,:) = 0
  lsel(:,:) = 0
  vmean_dep(:) = 0.0
  vmean_abs(:) = 0.0
  vstd_dep(:)  = 0.0
  vstd_abs(:)  = 0.0

  200  CONTINUE

  READ(UNIT=10,END=265)  tovs%nchan, tovs%npred    ! Read in data
  REWIND(UNIT=10)

  allocate(tovs%tb(tovs%nchan))
  allocate(tovs%omb(tovs%nchan))
  allocate(tovs%bias(tovs%nchan))
  allocate(tovs%qc_flag(tovs%nchan))
  allocate(tovs%cloud_flag(tovs%nchan))
  allocate(tovs%pred(tovs%npred))

  IF (lscan) THEN
    DO J=1, tovs%nchan   ! get scan biases (global)
        READ(112) JJ, vmnrl(J,1:KSCAN)
    ENDDO

    DO J=1, tovs%nchan   ! get scan biases (latitude bands)
    DO sband=1,JBAND
        READ(112) JJ, vmnrlb(J,1:KSCAN,sband)
    ENDDO
    ENDDO
  ENDIF

loop1:&
  DO
    icount=icount+1

    call da_read_biasprep(tovs,10,ierr)
    if (ierr == 0) then      ! not end
         continue
    elseif (ierr == 1) then  ! end
         exit
    else                     ! error
         stop 'read error in da_airmass_bias'
    endif

! select lat band and get scan bias
      iband = INT(tovs%lat/30.000001) + 3
      IF (lscan) THEN
        JSCAN = tovs%scanpos
       if (global) then
        CALL GET_SCORR(JPCHAN,SCORR(1:JPCHAN),tovs%lat,vmnrlb,JSCAN)
       else
        SCORR(1:tovs%nchan) = vmnrl(1:tovs%nchan,JSCAN)
       end if
      ELSE
        JSCAN = KSCAN/2
        SCORR(1:JPCHAN) = 0.0
      ENDIF

! apply scan bias to the departure (from FG or analysis) and the TB
      vec_dep(1:tovs%nchan) = tovs%omb(1:tovs%nchan) - SCORR(1:tovs%nchan)
      vec_abs(1:tovs%nchan) =  tovs%tb(1:tovs%nchan) - SCORR(1:tovs%nchan)

!
!   2.2 QC: extrme values/extrme departure/window channel
!
      if (tovs%sensor_id == 3) then
           call qc_amsua(tovs)
      elseif(tovs%sensor_id == 4) then
           call qc_amsub(tovs)
      endif

!-------------------------
! 2.3 limb sounding check
!-------------------------
  if (check_limb) then
    IF ((tovs%scanpos <= 2) .OR. (tovs%scanpos >= (KSCAN-1))) THEN  ! Reject edge of scan
      nsel(2) = nsel(2) + 1
      CYCLE loop1
    ENDIF
  end if

!-----------------------------------------
! 2.4 Reject data outside radiosonde mask
!-----------------------------------------
    IF (check_mask) THEN     
      CALL MASK(tovs%lat,tovs%lon,LMASK) 
      IF (.NOT. LMASK) THEN
        nsel(8) = nsel(8) + 1
        CYCLE loop1
      ENDIF
    ENDIF

! Good data : count and use in the statistics

    DO j=1, tovs%nchan
      IF ( tovs%qc_flag(j) == 1 ) THEN
!  statistics for channel
!--------------------------
        nobs(j) = nobs(j) + 1
        vmean_dep(j) = vmean_dep(j) + vec_dep(j)
         vstd_dep(j) = vstd_dep(j) + vec_dep(j)*vec_dep(j)
        vmean_abs(j) = vmean_abs(j) + vec_abs(j)
         vstd_abs(j) = vstd_abs(j) + vec_abs(j)*vec_abs(j)
      ENDIF
    ENDDO

  ENDDO loop1

  265 CONTINUE

!---------------------------------
!  2.8 mean and std for channels
!---------------------------------
  WHERE (nobs(:) /= 0)
    vmean_dep(:) = vmean_dep(:)/nobs(:)
    vstd_dep(:)  = vstd_dep(:)/nobs(:) - vmean_dep(:)**2
    vstd_dep(:)  = SQRT(MAX(0.0,vstd_dep(:)))
    vmean_abs(:) = vmean_abs(:)/nobs(:)
    vstd_abs(:)  = vstd_abs(:)/nobs(:) - vmean_abs(:)**2
    vstd_abs(:)  = SQRT(MAX(0.0,vstd_abs(:)))
  END WHERE

  WRITE (6,270) icount,nobs(1:tovs%nchan)
  270 FORMAT (/1X,'NUMBER OF DATA Total/ACCEPTED'/1X,i10/1X,15I10)

  WRITE (6,288)
  288 FORMAT (/1X,'FIRST PASS: MEANS AND STANDARD DEVIATIONS')

  DO j=1, tovs%nchan
    jv = j
    WRITE (6,289) jv, nobs(j), vmean_abs(j), vstd_abs(j), vmean_dep(j), vstd_dep(j)
    289 FORMAT (1X,I5,I10,4F15.2)
  ENDDO 

!-----------------------------------------------------------------------------
!     3.   SECOND PASS THROUGH DATA, EXTRA Q.C. (sigma-elimination)
!          ------ ---- ------- ----- ----- ----

  300 CONTINUE

  vmn(:) = vmean_dep(:)
  vstd(:)= vstd_dep(:)

  REWIND(UNIT=10)

  vmean_dep(:) = 0.0                                    ! Clear matrices
  vmean_abs(:) = 0.0                                    ! Clear matrices
  nobs(:) = 0
  nsel(:) = 0
  ssel(:,:) = 0
  lsel(:,:) = 0
  nband(:) = 0
  nbandl(:,:) = 0
  nobsy(:) = 0

  xbar(:,:)   = 0.0
  ybar(:)     = 0.0
  xcov(:,:,:) = 0.0
  ycov(:)     = 0.0
  xycov(:,:)  = 0.0

  icount = 0
loop2:&
  DO
    icount = icount + 1

    call da_read_biasprep(tovs,10,ierr)
    if (ierr == 0) then      ! not end
         continue
    elseif (ierr == 1) then  ! end
         exit
    else                     ! error
         stop 'read error in da_airmass_bias'
    endif

! latitude band
    iband = INT(tovs%lat/30.000001) + 3
    IF (lscan) THEN
        JSCAN = tovs%scanpos
        CALL GET_SCORR(JPCHAN,SCORR(1:JPCHAN),tovs%lat,vmnrlb,JSCAN)
    ELSE
        JSCAN = KSCAN/2
        SCORR(1:JPCHAN) = 0.0
    ENDIF

    vec_dep(1:JPCHAN) = tovs%omb(1:JPCHAN) - SCORR(1:JPCHAN)
    vec_abs(1:JPCHAN) = tovs%tb(1:JPCHAN) - SCORR(1:JPCHAN)

! 3.2 QC:
      if (tovs%sensor_id == 3) then
           call qc_amsua(tovs)
      elseif(tovs%sensor_id == 4) then
           call qc_amsub(tovs)
      endif

! 3.3 limb scan check
!---------------------
   if (check_limb) then
    IF ((tovs%scanpos <= 2) .OR. (tovs%scanpos >= (KSCAN-1))) THEN
      nsel(2) = nsel(2) + 1
      CYCLE loop2
    ENDIF
   end if

! 3.4 Reject data outside radiosonde mask
!------------------------------------------
    IF (check_mask) THEN
      CALL MASK(tovs%lat,tovs%lon,LMASK)
      IF (.NOT. LMASK) THEN
        nsel(8) = nsel(8) + 1
        CYCLE loop2
      ENDIF
    ENDIF

! 3.5 Reject outliers : facx*sigma, sigma calculated in first pass : loop1
!--------------------------------------------------------------------------
    DO j=1, tovs%nchan
      IF ( (ABS(vec_dep(j)-vmn(j)) > (vstd(j)*FAC)) ) THEN
        tovs%qc_flag(j) = -1
      ENDIF
    ENDDO

! Good data : count and use in the statistics

    nbandl(iband,tovs%landmask) = nbandl(iband,tovs%landmask) + 1
    nbandl(    6,tovs%landmask) = nbandl(    6,tovs%landmask) + 1

    nband(iband) = nband(iband) + 1
    nband(    6) = nband(    6) + 1

! mean/std statistics for relative scan-bias corrected values
    DO j=1, tovs%nchan           
      IF ( tovs%qc_flag(j) == 1 ) THEN
        jv = j
        n_band_ch(iband,j,tovs%landmask) = n_band_ch(iband,J,tovs%landmask) + 1
        n_band_ch(    6,j,tovs%landmask) = n_band_ch(    6,J,tovs%landmask) + 1
        nobs(j) = nobs(j) + 1
        vmean_dep(j) = vmean_dep(j) + vec_dep(j)
         vstd_dep(j) = vstd_dep(j) + vec_dep(j)*vec_dep(j)
        vmean_abs(j) = vmean_abs(j) + vec_abs(j)
         vstd_abs(j) = vstd_abs(j) + vec_abs(j)*vec_abs(j)
      ENDIF
    ENDDO

      PRED(1:tovs%npred) = tovs%pred(1:tovs%npred)

! compute regression variables mean/var/cov: y:departure; x:predictors
    DO j=1, tovs%nchan
     IF ( tovs%qc_flag(j) == 1 ) THEN 
        jv = j

        ybar(j) = ybar(j) + vec_dep(j)             ! mean of y
        ycov(j) = ycov(j) + vec_dep(j)*vec_dep(j)  ! variance of y

        DO i=1, tovs%npred                         ! Covariances for regression
           xbar(j,i) = xbar(j,i) + pred(i)         ! mean of x
          xycov(j,i) = xycov(j,i) + vec_dep(j)*pred(i) ! cov of x and y
          DO ii=1, tovs%npred
            xcov(j,i,ii) = xcov(j,i,ii) + pred(i)*pred(ii) ! cov of x and x
          ENDDO
        ENDDO

     ENDIF
    ENDDO

  ENDDO loop2

  365 CONTINUE

! Output arrays to bias 'core' file.

  WRITE(11) nobs(:)
  WRITE(11) vmean_dep(:)
  WRITE(11) vstd_dep(:)
  WRITE(11) vmean_abs(:)
  WRITE(11) vstd_abs(:)

  WRITE(11) nbandl(:,:)
  WRITE(11) nband(:)
  WRITE(11) n_band_ch(:,:,:)

  WRITE(11) ybar(:)
  WRITE(11) ycov(:)
  WRITE(11) xbar(:,:)
  WRITE(11) xycov(:,:)
  WRITE(11) xcov(:,:,:)

! Calculate means, standard deviations and covariances

  WHERE (nobs(:) /= 0)
    vmean_dep(:) = vmean_dep(:)/nobs(:)
    vstd_dep(:)  = vstd_dep(:)/nobs(:) - vmean_dep(:)**2
    vstd_dep(:)  = SQRT(MAX(0.0,vstd_dep(:)))
    vmean_abs(:) = vmean_abs(:)/nobs(:)
    vstd_abs(:)  = vstd_abs(:)/nobs(:) - vmean_abs(:)**2
    vstd_abs(:)  = SQRT(MAX(0.0,vstd_abs(:)))
  ENDWHERE

  DO j=1, tovs%nchan
    IF (nobs(j) /= 0) THEN
      ybar(j) = ybar(j)/nobs(j)
      ycov(j) = ycov(j)/nobs(j) - ybar(j)*ybar(j)
      DO i=1, tovs%npred
         xbar(j,i) = xbar(j,i)/nobs(j)
        xycov(j,i) = xycov(j,i)/nobs(j) - xbar(j,i)*ybar(j)
      ENDDO
      DO i=1, tovs%npred
        xcov(j,i,1:tovs%npred) = xcov(j,i,1:tovs%npred)/nobs(j) - xbar(j,i)*xbar(j,1:tovs%npred)
      ENDDO
    ENDIF
  ENDDO

  WRITE (6,270) icount,nobs(1:tovs%nchan)

  WRITE (6,388)
  388 FORMAT (/1X,'SECOND PASS: MEANS AND STANDARD DEVIATIONS')

  DO j=1, tovs%nchan
     jv = j
    WRITE (6,289) jv, nobs(j), vmean_abs(j), vstd_abs(j), vmean_dep(j), vstd_dep(j)
  ENDDO

  PRINT *, ' '
  PRINT *, 'PREDICTOR MEANS AND STANDARD DEVIATIONS'
  DO j=1, tovs%nchan
     jv = j
    PRINT *, ' '
    PRINT *, 'CHANNEL ', jv, ' NOBS = ', nobs(j)
    DO i=1, tovs%npred
      WRITE (6,390) i, xbar(j,i), SQRT(xcov(j,i,i))
    ENDDO
  ENDDO
  390 FORMAT (1X,I5,4F15.2)

  WRITE(6,389) '(land)', (ib,ib=1,6)
  DO J=1, tovs%nchan
    jv = j
    IF (MAXVAL(n_band_ch(1:6,j,0)) > 0) WRITE(6,392) jv, n_band_ch(1:6,j,0)
  ENDDO

  WRITE(6,389) '(sea)', (ib,ib=1,6)
  DO J=1, tovs%nchan
    jv = j
    IF (MAXVAL(n_band_ch(1:6,j,1)) > 0) WRITE(6,392) jv, n_band_ch(1:6,j,1)
  ENDDO

  WRITE(6,391)  nband(1:6)

  389 FORMAT(//1X,'Data distribution by latitude band ',A//,10X,6(I5,3X)/)
  392 FORMAT(1X,I4,5X,6(I5,3X))
  391 FORMAT(/1X,'Total',I15,5I18)

!----------------------------------------------------------------------------
!       4.   CALCULATE REGRESSION COEFFICIENTS.
!            --------- ---------- ------------

    DO j=1, tovs%nchan
       jv = j
       PRINT *, 'REGRESSION : CHANNEL ', jv
       CALL REGRESS_ONE(tovs%npred,xbar(j,1:tovs%npred),ybar(j), &
                        xcov(j,1:tovs%npred,1:tovs%npred), &
                        ycov(j),xycov(j,1:tovs%npred), &
                        tovs%npred,coef(j,1:tovs%npred),coef0(j),reserr(j),&
                        rescov(j,j),xvec(j,1:tovs%npred,1:tovs%npred))
    ENDDO

    PRINT *, 'PREDICTOR EIGENVECTORS'
    DO j=1, tovs%nchan
       jv = j
      PRINT *, ' '
      PRINT *,  'CHANNEL ', jv
      DO i=1, tovs%npred
        PRINT 888, xvec(j,1:tovs%npred,i)
      ENDDO
    ENDDO
    888 FORMAT(1X,6F12.4)

    xcoef0 = 0.0
    xreser = 0.0
    xcoef = 0.0

    DO JJ=1,tovs%nchan
      J = JJ
      xcoef0(J) = coef0(JJ)
      xreser(J) = reserr(JJ)
      DO I=1, tovs%npred
        xcoef(J,I) = coef(JJ,I)
      ENDDO
    ENDDO

!        5.   OUTPUT RESULTS.
!             ------ -------

    coef_year  = REAL(CDATE/1000000)
    coef_month = REAL(MOD(CDATE,1000000)/10000)
    coef_day   = REAL(MOD(CDATE,10000)/100)                  ! Set date
    coef_time  = REAL(MOD(CDATE,100))

    WRITE (6,*)
    WRITE (6,502) platform_id,satellite_id,sensor_id, &
                  coef_year, coef_month, coef_day,coef_time
    502  FORMAT (/1X, 'SAT',3I7,'   YEAR',F5.0,'   MONTH',F5.0,'   DAY',F5.0,' TIME',F6.0,//&
                  1X,3X,' CH    MEAN  STDDEV  RES.SD  COEFFICIENTS')

    DO I=1, tovs%nchan
      WRITE (6,505) I,vmean_dep(I),vstd_dep(I),xreser(I),(xcoef(I,J),J=1,tovs%npred),xcoef0(I)
      505 FORMAT (1X,I3,3F8.2,8F12.5)
    ENDDO

    WRITE (6,508) (I,I=1,tovs%nchan)
    508 FORMAT (/1X,'RESIDUAL ERROR COVARIANCE'/1X,21I6)
 
    DO J=1, tovs%nchan
      WRITE (6,510) (rescov(I,J),I=1,tovs%nchan)
    ENDDO
    510 FORMAT (1X,21F6.2)

    WRITE (6,511) (I,I=1,tovs%nchan)
    511 FORMAT (/1X,'RESIDUAL ERROR CORRELATION'/1X,21I6)

    DO J=1, tovs%nchan
      DO I=1, tovs%nchan
        rescov(I,J) = rescov(I,J)/(reserr(I)*reserr(J))
      ENDDO
      WRITE (6,510) (rescov(I,J),I=1,tovs%nchan)
    ENDDO

! 6.0 output of coefficients of calculated regression
!   -------------------------------------------------------------
      WRITE(111) platform_id,satellite_id,sensor_id, &
                coef_year, coef_month, coef_day, coef_time, &
                tovs%nchan, tovs%npred, KSCAN, JBAND, global
      DO I=1, tovs%nchan
        WRITE(111) I, vmean_dep(I), vstd_dep(I), xreser(I),  &
                    (xcoef(I,J),J=1,tovs%npred), xcoef0(I)
      ENDDO

      DO J=1,tovs%nchan     ! write scan biases (global)
        WRITE(111) J, VMNRL(J,1:KSCAN)
      ENDDO

      DO J=1,tovs%nchan     ! write scan biases (band)
      DO SBAND=1,JBAND
        WRITE(111) J, VMNRLB(J,1:KSCAN,SBAND)
      ENDDO
      ENDDO

!----------------------------------------------------------------------------

   deallocate(tovs%tb)
   deallocate(tovs%omb)
   deallocate(tovs%qc_flag)
   deallocate(tovs%cloud_flag)
   deallocate(tovs%pred)

  CLOSE(UNIT=10)
  CLOSE(UNIT=11)
  CLOSE(UNIT=111)
  IF (lscan) CLOSE(UNIT=112)

  print * ,' end of program da_airmass_bias'

  END PROGRAM da_airmass_bias
