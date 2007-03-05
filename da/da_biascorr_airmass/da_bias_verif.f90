  PROGRAM da_bias_verif

  USE RAD_BIAS

! PURPOSE.
! --------
! Apply bias correction to radiance data
! to statistically verify algorithm

  IMPLICIT NONE

  TYPE(BIAS) :: tovs

  REAL(KIND=LONG) :: vmean_dep(JPCHAN), vmean_abs(JPCHAN)
  REAL(KIND=LONG) :: vec_dep(JPCHAN), vec_abs(JPCHAN)
  REAL(KIND=LONG) :: vstd_dep(JPCHAN), vstd_abs(JPCHAN)
  REAL(KIND=LONG) :: vmean_dep1(JPCHAN), vmean_abs1(JPCHAN)
  REAL(KIND=LONG) :: vstd_dep1(JPCHAN), vstd_abs1(JPCHAN)

  REAL(KIND=LONG) :: vmn(JPCHAN), vstd(JPCHAN), airbias(JPCHAN)

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
  INTEGER :: nobs(JPCHAN),nobs1(JPCHAN)
  INTEGER :: nobsy(JPNY),lchan

  LOGICAL :: LMASK

  LOGICAL :: lscan = .FALSE.

  INTEGER :: IR, ibin, I, iband, II, IV, ib, ierr
  INTEGER :: JS, J, JJ, JCOUNT, JBOX, JMINI, JSCAN, jv, IIV, JJV, sband

  REAL(KIND=LONG) :: xcorr(JPCHAN)
  REAL(KIND=LONG) :: coef_year, coef_month, coef_day, coef_time

  INTEGER :: kscanx, jbandx
  LOGICAL :: check_limb=.false., check_mask=.false., global
  REAL    :: FAC = 3.0      ! Number of SD' for QC

  INTEGER :: nchan,nscan,nband,npred

  NAMELIST /INPUTS/ global,lscan, check_limb, check_mask, FAC

!------------------------------------------------------------------------------
!        1.   SETUP.
!             -----
  READ(5,INPUTS,END=100)
  100 CONTINUE
  WRITE(6,INPUTS)

! 1.0 read bias correction coefficients
!   -------------------------------------------------------------
     open(UNIT=12,file='scor.asc',form='formatted')
     read (12,'(4i6)') nchan,nscan,nband,npred
     close(12)

     call read_biascoef(nchan,nscan,nband,npred,global, &
                      VMNRL(1:nchan,1:nscan),VMNRLB(1:nchan,1:nscan,1:nband), &
                      xcoef(1:nchan,1:npred),xcoef0(1:nchan), &
                      nobs(1:nchan),vmean_abs(1:nchan), &
                      vstd_abs(1:nchan),vmean_dep(1:nchan), &
                      vstd_dep(1:nchan) )

!----------------------------------------------------------------------
!        2.   READ IN DATA, Q.C., CALC MEANS AND VARIANCES.
!             ---- -- ----- ----- ---- ----- --- ---------

  nobs1(:)  = 0
  nsel(:)  = 0
  vmean_dep1(:) = 0.0
  vmean_abs1(:) = 0.0
  vstd_dep1(:)  = 0.0
  vstd_abs1(:)  = 0.0

  200  CONTINUE

  READ(UNIT=10,END=300)  tovs%nchan, tovs%npred    ! Read in data
  REWIND(UNIT=10)

  allocate(tovs%tb(tovs%nchan))
  allocate(tovs%omb(tovs%nchan))
  allocate(tovs%bias(tovs%nchan))
  allocate(tovs%qc_flag(tovs%nchan))
  allocate(tovs%cloud_flag(tovs%nchan))
  allocate(tovs%pred(tovs%npred))

  300 CONTINUE

loop2:&
  DO
    call da_read_biasprep(tovs,10,ierr)
    if (ierr == 0) then      ! not end
         continue
    elseif (ierr == 1) then  ! end
         exit
    else                     ! error
         stop 'read error in da_veri_bias'
    endif

! latitude band
    iband = INT(tovs%lat/30.000001) + 3
    IF (lscan) THEN
        JSCAN = tovs%scanpos
      if (global) then
        CALL GET_SCORR(JPCHAN,SCORR(1:JPCHAN),tovs%lat,vmnrlb,JSCAN)
      else
        SCORR(1:nchan) = vmnrl(1:nchan,JSCAN)
      end if
    ELSE
        JSCAN = KSCANX/2
        SCORR(1:JPCHAN) = 0.0
    ENDIF

    vec_dep(1:nchan) = tovs%omb(1:nchan) - SCORR(1:nchan)
    vec_abs(1:nchan) =  tovs%tb(1:nchan) - SCORR(1:nchan)

! 3.2 QC:
      if (tovs%sensor_id == 3) then
           call qc_amsua(tovs)
      elseif(tovs%sensor_id == 4) then
           call qc_amsub(tovs)
      endif

! 3.3 limb scan check
!---------------------
   if (check_limb) then
    IF ((tovs%scanpos <= 2) .OR. (tovs%scanpos >= (KSCANX-1))) THEN
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

! 3.5 Reject outliers : facx*sigma, 
!--------------------------------------------------------------------------
    DO j=1, tovs%nchan
      IF ( (ABS(vec_dep(j)-vmean_dep(j)) > (vstd_dep(j)*FAC)) ) THEN
        tovs%qc_flag(j) = -1
      ENDIF
    ENDDO

    do i=1,nchan 
      airbias(i) = xcoef0(i)
      do j=1,npred
        airbias(i) = airbias(i) + tovs%pred(j)*xcoef(i,j)
      end do
      vec_dep(i) = vec_dep(i) - airbias(i)
      vec_abs(i) = vec_abs(i) - airbias(i)
      if ( tovs%omb(i) == -888888.00 ) vec_dep(i)=tovs%omb(i)
    end do

! mean/std statistics for scan/airmass-bias corrected values                      
    DO j=1, nchan
      IF ( tovs%qc_flag(j) == 1 ) THEN
        jv = j
        nobs1(j) = nobs1(j) + 1
        vmean_dep1(j) = vmean_dep1(j) + vec_dep(j)
         vstd_dep1(j) = vstd_dep1(j) + vec_dep(j)*vec_dep(j)
        vmean_abs1(j) = vmean_abs1(j) + vec_abs(j)
         vstd_abs1(j) = vstd_abs1(j) + vec_abs(j)*vec_abs(j)
      ENDIF
    ENDDO

    if (nchan == 15) then
      write(11,'(15i15)') tovs%qc_flag(1:nchan)
      write(11,'(15f15.3)') tovs%omb(1:nchan)  ! omb no-biascorrection
      write(11,'(15f15.3)') vec_dep(1:nchan)   ! omb with bias correction
    else if(nchan==5) then
      write(11,'(5i15)') tovs%qc_flag(1:nchan)
      write(11,'(5f15.3)') tovs%omb(1:nchan)  ! omb no-biascorrection
      write(11,'(5f15.3)') vec_dep(1:nchan)   ! omb with bias correction
    else if(nchan==19) then
      write(11,'(19i15)') tovs%qc_flag(1:nchan)
      write(11,'(19f15.3)') tovs%omb(1:nchan)  ! omb no-biascorrection
      write(11,'(19f15.3)') vec_dep(1:nchan)   ! omb with bias correction
    else
      write(*,*) 'Unknow sensor'
    end if

  ENDDO loop2

  365 CONTINUE

! Calculate means, standard deviations and covariances

  WHERE (nobs1(:) /= 0)
    vmean_dep1(:) = vmean_dep1(:)/nobs1(:)
    vstd_dep1(:)  = vstd_dep1(:)/nobs1(:) - vmean_dep1(:)**2
    vstd_dep1(:)  = SQRT(MAX(0.0,vstd_dep1(:)))
    vmean_abs1(:) = vmean_abs1(:)/nobs1(:)
    vstd_abs1(:)  = vstd_abs1(:)/nobs1(:) - vmean_abs1(:)**2
    vstd_abs1(:)  = SQRT(MAX(0.0,vstd_abs1(:)))
  ENDWHERE

!----------------------------------------------------------------------------

   deallocate(tovs%tb)
   deallocate(tovs%omb)
   deallocate(tovs%qc_flag)
   deallocate(tovs%cloud_flag)
   deallocate(tovs%pred)

! out coefs to ASCII file bcor.asc
  call write_biascoef(nchan,nscan,nband,npred,global, &
                      VMNRL(1:nchan,1:nscan),VMNRLB(1:nchan,1:nscan,1:nband), &
                      xcoef(1:nchan,1:npred),xcoef0(1:nchan), &
                      nobs1(1:nchan),vmean_abs1(1:nchan), &
                      vstd_abs1(1:nchan),vmean_dep1(1:nchan), &
                      vstd_dep1(1:nchan) )

  END PROGRAM da_bias_verif
