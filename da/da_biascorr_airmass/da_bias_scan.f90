  PROGRAM da_bias_scan

  USE RAD_BIAS

! PURPOSE.
! -------
! TO CALCULATE SCAN BIAS CORRECTIONS.

! REFERENCE.
! ----------
! The ECMWF Tovs BIAs Correction, Harris and Kelly..

  IMPLICIT NONE

! Local Variables

  TYPE(BIAS) :: tovs

  INTEGER :: nobs(JPCHAN)
  INTEGER :: nscan(JPSCAN), nscanch(JPCHAN,JPSCAN)
  INTEGER :: nscanchb(JPCHAN,JPSCAN,JBAND)

  REAL(KIND=LONG) :: vmean_dep(JPCHAN), vmean_abs(JPCHAN)
  REAL(KIND=LONG) :: vstd_dep(JPCHAN), vstd_abs(JPCHAN)
  REAL(KIND=LONG) :: vec_dep(JPCHAN), vec_abs(JPCHAN)

  REAL(KIND=LONG) :: vmn(JPCHAN), vstd(JPCHAN)
  REAL(KIND=LONG) :: vmnsc(JPCHAN,JPSCAN), vstdsc(JPCHAN,JPSCAN), vmnrl(JPCHAN,JPSCAN)
 
  REAL(KIND=LONG) :: vmnscb(JPCHAN,JPSCAN,JBAND), vstdscb(JPCHAN,JPSCAN,JBAND)
  REAL(KIND=LONG) :: vmnrlb(JPCHAN,JPSCAN,JBAND)

! smoothed values
  INTEGER :: nscanchbt(JPCHAN,JPSCAN,JBAND)
  REAL(KIND=LONG) :: vmnscbt(JPCHAN,JPSCAN,JBAND), vstdscbt(JPCHAN,JPSCAN,JBAND)
  REAL(KIND=LONG) :: vmnrlbt(JPCHAN,JPSCAN,JBAND)

  INTEGER :: ibin, ierr, iscanm
  INTEGER :: j, jv, jscan, js, k , l
  INTEGER :: path, cloud_flag

  INTEGER :: iband, ia, iab, aoff
  INTEGER :: avb 

  CHARACTER(LEN=2) :: cchan

  REAL(KIND=LONG) :: vmn0, vmn0b(JBAND), VDHL_WINDOW, VDLL_WINDOW

  LOGICAL :: global, smoothing

! definition of defaut namelist values
!-----------------------------------------
  INTEGER :: sband =1
  INTEGER :: eband =18
  INTEGER :: kscan =90
  INTEGER :: ICOUNT = 0                              ! connter                                
  REAL(KIND=LONG)   :: fac = 3.0                    ! Sdev factor
  NAMELIST /INPUTS/ kscan,fac,global,sband,eband,smoothing

!------------------------------------------------------------------------------
!       1.   SETUP.
!            -----

  READ(5,INPUTS,END=100)

  100 CONTINUE

  WRITE(6,INPUTS)

  OPEN(UNIT=10,FORM='UNFORMATTED')       ! Open input file  : Input data from select
  OPEN(UNIT=11,FORM='UNFORMATTED')       ! Open output file : Scan mean core arrays
  OPEN(UNIT=12,FORM='UNFORMATTED')       ! Open output file : Scan bias coef


!------------------------------------------------------------------------------
!       2.   READ IN DATA, Q.C., CALC MEANS AND VARIANCES.
!            ---- -- ----- ----- ---- ----- --- ---------

  vmean_dep = 0.0                                          ! Clear matrices
  vstd_dep  = 0.0
  vmean_abs = 0.0                                          ! Clear matrices
  vstd_abs  = 0.0
  nobs      = 0

  READ(UNIT=10,END=265)  tovs%nchan, tovs%npred    ! Read in data
  REWIND(UNIT=10)

  allocate(tovs%tb(tovs%nchan))
  allocate(tovs%omb(tovs%nchan))
  allocate(tovs%bias(tovs%nchan))
  allocate(tovs%qc_flag(tovs%nchan))
  allocate(tovs%cloud_flag(tovs%nchan))
  allocate(tovs%pred(tovs%npred))

loop1:&
  DO
      icount=icount+1

!
!  2.1 read in data
!
      call da_read_biasprep(tovs,10,ierr)
      if (ierr == 0) then      ! not end
          continue
      elseif (ierr == 1) then  ! end
          exit
      else                     ! error
          stop 'read error in da_scan_bias'
      endif

      if (icount < 2) call print_bias(tovs)

      vec_dep(1:tovs%nchan) = tovs%omb(1:tovs%nchan) ! obs minus guess
      vec_abs(1:tovs%nchan) = tovs%tb(1:tovs%nchan)  ! obs

! 
!   2.2 QC: extrme values/extrme departure/window channel 
!
      if (tovs%sensor_id == 3) then
           call qc_amsua(tovs)
      elseif(tovs%sensor_id == 4) then
           call qc_amsub(tovs)
      endif

    DO j=1,tovs%nchan 
!-------------------------------
!  2.3 compute the statistics
!-------------------------------    
      IF ( tovs%qc_flag(j) == 1 ) THEN
        nobs(j) = nobs(j) + 1
        vmean_dep(j) = vmean_dep(j) + vec_dep(j)
        vstd_dep(j) = vstd_dep(j) + vec_dep(j)*vec_dep(j)
        vmean_abs(j) = vmean_abs(j) + vec_abs(j)
        vstd_abs(j) = vstd_abs(j) + vec_abs(j)*vec_abs(j)
      ENDIF
    ENDDO

  ENDDO loop1

  265 CONTINUE            ! MEANS AND STANDARD DEVIATIONS

  WRITE (6,270) icount,nobs(1:tovs%nchan)
  270 FORMAT (/1X,'NUMBER OF DATA Total/ACCEPTED'/1X,i10/1X,15I10)

!---------------------------------
!  2.8 mean and std for channels
!---------------------------------
  WHERE (nobs(:) /= 0)
    vmean_dep(:) = vmean_dep(:)/nobs(:)
     vstd_dep(:) = vstd_dep(:)/nobs(:) - vmean_dep(:)**2
     vstd_dep(:) = SQRT(MAX(0.0,vstd_dep(:)))
    vmean_abs(:) = vmean_abs(:)/nobs(:)
     vstd_abs(:) = vstd_abs(:)/nobs(:) - vmean_abs(:)**2
     vstd_abs(:) = SQRT(MAX(0.0,vstd_abs(:)))
  END WHERE

  vmn(:) = vmean_dep(:)   ! used by outlier check later
  vstd(:) = vstd_dep(:)

  WRITE (6,288)
  288 FORMAT (/1X,'FIRST PASS: MEANS AND STANDARD DEVIATIONS')

  DO j=1,tovs%nchan
    jv = j
    WRITE (6,289) jv, nobs(j), vmean_abs(j),vstd_abs(j), vmean_dep(j),vstd_dep(j)
  ENDDO
  289 FORMAT (1X,I5,I10,4F15.2)

!-------------------------------------------------------------------------------
!     3.   SECOND PASS THROUGH DATA, EXTRA Q.C.
!          ------ ---- ------- ----- ----- ----

  300 CONTINUE
  
  REWIND(UNIT=10)

  vmean_dep = 0.0                                          ! Clear matrices
  vstd_dep = 0.0
  vmean_abs = 0.0                                          ! Clear matrices
  vstd_abs = 0.0
  vmnsc = 0.0
  vstdsc = 0.0
  vmnscb = 0.0
  vstdscb = 0.0
  nobs = 0
  nscan = 0
  nscanch = 0
  nscanchb = 0

loop2:&
  DO

    call da_read_biasprep(tovs,10,ierr)
    if (ierr == 0) then      ! not end
         continue
    elseif (ierr == 1) then  ! end
         exit
    else                     ! error
         stop 'read error in da_scan_bias'
    endif

      vec_dep(1:tovs%nchan) = tovs%omb(1:tovs%nchan)
      vec_abs(1:tovs%nchan) = tovs%tb(1:tovs%nchan)

      if (tovs%sensor_id == 3) then
           call qc_amsua(tovs)
      elseif(tovs%sensor_id == 4) then
           call qc_amsub(tovs)
      endif

!---------------------------------------------------------------------------------
! 3.6 Reject outliers : using fac and stdv calculated in the first pass : loop 1
!---------------------------------------------------------------------------------
    DO j=1, tovs%nchan
      IF ( (ABS(vec_dep(j)-vmn(j)) > (vstd(j)*FAC)) ) THEN
          tovs%qc_flag(j) = -1
      ENDIF
    ENDDO

! Good data : count and use in the ststistics
!----------------------------------------------------

    jscan = tovs%scanpos
    nscan(jscan) = nscan(jscan) + 1

    iband = FLOOR(tovs%lat/BDIV) + BOFF   !! latitude band

    DO j=1, tovs%nchan
      IF ( tovs%qc_flag(j) == 1 ) THEN

!  statistics for channel
!--------------------------
        nobs(j) = nobs(j) + 1
        vmean_dep(j) = vmean_dep(j) + vec_dep(j)
         vstd_dep(j) = vstd_dep(j) + vec_dep(j)*vec_dep(j)
        vmean_abs(j) = vmean_abs(j) + vec_abs(j)
         vstd_abs(j) = vstd_abs(j) + vec_abs(j)*vec_abs(j)
        
!  statistics for channel+scan position
!----------------------------------------
        nscanch(j,jscan) = nscanch(j,jscan) + 1
          vmnsc(j,jscan) = vmnsc(j,jscan) + vec_dep(j)
         vstdsc(j,jscan) = vstdsc(j,jscan) + vec_dep(j)*vec_dep(j)

!  statistics for channel + scan position + lat band
!---------------------------------------------------------- 
        nscanchb(j,jscan,iband) =  nscanchb(j,jscan,iband) + 1
          vmnscb(j,jscan,iband) = vmnscb(j,jscan,iband) + vec_dep(j)
         vstdscb(j,jscan,iband) = vstdscb(j,jscan,iband) + vec_dep(j)*vec_dep(j)

      ENDIF
    ENDDO

  ENDDO loop2

  990 CONTINUE

! Write scan 'core' arrays NOTE: not divided by nobs
!------------------------------------------------------
  WRITE(11) nobs(:)         ! statistics for channel
  WRITE(11) vmean_dep(:)
  WRITE(11) vstd_dep(:)
  WRITE(11) vmean_abs(:)
  WRITE(11) vstd_abs(:)

  WRITE(11) nscanch(:,:)    ! statistics for channel + scan position
  WRITE(11) vmnsc(:,:)
  WRITE(11) vstdsc(:,:)

  WRITE(11) nscanchb(:,:,:) ! statistics for channel + scan position + lat band
  WRITE(11) vmnscb(:,:,:)
  WRITE(11) vstdscb(:,:,:)

! Write out copious amounts of output
!----------------------------------------
  WRITE (6,270) icount,nobs(1:tovs%nchan)

  WHERE (nobs(:) /= 0)     ! for channels
    vmean_dep(:) = vmean_dep(:)/nobs(:)
    vstd_dep(:) = vstd_dep(:)/nobs(:) - vmean_dep(:)**2
    vstd_dep(:) = SQRT(MAX(0.0,vstd_dep(:)))
    vmean_abs(:) = vmean_abs(:)/nobs(:)
    vstd_abs(:) = vstd_abs(:)/nobs(:) - vmean_abs(:)**2
    vstd_abs(:) = SQRT(MAX(0.0,vstd_abs(:)))
  END WHERE

  DO j=1, tovs%nchan
    WHERE (nscanch(j,:) /= 0)  ! for channels + scan
      vmnsc(j,:) = vmnsc(j,:)/nscanch(j,:)
      vstdsc(j,:) = vstdsc(j,:)/nscanch(j,:) - vmnsc(j,:)**2
      vstdsc(j,:) = SQRT(MAX(0.0,vstdsc(j,:)))
    END WHERE
    WHERE (nscanchb(j,:,:) /= 0) ! for channels + scan + band
      vmnscb(j,:,:) = vmnscb(j,:,:)/nscanchb(j,:,:)
      vstdscb(j,:,:) = vstdscb(j,:,:)/nscanchb(j,:,:) - vmnscb(j,:,:)**2
      vstdscb(j,:,:) = SQRT(MAX(0.0,vstdscb(j,:,:)))
    END WHERE

!  4.1 compute central scan position mean
!--------------------------------------------
    IF (MOD(KSCAN,2) == 0 ) THEN   ! even scan number
      iscanm = KSCAN/2     !! middle scan position

      IF( nscanch(j,iscanm) .ne. 0 .and. nscanch(j,iscanm+1) .ne.0 )then  
         vmn0 = 0.5*(vmnsc(j,iscanm)+vmnsc(j,iscanm+1))
         vmn0b(:) = 0.5*(vmnscb(j,iscanm,:)+vmnscb(j,iscanm+1,:))
      ENDIF

      IF( nscanch(j,iscanm) .eq. 0 .and. nscanch(j,iscanm+1) .ne.0 )then  
         vmn0 = vmnsc(j,iscanm+1)
         vmn0b(:) = vmnscb(j,iscanm+1,:)
      ENDIF

      IF( nscanch(j,iscanm) .ne. 0 .and. nscanch(j,iscanm+1) .eq.0 )then  
         vmn0 = vmnsc(j,iscanm)
         vmn0b(:) = vmnscb(j,iscanm,:)
      ENDIF

    ELSE
      iscanm = kscan/2 + 1
      vmn0 = vmnsc(j,iscanm)
      vmn0b(:) = vmnscb(j,iscanm,:)
    ENDIF

!  4.2 compute relative bias
!------------------------------------
   DO k=1,KSCAN
   IF( nscanch(j,k) .ne. 0 )then
    vmnrl(j,k)=vmnsc(j,k) - vmn0
    DO l=1, JBAND
      vmnrlb(j,K,l) = vmnscb(j,K,l) - vmn0b(l)
    ENDDO
   ENDIF
   ENDDO

  ENDDO

! prinit output

!------------------
  WRITE (6,388)
  388 FORMAT (/1X,'SECOND PASS: MEANS AND STANDARD DEVIATIONS')
  DO j=1, tovs%nchan
    jv = j
    WRITE (6,289) jv, nobs(j), vmean_abs(j), vstd_abs(j), vmean_dep(j), vstd_dep(j)
  ENDDO

!-------------------
  WRITE (6,370) (js,js=1,KSCAN)
  370 FORMAT (/5X,'NUMBER AT EACH SCAN POSITION'/5X,30I7)

  DO j=1, tovs%nchan
    jv = j
    WRITE(6,371) jv, nscanch(j,1:KSCAN)
  ENDDO
  371 FORMAT(1X,I4,30I7)

  WRITE (6,391) (js,js=1,KSCAN)
  391 FORMAT (/1X,'BIASES FOR EACH SCAN ANGLE'/4X,30I7)
  DO j=1, tovs%nchan
    jv = j
    WRITE (6,393) jv, (vmnsc(j,js),js=1,KSCAN)
  ENDDO
  393 FORMAT (1X,I3,30F7.2)

  WRITE (6,394) (js,js=1,KSCAN)
  394 FORMAT (/1X,'STD DEV FOR EACH SCAN ANGLE'/4X,30I7)
  DO j=1, tovs%nchan
     jv = j
    WRITE (6,396) jv, (vstdsc(j,js),js=1,KSCAN)
  ENDDO
  396  FORMAT (1X,I3,30F7.2)

!----------------------
  if (global) then
    WRITE(6,'(1x,a17,2i4)') 'SCAN COEFFICIENTS', jband,kscan
    DO j=1,tovs%nchan
      jv = j
      DO iband=1, jband
         WRITE(6,'(2i3,30f7.2)') jv, iband, vmnrlb(j,1:KSCAN,iband)
      ENDDO
    ENDDO

    if (smoothing) then

     CLOSE(11)

     OPEN(11,FORM='UNFORMATTED')

     READ(11) nobs(:)
     READ(11) vmean_dep(:)
     READ(11) vstd_dep(:)
     READ(11) vmean_abs(:)
     READ(11) vstd_abs(:)

     READ(11) nscanch(:,:)
     READ(11) vmnsc(:,:)
     READ(11) vstdsc(:,:)

     READ(11) nscanchb(:,:,:)
     READ(11) vmnscb(:,:,:)
     READ(11) vstdscb(:,:,:)

! Perform smoothing on banded corrections.
! for absolute bias/stdev

     vmnscbt = 0.0
     vstdscbt= 0.0
     nscanchbt = 0

     DO j=1, tovs%nchan
      DO iband=1, JBAND

!      IF (iband <= AVBAND) THEN
!        aoff = AVBAND - iband + 1
!        avb = AVBAND
!      ELSEIF ((iband < 2*AVBAND+1) .AND. (iband > AVBAND))  THEN
!        aoff = 0
!        avb = 2*AVBAND+1 - iband
!      ELSEIF ((iband >= 2*AVBAND+1) .AND. (JBAND-iband+1 >= 2*AVBAND+1)) THEN
!        aoff = 0
!        avb = 0
!      ELSEIF ((JBAND-iband+1 < 2*AVBAND+1) .AND. (JBAND-iband+1 > AVBAND)) THEN
!        aoff = 0
!        avb = 2*AVBAND+1 - (JBAND-iband+1)
!      ELSEIF (JBAND-iband+1 <= AVBAND) THEN
!        aoff = -(AVBAND - (JBAND-iband+1) + 1)
!        avb = AVBAND
!      ENDIF

!      DO ia=-avb, avb
!        iab = iband + ia + aoff
!        vmnscbt(j,1:KSCAN,iband) = vmnscbt(j,1:KSCAN,iband) + vmnscb(j,1:KSCAN,IAb)
!        vstdscbt(j,1:KSCAN,iband) = vstdscbt(j,1:KSCAN,iband) + vstdscb(j,1:KSCAN,IAb)
!        nscanchbt(j,1:KSCAN,iband) = nscanchbt(j,1:KSCAN,iband) + nscanchb(j,1:KSCAN,IAb)
!      ENDDO
        vmnscbt(j,1:KSCAN,iband) =  vmnscb(j,1:KSCAN,iband)
        vstdscbt(j,1:KSCAN,iband) = vstdscb(j,1:KSCAN,iband)
        nscanchbt(j,1:KSCAN,iband) = nscanchb(j,1:KSCAN,iband)
      ENDDO
    ENDDO
 
    DO j=1, tovs%nchan
      WHERE (nscanchbt(j,1:KSCAN,:) /= 0)
       vmnscbt(j,1:KSCAN,:) = vmnscbt(j,1:KSCAN,:)/nscanchbt(j,1:KSCAN,:)
       vstdscbt(j,1:KSCAN,:) = vstdscbt(j,1:KSCAN,:)/nscanchbt(j,1:KSCAN,:) - vmnscbt(j,1:KSCAN,:)**2
       vstdscbt(j,1:KSCAN,:) = SQRT(MAX(0.0,vstdscbt(j,1:KSCAN,:)))
      END WHERE

! get bias at nadir
    IF (MOD(KSCAN,2) == 0 ) THEN  ! even scan number
      iscanm = KSCAN/2

      IF( nscanch(j,iscanm) .ne. 0 .and. nscanch(j,iscanm+1) .ne.0 )then
         vmn0b(:) = 0.5*(vmnscbt(j,iscanm,:)+vmnscbt(j,iscanm+1,:))
      ENDIF

      IF( nscanch(j,iscanm) .eq. 0 .and. nscanch(j,iscanm+1) .ne.0 )then
         vmn0b(:) = vmnscbt(j,iscanm+1,:)
      ENDIF

      IF( nscanch(j,iscanm) .ne. 0 .and. nscanch(j,iscanm+1) .eq.0 )then
         vmn0b(:) = vmnscbt(j,iscanm,:)
      ENDIF

    ELSE
      iscanm = kscan/2 + 1
      vmn0b(:) = vmnscbt(j,iscanm,:)
    ENDIF

!  4.2 compute relative bias
!------------------------------------
   DO k=1,KSCAN
   IF( nscanch(j,k) .ne. 0 )then
    DO l=1, JBAND
      vmnrlb(j,k,l) = vmnscbt(j,k,l) - vmn0b(l)
    ENDDO
   ENDIF
   ENDDO

  ENDDO

! Perform smoothing on banded corrections.
! for relative bias

!  DO j=1, tovs%nchan
!    vmnrlbt(j,1:KSCAN,1) = 0.50*vmnrlb(j,1:KSCAN,1) &
!                         + 0.50*vmnrlb(j,1:KSCAN,2)
!    DO iband=2, JBAND-1
!      vmnrlbt(j,1:KSCAN,iband) = 0.25*vmnrlb(j,1:KSCAN,iband-1) &
!                               + 0.50*vmnrlb(j,1:KSCAN,iband) &
!                               + 0.25*vmnrlb(j,1:KSCAN,iband+1)
!    ENDDO
!    vmnrlbt(j,1:KSCAN,JBAND) = 0.50*vmnrlb(j,1:KSCAN,JBAND-1) &
!                             + 0.50*vmnrlb(j,1:KSCAN,JBAND)
!  ENDDO

 if ( (eband-sband+1) >= 3 ) then
  DO j=1, tovs%nchan
    vmnrlbt(j,1:KSCAN,sband) = 0.50*vmnrlb(j,1:KSCAN,sband) &
                         + 0.50*vmnrlb(j,1:KSCAN,sband+1)
    DO iband=sband, eband-1
      vmnrlbt(j,1:KSCAN,iband) = 0.25*vmnrlb(j,1:KSCAN,iband-1) &
                               + 0.50*vmnrlb(j,1:KSCAN,iband) &
                               + 0.25*vmnrlb(j,1:KSCAN,iband+1)
    ENDDO
    vmnrlbt(j,1:KSCAN,eband) = 0.50*vmnrlb(j,1:KSCAN,eband-1) &
                             + 0.50*vmnrlb(j,1:KSCAN,eband)
  ENDDO
 elseif ( (eband-sband+1) == 2 ) then
    vmnrlbt(j,1:KSCAN,sband) = 0.50*vmnrlb(j,1:KSCAN,sband) &
                         + 0.50*vmnrlb(j,1:KSCAN,sband+1)
    vmnrlbt(j,1:KSCAN,eband) = 0.50*vmnrlb(j,1:KSCAN,eband-1) &
                             + 0.50*vmnrlb(j,1:KSCAN,eband)
 elseif ( (eband-sband+1) == 1 ) then 
    vmnrlbt(j,1:KSCAN,sband) = vmnrlb(j,1:KSCAN,sband)
    vmnrlbt(j,1:KSCAN,eband) = vmnrlb(j,1:KSCAN,eband) 
 endif

! output relative bias
  DO j=1, tovs%nchan
     jv = j
    DO iband=1,JBAND
      WRITE(12) jv, vmnrlbt(j,1:KSCAN,iband)
    ENDDO
  ENDDO

!----------------------
    WRITE(6,'(1x,a30,2i4)') 'SMOOTHED SCAN COEFFICIENTS', jband,kscan
    DO j=1,tovs%nchan
      jv = j
      DO iband=1, jband
         WRITE(6,'(2i3,30f7.2)') jv, iband, vmnrlbt(j,1:KSCAN,iband)
      ENDDO
    ENDDO

 end if   ! end if smoothing

 else  ! regional 
!---------------------
  WRITE (6,397) (js,js=1,KSCAN)
  397 FORMAT (/1X,'RELATIVE BIASES FOR EACH SCAN ANGLE'/4X,30I7)
  DO j=1, tovs%nchan
      jv = j
    WRITE (6,399) jv, vmnrl(j,1:KSCAN)
  ENDDO
  399 FORMAT (1X,I3,30F7.2)

  DO j=1, tovs%nchan
     jv = j
     WRITE(12) jv, vmnrl(j,1:KSCAN)
  ENDDO

!----------------------
 end if   ! global

   deallocate(tovs%tb)
   deallocate(tovs%omb)
   deallocate(tovs%bias)
   deallocate(tovs%qc_flag)
   deallocate(tovs%cloud_flag)
   deallocate(tovs%pred)

  CLOSE(UNIT=10)
  CLOSE(UNIT=11)
  CLOSE(UNIT=12)

  END PROGRAM da_bias_scan
