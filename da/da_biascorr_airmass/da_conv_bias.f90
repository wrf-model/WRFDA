  PROGRAM da_conv_bias
! program to combine reg and scan bias file from rtovs /atovs to a bcor.dat file
! input file "coefs_noaa" contains one or more files concatenated together
! output a file call "bcor.noaa"

  USE RAD_BIAS
  IMPLICIT NONE

  REAL(KIND=LONG)  :: z3,year,month,day,time
  logical :: global

  INTEGER :: i, j,k, ic, band
  INTEGER :: nchan , npred, KSCANX ,JBANDX

  REAL(KIND=LONG)  ::  mean_dep(JPCHAN), std_dep(JPCHAN), &
                       bccoef(JPCHAN,JPNX+1), &
                       sccoef(JPCHAN,JPSCAN), &
                       sccoefb(JPCHAN,JPSCAN,JBAND)

  INTEGER :: yy, mm, dd, tt
  INTEGER :: platform_id, satellite_id, sensor_id

!----------------------------------------------------
  print * ,' start da_conv_bias '

  bccoef(:,:) = 0.0
  sccoefb(:,:,:) = 0.0

  OPEN(19,FORM='UNFORMATTED',STATUS='UNKNOWN')  ! binary input 
  OPEN(77,FORM='FORMATTED',STATUS='UNKNOWN')    ! ascii output

! 1.0 read/write header
!---------------------------
   READ(19,END=110) platform_id, satellite_id, sensor_id, &
                    year, month, day, time, &
                    nchan, npred, KSCANX ,JBANDX, global 
   yy  = int(year)
   dd  = int(day)
   mm  = int(month)  

   WRITE(77,*) 'HEADER'
   WRITE(77,'(8i6,3x,L1)') platform_id, satellite_id, sensor_id, &
                        nchan, npred, yy, mm, dd, global

! 2.0 read/write air-mass bias coefs
!-----------------------------------------
    WRITE(77,*) 'AIR-MASS BIAS COEFFICIENTS'
    DO i=1,nchan
       READ(19,end=110) ic, mean_dep(i), std_dep(i), z3, bccoef(i,1:npred+1)
     do j=1,npred+1
       if(bccoef(i,j) < -1.e20 ) bccoef(i,j) = 0.0
       if(bccoef(i,j) >  1.e20 ) bccoef(i,j) = 0.0
     enddo
       WRITE(77,'(i3,8f16.8)') ic, bccoef(i,1:npred+1)
    ENDDO

! 3.0 read/write scan bias
!-----------------------------
    WRITE(77,'(1x,a30,i4)') 'GLOBAL SCAN COEFFICIENTS', kscanx
    DO i=1,nchan
      READ(19,end=110) ic, sccoef(i,1:KSCANX)                                                
      WRITE(77,'(i3,30f7.2)') ic, sccoef(i,1:KSCANX)                                  
    ENDDO

    WRITE(77,'(1x,a17,2i4)') 'LAT-BAND SCAN COEFFICIENTS', kscanx, jbandx
    DO i=1,nchan
    DO band=1, jbandx
      READ(19,end=110) ic, sccoefb(i,1:KSCANX,band)
      WRITE(77,'(2i3,90f7.2)') ic, band, sccoefb(i,1:KSCANX,band)
    ENDDO
    ENDDO

  110 CONTINUE

    print *, 'End da_conv_bias'

  END PROGRAM da_conv_bias
