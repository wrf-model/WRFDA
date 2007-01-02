SUBROUTINE DA_Check_Vp_Errors( vp1, vp2, kkz, &
                               its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Test invertibility of transform to/from Vp or Vv
!
!  METHOD:  Perform statistics on differences in initial and final Vv or Vp
!
!  HISTORY: 02/23/2000 - Creation of F90 version.       Dale Barker
!           02/02/2005 - Updated Rt owork for Vv also   Syed RH Rizvi    
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (vp_type), INTENT(IN)     :: vp1         ! Test input
   TYPE (vp_type), INTENT(IN)     :: vp2         ! Test output.

   integer, intent(in)            :: kkz(5)      ! Vertical dims               
   integer, intent(in)            :: its,ite, jts,jte, kts,kte ! tile   dims.

   REAL                           :: inv_size    ! 1/size of array.
   REAL                           :: rms_fild    ! RMS of field.
   REAL                           :: rms_diff    ! RMS of differnce.

   REAL, DIMENSION(its:ite, jts:jte, kts:kte) :: diff ! Difference

   inv_size = 1.0 / REAL( (ite-its+1) * (jte-jts+1) * (kte-kts+1) )

!-------------------------------------------------------------------------
!  [1.0]: Check v1 differences:
!-------------------------------------------------------------------------
   diff(its:ite, jts:jte, 1:kkz(1)) = vp2 % v1(its:ite, jts:jte, 1:kkz(1)) &
                                      - vp1 % v1(its:ite, jts:jte, 1:kkz(1))

   rms_fild = SQRT( SUM( vp1 % v1(its:ite, jts:jte, 1:kkz(1)) &
                       * vp1 % v1(its:ite, jts:jte, 1:kkz(1)) ) * inv_size )
   rms_diff = SQRT( SUM( diff(its:ite, jts:jte, 1:kkz(1)) &
                       * diff(its:ite, jts:jte, 1:kkz(1)) ) * inv_size )
     
   IF ( rms_fild == 0.0 ) THEN
      write(unit=*, fmt='(a)') ' v1 is zero ' 
   ELSE
      write(unit=*, fmt='(a,1pe10.4)') &
           ' v1 RMS error/RMS field = ', rms_diff/rms_fild
   END IF      

!-------------------------------------------------------------------------
!  [2.0]: Check v2 differences:
!-------------------------------------------------------------------------

   diff(its:ite, jts:jte, 1:kkz(2)) = vp2 % v2(its:ite, jts:jte, 1:kkz(2)) &
                                      - vp1 % v2(its:ite, jts:jte, 1:kkz(2))

   rms_fild = SQRT( SUM( vp1 % v2(its:ite, jts:jte, 1:kkz(2)) &
                       * vp1 % v2(its:ite, jts:jte, 1:kkz(2)) ) * inv_size )
   rms_diff = SQRT( SUM( diff(its:ite, jts:jte, 1:kkz(2)) &
                       * diff(its:ite, jts:jte, 1:kkz(2)) ) * inv_size )
     
   IF ( rms_fild == 0.0 ) THEN
      write(unit=*, fmt='(a)') ' v2 is zero ' 
   ELSE
      write(unit=*, fmt='(a,1pe10.4)') &
           ' v2 RMS error/RMS field = ', rms_diff/rms_fild
   END IF                    
     
!-------------------------------------------------------------------------
!  [3.0]: Check v3 differences:
!-------------------------------------------------------------------------

   diff(its:ite, jts:jte, 1:kkz(3)) = vp2 % v3(its:ite, jts:jte, 1:kkz(3)) &
                                      - vp1 % v3(its:ite, jts:jte, 1:kkz(3))

   rms_fild = SQRT( SUM( vp1 % v3(its:ite, jts:jte, 1:kkz(3)) &
                       * vp1 % v3(its:ite, jts:jte, 1:kkz(3)) ) * inv_size )
   rms_diff = SQRT( SUM( diff(its:ite, jts:jte, 1:kkz(3)) &
                       * diff(its:ite, jts:jte, 1:kkz(3)) ) * inv_size )
     
   IF ( rms_fild == 0.0 ) THEN
      write(unit=*, fmt='(a)') ' v3 is zero ' 
   ELSE
      write(unit=*, fmt='(a,1pe10.4)') &
           ' v3 RMS error/RMS field = ', rms_diff/rms_fild
   END IF                        

!-------------------------------------------------------------------------
!  [4.0]: Check v4 differences:
!-------------------------------------------------------------------------

   diff(its:ite, jts:jte, 1:kkz(4)) = vp2 % v4(its:ite, jts:jte, 1:kkz(4)) &
                                      - vp1 % v4(its:ite, jts:jte, 1:kkz(4))

   rms_fild = SQRT( SUM( vp1 % v4(its:ite, jts:jte, 1:kkz(4)) &
                       * vp1 % v4(its:ite, jts:jte, 1:kkz(4)) ) * inv_size )
   rms_diff = SQRT( SUM( diff(its:ite, jts:jte, 1:kkz(4)) &
                       * diff(its:ite, jts:jte, 1:kkz(4)) ) * inv_size )
     
   IF ( rms_fild == 0.0 ) THEN
      write(unit=*, fmt='(a)') ' v4 is zero ' 
   ELSE
      write(unit=*, fmt='(a,1pe10.4)') &
           ' v4 RMS error/RMS field = ', rms_diff/rms_fild
   END IF    
      
!-------------------------------------------------------------------------
!  [5.0]: Check v5 differences:
!-------------------------------------------------------------------------

   diff(its:ite, jts:jte, 1:kkz(5)) = vp2 % v5(its:ite, jts:jte, 1:kkz(5)) &
                                      - vp1 % v5(its:ite, jts:jte, 1:kkz(5))

   rms_fild = SQRT( SUM( vp1 % v5(its:ite, jts:jte, 1:kkz(5)) &
                       * vp1 % v5(its:ite, jts:jte, 1:kkz(5)) ) * inv_size )
   rms_diff = SQRT( SUM( diff(its:ite, jts:jte, 1:kkz(5)) &
                       * diff(its:ite, jts:jte, 1:kkz(5)) ) * inv_size )
     
   IF ( rms_fild == 0.0 ) THEN
      write(unit=*, fmt='(a)') ' v5 is zero ' 
   ELSE
      write(unit=*, fmt='(a,1pe10.4)') &
           ' v5 RMS error/RMS field = ', rms_diff/rms_fild
   END IF    
      
END SUBROUTINE DA_Check_Vp_Errors
