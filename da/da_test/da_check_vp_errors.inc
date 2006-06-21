subroutine DA_Check_Vp_Errors( vp1, vp2, ne, &
                               its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Test invertibility of transform to/from Vp or Vv
!
!  METHOD:  Perform statistics on differences in initial and final Vv or Vp
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (vp_type), INTENT(IN)     :: vp1         ! Test input
   TYPE (vp_type), INTENT(IN)     :: vp2         ! Test output.
   integer, intent(in)            :: ne          ! Ensemble size.
   integer, intent(in)            :: its,ite, jts,jte, kts,kte ! tile   dims.

   REAL                           :: inv_size    ! 1/size of array.
   REAL                           :: rms_fild    ! RMS of field.
   REAL                           :: rms_diff    ! RMS of differnce.

   REAL, DIMENSION(its:ite, jts:jte, kts:kte) :: diff ! Difference
   real                           :: diff_alpha(its:ite,jts:jte,1:ne)

   inv_size = 1.0 / REAL( (ite-its+1) * (jte-jts+1) * (kte-kts+1) )

!-------------------------------------------------------------------------
!  [1.0]: Check v1 differences:
!-------------------------------------------------------------------------

   diff(its:ite,jts:jte, kts:kte) = vp2 % v1(its:ite,jts:jte,kts:kte) - &
                                    vp1 % v1(its:ite,jts:jte,kts:kte)

   rms_fild = SQRT( SUM( vp1 % v1(its:ite, jts:jte,kts:kte) &
                       * vp1 % v1(its:ite, jts:jte,kts:kte) ) * inv_size )
   rms_diff = SQRT( SUM( diff(its:ite, jts:jte,kts:kte) &
                       * diff(its:ite, jts:jte,kts:kte) ) * inv_size )
     
   IF ( rms_fild == 0.0 ) THEN
      write(unit=stdout, fmt='(a)') ' v1 is zero ' 
   ELSE
      write(unit=stdout, fmt='(a,1pe10.4)') &
           ' v1 RMS error/RMS field = ', rms_diff/rms_fild
   END IF      

!-------------------------------------------------------------------------
!  [2.0]: Check v2 differences:
!-------------------------------------------------------------------------

   diff(its:ite,jts:jte, kts:kte) = vp2 % v2(its:ite,jts:jte,kts:kte) - &
                                    vp1 % v2(its:ite,jts:jte,kts:kte)

   rms_fild = SQRT( SUM( vp1 % v2(its:ite, jts:jte,kts:kte) &
                       * vp1 % v2(its:ite, jts:jte,kts:kte) ) * inv_size )
   rms_diff = SQRT( SUM( diff(its:ite, jts:jte,kts:kte) &
                       * diff(its:ite, jts:jte,kts:kte) ) * inv_size )
     
   IF ( rms_fild == 0.0 ) THEN
      write(unit=stdout, fmt='(a)') ' v2 is zero ' 
   ELSE
      write(unit=stdout, fmt='(a,1pe10.4)') &
           ' v2 RMS error/RMS field = ', rms_diff/rms_fild
   END IF      

!-------------------------------------------------------------------------
!  [3.0]: Check v3 differences:
!-------------------------------------------------------------------------

   diff(its:ite,jts:jte, kts:kte) = vp2 % v3(its:ite,jts:jte,kts:kte) - &
                                    vp1 % v3(its:ite,jts:jte,kts:kte)

   rms_fild = SQRT( SUM( vp1 % v3(its:ite, jts:jte,kts:kte) &
                       * vp1 % v3(its:ite, jts:jte,kts:kte) ) * inv_size )
   rms_diff = SQRT( SUM( diff(its:ite, jts:jte,kts:kte) &
                       * diff(its:ite, jts:jte,kts:kte) ) * inv_size )
     
   IF ( rms_fild == 0.0 ) THEN
      write(unit=stdout, fmt='(a)') ' v3 is zero ' 
   ELSE
      write(unit=stdout, fmt='(a,1pe10.4)') &
           ' v3 RMS error/RMS field = ', rms_diff/rms_fild
   END IF      

!-------------------------------------------------------------------------
!  [4.0]: Check v4 differences:
!-------------------------------------------------------------------------

   diff(its:ite,jts:jte, kts:kte) = vp2 % v4(its:ite,jts:jte,kts:kte) - &
                                    vp1 % v4(its:ite,jts:jte,kts:kte)

   rms_fild = SQRT( SUM( vp1 % v4(its:ite, jts:jte,kts:kte) &
                       * vp1 % v4(its:ite, jts:jte,kts:kte) ) * inv_size )
   rms_diff = SQRT( SUM( diff(its:ite, jts:jte,kts:kte) &
                       * diff(its:ite, jts:jte,kts:kte) ) * inv_size )
     
   IF ( rms_fild == 0.0 ) THEN
      write(unit=stdout, fmt='(a)') ' v4 is zero ' 
   ELSE
      write(unit=stdout, fmt='(a,1pe10.4)') &
           ' v4 RMS error/RMS field = ', rms_diff/rms_fild
   END IF
      
!-------------------------------------------------------------------------
!  [5.0]: Check v5 differences:
!-------------------------------------------------------------------------

   inv_size = 1.0 / REAL( (ite-its+1) * (jte-jts+1) )

   diff(its:ite, jts:jte,kts:kte) = vp2 % v5(its:ite, jts:jte,kts:kte) - vp1 % v5(its:ite, jts:jte,kts:kte)

   rms_fild = SQRT( SUM( vp1 % v5(its:ite, jts:jte,kts:kte) * vp1 % v5(its:ite, jts:jte,kts:kte) ) * inv_size )
   rms_diff = SQRT( SUM( diff(its:ite, jts:jte,1) * diff(its:ite, jts:jte,1) ) * inv_size )
     
   IF ( rms_fild == 0.0 ) THEN
      write(unit=stdout, fmt='(a)') ' v5 is zero ' 
   ELSE
      write(unit=stdout, fmt='(a,1pe10.4)') &
           ' v5 RMS error/RMS field = ', rms_diff/rms_fild
   END IF    
      
!-------------------------------------------------------------------------
!  [6.0]: Check alpha differences:
!-------------------------------------------------------------------------
 
   if ( ne > 0 ) then
      inv_size = 1.0 / REAL( (ite-its+1) * (jte-jts+1) * ne )
      diff_alpha(its:ite,jts:jte,1:ne) = vp2 % alpha(its:ite,jts:jte,1:ne) - &
                                         vp1 % alpha(its:ite,jts:jte,1:ne)

      rms_fild = SQRT( SUM( vp1 % alpha(its:ite, jts:jte,1:ne) &
                          * vp1 % alpha(its:ite, jts:jte,1:ne) ) * inv_size )
      rms_diff = SQRT( SUM( diff_alpha(its:ite, jts:jte,1:ne) &
                          * diff_alpha(its:ite, jts:jte,1:ne) ) * inv_size )
     
      if ( rms_fild /= 0.0 ) then
         write(unit=stdout, fmt='(a,1pe10.4)') ' alpha RMS error/RMS field = ', rms_diff/rms_fild
      end if
   end if

end subroutine DA_Check_Vp_Errors
