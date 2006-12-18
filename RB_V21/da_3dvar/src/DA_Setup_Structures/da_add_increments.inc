SUBROUTINE da_add_increments( it, xp, xa, xb, xbx )

!------------------------------------------------------------------------------
!  PURPOSE: Add analysis increments to background field.
!
!  METHOD:
!
!  HISTORYi : 05/02/2001 - Creation of F90 version.           Dale Barker
!  Additions: 07/25/2003 -                                    S.R.H. Rizvi
!                     1. Surface fields are updated
!                        by calling da_transfer_xbtoxb 
!                     2. Check on Humidity is applied for each outer iteration
!  PARENT_MODULE: DA_Minimisation
!  
!------------------------------------------------------------------------------

   implicit none

   integer                              :: it    ! External iteration.
   type (xpose_type), intent(in)        :: xp    ! Domain decomposition vars.
   type (x_type),  intent(inout)        :: xa    ! Analysis increment
   type (xb_type), intent(inout)        :: xb    ! First guess state
   type (xbx_type), intent(in)          :: xbx   ! For header & non-grid arrays.

   integer                              :: is,ie,js,je,ks,ke

   is = xp%its
   ie = xp%ite
   js = xp%jts
   je = xp%jte
   ks = xp%kts
   ke = xp%kte

!------------------------------------------------------------------------------
!  [1.0] Check bounds on relative humidity and recalculate rh, q increments:
!------------------------------------------------------------------------------

   if ( cv_options == 3 .or. cv_options_hum == 1  ) then

!     Calculate rh increments 
!     (This is the only place they are used with cv_options_hum=1)

      call da_tpq_to_rh_lin( xb, xp, xa )
   end if

      if ( check_rh == 1 ) THEN

!        Simple resetting to max/min values:
         call da_check_rh_simple( xb, xa, xp )

      else if ( check_rh == 2 ) THEN

!        Shu-Hua~s TPW conservation:
         CALL da_check_rh( xb, xa, xp )

      end if

!----------------------------------------------------------------------------
!  [2.0] Straightforward addition:
!----------------------------------------------------------------------------

   xb % u (is:ie,js:je,ks:ke) = xb % u (is:ie,js:je,ks:ke) + xa % u (is:ie,js:je,ks:ke)
   xb % v (is:ie,js:je,ks:ke) = xb % v (is:ie,js:je,ks:ke) + xa % v (is:ie,js:je,ks:ke)
   xb % t (is:ie,js:je,ks:ke) = xb % t (is:ie,js:je,ks:ke) + xa % t (is:ie,js:je,ks:ke)


   xb % psfc(is:ie,js:je) = xb % psfc(is:ie,js:je) + xa % psfc (is:ie,js:je)
   xb % p (is:ie,js:je,ks:ke) = xb % p (is:ie,js:je,ks:ke) + xa % p (is:ie,js:je,ks:ke)

   xb % q (is:ie,js:je,ks:ke) = xb % q (is:ie,js:je,ks:ke) + xa % q (is:ie,js:je,ks:ke)

   xb % qcw (is:ie,js:je,ks:ke) = xb % qcw (is:ie,js:je,ks:ke) + xa % qcw (is:ie,js:je,ks:ke)

   xb % qrn (is:ie,js:je,ks:ke) = xb % qrn (is:ie,js:je,ks:ke) + xa % qrn (is:ie,js:je,ks:ke)

   xb % qci (is:ie,js:je,ks:ke) = xb % qci (is:ie,js:je,ks:ke) + xa % qci (is:ie,js:je,ks:ke)
   xb % qsn (is:ie,js:je,ks:ke) = xb % qsn (is:ie,js:je,ks:ke) + xa % qsn (is:ie,js:je,ks:ke)
   xb % qgr (is:ie,js:je,ks:ke) = xb % qgr (is:ie,js:je,ks:ke) + xa % qgr (is:ie,js:je,ks:ke)

   xb % rh(is:ie,js:je,ks:ke) = xb % rh(is:ie,js:je,ks:ke) + xa % rh(is:ie,js:je,ks:ke)

   if(W_INCREMENTS) then
      xb % w(is:ie,js:je,ks:ke+1) = xb % w(is:ie,js:je,ks:ke+1) + xa % w(is:ie,js:je,ks:ke+1)
   endif

!------------------------------------------------------------------------------
!  [3.0] Calculate derived variables:
!------------------------------------------------------------------------------

!  Calculate density:

   xb % rho(is:ie,js:je,ks:ke) = xb % p(is:ie,js:je,ks:ke) / &
                              ( gas_constant * xb % t(is:ie,js:je,ks:ke) )

!------------------------------------------------------------------------------
!  [4.0] Calculate humidity variables:
!------------------------------------------------------------------------------

!  Calculate dew-point temperature

   call DA_TRH_To_TD (xb % rh, xb % t, xb % td, xp )
      
!---- -------------------------------------------------------------------------
!  [5.0] Calculate analysis increment (A-B) statistics:
!------------------------------------------------------------------------------

!   CALL DA_Analysis_Stats ( stats_unit, xp, xa )

!------------------------------------------------------------------------------
!  [6.0] ADD PBL and SURFACE INFO.
!------------------------------------------------------------------------------

   CALL DA_Add_PBL_And_SFC_Info( xp, xb, xbx )

end subroutine da_add_increments

