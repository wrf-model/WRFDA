SUBROUTINE DA_New_Statistics_Variable( diff, xb, xbx, xp, &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )
!-------------------------------------------------------------------------
!  PURPOSE: Derive Control Variables and several bv for CV=3
!
!  HISTORY: 09/01/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

   IMPLICIT NONE
   
   type (statistics_type), INTENT(INOUT) :: diff         
   type (xb_type),         INTENT(IN)    :: xb       
   TYPE (xbx_type),        INTENT(IN)    :: xbx        ! Header/non-gridded vars.
   type (xpose_type),      INTENT(INOUT) :: xp         ! Dimensions and xpose buf

   integer, intent(in)          :: ids,ide, jds,jde, kds,kde
   integer, intent(in)          :: ims,ime, jms,jme, kms,kme
   integer, intent(in)          :: its,ite, jts,jte, kts,kte

   real, dimension(ims:ime,jms:jme)         :: coeff  ! Gridpoint coeffs.

   INTEGER :: i,j,k,l       ! Dummy array size.

!-------------------------------------------------------------------------

   print *, 'cv_options=', cv_options

   coeff(its:ite,jts:jte) = 1.0 / ( xb % map_factor(its:ite,jts:jte) * &
                                       xb % map_factor(its:ite,jts:jte) )

!  [2.1] Transform u, v to streamfunction via vorticity:
#ifndef DEREF_KLUDGE
   CALL DA_UV_To_Vorticity( xb % ds, xb % map_factor, diff % u, diff % v, diff%vor, &
#else
   CALL DA_UV_To_Vorticity( xb % ds, &
                            xb % map_factor(ims,jms), &
                            diff % u(ims,jms,kms), &
                            diff % v(ims,jms,kms), &
                            diff % vor(ims,jms,kms), &
#endif
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )
!  Convert vorticity to Del**2 psi:

   DO k = kts, kte
     diff%vor(its:ite,jts:jte,k) = coeff(its:ite,jts:jte) * & 
                                   diff%vor(its:ite,jts:jte,k)
   END DO

!--------------------------------------------------------------------
!   CALL DA_Solve_PoissonEqn_FCT(xb, xbx, diff%vor, diff%psi, xp)
   CALL DA_Solve_PoissonEqn_FST(xb, xbx, xp, diff%vor, diff%psi,    &
                                    ids,ide, jds,jde, kds,kde,      &
                                    ims,ime, jms,jme, kms,kme,      &
                                    its,ite, jts,jte, kts,kte )
    diff%psi(ids:ide,jds,kds:kde)= diff%psi(ids:ide,jds+1,kds:kde)
    diff%psi(ids,jds:jde,kds:kde)= diff%psi(ids+1,jds:jde,kds:kde)

!  [2.2] Transform u, v to velocity potential via divergence:
#ifndef DEREF_KLUDGE
   CALL DA_UV_To_Divergence( xb % ds, xb % map_factor, diff % u, diff % v, diff%div, &
#else
   CALL DA_UV_To_Divergence(xb % ds, &
                            xb % map_factor(ims,jms), &
                            diff % u(ims,jms,kms), &
                            diff % v(ims,jms,kms), &
                            diff%div(ims,jms,kms), &
#endif
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

!  Convert divergence to Del**2 chi:

   DO k = kts, kte
    diff%div(its:ite,jts:jte,k) = coeff(its:ite,jts:jte) * &
                             diff%div(its:ite,jts:jte,k)
   END DO

!  Calculate chi:

!   CALL DA_Solve_PoissonEqn_FCT(xb, xbx, diff%div, diff%chi, xp)
   CALL DA_Solve_PoissonEqn_FST(xb, xbx, xp, diff%div, diff%chi,    &
                                    ids,ide, jds,jde, kds,kde,      &
                                    ims,ime, jms,jme, kms,kme,      &
                                    its,ite, jts,jte, kts,kte )

    diff%chi(ids:ide,jds,kds:kde)= diff%chi(ids:ide,jds+1,kds:kde)
    diff%chi(ids,jds:jde,kds:kde)= diff%chi(ids+1,jds:jde,kds:kde)

END SUBROUTINE DA_New_Statistics_Variable
