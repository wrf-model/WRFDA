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
   CALL DA_UV_To_Vorticity( xb, diff % u, diff % v, diff % vor, &
#else
   CALL DA_UV_To_Vorticity( xb, diff % u(ims,jms,kms), &
                            diff % v(ims,jms,kms), &
                            diff % vor(ims,jms,kms), &
#endif
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

!   write (*,'(a,e20.12)') '1, Vor: diff%vor(10,10,10)=', diff%vor(10,10,10)

!  Convert vorticity to Del**2 psi:

   DO k = kts, kte
     diff%vor(its:ite,jts:jte,k) = coeff(its:ite,jts:jte) * & 
                                   diff%vor(its:ite,jts:jte,k)
   END DO
!   write (*,'(a,e20.12)') '2, Vor: diff%vor(10,10,10)=', diff%vor(10,10,10)

!--------------------------------------------------------------------
   CALL DA_Solve_PoissonEqn_FST(xb, xbx, xp, diff%vor, diff%psi,    &
                                    ids,ide, jds,jde, kds,kde,      &
                                    ims,ime, jms,jme, kms,kme,      &
                                    its,ite, jts,jte, kts,kte )
   if(jts == jds) then
    diff%psi(its:ite,jds,kts:kte)= diff%psi(its:ite,jds+1,kts:kte)
   endif

   if(its == ids) then
    diff%psi(ids,jts:jte,kts:kte)= diff%psi(ids+1,jts:jte,kts:kte)
   endif

!  [2.2] Transform u, v to velocity potential via divergence:
#ifndef DEREF_KLUDGE
   CALL DA_UV_To_Divergence( xb, diff % u, diff % v, diff % div, &
#else
   CALL DA_UV_To_Divergence( xb, diff % u(ims,jms,kms), &
                                 diff % v(ims,jms,kms), &
                                 diff % div(ims,jms,kms), &
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

   CALL DA_Solve_PoissonEqn_FST(xb, xbx, xp, diff%div, diff%chi,    &
                                    ids,ide, jds,jde, kds,kde,      &
                                    ims,ime, jms,jme, kms,kme,      &
                                    its,ite, jts,jte, kts,kte )

   if(jts == jds) then
    diff%chi(its:ite,jds,kts:kte)= diff%chi(its:ite,jds+1,kts:kte)
   endif

   if(its == ids) then
    diff%chi(ids,jts:jte,kts:kte)= diff%chi(ids+1,jts:jte,kts:kte)
   endif

END SUBROUTINE DA_New_Statistics_Variable

