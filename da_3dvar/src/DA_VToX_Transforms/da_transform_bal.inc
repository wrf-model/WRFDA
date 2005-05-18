SUBROUTINE da_transform_bal( xb, vp, xa, be ,xp,&
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)           :: xb ! First guess structure.
   TYPE (vp_type), INTENT(INOUT)        :: vp ! work array.
   TYPE (x_type), INTENT(OUT)           :: xa ! Analysis increments.
   TYPE (be_type), INTENT(IN)           :: be ! Background errors.
   type (xpose_type), intent(inout)     :: xp   ! Dimensions and xpose buffers.
   integer, intent(in)  :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)  :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)  :: its,ite, jts,jte, kts,kte ! tile   dims.


   INTEGER                              :: i, j, k, kk  ! Loop counters.
   
!-------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------- 
!  linear balance btw psi and t-b, Psfc_b and chi_b 
!  [3.1] Calculate t_b from psi

   xa % t(its:ite,jts:jte,kts:kte)=vp % v3(its:ite,jts:jte,kts:kte)

   DO kk = kts,kte
   DO k = kts,kte
   DO j = jts,jte
   DO i= its,ite
         xa % t(i,j,k) = xa % t(i,j,k) + &
                         be % agvz(i,j,k,kk) * vp % v1(i,j,kk)
   END DO
   END DO
   END DO
   END DO

!  [3.2] Calculate chi_b from psi

      DO k = kts,kte
      DO j = jts,jte
      DO i= its,ite
         vp % v2(i,j,k) = vp % v2(i,j,k) + &
                          be % bvz(i,j,k) * vp % v1(i,j,k)
      END DO
      END DO
      END DO

!  [3.3] Calculate Psfc_b from psi

   xa % psfc(its:ite,jts:jte)=vp % v5(its:ite,jts:jte,1)

   DO kk = kts,kte
   DO j = jts,jte
   DO i= its,ite
      xa % psfc(i,j) = xa % psfc(i,j) + &
                      be % wgvz(i,j,kk) * vp % v1(i,j,kk)
   END DO
   END DO
   END DO

!--convert from delt.ln(ps) to delt.ps
   xa%psfc(its:ite,jts:jte) = xa%psfc(its:ite,jts:jte) &
                            * xb%psfc(its:ite,jts:jte)

!  [3.4] Transform psi and chi to u and v:

!  Communicate halo region.
   CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id1)

   CALL DA_PsiChi_To_UV( vp % v1, vp % v2, xb % coefx, &
                         xb % coefy, xa % u, xa % v,   &
                         ids,ide, jds,jde, kds,kde,    &
                         ims,ime, jms,jme, kms,kme,    &
                         its,ite, jts,jte, kts,kte )

!  [3.5] treat humidity                         


   IF ( cv_options == 3 ) THEN
      xa % q(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte) * &
                                        xb % qs(its:ite,jts:jte,kts:kte)
   ELSE IF ( cv_options_hum == 1 ) THEN

      xa % q(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte)

   ELSE IF ( cv_options_hum == 2 ) THEN

      xa % rh(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte)

      CALL DA_TPRH_To_Q_Lin( xb, xp, xa )

   END IF

END SUBROUTINE da_transform_bal   

