SUBROUTINE DA_Transform_XToVp( xb, xbx, xa, xp, vp, be, &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)           :: xb         ! First guess structure.
   TYPE (xbx_type),INTENT(IN)           :: xbx        ! Header/non-gridded vars.
   TYPE (x_type), INTENT(INOUT)         :: xa         ! Analysis increments.
   type (xpose_type), intent(inout)     :: xp         ! Dimensions and xpose buffers. 
   TYPE (vp_type), INTENT(OUT)          :: vp         ! CV on grid structure.
   type (be_type), intent(in), optional :: be         ! Background errors.
   integer, intent(in)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
   integer, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
   integer, intent(in)  :: its,ite, jts,jte, kts,kte  ! tile   dims.

   real, dimension(ims:ime,jms:jme)         :: grid_coeff  ! Gridpoint coeffs.
   real, dimension(ims:ime,jms:jme,kms:kme) :: vor, & ! Vorticity.
                                               div, & ! Divergence.
                                               phi_b  ! Balanced mass increment.

   real, dimension(kms:kme,ims:ime,jms:jme) :: phi_b_copy
   real, dimension(kms:kme,jms:jme,kms:kme) :: reg_copy

   integer :: i, j, k, kk                             ! Loop counters.
   real :: sum_vphi                                   ! Summing variable.

!-------------------------------------------------------------------
!  [1.0] Perform transform v = U^{-1} x
!-------------------------------------------------------------------      

   IF ( cv_options /= 2) THEN
      write(unit=*, fmt='(a,i6)') &
           'Cannot perform Transform_XToVp for cv_options:', cv_options

      CALL wrf_shutdown
      stop 'Wrong cv_options.'
   ENDIF

   print *, 'cv_options=', cv_options

   grid_coeff(its:ite,jts:jte) = 1.0 / ( xb % map_factor(its:ite,jts:jte) * &
                                       xb % map_factor(its:ite,jts:jte) )

   call DA_Zero_vp_type ( vp )

!  [2.2] Transform u, v to streamfunction via vorticity:

!  Communicate halo region.
   CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)

#ifndef DEREF_KLUDGE
   CALL DA_UV_To_Vorticity( xb % ds, xb % map_factor, xa % u, xa % v, vor, &
#else
   CALL DA_UV_To_Vorticity( xb % ds, &
                            xb % map_factor(ims,jms), &
                            xa % u(ims,jms,kms), &
                            xa % v(ims,jms,kms), &
                            vor(ims,jms,kms), &
#endif
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

!  Convert vorticity to Del**2 psi:

   DO k = kts, kte
      vor(its:ite,jts:jte,k) = grid_coeff(its:ite,jts:jte)*vor(its:ite,jts:jte,k)
   END DO

!  Calculate psi:

   CALL DA_Solve_PoissonEqn_FCT(xb, xbx, vor, vp%v1, xp)

!  [2.3] Transform u, v to velocity potential via divergence:

#ifndef DEREF_KLUDGE
   CALL DA_UV_To_Divergence( xa % u, xa % v, xb%coefx, xb%coefy, div, &
#else
   CALL DA_UV_To_Divergence(xa % u(ims,jms,kms), &
                            xa % v(ims,jms,kms), &
                            xb % coefx(ims,jms), &
                            xb % coefy(ims,jms), &
                            div(ims,jms,kms), &
#endif
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

!  Convert divergence to Del**2 chi:

   DO k = kts, kte
      div(its:ite,jts:jte,k) = grid_coeff(its:ite,jts:jte) * div(its:ite,jts:jte,k)
   END DO

!  Calculate chi:

   CALL DA_Solve_PoissonEqn_FCT(xb, xbx, div, vp%v2, xp)

!  [2.4] Transform chi to chi_u:

!  Communicate halo region.
   CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id1)

!  [2.5] Calculate phi_b from u and v and use it with p
!   to calculate unbalanced phi:

#ifndef DEREF_KLUDGE
   CALL DA_Balance_Equation_Lin( xb, xbx, xp,                &
                                 xa % u, xa % v, phi_b,      &
#else
   CALL DA_Balance_Equation_Lin( xb, xbx, xp,                &
                                 xa % u(ims,jms,kms), &
                                 xa % v(ims,jms,kms), &
                                 phi_b(ims,jms,kms), &
#endif

                                 ids,ide, jds,jde, kds,kde,  &
                                 ims,ime, jms,jme, kms,kme,  &
                                 its,ite, jts,jte, kts,kte )

!  [2.4] Compute phi:

   IF ( PRESENT(be) ) THEN

      DO k = kts, kte ! Need copies for all model levels for sum:
         reg_copy(k,jts:jte,kts:kte) = be % pb_vert_reg(jts:jte,kts:kte,k)
         phi_b_copy(k,its:ite,jts:jte) = phi_b(its:ite,jts:jte,k)
      END DO

      DO k = kts, kte
      DO j = jts, jte
      DO i = its, ite
!--------SUM OVER ALL LEVELS (kts:kte):
         sum_vphi = 0.0

         DO kk = kts, kte
            sum_vphi = sum_vphi + reg_copy(kk,j,k) * phi_b_copy(kk,i,j)
         END DO
         xa % p(i,j,k) = sum_vphi
      END DO
      END DO
      END DO
   ELSE
      xa % p(its:ite,jts:jte,kts:kte) = phi_b(its:ite,jts:jte,kts:kte)
   ENDIF

   vp % v3(its:ite,jts:jte,kts:kte) = xa % p(its:ite,jts:jte,kts:kte)

!  [2.6] Choice of moisture control variable: 
  
   IF ( cv_options_hum == 2 ) THEN

      CALL DA_TPQ_To_RH_Lin( xb, xp, xa )

      vp % v4(its:ite,jts:jte,kts:kte) = xa % rh(its:ite,jts:jte,kts:kte)

!!!-------------------------------------------------------------------
   else IF ( cv_options_hum == 3 ) THEN
     xa % qt(its:ite,jts:jte,kts:kte) = xa % q  (its:ite,jts:jte,kts:kte) +   &
                                        xa % qcw(its:ite,jts:jte,kts:kte) +   &
                                        xa % qrn(its:ite,jts:jte,kts:kte)
     vp % v4(its:ite,jts:jte,kts:kte) = xa % qt (its:ite,jts:jte,kts:kte)
!!!-------------------------------------------------------------------

      ELSE IF ( cv_options_hum == 1 ) THEN

      vp % v4(its:ite,jts:jte,kts:kte) = xa % q(its:ite,jts:jte,kts:kte)
      
   END IF
               
!  [2.7] v5 set to zero (ground temperature in future):

   vp % v5(its:ite,jts:jte,kts:kte) = 0.0

END SUBROUTINE DA_Transform_XToVp

