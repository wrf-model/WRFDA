subroutine DA_Transform_VpToX( xb, xbx, vp, xp, xa, be,    &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )

   implicit none
   
   type (xb_type), intent(in)           :: xb  ! First guess structure.
   type (xbx_type),intent(in)           :: xbx ! Header & non-gridded vars.
   type (vp_type), intent(in)           :: vp  ! CV on grid structure.
   type (xpose_type), intent(inout)     :: xp  ! Transpose variables.
   type (x_type), intent(out)           :: xa  ! Analysis increments.
   type (be_type), intent(in), optional :: be  ! Background errors.

   integer, intent(in)  :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)  :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)  :: its,ite, jts,jte, kts,kte ! tile   dims.

   integer              :: i, j, k, kk               ! Loop counters.
   real                 :: sum_vphi                  ! Summing variable.

   real, dimension(ims:ime,jms:jme,kms:kme) :: phi_b ! Balanced mass incrmt

   real, dimension(kms:kme,ims:ime,jms:jme) :: phi_b_copy
   real, dimension(kms:kme,jms:jme,kms:kme) :: reg_copy

!     [2.2] Transform psi and chi_u to chi:

!     [2.3] Transform psi and chi to u and v:

!     Communicate halo region.
      CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id1)

      CALL DA_PsiChi_To_UV( vp % v1, vp % v2, xb % map_factor, &
                            xb % ds, xa % u, xa % v, &
                            ids,ide, jds,jde, kds,kde, &
                            ims,ime, jms,jme, kms,kme, &
                            its,ite, jts,jte, kts,kte )

!     Communicate halo region.
      CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id1)

!     [2.3] Calculate phi_b from u and v:
      
      CALL DA_Balance_Equation_Lin( xb, xbx, xp,                &
                                    xa % u, xa % v, phi_b,      &
                                    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte )

!     [2.4] Compute phi:

      IF ( PRESENT(be) ) THEN

         DO k = kts, kte ! Need copies for all model levels for sum:
            reg_copy(k,jts:jte,kts:kte) = be % pb_vert_reg(jts:jte,kts:kte,k)
            phi_b_copy(k,its:ite,jts:jte) = phi_b(its:ite,jts:jte,k)
         END DO

         DO k = kts, kte
            DO i = its, ite
               DO j = jts, jte
!                 SUM OVER ALL LEVELS (kts:kte):
                  sum_vphi = reg_copy(kts,j,k) * phi_b_copy(kts,i,j)

                  DO kk = kts+1, kte ! SUM OVER ALL LEVELS
                     sum_vphi = sum_vphi + reg_copy(kk,j,k) * phi_b_copy(kk,i,j)
                  END DO
                  xa % p(i,j,k) = vp % v3(i,j,k) + sum_vphi
               END DO
            END DO
         END DO
         
      ELSE
         xa % p(its:ite,jts:jte,kts:kte) = phi_b(its:ite,jts:jte,kts:kte) + &
                                           vp % v3(its:ite,jts:jte,kts:kte) 
      END IF

!     [2.5] Compute density increments using hydrostatic eqn:

      CALL DA_HydrostaticP_To_Rho_Lin( xb, xp, xa % p, xa % rho )
         
!     [2.6] Compute temperature increments from p/rho increments:

      CALL DA_PRho_To_T_Lin( xb, xp, xa )

!     [2.7] Choice of moisture control variable:

      IF ( cv_options_hum == 1 ) THEN

         xa % q(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte)
  
      ELSE IF ( cv_options_hum == 2 ) THEN
      
         xa % rh(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte)
      
         CALL DA_TPRH_To_Q_Lin( xb, xp, xa )

      ELSE IF ( cv_options_hum == 3 ) THEN

!   [2.10] Partition of hydrometeor increments via warm rain process

         xa % qt(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte)

         CALL DA_Moist_Phys_Lin( xb, xa, xp,                     &
                                 ids,ide, jds,jde, kds,kde,      &
                                 ims,ime, jms,jme, kms,kme,      &
                                 its,ite, jts,jte, kts,kte )

      END IF
         
!     [2.8] Ground temperature control variable:

!      xa % tgrn(its:ite,jts:jte) = vp % v5(its:ite,jts:jte)

!--surface pressure increments
 
   if (sfc_assi_options == 2) then
      call DA_Transform_XToWTQ ( xp, xb, xa )
   else
      xa%psfc(its:ite,jts:jte)=xa%p(its:ite,jts:jte,kts)
   endif
         
END subroutine DA_Transform_VpToX

