subroutine DA_Transform_VpToX( xb, xbx, vp, xp, xa, be,    &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )
!--------------------------------------------------------------------------
! Purpose : Physical transform of variables
!
! History   : Creation                  --
! Additions : CV_Options = 4         10/06/2005     Syed RH Rizvi
!--------------------------------------------------------------------------
   implicit none
   
   type (xb_type), intent(in)           :: xb  ! First guess structure.
   type (xbx_type),intent(in)           :: xbx ! Header & non-gridded vars.
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   type (xpose_type), intent(inout)     :: xp  ! Transpose variables.
   type (x_type), intent(out)           :: xa  ! Analysis increments.
   type (be_type), intent(in), optional :: be  ! Background errors.

   integer, intent(in)  :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)  :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)  :: its,ite, jts,jte, kts,kte ! tile   dims.

   integer              :: i, j, k, l, kk            ! Loop counters.
   real                 :: sum_vphi                  ! Summing variable.

   real, dimension(ims:ime,jms:jme,kms:kme) :: phi_b ! Balanced mass incrmt

   real, dimension(kms:kme,ims:ime,jms:jme) :: phi_b_copy
   real, dimension(kms:kme,jms:jme,kms:kme) :: reg_copy
!
 if( cv_options == 2) then    ! psi, chi, p_u, q or rh 
! Transform psi and chi to u and v:

!     Communicate halo region.
      CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id1)

      CALL DA_PsiChi_To_UV(vp % v1, vp % v2, xb % coefx, &
                            xb % coefy , xa % u, xa % v, &
                            ids,ide, jds,jde, kds,kde, &
                            ims,ime, jms,jme, kms,kme, &
                            its,ite, jts,jte, kts,kte )

!     Communicate halo region.
      CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id1)

!     [2.3] Calculate phi_b from u and v:
! Initialise phi_b
      phi_b = 0.
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
 
      xa%psfc(its:ite,jts:jte)=xa%p(its:ite,jts:jte,kts)

 else if( cv_options == 4) then    ! psi, chi_u, t_u, pseudo_rh , psfc_u

! Get Full fields:        
! Temperature
   xa%t(its:ite,jts:jte,kts:kte) = vp%v3(its:ite,jts:jte,kts:kte)
   do l=kts,kte
   do k=kts,kte
   do j=jts,jte
      xa%t(its:ite,j,k) = xa%t(its:ite,j,k)  + &
                        be%reg_t(j,k,l)*vp%v1(its:ite,j,l)
   enddo
   enddo
   enddo


! Chi
   do k=kts,kte
   do j=jts,jte
     vp%v2(its:ite,j,k) = vp%v2(its:ite,j,k) + &
                       be%reg_chi(j,k)* vp%v1(its:ite,j,k)
   enddo
   enddo
  
! Surface Pressure
   xa%psfc(its:ite,jts:jte) = vp%v5(its:ite,jts:jte,1) 
   do k=kts,kte
   do j=jts,jte

! Note:
!      1. Check the unbalanced surface pressure variable used in RHS.   
!         Is it Psfc  or ln (Psfc)  ?
!      2. Check units of pressure at this stage (It should be Pascal) 
!      3. Pressure at full levels are computed are in da_transform_vtox
!         using  P = A + B * Psfc
!
   xa%psfc(its:ite,j) = xa%psfc(its:ite,j) + &
                              be%reg_ps(j,k)*vp%v1(its:ite,j,k)
   enddo
   enddo

! Transform psi and chi to u and v:

!     Communicate halo region.
      CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id1)

      CALL DA_PsiChi_To_UV(vp % v1, vp % v2, xb % coefx, &
                            xb % coefy , xa % u, xa % v, &
                            ids,ide, jds,jde, kds,kde, &
                            ims,ime, jms,jme, kms,kme, &
                            its,ite, jts,jte, kts,kte )

! Moisture variable (Pseudo RH --> Specific humidity)
!  Check the units for Pseudo humidity 
  xa % q(its:ite,jts:jte,kts:kte)  = vp%v4(its:ite,jts:jte,kts:kte) * &
                                     xb%qs(its:ite,jts:jte,kts:kte) 
elseif (cv_options == 5) then    ! psi, chi_u, t_u, pseudo_rh , psfc_u      

!  [3.1] Calculate t_b from psi

   xa % t(its:ite,jts:jte,kts:kte)=vp % v3(its:ite,jts:jte,kts:kte)

   DO kk = kts,kte
   DO k = kts,kte
   DO j = jts,jte
   DO i= its,ite
         xa % t(i,j,k) = xa % t(i,j,k) + &
                         be % reg_t(j,k,kk) * vp % v1(i,j,kk)
   END DO
   END DO
   END DO
   END DO

!  [3.2] Calculate chi_b from psi

      DO k = kts,kte
      DO j = jts,jte
      DO i= its,ite
         vp % v2(i,j,k) = vp % v2(i,j,k) + &
                          be % reg_chi(j,k) * vp % v1(i,j,k)
      END DO
      END DO
      END DO

! Surface Pressure
   xa%psfc(its:ite,jts:jte) = vp%v5(its:ite,jts:jte, 1)

!--convert from delt.ln(ps) to delt.ps
!   xa%psfc(its:ite,jts:jte) = xa%psfc(its:ite,jts:jte) &
!                            * xb%psfc(its:ite,jts:jte)
   do k=kts,kte
   do j=jts,jte
   DO i= its,ite
   xa%psfc(i,j) = xa%psfc(i,j) + &
                              be%reg_ps(j,k)*vp%v1(i,j,k)
   enddo
   enddo
   enddo

!   print '(a,e20.12)', &
!         "ps_u part  =",&
!      sqrt( sum(vp%v5(its:ite,jts:jte,1)**2)/real((ite-its+1)*(jte-jts+1)) )
!   print '(a,e20.12)',  &
!         "psfc total =",&
!      sqrt( sum(xa%psfc(its:ite,jts:jte)**2)/real((ite-its+1)*(jte-jts+1)) )

!--convert from delt.ln(ps) to delt.ps
!   xa%psfc(its:ite,jts:jte) = xa%psfc(its:ite,jts:jte) &
!                            * xb%psfc(its:ite,jts:jte)

!  [3.4] Transform psi and chi to u and v:

!  Communicate halo region.
   CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id1)


   CALL DA_PsiChi_To_UV(vp % v1, vp % v2, xb % coefx, &
                        xb % coefy , xa % u, xa % v, &
                        ids,ide, jds,jde, kds,kde, &
                        ims,ime, jms,jme, kms,kme, &
                        its,ite, jts,jte, kts,kte )

!  [3.5] treat humidity

      xa % q(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte) * &
                                        xb % qs(its:ite,jts:jte,kts:kte)
 end if

END subroutine DA_Transform_VpToX
