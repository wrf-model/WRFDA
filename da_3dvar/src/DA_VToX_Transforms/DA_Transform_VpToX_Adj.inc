subroutine DA_Transform_VpToX_Adj( xb, xbx, xa, vp, be, xp, &
                                   ids,ide, jds,jde, kds,kde,  &
                                   ims,ime, jms,jme, kms,kme,  &
                                   its,ite, jts,jte, kts,kte )
!--------------------------------------------------------------------------
! Purpose : Adjoint for Physical transform of variables 
!
! History   : Creation                  --
! Additions : CV_Options = 4         10/06/2005     Syed RH Rizvi
!--------------------------------------------------------------------------
   implicit none

   type (xb_type), intent(in)           :: xb  ! First guess structure.
   type (xbx_type),intent(in)           :: xbx ! Header & non-gridded vars.
   type (x_type), intent(inout)         :: xa  ! Grad_x(Jo)
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   type (be_type), intent(in), optional :: be  ! Background errors.
   type (xpose_type), intent(inout)     :: xp  ! Transpose variables.
   integer, intent(in)             :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)             :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)             :: its,ite, jts,jte, kts,kte ! tile   dims.

   integer         :: i, j, k, l,  kk         ! Loop counters.
   real            :: sum_v_p                 ! Summing variable.
   real, dimension(ims:ime,jms:jme,kms:kme) :: phi_b ! Balanced mass increment.

   real, dimension(kms:kme,ims:ime,jms:jme) :: p_copy
   real, dimension(kms:kme,jms:jme,kms:kme) :: reg_copy

 if( cv_options == 2 ) then
!--surface pressure increments

      xa%p(its:ite,jts:jte,kts) = xa%p(its:ite,jts:jte,kts) &
                                + xa%psfc(its:ite,jts:jte)
      xa%psfc(its:ite,jts:jte) = 0.0

!    [2.8] Ground temperature control variable:

!     vp % v5(its:ite,jts:jte) = vp % v5(its:ite,jts:jte) + &
!                                xa % tgrn(its:ite,jts:jte)
    
!     [2.7] Choice of moisture control variable:
      
      IF ( cv_options_hum == 1 ) THEN

         vp % v4(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte)+&
                                            xa % q(its:ite,jts:jte,kts:kte)
  
      ELSE IF ( cv_options_hum == 2 ) THEN
      
         CALL DA_TPRH_To_Q_Adj( xb, xp, xa )

         vp % v4(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte)+&
                                            xa % rh(its:ite,jts:jte,kts:kte)

      ELSE IF ( cv_options_hum == 3 ) THEN

!    [2.10] Partition of hydrometeor increments via warm rain process

         CALL DA_Moist_Phys_Adj( xb, xa, xp,                 &
                                 ids,ide, jds,jde, kds,kde,  &
                                 ims,ime, jms,jme, kms,kme,  &
                                 its,ite, jts,jte, kts,kte)

         vp % v4(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte) + &
                                            xa % qt(its:ite,jts:jte,kts:kte)


      END IF
!     [2.6] Compute temperature increments from p/rho increments:
               
      CALL DA_PRho_To_T_Adj( xb, xp, xa )

!     [2.5] Compute density increments using hydrostatic eqn:

      CALL DA_HydrostaticP_To_Rho_Adj( xb, xp, xa % rho, xa % p )
      
!     [2.4] Compute phi:

      IF ( PRESENT(be) ) THEN

         DO k = kts, kte ! Need copies for all model levels for sum:
            reg_copy(k,jts:jte,kts:kte) = be % pb_vert_reg(jts:jte,k,kts:kte)
            p_copy(k,its:ite,jts:jte) = xa % p(its:ite,jts:jte,k)
         END DO

         DO k = kts, kte
            DO j = jts, jte
               DO i= its, ite
!                 SUM OVER ALL LEVELS (kts:kte):
                  sum_v_p = reg_copy(kts,j,k) * p_copy(kts,i,j)
                  DO kk = kts+1, kte ! SUM OVER ALL LEVELS
                     sum_v_p= sum_v_p + reg_copy(kk,j,k) * p_copy(kk,i,j)
                  END DO
                  phi_b(i,j,k) = sum_v_p
               END DO
            END DO
         END DO

      ELSE
         phi_b(its:ite,jts:jte,kts:kte) = xa % p(its:ite,jts:jte,kts:kte)
      END IF
      vp % v3(its:ite,jts:jte,kts:kte) = vp % v3(its:ite,jts:jte,kts:kte) + &
                                         xa % p(its:ite,jts:jte,kts:kte)

!     [2.3] Calculate phi_b from u and v:

      CALL DA_Balance_Equation_Adj( xb, xbx, xp, phi_b, xa % u, xa % v, &
                                    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte )

      CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)
!     [2.2] Transform psi and chi to u and v:
         
     CALL DA_PsiChi_To_UV_Adj( xa % u, xa % v, xb % coefx,   &
                               xb % coefy, vp % v1, vp % v2, &
                               ids,ide, jds,jde, kds,kde,    &
                               ims,ime, jms,jme, kms,kme,    &
                               its,ite, jts,jte, kts,kte )
 else if( cv_options == 4) then    ! psi, chi_u, t_u, pseudo_rh , psfc_u
  vp%v4(its:ite,jts:jte,kts:kte)  = vp%v4(its:ite,jts:jte,kts:kte) + &
                                    xa%q (its:ite,jts:jte,kts:kte) * &
                                    xb%qs(its:ite,jts:jte,kts:kte)
                                    
      CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)

!     [2.2] Transform psi and chi to u and v:
      CALL DA_PsiChi_To_UV_Adj( xa % u, xa % v, xb % coefx,   &
                                xb % coefy, vp % v1, vp % v2, &
                                ids,ide, jds,jde, kds,kde,    &
                                ims,ime, jms,jme, kms,kme,    &
                                its,ite, jts,jte, kts,kte )


! Surface Pressure
   do k=kts,kte
   do j=jts,jte
      vp%v1(its:ite,j,k)   = vp%v1(its:ite,j,k) + &
                          be%reg_ps(j,k)*xa%psfc(its:ite,j)
   enddo
   enddo
   vp%v5(its:ite,jts:jte,1) = xa%psfc(its:ite,jts:jte) 

! Chi
   do k=kts,kte
   do j=jts,jte
     vp%v1(its:ite,j,k) = vp%v1(its:ite,j,k)  + &
                       be%reg_chi(j,k)*vp%v2(its:ite,j,k)
 
   enddo
   enddo

! Temperature
   do l=kts,kte
   do k=kts,kte
   do j=jts,jte
       vp%v1(its:ite,j,l) = vp%v1(its:ite,j,l) + &
                        be%reg_t(j,k,l)*xa%t(its:ite,j,k)
   enddo
   enddo
   enddo

   vp%v3(its:ite,jts:jte,kts:kte) = xa%t(its:ite,jts:jte,kts:kte)

elseif( cv_options == 5) then    ! psi, chi_u, t_u, pseudo_rh , psfc_u

!  [3.5] treat humidity
      vp % v4(its:ite,jts:jte,kts:kte) = xa % q(its:ite,jts:jte,kts:kte) &
                                       * xb % qs(its:ite,jts:jte,kts:kte)

!  [3.4]  adj of Transform psi and chi to u and v:

   CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)

!  vp%v1(:,:,:)=0.
!  vp%v2(:,:,:)=0.
         
   CALL DA_PsiChi_To_UV_Adj( xa % u, xa % v, xb % coefx,   &
                             xb % coefy, vp % v1, vp % v2, &
                             ids,ide, jds,jde, kds,kde,    &
                             ims,ime, jms,jme, kms,kme,    &
                             its,ite, jts,jte, kts,kte )

!  [3.3]  adj Calculate Psfc_b from psi

    DO k = kts,kte
    DO j = jts,jte
    DO i= its,ite
       vp % v1(i,j,k)= vp % v1(i,j,k)+ &
                        be % reg_ps(j,k) * xa % psfc(i,j)
    END DO
    END DO
    END DO

   vp % v5(its:ite,jts:jte,1)=xa % psfc(its:ite,jts:jte)

!  [3.2] adj Calculate chi_b from psi

    DO k = kts,kte
     DO j = jts,jte
      DO i= its,ite
         vp % v1(i,j,k)= vp % v1(i,j,k)+ &
                         be % reg_chi(j,k) * vp % v2(i,j,k)
      END DO
     END DO
    END DO

!  [3.1] Calculate t_b from psi

   DO kk = kts,kte
    DO k = kts,kte
     DO j = jts,jte
      DO i= its,ite
         vp % v1(i,j,kk)= vp % v1(i,j,kk)+ &
                          be % reg_t(j,k,kk) * xa % t(i,j,k)
      END DO
     END DO
    END DO
   END DO

   vp % v3(its:ite,jts:jte,kts:kte)= xa % t(its:ite,jts:jte,kts:kte)
 end if

END subroutine DA_Transform_VpToX_Adj
