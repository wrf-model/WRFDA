subroutine da_transform_vptox_adj( xb, xbx, xa, vp, be, ep, xp, &
                                   ids,ide, jds,jde, kds,kde,  &
                                   ims,ime, jms,jme, kms,kme,  &
                                   its,ite, jts,jte, kts,kte )

!--------------------------------------------------------------------------
! Purpose : Adjoint for Physical transform of variables 
!--------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)           :: xb  ! First guess structure.
   type (xbx_type),intent(in)           :: xbx ! Header & non-gridded vars.
   type (x_type), intent(inout)         :: xa  ! Grad_x(Jo)
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbation.
   type (be_type), intent(in), optional :: be  ! Background errors.
   type (xpose_type), intent(inout)     :: xp  ! Transpose variables.
   integer, intent(in)             :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)             :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)             :: its,ite, jts,jte, kts,kte ! tile   dims.

   integer         :: n, k, j, k1              ! Loop counters.

!  Pseudo RH --> Specific humidity:
   vp%v4(its:ite,jts:jte,kts:kte)  = vp%v4(its:ite,jts:jte,kts:kte) + &
                                     xa%q (its:ite,jts:jte,kts:kte) * &
                                     xb%qs(its:ite,jts:jte,kts:kte)

   IF (trace_use) CALL da_trace_entry("da_transform_vptox_adj")

                                    
   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)

!  Transform psi and chi to u and v:
   call da_psichi_to_uv_adj( xa % u, xa % v, xb % coefx,   &
                             xb % coefy, vp % v1, vp % v2, &
                             ids,ide, jds,jde, kds,kde,    &
                             ims,ime, jms,jme, kms,kme,    &
                             its,ite, jts,jte, kts,kte )


!  Surface Pressure
   do k=kts,kte
      do j=jts,jte
         vp%v1(its:ite,j,k) = vp%v1(its:ite,j,k) + &
                              be%reg_ps(j,k)*xa%psfc(its:ite,j)
      end do
   end do
   vp%v5(its:ite,jts:jte,1) = xa%psfc(its:ite,jts:jte) 

! Temperature
   do k1 = kts,kte
      do k = kts,kte
         do j = jts,jte
            vp%v1(its:ite,j,k1) = vp%v1(its:ite,j,k1) + &
                                  be%reg_t(j,k,k1)*xa%t(its:ite,j,k)
         end do
      end do
   end do
   vp%v3(its:ite,jts:jte,kts:kte) = xa%t(its:ite,jts:jte,kts:kte)

!  Chi
   do k = kts,kte
      do j = jts,jte
         vp%v1(its:ite,j,k) = vp%v1(its:ite,j,k)  + &
                              be%reg_chi(j,k)*vp%v2(its:ite,j,k)
 
      end do
   end do

!--------------------------------------------------------------------------
!  [1] Include alpha control variable:
!--------------------------------------------------------------------------

   vp % alpha(:,:,:) = 0.0

   do n = be % ne, 1, -1

      do k = kte, kts, -1
!       ps_u:

        vp % alpha(its:ite,jts:jte,n) = vp % alpha(its:ite,jts:jte,n) + &
                                        ep % v5(its:ite,jts:jte,k,n) * &
                                        vp % v5(its:ite,jts:jte,k)

!        rh:
         vp % alpha(its:ite,jts:jte,n) = vp % alpha(its:ite,jts:jte,n) + &
                                         ep % v4(its:ite,jts:jte,k,n) * &
                                         vp % v4(its:ite,jts:jte,k)

!        t_u:
         vp % alpha(its:ite,jts:jte,n) = vp % alpha(its:ite,jts:jte,n) + &
                                         ep % v3(its:ite,jts:jte,k,n) * &
                                         vp % v3(its:ite,jts:jte,k)

!        chi_u:
         vp % alpha(its:ite,jts:jte,n) = vp % alpha(its:ite,jts:jte,n) + &
                                         ep % v2(its:ite,jts:jte,k,n) * &
                                         vp % v2(its:ite,jts:jte,k)

!        psi:
         vp % alpha(its:ite,jts:jte,n) = vp % alpha(its:ite,jts:jte,n) + &
                                         ep % v1(its:ite,jts:jte,k,n) * &
                                         vp % v1(its:ite,jts:jte,k)

      end do
   end do

   IF (trace_use) CALL da_trace_entry("da_transform_vptox_adj")


end subroutine da_transform_vptox_adj

