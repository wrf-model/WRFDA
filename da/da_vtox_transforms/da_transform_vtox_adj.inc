subroutine da_transform_vtox_adj(cv_size, xb, xbx, be, ep, xa, xp, vp, vv, cv)

   !--------------------------------------------------------------------------
   ! Purpose: Control variable transform Adjoint
   !--------------------------------------------------------------------------

   implicit none

   integer, intent(in)              :: cv_size ! Size of cv array.
   type (xb_type), intent(in)       :: xb   ! first guess.
   type (xbx_type),intent(in)       :: xbx  ! For header & non-grid arrays.
   type (be_type), intent(in)       :: be   ! background errors.
   type (ep_type), intent(in)       :: ep   ! ensemble perturbation structure.
   type (x_type), intent(inout)     :: xa   ! grad_x(jo)
   type (xpose_type), intent(inout) :: xp   ! Dimensions and xpose buffers. 
   type (vp_type),intent(out)       :: vp   ! grdipt/level cv (local).
   type (vp_type),intent(out)       :: vv   ! grdipt/eof cv (local).
   real, intent(inout)              :: cv(1:cv_size) ! control variables.

   integer                          :: i, j, k
   real :: sdmd, s1md, mu
   real, dimension(xb%kms:xb%kme) :: p, mr_a, mr_b
   real    :: PU, PD, coeff

  if (trace_use) call da_trace_entry("da_transform_vtox_adj")

   !-------------------------------------------------------------------------
   ! Polar treatment for Global
   !-------------------------------------------------------------------------

   if (global) then     
      ! Poles treatment for global WRFVAR
#ifndef DEREF_KLUDGE
      call da_get_avpoles(xa%u,xa%v,&
            ids, ide, jds, jde,  &
            ims, ime, jms, jme, kms, kme,  &
            its, ite, jts, jte, kts, kte  )
      call da_get_aspoles(xa%t,            &
            ids, ide, jds, jde,  &
            ims, ime, jms, jme, kms, kme,  &
            its, ite, jts, jte, kts, kte  )
      call da_get_aspoles(xa%p,            &
            ids, ide, jds, jde,  &
            ims, ime, jms, jme, kms, kme,  &
            its, ite, jts, jte, kts, kte  )
      call da_get_aspoles(xa%q,            &
            ids, ide, jds, jde,  &
            ims, ime, jms, jme, kms, kme,  &
            its, ite, jts, jte, kts, kte  )
      call da_get_aspoles(xa%psfc,         &
            ids, ide, jds, jde,  &
            ims, ime, jms, jme,   1,   1,  &
            its, ite, jts, jte,   1,   1  )
#else
      call da_get_avpoles(xa%u(ims,jms,kms),         &
                          xa%v(ims,jms,kms),         &
                      ids, ide, jds, jde,  &
                      ims, ime, jms, jme, kms, kme,  &
                      its, ite, jts, jte, kts, kte  )
      call da_get_aspoles(xa%t(ims,jms,kms),         &
                      ids, ide, jds, jde,   &
                      ims, ime, jms, jme, kms, kme,  &
                      its, ite, jts, jte, kts, kte  )
      call da_get_aspoles(xa%p(ims,jms,kms),         &
                      ids, ide, jds, jde,  &
                      ims, ime, jms, jme, kms, kme,  &
                      its, ite, jts, jte, kts, kte  )
      call da_get_aspoles(xa%q(ims,jms,kms),         &
                      ids, ide, jds, jde,  &
                      ims, ime, jms, jme, kms, kme,  &
                      its, ite, jts, jte, kts, kte  )
      call da_get_aspoles(xa%psfc(ims,jms),          &
                      ids, ide, jds, jde,  &
                      ims, ime, jms, jme,   1,   1,  &
                      its, ite, jts, jte,   1,   1  )
#endif
   end if     

   ! Compute w increments using Richardson's eqn.
   if (Use_RadarObs) then

      do k=xp%kts,xp%kte
         do j=xp%jts,xp%jte
            do i=xp%its,xp%ite
               xa%w(i,j,k)=xa%w(i,j,k)+0.5*xa%wh(i,j,k)
               xa%w(i,j,k+1)=xa%w(i,j,k+1)+0.5*xa%wh(i,j,k)
               xa%wh(i,j,k)=0.
            end do
         end do
      end do

     call da_uvprho_to_w_adj(xb, xa, xp)

   end if

   !-------------------------------------------------------------------------
   ! If Testing_wrfvar = .true., not "XToY" transform needed to do here (YRG):

   if (.not.testing_wrfvar) then

      if (use_ssmt1obs .or. use_ssmt2obs .or. Use_GpspwObs .or. &
           Use_SsmiTbObs .or. Use_SsmiRetrievalObs .or. use_GpsrefObs) then

         if (use_ssmiTbobs) call da_transform_xtotb_adj(xa, xb)

         if (use_ssmt1obs .or. use_ssmt2obs .or. &
              Use_SsmiTbObs .or. Use_SsmiRetrievalObs) then
         if (global) then
            call da_error(__FILE__,__LINE__, &
               (/"xb%speed is not available, see da_transfer_kmatoxb.inc"/))
         end if
            call da_transform_xtoseasfcwind_adj(xa, xb)
         end if

         ! GPS Refractivity:
         if (use_GpsrefObs) &
            call da_transform_xtogpsref_adj(xa, xb, xp)

         ! Now for PW.
         call da_transform_xtotpw_adj(xa, xb)

      end if

      if (sfc_assi_options == 2) call da_transform_xtowtq_adj(xp, xb, xa)

   end if
   !-------------------------------------------------------------------------
 
   call da_pt_to_rho_adj(xb, xa)
   do j=xb%jts,xb%jte
      do i=xb%its,xb%ite
         if (fg_format == fg_format_wrf) then
            mu=0.0
            s1md=0.0

            p(:)=0.0

            do k=xb%kts,xb%kte
               mr_b(k) = xb%q(i,j,k)/(1.0 - xb%q(i,j,k))
               s1md=s1md+(1.0+mr_b(k))*xb%dnw(k)

               p(k) = p(k) + 0.5*xa%p(i,j,k)
               p(k+1) = p(k+1) + 0.5*xa%p(i,j,k)

               mu = mu - p(k)*(1.0+mr_b(k))*xb%dnw(k)

               mr_a(k) = - p(k)*xb%psac(i,j)*xb%dnw(k)
               p(k+1) = p(k+1) + p(k)
            end do

            xa%psfc(i,j) = xa%psfc(i,j) - mu/s1md
            sdmd=-mu*xb%psac(i,j)/s1md

            do k=xb%kts,xb%kte
               mr_a(k) = mr_a(k) + sdmd*xb%dnw(k)
               xa%q(i,j,k) = xa%q(i,j,k) + mr_a(k)/(1.0 - xb%q(i,j,k))**2
            end do
         else if (fg_format == fg_format_kma)then
            do k=xb%kts,xb%kte
               if (k == xb%kte) then
                  coeff = xb%KMA_B(K)/(xb%KMA_A(K)+xb%KMA_B(K)* &
                     xb%psfc(I,J)/100.0)
               else
                  PU = xb%KMA_A(K+1) + xb%KMA_B(K+1)*xb%psfc(I,J)/100.0
                  PD = xb%KMA_A(K ) + xb%KMA_B(K )*xb%psfc(I,J)/100.0
                  coeff=xb%KMA_B(K)*1.0/(PD-PU)**2*(-PU*(LOG(PD)-LOG(PU))+PD-PU)&
                     + xb%KMA_B(K+1)*1.0/(PD-PU)**2*(PD*(LOG(PD)-LOG(PU))-PD+PU)
               end if
      
               xa%psfc(i,j) = xa % psfc(i,j) + &
                 xb%p(i,j,k) * xa % p(i,j,k)/100.0 * coeff 
            end do
         end if
      end do
   end do

   if (global) then     
      call da_set_boundary_xa(xa, xb)
    end if

   !-------------------------------------------------------------------------
   ! [3.0]: Perform x = u_p vp transform::
   !-------------------------------------------------------------------------

   call da_zero_vp_type (vp)
   call da_transform_vptox_adj(xb, xbx, xa, vp, be, ep, xp)

   !-------------------------------------------------------------------------
   ! [2.0]: Perform vp = u_v vv transform:
   !-------------------------------------------------------------------------

   call da_zero_vp_type (vv)

   ! Uv for alpha is a null transform:
   ! if (be % ne > 0) then
   !    vv % alpha(its:ite,jts:jte,1:be%ne) = &
   !    vp % alpha(its:ite,jts:jte,1:be%ne) 
   ! end if

   if (vert_corr == 2) then      
      call da_vertical_transform('u_adj', be, &
#ifndef DEREF_KLUDGE
                                  xb % vertical_inner_product, &
#else
                                  xb % vertical_inner_product(ims,jms,kms), &
#endif
                                  vv, vp)
   else
      vv % v1(its:ite,jts:jte,kts:kte) = vp % v1(its:ite,jts:jte,kts:kte)
      vv % v2(its:ite,jts:jte,kts:kte) = vp % v2(its:ite,jts:jte,kts:kte)
      vv % v3(its:ite,jts:jte,kts:kte) = vp % v3(its:ite,jts:jte,kts:kte)
      vv % v4(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte)
      vv % v5(its:ite,jts:jte,kts:kte) = vp % v5(its:ite,jts:jte,kts:kte)
   end if

   !-------------------------------------------------------------------------
   ! [1.0]: perform vv = u_h cv transform:
   !-------------------------------------------------------------------------

   if (global) then
      call da_transform_vtovv_global_adj(cv_size, xb, xbx, be, cv, vv)
   else
      call da_transform_vtovv_adj(cv_size, xb, be, cv, vv, xp)
   end if

   if (trace_use) call da_trace_exit("da_transform_vtox_adj")

end subroutine da_transform_vtox_adj


