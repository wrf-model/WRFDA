subroutine da_transform_vtox_adj( xb, xbx, be, xa, xp, vp, vv, cv, &
                                  ids, ide, jds, jde, kds, kde,           &
                                  ims, ime, jms, jme, kms, kme,           &
                                  its, ite, jts, jte, kts, kte )
   implicit none

   type (xb_type), intent(in)       :: xb   ! first guess.
   TYPE (xbx_type),intent(in)       :: xbx  ! For header & non-grid arrays.
   type (be_type), intent(in)       :: be   ! background errors.
   type (x_type), intent(inout)     :: xa   ! grad_x(jo)
   type (xpose_type), intent(inout) :: xp   ! Dimensions and xpose buffers. 
   type (vp_type),intent(out)       :: vp   ! grdipt/level cv (local).
   type (vp_type),intent(out)       :: vv   ! grdipt/eof cv (local).
   real, intent(inout)              :: cv(:)! grad_v(jo)
   integer, intent(in)              :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)              :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)              :: its,ite, jts,jte, kts,kte ! tile   dims

   integer :: i, j, k
   real :: sdmd, s1md, mu
   real, dimension(xb%kms:xb%kme) :: p, mr_a, mr_b

!-------------------------------------------------------------------------

!--Compute w increments using Richardson's eqn.

     if ( Use_RadarObs ) then

        do k=xp%kts,xp%kte
        do j=xp%jts,xp%jte
        do i=xp%its,xp%ite
           xa%w(i,j,k)=xa%w(i,j,k)+0.5*xa%wh(i,j,k)
           xa%w(i,j,k+1)=xa%w(i,j,k+1)+0.5*xa%wh(i,j,k)
           xa%wh(i,j,k)=0.
        enddo
        enddo
        enddo

       CALL DA_UVPRho_To_W_Adj( xb, xa, xp,                 &
                                ids,ide, jds,jde, kds,kde,  &
                                ims,ime, jms,jme, kms,kme,  &
                                its,ite, jts,jte, kts,kte)

     endif

!-------------------------------------------------------------------------

!--If Testing_3DVAR = .TRUE., not "XToY" transform needed to do here (YRG): 
 
   if (.not.Testing_3DVAR) then 

      if ( use_ssmt1obs .or. use_ssmt2obs .or. Use_GpspwObs .or. &
           Use_SsmiTbObs .or. Use_SsmiRetrievalObs .or. use_GpsrefObs) then

         if ( use_ssmiTbobs) call DA_Transform_XToTb_Adj( xa, xb)
   
         if ( use_ssmt1obs .or. use_ssmt2obs .or. &
              Use_SsmiTbObs .or. Use_SsmiRetrievalObs ) then
            call DA_Transform_XToSeaSfcWind_Adj( xa, xb )
         endif

!--------GPS Refractivity: 
         if ( use_GpsrefObs ) & 
            call da_transform_XTogpsref_adj(xa, xb, xp) 

!--------Now for PW.
         call DA_Transform_XToTPW_ADJ( xa, xb )
      endif
   endif

!-------------------------------------------------------------------------

 if(cv_options == 2)then

!-------------------------------------------------------------------------
!  [3.0]: Perform x = u_p vp transform::
!-------------------------------------------------------------------------

   call DA_Zero_vp_type ( vp )
   call da_transform_vptox_adj( xb, xbx, xa, vp, be, xp, &
                                ids,ide, jds,jde, kds,kde,  &
                                ims,ime, jms,jme, kms,kme,  &
                                its,ite, jts,jte, kts,kte )

!-------------------------------------------------------------------------
!  [2.0]: Perform vp = u_v vv transform:
!-------------------------------------------------------------------------

   call DA_Zero_vp_type ( vv )
   if ( vert_corr == 2 ) then      
      call da_vertical_transform( 'u_adj', be, &
#ifndef DEREF_KLUDGE
                                  xb % vertical_inner_product, &
#else
                                  xb % vertical_inner_product(ims,jms,kms), &
#endif
                                  vv, vp, &
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte )
   else
      vv % v1(its:ite,jts:jte,kts:kte) = vp % v1(its:ite,jts:jte,kts:kte)
      vv % v2(its:ite,jts:jte,kts:kte) = vp % v2(its:ite,jts:jte,kts:kte)
      vv % v3(its:ite,jts:jte,kts:kte) = vp % v3(its:ite,jts:jte,kts:kte)
      vv % v4(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte)
      vv % v5(its:ite,jts:jte,kts:kte) = vp % v5(its:ite,jts:jte,kts:kte)
   end if
   
!-------------------------------------------------------------------------
!  [1.0]: perform vv = u_h cv transform:
!-------------------------------------------------------------------------

   call da_transform_vtovv_adj( xb, be, vv, cv, xp, &
                                ids,ide, jds,jde, kds,kde,  &
                                ims,ime, jms,jme, kms,kme,  &
                                its,ite, jts,jte, kts,kte )

 else if(cv_options==3)then
   call DA_PT_To_Rho_Adj( xb, xa )

   do j=xb%jts,xb%jte
   do i=xb%its,xb%ite
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
      enddo

      xa%psfc(i,j) = xa%psfc(i,j) - mu/s1md
      sdmd=-mu*xb%psac(i,j)/s1md

      do k=xb%kts,xb%kte
         mr_a(k) = mr_a(k) + sdmd*xb%dnw(k)
         xa%q(i,j,k) = xa%q(i,j,k) + mr_a(k)/(1.0 - xb%q(i,j,k))**2
      enddo
   enddo
   enddo

   call da_transform_bal_adj( xb, vp, xa, be,xp,&
                                ids,ide, jds,jde, kds,kde,  &
                                ims,ime, jms,jme, kms,kme,  &
                                its,ite, jts,jte, kts,kte )



   call da_apply_be_adj( be, cv, vp ,xb,xp,&
                                ids,ide, jds,jde, kds,kde,  &
                                ims,ime, jms,jme, kms,kme,  &
                                its,ite, jts,jte, kts,kte )

 endif

end subroutine da_transform_vtox_adj

