subroutine da_transform_vtox( xb, xbx, be, cv, vv, vp, xp, xa,        &
                              ids, ide, jds, jde, kds, kde,           &
                              ims, ime, jms, jme, kms, kme,           &
                              its, ite, jts, jte, kts, kte )

   implicit none

   type (xb_type), intent(in)           :: xb   ! first guess.
   type (xbx_type),intent(in)           :: xbx  ! For header & non-grid arrays.
   type (be_type), intent(in)           :: be   ! background errors.
   real, intent(in)                     :: cv(:)! control variable.

   type (x_type), intent(out)           :: xa   ! model space analysis (local).
   type (vp_type),intent(out)           :: vv   ! grdipt/eof cv (local).
   type (vp_type),intent(out)           :: vp   ! grdipt/level cv (local).
   type (xpose_type), intent(inout)     :: xp   ! Dimensions and xpose buffers. 

   integer, intent(in)              :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)              :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)              :: its,ite, jts,jte, kts,kte ! tile   dims

   integer :: i, j, k

   real :: sdmd, s1md, mu
   real, dimension(xb%kms:xb%kme) :: p, mr_a, mr_b

   call da_zero_x ( xa )
!-------------------------------------------------------------------------
!  [1.0]: Perform vv = u_h cv transform:
!-------------------------------------------------------------------------
   if(cv_options == 2)then 
   call da_transform_vtovv( xb, be, cv, vv, xp,         &
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

!-------------------------------------------------------------------------
!  [2.0]: Perform vp = u_v vv transform:
!-------------------------------------------------------------------------

   if ( vert_corr == 2 ) then      
      call da_vertical_transform( 'u', be, &
#ifndef DEREF_KLUDGE
                                  xb % vertical_inner_product, &
#else
                                  xb % vertical_inner_product(ims,jms,kms), &
#endif
                                  vv, vp,  &
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte )
   else
      vp % v1(its:ite,jts:jte,kts:kte) = vv % v1(its:ite,jts:jte,kts:kte)
      vp % v2(its:ite,jts:jte,kts:kte) = vv % v2(its:ite,jts:jte,kts:kte)
      vp % v3(its:ite,jts:jte,kts:kte) = vv % v3(its:ite,jts:jte,kts:kte)
      vp % v4(its:ite,jts:jte,kts:kte) = vv % v4(its:ite,jts:jte,kts:kte)
      vp % v5(its:ite,jts:jte,kts:kte) = vv % v5(its:ite,jts:jte,kts:kte)
   end if
   
!-------------------------------------------------------------------------
!  [3.0]: Perform x = u_p vp transform::
!-------------------------------------------------------------------------

   call da_transform_vptox( xb, xbx, vp, xp, xa, be,    &
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )
 else if(cv_options==3) then

   call da_apply_be( be, cv, vp, xb, xp, &
                     ids,ide, jds,jde, kds,kde,  &
                     ims,ime, jms,jme, kms,kme,  &
                     its,ite, jts,jte, kts,kte )

   call da_transform_bal( xb, vp, xa, be, xp, &
                          ids,ide, jds,jde, kds,kde,  &
                          ims,ime, jms,jme, kms,kme,  &
                          its,ite, jts,jte, kts,kte )
   do j=xb%jts,xb%jte
   do i=xb%its,xb%ite
      sdmd=0.0
      s1md=0.0
      do k=xb%kts,xb%kte
         mr_a(k) = xa%q(i,j,k)/(1.0 - xb%q(i,j,k))**2
         mr_b(k) = xb%q(i,j,k)/(1.0 - xb%q(i,j,k))

         sdmd=sdmd+mr_a(k)*xb%dnw(k)
         s1md=s1md+(1.0+mr_b(k))*xb%dnw(k)
      enddo

      mu=-(xa%psfc(i,j)+xb%psac(i,j)*sdmd)/s1md

      p(xb%kte+1)=0.0

      do k=xb%kte,xb%kts,-1
         p(k)=p(k+1)-(mu*(1.0+mr_b(k)) &
                    + xb%psac(i,j)*mr_a(k))*xb%dnw(k)

         xa%p(i,j,k)=0.5*(p(k)+p(k+1))
      enddo
   enddo
   enddo

   call DA_PT_To_Rho_Lin( xb, xa, xp )
 else
    write(unit=*, fmt='(a, i4)') &
         'Unkown cv_options:', cv_options

    CALL wrf_shutdown
    STOP 'Stopped, as cv_options not valid.'
 endif

! If Testing_3DVAR = .TRUE., not "XToY" transform needed to do here (YRG):  
  
   if (.not.Testing_3DVAR) then 
!--Exchange XA halo region.
   CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id4)

   if (sfc_assi_options == 2) then
!-----Exchange XA (surface variable) halo region.
      CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id6)
   endif

   if ( use_ssmt1obs .or. use_ssmt2obs .or. Use_GpspwObs .or. &
        Use_SsmiTbObs .or. Use_SsmiRetrievalObs .or. Use_GpsrefObs) then

!-----Now do something for PW
      call DA_Transform_XToTPW( xa, xb )

!--------GPS Refractivity: 
         if ( use_GpsrefObs ) & 
         call da_transform_XTogpsref_lin(xa, xb, xp) 

      if ( use_ssmt1obs .or. use_ssmt2obs .or. &
           Use_SsmiTbObs .or. Use_SsmiRetrievalObs ) then
         call DA_Transform_XToSeaSfcWind_Lin( xa, xb )
      endif

      if ( Use_SsmiTbObs ) call DA_Transform_XToTb_Lin (xa, xb)

!-----Exchange XA halo region.
      CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id8)
   endif

   endif

!--Compute w increments using Richardson's eqn.

   if ( Use_RadarObs ) then

        CALL DA_UVPRho_To_W_Lin( xb, xa, xp,                 &
                                 ids,ide, jds,jde, kds,kde,  &
                                 ims,ime, jms,jme, kms,kme,  &
                                 its,ite, jts,jte, kts,kte )

         do k=xp%kts,xp%kte
         do j=xp%jts,xp%jte
         do i=xp%its,xp%ite
            xa%wh(i,j,k)=0.5*(xa%w(i,j,k)+xa%w(i,j,k+1))
         enddo
         enddo
         enddo

         CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id13)

   endif

end subroutine da_transform_vtox

