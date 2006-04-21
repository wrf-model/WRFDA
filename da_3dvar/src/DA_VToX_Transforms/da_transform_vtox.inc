subroutine da_transform_vtox( cv_size, xb, xbx, be, ep, cv, vv, vp, xp, xa, &
                              ids, ide, jds, jde, kds, kde,           &
                              ims, ime, jms, jme, kms, kme,           &
                              its, ite, jts, jte, kts, kte )

!--------------------------------------------------------------------------
! Purpose   : Control variable transform x' = Uv. 
!--------------------------------------------------------------------------

   implicit none

   integer, intent(in)                  :: cv_size ! Size of cv array.
   type (xb_type), intent(in)           :: xb   ! first guess.
   type (xbx_type),intent(in)           :: xbx  ! For header & non-grid arrays.
   type (be_type), intent(in)           :: be   ! background errors.
   type (ep_type), intent(in)           :: ep   ! Ensemble perturbations.
   real, intent(in)                     :: cv(1:cv_size)   ! control variables.
   type (vp_type),intent(out)           :: vv   ! grdipt/eof cv (local).
   type (vp_type),intent(out)           :: vp   ! grdipt/level cv (local).
   type (xpose_type), intent(inout)     :: xp   ! Dimensions and xpose buffers. 
   type (x_type), intent(out)           :: xa   ! model space analysis (local).

   integer, intent(in)              :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)              :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)              :: its,ite, jts,jte, kts,kte ! tile   dims

   integer                              :: i, j, k

   real :: sdmd, s1md, mu
   real, dimension(xb%kms:xb%kme) :: p, mr_a, mr_b
   real    :: PU, PD, coeff

  IF (trace_use) CALL da_trace_entry("da_transform_vtox")


   call da_zero_x ( xa )

!-------------------------------------------------------------------------
!  [1.0]: Perform vv = u_h cv transform:
!-------------------------------------------------------------------------

   if ( global ) then
      call da_transform_vtovv_global( cv_size, xb, xbx, be, cv, vv, &
                                      ids,ide, jds,jde, kds,kde,  &
                                      ims,ime, jms,jme, kms,kme,  &
                                      its,ite, jts,jte, kts,kte ) 
   else
      call da_transform_vtovv( cv_size, xb, be, cv, vv, xp,         &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )
   end if

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

   if ( be % ne > 0 ) then
      vp % alpha(its:ite,jts:jte,1:be%ne) = vv % alpha(its:ite,jts:jte,1:be%ne)
   end if

!-------------------------------------------------------------------------
!  [3.0]: Perform x = u_p vp transform::
!-------------------------------------------------------------------------

   call da_transform_vptox( xb, xbx, vp, xp, xa, be, ep,   &
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

!-------------------------------------------------------------------------
!  [4.0]: Move the following:
!-------------------------------------------------------------------------

   do j=xb%jts,xb%jte
   do i=xb%its,xb%ite
      if (fg_format == 1) then
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
      else if (fg_format == 3) then
         do k=xb%kts,xb%kte
            if( k == xb%kte) then
               coeff = xb%KMA_B(K)/(xb%KMA_A(K)+xb%KMA_B(K)*xb%psfc(I,J)/100.0)
            else
               PU = xb%KMA_A(K+1) + xb%KMA_B(K+1)*xb%psfc(I,J)/100.0
               PD = xb%KMA_A(K  ) + xb%KMA_B(K  )*xb%psfc(I,J)/100.0
               coeff=xb%KMA_B(K)  *1.0/(PD-PU)**2*(-PU*(LOG(PD)-LOG(PU))+PD-PU)&
                 + xb%KMA_B(K+1)*1.0/(PD-PU)**2*( PD*(LOG(PD)-LOG(PU))-PD+PU)
            end if
!           Here since xa%psfc holds value in Pa. dlnp -> dp
            xa%p(i,j,k) =  xb%p(i,j,k) * xa%psfc(I,J)/100. * coeff

         end do
      end if
   enddo
   enddo

   call DA_PT_To_Rho_Lin( xb, xa, xp )
 
! If Testing_3DVAR = .TRUE., not "XToY" transform needed to do here (YRG):
!
   if (.not.testing_wrfvar) then
!--Exchange XA halo region.
   CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id4)

   if (sfc_assi_options == 2) then
      call DA_Transform_XToWTQ ( xp, xb, xa )
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
         if( global ) then
            CALL wrf_error_fatal3(__FILE__,__LINE__, &
            "xb%speed is not available, see da_transfer_kmatoxb.inc")
         end if
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
!---------------------------------------------------------------
!  Polar treatment for Global 
!---------------------------------------------------------------
   if( global )  then   
#ifndef DEREF_KLUDGE
    call DA_Get_VPoles(xa%u,xa%v,xb%lon, &
          ids, ide, jds, jde, kds, kde,  &
          ims, ime, jms, jme, kms, kme,  &
          its, ite, jts, jte, kts, kte   )
    call DA_Get_SPoles(xa%t,             &
          ids, ide, jds, jde, kds, kde,  &
          ims, ime, jms, jme, kms, kme,  &
          its, ite, jts, jte, kts, kte   )
    call DA_Get_SPoles(xa%p,             &
          ids, ide, jds, jde, kds, kde,  &
          ims, ime, jms, jme, kms, kme,  &
          its, ite, jts, jte, kts, kte   )
    call DA_Get_SPoles(xa%q,             &
          ids, ide, jds, jde, kds, kde,  &
          ims, ime, jms, jme, kms, kme,  &
          its, ite, jts, jte, kts, kte   )
    call DA_Get_SPoles(xa%psfc,          &
          ids, ide, jds, jde,   1,   1,  &
          ims, ime, jms, jme,   1,   1,  &
          its, ite, jts, jte,   1,   1   )
#else
    call DA_Get_VPoles(xa%u(ims,jms,kms),          &
                       xa%v(ims,jms,kms),          &
                       xb%lon(ims,jms),            &
                    ids, ide, jds, jde, kds, kde,  &
                    ims, ime, jms, jme, kms, kme,  &
                    its, ite, jts, jte, kts, kte   )
    call DA_Get_SPoles(xa%t(ims,jms,kms),          &
                    ids, ide, jds, jde, kds, kde,  &
                    ims, ime, jms, jme, kms, kme,  &
                    its, ite, jts, jte, kts, kte   )
    call DA_Get_SPoles(xa%p(ims,jms,kms),          &
                    ids, ide, jds, jde, kds, kde,  &
                    ims, ime, jms, jme, kms, kme,  &
                    its, ite, jts, jte, kts, kte   )
    call DA_Get_SPoles(xa%q(ims,jms,kms),          &
                    ids, ide, jds, jde, kds, kde,  &
                    ims, ime, jms, jme, kms, kme,  &
                    its, ite, jts, jte, kts, kte   )
    call DA_Get_SPoles(xa%psfc(ims,jms),           &
                    ids, ide, jds, jde,   1,   1,  &
                    ims, ime, jms, jme,   1,   1,  &
                    its, ite, jts, jte,   1,   1   )
#endif
    call DA_Set_Boundary_Xa(xa, xb)
   end if   

  IF (trace_use) CALL da_trace_exit("da_transform_vtox")


end subroutine da_transform_vtox

