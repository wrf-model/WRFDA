SUBROUTINE DA_Test_VXTransform( xb, xbx, xp, be, &
                                xa, vv, vp, &
                                ids,ide, jds,jde, kds,kde, &
                                ims,ime, jms,jme, kms,kme, &
                                its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Perform inverse/adjoint tests on control variable transform.
!
!  METHOD:  1) Test inverse and adjoint of physical variable transform.
!           2) Test inverse and adjoint of vertical transform.
!           3) Perform adjoint test on complete transform: <x,x> = <v_adj,v>.
!
!  HISTORY: 02/23/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE

   type (xb_type), intent(in)        :: xb    ! first guess (local).
   type (xbx_type),intent(in)        :: xbx   ! Header & non-gridded vars.
   type (xpose_type), intent(inout)  :: xp    ! Dimensions and xpose buffers.
   type (be_type), intent(in)        :: be    ! background error structure.
   type (x_type), intent(out)        :: xa    ! analysis increments (local).

   integer, intent(in)               :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)               :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)               :: its,ite, jts,jte, kts,kte ! tile   dims.

   TYPE (vp_type), intent(out)       :: vp          ! Test CV structure.
   TYPE (vp_type), intent(out)       :: vv          ! Test CV structure.

   real, dimension(1:cv_size)        :: cv          ! Test control variable.
      
!-------------------------------------------------------------------------

   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_u, xa2_v, xa2_t, &
                                                 xa2_p, xa2_q, xa2_rh, xa2_rho, &
                                                 xa2_qt, xa2_qcw, xa2_qrn
   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_w

!-------------------------------------------------------------------------

   write(unit=*, fmt='(/a/)') &
        'DA_TestVXTransform:'

   write(unit=*, fmt='(/a/)') &
        '---------------------------------------'


!--Make cv all constant value 1.0
!  cv(:) = 1.0

   call random_number(cv(:))
   cv(:) = cv(:) - 0.5

   if(cv_options == 2) then
   call da_zero_x(xa)

   call da_transform_vtox( xb, xbx, be, cv, &
                           vv, vp, xp, xa, &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte )

!-------------------------------------------------------------------------
!  [1.0]: perform initial transform to control variables:
!-------------------------------------------------------------------------

   call DA_Transform_XToVp( xb, xbx, xa, xp, vp, be, &
                           ids,ide, jds,jde, kds,kde, &
                           ims,ime, jms,jme, kms,kme, &
                           its,ite, jts,jte, kts,kte )

   if ( vert_corr == 2 ) then

!  perform vv = u_v^{-1} vp transform:

      call da_vertical_transform( 'u_inv', be, &
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

      vv % v1(its:ite,jts:jte,1:kz_vv(1)) = vp % v1(its:ite,jts:jte,1:kz_vp(1))
      vv % v2(its:ite,jts:jte,1:kz_vv(2)) = vp % v2(its:ite,jts:jte,1:kz_vp(2))
      vv % v3(its:ite,jts:jte,1:kz_vv(3)) = vp % v3(its:ite,jts:jte,1:kz_vp(3))
      vv % v4(its:ite,jts:jte,1:kz_vv(4)) = vp % v4(its:ite,jts:jte,1:kz_vp(4))
      vv % v5(its:ite,jts:jte,1:kz_vv(5)) = vp % v5(its:ite,jts:jte,1:kz_vp(5))

   end if

!-------------------------------------------------------------------------
!  [2.0] Test inverse and adjoint of physical variable transform:
!-------------------------------------------------------------------------
        
!  [2.1] Perform x = U_p v_p transform:

   xa2_u(ims:ime,jms:jme,:) = xa % u(ims:ime,jms:jme,:)
   xa2_v(ims:ime,jms:jme,:) = xa % v(ims:ime,jms:jme,:)
   xa2_w(ims:ime,jms:jme,:) = xa % w(ims:ime,jms:jme,:)
   xa2_t(ims:ime,jms:jme,:) = xa % t(ims:ime,jms:jme,:)
   xa2_p(ims:ime,jms:jme,:) = xa % p(ims:ime,jms:jme,:)
   xa2_q(ims:ime,jms:jme,:) = xa % q(ims:ime,jms:jme,:)
   xa2_rho(ims:ime,jms:jme,:) = xa % rho(ims:ime,jms:jme,:)

   IF ( cv_options_hum == 2 ) THEN
      xa2_rh(ims:ime,jms:jme,:) = xa % rh(ims:ime,jms:jme,:)
   END IF

!------------------------------------------------------------
   IF ( cv_options_hum == 3 ) THEN
      xa2_qt (ims:ime,jms:jme,:) = xa % qt (ims:ime,jms:jme,:)
      xa2_qcw(ims:ime,jms:jme,:) = xa % qcw(ims:ime,jms:jme,:)
      xa2_qrn(ims:ime,jms:jme,:) = xa % qrn(ims:ime,jms:jme,:)
   END IF
!------------------------------------------------------------

   call da_zero_x(xa)

   call da_transform_vptox( xb, xbx, vp, xp, xa, be,    &
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

!  [2.2] Test XToVpToX differences:

   write(unit=*, fmt='(/a/)') &
        'DA_Check_XToVpToX_Errors'
   
   CALL DA_Check_XToVpToX_Errors( xa, xa2_u, xa2_v, xa2_w, xa2_t, &
                               xa2_p, xa2_q, xa2_rh, xa2_rho, &
                               xa2_qt, xa2_qcw, xa2_qrn, &
                              ids,ide, jds,jde, kds,kde, &
                              ims,ime, jms,jme, kms,kme, &
                              its,ite, jts,jte, kts,kte )

!  [2.3] Perform v_{p} = U_{p}^{-1} x transform (again):

   call DA_Transform_XToVp( xb, xbx, xa, xp, vv, be, &
                            ids,ide, jds,jde, kds,kde, &
                            ims,ime, jms,jme, kms,kme, &
                            its,ite, jts,jte, kts,kte )
      
!  [2.4] Check inverse errors:

   write(unit=*, fmt='(/a/)') &
        'DA_Check_Vp_Errors'

   call DA_Check_Vp_Errors( vp, vv, its,ite, jts,jte, kts,kte )

!  [2.5] Perform adjoint tests: < vp, vp > = < vv_adj, vv >:

   call DA_Check_VvToVp_Adjoint( xb, xbx, xp, be, vp, vv, &
                                 ids,ide, jds,jde, kds,kde, &
                                 ims,ime, jms,jme, kms,kme, &
                                 its,ite, jts,jte, kts,kte )

   
!  [2.6] Test adjoint of physical variable transform:

   call DA_Check_VpToX_Adjoint( xb, xbx, be, xa, xp, vp, &
                               ids, ide, jds, jde, kds, kde,    &
                               ims, ime, jms, jme, kms, kme,    &
                               its, ite, jts, jte, kts, kte )

!-------------------------------------------------------------------------
!  [3.0]: Perform adjoint test on complete transform: <x,x> = <v_adj,v>
!-------------------------------------------------------------------------

   call vv_to_cv(vv, xp, be, cv_size, cv )

!  [4.0] Check_VToX_Adjoint:

   call DA_Check_VToX_Adjoint( xb, xbx, be, cv, vv, vp, xp, xa, &
                               ids, ide, jds, jde, kds, kde,      &
                               ims, ime, jms, jme, kms, kme,      &
                               its, ite, jts, jte, kts, kte )

!  [5.0] Check_CvToVv_Adjoint:

   call DA_Check_CvToVv_Adjoint( xb, xbx, xp, be, cv, vv, &
                               ids, ide, jds, jde, kds, kde,      &
                               ims, ime, jms, jme, kms, kme,      &
                               its, ite, jts, jte, kts, kte )

   else
   call cv_to_vv ( cv_size, cv, xp, be, vp )
   call DA_Check_VToX_Adjoint( xb, xbx, be, cv, vv, vp, xp, xa, &
                               ids, ide, jds, jde, kds, kde,      &
                               ims, ime, jms, jme, kms, kme,      &
                               its, ite, jts, jte, kts, kte )
   endif

END SUBROUTINE DA_Test_VXTransform

