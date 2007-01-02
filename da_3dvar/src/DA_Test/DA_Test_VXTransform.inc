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
!           01/30/2005 - Added Spectral transform test      Syed RH Rizvi 
!           02/04/2005 - Added VvToVpToVv Test              Syed RH Rizvi
!           02/04/2005 - Added VpToVvToVp Test              Syed RH Rizvi
!           08/02/2005 - Inverse tests is done only for
!                        cv_options =5 and                  Syed RH Rizvi
!                        single processor run               
!
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
   integer            :: i   
   TYPE (vp_type)                    :: vp1, vv1    ! Test CV structure.
   real, dimension(ims:ime, jms:jme) :: field
!-------------------------------------------------------------------------

   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_u, xa2_v, xa2_t, &
                                                 xa2_p, xa2_q, xa2_rh, xa2_rho, &
                                                 xa2_qt, xa2_qcw, xa2_qrn
   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_w

!-------------------------------------------------------------------------
 ALLOCATE (vp1 % v1(ims:ime,jms:jme,kms:kme))
 ALLOCATE (vp1 % v2(ims:ime,jms:jme,kms:kme))
 ALLOCATE (vp1 % v3(ims:ime,jms:jme,kms:kme))
 ALLOCATE (vp1 % v4(ims:ime,jms:jme,kms:kme))
 ALLOCATE (vp1 % v5(ims:ime,jms:jme,kms:kme))

 ALLOCATE (vv1 % v1(ims:ime,jms:jme,1:kz_vv(1)))
 ALLOCATE (vv1 % v2(ims:ime,jms:jte,1:kz_vv(2)))
 ALLOCATE (vv1 % v3(ims:ime,jms:jme,1:kz_vv(3)))
 ALLOCATE (vv1 % v4(ims:ime,jms:jme,1:kz_vv(4)))
 ALLOCATE (vv1 % v5(ims:ime,jms:jme,1:kz_vv(5)))

   call da_zero_vp_type(vp1)
   call da_zero_vp_type(vv1)

   write(unit=*, fmt='(/a/)') &
        'DA_TestVXTransform:'

   write(unit=*, fmt='(/a/)') &
        '---------------------------------------'
!--Make cv all constant value 1.0

   call random_number(cv(:))
   cv(:) = cv(:) - 0.5
   call da_zero_x(xa)
   
   if ( cv_options /= 5) then
   write(unit=*, fmt='(/a,i4,a)') &
         ' Inverse tests for this cv_options= ',cv_options,' will not be done'
   go to 1111
   end if

#ifdef DM_PARALLEL
   write(unit=*, fmt='(/a/)') &
         ' Inverse tests will not be done as it is MPP run'
#else
   write(unit=*, fmt='(/a/)') &
         ' Inverse tests follows:'
   call da_transform_vtox( xb, xbx, be, cv, &
                           vv, vp, xp, xa, &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte )

!-------------------------------------------------------------------------
! Store xa, Vv & Vp for inverse test
!-------------------------------------------------------------------------
   xa2_u(ims:ime,jms:jme,:) = xa % u(ims:ime,jms:jme,:)
   xa2_v(ims:ime,jms:jme,:) = xa % v(ims:ime,jms:jme,:)
   xa2_w(ims:ime,jms:jme,:) = xa % w(ims:ime,jms:jme,:)
   xa2_t(ims:ime,jms:jme,:) = xa % t(ims:ime,jms:jme,:)
   xa2_p(ims:ime,jms:jme,:) = xa % p(ims:ime,jms:jme,:)
   xa2_q(ims:ime,jms:jme,:) = xa % q(ims:ime,jms:jme,:)
   xa2_rho(ims:ime,jms:jme,:) = xa % rho(ims:ime,jms:jme,:)

!------------------------------------------------------------
   IF ( cv_options_hum == 2 ) THEN
      xa2_rh(ims:ime,jms:jme,:) = xa % rh(ims:ime,jms:jme,:)
   END IF
   IF ( cv_options_hum == 3 ) THEN
      xa2_qt (ims:ime,jms:jme,:) = xa % qt (ims:ime,jms:jme,:)
      xa2_qcw(ims:ime,jms:jme,:) = xa % qcw(ims:ime,jms:jme,:)
      xa2_qrn(ims:ime,jms:jme,:) = xa % qrn(ims:ime,jms:jme,:)
   END IF
!------------------------------------------------------------

      vv1 % v1(its:ite,jts:jte,1:kz_vv(1)) = vv % v1(its:ite,jts:jte,1:kz_vv(1))
      vv1 % v2(its:ite,jts:jte,1:kz_vv(2)) = vv % v2(its:ite,jts:jte,1:kz_vv(2))
      vv1 % v3(its:ite,jts:jte,1:kz_vv(3)) = vv % v3(its:ite,jts:jte,1:kz_vv(3))
      vv1 % v4(its:ite,jts:jte,1:kz_vv(4)) = vv % v4(its:ite,jts:jte,1:kz_vv(4))
      vv1 % v5(its:ite,jts:jte,1:kz_vv(5)) = vv % v5(its:ite,jts:jte,1:kz_vv(5))

      vp1 % v1(its:ite,jts:jte,1:kz_vp(1)) = vp % v1(its:ite,jts:jte,1:kz_vp(1))
      vp1 % v2(its:ite,jts:jte,1:kz_vp(2)) = vp % v2(its:ite,jts:jte,1:kz_vp(2))
      vp1 % v3(its:ite,jts:jte,1:kz_vp(3)) = vp % v3(its:ite,jts:jte,1:kz_vp(3))
      vp1 % v4(its:ite,jts:jte,1:kz_vp(4)) = vp % v4(its:ite,jts:jte,1:kz_vp(4))
      vp1 % v5(its:ite,jts:jte,1:kz_vp(5)) = vp % v5(its:ite,jts:jte,1:kz_vp(5))
!-------------------------------------------------------------------------
!  [1.0]: Perform VvToVpToVv test:                        
!-------------------------------------------------------------------------
!   call DA_Transform_XToVp( xb, xbx, xa, xp, vp, be, &
!                           ids,ide, jds,jde, kds,kde, &
!                           ims,ime, jms,jme, kms,kme, &
!                           its,ite, jts,jte, kts,kte )

!-------------------------------------------------------------------------
   if ( vert_corr == 2 ) then
!  perform vv = u_v^{-1} vp transform:
!-------------------------------------------------------------------------
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
   write(unit=*, fmt='(/a/)') &
        'DA_Check_VvToVpToVv_Errors'

   call DA_Check_Vp_Errors( vv1, vv, kz_vv, its,ite, jts,jte, kts,kte )

!-------------------------------------------------------------------------
!  [2.0]: Perform VpToVvToVp test:                        
!-------------------------------------------------------------------------
   if ( vert_corr == 2 ) then
!  perform vp = u_v (vv) transform:
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

      vp % v1(its:ite,jts:jte,1:kz_vv(1)) = vv % v1(its:ite,jts:jte,1:kz_vv(1))
      vp % v2(its:ite,jts:jte,1:kz_vv(2)) = vv % v2(its:ite,jts:jte,1:kz_vv(2))
      vp % v3(its:ite,jts:jte,1:kz_vv(3)) = vv % v3(its:ite,jts:jte,1:kz_vv(3))
      vp % v4(its:ite,jts:jte,1:kz_vv(4)) = vv % v4(its:ite,jts:jte,1:kz_vv(4))
      vp % v5(its:ite,jts:jte,1:kz_vv(5)) = vv % v5(its:ite,jts:jte,1:kz_vv(5))

   end if
!-------------------------------------------------------------------------
!  Check inverse errors:

   write(unit=*, fmt='(/a/)') &
        'DA_Check_VpToVvToVp_Errors'

   call DA_Check_Vp_Errors( vp1, vp, kz_vv, its,ite, jts,jte, kts,kte )
!-------------------------------------------------------------------------
!  [3.0] Check_CvToVv_Adjoint:
!-------------------------------------------------------------------------
   call DA_Check_CvToVv_Adjoint( xb, xbx, xp, be, cv, vv, &
                               ids, ide, jds, jde, kds, kde,      &
                               ims, ime, jms, jme, kms, kme,      &
                               its, ite, jts, jte, kts, kte )
!-------------------------------------------------------------------------
!  [4.0] Test inverse physical variable transform:
!  Note: Currently these test are developed for cp_options = 2 only
!-------------------------------------------------------------------------
   if(  cv_options == 2 .or. cv_options == 5 ) then
     call da_zero_x(xa)
     call da_transform_vptox( xb, xbx, vp, xp, xa, be,    &
                              ids,ide, jds,jde, kds,kde,  &
                              ims,ime, jms,jme, kms,kme,  &
                              its,ite, jts,jte, kts,kte )

!  [4.1] Test XToVpToX differences:

     write(unit=*, fmt='(/a/)') &
          'DA_Check_XToVpToX_Errors'
   
     CALL DA_Check_XToVpToX_Errors( xa, xa2_u, xa2_v, xa2_w, xa2_t, &
                                 xa2_p, xa2_q, xa2_rh, xa2_rho, &
                                 xa2_qt, xa2_qcw, xa2_qrn, &
                                 ids,ide, jds,jde, kds,kde, &
                                 ims,ime, jms,jme, kms,kme, &
                                 its,ite, jts,jte, kts,kte )

!  [4.2] Perform v_{p} = U_{p}^{-1} x transform (again):

     call DA_Transform_XToVp( xb, xbx, xa, xp, vv, be, &
                            ids,ide, jds,jde, kds,kde, &
                            ims,ime, jms,jme, kms,kme, &
                            its,ite, jts,jte, kts,kte )
      
!  [2.4] Check inverse errors:

     write(unit=*, fmt='(/a/)') &
          'DA_Check_VpToXToVp_Errors'

     call DA_Check_Vp_Errors( vp, vv, kz_vp, its,ite, jts,jte, kts,kte )
   end if
!-------------------------------------------------------------------------
!  [5.0] Perform Vv -> Vp adjoint test: 
!-------------------------------------------------------------------------
   call DA_Check_VvToVp_Adjoint( xb, xbx, xp, be, vp, vv, &
                                 ids,ide, jds,jde, kds,kde, &
                                 ims,ime, jms,jme, kms,kme, &
                                 its,ite, jts,jte, kts,kte )
!-------------------------------------------------------------------------
!  [6.0] Perform Vp -> X  adjoint tests: 
!-------------------------------------------------------------------------
   call DA_Check_VpToX_Adjoint( xb, xbx, be, xa, xp, vp, &
                               ids, ide, jds, jde, kds, kde,    &
                               ims, ime, jms, jme, kms, kme,    &
                               its, ite, jts, jte, kts, kte )

#endif
1111     continue
!-------------------------------------------------------------------------
!  [7.0]: Perform adjoint test on complete transform: <x,x> = <v_adj,v>
!-------------------------------------------------------------------------
   call DA_Check_VToX_Adjoint( xb, xbx, be, cv, vv, vp, xp, xa, &
                               ids, ide, jds, jde, kds, kde,      &
                               ims, ime, jms, jme, kms, kme,      &
                               its, ite, jts, jte, kts, kte )
!-------------------------------------------------------------------------
!  [8.0]: Perform Spectral transform tests for Global                    
!-------------------------------------------------------------------------
    if (global) then   
#ifdef DM_PARALLEL
  print*,' Inverse tests for spectral transforms will not be done as it is MPP run'
#else
    call random_number(field(:,:) )
    field(:,:) = field(:,:) - 0.5
    call da_test_spectral(xbx, field, &
                          ids, ide, jds, jde, &
                          ims, ime, jms, jme, &
                          its, ite, jts, jte )
#endif
   endif
!-------------------------------------------------------------------------
 DEALLOCATE (vp1 % v1)
 DEALLOCATE (vp1 % v2)
 DEALLOCATE (vp1 % v3)
 DEALLOCATE (vp1 % v4)
 DEALLOCATE (vp1 % v5)

 DEALLOCATE (vv1 % v1)
 DEALLOCATE (vv1 % v2)
 DEALLOCATE (vv1 % v3)
 DEALLOCATE (vv1 % v4)
 DEALLOCATE (vv1 % v5)

END SUBROUTINE DA_Test_VXTransform

