subroutine da_setup_testfield( xb, xa, xp, &
                              ids,ide, jds,jde, kds,kde, &
                              ims,ime, jms,jme, kms,kme, &
                              its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: produce test increment field based on xb field.
!
!  METHOD:  pass through x=uv transfom to ensure satisfies boundary conditions.
!
!  HISTORY: 02/23/2000 - creation of f90 version.        Dale Barker
!------------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)  :: xb     ! first guess (local).
   type (x_type) , intent(out) :: xa     ! gridded analy. incs. (local)
   type (xpose_type), intent(in)  :: xp  ! Transpose variables.

   integer, intent(in)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
   integer, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
   integer, intent(in)  :: its,ite, jts,jte, kts,kte  ! tile   dims.

   integer :: i, j

!-------------------------------------------------------------------------
!  [1.0]: initialise:
!-------------------------------------------------------------------------

   write(unit=*, fmt='(/a/)') &
        'Starting da_setup_testfield ...'

!-------------------------------------------------------------------------
!  [2.0]: set up test increment field structure:
!-------------------------------------------------------------------------

!  [2.1] test wind, temperature, pressure, and humidity.

#ifndef DEREF_KLUDGE
   call set_tst_trnsf_fld(xp, xa%u, xb%u, typical_u_rms)
   call set_tst_trnsf_fld(xp, xa%v, xb%v, typical_v_rms)
   call set_tst_trnsf_fld(xp, xa%w, xb%w, typical_w_rms)
   call set_tst_trnsf_fld(xp, xa%t, xb%t, typical_t_rms)
   call set_tst_trnsf_fld(xp, xa%p, xb%p, typical_p_rms)
   call set_tst_trnsf_fld(xp, xa%q, xb%q, typical_q_rms)
   call set_tst_trnsf_fld(xp, xa%qcw, xb%qcw, typical_qcw_rms)
   call set_tst_trnsf_fld(xp, xa%qrn, xb%qrn, typical_qrn_rms)
#else
   call set_tst_trnsf_fld(xp, xa%u(ims,jms,kms), xb%u(ims,jms,kms), typical_u_rms)
   call set_tst_trnsf_fld(xp, xa%v(ims,jms,kms), xb%v(ims,jms,kms), typical_v_rms)
   call set_tst_trnsf_fld(xp, xa%w(ims,jms,kms), xb%w(ims,jms,kms), typical_w_rms)
   call set_tst_trnsf_fld(xp, xa%t(ims,jms,kms), xb%t(ims,jms,kms), typical_t_rms)
   call set_tst_trnsf_fld(xp, xa%p(ims,jms,kms), xb%p(ims,jms,kms), typical_p_rms)
   call set_tst_trnsf_fld(xp, xa%q(ims,jms,kms), xb%q(ims,jms,kms), typical_q_rms)
   call set_tst_trnsf_fld(xp, xa%qcw(ims,jms,kms), xb%qcw(ims,jms,kms), typical_qcw_rms)
   call set_tst_trnsf_fld(xp, xa%qrn(ims,jms,kms), xb%qrn(ims,jms,kms), typical_qrn_rms)

#endif

!  [2.5] get test density increment from linearised ideal gas law:

   call da_pt_to_rho_lin( xb, xa, xp )

   xa%psfc(xp%its:xp%ite, xp%jts:xp%jte) = &
   xa%p   (xp%its:xp%ite, xp%jts:xp%jte, xp%kts)

   if(print_detail > 100) then
   write(unit=*, fmt='(2a,4x,a,i8)') &
        'file:', __FILE__, 'line:', __LINE__

   print *, 'xp%its, xp%ite, xp%jts, xp%jte) =', xp%its, xp%ite, xp%jts, xp%jte
   
   do j=xp%jts, xp%jte
   do i=xp%its, xp%ite
      if(i == j) then
         write(unit=*, fmt='(2(a,i4),a,f14.6)') &
              'xa%psfc(', i, ',', j, ') =', xa%psfc(i, j)
      endif
   enddo
   enddo
   endif

   if (cv_options < 3) then
      if (sfc_assi_options == 2) then
         call DA_Transform_XToWTQ ( xp, xb, xa )
      endif
   endif

!--Exchange XA halo region.
   CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id4)

   if (sfc_assi_options == 2) then
!-----Exchange XA (surface variable) halo region.
      CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id6)
   endif

   if ( use_ssmt1obs .or. use_ssmt2obs .or. Use_GpspwObs .or. &
        Use_SsmiTbObs .or. Use_SsmiRetrievalObs ) then

!-----Now do something for PW
      call DA_Transform_XToTPW( xa, xb )

!--------GPS Refractivity:
         if ( use_GpsrefObs ) &
         call da_transform_XTogpsref_lin(xa, xb, xp)

      if ( use_ssmt1obs .or. use_ssmt2obs .or. &
           Use_SsmiTbObs .or. Use_SsmiRetrievalObs ) then
         call DA_Transform_XToSeaSfcWind_Lin( xa, xb )
      endif

!-----Exchange XA halo region.
      CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id8)
   endif

   write(unit=*, fmt='(/a/)') &
        'End of da_setup_testfield.'

end subroutine da_setup_testfield

