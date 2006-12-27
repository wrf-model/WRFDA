subroutine da_setup_testfield(xb, xa, xp)

   !----------------------------------------------------------------------------
   ! Purpose: produce test increment field based on xb field.
   !
   ! Method:  pass through x=uv transfom to ensure satisfies boundary conditions
   !----------------------------------------------------------------------------

   implicit none

   type (xb_type),    intent(in)  :: xb  ! first guess (local).
   type (x_type) ,    intent(out) :: xa  ! gridded analy. incs. (local)
   type (xpose_type), intent(in)  :: xp  ! Transpose variables.

   integer :: i, j

   !-------------------------------------------------------------------------
   ! [1.0]: initialise:
   !-------------------------------------------------------------------------

   write(unit=stdout, fmt='(/a/)') &
      'Starting da_setup_testfield ...'

   !-------------------------------------------------------------------------
   ! [2.0]: set up test increment field structure:
   !-------------------------------------------------------------------------

   ! [2.1] test wind, temperature, pressure, and humidity.

   call da_set_tst_trnsf_fld(xp, xa%u, xb%u, typical_u_rms)
   call da_set_tst_trnsf_fld(xp, xa%v, xb%v, typical_v_rms)
   call da_set_tst_trnsf_fld(xp, xa%w, xb%w, typical_w_rms)
   call da_set_tst_trnsf_fld(xp, xa%t, xb%t, typical_t_rms)
   call da_set_tst_trnsf_fld(xp, xa%p, xb%p, typical_p_rms)
   call da_set_tst_trnsf_fld(xp, xa%q, xb%q, typical_q_rms)
   call da_set_tst_trnsf_fld(xp, xa%qcw, xb%qcw, typical_qcw_rms)
   call da_set_tst_trnsf_fld(xp, xa%qrn, xb%qrn, typical_qrn_rms)

   ! [2.5] get test density increment from linearised ideal gas law:

   call da_pt_to_rho_lin(xb, xa, xp)

   xa%psfc(xp%its:xp%ite, xp%jts:xp%jte) = &
   xa%p   (xp%its:xp%ite, xp%jts:xp%jte, xp%kts)

   if (print_detail_testing) then
      write(unit=stdout, fmt='(2a,4x,a,i8)') &
         'file:', __FILE__, 'line:', __LINE__

      write(unit=stdout, fmt=*) 'xp%its, xp%ite, xp%jts, xp%jte) =', &
         xp%its, xp%ite, xp%jts, xp%jte
   
      do j=xp%jts, xp%jte
         do i=xp%its, xp%ite
            if (i == j) then
               write(unit=stdout, fmt='(2(a,i4),a,f14.6)') &
                  'xa%psfc(', i, ',', j, ') =', xa%psfc(i, j)
            end if
         end do
      end do
   end if

   ! Exchange XA halo region.
   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id4)

   if (sfc_assi_options == 2) then
      ! Exchange XA (surface variable) halo region.
      call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id6)
   end if

   if (use_ssmt1obs .or. use_ssmt2obs .or. Use_GpspwObs .or. &
        Use_SsmiTbObs .or. Use_SsmiRetrievalObs) then

      ! Now do something for PW
      call da_transform_xtotpw(xa, xb)

      ! GPS Refractivity:
      if (use_GpsrefObs) &
         call da_transform_xtogpsref_lin(xa, xb, xp)

      if (use_ssmt1obs .or. use_ssmt2obs .or. &
           Use_SsmiTbObs .or. Use_SsmiRetrievalObs) then
         call da_transform_xtoseasfcwind_lin(xa, xb)
      end if

      ! Exchange XA halo region.
      call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id8)
   end if

   write(unit=stdout, fmt='(/a/)') &
      'End of da_setup_testfield.'

end subroutine da_setup_testfield


