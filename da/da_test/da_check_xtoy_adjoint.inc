subroutine da_check_xtoy_adjoint(xb, xa, iv, xp, y)
   
   !--------------------------------------------------------------------------
   ! Purpose: Test observation operator transform and adjoint for compatibility.
   !
   ! METHOD:  Standard adjoint test: < y, y > = < x, x_adj >.
   !---------------------------------------------------------------------------
   
   implicit none
   
   type (xb_type),    intent(in)    :: xb    ! first guess (local).
   type (x_type),     intent(inout) :: xa    ! analysis increments (local).
   type (ob_type),    intent(in)    :: iv    ! ob. increment vector.
   type (xpose_type), intent(in)    :: xp    ! Dimensions and xpose buffers(be).
   TYPE (y_type),     intent(inout) :: y     ! y = h (xa)

   real                           :: adj_ttl_lhs   ! < y, y >
   real                           :: adj_ttl_rhs   ! < x, x_adj >

   real                           :: partial_lhs   ! < y, y >
   real                           :: partial_rhs   ! < x, x_adj >

   real                           :: pertile_lhs   ! < y, y >
   real                           :: pertile_rhs   ! < x, x_adj >
 
   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_u, xa2_v, xa2_t, &
                                                 xa2_p, xa2_q, xa2_rh
   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_w
   real, dimension(ims:ime, jms:jme)          :: xa2_psfc
   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_qcw, xa2_qrn

   if (trace_use) call da_trace_entry("da_check_xtoy_adjoint")

   write (unit=stdout, fmt='(/a)') ' da_check_xtoy_adjoint: Adjoint Test Results:'

   !----------------------------------------------------------------------
   ! [1.0] Initialise:
   !----------------------------------------------------------------------

   ! Exchange XA (u, v, t, p, q, psfc) halo region.
   call wrf_dm_halo (xp%domdesc,xp%comms,xp%halo_id4)

   !  xa%u(ims:ime, jms:jme, kms:kme) = 0.0
   !  xa%v(ims:ime, jms:jme, kms:kme) = 0.0
   !  xa%w(ims:ime, jms:jme, kms:kme) = 0.0
   !  xa%t(ims:ime, jms:jme, kms:kme) = 0.0
   !  xa%p(ims:ime, jms:jme, kms:kme) = 0.0
   !  xa%q(ims:ime, jms:jme, kms:kme) = 0.0
   !  xa%rh(ims:ime, jms:jme, kms:kme) = 0.0
   !  xa%psfc(ims:ime, jms:jme) = 0.0

   xa2_u(ims:ime, jms:jme, kms:kme) = xa%u(ims:ime, jms:jme, kms:kme)
   xa2_v(ims:ime, jms:jme, kms:kme) = xa%v(ims:ime, jms:jme, kms:kme)
   xa2_t(ims:ime, jms:jme, kms:kme) = xa%t(ims:ime, jms:jme, kms:kme)
   xa2_p(ims:ime, jms:jme, kms:kme) = xa%p(ims:ime, jms:jme, kms:kme)
   xa2_q(ims:ime, jms:jme, kms:kme) = xa%q(ims:ime, jms:jme, kms:kme)
   xa2_w(ims:ime, jms:jme, kms:kme) = xa%w(ims:ime, jms:jme, kms:kme)
   xa2_rh(ims:ime, jms:jme, kms:kme)= xa%rh(ims:ime, jms:jme, kms:kme)
   xa2_psfc(ims:ime, jms:jme)       = xa%psfc(ims:ime, jms:jme)

   xa2_qcw(ims:ime, jms:jme, kms:kme) = xa%qcw(ims:ime, jms:jme, kms:kme)
   xa2_qrn(ims:ime, jms:jme, kms:kme) = xa%qrn(ims:ime, jms:jme, kms:kme)

   call da_pt_to_rho_lin(xb, xa, xp)

   if (sfc_assi_options == 2) then
      call da_transform_xtowtq (xp, xb, xa)
      ! Exchange XA (surface variable: u10, v10, t2, q2) halo region.
      call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id6)
   end if

   if (use_ssmt1obs .or. use_ssmt2obs .or. use_GpspwObs .or. &
       use_GpsrefObs .or.                                    &
       use_SsmiTbObs .or. use_SsmiRetrievalObs) then

      ! Now do something for PW
      call da_transform_xtotpw(xa, xb)

      ! GPS Refractivity:
      if (use_gpsrefobs) then
         call da_transform_xtogpsref_lin(xa, xb, xp)
      end if

      if (use_ssmt1obs .or. use_ssmt2obs .or. &
          use_SsmiTbObs .or. Use_SsmiRetrievalObs) then
         if (global) then
           call da_error(__FILE__,__LINE__, &
             (/"xb%speed is not available, see da_transfer_kmatoxb.inc"/))
         end if
         call da_transform_xtoseasfcwind_lin(xa, xb)
      end if

      if (use_SsmiTbObs) call da_transform_xtotb_lin (xa, xb)

      ! Exchange XA halo region.
      call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id8)
   end if

   !----------------------------------------------------------------------
   ! [2.0] Perform y = Hx transform:
   !----------------------------------------------------------------------
   
   call da_transform_xtoy(xb, iv, xa, xp, y)

   !----------------------------------------------------------------------
   ! [3.0] Calculate LHS of adjoint test equation and
   !       Rescale input to adjoint routine :
   !----------------------------------------------------------------------

   partial_lhs = 0.0
   pertile_lhs = 0.0

   if (use_SoundObs) then
      call da_check_xtoy_adjoint_sound(iv, y, partial_lhs, pertile_lhs)
      call da_check_xtoy_adjoint_sonde_sfc(iv, y, partial_lhs, pertile_lhs)
   end if

   if (use_SynopObs) &
      call da_check_xtoy_adjoint_synop(iv, y, partial_lhs, pertile_lhs)

   if (use_GeoAMVObs) &
      call da_check_xtoy_adjoint_geoamv(iv, y, partial_lhs, pertile_lhs)

   if (use_PolarAMVObs) &
      call da_check_xtoy_adjoint_polaramv(iv, y, partial_lhs, pertile_lhs)

   if (use_AirepObs) &
      call da_check_xtoy_adjoint_airep(iv, y, partial_lhs, pertile_lhs)

   if (use_PilotObs) &
      call da_check_xtoy_adjoint_pilot(iv, y, partial_lhs, pertile_lhs)

   if (use_RadarObs) &
      call da_check_xtoy_adjoint_radar(iv, y, partial_lhs, pertile_lhs)

   if (use_SatemObs) &
      call da_check_xtoy_adjoint_satem(iv, y, partial_lhs, pertile_lhs)

   if (use_MetarObs) &
      call da_check_xtoy_adjoint_metar(iv, y, partial_lhs, pertile_lhs)
 
   if (use_ShipsObs) &
      call da_check_xtoy_adjoint_ships(iv, y, partial_lhs, pertile_lhs)

   if (use_GpspwObs) &
      call da_check_xtoy_adjoint_gpspw(iv, y, partial_lhs, pertile_lhs)

   if (use_GpsrefObs) &
      call da_check_xtoy_adjoint_gpsref(iv, y, partial_lhs, pertile_lhs)
   
   if (use_SsmiTbObs .or. Use_SsmiRetrievalObs) &
      call da_check_xtoy_adjoint_ssmi (iv, y, partial_lhs, pertile_lhs)

   if (use_ssmt1obs) &
      call da_check_xtoy_adjoint_ssmt1(iv, y, partial_lhs, pertile_lhs)

   if (use_ssmt2obs) &
      call da_check_xtoy_adjoint_ssmt2(iv, y, partial_lhs, pertile_lhs)

   if (use_qscatobs) &
      call da_check_xtoy_adjoint_qscat(iv, y, partial_lhs, pertile_lhs)

   if (use_ProfilerObs) &
   call da_check_xtoy_adjoint_profiler(iv, y, partial_lhs, pertile_lhs)

   if (use_BuoyObs) &
      call da_check_xtoy_adjoint_buoy(iv, y, partial_lhs, pertile_lhs)

   if (use_BogusObs) &
      call da_check_xtoy_adjoint_bogus(iv, y, partial_lhs, pertile_lhs)

   if (use_Radiance) &
      call da_check_xtoy_adjoint_rad(iv, y, partial_lhs, pertile_lhs)

   !----------------------------------------------------------------------
   ! [5.0] Perform adjoint operation:
   !----------------------------------------------------------------------

   call da_zero_x (xa)
   
   call da_transform_xtoy_adj (xb, iv, xp, y, xa)


   if (use_ssmt1obs .or. use_ssmt2obs .or. use_GpspwObs .or. &
       use_GpsrefObs .or.                                    &
       use_SsmiTbObs .or. use_SsmiRetrievalObs) then

      if (Use_SsmiTbObs) call da_transform_xtotb_adj (xa, xb)

      ! for PW
      call da_transform_xtotpw_adj (xa, xb)

      ! GPS Refractivity:
      if (use_GpsrefObs) call da_transform_xtogpsref_adj (xa, xb, xp)

      if (use_ssmt1obs .or. use_ssmt2obs .or. &
          use_SsmiTbObs .or. use_SsmiRetrievalObs) then
         if (global) then
            call da_error(__FILE__,__LINE__, &
               (/"xb%speed is not available, see da_transfer_kmatoxb.inc"/))
         end if
         call da_transform_xtoseasfcwind_adj (xa, xb)
      end if
   end if

   ! Now do something for surface variables
   if (sfc_assi_options == 2) then
      call da_transform_xtowtq_adj (xp, xb, xa)
   end if

   call da_pt_to_rho_adj (xb, xa)

   pertile_rhs = sum (xa%u(ims:ime, jms:jme, kms:kme) * xa2_u(ims:ime, jms:jme, kms:kme)) &
               + sum (xa%v(ims:ime, jms:jme, kms:kme) * xa2_v(ims:ime, jms:jme, kms:kme)) &
               + sum (xa%w(ims:ime, jms:jme, kms:kme) * xa2_w(ims:ime, jms:jme, kms:kme)) &
               + sum (xa%t(ims:ime, jms:jme, kms:kme) * xa2_t(ims:ime, jms:jme, kms:kme)) &
               + sum (xa%p(ims:ime, jms:jme, kms:kme) * xa2_p(ims:ime, jms:jme, kms:kme)) &
               + sum (xa%q(ims:ime, jms:jme, kms:kme) * xa2_q(ims:ime, jms:jme, kms:kme)) &
               + sum (xa%rh(ims:ime, jms:jme, kms:kme)* xa2_rh(ims:ime, jms:jme, kms:kme)) &
               + sum (xa%psfc(ims:ime, jms:jme) * xa2_psfc(ims:ime, jms:jme))
   pertile_rhs = pertile_rhs &
               + sum (xa%qcw(ims:ime, jms:jme, kms:kme) * xa2_qcw(ims:ime, jms:jme, kms:kme)) &
               + sum (xa%qrn(ims:ime, jms:jme, kms:kme) * xa2_qrn(ims:ime, jms:jme, kms:kme))

   !----------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjoint test equation:
   !----------------------------------------------------------------------
   
   partial_rhs = sum (xa%u(its:ite, jts:jte, kts:kte) * xa2_u(its:ite, jts:jte, kts:kte)) &
               + sum (xa%v(its:ite, jts:jte, kts:kte) * xa2_v(its:ite, jts:jte, kts:kte)) &
               + sum (xa%w(its:ite, jts:jte, kts:kte+1) * xa2_w(its:ite, jts:jte, kts:kte+1)) &
               + sum (xa%t(its:ite, jts:jte, kts:kte) * xa2_t(its:ite, jts:jte, kts:kte)) &
               + sum (xa%p(its:ite, jts:jte, kts:kte) * xa2_p(its:ite, jts:jte, kts:kte)) &
               + sum (xa%q(its:ite, jts:jte, kts:kte) * xa2_q(its:ite, jts:jte, kts:kte)) &
               + sum (xa%rh(its:ite, jts:jte, kts:kte)* xa2_rh(its:ite, jts:jte, kts:kte)) &
               + sum (xa%psfc(its:ite, jts:jte) * xa2_psfc(its:ite, jts:jte)) 

   partial_rhs = partial_rhs &
               + sum (xa%qcw(its:ite, jts:jte, kts:kte) * xa2_qcw(its:ite, jts:jte, kts:kte)) &
               + sum (xa%qrn(its:ite, jts:jte, kts:kte) * xa2_qrn(its:ite, jts:jte, kts:kte))

   !----------------------------------------------------------------------
   !  [7.0] Print output:
   !----------------------------------------------------------------------
   
   write (unit=stdout, fmt='(A,1pe22.14)') &
      ' Tile < y, y     > = ', pertile_lhs, &
      ' Tile < x, x_adj > = ', pertile_rhs

   adj_ttl_lhs = wrf_dm_sum_real (partial_lhs)
   adj_ttl_rhs = wrf_dm_sum_real (partial_rhs)
   
   if (rootproc) then
      write (unit=stdout, fmt='(A,1pe22.14)') ' Whole Domain < y, y     > = ', adj_ttl_lhs
      write (unit=stdout, fmt='(A,1pe22.14)') ' Whole Domain < x, x_adj > = ', adj_ttl_rhs
   end if
   
   if (trace_use) call da_trace_exit("da_check_xtoy_adjoint")

end subroutine da_check_xtoy_adjoint


