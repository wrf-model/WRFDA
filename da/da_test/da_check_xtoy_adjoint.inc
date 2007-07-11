subroutine da_check_xtoy_adjoint(grid, iv, y)
   
   !--------------------------------------------------------------------------
   ! Purpose: Test observation operator transform and adjoint for compatibility.
   !
   ! Method:  Standard adjoint test: < y, y > = < x, x_adj >.
   !---------------------------------------------------------------------------
   
   implicit none

   type (domain), intent(inout)     :: grid
   
   type (ob_type),    intent(in)    :: iv    ! ob. increment vector.
   type (y_type),     intent(inout) :: y     ! y = h (grid%xa)

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

#ifdef DM_PARALLEL
#include "HALO_XA.inc"
#endif

   !  grid%xa%u(ims:ime, jms:jme, kms:kme) = 0.0
   !  grid%xa%v(ims:ime, jms:jme, kms:kme) = 0.0
   !  grid%xa%w(ims:ime, jms:jme, kms:kme) = 0.0
   !  grid%xa%t(ims:ime, jms:jme, kms:kme) = 0.0
   !  grid%xa%p(ims:ime, jms:jme, kms:kme) = 0.0
   !  grid%xa%q(ims:ime, jms:jme, kms:kme) = 0.0
   !  grid%xa%rh(ims:ime, jms:jme, kms:kme) = 0.0
   !  grid%xa%psfc(ims:ime, jms:jme) = 0.0

   xa2_u(ims:ime, jms:jme, kms:kme) = grid%xa%u(ims:ime, jms:jme, kms:kme)
   xa2_v(ims:ime, jms:jme, kms:kme) = grid%xa%v(ims:ime, jms:jme, kms:kme)
   xa2_t(ims:ime, jms:jme, kms:kme) = grid%xa%t(ims:ime, jms:jme, kms:kme)
   xa2_p(ims:ime, jms:jme, kms:kme) = grid%xa%p(ims:ime, jms:jme, kms:kme)
   xa2_q(ims:ime, jms:jme, kms:kme) = grid%xa%q(ims:ime, jms:jme, kms:kme)
   xa2_w(ims:ime, jms:jme, kms:kme) = grid%xa%w(ims:ime, jms:jme, kms:kme)
   xa2_rh(ims:ime, jms:jme, kms:kme)= grid%xa%rh(ims:ime, jms:jme, kms:kme)
   xa2_psfc(ims:ime, jms:jme)       = grid%xa%psfc(ims:ime, jms:jme)

   xa2_qcw(ims:ime, jms:jme, kms:kme) = grid%xa%qcw(ims:ime, jms:jme, kms:kme)
   xa2_qrn(ims:ime, jms:jme, kms:kme) = grid%xa%qrn(ims:ime, jms:jme, kms:kme)

   call da_pt_to_rho_lin(grid)

   if (sfc_assi_options == 2) then
      call da_transform_xtowtq (grid)
#ifdef DM_PARALLEL
#include "HALO_SFC_XA.inc"
#endif
   end if

   if (use_ssmt1obs .or. use_ssmt2obs .or. use_gpspwobs .or. &
       use_gpsrefobs .or.                                    &
       use_ssmitbobs .or. use_SsmiRetrievalObs) then

      ! Now do something for PW
      call da_transform_xtotpw(grid)

      ! GPS Refractivity:
      if (use_gpsrefobs) then
         call da_transform_xtogpsref_lin(grid)
      end if

      if (use_ssmt1obs .or. use_ssmt2obs .or. &
          use_ssmitbobs .or. use_ssmiretrievalobs) then
         if (global) then
           call da_error(__FILE__,__LINE__, &
             (/"grid%xb%speed is not available, see da_transfer_kmatoxb.inc"/))
         end if
         call da_transform_xtoseasfcwind_lin(grid)
      end if

      if (use_ssmitbobs) call da_transform_xtotb_lin (grid)

#ifdef DM_PARALLEL
#include "HALO_SSMI_XA.inc"
#endif
   end if

   !----------------------------------------------------------------------
   ! [2.0] Perform y = Hx transform:
   !----------------------------------------------------------------------
   
   call da_transform_xtoy(grid, iv, y)

   !----------------------------------------------------------------------
   ! [3.0] Calculate LHS of adjoint test equation and
   !       Rescale input to adjoint routine :
   !----------------------------------------------------------------------

   partial_lhs = 0.0
   pertile_lhs = 0.0

   if (use_soundobs) then
      call da_check_xtoy_adjoint_sound(iv, y, partial_lhs, pertile_lhs)
      call da_check_xtoy_adjoint_sonde_sfc(iv, y, partial_lhs, pertile_lhs)
   end if

   if (use_synopobs) &
      call da_check_xtoy_adjoint_synop(iv, y, partial_lhs, pertile_lhs)

   if (use_geoamvobs) &
      call da_check_xtoy_adjoint_geoamv(iv, y, partial_lhs, pertile_lhs)

   if (use_polaramvobs) &
      call da_check_xtoy_adjoint_polaramv(iv, y, partial_lhs, pertile_lhs)

   if (use_airepobs) &
      call da_check_xtoy_adjoint_airep(iv, y, partial_lhs, pertile_lhs)

   if (use_pilotobs) &
      call da_check_xtoy_adjoint_pilot(iv, y, partial_lhs, pertile_lhs)

   if (use_radarobs) &
      call da_check_xtoy_adjoint_radar(iv, y, partial_lhs, pertile_lhs)

   if (use_satemobs) &
      call da_check_xtoy_adjoint_satem(iv, y, partial_lhs, pertile_lhs)

   if (use_metarobs) &
      call da_check_xtoy_adjoint_metar(iv, y, partial_lhs, pertile_lhs)
 
   if (use_shipsobs) &
      call da_check_xtoy_adjoint_ships(iv, y, partial_lhs, pertile_lhs)

   if (use_gpspwobs) &
      call da_check_xtoy_adjoint_gpspw(iv, y, partial_lhs, pertile_lhs)

   if (use_gpsrefobs) &
      call da_check_xtoy_adjoint_gpsref(iv, y, partial_lhs, pertile_lhs)
   
   if (use_ssmitbobs .or. use_ssmiretrievalobs) &
      call da_check_xtoy_adjoint_ssmi (iv, y, partial_lhs, pertile_lhs)

   if (use_ssmt1obs) &
      call da_check_xtoy_adjoint_ssmt1(iv, y, partial_lhs, pertile_lhs)

   if (use_ssmt2obs) &
      call da_check_xtoy_adjoint_ssmt2(iv, y, partial_lhs, pertile_lhs)

   if (use_qscatobs) &
      call da_check_xtoy_adjoint_qscat(iv, y, partial_lhs, pertile_lhs)

   if (use_profilerobs) &
   call da_check_xtoy_adjoint_profiler(iv, y, partial_lhs, pertile_lhs)

   if (use_buoyobs) &
      call da_check_xtoy_adjoint_buoy(iv, y, partial_lhs, pertile_lhs)

   if (use_bogusobs) &
      call da_check_xtoy_adjoint_bogus(iv, y, partial_lhs, pertile_lhs)

   if (use_rad) &
      call da_check_xtoy_adjoint_rad(iv, y, partial_lhs, pertile_lhs)

   !----------------------------------------------------------------------
   ! [5.0] Perform adjoint operation:
   !----------------------------------------------------------------------

   call da_zero_x (grid%xa)
   
   call da_transform_xtoy_adj (grid, iv, y, grid%xa)


   if (use_ssmt1obs .or. use_ssmt2obs .or. use_gpspwobs .or. &
       use_gpsrefobs .or.                                    &
       use_ssmitbobs .or. use_SsmiRetrievalObs) then

      if (use_ssmitbobs) call da_transform_xtotb_adj (grid)

      ! for PW
      call da_transform_xtotpw_adj (grid)

      ! GPS Refractivity:
      if (use_gpsrefobs) call da_transform_xtogpsref_adj (grid)

      if (use_ssmt1obs .or. use_ssmt2obs .or. &
          use_ssmitbobs .or. use_SsmiRetrievalObs) then
         if (global) then
            call da_error(__FILE__,__LINE__, &
               (/"grid%xb%speed is not available, see da_transfer_kmatogrid%xb.inc"/))
         end if
         call da_transform_xtoseasfcwind_adj (grid)
      end if
   end if

   ! Now do something for surface variables
   if (sfc_assi_options == 2) then
      call da_transform_xtowtq_adj (grid)
   end if

   call da_pt_to_rho_adj (grid)

   pertile_rhs = sum (grid%xa%u(ims:ime, jms:jme, kms:kme) * xa2_u(ims:ime, jms:jme, kms:kme)) &
               + sum (grid%xa%v(ims:ime, jms:jme, kms:kme) * xa2_v(ims:ime, jms:jme, kms:kme)) &
               + sum (grid%xa%w(ims:ime, jms:jme, kms:kme) * xa2_w(ims:ime, jms:jme, kms:kme)) &
               + sum (grid%xa%t(ims:ime, jms:jme, kms:kme) * xa2_t(ims:ime, jms:jme, kms:kme)) &
               + sum (grid%xa%p(ims:ime, jms:jme, kms:kme) * xa2_p(ims:ime, jms:jme, kms:kme)) &
               + sum (grid%xa%q(ims:ime, jms:jme, kms:kme) * xa2_q(ims:ime, jms:jme, kms:kme)) &
               + sum (grid%xa%rh(ims:ime, jms:jme, kms:kme)* xa2_rh(ims:ime, jms:jme, kms:kme)) &
               + sum (grid%xa%psfc(ims:ime, jms:jme) * xa2_psfc(ims:ime, jms:jme))
   pertile_rhs = pertile_rhs &
               + sum (grid%xa%qcw(ims:ime, jms:jme, kms:kme) * xa2_qcw(ims:ime, jms:jme, kms:kme)) &
               + sum (grid%xa%qrn(ims:ime, jms:jme, kms:kme) * xa2_qrn(ims:ime, jms:jme, kms:kme))

   !----------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjoint test equation:
   !----------------------------------------------------------------------
   
   partial_rhs = sum (grid%xa%u(its:ite, jts:jte, kts:kte) * xa2_u(its:ite, jts:jte, kts:kte)) &
               + sum (grid%xa%v(its:ite, jts:jte, kts:kte) * xa2_v(its:ite, jts:jte, kts:kte)) &
               + sum (grid%xa%w(its:ite, jts:jte, kts:kte+1) * xa2_w(its:ite, jts:jte, kts:kte+1)) &
               + sum (grid%xa%t(its:ite, jts:jte, kts:kte) * xa2_t(its:ite, jts:jte, kts:kte)) &
               + sum (grid%xa%p(its:ite, jts:jte, kts:kte) * xa2_p(its:ite, jts:jte, kts:kte)) &
               + sum (grid%xa%q(its:ite, jts:jte, kts:kte) * xa2_q(its:ite, jts:jte, kts:kte)) &
               + sum (grid%xa%rh(its:ite, jts:jte, kts:kte)* xa2_rh(its:ite, jts:jte, kts:kte)) &
               + sum (grid%xa%psfc(its:ite, jts:jte) * xa2_psfc(its:ite, jts:jte)) 

   partial_rhs = partial_rhs &
               + sum (grid%xa%qcw(its:ite, jts:jte, kts:kte) * xa2_qcw(its:ite, jts:jte, kts:kte)) &
               + sum (grid%xa%qrn(its:ite, jts:jte, kts:kte) * xa2_qrn(its:ite, jts:jte, kts:kte))

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


