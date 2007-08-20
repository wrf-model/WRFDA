subroutine da_get_y_lhs_value (iv, y, partial_lhs, pertile_lhs, adj_ttl_lhs) 

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
    
   implicit none
   
   type(iv_type), intent(in)    :: iv    ! ob. increment vector.
   type(y_type),  intent(inout) :: y     ! y = h(xa)
   real,          intent(out)   :: partial_lhs, pertile_lhs, adj_ttl_lhs

   if (trace_use) call da_trace_entry("da_get_y_lhs_value")

   partial_lhs = 0.0
   pertile_lhs = 0.0

   if (iv%nlocal(sound) > 0)  then
      call da_check_xtoy_adjoint_sound( iv, y, partial_lhs, pertile_lhs) 
      call da_check_xtoy_adjoint_sonde_sfc( iv, y, partial_lhs, pertile_lhs) 
   end if

   if (iv%nlocal(synop)          > 0) call da_check_xtoy_adjoint_synop    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(geoamv)         > 0) call da_check_xtoy_adjoint_geoamv   (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(polaramv)       > 0) call da_check_xtoy_adjoint_polaramv (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(airep)          > 0) call da_check_xtoy_adjoint_airep    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(pilot)          > 0) call da_check_xtoy_adjoint_pilot    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(radar)          > 0) call da_check_xtoy_adjoint_radar    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(satem)          > 0) call da_check_xtoy_adjoint_satem    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(metar)          > 0) call da_check_xtoy_adjoint_metar    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(ships)          > 0) call da_check_xtoy_adjoint_ships    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(gpspw)          > 0) call da_check_xtoy_adjoint_gpspw    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(gpsref)         > 0) call da_check_xtoy_adjoint_gpsref   (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(ssmi_tb)        > 0) call da_check_xtoy_adjoint_ssmi_tb  (iv, y, partial_lhs, pertile_lhs)
   if (iv%nlocal(ssmi_rv) > 0) call da_check_xtoy_adjoint_ssmi_rv  (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(ssmt1)          > 0) call da_check_xtoy_adjoint_ssmt1    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(ssmt2)          > 0) call da_check_xtoy_adjoint_ssmt2    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(qscat)          > 0) call da_check_xtoy_adjoint_qscat    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(profiler)       > 0) call da_check_xtoy_adjoint_profiler (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(buoy)           > 0) call da_check_xtoy_adjoint_buoy     (iv, y, partial_lhs, pertile_lhs) 
   if (iv%nlocal(bogus)          > 0) call da_check_xtoy_adjoint_bogus    (iv, y, partial_lhs, pertile_lhs) 

   ! FIX? consider using dm_sum_real
#ifdef DM_PARALLEL
   call mpi_allreduce( partial_lhs, adj_ttl_lhs, 1, true_mpi_real, mpi_sum, comm, ierr) 
#else
   adj_ttl_lhs = partial_lhs
#endif

   if (trace_use) call da_trace_exit("da_get_y_lhs_value")
   
end subroutine da_get_y_lhs_value


