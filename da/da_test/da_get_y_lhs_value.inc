subroutine da_get_y_lhs_value( iv, y, &
                               partial_lhs, pertile_lhs, adj_ttl_lhs) 
    
   implicit none
   
   type (ob_type),    intent(in)    :: iv    ! ob. increment vector.
   type (y_type),     intent(inout) :: y     ! y = h (xa)

   real,              intent(out)   :: partial_lhs, pertile_lhs, adj_ttl_lhs

   partial_lhs = 0.0
   pertile_lhs = 0.0

   if (use_soundobs)  then
       call da_check_xtoy_adjoint_sound( iv, y, partial_lhs, pertile_lhs) 
       call da_check_xtoy_adjoint_sonde_sfc( iv, y, partial_lhs, pertile_lhs) 
   end if

   if (use_synopobs)  &
      call da_check_xtoy_adjoint_synop( iv, y, partial_lhs, pertile_lhs) 

   if (use_geoamvobs)  &
      call da_check_xtoy_adjoint_geoamv( iv, y, partial_lhs, pertile_lhs) 

   if (use_polaramvobs)  &
      call da_check_xtoy_adjoint_polaramv( iv, y, partial_lhs, pertile_lhs) 

   if (use_airepobs)  &
      call da_check_xtoy_adjoint_airep( iv, y, partial_lhs, pertile_lhs) 

   if (use_pilotobs)  &
      call da_check_xtoy_adjoint_pilot( iv, y, partial_lhs, pertile_lhs) 

   if (use_radarobs)  &
      call da_check_xtoy_adjoint_radar( iv, y, partial_lhs, pertile_lhs) 

   if (use_satemobs)  &
      call da_check_xtoy_adjoint_satem( iv, y, partial_lhs, pertile_lhs) 

   if (use_metarobs)  &
      call da_check_xtoy_adjoint_metar( iv, y, partial_lhs, pertile_lhs) 
 
   if (use_shipsobs)  &
      call da_check_xtoy_adjoint_ships( iv, y, partial_lhs, pertile_lhs) 

   if (use_gpspwobs)  &
      call da_check_xtoy_adjoint_gpspw( iv, y, partial_lhs, pertile_lhs) 

   if ( use_gpsrefobs)  &
      call da_check_xtoy_adjoint_gpsref( iv, y, partial_lhs, pertile_lhs) 
   
   if (use_ssmitbobs .or. use_ssmiretrievalobs)  &
      call da_check_xtoy_adjoint_ssmi ( iv, y, partial_lhs, pertile_lhs) 

   if (use_ssmt1obs)  &
      call da_check_xtoy_adjoint_ssmt1( iv, y, partial_lhs, pertile_lhs) 

   if (use_ssmt2obs)  &
      call da_check_xtoy_adjoint_ssmt2( iv, y, partial_lhs, pertile_lhs) 

   if (use_qscatobs)  &
      call da_check_xtoy_adjoint_qscat( iv, y, partial_lhs, pertile_lhs) 

   if (use_profilerobs)  &
      call da_check_xtoy_adjoint_profiler( iv, y, partial_lhs, pertile_lhs) 

   if (use_buoyobs)  &
      call da_check_xtoy_adjoint_buoy( iv, y, partial_lhs, pertile_lhs) 

   if (use_bogusobs)  &
      call da_check_xtoy_adjoint_bogus( iv, y, partial_lhs, pertile_lhs) 

   ! JRB consider using dm_sum_real
#ifdef DM_PARALLEL
   call mpi_allreduce( partial_lhs, adj_ttl_lhs, 1, true_mpi_real, mpi_sum, &
                       comm, ierr) 
#else
   adj_ttl_lhs = partial_lhs
#endif
   
end subroutine da_get_y_lhs_value


