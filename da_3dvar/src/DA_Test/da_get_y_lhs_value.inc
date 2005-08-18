SUBROUTINE da_get_y_lhs_value( iv, y, &
                               partial_lhs, pertile_lhs, adj_ttl_lhs )
   
!------------------------------------------------------------------------------
   
   implicit none
   
   type (ob_type),    intent(in)    :: iv    ! ob. increment vector.
   TYPE (y_type),     intent(inout) :: y     ! y = h (xa)

   real,              intent(out)   :: partial_lhs, pertile_lhs, adj_ttl_lhs

!-------------------------------------------------------------------------

   integer                        :: ierror        ! MPI error code.

!-------------------------------------------------------------------------

   partial_lhs = 0.0
   pertile_lhs = 0.0

   if( Use_SoundObs ) then
       call da_check_xtoy_adjoint_Sound( iv, y, partial_lhs, pertile_lhs )
       call da_check_xtoy_adjoint_Sonde_sfc( iv, y, partial_lhs, pertile_lhs )
   endif
   if( Use_SynopObs ) &
   call da_check_xtoy_adjoint_Synop( iv, y, partial_lhs, pertile_lhs )

   if( Use_GeoAMVObs ) &
   call da_check_xtoy_adjoint_GeoAMV( iv, y, partial_lhs, pertile_lhs )

   if( Use_PolarAMVObs ) &
   call da_check_xtoy_adjoint_PolarAMV( iv, y, partial_lhs, pertile_lhs )

   if( Use_AirepObs ) &
   call da_check_xtoy_adjoint_Airep( iv, y, partial_lhs, pertile_lhs )

   if( Use_PilotObs ) &
   call da_check_xtoy_adjoint_Pilot( iv, y, partial_lhs, pertile_lhs )

   if( Use_RadarObs ) &
   call da_check_xtoy_adjoint_Radar( iv, y, partial_lhs, pertile_lhs )

   if( Use_SatemObs ) &
   call da_check_xtoy_adjoint_Satem( iv, y, partial_lhs, pertile_lhs )

   if( Use_MetarObs ) &
   call da_check_xtoy_adjoint_Metar( iv, y, partial_lhs, pertile_lhs )
 
   if( Use_ShipsObs ) &
   call da_check_xtoy_adjoint_Ships( iv, y, partial_lhs, pertile_lhs )

   if( Use_GpspwObs ) &
   call da_check_xtoy_adjoint_Gpspw( iv, y, partial_lhs, pertile_lhs )

   if ( use_GpsrefObs ) &
   call da_check_xtoy_adjoint_Gpsref( iv, y, partial_lhs, pertile_lhs )
   
   if( Use_SsmiTbObs .or. Use_SsmiRetrievalObs ) &
   call da_check_xtoy_adjoint_SSMI ( iv, y, partial_lhs, pertile_lhs )

   if( use_ssmt1obs ) &
   call da_check_xtoy_adjoint_ssmt1( iv, y, partial_lhs, pertile_lhs )

   if( use_ssmt2obs ) &
   call da_check_xtoy_adjoint_ssmt2( iv, y, partial_lhs, pertile_lhs )

   if( use_qscatobs ) &
   call da_check_xtoy_adjoint_qscat( iv, y, partial_lhs, pertile_lhs )

   if( Use_ProfilerObs ) &
   call da_check_xtoy_adjoint_Profiler( iv, y, partial_lhs, pertile_lhs )

   if( Use_BuoyObs ) &
   call da_check_xtoy_adjoint_Buoy( iv, y, partial_lhs, pertile_lhs )

   if( Use_BogusObs ) &
   call da_check_xtoy_adjoint_Bogus( iv, y, partial_lhs, pertile_lhs )

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( partial_lhs, adj_ttl_lhs, 1, TRUE_MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )
#else
   adj_ttl_lhs = partial_lhs
#endif
   
END SUBROUTINE da_get_y_lhs_value

