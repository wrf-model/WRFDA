subroutine da_oi_stats_geoamv( stats_unit, oi )

   !-------------------------------------------------------------------------
   ! Purpose: Calculates (Obs - Analysis) statistics for Geo. AMV's
   !-------------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI

   type (stats_geoamv_type)         :: geoamv
   integer                          :: nu, nv
   integer                          :: n, k

   nu = 0
   nv = 0

   geoamv%maximum%u = maxmin_type( missing_r, 0, 0)
   geoamv%maximum%v = maxmin_type( missing_r, 0, 0)
   geoamv%minimum%u = maxmin_type(-missing_r, 0, 0)
   geoamv%minimum%v = maxmin_type(-missing_r, 0, 0)
   geoamv%average = residual_geoamv1_type( 0.0, 0.0)
   geoamv%rms_err = geoamv%average

   if ( oi%num_geoamv > 0 ) then
      do n=1, oi%num_geoamv
         if( oi%geoamv(n)%loc%proc_domain ) then
            do k=1, oi%geoamv(n)%info%levels
               call da_stats_calculate( oi%geoamv(n)%loc%obs_global_index, &
                                   0, oi%geoamv(n)%u(k)%qc, &
                                   oi%geoamv(n)%u(k)%inv, nu, &
                                   geoamv%minimum%u, geoamv%maximum%u, &
                                   geoamv%average%u, geoamv%rms_err%u )

               call da_stats_calculate( oi%geoamv(n)%loc%obs_global_index, &
                                   0, oi%geoamv(n)%v(k)%qc, &
                                   oi%geoamv(n)%v(k)%inv, nv, &
                                   geoamv%minimum%v, geoamv%maximum%v, &
                                  geoamv%average%v, geoamv%rms_err%v )
           end do
        end if    ! end if( oi%geoamv(n)%loc%proc_domain )
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int( nu )
   call da_proc_sum_int( nv )
   call da_proc_stats_combine(geoamv%average%u, geoamv%rms_err%u, &
                           geoamv%minimum%u%value, geoamv%maximum%u%value, &
                           geoamv%minimum%u%n, geoamv%maximum%u%n, &
                           geoamv%minimum%u%l, geoamv%maximum%u%l )
   call da_proc_stats_combine(geoamv%average%v, geoamv%rms_err%v, &
                           geoamv%minimum%v%value, geoamv%maximum%v%value, &
                           geoamv%minimum%v%n, geoamv%maximum%v%n, &
                           geoamv%minimum%v%l, geoamv%maximum%v%l )


   if ( rootproc ) then
      if (nu /= 0 .or. nv /= 0 ) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for geoamv'
         call da_print_stats_geoamv( stats_unit, nu, nv, geoamv )
      end if
   end if

end subroutine da_oi_stats_geoamv


