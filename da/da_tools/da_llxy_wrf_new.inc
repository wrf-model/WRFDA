subroutine da_llxy_wrf_new(proj, info)

   !-----------------------------------------------------------------------
   ! Purpose: Converts input lat/lon values to the cartesian (x,y) value
   ! for the given projection. 
   !-----------------------------------------------------------------------

   implicit none

   type(proj_info), intent(in)    :: proj
   type(infa_type), intent(inout) :: info

   if (trace_use) call da_trace_entry("da_llxy_wrf_new")

   if (.NOT.proj%init) then
      call da_error(__FILE__,__LINE__, &
        (/"You have not called map_set for this projection!"/))
   end if

   select case(proj%code)
 
      case(PROJ_LATLON)
         call da_llxy_latlon_new(proj,info)

      case(PROJ_MERC)
         call da_llxy_merc_new(proj,info)
         info%x(:,:) = info%x(:,:) + proj%knowni - 1.0
         info%y(:,:) = info%y(:,:) + proj%knownj - 1.0

      case(PROJ_PS)
         call da_llxy_ps_new(proj,info)
      
      case(PROJ_LC)
         call da_llxy_lc_new(proj,info)
         info%x(:,:) = info%x(:,:) + proj%knowni - 1.0
         info%y(:,:) = info%y(:,:) + proj%knownj - 1.0

      case default
         write(unit=message(1),fmt='(A,I2)') &
            'Unrecognized map projection code: ', proj%code
         call da_error(__FILE__,__LINE__,message(1:1))
   end select

   if (trace_use) call da_trace_exit("da_llxy_wrf_new")

end subroutine da_llxy_wrf_new


 &
                       metar%average%t/real(nt), metar%average%p/real(np), &
                       metar%average%q/real(nq), &
      '    RMSE     : ', sqrt(metar%rms_err%u/real(nu)), &
                       sqrt(metar%rms_err%v/real(nv)), &
                       sqrt(metar%rms_err%t/real(nt)), &
                       sqrt(metar%rms_err%p/real(np)), &
                       sqrt(metar%rms_err%q/real(nq))

   if (trace_use_dull) call da_trace_exit("da_print_stats_metar")

end subroutine da_print_stats_metar


ne(stats%average%v, stats%rms_err%v, &
      stats%minimum%v%value, stats%maximum%v%value, &
      stats%minimum%v%n, stats%maximum%v%n, &
      stats%minimum%v%l, stats%maximum%v%l)

   if (rootproc) then
      if (nu /= 0 .or. nv /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for pilot'
         call da_print_stats_pilot(stats_unit, nu, nv, stats)
      end if
   end if
   
   if (trace_use_dull) call da_trace_exit("da_ao_stats_pilot")

end subroutine da_ao_stats_pilot


