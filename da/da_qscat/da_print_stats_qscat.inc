subroutine da_print_stats_qscat(stats_unit, nu, nv, qscat)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit  
   integer,                 intent(inout) :: nu, nv      
   type (stats_qscat_type), intent(in)    :: qscat

   if (trace_use_dull) call da_trace_entry("da_print_stats_qscat")

      write(unit=stats_unit, fmt='(3a/)') &
        '   var             ', &
        'u (m/s)     n    k    ', &
        'v (m/s)     n    k    '

   write(unit=stats_unit, fmt='(a,i16,4i22)') &
        '  Number: ', nu, nv

   if (nu < 1) nu = 1
   if (nv < 1) nv = 1

   write(unit=stats_unit, fmt='((a,2(f12.4,2i5)))') &
        ' Minimum(n,k): ', qscat%minimum%u, qscat%minimum%v, &
        ' Maximum(n,k): ', qscat%maximum%u, qscat%maximum%v
   
   write(unit=stats_unit, fmt='((a,2(f12.4)))') &
        ' Average     : ', qscat%average%u/real(nu), qscat%average%v/real(nv), &
        '    RMSE     : ', sqrt(qscat%rms_err%u/real(nu)), &
                      sqrt(qscat%rms_err%v/real(nv))

   if (trace_use_dull) call da_trace_exit("da_print_stats_qscat")
   
end subroutine da_print_stats_qscat


ad(i)%ichan(1:iv%instid(i)%nchan)%maximum%n      = 1
      do k=1,iv%instid(i)%nchan
         rad(i)%ichan(k)%minimum%l      = k
         rad(i)%ichan(k)%maximum%l      = k
      end do

      if (iv%instid(i)%num_rad < 1) cycle

      do k=1, iv%instid(i)%nchan               !! loop for channels
         do n=1, iv%instid(i)%num_rad              !! loop for pixels
            if (iv%instid(i)%info%proc_domain(1,n)) then
               call da_stats_calculate( n,k,iv%instid(i)%tb_qc(k,n), &
                                 re%instid(i)%tb(k,n), rad(i)%ichan(k)%num, &
                                 rad(i)%ichan(k)%minimum, rad(i)%ichan(k)%maximum, &
                                 rad(i)%ichan(k)%ave, rad(i)%ichan(k)%rms)

            end if          ! end if( oi%sound(n)%loc%proc_domain )
         end do                                 !! end loop for pixels
      end do                        !  end loop for channels
   end do                         !  end loop for sensor

   do i = 1, iv%num_inst                          !! loop for sensors
      do k=1, iv%instid(i)%nchan               !! loop for channels
         ! FIX? generate 1D array to allow a da_proc_sum_ints call here
         ! Do inter-processor communication to gather statistics.
         call da_proc_sum_int ( rad(i)%ichan(k)%num )
         call da_proc_stats_combine(rad(i)%ichan(k)%ave, rad(i)%ichan(k)%rms, &
                           rad(i)%ichan(k)%minimum%value, rad(i)%ichan(k)%maximum%value, &
                           rad(i)%ichan(k)%minimum%n, rad(i)%ichan(k)%maximum%n, &
                           rad(i)%ichan(k)%minimum%l, rad(i)%ichan(k)%maximum%l )
 
         iv%nstats(radiance) = iv%nstats(radiance) + rad(i)%ichan(k)%num
      end do                        !  end loop for channels

      if (rootproc) then
         if (any(rad(i)%ichan(:)%num /= 0)) then
            write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for radiance         '//iv%instid(i)%rttovid_string
            call da_print_stats_rad( stats_unit, iv%instid(i)%nchan, rad(i) )
         end if
      end if
   end do                         !  end loop for sensor

   do i = 1, iv%num_inst           ! loop for sensors
      deallocate (rad(i)%ichan)
   end do

   deallocate (rad)

   if (trace_use) call da_trace_exit("da_ao_stats_rad")

end subroutine da_ao_stats_rad


