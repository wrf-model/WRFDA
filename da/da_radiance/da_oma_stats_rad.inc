subroutine da_oma_stats_rad ( stats_unit, iv, re )

   !---------------------------------------------------------------------------
   ! PURPOSE: Calculate statistics of obs minus analysis for radiance data.
   !
   ! METHOD:  average, rms, minimum, maximum of re
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: iv            ! Innovation
   TYPE (y_type),  INTENT (in)      :: re            ! o-a

   TYPE (stats_rad_type),    pointer  :: rad(:)
   INTEGER                          :: n, k, i

   IF ( iv%num_inst < 1 ) RETURN

   if (trace_use) call da_trace_entry("da_oma_stats_rad")

   Allocate ( rad(1:iv%num_inst) )

   num_radiance_tot = 0

   do i = 1, iv%num_inst                          !! loop for sensors

      Allocate ( rad(i)%ichan(1:iv%instid(i)%nchan) )
      rad(i)%ichan(1:iv%instid(i)%nchan)%num  = 0
      rad(i)%ichan(1:iv%instid(i)%nchan)%ave  = 0.0
      rad(i)%ichan(1:iv%instid(i)%nchan)%rms  = 0.0
      rad(i)%ichan(1:iv%instid(i)%nchan)%minimum%value  = -missing_r
      rad(i)%ichan(1:iv%instid(i)%nchan)%maximum%value  =  missing_r
      rad(i)%ichan(1:iv%instid(i)%nchan)%minimum%n      = 1
      rad(i)%ichan(1:iv%instid(i)%nchan)%maximum%n      = 1
      do k=1,iv%instid(i)%nchan
         rad(i)%ichan(k)%minimum%l      = k
         rad(i)%ichan(k)%maximum%l      = k
      end do

      IF ( iv%instid(i)%num_rad < 1 ) cycle

      do k=1, iv%instid(i)%nchan               !! loop for channels
         DO n=1, iv%instid(i)%num_rad              !! loop for pixels
            if (iv%instid(i)%rad(n)%loc%proc_domain) then
               CALL da_stats_calculate( n,k,iv%instid(i)%rad(n)%tb(k)%qc, &
                                 re%instid(i)%rad(n)%tb(k), rad(i)%ichan(k)%num, &
                                 rad(i)%ichan(k)%minimum, rad(i)%ichan(k)%maximum, &
                                 rad(i)%ichan(k)%ave, rad(i)%ichan(k)%rms)

            endif          ! end if( oi%sound(n)%loc%proc_domain )
         END DO                                 !! end loop for pixels
      end do                        !  end loop for channels
   end do                         !  end loop for sensor

   do i = 1, iv%num_inst                          !! loop for sensors
      do k=1, iv%instid(i)%nchan               !! loop for channels
         ! Do inter-processor communication to gather statistics.
         call da_proc_sum_int ( rad(i)%ichan(k)%num )
         call da_proc_stats_combine(rad(i)%ichan(k)%ave, rad(i)%ichan(k)%rms, &
                           rad(i)%ichan(k)%minimum%value, rad(i)%ichan(k)%maximum%value, &
                           rad(i)%ichan(k)%minimum%n, rad(i)%ichan(k)%maximum%n, &
                           rad(i)%ichan(k)%minimum%l, rad(i)%ichan(k)%maximum%l )
 
         num_radiance_tot = num_radiance_tot + rad(i)%ichan(k)%num
      end do                        !  end loop for channels

      IF (rootproc) THEN
         IF (any(rad(i)%ichan(:)%num /= 0)) then
            write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for Radiance         '//iv%instid(i)%rttovid_string 
            CALL da_print_stats_rad( stats_unit, iv%instid(i)%nchan, rad(i) )
         END IF
      END IF
   end do                         !  end loop for sensor

   do i = 1, iv%num_inst           ! loop for sensors
      Deallocate ( rad(i)%ichan )
   end do

   Deallocate ( rad )

   if (trace_use) call da_trace_exit("da_oma_stats_rad")

end subroutine da_oma_stats_rad


