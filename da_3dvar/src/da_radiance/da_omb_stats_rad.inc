SUBROUTINE da_omb_stats_rad ( stats_unit, iv )
!------------------------------------------------------------------------------
!  PURPOSE: Calculate statistics of obs minus background for radiance data.
!
!  METHOD:  average, rms, minimum, maximum of iv
!
!  HISTORY: 08/2005 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: iv            ! Innovation

   TYPE (stats_rad_type),    pointer  :: rad(:)
   INTEGER                          :: n, k, i

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   IF ( iv%num_inst < 1 ) RETURN

   WRITE(UNIT=stdout,*) 'stats_unit  = ', stats_unit
   WRITE(UNIT=stdout,*) 'iv%num_inst = ', iv%num_inst

 Allocate ( rad(1:iv%num_inst) )

 do i = 1, iv%num_inst                          !! loop for sensors

!   print *, 'iv%instid(i)%num_rad = ', iv%instid(i)%num_rad
!   print *, 'iv%instid(i)%nchan   = ', iv%instid(i)%nchan

!   Allocate ( rad(i)%ichan(1:iv%instid(i)%nchan) )
   rad(i)%ichan(1:19)%num  = 0
   rad(i)%ichan(1:19)%ave  = 0.0
   rad(i)%ichan(1:19)%rms  = 0.0
   rad(i)%ichan(1:19)%minimum%value  = -missing_r
   rad(i)%ichan(1:19)%maximum%value  =  missing_r
   rad(i)%ichan(1:19)%minimum%n      = 1
   rad(i)%ichan(1:19)%maximum%n      = 1
  do k=1,19
   rad(i)%ichan(k)%minimum%l      = k
   rad(i)%ichan(k)%maximum%l      = k
  end do

   IF ( iv%instid(i)%num_rad < 1 ) cycle

    do k=1, iv%instid(i)%nchan               !! loop for channels

      DO n=1, iv%instid(i)%num_rad              !! loop for pixels

      if( iv%instid(i)%rad(n)%loc%proc_domain ) then

         CALL Stats_Calculate( n,k,iv%instid(i)%rad(n)%tb(k)%qc, &
                               iv%instid(i)%rad(n)%tb(k)%inv, rad(i)%ichan(k)%num, &
                               rad(i)%ichan(k)%minimum, rad(i)%ichan(k)%maximum, &
                               rad(i)%ichan(k)%ave, rad(i)%ichan(k)%rms)

      endif          ! end if( oi%sound(n)%loc%proc_domain )
      END DO                                 !! end loop for pixels

   end do                        !  end loop for channels

  end do                         !  end loop for sensor


  do i = 1, iv%num_inst                          !! loop for sensors
!  IF ( iv%instid(i)%num_rad < 1 ) cycle
  do k=1, iv%instid(i)%nchan               !! loop for channels
! Do inter-processor communication to gather statistics.
   call proc_sum_int ( rad(i)%ichan(k)%num )
!   print *,   rad(i)%ichan(k)%num, rad(i)%ichan(k)%ave, &
!              rad(i)%ichan(k)%rms, rad(i)%ichan(k)%minimum%value, &
!              rad(i)%ichan(k)%maximum%value, &
!            rad(i)%ichan(k)%minimum%n, rad(i)%ichan(k)%maximum%n, &
!            rad(i)%ichan(k)%minimum%l, rad(i)%ichan(k)%maximum%l
   call proc_stats_combine(rad(i)%ichan(k)%ave, rad(i)%ichan(k)%rms, &
                           rad(i)%ichan(k)%minimum%value, rad(i)%ichan(k)%maximum%value, &
                           rad(i)%ichan(k)%minimum%n, rad(i)%ichan(k)%maximum%n, &
                           rad(i)%ichan(k)%minimum%l, rad(i)%ichan(k)%maximum%l )
 
!   print *, ' rad(i)%ichan(k)%ave= ', rad(i)%ichan(k)%ave
  end do                        !  end loop for channels

   IF ( wrf_dm_on_monitor() ) THEN
     write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OMB for Radiance '//iv%instid(i)%rttovid_string 
     CALL DA_Print_Stats_Rad( stats_unit, iv%instid(i)%nchan, rad(i) )
   END IF

  end do                         !  end loop for sensor

! do i = 1, iv%num_inst                          !! loop for sensors
!    IF ( iv%instid(i)%num_rad < 1 ) cycle
!    Deallocate ( rad(i)%ichan )
! end do

    Deallocate ( rad )

END SUBROUTINE da_omb_stats_rad
