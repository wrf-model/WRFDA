SUBROUTINE DA_OI_Stats_GPSRef ( stats_unit, oi )

! ----------------------------------------------------------------------
!  Called by DA_Minimisation/da_write_diagnostics.inc.
!
!                                           Y.-R. Guo  06/02/2004
! ----------------------------------------------------------------------
   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI

   TYPE (stats_gpsref_type)          :: GPSRef
   INTEGER                          :: ngpsref
   INTEGER                          :: n, k

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   ngpsref = 0
   
   GPSRef%maximum%ref = maxmin_type( missing_r, 0, 0)
   GPSRef%minimum%ref = maxmin_type(-missing_r, 0, 0)
   GPSRef%average = residual_gpsref1_type(0.0,0.0,0.0,0.0)
   GPSRef%rms_err = GPSRef%average

   DO n=1, oi%num_GPSRef

      if( oi%gpsref(n)%loc%proc_domain ) then
         do k=1, oi%gpsref(n)%info%levels

            CALL Stats_Calculate( n, k, oi%GPSRef(n)%ref(k)%qc, &
                          oi%GPSRef(n)%ref(k)%inv, ngpsref, &
                          GPSRef%minimum%ref, GPSRef%maximum%ref, &
                          GPSRef%average%ref, GPSRef%rms_err%ref)
         end do
      endif    ! end if( oi%gpsref(n)%loc%proc_domain )
   END DO

!--Do inter-processor communication to gather statistics.

   call proc_sum_int( ngpsref )
   
   call proc_stats_combine(GPSRef%average%ref, GPSRef%rms_err%ref, &
             GPSRef%minimum%ref%value, GPSRef%maximum%ref%value, &
             GPSRef%minimum%ref%n, GPSRef%maximum%ref%n, &
             GPSRef%minimum%ref%l, GPSRef%maximum%ref%l )
   
   IF ( wrf_dm_on_monitor() .and. (ngpsref > 0)) THEN
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for GPSRef'
   
      CALL DA_Print_Stats_GPSRef( stats_unit, ngpsref, GPSRef )
   END IF

END SUBROUTINE DA_OI_Stats_GPSRef

