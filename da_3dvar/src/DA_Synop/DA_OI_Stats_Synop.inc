SUBROUTINE DA_OI_Stats_Synop ( stats_unit, oi )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI

   TYPE (stats_synop_type)          :: synop
   INTEGER                          :: nu, nv, nt, np, nq
   INTEGER                          :: n

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nu = 0
   nv = 0
   nt = 0
   np = 0
   nq = 0
   
   synop%maximum%u = maxmin_type( missing_r, 0, 0)
   synop%maximum%v = maxmin_type( missing_r, 0, 0)
   synop%maximum%t = maxmin_type( missing_r, 0, 0)
   synop%maximum%p = maxmin_type( missing_r, 0, 0)
   synop%maximum%q = maxmin_type( missing_r, 0, 0)
   synop%minimum%u = maxmin_type(-missing_r, 0, 0)
   synop%minimum%v = maxmin_type(-missing_r, 0, 0)
   synop%minimum%t = maxmin_type(-missing_r, 0, 0)
   synop%minimum%p = maxmin_type(-missing_r, 0, 0)
   synop%minimum%q = maxmin_type(-missing_r, 0, 0)
   synop%average = residual_synop1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   synop%rms_err = synop%average

   IF ( oi%num_synop > 0 ) THEN

      DO n=1, oi%num_synop

       if( oi%synop(n)%loc%proc_domain ) then

         CALL Stats_Calculate( n, 0, oi%synop(n)%u%qc, &
                               oi%synop(n)%u%inv, nu, &
                               synop%minimum%u, synop%maximum%u, &
                               synop%average%u, synop%rms_err%u)
         CALL Stats_Calculate( n, 0, oi%synop(n)%v%qc, &
                               oi%synop(n)%v%inv, nv, &
                               synop%minimum%v, synop%maximum%v, &
                               synop%average%v, synop%rms_err%v)
         CALL Stats_Calculate( n, 0, oi%synop(n)%t%qc, &
                               oi%synop(n)%t%inv, nt, &
                               synop%minimum%t, synop%maximum%t, &
                               synop%average%t, synop%rms_err%t)
         CALL Stats_Calculate( n, 0, oi%synop(n)%p%qc, &
                               oi%synop(n)%p%inv, np, &
                               synop%minimum%p, synop%maximum%p, &
                               synop%average%p, synop%rms_err%p)

         CALL Stats_Calculate( n, 0, oi%synop(n)%q%qc, &
                               oi%synop(n)%q%inv, nq, &
                               synop%minimum%q, synop%maximum%q, &
                               synop%average%q, synop%rms_err%q )

       endif    ! end if( oi%synop(n)%loc%proc_domain )

      END DO

   END IF

! Do inter-processor communication to gather statistics.
    call proc_sum_int( nu )
    call proc_sum_int( nv )
    call proc_sum_int( nt )
    call proc_sum_int( np )
    call proc_sum_int( nq )
   
    call proc_stats_combine(Synop%average%u, Synop%rms_err%u, &
                            Synop%minimum%u%value, Synop%maximum%u%value, &
                            Synop%minimum%u%n, Synop%maximum%u%n, &
                            Synop%minimum%u%l, Synop%maximum%u%l )
   
    call proc_stats_combine(Synop%average%v, Synop%rms_err%v, &
                            Synop%minimum%v%value, Synop%maximum%v%value, &
                            Synop%minimum%v%n, Synop%maximum%v%n, &
                            Synop%minimum%v%l, Synop%maximum%v%l )
   
    call proc_stats_combine(Synop%average%t, Synop%rms_err%t, &
                            Synop%minimum%t%value, Synop%maximum%t%value, &
                            Synop%minimum%t%n, Synop%maximum%t%n, &
                            Synop%minimum%t%l, Synop%maximum%t%l )
   
    call proc_stats_combine(Synop%average%p, Synop%rms_err%p, &
                            Synop%minimum%p%value, Synop%maximum%p%value, &
                            Synop%minimum%p%n, Synop%maximum%p%n, &
                            Synop%minimum%p%l, Synop%maximum%p%l )
   
    call proc_stats_combine(Synop%average%q, Synop%rms_err%q, &
                            Synop%minimum%q%value, Synop%maximum%q%value, &
                            Synop%minimum%q%n, Synop%maximum%q%n, &
                            Synop%minimum%q%l, Synop%maximum%q%l )
   
    IF ( wrf_dm_on_monitor() ) THEN
   
      IF (nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. np /= 0 .or. nq /= 0 ) THEN
   
       write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for synop'
   
       CALL DA_Print_Stats_Synop( stats_unit, nu, nv, nt, np, nq, synop )
   
!    min_val = -50.0
!    max_val = 50.0
!    bin_width = 1.0
!    CALL DA_Data_Distribution( 'synop u ', oi%num_synop, min_val, max_val, &
!                               bin_width, oi%synop(:)%u%inv )
!    CALL DA_Data_Distribution( 'synop v ', oi%num_synop, min_val, max_val, &
!                               bin_width, oi%synop(:)%v%inv )
!    CALL DA_Data_Distribution( 'synop t ', oi%num_synop, min_val, max_val, &
!                               bin_width, oi%synop(:)%t%inv )
!    CALL DA_Data_Distribution( 'synop p ', oi%num_synop, -1000.0, 1000.0, &
!                               50.0, oi%synop(:)%p%inv )
!    CALL DA_Data_Distribution( 'synop q ', oi%num_synop, -0.03, 0.03, &
!                               0.001, oi%synop(:)%q%inv )
     END IF
   
   END IF

END SUBROUTINE DA_OI_Stats_Synop
