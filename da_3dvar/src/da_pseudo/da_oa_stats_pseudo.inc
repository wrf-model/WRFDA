SUBROUTINE da_oa_stats_pseudo ( stats_unit, oi, re )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! O-B
   TYPE ( y_type), INTENT (in)      :: re            ! O-A

   TYPE (stats_pseudo_type)         :: pseudo
   INTEGER                          :: nu, nv, nt, np, nq
   INTEGER                          :: n
   real                             :: o_minus_b, o_minus_a, sigma_o, sigma_b

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nu = 0
   nv = 0
   nt = 0
   np = 0
   nq = 0
   
   pseudo%maximum%u = maxmin_type( missing_r, 0, 0)
   pseudo%maximum%v = maxmin_type( missing_r, 0, 0)
   pseudo%maximum%t = maxmin_type( missing_r, 0, 0)
   pseudo%maximum%p = maxmin_type( missing_r, 0, 0)
   pseudo%maximum%q = maxmin_type( missing_r, 0, 0)
   pseudo%minimum%u = maxmin_type(-missing_r, 0, 0)
   pseudo%minimum%v = maxmin_type(-missing_r, 0, 0)
   pseudo%minimum%t = maxmin_type(-missing_r, 0, 0)
   pseudo%minimum%p = maxmin_type(-missing_r, 0, 0)
   pseudo%minimum%q = maxmin_type(-missing_r, 0, 0)
   pseudo%average = residual_pseudo1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   pseudo%rms_err = pseudo%average

   IF ( oi%num_pseudo > 0 ) THEN

      DO n=1, oi%num_pseudo

       if( oi%pseudo(n)%loc%proc_domain ) then

         CALL Stats_Calculate( n, 0, oi%pseudo(n)%u%qc, & 
                               re%pseudo(n)%u, nu, & 
                               pseudo%minimum%u, pseudo%maximum%u, &
                               pseudo%average%u, pseudo%rms_err%u )
         CALL Stats_Calculate( n, 0, oi%pseudo(n)%v%qc, & 
                               re%pseudo(n)%v, nv, & 
                               pseudo%minimum%v, pseudo%maximum%v, &
                               pseudo%average%v, pseudo%rms_err%v )
         CALL Stats_Calculate( n, 0, oi%pseudo(n)%t%qc, & 
                               re%pseudo(n)%t, nt, & 
                               pseudo%minimum%t, pseudo%maximum%t, &
                               pseudo%average%t, pseudo%rms_err%t )
         CALL Stats_Calculate( n, 0, oi%pseudo(n)%p%qc, & 
                               re%pseudo(n)%p, np, & 
                               pseudo%minimum%p, pseudo%maximum%p, &
                               pseudo%average%p, pseudo%rms_err%p )
         CALL Stats_Calculate( n, 0, oi%pseudo(n)%q%qc, & 
                               re%pseudo(n)%q, nq, & 
                               pseudo%minimum%q, pseudo%maximum%q, &
                               pseudo%average%q, pseudo%rms_err%q )
         
         if ( nu > 0 ) then
            o_minus_b = oi%pseudo(n)%u%inv
            o_minus_a = re%pseudo(n)%u
            sigma_o   = oi%pseudo(n)%u%error
         else if ( nv > 0 ) then
            o_minus_b = oi%pseudo(n)%v%inv
            o_minus_a = re%pseudo(n)%v
            sigma_o   = oi%pseudo(n)%v%error
         else if ( nt > 0 ) then
            o_minus_b = oi%pseudo(n)%t%inv
            o_minus_a = re%pseudo(n)%t
            sigma_o   = oi%pseudo(n)%t%error
         else if ( np > 0 ) then
            o_minus_b = oi%pseudo(n)%p%inv
            o_minus_a = re%pseudo(n)%p
            sigma_o   = oi%pseudo(n)%p%error
         else if ( nq > 0 ) then
            o_minus_b = oi%pseudo(n)%q%inv
            o_minus_a = re%pseudo(n)%q
            sigma_o   = oi%pseudo(n)%q%error
         end if

         write(stats_unit,'(/A,A1,A,e15.5)')' Pseudo ', pseudo_var, ' O-B: ', o_minus_b
         write(stats_unit,'( A,A1,A,e15.5)')' Pseudo ', pseudo_var, ' O-A: ', o_minus_a
         write(stats_unit,'( A,A1,A,e15.5)')' Pseudo ', pseudo_var, ' sigma_o: ', sigma_o

!        Calculate equivalent sigma_b using O-A=(O-B)*sigma_o/(sigma_o+sigma_b)
         sigma_b = SQRT( (o_minus_b - o_minus_a) / o_minus_a ) * sigma_o
         write(stats_unit,'(A,A1,A,e15.5)')' Pseudo ', pseudo_var, ' sigma_b: ', sigma_b
         
       endif    ! end if( oi%pseudo(n)%loc%proc_domain )

      END DO
      
   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   call proc_sum_int( nt )
   call proc_sum_int( np )
   call proc_sum_int( nq )
   
   call proc_stats_combine(Pseudo%average%u, Pseudo%rms_err%u, &
                           Pseudo%minimum%u%value, Pseudo%maximum%u%value, &
                           Pseudo%minimum%u%n, Pseudo%maximum%u%n, &
                           Pseudo%minimum%u%l, Pseudo%maximum%u%l )
   call proc_stats_combine(Pseudo%average%v, Pseudo%rms_err%v, &
                           Pseudo%minimum%v%value, Pseudo%maximum%v%value, &
                           Pseudo%minimum%v%n, Pseudo%maximum%v%n, &
                           Pseudo%minimum%v%l, Pseudo%maximum%v%l )
   call proc_stats_combine(Pseudo%average%t, Pseudo%rms_err%t, &
                           Pseudo%minimum%t%value, Pseudo%maximum%t%value, &
                           Pseudo%minimum%t%n, Pseudo%maximum%t%n, &
                           Pseudo%minimum%t%l, Pseudo%maximum%t%l )
   call proc_stats_combine(Pseudo%average%p, Pseudo%rms_err%p, &
                           Pseudo%minimum%p%value, Pseudo%maximum%p%value, &
                           Pseudo%minimum%p%n, Pseudo%maximum%p%n, &
                           Pseudo%minimum%p%l, Pseudo%maximum%p%l )
   call proc_stats_combine(Pseudo%average%q, Pseudo%rms_err%q, &
                           Pseudo%minimum%q%value, Pseudo%maximum%q%value, &
                           Pseudo%minimum%q%n, Pseudo%maximum%q%n, &
                           Pseudo%minimum%q%l, Pseudo%maximum%q%l )
   
   IF ( wrf_dm_on_monitor() ) THEN
   
    IF ( nu /= 0 .or. nv /= 0 .OR. nt /= 0 .or. np /= 0 .OR. nq /= 0 ) THEN
   
     write(unit=stats_unit, fmt='(/a/)') ' O-A Diagnostics for pseudo'
   
     CALL da_print_stats_pseudo( stats_unit, nu, nv, nt, np, nq, pseudo )
   
    END IF
   
   END IF

END SUBROUTINE da_oa_stats_pseudo
