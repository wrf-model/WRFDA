SUBROUTINE DA_AO_Stats_Gpspw( stats_unit, oi, re )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI
   TYPE ( y_type), INTENT (in)      :: re            ! A - O

   TYPE (stats_gpspw_type)                :: Gpspw
   INTEGER                          :: ntpw
   INTEGER                          :: n
   real                             :: o_minus_b, o_minus_a, sigma_o, sigma_b
   real                             :: o_minus_b_0, o_minus_a_0, sigma_o_0, sigma_b_0 

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   integer                        :: ierror        ! MPI error code.

   ntpw = 0
   num_gpspw_tot = 0
   o_minus_b_0 = 0.0; o_minus_a_0 = 0.0; sigma_o_0 = 0.0; sigma_b_0 = 0.0

   Gpspw%maximum%tpw = maxmin_type( missing_r, 0, 0)
   Gpspw%minimum%tpw = maxmin_type(-missing_r, 0, 0)
   Gpspw%average = residual_gpspw1_type(0.0)
   Gpspw%rms_err = Gpspw%average

   IF ( oi%num_Gpspw > 0 ) THEN

      DO n=1, oi%num_Gpspw

       if( oi%gpspw(n)%loc%proc_domain ) then

        CALL  Stats_Calculate( n, 0, oi%gpspw(n)%tpw%qc, &
                               re%gpspw(n)%tpw, ntpw,   &
                               gpspw%minimum%tpw,  gpspw%maximum%tpw, &
                               gpspw%average%tpw,  gpspw%rms_err%tpw)

           if ( pseudo_var(1:3) == 'tpw' .and. num_pseudo > 0) then

                o_minus_b = oi%GPSpw(n)%tpw%inv
                o_minus_a = re%gpspw(n)%tpw
                sigma_o   = oi%gpspw(n)%tpw%error

    !          Calculate equivalent sigma_b using
    ! 
    !          O-A=(O-B)*sigma_o/(sigma_o+sigma_b)

                sigma_b = SQRT( (o_minus_b - o_minus_a) &
                       / o_minus_a ) * sigma_o
#ifdef DM_PARALLEL  
#if 0
   call MPI_ALLREDUCE( o_minus_b, o_minus_b_0, 1, TRUE_MPI_REAL, MPI_SUM, &  
                       MPI_COMM_WORLD, IERROR )  
   call MPI_ALLREDUCE( o_minus_a, o_minus_a_0, 1, TRUE_MPI_REAL, MPI_SUM, &  
                       MPI_COMM_WORLD, IERROR )  
   call MPI_ALLREDUCE( sigma_o, sigma_o_0, 1, TRUE_MPI_REAL, MPI_SUM, &  
                       MPI_COMM_WORLD, IERROR )  
   call MPI_ALLREDUCE( sigma_b, sigma_b_0, 1, TRUE_MPI_REAL, MPI_SUM, &  
                       MPI_COMM_WORLD, IERROR )  
   IF ( wrf_dm_on_monitor() ) THEN  
                write(stats_unit,'(/A,A3,A,f12.3)')  &
                         ' Pseudo ', pseudo_var, ' O-B: ', o_minus_b
                write(stats_unit,'( A,A3,A,f12.3)')  &
                         ' Pseudo ', pseudo_var, ' O-A: ', o_minus_a
                write(stats_unit,'( A,A3,A,f12.3)')  &
                         ' Pseudo ', pseudo_var, ' sigma_o: ', sigma_o
                write(stats_unit,'(A,A3,A,f12.3)')  &
                         ' Pseudo ', pseudo_var, ' sigma_b: ', sigma_b
   ENDIF  
#endif
#else  
   o_minus_b_0 = o_minus_b 
   o_minus_a_0 = o_minus_a 
   sigma_o_0   = sigma_o 
   sigma_b_0   = sigma_b 
             write(stats_unit,'(/A,A3,A,f12.3)')  &  
                         ' Pseudo ', pseudo_var, ' O-B: ', o_minus_b_0  
             write(stats_unit,'( A,A3,A,f12.3)')  &  
                         ' Pseudo ', pseudo_var, ' O-A: ', o_minus_a_0  
             write(stats_unit,'( A,A3,A,f12.3)')  &  
                         ' Pseudo ', pseudo_var, ' sigma_o: ', sigma_o_0  
             write(stats_unit,'(A,A3,A,f12.3)')  &  
                         ' Pseudo ', pseudo_var, ' sigma_b: ', sigma_b_0 
#endif  

           endif

       endif    ! end if( oi%gpspw(n)%loc%proc_domain )

      END DO
   end if

! Do inter-processor communication to gather statistics.
   call proc_sum_int( ntpw )
   num_gpspw_tot = ntpw
   
   call proc_stats_combine(Gpspw%average%tpw, Gpspw%rms_err%tpw, &
                           Gpspw%minimum%tpw%value, Gpspw%maximum%tpw%value, &
                           Gpspw%minimum%tpw%n, Gpspw%maximum%tpw%n, &
                           Gpspw%minimum%tpw%l, Gpspw%maximum%tpw%l )
   
   IF ( wrf_dm_on_monitor() ) THEN
   
    IF ( ntpw /= 0) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for Gpspw'
   
      call DA_Print_Stats_Gpspw(stats_unit, ntpw, Gpspw)
   
    END IF
   
   END IF

END SUBROUTINE DA_AO_Stats_Gpspw

