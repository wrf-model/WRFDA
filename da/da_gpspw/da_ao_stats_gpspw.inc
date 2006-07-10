subroutine da_ao_stats_gpspw( stats_unit, oi, re )

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

   ntpw = 0
   num_gpspw_tot = 0
   o_minus_b_0 = 0.0; o_minus_a_0 = 0.0; sigma_o_0 = 0.0; sigma_b_0 = 0.0

   Gpspw%maximum%tpw = maxmin_type( missing_r, 0, 0)
   Gpspw%minimum%tpw = maxmin_type(-missing_r, 0, 0)
   Gpspw%average = residual_gpspw1_type(0.0)
   Gpspw%rms_err = Gpspw%average

   IF ( oi%num_Gpspw > 0 ) THEN
      DO n=1, oi%num_Gpspw
         if (oi%gpspw(n)%loc%proc_domain) then
            call  da_stats_calculate( n, 0, oi%gpspw(n)%tpw%qc, &
                                  re%gpspw(n)%tpw, ntpw,   &
                                  gpspw%minimum%tpw,  gpspw%maximum%tpw, &
                                  gpspw%average%tpw,  gpspw%rms_err%tpw)

            if ( pseudo_var(1:3) == 'tpw' .and. num_pseudo > 0) then
               o_minus_b = oi%GPSpw(n)%tpw%inv
               o_minus_a = re%gpspw(n)%tpw
               sigma_o   = oi%gpspw(n)%tpw%error

               ! Calculate equivalent sigma_b using
               ! 
               ! O-A=(O-B)*sigma_o/(sigma_o+sigma_b)

               sigma_b = SQRT( (o_minus_b - o_minus_a) / o_minus_a ) * sigma_o
               o_minus_b_0 = wrf_dm_sum_real( o_minus_b )
               o_minus_a_0 = wrf_dm_sum_real( o_minus_a )
               sigma_o_0 = wrf_dm_sum_real( sigma_o )
               sigma_b_0 = wrf_dm_sum_real( sigma_b )
               PRINT *,'TEST_COVERAGE_DA_AO_Stats_Gpspw:  o_minus_b_0 = ', o_minus_b_0
               PRINT *,'TEST_COVERAGE_DA_AO_Stats_Gpspw:  o_minus_a_0 = ', o_minus_a_0
               PRINT *,'TEST_COVERAGE_DA_AO_Stats_Gpspw:  sigma_o_0 = ', sigma_o_0
               PRINT *,'TEST_COVERAGE_DA_AO_Stats_Gpspw:  sigma_b_0 = ', sigma_b_0
               IF ( wrf_dm_on_monitor() ) THEN  
                  write(stats_unit,'(/A,A3,A,f12.3)')  &  
                    ' Pseudo ', pseudo_var, ' O-B: ', o_minus_b_0  
                  write(stats_unit,'( A,A3,A,f12.3)')  &  
                    ' Pseudo ', pseudo_var, ' O-A: ', o_minus_a_0  
                  write(stats_unit,'( A,A3,A,f12.3)')  &  
                    ' Pseudo ', pseudo_var, ' sigma_o: ', sigma_o_0  
                  write(stats_unit,'(A,A3,A,f12.3)')  &  
                    ' Pseudo ', pseudo_var, ' sigma_b: ', sigma_b_0 
               ENDIF  
            endif
         endif    ! end if( oi%gpspw(n)%loc%proc_domain )
      END DO
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int( ntpw )
   num_gpspw_tot = ntpw

   call da_proc_stats_combine(Gpspw%average%tpw, Gpspw%rms_err%tpw, &
                           Gpspw%minimum%tpw%value, Gpspw%maximum%tpw%value, &
                           Gpspw%minimum%tpw%n, Gpspw%maximum%tpw%n, &
                           Gpspw%minimum%tpw%l, Gpspw%maximum%tpw%l )

   IF ( wrf_dm_on_monitor() ) THEN
      IF ( ntpw /= 0) THEN
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for Gpspw'
         call da_print_stats_gpspw(stats_unit, ntpw, Gpspw)
       END IF
   END IF

end subroutine da_ao_stats_gpspw

