subroutine da_ao_stats_gpspw (stats_unit, oi, re)

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI
   type  (y_type), intent (in)      :: re            ! A - O

   type (stats_gpspw_type)                :: Gpspw
   integer                          :: ntpw
   integer                          :: n
   real                             :: o_minus_b, o_minus_a, sigma_o, sigma_b
   real                             :: o_minus_b_0, o_minus_a_0, sigma_o_0, sigma_b_0 

   ntpw = 0
   num_gpspw_tot = 0
   o_minus_b_0 = 0.0; o_minus_a_0 = 0.0; sigma_o_0 = 0.0; sigma_b_0 = 0.0

   Gpspw%maximum%tpw = maxmin_type (missing_r, 0, 0)
   Gpspw%minimum%tpw = maxmin_type(-missing_r, 0, 0)
   Gpspw%average = residual_gpspw1_type(0.0)
   Gpspw%rms_err = Gpspw%average

   if (oi%num_Gpspw > 0) then
      do n=1, oi%num_Gpspw
         if (oi%gpspw(n)%loc%proc_domain) then
            call  da_stats_calculate (n, 0, oi%gpspw(n)%tpw%qc, &
                                  re%gpspw(n)%tpw, ntpw,   &
                                  gpspw%minimum%tpw,  gpspw%maximum%tpw, &
                                  gpspw%average%tpw,  gpspw%rms_err%tpw)

            if (pseudo_var(1:3) == 'tpw' .and. num_pseudo > 0) then
               o_minus_b = oi%GPSpw(n)%tpw%inv
               o_minus_a = re%gpspw(n)%tpw
               sigma_o   = oi%gpspw(n)%tpw%error

               ! Calculate equivalent sigma_b using
               ! 
               ! O-A=(O-B)*sigma_o/(sigma_o+sigma_b)

               sigma_b = sqrt ((o_minus_b - o_minus_a) / o_minus_a) * sigma_o
               o_minus_b_0 = wrf_dm_sum_real (o_minus_b)
               o_minus_a_0 = wrf_dm_sum_real (o_minus_a)
               sigma_o_0 = wrf_dm_sum_real (sigma_o)
               sigma_b_0 = wrf_dm_sum_real (sigma_b)
               write(unit=stdout,fmt='(A,F10.2)') &
                  'TEST_COVERAGE_da_ao_stats_gpspw:  o_minus_b_0 = ', o_minus_b_0
               write(unit=stdout,fmt='(A,F10.2)') &
                  'TEST_COVERAGE_da_ao_stats_gpspw:  o_minus_a_0 = ', o_minus_a_0
               write(unit=stdout,fmt='(A,F10.2)') &
                  'TEST_COVERAGE_da_ao_stats_gpspw:  sigma_o_0 = ', sigma_o_0
               write(unit=stdout,fmt='(A,F10.2)') &
                  'TEST_COVERAGE_da_ao_stats_gpspw:  sigma_b_0 = ', sigma_b_0
               if (rootproc) then  
                  write(stats_unit,'(/A,A3,A,f12.3)')  &  
                    ' Pseudo ', pseudo_var, ' O-B: ', o_minus_b_0  
                  write(stats_unit,' (A,A3,A,f12.3)')  &  
                    ' Pseudo ', pseudo_var, ' O-A: ', o_minus_a_0  
                  write(stats_unit,' (A,A3,A,f12.3)')  &  
                    ' Pseudo ', pseudo_var, ' sigma_o: ', sigma_o_0  
                  write(stats_unit,'(A,A3,A,f12.3)')  &  
                    ' Pseudo ', pseudo_var, ' sigma_b: ', sigma_b_0 
               end if  
            end if
         end if    ! end if (oi%gpspw(n)%loc%proc_domain)
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (ntpw)
   num_gpspw_tot = ntpw

   call da_proc_stats_combine(Gpspw%average%tpw, Gpspw%rms_err%tpw, &
                           Gpspw%minimum%tpw%value, Gpspw%maximum%tpw%value, &
                           Gpspw%minimum%tpw%n, Gpspw%maximum%tpw%n, &
                           Gpspw%minimum%tpw%l, Gpspw%maximum%tpw%l)

   if (rootproc) then
      if (ntpw /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for Gpspw'
         call da_print_stats_gpspw(stats_unit, ntpw, Gpspw)
       end if
   end if

end subroutine da_ao_stats_gpspw


