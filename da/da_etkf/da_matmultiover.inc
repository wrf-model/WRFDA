subroutine da_matmultiover(mata,matb,ni,nj)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: ni, nj
   real,    intent(in)    :: matb(nj, nj)
   real,    intent(inout) :: mata(ni,nj)

   integer :: i, j, k          ! Loop counters
   real    :: tmp(1:nj)

   if (trace_use) call da_trace_entry("da_matmultiover")

   do i=1,ni
      tmp = 0.0
      do j=1,nj 
         do k=1,nj
            tmp(j) = tmp(j) + mata(i,k)*matb(k,j)
         end do
      end do
      do j=1,nj
         mata(i,j) = tmp(j) 
      end do
   end do

   if (trace_use) call da_trace_exit("da_matmultiover")

end subroutine da_matmultiover


0.0)
   stats%rms_err = stats%average

   nthickness = 0

   stats%maximum%thickness = maxmin_type(0.0, 0, 0)

   stats%minimum = stats%maximum
   stats%average = residual_satem1_type(0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(satem)%nlocal
      if (iv%satem(n)%loc%proc_domain) then
         do k=1, iv%satem(n)%info%levels
            call da_stats_calculate (n, k, iv%satem(n)%thickness(k)%qc, & 
               re%satem(n)%thickness(k), nthickness, &
               stats%minimum%thickness, stats%maximum%thickness, &
               stats%average%thickness, stats%rms_err%thickness)
         end do
      end if    ! end if (iv%satem(n)%loc%proc_domain)
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (nthickness)
   iv%nstats(satem) = nthickness

   call da_proc_stats_combine(stats%average%thickness, stats%rms_err%thickness, &
      stats%minimum%thickness%value, stats%maximum%thickness%value, &
      stats%minimum%thickness%n, stats%maximum%thickness%n, &
      stats%minimum%thickness%l, stats%maximum%thickness%l)

   if (rootproc) then
      if (nthickness /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for satem'
         call da_print_stats_satem(stats_unit, nthickness, stats)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_ao_stats_satem")

end subroutine da_ao_stats_satem


