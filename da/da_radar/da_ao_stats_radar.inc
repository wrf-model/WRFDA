subroutine da_matmulti(mata,matb,matc,ni,nj,nab)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: ni, nj, nab
   real,    intent(in)  :: mata(ni,nab), matb(nab, nj)
   real,    intent(out) :: matc(ni,nj)

   integer :: i, j, k            ! Loop counters

   if (trace_use) call da_trace_entry("da_matmultiover")

   matc = 0.0

   do i=1,ni
      do j=1,nj 
         do k=1,nab
            matc(i,j) = matc(i,j) + mata(i,k)*matb(k,j)
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_matmultiover")

end subroutine da_matmulti


_nchan + 1
   end do
   write(unit=stats_unit, fmt='((a,i5))')  ' used_nchan: ', used_nchan 

   write(unit=stats_unit, fmt='(6a)') &
        ' Channel ', &
        ' num  ', &
        ' ave  ', &
        ' rms  ', &
        ' min  ', &
        ' max  '

   do k=1, nchan                  !! loop for channels
      if (rad%ichan(k)%num > 0) then
         n    = rad%ichan(k)%num
         nmin = rad%ichan(k)%minimum%n
         nmax = rad%ichan(k)%maximum%n

         write(unit=stats_unit, fmt='((i3,i7,4f8.2))') &
            k, rad%ichan(k)%num, rad%ichan(k)%ave/real(n), &
            sqrt(rad%ichan(k)%rms/real(n)), &
            rad%ichan(k)%minimum%value, rad%ichan(k)%maximum%value
      end if
   end do

   if (trace_use) call da_trace_exit("da_print_stats_rad")

end subroutine da_print_stats_rad


