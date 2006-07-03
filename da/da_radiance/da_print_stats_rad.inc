subroutine da_print_stats_rad( stats_unit, nchan, rad )

   !---------------------------------------------------------------------------
   ! PURPOSE: print out statistics of omb, oma for radiance data.
   !
   ! METHOD:  print out average, rms, minimum, maximum of iv, re
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER,           INTENT(IN)    :: stats_unit, nchan
   TYPE (stats_rad_type), INTENT(IN)    :: rad
   
   INTEGER                       :: i, k, n, nmin, nmax

   write(unit=stats_unit, fmt='(6a/)') &
      ' Channel ', &
      ' num  ', &
      ' ave  ', &
      ' rms  ', &
      ' min  ', &
      ' max  '

   do k=1, nchan                  !! loop for channels
      if (rad%ichan(k)%num > 1) then
         n    = rad%ichan(k)%num
         nmin = rad%ichan(k)%minimum%n
         nmax = rad%ichan(k)%maximum%n

         write(unit=stats_unit, fmt='((i3,i7,4f8.2))') &
            k, rad%ichan(k)%num, rad%ichan(k)%ave/real(n), &
            sqrt(rad%ichan(k)%rms/real(n)), &
            rad%ichan(k)%minimum%value, rad%ichan(k)%maximum%value
      end if
   end do

end subroutine da_print_stats_rad

