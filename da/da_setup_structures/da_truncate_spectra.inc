subroutine da_truncate_spectra( max_wave, nw, power_trunc, power, &
   max_wave_trunc )

   implicit none

   integer, intent(in)  :: max_wave         ! Smallest wave for domain.
   integer, intent(in)  :: nw               ! Dimension of input power spectrum.
   real, intent(in)     :: power_trunc      ! Power truncation (fraction).
   real, intent(in)     :: power(0:nw)      ! Power spectrum.
   integer, intent(out) :: max_wave_trunc   ! Smallest wave after truncation.

   integer              :: l                ! Loop counter.
   real                 :: truncated_power  ! Truncated power.
   real                 :: cumul_power      ! Cumulative power.

   truncated_power = power_trunc * sum(power(0:nw))

   cumul_power = 0.0
   max_wave_trunc = max_wave
   do l = 0, nw - 1 
      cumul_power = cumul_power + power(l)
      if ( cumul_power > truncated_power ) then
         max_wave_trunc = l - 1
         exit
      end if
   end do

   if ( max_wave_trunc > max_wave ) then
      write(unit=errmsg(1),FMT='(a)') &
         'da_truncate_spectra: Power requested needs higher resolution.'     
      write(unit=errmsg(2),FMT='(a,i8)') &
         'Maximum grid wavenumber =  ', max_wave
      write(unit=errmsg(3),FMT='(a,i8)') &
         'Truncating to wavenumber = ', max_wave_trunc
      call wrf_warning(__FILE__,__LINE__,errmsg(1:3))
      max_wave_trunc = max_wave
   end if

end subroutine da_truncate_spectra

