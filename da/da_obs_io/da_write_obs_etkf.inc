subroutine da_write_obs_etkf(ob, iv, re)

   !-------------------------------------------------------------------------
   ! Purpose: Writes out components of iv=O-B structure.
   !-------------------------------------------------------------------------   

   implicit none

   type (y_type), intent(in)     :: ob      ! Observation structure.
   type (ob_type), intent(in)    :: iv      ! O-B structure.
   type (y_type), intent(inout)  :: re      ! residual vector.
      
   integer                       :: n, k, num_obs, ios
   integer                       :: ounit     ! Output unit           
   character(len=20)             :: filename

   if (trace_use) call da_trace_entry("da_write_obs_etkf")

   !-------------------------------------------------------------------------
   ! Fix output unit
   !-------------------------------------------------------------------------

   call da_get_unit(ounit)

#ifdef DM_PARALLEL
    write(unit=filename, fmt='(a,i3.3)') 'ob.etkf.', myproc
#else
    write(unit=filename, fmt='(a)') 'ob.etkf.000'
#endif

   open (unit=ounit,file=trim(filename),form='formatted',status='replace', &
      iostat=ios)
   if (ios /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open ETKF observation file"//filename/))
   end if


   ! [6] Transfer sonde obs:

   if (iv % num_sound > 0) then
      num_obs = 0
      do n = 1, iv % num_sound
        if (iv%sound(n)%loc%proc_domain) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv % num_sound
            if (iv%sound(n)%loc%proc_domain) then
               num_obs = num_obs + 1
               do k = 1, iv % sound(n) % info % levels
                  if ( iv%sound(n)%u(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%sound(n)%u(k), iv%sound(n)%u(k)%inv, iv%sound(n)%u(k)%error
                  end if
                  if ( iv%sound(n)%v(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%sound(n)%v(k), iv%sound(n)%v(k)%inv, iv%sound(n)%v(k)%error
                  end if
                  if ( iv%sound(n)%t(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%sound(n)%t(k), iv%sound(n)%t(k)%inv, iv%sound(n)%t(k)%error
                  end if
                  if ( iv%sound(n)%q(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%sound(n)%q(k), iv%sound(n)%q(k)%inv, iv%sound(n)%q(k)%error
                  end if
               end do
            end if
         end do
      end if
   end if

   close (ounit)
   call da_free_unit(ounit)

   if (trace_use) call da_trace_exit("da_write_obs_etkf")

end subroutine da_write_obs_etkf

