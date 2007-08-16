subroutine da_setup_obs_structures_bufr(xp, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: Define, allocate and read observation structure.
   !---------------------------------------------------------------------------

   implicit none
   
   type (xpose_type), intent(in)  :: xp         ! Domain decomposition vars.
   type (y_type),     intent(out) :: ob          ! Observation structure.
   type (ob_type),    intent(out) :: iv          ! O-B structure.

   character(len=120)           :: filename
   integer                      :: n   

   if (trace_use) call da_trace_entry("da_setup_obs_structures_bufr")

   !--------------------------------------------------------------------------
   ! [1.0] Scan BUFR observation header and get idea of number of obs:
   !--------------------------------------------------------------------------

   if (num_fgat_time > 1) then
      filename = ' '

      do n=1, num_fgat_time
         iv%time = n

         write(filename(1:10), fmt='(a, i2.2,a)') 'ob', n,'.bufr'

         ! scan prepbufr observation file
         call da_scan_bufr (iv, xp, filename)

         ! if (use_radarobs) then
         !    ! scan radar observation file
         !    write(filename(1:10), fmt='(a, i2.2,a)') 'radar', n,'.dat'
         !    call da_scan_bufr_radar(iv, xp, filename)
         ! end if
      end do
   else
      iv%time = 1
      filename="ob.bufr"
      call da_scan_bufr(iv, xp, filename)

      ! scan main body of radar observation file
      ! if (use_radarobs) &
      !    call da_scan_bufr_radar(iv, xp,'radar.dat')
   end if

   !-------------------------------------------------------------------------
   ! Allocate the iv based on input number of obs:
   !--------------------------------------------------------------------------

   call da_allocate_observations (iv)

   if (num_fgat_time > 1) then
      do n=1, num_fgat_time
         iv%time = n

         write(filename(1:10), fmt='(a, i2.2)') 'ob.', n

         ! read prepbufr observation file
         call da_read_bufr (iv, xp, filename)

         ! if (use_radarobs) then
         !    ! read radar observation file
         !    write(filename(1:10), fmt='(a, i2.2)') 'radarob.', n
         !    call da_read_bufr_radar(iv, xp, filename)
         ! end if
      end do
   else

      iv%time = 1

      filename="ob.bufr"
      call da_read_bufr(iv, xp,filename)

      ! if (use_radarobs) then
      !    ! read radar observation file
      !    call da_read_bufr_radar(iv, xp)
      ! end if
   end if

   !--------------------------------------------------------------------------
   ! [3.0] Calculate innovation vector (O-B) and create (smaller) ob structure:
   !--------------------------------------------------------------------------

   call da_fill_obs_structures(xp, iv, ob)

   iv%time = 1

   if (trace_use) call da_trace_exit("da_setup_obs_structures_bufr")

end subroutine da_setup_obs_structures_bufr


