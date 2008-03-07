subroutine da_deallocate_y(y)

   !---------------------------------------------------------------------------
   ! Purpose: Deallocate arrays used in y and residual obs structures.
   !
   ! Method:  Deallocate component in turn.
   !---------------------------------------------------------------------------

   implicit none
   
   type (y_type), intent(inout)          :: y      ! residual type structure.
   integer                               :: n,i  ! Loop counter.


   if (trace_use) call da_trace_entry("da_deallocate_y")

   !---------------------------------------------------------------------------
   ! [1.0] Deallocate:
   !---------------------------------------------------------------------------

   if (y % num_synop > 0) deallocate (y % synop)

   if (y % num_ships > 0) deallocate (y % ships)

   if (y % num_metar > 0) deallocate (y % metar)


   if (y % num_sound > 0) then
      do n = 1, y % num_sound
         deallocate (y % sound(n)%u)
         deallocate (y % sound(n)%v)
         deallocate (y % sound(n)%t)
         deallocate (y % sound(n)%q)
      end do

      deallocate (y % sound)

      deallocate (y % sonde_sfc)
   end if
!whl
   if (y % num_mtgirs > 0) then
      do n = 1, y % num_mtgirs
         deallocate (y % mtgirs(n)%u)
         deallocate (y % mtgirs(n)%v)
         deallocate (y % mtgirs(n)%t)
         deallocate (y % mtgirs(n)%q)
      end do

      deallocate (y % mtgirs)
   end if

   if (y % num_pilot > 0) then
      do n = 1, y % num_pilot
         deallocate (y % pilot(n)%u)
         deallocate (y % pilot(n)%v)
      end do
      deallocate (y % pilot)
   end if

   if (y % num_bogus > 0) then
      do n = 1, y % num_bogus
         deallocate (y % bogus(n)%u)
         deallocate (y % bogus(n)%v)
         deallocate (y % bogus(n)%t)
         deallocate (y % bogus(n)%q)
      end do
      deallocate (y % bogus)
   end if

    if (y % num_radar > 0) then
       do n = 1, y % num_radar
          deallocate (y % radar(n)%rv)
          deallocate (y % radar(n)%rf)
       end do
       deallocate (y % radar)
    end if


   if (y % num_airep > 0) then
      do n = 1, y % num_airep
         deallocate (y % airep(n)%u)
         deallocate (y % airep(n)%v)
         deallocate (y % airep(n)%t)
      end do
      deallocate (y % airep)
   end if

   if (y % num_geoamv > 0) then
      do n=1, y % num_geoamv
         deallocate (y % geoamv(n) % u)
         deallocate (y % geoamv(n) % v)
      end do
      deallocate (y % geoamv)
   end if

   if (y % num_polaramv > 0) then
      do n=1, y % num_polaramv
         deallocate (y % polaramv(n) % u)
         deallocate (y % polaramv(n) % v)
      end do
      deallocate (y % polaramv)
   end if

   if (y % num_gpspw > 0) deallocate (y % gpspw)

   if (y % num_gpsref > 0) then
      do n = 1, y % num_gpsref
         deallocate (y % gpsref(n)%ref)
         deallocate (y % gpsref(n)%  p)
         deallocate (y % gpsref(n)%  t)
         deallocate (y % gpsref(n)%  q)
      end do
      deallocate (y % gpsref)
   end if

   if (y % num_satem > 0) then
      do n = 1, y % num_satem
         deallocate (y % satem(n) % thickness)
      end do
      deallocate (y % satem)
   end if

   if (y % num_ssmi_tb > 0) deallocate (y % ssmi_tb)
   if (y % num_ssmi_retrieval > 0) deallocate (y % ssmi_retrieval)
   if (y % num_pseudo > 0) deallocate (y % pseudo)

   if (y % num_ssmt1 > 0) then
      do n = 1, y % num_ssmt1
         deallocate (y % ssmt1(n) % t)
      end do
      deallocate (y % ssmt1)
   end if

   if (y % num_ssmt2 > 0) then
      do n = 1, y % num_ssmt2
         deallocate (y % ssmt2(n) % rh)
      end do
      deallocate (y % ssmt2)
   end if

   if (y % num_qscat > 0) deallocate (y % qscat)

   if (y % num_profiler > 0) then
      do n = 1, y % num_profiler
         deallocate (y % profiler(n)%u)
         deallocate (y % profiler(n)%v)
      end do
      deallocate (y % profiler)
   end if

   if (y % num_buoy  > 0) deallocate (y % buoy)

   !  radiance:
 
   if (y % num_inst > 0) then
      do i = 1,  y % num_inst
        if (y % instid(i) % num_rad < 1) cycle
        ! deallocate (y % instid(i) % ichan)
        deallocate ( y % instid(i) % tb )
      end do
      deallocate (y % instid)
   end if
   if (y % num_airsr > 0) then
      do n = 1, y % num_airsr
         deallocate (y % airsr(n)%t)
         deallocate (y % airsr(n)%q)
      end do
      deallocate (y % airsr)
   end if

   if (trace_use) call da_trace_exit("da_deallocate_y")

end subroutine da_deallocate_y


