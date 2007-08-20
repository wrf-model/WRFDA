subroutine da_deallocate_observations (ob)

   !---------------------------------------------------------------------------
   ! Purpose: Deallocate components of observation structure.
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout)        :: ob     ! Observation structure.
   integer   :: n

   if (trace_use) call da_trace_entry("da_deallocate_observations")

   !---------------------------------------------------------------------------
   ! [1.0] Deallocate:
   !---------------------------------------------------------------------------

   if (ob % nlocal(sound) > 0) then
      do n = 1, ob % nlocal(sound)
         if (ob%sound(n)%info%levels > 0) then
            deallocate (ob % sound (n) % h)
            deallocate (ob % sound (n) % p)
            deallocate (ob % sound (n) % zk)
            deallocate (ob % sound (n) % u)
            deallocate (ob % sound (n) % v)
            deallocate (ob % sound (n) % t)
            deallocate (ob % sound (n) % q)
         end if
      end do

      deallocate (ob % sound)

      deallocate (ob % sonde_sfc)
   end if

   if (ob % nlocal(synop) > 0) deallocate (ob % synop)

   if (ob % nlocal(airep) > 0) then
      do n = 1, ob % nlocal(airep)
         deallocate (ob % airep (n) % h)
         deallocate (ob % airep (n) % p)
         deallocate (ob % airep (n) % zk)
         deallocate (ob % airep (n) % u)
         deallocate (ob % airep (n) % v)
         deallocate (ob % airep (n) % t)
      end do

      deallocate (ob % airep)
   end if

   if (ob % nlocal(satem) > 0) then
      do n = 1, ob % nlocal(satem)
         deallocate (ob % satem(n) % p)
         deallocate (ob % satem(n) % thickness)
         deallocate (ob % satem(n) % org_thickness)
      end do
      deallocate (ob % satem)
   end if

   if (ob % nlocal(geoamv) > 0) then
      do n = 1, ob % nlocal(geoamv)
         deallocate (ob % geoamv(n) % p)
         deallocate (ob % geoamv(n) % zk)
         deallocate (ob % geoamv(n) % u)
         deallocate (ob % geoamv(n) % v)
      end do
      deallocate (ob % geoamv)
   end if


   if (ob % nlocal(polaramv) > 0) then
      do n = 1, ob % nlocal(polaramv)
         deallocate (ob % polaramv(n) % p)
         deallocate (ob % polaramv(n) % zk)
         deallocate (ob % polaramv(n) % u)
         deallocate (ob % polaramv(n) % v)
      end do
      deallocate (ob % polaramv)
   end if

   if (ob % nlocal(metar) > 0) deallocate (ob % metar)
   if (ob % nlocal(ships) > 0) deallocate (ob % ships)

   if (ob % nlocal(pilot) > 0) then
      do n = 1, ob % nlocal(pilot)
         deallocate (ob % pilot (n) % p)
         deallocate (ob % pilot (n) % zk)
         deallocate (ob % pilot (n) % u)
         deallocate (ob % pilot (n) % v)
      end do

      deallocate (ob % pilot)
   end if

   if (ob % nlocal(bogus) > 0) then
      do n = 1, ob % nlocal(bogus)
         deallocate (ob % bogus (n) % h)
         deallocate (ob % bogus (n) % p)
         deallocate (ob % bogus (n) % zk)
         deallocate (ob % bogus (n) % u)
         deallocate (ob % bogus (n) % v)
         deallocate (ob % bogus (n) % t)
         deallocate (ob % bogus (n) % q)
      end do

      deallocate (ob % bogus)
   end if

   if (ob % nlocal(radar) > 0) then
      do n = 1, ob % nlocal(radar)
         deallocate (ob % radar (n) % height  )
         deallocate (ob % radar (n) % height_qc)
         deallocate (ob % radar (n) % zk      )
         deallocate (ob % radar (n) % rv      )
         deallocate (ob % radar (n) % rf      )
      end do

      deallocate (ob % radar)
   end if

   if (ob % nlocal(gpspw) > 0) deallocate (ob % gpspw)

   if (ob % nlocal(gpsref) > 0) then
      do n = 1, ob % nlocal(gpsref)
         deallocate (ob % gpsref(n) %  h)
         deallocate (ob % gpsref(n) %  zk)
         deallocate (ob % gpsref(n) % ref)
         deallocate (ob % gpsref(n) %   p)
         deallocate (ob % gpsref(n) %   t)
         deallocate (ob % gpsref(n) %   q)
      end do
      deallocate (ob % gpsref)
   end if

   if (ob % nlocal(ssmi_tb) > 0) deallocate (ob % ssmi_tb)
   if (ob % nlocal(ssmi_rv) > 0) deallocate (ob % ssmi_rv)

   if (ob % nlocal(ssmt1) > 0) then
      do n = 1, ob % nlocal(ssmt1)
         deallocate (ob % ssmt1(n) % h)
         deallocate (ob % ssmt1(n) % p)
         deallocate (ob % ssmt1(n) % zk)
         deallocate (ob % ssmt1(n) % t)
      end do
   
      deallocate (ob % ssmt1)
   end if
   
   if (ob % nlocal(ssmt2) > 0) then
      do n = 1, ob % nlocal(ssmt2)
         deallocate (ob % ssmt2(n) % h)
         deallocate (ob % ssmt2(n) % p)
         deallocate (ob % ssmt2(n) % zk)
         deallocate (ob % ssmt2(n) % rh)
      end do
   
      deallocate (ob % ssmt2)
   end if

   if (ob % nlocal(qscat) > 0) deallocate (ob % qscat)

   if (ob%nlocal(profiler) > 0) then
      do n = 1, ob % nlocal(profiler)
         deallocate (ob%profiler(n)%p)
         deallocate (ob%profiler(n)%zk)
         deallocate (ob%profiler(n)%u)
         deallocate (ob%profiler(n)%v)
      end do

      deallocate(ob%profiler)
   end if

   if (ob%nlocal(buoy)     > 0) deallocate(ob%buoy)

   if (ob % nlocal(airsr) > 0) then
      do n = 1, ob % nlocal(airsr)
         deallocate (ob % airsr (n) % h)
         deallocate (ob % airsr (n) % p)
         deallocate (ob % airsr (n) % zk)
         deallocate (ob % airsr (n) % t)
         deallocate (ob % airsr (n) % q)
      end do

      deallocate (ob % airsr)
   end if

   if (trace_use) call da_trace_exit("da_deallocate_observations")

end subroutine da_deallocate_observations


