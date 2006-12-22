subroutine da_deallocate_observations ( ob )

   !---------------------------------------------------------------------------
   ! PURPOSE: Deallocate components of observation structure.
   !---------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(inout)        :: ob     ! Observation structure.
   integer   :: n

   if (trace_use) call da_trace_entry("da_deallocate_observations")

   !---------------------------------------------------------------------------
   ! [1.0] Deallocate:
   !---------------------------------------------------------------------------

   if ( ob % num_sound > 0 ) then
      do n = 1, ob % num_sound
         deallocate (ob % sound ( n ) % h )
         deallocate (ob % sound ( n ) % p )
         deallocate (ob % sound ( n ) % zk)
         deallocate (ob % sound ( n ) % u )
         deallocate (ob % sound ( n ) % v )
         deallocate (ob % sound ( n ) % t )
         deallocate (ob % sound ( n ) % q )
      end do

      deallocate (ob % sound)

      deallocate (ob % sonde_sfc)
   end if

   if (ob % num_synop > 0) deallocate (ob % synop)

   if ( ob % num_airep > 0 ) then
      do n = 1, ob % num_airep
         deallocate (ob % airep ( n ) % h )
         deallocate (ob % airep ( n ) % p )
         deallocate (ob % airep ( n ) % zk)
         deallocate (ob % airep ( n ) % u )
         deallocate (ob % airep ( n ) % v )
         deallocate (ob % airep ( n ) % t )
      end do

      deallocate (ob % airep)
   end if

   if (ob % num_satem > 0) then
      do n = 1, ob % num_satem
         deallocate (ob % satem(n) % p)
         deallocate (ob % satem(n) % thickness)
         deallocate (ob % satem(n) % org_thickness)
      end do
      deallocate (ob % satem)
   end if

   if (ob % num_geoamv > 0) then
      do n = 1, ob % num_geoamv
         deallocate (ob % geoamv(n) % p)
         deallocate (ob % geoamv(n) % zk)
         deallocate (ob % geoamv(n) % u)
         deallocate (ob % geoamv(n) % v)
      end do
      deallocate (ob % geoamv)
   end if


   if (ob % num_polaramv > 0) then
      do n = 1, ob % num_polaramv
         deallocate (ob % polaramv(n) % u)
         deallocate (ob % polaramv(n) % v)
      end do
      deallocate (ob % polaramv)
   end if

   if (ob % num_metar > 0) deallocate (ob % metar)
   if (ob % num_ships > 0) deallocate (ob % ships)

   if ( ob % num_pilot > 0 ) then
      do n = 1, ob % num_pilot
         deallocate (ob % pilot ( n ) % p )
         deallocate (ob % pilot ( n ) % zk)
         deallocate (ob % pilot ( n ) % u )
         deallocate (ob % pilot ( n ) % v )
      end do

      deallocate (ob % pilot)
   end if

   if ( ob % num_bogus > 0 ) then
      do n = 1, ob % num_bogus
         deallocate (ob % bogus ( n ) % h )
         deallocate (ob % bogus ( n ) % p )
         deallocate (ob % bogus ( n ) % zk)
         deallocate (ob % bogus ( n ) % u )
         deallocate (ob % bogus ( n ) % v )
         deallocate (ob % bogus ( n ) % t )
         deallocate (ob % bogus ( n ) % q )
      end do

      deallocate (ob % bogus)
   end if

   if ( ob % num_Radar > 0 ) then
      do n = 1, ob % num_Radar
         deallocate (ob % Radar ( n ) % height   )
         deallocate (ob % Radar ( n ) % height_qc)
         deallocate (ob % Radar ( n ) % zk       )
         deallocate (ob % Radar ( n ) % rv       )
         deallocate (ob % Radar ( n ) % rf       )
      end do

      deallocate (ob % Radar)
   end if

   if (ob % num_gpspw > 0) deallocate (ob % gpspw)

   if ( ob % num_gpsref > 0 ) then
      do n = 1, ob % num_gpsref
         deallocate (ob % gpsref(n) %  h )
         deallocate (ob % gpsref(n) %  zk)
         deallocate (ob % gpsref(n) % ref)
         deallocate (ob % gpsref(n) %   p)
         deallocate (ob % gpsref(n) %   t)
         deallocate (ob % gpsref(n) %   q)
      end do
      deallocate (ob % gpsref)
   end if

   if (ob % num_ssmi_tb > 0) deallocate (ob % ssmi_tb)
   if (ob % num_ssmi_retrieval > 0) deallocate (ob % ssmi_retrieval)

   if (ob % num_ssmt1 > 0) then
      do n = 1, ob % num_ssmt1
         deallocate (ob % ssmt1(n) % h )
         deallocate (ob % ssmt1(n) % p )
         deallocate (ob % ssmt1(n) % zk)
         deallocate (ob % ssmt1(n) % t )
      end do
   
      deallocate (ob % ssmt1)
   end if
   
   if (ob % num_ssmt2 > 0) then
      do n = 1, ob % num_ssmt2
         deallocate (ob % ssmt2(n) % h )
         deallocate (ob % ssmt2(n) % p )
         deallocate (ob % ssmt2(n) % zk)
         deallocate (ob % ssmt2(n) % rh)
      end do
   
      deallocate (ob % ssmt2)
   end if

   if (ob % num_qscat > 0) deallocate (ob % qscat)

   if (ob%num_profiler > 0) then
      do n = 1, ob % num_profiler
         deallocate (ob%profiler(n)%p)
         deallocate (ob%profiler(n)%zk)
         deallocate (ob%profiler(n)%u)
         deallocate (ob%profiler(n)%v)
      end do

      deallocate(ob%profiler)
   end if

   if (ob%num_buoy     > 0) deallocate(ob%buoy)

   if (ob % num_airsr > 0) then
      do n = 1, ob % num_airsr
         deallocate (ob % airsr ( n ) % h )
         deallocate (ob % airsr ( n ) % p )
         deallocate (ob % airsr ( n ) % zk)
         deallocate (ob % airsr ( n ) % t )
         deallocate (ob % airsr ( n ) % q )
      end do

      deallocate (ob % airsr)
   end if

   if (trace_use) call da_trace_exit("da_deallocate_observations")

end subroutine da_deallocate_observations


