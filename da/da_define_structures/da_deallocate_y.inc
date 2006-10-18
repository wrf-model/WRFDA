SUBROUTINE da_deallocate_y( y )

   !---------------------------------------------------------------------------
   ! PURPOSE: Deallocate arrays used in y and residual obs structures.
   !
   ! METHOD:  Deallocate component in turn.
   !---------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (y_type), INTENT(INOUT)          :: y      ! residual type structure.
   INTEGER                               :: n,i,j  ! Loop counter.


   IF (trace_use) CALL da_trace_entry("da_deallocate_y")

   !---------------------------------------------------------------------------
   ! [1.0] Deallocate:
   !---------------------------------------------------------------------------

   IF ( y % num_synop > 0 ) DEALLOCATE (y % synop)

   IF ( y % num_ships > 0 ) DEALLOCATE (y % ships)

   IF ( y % num_metar > 0 ) DEALLOCATE (y % metar)

   IF ( y % num_sound > 0 ) then
      do n = 1, y % num_sound
         DEALLOCATE (y % sound(n)%u)
         DEALLOCATE (y % sound(n)%v)
         DEALLOCATE (y % sound(n)%t)
         DEALLOCATE (y % sound(n)%q)
      enddo

      DEALLOCATE (y % sound)

      DEALLOCATE (y % sonde_sfc)
   ENDIF

   IF ( y % num_pilot > 0 ) then
      do n = 1, y % num_pilot
         DEALLOCATE (y % pilot(n)%u)
         DEALLOCATE (y % pilot(n)%v)
      enddo
      DEALLOCATE (y % pilot)
   endif

   IF ( y % num_bogus > 0 ) then
      do n = 1, y % num_bogus
         DEALLOCATE (y % bogus(n)%u)
         DEALLOCATE (y % bogus(n)%v)
         DEALLOCATE (y % bogus(n)%t)
         DEALLOCATE (y % bogus(n)%q)
      enddo
      DEALLOCATE (y % bogus)
   endif

    IF ( y % num_Radar > 0 ) then
       do n = 1, y % num_Radar
          DEALLOCATE (y % Radar(n)%rv)
          DEALLOCATE (y % Radar(n)%rf)
       enddo
       DEALLOCATE (y % Radar)
    endif


   IF ( y % num_airep > 0 ) then
      do n = 1, y % num_airep
         DEALLOCATE (y % airep(n)%u)
         DEALLOCATE (y % airep(n)%v)
         DEALLOCATE (y % airep(n)%t)
      enddo
      DEALLOCATE (y % airep)
   endif

   IF ( y % num_geoamv > 0 ) DEALLOCATE (y % geoamv)
   IF ( y % num_polaramv > 0 ) DEALLOCATE (y % polaramv)

   IF ( y % num_gpspw > 0 ) DEALLOCATE (y % gpspw)

   IF ( y % num_gpsref > 0 ) then
      do n = 1, y % num_gpsref
         DEALLOCATE (y % gpsref(n)%ref)
         DEALLOCATE (y % gpsref(n)%  p)
         DEALLOCATE (y % gpsref(n)%  t)
         DEALLOCATE (y % gpsref(n)%  q)
      enddo
      DEALLOCATE (y % gpsref)
   ENDIF

   IF ( y % num_satem > 0 ) then
      do n = 1, y % num_satem
         DEALLOCATE (y % satem(n) % thickness)
      enddo
      DEALLOCATE (y % satem)
   ENDIF

   IF ( y % num_ssmi_tb > 0 ) DEALLOCATE (y % ssmi_tb)
   IF ( y % num_ssmi_retrieval > 0 ) DEALLOCATE (y % ssmi_retrieval)
   IF ( y % num_pseudo > 0 ) DEALLOCATE (y % pseudo)

   IF ( y % num_ssmt1 > 0 ) then
      do n = 1, y % num_ssmt1
         DEALLOCATE (y % ssmt1(n) % t)
      enddo
      DEALLOCATE (y % ssmt1)
   ENDIF

   IF ( y % num_ssmt2 > 0 ) then
      do n = 1, y % num_ssmt2
         DEALLOCATE (y % ssmt2(n) % rh)
      enddo
      DEALLOCATE (y % ssmt2)
   ENDIF

   IF ( y % num_qscat > 0 ) DEALLOCATE (y % qscat)

   IF ( y % num_profiler > 0 ) then
      do n = 1, y % num_profiler
         DEALLOCATE (y % profiler(n)%u)
         DEALLOCATE (y % profiler(n)%v)
      enddo
      DEALLOCATE (y % profiler)
   endif

   IF ( y % num_buoy  > 0 ) DEALLOCATE (y % buoy )

   !  radiance:
 
   IF ( y % num_inst > 0 ) THEN
      do i = 1,  y % num_inst
        if (y % instid(i) % num_rad < 1) cycle
        ! DEALLOCATE ( y % instid(i) % ichan )
        do j = 1, y % instid(i) % num_rad
           DEALLOCATE ( y % instid(i) % rad(j) % tb )
        end do
        DEALLOCATE ( y % instid(i) % rad )
      end do
      DEALLOCATE (y % instid)
   END IF
   IF ( y % num_airsr > 0 ) then
      do n = 1, y % num_airsr
         DEALLOCATE (y % airsr(n)%t)
         DEALLOCATE (y % airsr(n)%q)
      enddo
      DEALLOCATE (y % airsr)
   endif

   IF (trace_use) CALL da_trace_exit("da_deallocate_y")

END SUBROUTINE da_deallocate_y
