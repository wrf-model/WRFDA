subroutine da_qfrmrh ( p, t, rh, q )

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: p, t, rh
   real, intent(out) :: q

   real   :: a, b, e, qs

   if (trace_use) call da_trace_entry("da_qfrmrh")

   a=17.26
   b=35.86
   if (t <= 263.0) a=21.87
   if (t <= 263.0) b= 7.66
   e  = 6.11*exp(a*(t-t_triple)/(t-b))
   qs = 0.622*e/(p-0.378*e)
   q  = qs*rh/100.0

   if (trace_use) call da_trace_exit("da_qfrmrh")

end subroutine da_qfrmrh


-----------------------
   ! [1.0] Compute components of Grad_y(Jo):
   !-------------------------------------------------------------------------

   ! Radiosonde

   call da_calculate_grady_sound(iv, re,  jo_grad_y)
   call da_calculate_grady_sonde_sfc(iv, re,  jo_grad_y)

   ! Surface

   call da_calculate_grady_synop(iv, re,  jo_grad_y)

   ! Geostationary Satellite AMVs

   call da_calculate_grady_geoamv(iv, re,  jo_grad_y)

   ! Polar orbitting Satellite AMVs

   call da_calculate_grady_polaramv(iv, re,  jo_grad_y)

   ! Airep

   call da_calculate_grady_airep(iv, re,  jo_grad_y)

   ! Pilot

   call da_calculate_grady_pilot(iv, re,  jo_grad_y)

   ! Profiler
   call da_calculate_grady_profiler(iv, re,  jo_grad_y)

   ! satem

   call da_calculate_grady_satem(iv, re,  jo_grad_y)

   ! Metar:

   call da_calculate_grady_metar(iv, re,  jo_grad_y)

   ! Ships:

   call da_calculate_grady_ships(iv, re,  jo_grad_y)

   ! Buoy :

   call da_calculate_grady_buoy (iv, re,  jo_grad_y)

   ! GPSPW:

   call da_calculate_grady_gpspw(iv, re,  jo_grad_y)

   ! GPSRF:

   call da_calculate_grady_gpsref(iv, re,  jo_grad_y)

   ! SSMI:

   call da_calculate_grady_ssmi(iv, re, jo_grad_y) 

   ! SSMT1:

   call da_calculate_grady_ssmt1(iv, re,  jo_grad_y)

   ! SSMT2:

   call da_calculate_grady_ssmt2(iv, re,  jo_grad_y)  

   ! Pseudo obs:

   call da_calculate_grady_pseudo(iv, re,  jo_grad_y)    

   ! TC bogus obs:

   call da_calculate_grady_bogus(iv, re,  jo_grad_y)    

   ! Quikscat scatterometer:

   call da_calculate_grady_qscat(iv, re, jo_grad_y)

   ! radar:               

   call da_calculate_grady_radar(iv, re, jo_grad_y)

   ! Radiance:
   call da_calculate_grady_rad(iv, re, jo_grad_y)

   ! AIRS retrievals

   call da_calculate_grady_airsr(iv, re,  jo_grad_y)

   if (trace_use) call da_trace_exit("da_calculate_grady")

end subroutine da_calculate_grady


