subroutine da_transform_xtospeed_lin(TGL_speed,U,V,TGL_u,TGL_v)

   !----------------------------------------------------------------------------
   ! Convert (U-V in m/s) components into wind speed (Speed in m/s)
   !----------------------------------------------------------------------------

   IMPLICIT NONE

   REAL,     INTENT (out  )    :: TGL_speed
   REAL,     INTENT (in   )    :: U,V
   REAL,     INTENT (in   )    :: TGL_u,TGL_v
   REAL                        :: speed

   IF (trace_use) call da_trace_entry("da_transform_xtospeed_lin")

   speed = sqrt( U*U + V*V + 1.E-6)

   TGL_speed = (U*TGL_u + V*TGL_v)/speed

   IF (trace_use) call da_trace_exit("da_transform_xtospeed_lin")

end subroutine da_transform_xtospeed_lin


