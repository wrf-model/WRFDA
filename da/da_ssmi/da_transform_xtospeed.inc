subroutine da_transform_xtospeed(Speed,U,V)

   !----------------------------------------------------------------------------
   ! Convert (U-V in m/s) components into wind speed (Speed in m/s)
   !----------------------------------------------------------------------------

    implicit none

    real,   intent (out  ) :: Speed
    real,   intent (in   ) :: U, V

    Speed = sqrt( U*U + V*V )

end subroutine da_transform_xtospeed


