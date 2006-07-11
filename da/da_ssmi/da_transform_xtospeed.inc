subroutine da_transform_xtospeed(Speed,U,V)

   !----------------------------------------------------------------------------
   ! Convert (U-V in m/s) components into wind speed (Speed in m/s)
   !----------------------------------------------------------------------------

    IMPLICIT NONE

    REAL,   INTENT (out  ) :: Speed
    REAL,   INTENT (in   ) :: U, V

    Speed = sqrt( U*U + V*V )

end subroutine da_transform_xtospeed


