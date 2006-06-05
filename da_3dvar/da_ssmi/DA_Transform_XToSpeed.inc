SUBROUTINE DA_Transform_XToSpeed(Speed,U,V)

!------------------------------------------------------------------------------!
! Convert (U-V in m/s) components into wind speed (Speed in m/s)
!------------------------------------------------------------------------------!

    IMPLICIT NONE

    REAL,   INTENT (out  ) :: Speed
    REAL,   INTENT (in   ) :: U, V
!------------------------------------------------------------------------------!

    Speed = sqrt( U*U + V*V )

END SUBROUTINE DA_Transform_XToSpeed


