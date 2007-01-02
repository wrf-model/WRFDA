
SUBROUTINE DA_Transform_XToSpeed_Lin(TGL_speed,U,V,TGL_u,TGL_v)

!------------------------------------------------------------------------------!
! Convert (U-V in m/s) components into wind speed (Speed in m/s)
!------------------------------------------------------------------------------!

    IMPLICIT NONE

    REAL,     INTENT (out  )    :: TGL_speed
    REAL,     INTENT (in   )    :: U,V
    REAL,     INTENT (in   )    :: TGL_u,TGL_v
    REAL                        :: speed
!------------------------------------------------------------------------------!

    speed = sqrt( U*U + V*V + 1.E-6)

    TGL_speed = (U*TGL_u + V*TGL_v)/speed

END SUBROUTINE DA_Transform_XToSpeed_Lin


