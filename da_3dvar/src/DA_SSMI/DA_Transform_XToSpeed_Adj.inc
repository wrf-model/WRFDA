SUBROUTINE DA_Transform_XToSpeed_Adj(ADJ_speed,U,V,ADJ_u,ADJ_v)

!------------------------------------------------------------------------------!
! Convert (U-V in m/s) components into wind speed (Speed in m/s)
!------------------------------------------------------------------------------!

    IMPLICIT NONE

    INTEGER                :: i, j
    REAL,   INTENT (in   ) :: ADJ_speed
    REAL,   INTENT (in   ) :: U, V
    REAL,   INTENT (inout) :: ADJ_u, ADJ_v
    REAL                   :: speed

!------------------------------------------------------------------------------!

    speed = sqrt(U*U+V*V+ 1.E-6)

    ADJ_u = U*ADJ_speed/speed + ADJ_u
    ADJ_v = V*ADJ_speed/speed + ADJ_v

END SUBROUTINE DA_Transform_XToSpeed_Adj

