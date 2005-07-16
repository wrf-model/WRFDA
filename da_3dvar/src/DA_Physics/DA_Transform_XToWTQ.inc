Subroutine DA_Transform_XToWTQ ( xp, xb, xa )

   IMPLICIT NONE

   TYPE (x_type),  INTENT(INOUT) :: xa       ! gridded analysis increment.
   TYPE (xb_type), INTENT(IN)    :: xb       ! first guess state.
   TYPE (xpose_type), intent(in) :: xp       ! Domain decomposition vars.

   integer                      :: i, j, kz

! --------------------------------------------------------------------------
!  [1.0] Calculate surface variable(1-m wind, 2-m moisture and temperature)
! --------------------------------------------------------------------------

   kz = xp%kts

! --------------------------------------------------------------------------
!  [2.0] Calculate sfc variable perturbations at the cross point
! --------------------------------------------------------------------------

   do j=xp%jts, xp%jte
   do i=xp%its, xp%ite
      xa%tgrn(i,j) = 0.0

      call sfc_wtq_Lin(xb%psfc(i,j), xb%tgrn(i,j), &
                       xb%p(i,j,kz), xb%t(i,j,kz), xb%q(i,j,kz), &
                       xb%u(i,j,kz), xb%v(i,j,kz), &
                       xb%regime(i,j), &
                       xa%psfc(i,j), xa%tgrn(i,j), &
                       xa%p(i,j,kz), xa%t(i,j,kz), xa%q(i,j,kz), &
                       xa%u(i,j,kz), xa%v(i,j,kz), &
                       xb%h(i,j,kz), xb%rough(i,j), xb%xland(i,j), &
                       xa%u10(i,j), xa%v10(i,j), &
                       xa%t2(i,j),  xa%q2(i,j))
   enddo
   enddo

END Subroutine DA_Transform_XToWTQ
 
