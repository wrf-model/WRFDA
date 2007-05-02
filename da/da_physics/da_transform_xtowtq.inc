subroutine da_transform_xtowtq (xp, xb, xa)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------
 
   implicit none

   type (x_type),  intent(inout) :: xa       ! gridded analysis increment.
   type (xb_type), intent(in)    :: xb       ! first guess state.
   type (xpose_type), intent(in) :: xp       ! domain decomposition vars.

   integer                      :: i, j, kz
   real                         :: height

   !------------------------------------------------------------------------
   ! [1.0] Calculate surface variable(1-m wind, 2-m moisture and temperature)
   !------------------------------------------------------------------------

   kz = xp%kts

   !------------------------------------------------------------------------
   ! [2.0] Calculate sfc variable perturbations at the cross point
   !------------------------------------------------------------------------

   do j=xp%jts, xp%jte
      do i=xp%its, xp%ite
         xa%tgrn(i,j) = 0.0
         height = xb%h(i,j,kz) - xb%terr(i,j)                 
         if (height <= 0.0) then
            message(1) = "Negative height found"
            write(unit=message(2),FMT='(2I6,A,F10.2,A,F10.2)') &
               i,j,' ht = ',xb%h(i,j,kz) ,' terr =  ',xb%terr(i,j)
            call da_error(__FILE__,__LINE__,message(1:2))
         endif
         call da_sfc_wtq_lin(xb%psfc(i,j), xb%tgrn(i,j), &
                       xb%p(i,j,kz), xb%t(i,j,kz), xb%q(i,j,kz), &
                       xb%u(i,j,kz), xb%v(i,j,kz), &
                       xb%regime(i,j), &
                       xa%psfc(i,j), xa%tgrn(i,j), &
                       xa%p(i,j,kz), xa%t(i,j,kz), xa%q(i,j,kz), &
                       xa%u(i,j,kz), xa%v(i,j,kz), &
                       height      , xb%rough(i,j), xb%xland(i,j), &
                       xa%u10(i,j), xa%v10(i,j), &
                       xa%t2(i,j),  xa%q2(i,j))
      end do
   end do

end subroutine da_transform_xtowtq
 

