subroutine da_pt_to_rho_lin(xb, xa, xp)

   !---------------------------------------------------------------------------
   ! Purpose: Calculates density increments from pressure/temperature increments
   !
   ! Method:  Linearised ideal gas law: rho~/rho = p'/p - T'/T
   !
   ! Assumptions: 1) Model level stored top down.
   !---------------------------------------------------------------------------

   implicit none
   
   type (xb_type), intent(in)    :: xb           ! First guess structure.
   type (x_type), intent(inout)  :: xa           ! increment structure.
   type (xpose_type), intent(in) :: xp           ! Dimensions and xpose buffers.

   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.
   integer                       :: ks, ke       ! 3rd dim. end points.

   !---------------------------------------------------------------------------
   ! [1.0] initialise:
   !---------------------------------------------------------------------------

   is = xp%its
   ie = xp%ite
   js = xp%jts
   je = xp%jte
   ks = xp%kts
   ke = xp%kte

   !---------------------------------------------------------------------------
   ! [2.0] Calculate temperature increments:
   !---------------------------------------------------------------------------

   xa % rho(is:ie,js:je,ks:ke) = xb % rho(is:ie,js:je,ks:ke) * &
                               (xa % p(is:ie,js:je,ks:ke) / &
                                 xb % p(is:ie,js:je,ks:ke) - &
                                 xa % t(is:ie,js:je,ks:ke) / &
                                 xb % t(is:ie,js:je,ks:ke))                       

end subroutine da_pt_to_rho_lin


