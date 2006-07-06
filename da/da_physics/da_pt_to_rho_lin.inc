subroutine da_pt_to_rho_lin( xb, xa, xp )

   !---------------------------------------------------------------------------
   ! PURPOSE: Calculates density increments from pressure/temperature increments
   !
   ! METHOD:  Linearised ideal gas law: rho~/rho = p'/p - T'/T
   !
   ! ASSUMPTIONS: 1) Model level stored top down.
   !---------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xb_type), INTENT(IN)    :: xb           ! First guess structure.
   TYPE (x_type), INTENT(INOUT)  :: xa           ! Increment structure.
   TYPE (xpose_type), INTENT(IN) :: xp           ! Dimensions and xpose buffers.

   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.
   integer                       :: ks, ke       ! 3rd dim. end points.

   !---------------------------------------------------------------------------
   ! [1.0] Initialise:
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
                               ( xa % p(is:ie,js:je,ks:ke) / &
                                 xb % p(is:ie,js:je,ks:ke) - &
                                 xa % t(is:ie,js:je,ks:ke) / &
                                 xb % t(is:ie,js:je,ks:ke) )                       

end subroutine da_pt_to_rho_lin

