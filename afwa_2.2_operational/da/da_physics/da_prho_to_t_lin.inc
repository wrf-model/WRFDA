subroutine da_prho_to_t_lin(xb, xp, xa) 

   !---------------------------------------------------------------------------
   ! Purpose: Calculates temperature increments from pressure/density increments
   !
   ! Method:  Linearised ideal gas law: T~ = T (p"/p - rho"/rho)
   !
   ! Assumptions: 1) Model level stored top down.
   !---------------------------------------------------------------------------

   implicit none
   
   type (xb_type),    intent(in)    :: xb        ! First guess structure.
   type (xpose_type), intent(in)    :: xp        ! Dimensions and xpose buffers.
   type (x_type),     intent(inout) :: xa        ! increment structure.

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

   xa % t(is:ie,js:je,ks:ke) = xb % t(is:ie,js:je,ks:ke) * &
                            (xa % p(is:ie,js:je,ks:ke) / &
                              xb % p(is:ie,js:je,ks:ke) - &
                              xa % rho(is:ie,js:je,ks:ke) / &
                              xb % rho(is:ie,js:je,ks:ke))     

end subroutine da_prho_to_t_lin


