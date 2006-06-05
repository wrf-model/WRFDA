SUBROUTINE DA_PRho_To_T_Lin( xb, xp, xa ) 
 
!------------------------------------------------------------------------------
!  PURPOSE: Calculates temperature increments from pressure/density increments.
!
!  METHOD:  Linearised ideal gas law: T~ = T ( p"/p - rho"/rho )
!
!  ASSUMPTIONS: 1) Model level stored top down.
!
!  HISTORY: 03/21/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xb_type), INTENT(IN)    :: xb           ! First guess structure.
   TYPE (xpose_type), INTENT(IN) :: xp           ! Dimensions and xpose buffers.
   TYPE (x_type), INTENT(INOUT)  :: xa           ! Increment structure.

   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.
   integer                       :: ks, ke       ! 3rd dim. end points.
!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   is = xp%its
   ie = xp%ite
   js = xp%jts
   je = xp%jte
   ks = xp%kts
   ke = xp%kte

!------------------------------------------------------------------------------
!  [2.0] Calculate temperature increments:
!------------------------------------------------------------------------------

   xa % t(is:ie,js:je,ks:ke) = xb % t(is:ie,js:je,ks:ke) * &
                            ( xa % p(is:ie,js:je,ks:ke) / &
                              xb % p(is:ie,js:je,ks:ke) - &
                              xa % rho(is:ie,js:je,ks:ke) / &
                              xb % rho(is:ie,js:je,ks:ke) )     

END SUBROUTINE DA_PRho_To_T_Lin
