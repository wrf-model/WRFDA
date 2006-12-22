subroutine da_balance_geoterm_nl( xb, k, u, v, term_x, term_y ) 
 
   !---------------------------------------------------------------------------
   !  PURPOSE: Calculates nonlinear geostrophic term in balance equation.
   !
   !  METHOD:  Term is k x rho f u on a single level.
   !
   !  ASsumPTIONS: Various (see documentation).
   !---------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in) :: xb           ! First guess structure.
   integer, intent(in)        :: k            ! Model level.
   real, intent(in)           :: u(:,:)       ! u wind comp. (dot pts)
   real, intent(in)           :: v(:,:)       ! v wind comp. (dot pts)
   real, intent(out)          :: term_x(:,:)  ! x component of term.
   real, intent(out)          :: term_y(:,:)  ! y component of term.

   !---------------------------------------------------------------------------
   !  [1.0] Initialise:
   !---------------------------------------------------------------------------

      term_x(:,:) = 0.0
      term_y(:,:) = 0.0

   !---------------------------------------------------------------------------
   !  [2.0] Calculate term_x = -f rho v~:
   !---------------------------------------------------------------------------

   term_x(xb%its:xb%ite, xb%jts:xb%jte) = -xb%rho(xb%its:xb%ite, xb%jts:xb%jte, k) &
                                          *xb%  v(xb%its:xb%ite, xb%jts:xb%jte, k) &
                                          *xb%cori(xb%its:xb%ite, xb%jts:xb%jte)

   !---------------------------------------------------------------------------
   !  [3.0] Calculate term_y = f rho u~:
   !---------------------------------------------------------------------------

   term_y(xb%its:xb%ite, xb%jts:xb%jte) =  xb%rho(xb%its:xb%ite, xb%jts:xb%jte, k) &
                                          *xb%  u(xb%its:xb%ite, xb%jts:xb%jte, k) &
                                          *xb%cori(xb%its:xb%ite, xb%jts:xb%jte)

end subroutine da_balance_geoterm_nl


