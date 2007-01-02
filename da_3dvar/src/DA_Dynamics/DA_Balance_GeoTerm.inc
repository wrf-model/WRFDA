SUBROUTINE DA_Balance_GeoTerm_NL( xb, k, u, v, term_x, term_y ) 
 
!------------------------------------------------------------------------------
!  PURPOSE: Calculates nonlinear geostrophic term in balance equation.
!
!  METHOD:  Term is k x rho f u on a single level.
!
!  ASSUMPTIONS: Various (see documentation).
!
!  HISTORY: 03/02/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xb_type), INTENT(IN) :: xb           ! First guess structure.
   INTEGER, INTENT(IN)        :: k            ! Model level.
   REAL, INTENT(IN)           :: u(:,:)       ! u wind comp. (dot pts)
   REAL, INTENT(IN)           :: v(:,:)       ! v wind comp. (dot pts)
   REAL, INTENT(OUT)          :: term_x(:,:)  ! x component of term.
   REAL, INTENT(OUT)          :: term_y(:,:)  ! y component of term.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   term_x(:,:) = 0.0
   term_y(:,:) = 0.0

!------------------------------------------------------------------------------
!  [2.0] Calculate term_x = -f rho v~:
!------------------------------------------------------------------------------

   term_x(xb%its:xb%ite, xb%jts:xb%jte) = -xb%rho(xb%its:xb%ite, xb%jts:xb%jte, k) &
                                          *xb%  v(xb%its:xb%ite, xb%jts:xb%jte, k) &
                                          *xb%cori(xb%its:xb%ite, xb%jts:xb%jte)

!------------------------------------------------------------------------------
!  [3.0] Calculate term_y = f rho u~:
!------------------------------------------------------------------------------

   term_y(xb%its:xb%ite, xb%jts:xb%jte) =  xb%rho(xb%its:xb%ite, xb%jts:xb%jte, k) &
                                          *xb%  u(xb%its:xb%ite, xb%jts:xb%jte, k) &
                                          *xb%cori(xb%its:xb%ite, xb%jts:xb%jte)

END SUBROUTINE DA_Balance_GeoTerm_NL

