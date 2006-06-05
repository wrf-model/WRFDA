SUBROUTINE DA_TPRH_To_Q_Lin( xb, xp, xa )

!------------------------------------------------------------------------------
!  PURPOSE: Convert T/pressure/rh to specific humidity increments.
!
!  METHOD: q~ = q (rh~/rh + qs~/qs).
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!           02/21/2002 - Modified for parallel version.     Al Bourgeois
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xb_type), INTENT(IN)    :: xb           ! First guess structure.
   TYPE (xpose_type), INTENT(IN) :: xp           ! Dimensions and xpose buffers.
   TYPE (x_type), INTENT(INOUT)  :: xa           ! Increment structure.
   
   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.
   integer                       :: ks, ke       ! 3rd dim. end points.
   real :: qs_prime_over_qs(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte) ! qs~/qs.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   is = xp%its; ie = xp%ite
   js = xp%jts; je = xp%jte
   ks = xp%kts; ke = xp%kte   

!------------------------------------------------------------------------------
!  [1.0] Calculate saturation specific humidity ratio qs~/qs:
!------------------------------------------------------------------------------

   CALL DA_TP_To_Qs_Lin( xb, xp, xa, qs_prime_over_qs )
   
!------------------------------------------------------------------------------
!  [2.0] Calculate specific humidity increment:
!------------------------------------------------------------------------------

   xa % q(is:ie,js:je,ks:ke) = xb % q(is:ie,js:je,ks:ke) * &
                               ( xa % rh(is:ie,js:je,ks:ke) / &
                                 xb % rh(is:ie,js:je,ks:ke) + &
                                 qs_prime_over_qs(is:ie,js:je,ks:ke) )

END SUBROUTINE DA_TPRH_To_Q_Lin

SUBROUTINE DA_TPRH_To_Q_Lin1( t, p, es, q, rh, &
                              t_prime, p_prime, rh_prime, q_prime )

!------------------------------------------------------------------------------
!  PURPOSE: Convert T/pressure/rh to specific humidity increments.
!
!  METHOD: q~ = q (rh~/rh + qs~/qs).
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE

   REAL, INTENT(IN)              :: t        ! Temperature.
   REAL, INTENT(IN)              :: p        ! Pressure.
   REAL, INTENT(IN)              :: es       ! Saturation vapour pressure.
   REAL, INTENT(IN)              :: q        ! Specific humidity.
   REAL, INTENT(IN)              :: rh       ! Relative Humidity.
   REAL, INTENT(IN)              :: t_prime  ! Temperature increment.
   REAL, INTENT(IN)              :: p_prime  ! Pressure increment.
   REAL, INTENT(IN)              :: rh_prime ! Pressure increment.
   REAL, INTENT(OUT)             :: q_prime  ! Pressure increment.
   
   REAL                          :: qs_prime_over_qs ! qs~/qs.

!------------------------------------------------------------------------------
!  [1.0] Calculate saturation specific humidity ratio qs~/qs:
!------------------------------------------------------------------------------

   CALL DA_TP_To_Qs_Lin1( t, p, es, t_prime, p_prime, qs_prime_over_qs )
   
!------------------------------------------------------------------------------
!  [2.0] Calculate specific humidity increment:
!------------------------------------------------------------------------------

   q_prime = q * ( rh_prime / rh + qs_prime_over_qs )

END SUBROUTINE DA_TPRH_To_Q_Lin1
