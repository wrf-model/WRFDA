SUBROUTINE da_tprh_to_q_lin( xb, xp, xa )

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

   CALL da_tp_to_qs_lin( xb, xp, xa, qs_prime_over_qs )
   
!------------------------------------------------------------------------------
!  [2.0] Calculate specific humidity increment:
!------------------------------------------------------------------------------

   xa % q(is:ie,js:je,ks:ke) = xb % q(is:ie,js:je,ks:ke) * &
                               ( xa % rh(is:ie,js:je,ks:ke) / &
                                 xb % rh(is:ie,js:je,ks:ke) + &
                                 qs_prime_over_qs(is:ie,js:je,ks:ke) )

END SUBROUTINE da_tprh_to_q_lin
