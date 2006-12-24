subroutine da_tprh_to_q_lin( xb, xp, xa )

   !---------------------------------------------------------------------------
   ! Purpose: Convert T/pressure/rh to specific humidity increments.
   !
   ! METHOD: q~ = q (rh~/rh + qs~/qs)
   !---------------------------------------------------------------------------

   implicit none
   
   type (xb_type), intent(in)    :: xb           ! First guess structure.
   type (xpose_type), intent(in) :: xp           ! Dimensions and xpose buffers.
   type (x_type), intent(inout)  :: xa           ! Increment structure.
   
   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.
   integer                       :: ks, ke       ! 3rd dim. end points.
   real :: qs_prime_over_qs(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte) ! qs~/qs.

   !---------------------------------------------------------------------------
   !  [1.0] Initialise:
   !---------------------------------------------------------------------------

   is = xp%its; ie = xp%ite
   js = xp%jts; je = xp%jte
   ks = xp%kts; ke = xp%kte   

   !---------------------------------------------------------------------------
   ! [1.0] Calculate saturation specific humidity ratio qs~/qs:
   !---------------------------------------------------------------------------

   call da_tp_to_qs_lin( xb, xp, xa, qs_prime_over_qs )
   
   !---------------------------------------------------------------------------
   ! [2.0] Calculate specific humidity increment:
   !---------------------------------------------------------------------------

   xa % q(is:ie,js:je,ks:ke) = xb % q(is:ie,js:je,ks:ke) * &
                               ( xa % rh(is:ie,js:je,ks:ke) / &
                                 xb % rh(is:ie,js:je,ks:ke) + &
                                 qs_prime_over_qs(is:ie,js:je,ks:ke) )

end subroutine da_tprh_to_q_lin


