subroutine da_tpq_to_rh_lin( xb, xp, xa )

   !---------------------------------------------------------------------------
   !  Purpose: Convert T/pressure/q to relative humidity increments.
   !
   !  METHOD: r~ = r (q~/q - qs~/qs).
   !
   ! When q approaching to zero, the above formula is undefined. The
   ! general formula below must be used:
   ! 
   !  METHOD: r~ = 100 * (q~/qs - q*(qs~/qs)/qs))
   !             = 100 * q~/qs - (100*q/qs)*(qs~/qs)
   !             = 100 * q~/qs - rh * (qs~/qs) 
   !
   !---------------------------------------------------------------------------

   implicit none
   
   type (xb_type), intent(in)    :: xb           ! First guess structure.
   type (xpose_type), intent(in) :: xp           ! Dimensions and xpose buffers.
   type (x_type), intent(inout)  :: xa           ! Increment structure.
   
   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.
   integer                       :: ks, ke       ! 3rd dim. end points.
   real, dimension(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte) :: qs, es, &
                                                       qs_prime_over_qs

   !---------------------------------------------------------------------------
   ! [1.0] Initialise:
   !---------------------------------------------------------------------------

   is = xp%its; ie = xp%ite
   js = xp%jts; je = xp%jte
   ks = xp%kts; ke = xp%kte   

   !---------------------------------------------------------------------------
   ! [1.0] Calculate saturation specific humidity ratio qs~/qs:
   !---------------------------------------------------------------------------
 
   call da_tp_to_qs_lin( xb, xp, xa, qs_prime_over_qs )

   !--------------------------------------------------------------------------
   ! [2.0] Culcalete background saturation specific humidity qs:
   !--------------------------------------------------------------------------

   call da_tp_to_qs1( xb, xp, es, qs) 
   
   !---------------------------------------------------------------------------
   ! [3.0] Calculate relative humidity increment:
   !---------------------------------------------------------------------------

   xa % rh(is:ie,js:je,ks:ke) = 100. * &
                                ( xa % q(is:ie,js:je,ks:ke) / &
                                      qs(is:ie,js:je,ks:ke) ) - &
                                  xb % rh(is:ie,js:je,ks:ke) * &
                                  qs_prime_over_qs(is:ie,js:je,ks:ke)

end subroutine da_tpq_to_rh_lin


