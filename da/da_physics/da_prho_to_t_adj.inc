subroutine da_prho_to_t_adj(xb, xp, xa) 

   !---------------------------------------------------------------------------
   !  Purpose: Adjoint of da_prho_to_t.
   !
   !  METHOD:  Standard adjoint coding.
   !
   !  ASSUMPTIONS: 1) Model level stored top down.
   !---------------------------------------------------------------------------

   implicit none
   
   type (xb_type), intent(in)    :: xb           ! First guess structure.
   type (xpose_type), intent(in) :: xp           ! Dimensions and xpose buffers.
   type (x_type), intent(inout)  :: xa           ! Increment structure.

   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.
   integer                       :: ks, ke       ! 3rd dim. end points.
   integer                       :: k            ! Loop counter.
   real                  :: temp(xp%its:xp%ite,xp%jts:xp%jte) ! Temporary array.

   !---------------------------------------------------------------------------
   !  [1.0] Initialise:
   !---------------------------------------------------------------------------

   is = xp%its
   ie = xp%ite
   js = xp%jts
   je = xp%jte
   ks = xp%kts
   ke = xp%kte
   
   if (Testing_WRFVAR) then
      is = xb%its-1
      js = xb%jts-1

      ie = xb%ite+1
      je = xb%jte+1

      if (is < xb%ids) is = xb%ids
      if (js < xb%jds) js = xb%jds

      if (ie > xb%ide) ie = xb%ide
      if (je > xb%jde) je = xb%jde
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Calculate temperature increments:
   !---------------------------------------------------------------------------

   do k = ks, ke
      temp(is:ie,js:je) = xa % t(is:ie,js:je,k) * xb % t(is:ie,js:je,k)

      xa % p(is:ie,js:je,k) = xa % p(is:ie,js:je,k) + &
                            temp(is:ie,js:je) / xb % p(is:ie,js:je,k)
                         
      xa % rho(is:ie,js:je,k) = xa % rho(is:ie,js:je,k) - &
                              temp(is:ie,js:je) / xb % rho(is:ie,js:je,k)
   end do  
                             
end subroutine da_prho_to_t_adj


