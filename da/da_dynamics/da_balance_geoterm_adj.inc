subroutine da_balance_geoterm_adj( cori, rho, term_x, term_y, u, v)
 
   !---------------------------------------------------------------------------
   ! Purpose: Adjoint of da_balance_geoterm.
   !---------------------------------------------------------------------------

   implicit none
   
   real, dimension(ims:ime,jms:jme),intent(in):: cori, &  ! Coriolis factor.
                                                 rho, &   ! Density
                                                 term_x,& ! x component of term.
                                                 term_y   ! y component of term.

   real, dimension(ims:ime,jms:jme), intent(inout) :: u, & ! u wind increment
                                                      v    ! v wind increment

   !---------------------------------------------------------------------------
   ! [2.0] Calculate term_y = f rho u~:
   !---------------------------------------------------------------------------

   u(its:ite,jts:jte) = u(its:ite,jts:jte) + &
                      rho(its:ite,jts:jte) * cori(its:ite,jts:jte) * &
                   term_y(its:ite,jts:jte)

   !---------------------------------------------------------------------------
   ! [1.0] Calculate term_x = -f rho v~:
   !---------------------------------------------------------------------------

   v(its:ite,jts:jte) = v(its:ite,jts:jte) - &
                      rho(its:ite,jts:jte) * cori(its:ite,jts:jte) * &
                   term_x(its:ite,jts:jte)

end subroutine da_balance_geoterm_adj


