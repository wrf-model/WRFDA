SUBROUTINE DA_HydrostaticP_To_Rho_Lin( xb, xp, p, rho ) 
 
!------------------------------------------------------------------------------
!  PURPOSE: Calculates density increment from pressure increment field.
!
!  METHOD:  Hydrostatic eqn => rho~ = -dP~/dz / g.
!
!  ASSUMPTIONS: 1) Hydrostatic pressure.
!               2) Model level stored top down.
!
!  HISTORY: 03/21/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xb_type), INTENT(IN)     :: xb       ! First guess structure.
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Dimensions and xpose buffers.
   REAL, INTENT(IN)  :: p(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme)   ! Pressure inc. (cross pts)
   REAL, INTENT(OUT) :: rho(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme) ! Density inc. (cross pts)

   integer                    :: is, ie       ! 1st dim. end points.
   integer                    :: js, je       ! 2nd dim. end points.
   integer                    :: ks, ke       ! 3rd dim. end points.
   INTEGER                    :: i, j, k      ! Loop counters.
   REAL                       :: delta1       ! Height difference.
   REAL                       :: delta2       ! Height difference.
   REAL                       :: dPdz         ! Vertical pressure gradient.

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
!  [2.0] Calculate density increments at all levels except top/bottom:
!------------------------------------------------------------------------------

   DO k = ks+1, ke-1
   
     rho(is:ie,js:je,k) =  -( p(is:ie,js:je,k+1) - p(is:ie,js:je,k-1) ) / &
                            ( ( xb % h(is:ie,js:je,k+1) - &
                            xb % h(is:ie,js:je,k-1) ) * gravity )
   END DO                                  

!------------------------------------------------------------------------------
!  [3.0] Calculate density increment on bottom level:
!------------------------------------------------------------------------------

   k = ks
   do j = js, je
      do i = is, ie

!        dP~/dz by backwards one-sided 2nd order finie differences:
         
         delta1 = xb % h(i,j,k+1) - xb % h(i,j,k)
         delta2 = xb % h(i,j,k+2) - xb % h(i,j,k)
         dPdz = -( delta1 + delta2 ) * p(i,j,k)  / ( delta1 * delta2 ) + &
                 ( delta2 / delta1 ) * p(i,j,k+1)/ ( delta2 - delta1 ) - &
                 ( delta1 / delta2 ) * p(i,j,k+2)/ ( delta2 - delta1 )
                      
!        Put together to get density increment at top level:
         rho(i,j,k) = -dPdz / gravity
      
      end do
   end do
                       
!------------------------------------------------------------------------------
!  [4.0] Calculate density increment on top level:
!------------------------------------------------------------------------------

   k = ke
   do j = js, je
      do i = is, ie
      
!        dP~/dz by forwards one-sided 2nd order finite differences:
         
         delta1 = xb % h(i,j,k) - xb % h(i,j,k-1)
         delta2 = xb % h(i,j,k) - xb % h(i,j,k-2)
         
         dPdz = ( delta1 + delta2 ) * p(i,j,k)   / ( delta1 * delta2 ) - &
                ( delta2 / delta1 ) * p(i,j,k-1) / ( delta2 - delta1 ) + &
                ( delta1 / delta2 ) * p(i,j,k-2) / ( delta2 - delta1 )

!        Put together to get density increment at bottom level:
         rho(i,j,k) = -dPdz / gravity

      end do
   end do
   
END SUBROUTINE DA_HydrostaticP_To_Rho_Lin

