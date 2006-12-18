SUBROUTINE DA_PRho_To_T_Adj( xb, xp, xa ) 
 
!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of DA_PRho_To_T.
!
!  METHOD:  Standard adjoint coding.
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
   INTEGER                       :: k            ! Loop counter.
   REAL                  :: temp(xp%its:xp%ite,xp%jts:xp%jte) ! Temporary array.
   
!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   is = xp%its
   ie = xp%ite
   js = xp%jts
   je = xp%jte
   ks = xp%kts
   ke = xp%kte
   
   if ( Testing_3DVAR ) then
      is = xb%its-1
      js = xb%jts-1

      ie = xb%ite+1
      je = xb%jte+1

      if ( is < xb%ids ) is = xb%ids
      if ( js < xb%jds ) js = xb%jds

      if ( ie > xb%ide ) ie = xb%ide
      if ( je > xb%jde ) je = xb%jde
   endif

!------------------------------------------------------------------------------
!  [2.0] Calculate temperature increments:
!------------------------------------------------------------------------------

   do k = ks, ke
   
      temp(is:ie,js:je) = xa % t(is:ie,js:je,k) * xb % t(is:ie,js:je,k)

      xa % p(is:ie,js:je,k) = xa % p(is:ie,js:je,k) + &
                            temp(is:ie,js:je) / xb % p(is:ie,js:je,k)
                         
      xa % rho(is:ie,js:je,k) = xa % rho(is:ie,js:je,k) - &
                              temp(is:ie,js:je) / xb % rho(is:ie,js:je,k)
   end do  
                             
END SUBROUTINE DA_PRho_To_T_Adj
