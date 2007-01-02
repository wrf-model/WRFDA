SUBROUTINE DA_PT_To_Rho_Adj( xb, xa ) 
 
!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of DA_PT_To_Rho.
!
!  METHOD:  Standard adjoint coding.
!
!  ASSUMPTIONS: 1) Model level stored top down.
!
!  HISTORY: 03/21/2000 - Creation of F90 version.           Dale Barker
!  MODIFY:  04/23/2003 - For MPI                            Wei  Huang
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xb_type), INTENT(IN)    :: xb           ! First guess structure.
   TYPE (x_type),  INTENT(INOUT) :: xa           ! Increment structure.

   INTEGER                       :: i,j,k        ! Loop counter.

   INTEGER                       :: is, ie, js, je

   REAL                          :: temp
   
!------------------------------------------------------------------------------

   is = xb%its
   js = xb%jts

   ie = xb%ite
   je = xb%jte

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
!  Calculate rho increments:
!------------------------------------------------------------------------------

   do j=js, je
   do k=xb%kts, xb%kte
   do i=is, ie
      temp = xa%rho(i,j,k) * xb%rho(i,j,k)

      xa%p(i,j,k) = xa%p(i,j,k) + temp/xb%p(i,j,k)
                            
      xa%t(i,j,k) = xa%t(i,j,k) - temp/xb%t(i,j,k)

      xa%rho(i,j,k) = 0.0
   end do        
   end do        
   end do        

END SUBROUTINE DA_PT_To_Rho_Adj

