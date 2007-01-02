SUBROUTINE DA_Transform_XToTPW( xa, xb )

!------------------------------------------------------------------------
!  Modified: 04/25/03                                   Wei Huang
!------------------------------------------------------------------------

   implicit none
   
   type (x_type),  intent(inout) :: xa       ! gridded analysis increment.
   type (xb_type), intent(in)    :: xb       ! first guess state.

   integer :: i, j, K

   real    :: pw

!--WEIGHTED SUM OF VERTICAL COLUMN
   do j=xb%jts, xb%jte
   do i=xb%its, xb%ite
      pw = 0.0
      do k=xb%kts, xb%kte
         pw = pw + (xb%hf(i,j,k+1)-xb%hf(i,j,k)) &
                 * (xa%q(i,j,k)*xb%rho(i,j,k) &
                 +  xb%q(i,j,k)*xa%rho(i,j,k) )
      enddo
 
!-----To convert the unit of PW to cm:
      xa%tpw(i,j) = 0.1*pw
   enddo
   enddo
 
END SUBROUTINE DA_Transform_XToTPW

SUBROUTINE INTEGRAT_dZ( xb )

!------------------------------------------------------------------------------
!  Non-linear PW forward operator.
!  ===============================
!
!  PURPOSE: To calculate the IWV from the model QV and PP, TT.
!
!  METHOD:  IWV = SUM {QV * RHO * dZ}
!
!           Unit: Qv (Kg/Kg), RHO(Kg/M^3), dZ(M)
!                 PW (cm)
!
!  Input     : QV, PP, TT
!
!  Output    : PW
!         
!  Modified for WRF 3DVAR: 04/18/03                     Wei Huang
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

   type (xb_type), intent(inout) :: xb    ! first guess (local).

   integer :: i, j, K 

   real    :: pw

!--WEIGHTED SUM OF VERTICAL COLUMN 

   do j=xb%jts, xb%jte
   do i=xb%its, xb%ite
      pw = 0.0
      do k=xb%kts, xb%kte
         pw = pw + (xb%hf(i,j,k+1)-xb%hf(i,j,k)) &
                 *  xb%q(i,j,k)*xb%rho(i,j,k)
      enddo

      xb%tpw(i,j) = 0.1*pw
   enddo
   enddo

END SUBROUTINE INTEGRAT_dZ

