Subroutine DA_Difference ( diff, xb24, xb12,its,ite, jts,jte, kts,kte)

!-------------------------------------------------------------------------
!  PURPOSE: Caculation of Xprime
!
!  HISTORY: 09/01/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

   IMPLICIT NONE

   type (statistics_type), intent(out) :: diff
   type (xb_type), intent(in)  :: xb24, xb12
   integer, intent(in)         :: its,ite, jts,jte, kts,kte
   integer                     :: i,j

   diff%u =xb24%u  - xb12%u
   diff%v =xb24%v  - xb12%v
   diff%t =xb24%t  - xb12%t
   diff%q =xb24%q  - xb12%q

   diff%rh=(xb24%rh - xb12%rh)/100.

   diff%psfc=xb24%psfc - xb12%psfc

END SUBROUTINE DA_Difference


