SUBROUTINE DA_Assign_Value( xb_in, xb_out )
   IMPLICIT NONE

!-------------------------------------------------------------------------
!  PURPOSE: Assign Value
!
!  HISTORY: 09/01/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

   TYPE (xb_type), intent(in)  :: xb_in
   TYPE (xb_type), intent(out) :: xb_out

   xb_out % u  = xb_in % u
   xb_out % v  = xb_in % v 
   xb_out % t  = xb_in % t 
   xb_out % p  = xb_in % p 
   xb_out % q  = xb_in % q 

   xb_out % h  =  xb_in % h  
   xb_out % hf = xb_in % hf
   xb_out % rh = xb_in % rh  

   xb_out % psfc = xb_in % psfc

   xb_out % lat = xb_in % lat

END SUBROUTINE DA_Assign_Value

