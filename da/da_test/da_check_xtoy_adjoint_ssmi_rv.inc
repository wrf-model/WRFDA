SUBROUTINE DA_Check_XToY_Adjoint_SSMI_Rv( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(INOUT)  :: y             ! y = h (xa)
   REAL          , INTENT(INOUT)  :: adjtest_lhs, pertile_lhs   

   INTEGER                        :: n             ! loop counter.

   real :: var

   IF ( oi%num_ssmi_retrieval > 0 ) THEN

   do n=1, oi%num_ssmi_retrieval
      if( oi%ssmi_retrieval(n)%loc%proc_domain_with_halo ) then
         y%ssmi_retrieval(n)%speed = y%ssmi_retrieval(n)%speed/typical_speed_rms
         y%ssmi_retrieval(n)%tpw   = y%ssmi_retrieval(n)%tpw/typical_tpw_rms

         var = (y%ssmi_retrieval(n)%speed) ** 2 &
             + (y%ssmi_retrieval(n)%tpw) ** 2

         pertile_lhs = pertile_lhs + var

         if( oi%ssmi_retrieval(n)%loc%proc_domain ) then
            adjtest_lhs = adjtest_lhs + var
         endif

         y%ssmi_retrieval(n)%speed = y%ssmi_retrieval(n)%speed/typical_speed_rms
         y%ssmi_retrieval(n)%tpw   = y%ssmi_retrieval(n)%tpw/typical_tpw_rms
      endif
   end do

   ENDIF

END SUBROUTINE DA_Check_XToY_Adjoint_SSMI_Rv

