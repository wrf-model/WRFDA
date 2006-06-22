subroutine da_transform_xtoseasfcwind(U,V,Speed,zhmkz)

   !----------------------------------------------------------------------------
   ! Convert (U-V in m/s) components into wind speed (Speed in m/s)
   !----------------------------------------------------------------------------

   IMPLICIT NONE

   REAL,   INTENT (out  ) :: Speed
   REAL,   INTENT (in   ) :: U, V, zhmkz
   REAL                   :: usfc, vsfc

   if (trace_use) call da_trace_entry("da_transform_xtoseasfcwind")

   usfc   = U*log(10./0.0001)/log(zhmkz/0.0001) ! roughness = 0.0001
   vsfc   = V*log(10./0.0001)/log(zhmkz/0.0001) ! roughness = 0.0001
   speed  = sqrt(usfc*usfc + vsfc*vsfc)

   if (trace_use) call da_trace_exit("da_transform_xtoseasfcwind")

end subroutine da_transform_xtoseasfcwind

