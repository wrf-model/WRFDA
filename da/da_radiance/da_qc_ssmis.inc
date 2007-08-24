subroutine da_qc_ssmis (i, nchan, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for ssmis data.
   !---------------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: i          ! sensor index.
   integer,        intent(in)    :: nchan      ! number of channel
   type (y_type),  intent(in)    :: ob         ! Observation structure.
   type (iv_type), intent(inout) :: iv         ! O-B structure.

   if (trace_use_dull) call da_trace_entry("da_qc_ssmis")

   write(*,*) 'To be implemented for SSMIS'

   if (trace_use_dull) call da_trace_exit("da_qc_ssmis")

end subroutine da_qc_ssmis


