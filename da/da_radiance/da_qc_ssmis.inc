subroutine da_qc_ssmis (i, nchan, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for ssmis data.
   !---------------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: i          ! sensor index.
   integer,        intent(in)    :: nchan      ! number of channel
   type (y_type),  intent(in)    :: ob         ! Observation structure.
   type (ob_type), intent(inout) :: iv         ! O-B structure.

   write(*,*) 'To be implemented for SSMIS'

end subroutine da_qc_ssmis


