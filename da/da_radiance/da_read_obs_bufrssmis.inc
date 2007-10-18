subroutine da_read_obs_bufrssmis (obstype,iv,infile)

   !---------------------------------------------------------------------------
   !  Purpose: read in NCEP bufr SSM/IS data to innovation structure
   !
   !   METHOD: use F90 sequential data structure to avoid reading file twice  
   !            1. read file radiance data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !               and deallocate sequential data structure
   !---------------------------------------------------------------------------

   implicit none

   character(5) ,     intent (in)    :: obstype    ! ssmis
   character(20),     intent (in)    :: infile     ! ssmis.bufr
   type (iv_type),    intent (inout) :: iv

   if (trace_use) call da_trace_entry("da_read_obs_bufrssmis")

   write(*,*) ' To be implemented for SSMIS'

   if (trace_use) call da_trace_exit("da_read_obs_bufrssmis")

end subroutine da_read_obs_bufrssmis


ubroutine da_calculate_grady_satem


