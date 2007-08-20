subroutine da_read_bufrssmis (obstype,iv,xp,infile)

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
   type (xpose_type), intent (in)    :: xp
   type (iv_type),    intent (inout) :: iv

   write(*,*) ' To be implemented for SSMIS'

end subroutine da_read_bufrssmis


