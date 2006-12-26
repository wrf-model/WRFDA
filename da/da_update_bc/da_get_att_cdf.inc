subroutine da_get_att_cdf(file, var, debug)
 
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

#include "netcdf.inc"

   character (len=80), intent(in) :: file
   character (len=*), intent(in) :: var
   logical, intent(in) :: debug

   integer :: cdfid, status, varid, n, natts
   character (len=256) :: att_name

   status = NF_OPEN(file, NF_NOWRITE, cdfid)

   status = NF_inQ_VARID(cdfid, var, varid)

   if (status == 0) then
      if (debug) write(6,*) ' open netcdf file ', trim(file)
   else
      write(6,*) ' error openiing netcdf file ', trim(file)
      stop
   end if

   status = NF_inQ_VARNATTS(cdfid, varid, natts)

   do n=1, natts
      status = NF_inQ_ATTNAME(cdfid, varid, n, att_name)

      write(unit=*, fmt='(a,i2,2a)') &
        'att_name(',n,')=', trim(att_name)
   end do

   status = NF_close(cdfid)

end subroutine da_get_att_cdf


