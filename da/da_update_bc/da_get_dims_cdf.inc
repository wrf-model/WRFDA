subroutine da_get_dims_cdf(file, var, dims, ndims, debug)
 
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
        
   implicit none

#include "netcdf.inc"

   character (len=80), intent(in) :: file
   character (len=*), intent(in) :: var
   logical, intent(in) :: debug
   integer, intent(out), dimension(4) :: dims
   integer, intent(out) :: ndims

   integer cdfid, rcode, id_time
   character (len=80) :: varnam
   integer :: natts, dimids(10)
   integer :: i, ivtype

   cdfid = ncopn(file, NCNOWRIT, rcode)

   if (rcode == 0) then
      if (debug) write(6,*) ' open netcdf file ', trim(file)
   else
      write(6,*) ' error openiing netcdf file ', trim(file)
      stop
   end if

   id_time = ncvid(cdfid, var, rcode)

   rcode = nf_inq_var(cdfid, id_time, varnam, ivtype, ndims, dimids, natts)
   if (debug) then
      write(6,*) ' number of dims for ',var,' ',ndims
   end if
   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), dims(i))
      if (debug) write(6,*) ' dimension ',i,dims(i)
   end do

   call ncclos(cdfid,rcode)

end subroutine da_get_dims_cdf


