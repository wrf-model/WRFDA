subroutine da_stage0_initialize( input_file, var, dim1, dim2, dim3, ds )

   implicit none

#include <netcdf.inc>

   character (len=200), intent(in):: input_file       ! NETCDF file name.
   character (len=10), intent(in) :: var              ! Variable to search for.
   integer, intent(out)           :: dim1             ! Dimensions of field. 
   integer, intent(out)           :: dim2             ! Dimensions of field. 
   integer, intent(out)           :: dim3             ! Dimensions of field. 
   real, intent(out)              :: ds               ! Grid resolution.

   character (len=80)             :: att_name         ! Attribute name.
   integer                        :: i, att           ! Loop counters.
   integer                        :: attlen           ! Length of attribute.
   integer                        :: cdfid            ! NETCDF file id.
   integer                        :: rcode            ! Return code (0=ok).
   integer                        :: length           ! Length of filename.
   integer                        :: id_var           ! NETCDF variable ID. 
   integer                        :: ivtype           ! 4=integer, 5=real, 6=d.p.
   integer                        :: nvars            !
   integer                        :: ndims            !
   integer                        :: natts            ! Number of attributes.
   integer                        :: unlimdimid       ! Number of attributes.

   integer                        :: dimids(10)       !
   integer                        :: dims(1:4)        ! Dimensions of field. 
   real (kind=4), allocatable     :: value_real(:)    ! Real attribute value. 
   character (len=80)             :: value_chr        ! Character.

!  Check file exists:
   length = len_trim(input_file)
   rcode = nf_open( input_file(1:length), NF_NOWRITE, cdfid )
   if ( rcode /= 0) then
     write(6,*) ' Error opening netcdf file ', input_file(1:length)
     stop
   end if

!  Check variable is in file:
   rcode = nf_inq_varid ( cdfid, var, id_var )
   if ( rcode /= 0 ) then 
      write(6,'(2a)')var, ' variable is not in input file. Stop'
      stop
   endif

!  Get metadata (time, dimensions, global attributes):

   rcode = nf_inq_var( cdfid, id_var, var, ivtype, ndims, dimids, natts )
   do i = 1, ndims
     rcode = nf_inq_dimlen( cdfid, dimids(i), dims(i) )
   enddo
   dim1 = dims(1)
   dim2 = dims(2)
   dim3 = dims(3)
!  Get resolution:
   att_name = 'DX'  ! Assume dx= dy.
   rcode = nf_inq_att( cdfid, nf_global, att_name, ivtype, attlen )
   allocate( value_real(1:attlen) )
   rcode = NF_GET_ATT_REAL(cdfid, nf_global, att_name, value_real )
   ds = value_real(1)
   deallocate( value_real )

   rcode = nf_close( cdfid )

end subroutine da_stage0_initialize

