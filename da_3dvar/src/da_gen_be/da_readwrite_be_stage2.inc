subroutine da_readwrite_be_stage2( outunit, be_method, nk )

   implicit none

   integer, intent(in)      :: outunit                    ! Output unit number.
   character*3, intent(in)  :: be_method                  ! Be method ('NMC', or 'ENS')
   integer, intent(out)     :: nk                         ! Number of vertical levels/modes.

   character*80             :: filename                   ! Input filename.

   integer                  :: ni, nj                     ! Number of points in x/y direction.
   integer                  :: num_bins                   ! Number of 3D bins.

   real, allocatable   :: regcoeff1(:)                    ! psi/chi regression cooefficient.
   real, allocatable   :: regcoeff2(:,:)                  ! psi/ps regression cooefficient.
   real, allocatable   :: regcoeff3(:,:,:)                ! psi/T regression cooefficient.
 
   filename = 'gen_be_stage2.'//trim(be_method)//'.dat'
   open (iunit, file = filename, form='unformatted')
   read(iunit)ni, nj, nk
   read(iunit)num_bins

   allocate( regcoeff1(1:num_bins) )
   allocate( regcoeff2(1:nk,1:num_bins) )
   allocate( regcoeff3(1:nk,1:nk,1:num_bins) )

   read(iunit)regcoeff1 ! psi/chi regression coefficient.
   read(iunit)regcoeff2 ! psi/ps  regression coefficient.
   read(iunit)regcoeff3 ! psi/T   regression coefficient.
   close(iunit)

   write(outunit)ni, nj, nk
   write(outunit)num_bins
   write(outunit)regcoeff1
   write(outunit)regcoeff2
   write(outunit)regcoeff3

   deallocate( regcoeff1 )
   deallocate( regcoeff2 )
   deallocate( regcoeff3 )

end subroutine da_readwrite_be_stage2
