program gen_be_diags_read

   use da_constants
   use da_gen_be

   implicit none

   character*10        :: variable                   ! Variable name
   character*3         :: be_method                  ! Be method ('NMC', or 'ENS')
   character*80        :: filename                   ! Input filename.
   integer             :: outunit                    ! Output unit for diagnostics.
   integer             :: ni, nj, nk                 ! Dimensions read in.
   integer             :: bin_type                   ! Type of bin to average over. !!!DALE ADD.
   integer             :: num_bins_hgt               ! Used if bin_type = 2. !!!DALE ADD..
   integer             :: num_bins                   ! Number of 3D bins.
   integer             :: num_bins2d                 ! Number of 2D bins.
   integer             :: k                          ! Loop counter.
   integer             :: kdum                       ! Dummy vertical index.
   integer             :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees). !!!DALE ADD..
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m). !!!DALE ADD..
   logical             :: data_on_levels             ! False if data is projected onto EOFs.
   logical             :: use_global_eofs            ! True if projected data uses global EOFs.

   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.
   real, allocatable   :: regcoeff1(:)               ! psi/chi regression cooefficient.
   real, allocatable   :: regcoeff2(:,:)             ! psi/ps regression cooefficient.
   real, allocatable   :: regcoeff3(:,:,:)           ! psi/T regression cooefficient.
   real, allocatable   :: e_vec(:,:)                 ! Domain-averaged eigenvectors.
   real, allocatable   :: e_val(:)                   ! Domain-averaged eigenvalues.  
   real, allocatable   :: e_vec_loc(:,:,:)           ! Latitudinally varying eigenvectors.
   real, allocatable   :: e_val_loc(:,:)             ! Latitudinally varying eigenvalues.
   real, allocatable   :: total_power(:)             ! Total Power spectrum.

   be_method = 'NMC'
   filename = 'gen_be.'//trim(be_method)//'.dat'
   open (iunit, file = filename, form='unformatted')

!----------------------------------------------------------------------------
!   [1] Gather regression coefficients.
!----------------------------------------------------------------------------

   read(iunit)ni, nj, nk
   read(iunit)bin_type, num_bins_hgt, binwidth_hgt, binwidth_lat
   read(iunit)num_bins, num_bins2d

   allocate( bin(1:ni,1:nj,1:nk) )
   allocate( bin2d(1:ni,1:nj) )

   call da_create_bins( ni, nj, nk, bin_type, num_bins, num_bins2d, bin, bin2d )

   allocate( regcoeff1(1:num_bins) )
   allocate( regcoeff2(1:nk,1:num_bins2d) )
   allocate( regcoeff3(1:nk,1:nk,1:num_bins2d) )

   read(iunit)regcoeff1
   read(iunit)regcoeff2
   read(iunit)regcoeff3

   outunit = ounit + 100
   call da_print_be_stats_p( outunit, ni, nj, nk, num_bins, num_bins2d, &
                             bin, bin2d, regcoeff1, regcoeff2, regcoeff3 )

!----------------------------------------------------------------------------
!   [2] Gather vertical error eigenvectors, eigenvalues.
!----------------------------------------------------------------------------

   read(iunit)variable
   read(iunit)nk, num_bins2d

   allocate( e_vec(1:nk,1:nk) )
   allocate( e_val(1:nk) )
   allocate( e_vec_loc(1:nk,1:nk,1:num_bins2d) )
   allocate( e_val_loc(1:nk,1:num_bins2d) )

   read(iunit)e_vec
   read(iunit)e_val
   read(iunit)e_vec_loc
   read(iunit)e_val_loc
   call da_print_be_stats_v( outunit, variable, nk, num_bins2d, &
                             e_vec, e_val, e_vec_loc, e_val_loc )
   read(iunit)variable
   read(iunit)nk, num_bins2d
   read(iunit)e_vec
   read(iunit)e_val
   read(iunit)e_vec_loc
   read(iunit)e_val_loc
   call da_print_be_stats_v( outunit, variable, nk, num_bins2d, &
                             e_vec, e_val, e_vec_loc, e_val_loc )

   read(iunit)variable
   read(iunit)nk, num_bins2d
   read(iunit)e_vec
   read(iunit)e_val
   read(iunit)e_vec_loc
   read(iunit)e_val_loc
   call da_print_be_stats_v( outunit, variable, nk, num_bins2d, &
                             e_vec, e_val, e_vec_loc, e_val_loc )

   read(iunit)variable
   read(iunit)nk, num_bins2d
   read(iunit)e_vec
   read(iunit)e_val
   read(iunit)e_vec_loc
   read(iunit)e_val_loc
   call da_print_be_stats_v( outunit, variable, nk, num_bins2d, &
                             e_vec, e_val, e_vec_loc, e_val_loc )

!----------------------------------------------------------------------------
!   [3] Gather horizontal error power spectra.
!----------------------------------------------------------------------------

   do k = 1, nk
      read(iunit)variable
      read(iunit)max_wavenumber, kdum
      read(iunit)use_global_eofs
      if ( k == 1 ) allocate( total_power(0:max_wavenumber) )
      read(iunit)total_power(:)
      call da_print_be_stats_h( outunit, variable, k, max_wavenumber, total_power )
   end do

   do k = 1, nk
      read(iunit)variable
      read(iunit)max_wavenumber, kdum
      read(iunit)use_global_eofs
      read(iunit)total_power(:)
      call da_print_be_stats_h( outunit, variable, k, max_wavenumber, total_power )
   end do

   do k = 1, nk
      read(iunit)variable
      read(iunit)max_wavenumber, kdum
      read(iunit)use_global_eofs
      read(iunit)total_power(:)
      call da_print_be_stats_h( outunit, variable, k, max_wavenumber, total_power )
   end do

   do k = 1, nk
      read(iunit)variable
      read(iunit)max_wavenumber, kdum
      read(iunit)use_global_eofs
      read(iunit)total_power(:)
      call da_print_be_stats_h( outunit, variable, k, max_wavenumber, total_power )
   end do

   read(iunit)variable
   read(iunit)max_wavenumber, kdum
   read(iunit)use_global_eofs
   read(iunit)total_power(:)
   call da_print_be_stats_h( outunit, variable, k, max_wavenumber, total_power )

   close(iunit)

end program gen_be_diags_read
