program gen_be_diags_read

   use da_constants
   use da_gen_be

   implicit none

   character*10        :: variable                   ! Variable name
   character*3         :: be_method                  ! Be method ('NMC', or 'ENS')
   character*8         :: uh_method                  ! Uh_method (power, scale)
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
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees). !!!DALE ADD..
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m). !!!DALE ADD..
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
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
   real, allocatable   :: scale_length(:)            ! Scale length for regional application.

   namelist / gen_be_diags_nl / be_method, uh_method

   be_method = 'NMC'
   open(unit=namelist_unit, file='gen_be_diags_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_diags_nl)
   close(namelist_unit)

   filename = 'gen_be.'//trim(be_method)//'.dat'
   print '("*** Unit=",i3,3X,"filename=",a40)',iunit, filename
   open (iunit, file = filename, form='unformatted')

!----------------------------------------------------------------------------
!   [1] Gather regression coefficients.
!----------------------------------------------------------------------------

   read(iunit)ni, nj, nk
   read(iunit)num_bins, num_bins2d

   allocate( bin(1:ni,1:nj,1:nk) )
   allocate( bin2d(1:ni,1:nj) )
   allocate( regcoeff1(1:num_bins) )
   allocate( regcoeff2(1:nk,1:num_bins2d) )
   allocate( regcoeff3(1:nk,1:nk,1:num_bins2d) )

   read(iunit)regcoeff1
   read(iunit)regcoeff2
   read(iunit)regcoeff3

!  Read bin info:
   filename = 'bin.data'
   open (iunit+1, file = filename, form='unformatted')
   read(iunit+1)bin_type
   read(iunit+1)lat_min, lat_max, binwidth_lat
   read(iunit+1)hgt_min, hgt_max, binwidth_hgt
   read(iunit+1)num_bins, num_bins2d
   read(iunit+1)bin(1:ni,1:nj,1:nk)
   read(iunit+1)bin2d(1:ni,1:nj)
   close(iunit+1)

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
   if (uh_method == 'power') then
     write(6,'(/a)') '[3] Gather horizontal error power spectra.'
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

   else if (uh_method == 'scale   ') then

      write(6,'(/a)') '[3] Gather horizontal scale length:'
      allocate (scale_length(1:nk))
! psi:
      read(iunit) variable
      read(iunit) scale_length
      print '("Scale_length for variable:",a)', variable
      write(outunit,'(/a,a)') 'Scale_length for variable:', variable
      write(outunit,'((2X,6(2X,"k=",i2,1x,e14.8)))') (k,scale_length(k),k=1,nk)
! chi_u:
      read(iunit) variable
      read(iunit) scale_length
      print '("Scale_length for variable:",a)', variable
      write(outunit,'(/a,a)') 'Scale_length for variable:', variable
      write(outunit,'((2X,6(2X,"k=",i2,1x,e14.8)))') (k,scale_length(k),k=1,nk)
! t_u:
      read(iunit) variable
      read(iunit) scale_length
      print '("Scale_length for variable:",a)', variable
      write(outunit,'(/a,a)') 'Scale_length for variable:', variable
      write(outunit,'((2X,6(2X,"k=",i2,1x,e14.8)))') (k,scale_length(k),k=1,nk)
! rh:
      read(iunit) variable
      read(iunit) scale_length
      print '("Scale_length for variable:",a)', variable
      write(outunit,'(/a,a)') 'Scale_length for variable:', variable
      write(outunit,'((2X,6(2X,"k=",i2,1x,e14.8)))') (k,scale_length(k),k=1,nk)

      deallocate (scale_length)
      allocate (scale_length(1:1))
! ps_u:
      read(iunit) variable
      read(iunit) scale_length
      print '("Scale_length for variable:",a)', variable
      write(outunit,'(/a,a)') 'Scale_length for variable:', variable
      write(outunit,'((4X,"k=",i2,1x,e14.8))') (k,scale_length(k),k=1,1)

   endif

   close(iunit)

end program gen_be_diags_read
