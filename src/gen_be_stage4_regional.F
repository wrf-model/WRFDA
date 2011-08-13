program gen_be_stage4_regional

!----------------------------------------------------------------------
! Purpose: Compute distance-binned horizontal correlation and
!          estimate the lenghtscale base on a correlation function
!
! History:
! Date     Author & Comment
! -------------------------
! 07/04/2011  Syed RH Rizvi , NCAR/NESL/MMM/DAS 
!             Implemented Variance method (sl_method=2) for computing lengthscale
! dd/mm/yy Dale Barker
!          Initial version
! 01/04/11 J.-F. Caron (Met Office)
!          Adaptation for the UK MetO model and variational system
! -------- End History
!
!----------------------------------------------------------------------

  use da_control, only : stderr, stdout, filename_len, num_fft_factors, pi
  use da_tools_serial, only : da_get_unit,da_advance_cymdh, da_free_unit,&
  da_find_fft_factors, da_find_fft_trig_funcs
  use module_ffts, only : fft551, fft661
  use da_fft_initialize
  use da_gen_be, only : da_get_field, update_rain_bin
  use da_change_wind_variables, only : da_laplacian_a
  use da_horizontal_covariances

   implicit none

   real , parameter    :: rsmall = 10.0e-30
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
 
   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable, variable_tmp, rainc_variable     ! Variable name
   character(len=filename_len)       :: run_dir      ! Run directory.
   character(len=filename_len)       :: filename     ! Input filename.
   character(len=filename_len)       :: output_file  ! Input filename.
   character*2         :: ck                         ! Level index -> character.
   character*2         :: ci                         ! Bin index -> character.
   character*3         :: ce                         ! Member index -> character.
   integer             :: ni, nj, nk                 ! Dimensions read in.
   integer             :: i,j,b,k,kdum,kk,nkdum,nsig ! Index.
   integer             :: stride                     ! Calculate correlation with every stride point.
   integer             :: nbins                      ! Number of latitude bins
   integer             :: ibin                       ! Which latitude bin to process (1:nbins)
   integer             :: nn                         ! Dimension of radii bins.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: member                     ! Loop counters.
   integer             :: jstart, jend               ! Starting and ending j indices
   real                :: count                      ! Counter for times/members.
   integer             :: d2                         ! Distance bin counter
   integer, allocatable:: nr(:)                      ! Number of points in each bin.
   real, allocatable   :: field_2d(:,:)              ! Input 2D field.
   real, allocatable   :: field_3d(:,:,:)            ! Input 3D field.
   real, allocatable   :: cov(:)                     ! Covariance as a function of distance.
   real, allocatable   :: cor(:)                     ! Correlation as a function of distance.
   real, allocatable   :: mean(:,:)                  ! Mean values in each distance bin.
   real, allocatable   :: var(:,:)                   ! Variance in each distance bin.
   real                :: coeffa,coeffb              ! Accumulating mean coefficients.
   integer             :: ifax1(1:num_fft_factors)   ! FFT factors.
   integer             :: ifax2(1:num_fft_factors)   ! FFT factors.
   integer             :: n1, n2                     ! Padded dimensions (n=dim-1+pad).
   integer             :: poisson_method             ! 1=Spectral, 2=SOR.
   integer             :: fft_method                 ! For poisson_method=1: 1=FCT, 2=FST.
   real, allocatable   :: trigs1(:)                  ! FFT trig functions.
   real, allocatable   :: trigs2(:)                  ! FFT trig functions.
   real, allocatable   :: fft_coeffs(:,:)            ! FFT coefficients.
   real, allocatable   :: mapfac_m(:,:)              ! Map factor
   real, allocatable   :: xlat(:,:), xlon(:,:), znu(:)
   real, allocatable   :: filter(:,:)                ! Spectral filter
   real, allocatable   :: lap_field_2d(:,:)          ! 2D Laplacian of field
   real, allocatable   :: var_lap_field(:,:,:)       ! 3D Laplacian field
   real, allocatable   :: var_field(:,:,:)           ! Variance
   real, allocatable   :: len_field(:,:,:)           ! 3D Lengthscale
   real, allocatable   :: med_len_field(:)           ! Median field
   real, allocatable   :: oned_len(:)                ! Oned lengthscale array
   
   real, allocatable   :: var_bin_field(:,:)          ! perturbation variance
   real, allocatable   :: var_bin_lap_field(:,:)      ! lap perturbation variance
   real, allocatable   :: len_bin_field(:,:)          ! perturbation lengthscale 
   
   integer, allocatable :: rain_class(:,:)           ! 2D rain class of perturbation
   real, allocatable    :: mask(:,:)                 ! 2D rain class of perturbation
   integer, allocatable :: bin(:,:,:)                ! Bin assigned to each 3D point.
   integer, allocatable :: bin2d(:,:)                ! Bin assigned to each 2D point.
   integer, allocatable :: bin_pts(:)                ! Number of points in bin (3D fields).
   integer, allocatable :: bin_pts2d(:)              ! Number of points in bin (2D fields).
   
   integer             :: sl_method                  ! =1 (curve fitting) =2 (Laplacian method) method
   integer             :: bin_type
   integer             :: num_bins                   ! Number of bins (3D fields).
   integer             :: num_bins2d                 ! Number of bins (3D fields).
   
   real                :: ds                         ! Grid distance in Km.
   real                :: mean1                      ! dummy tmp
   character (len=4)   :: horizfunct                 ! Horizontal correlation function = gaus or soar.
   character (len=6)   :: horizvar                   ! Horizontal variable function = covar or correl.
   real(kind=8)        :: ml, sl                     ! Gradient, scalelength.

!----------------------------------------------------------------------------

   namelist / gen_be_stage4_regional_nl / start_date, end_date, interval, variable, &
                                          ne, k, nbins, ibin, stride, run_dir,      &
                                          horizvar, horizfunct, ds, sl_method
   integer :: ounit,iunit,namelist_unit

   stderr = 0
   stdout = 6

   call da_get_unit(ounit)
   call da_get_unit(iunit)
   call da_get_unit(namelist_unit)

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!---------------------------------------------------------------------------------------------

   start_date = '2007081600'
   end_date   = '2007091512'
   interval = 24
   variable = 'psi'
   poisson_method = 1
   fft_method = 1
   ne = 1
   k = 1
   stride = 1
   nbins = 1
   ibin = 1
   sl_method = 1
   ds = 3.0
   horizvar = 'covar'
   horizfunct = 'gaus'

   run_dir = '/ptmp/rizvi/data/conus_3Km_cloud_case'
   open(unit=namelist_unit, file='gen_be_stage4_regional_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage4_regional_nl)
   close(namelist_unit)

   if (trim(horizvar).eq.'covar') then
      write(UNIT=6,FMT='(a)') " horizvar = Covariances"
   else
      write(UNIT=6,FMT='(a)') " horizvar = Correlation"
   endif

   if (horizfunct.eq.'gaus') then
      write(UNIT=6,FMT='(a)') " horizfunct = Gaussian"
   else
      write(UNIT=6,FMT='(a)') " horizfunct = SOAR"
   endif

   write(UNIT=6,FMT='(4a)') &
      ' Computing error correlation scales for dates ',&
      start_date, ' to ', end_date
   write(UNIT=6,FMT='(a,i3,a,i3)') &
      '                                    for bin ', &
      ibin, ' of ', nbins
   write(UNIT=6,FMT='(a,i8,a)') &
      ' Interval between dates = ', interval, 'hours.'
   write(UNIT=6,FMT='(a,i8)') &
      ' Number of ensemble members at each time = ', ne
   write(UNIT=6,FMT='(a,i8)') &
      ' Stride over which to jump points in correlation calculation = ', stride
   write(6,'(2a)') ' rundir is ',trim(run_dir)

   ds=ds*1000.0 ! km-> m
   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   date = start_date
   cdate = sdate  
   
!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [2] Input fields and calculate correlation as a function of distance between points.'
!---------------------------------------------------------------------------------------------

   count = 1.0

   do while ( cdate <= edate )
      do member = 1, ne

         write(UNIT=6,FMT='(5a,i4)')'    Date = ', date, ', variable ', trim(variable), &
                           ' and member ', member
         write(UNIT=ce,FMT='(i3.3)')member
	 
            !        Read Full-fields:
            variable_tmp = 'fullflds'
            filename     = trim(run_dir)//'/'//trim(variable_tmp)//'/'//trim(variable_tmp)
            filename     = trim(filename)//'.'//date(1:10)//'.e'//ce
            open (iunit, file = filename, form='unformatted')
            read(iunit)date,ni, nj, nk
	    close(iunit)

	  if ( count == 1.0 ) then
	    write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
            allocate( bin(1:ni,1:nj,1:nk) )
            allocate( bin2d(1:ni,1:nj) )
!           Read bin info:
            filename = trim(run_dir)//'/bin.data'
            open (iunit, file = filename, form='unformatted')
            read (iunit) bin_type
            read (iunit) lat_min, lat_max, binwidth_lat
            read (iunit) hgt_min, hgt_max, binwidth_hgt
            read (iunit) num_bins, num_bins2d
            read (iunit) bin(1:ni,1:nj,1:nk)
            read (iunit) bin2d(1:ni,1:nj)
            close(iunit)
	    
            if( sl_method == 1 ) then    ! sl computation via guassian curve fitting 
	       kdum = nk
               jstart = floor(real(ibin-1)*real(nj)/real(nbins))+1
               jend   = floor(real(ibin)  *real(nj)/real(nbins))
               write(UNIT=6,FMT='(a,i4,a,i4)') 'Computing length scale over j range ', jstart, ' to ', jend
               nn = ni * ni + nj * nj  ! Maximum radius across grid (squared).
               allocate(field_2d(1:ni,1:nj))
               allocate(nr(0:nn))
               allocate(cov(0:nn))
               cov(0:nn) = 0.0
               allocate(var(0:nn,1:2))
               var(0:nn,1:2) = 0.0
               allocate(mean(0:nn,1:2))
               mean(0:nn,1:2) = 0.0            ! mean = 0 harcoded here
	       
	       if (bin_type==7) then 
            	  ! compute over whole domain as bin is not function of latitude but rain class
            	  jstart = 1
            	  jend   = nj
                  allocate( rain_class(1:ni,1:nj) )
                  rain_class=0
                  allocate( mask(1:ni,1:nj) )
                  mask=0		  
	       else
                   call get_grid_info( ni, nj, nn, stride, nr, jstart, jend )
	       end if
	       
            else if (sl_method ==2 ) then  ! sl computation via Laplacian solver method 

               ! fields 2d
               allocate( lap_field_2d(1:ni,1:nj) )
               ! fields 3d
               allocate( field_3d(1:ni,1:nj,1:nk) )

	       if (bin_type==7) then
                  allocate( var_bin_field(1:num_bins2d,1:nk) )
                  allocate( var_bin_lap_field(1:num_bins2d,1:nk) )
                  ! lengthscale
                  allocate( len_bin_field(1:num_bins2d,1:nk) )
                  var_bin_field = 0.0
                  var_bin_lap_field = 0.0
                  len_bin_field = 0.0
		  allocate( bin_pts2d(1:num_bins2d) )
		  bin_pts2d(:)=0
	       else	       
                  ! variances
                  allocate( var_field(1:ni,1:nj,1:nk) )
                  allocate( var_lap_field(1:ni,1:nj,1:nk) )
                  ! lengthscale	       
                  allocate( len_field(1:ni,1:nj,1:nk) )
		  var_field = 0.0
                  var_lap_field = 0.0
	          len_field = 0.0
	       end if
               ! median lengthscale
               allocate( med_len_field(1:nk) )
               ! oned for median resarch
               allocate( oned_len(1:(ni*nj)))
               allocate( filter(1:ni,1:nj) )
	       
 	       ! Map projection factor
 	       allocate(znu(1:nk))
	       allocate( xlat(1:ni,1:nj) )
	       allocate( xlon(1:ni,1:nj) )
               allocate( mapfac_m(1:ni,1:nj) )

	       filename = trim(run_dir)//'/fixed_flds'
	       write(6,'(2a)') 'Reading mapfactors in file ',trim(filename)
   	       open (iunit, file = trim(filename), form='unformatted')
   	       read(iunit)ni,nj,nk
   	       read(iunit)znu 
   	       read(iunit)xlat
   	       read(iunit)xlon
   	       read(iunit)mapfac_m
   	       close(iunit)

               !  Initialize FFT coefficients:
               call da_fft_initialize1( ni, nj, n1, n2, ifax1, ifax2 )
               allocate( trigs1(1:3*n1) )
               allocate( trigs2(1:3*n2) )
               allocate( fft_coeffs(1:(n1+1),1:(n2+1)) )
               call da_fft_initialize2( n1, n2, ds, trigs1, trigs2, fft_coeffs )
           else
               write(6,*)' Improper sl_method ',sl_method
	       stop
           end if
        end if   ! end if for count==1.0

!        Calculate spatial correlation:
         if( sl_method == 1 ) then    ! sl computation via guassian curve fitting 
           write(UNIT=ck,FMT='(i2.2)') k

           !        Read EOF unbalanced field:
           filename = trim(run_dir)//'/'//trim(variable)//'/'//date(1:10)
           filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.'//ck
           open(iunit, file = trim(filename), form='unformatted')
           read(UNIT=iunit)ni, nj, kdum 
           read(UNIT=iunit)field_2d
           close(UNIT=iunit)
	   
	   if (bin_type==7) then 
            	! read rain class info
            	rain_class=0
            	mask=0.0
            	!        Read rain_class:
            	rainc_variable = 'raincl'
            	filename = trim(run_dir)//trim(rainc_variable)//'/'//date(1:10)
            	filename = trim(filename)//'.'//trim(rainc_variable)//'.e'//ce//'.01'
            	open (iunit, file = trim(filename), form='unformatted')
            	read(iunit)ni, nj, nkdum
            	read(iunit)rain_class
            	close(iunit)
            	! define mask fields following current bin
            	where (rain_class==(ibin-1))
               		mask=1.0
            	elsewhere
               		mask=0.0
            	end where
            	call get_grid_info_masked( ni, nj, nn, stride, nr, jstart, jend, mask )
            	! Calculate spatial correlation:
            	call get_covariance_masked( ni, nj, nn, stride, count, nr, jstart,&
			   jend, field_2d, mask, cov)
           else
           	call get_covariance( ni, nj, nn, stride, count, nr, jstart, &
                        	jend, field_2d, mean, var, cov )
	   end if

         else     ! sl computation Laplacian method              
	 
	   if (bin_type==7) then
              call update_rain_bin(ni, nj, nk, ce, date, bin2d, bin, run_dir)
	   end if  
	 
           coeffa = 1.0 / real(count)
           coeffb = real(count-1) * coeffa
           nsig = nk
           if ( trim(variable) == 'ps_u' .or. trim(variable) == 'ps' ) nsig = 1
           do kk = 1, nsig  
              write(UNIT=ck,FMT='(i2.2)') kk

              !        Read EOF unbalanced field:
              filename = trim(run_dir)//'/'//trim(variable)//'/'//date(1:10)
              filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.'//ck
              open(iunit, file = trim(filename), form='unformatted')
              read(iunit)ni, nj, nkdum
              read(iunit)field_3d(:,:,kk)
              close(iunit)

              ! Computes Laplacian fields
              call da_laplacian_a(ni, nj, n1, n2, fft_method, ifax1, ifax2, &
                   trigs1, trigs2, fft_coeffs, mapfac_m, field_3d(:,:,kk), lap_field_2d )
		   
	      if (bin_type==7) then
        	 do j = 1, nj
                    do i = 1, ni
                       b = bin2d(i,j)
                       bin_pts2d(b) = bin_pts2d(b) + 1
                       coeffa = 1.0 / real(bin_pts2d(b))
                       coeffb = real(bin_pts2d(b)-1) * coeffa
                       var_bin_field(b,kk) = coeffb * var_bin_field(b,kk) + coeffa * field_3d(i,j,kk)**2
                       var_bin_lap_field(b,kk) = coeffb * var_bin_lap_field(b,kk) + coeffa * lap_field_2d(i,j)**2
                    end do
        	 end do
              else
                ! Accumulates variances for fields
                var_field(:,:,kk) = coeffb * var_field(:,:,kk) + coeffa * field_3d(:,:,kk)**2  
                ! Accumulates variances for derived fields
                var_lap_field(:,:,kk) = coeffb * var_lap_field(:,:,kk) + coeffa * lap_field_2d(:,:)**2
	      end if
           end do
         end if  !  end if sl_method

         count = count + 1.0

      end do ! End loop over members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do

!---------------------------------------------------------------------------------------------
   write(UNIT=6,FMT='(a)')' [3] Compute fit of correlation to a straight line.'
!---------------------------------------------------------------------------------------------
  if ( sl_method == 1) then
!  Transform covariances into correlations
   allocate(cor(0:nn))
   cor(0:nn) = 0.0
   do d2 = 0, nn
      if ( nr(d2) > 0 ) then
         if (horizvar .eq. 'covar') then
            cor(d2)=cov(d2)/cov(0)                              ! pseudo-correlation
         else
            cor(d2)=cov(d2)/(sqrt(var(d2,1))*sqrt(var(d2,2)))   ! correlation
         endif
      end if
   end do

   filename = 'sl_print.'//trim(variable)//'.'//ck
   open( unit=ounit, file=trim(filename), form='formatted', &
         action='write', access='sequential', status='replace')

   call gauss_scale_length( sl, variable, ck, nn, nr, cor )

   if (horizfunct .eq. "soar" ) then
      ! sl from gauss_scale_length is used as first guess for soar fitting
      call soar_scale_length( sl, variable, ck, nn, nr, cor )
   endif

   ! write scale length
   if (sl/=0.0) then  
      ml = 1.0 / sl
   else
      ml = 0.0
   end if
   write(UNIT=ounit,FMT='(a,2e20.8)') ck, ml, sl

   close(unit=ounit, status='keep')

   deallocate (field_2d) 
   deallocate (cov)
   deallocate (cor)
   deallocate (mean)
   deallocate (var) 
   
  else

  !---------------------------------------------------------------------------------------------
  write(6,'(a)')' [3] Write raw variance/lengthscale files'
  !---------------------------------------------------------------------------------------------
  
  !        Calculate local lengthscales - Wu formula
  if (bin_type==7) then
     var_bin_lap_field = max(rsmall,var_bin_lap_field)
     len_bin_field=(8.0*var_bin_field/var_bin_lap_field)**0.25
     
     do b=1, num_bins2d
        write(ci,fmt='(i1)') b
        filename = 'sl_fvar_print.'//trim(variable)//'.bin'//ci
        open( unit=ounit, file=trim(filename), form='formatted', &
             action='write', access='sequential', status='replace')
        write(6,fmt='(2a)') " [3.1] Write lengthscale into ",trim(filename)
        do kk = 1,nsig
           write(unit=6,fmt='(a,i4,3e15.5)') " Level ",kk,var_bin_field(b,kk), &
                                             var_bin_lap_field(b,kk),len_bin_field(b,kk)
           write(ounit,'(i2.2,2e20.5)') kk,ds/len_bin_field(b,kk),len_bin_field(b,kk)/ds
        end do
        close(ounit)
     end do
     
  else
     var_lap_field = max(rsmall,var_lap_field)
     len_field=(8.0*var_field/var_lap_field)**0.25
 
     !        Writes variances
     write(6,'(2a)') variable,' variance '
     do kk=1,nsig
        write(6,'(i2.2,e15.5)') kk,sum(var_field(:,:,kk))/real(ni*nj)
     end do
     write(6,'(3a)') 'lap ',variable,' variance '
     do kk=1,nsig
	write(6,'(i2.2,e15.5)') kk,sum(var_lap_field(:,:,kk))/real(ni*nj)
     end do
     
     ! Because ps is not scaled by eigenvalue in stage3
     if ( trim(variable) == 'ps_u' .or. trim(variable) == 'ps' ) then
	mean1 = sum(var_field(:,:,1))/real(ni*nj)
	write(6,*)' pressure mean = ',mean1
	var_field(:,:,1)=var_field(:,:,1)/mean1
     end if

     !     Output local variances and lengthscales
     output_file = 'variances_'//trim(variable)//'_raw.dat'
     write(6,'(2a)') 'diagnostic: write raw variances in ',trim(output_file)
     open (ounit, file = output_file, form='unformatted')
     write(ounit)ni,nj,nsig
     do kk=1,nsig
	write(ounit)var_field(:,:,kk)
     end do
     close(ounit)

     output_file = 'variances_lap_'//trim(variable)//'_raw.dat'
     write(6,'(2a)') 'diagnostic: write raw variances in ',trim(output_file)
     open (ounit, file = output_file, form='unformatted')
     write(ounit)ni,nj,nsig
     do kk=1,nsig
	write(ounit)var_lap_field(:,:,kk)
     end do
     close(ounit)

     output_file = 'lengthscales_'//trim(variable)//'_raw.dat'
     write(6,'(2a)') 'diagnostic: write raw lengthscales in ',trim(output_file)
     open (ounit, file = output_file, form='unformatted')
     write(ounit)ni,nj,nsig
     do kk=1,nsig
	write(ounit)len_field(:,:,kk)
     end do
     close(ounit)

     !        Writes output
     write(6,'(2a)') trim(variable),' median lengthscale '
     filename = 'sl_print.'//trim(variable)
     open( unit=ounit, file=trim(filename), form='formatted', &
	  action='write', access='sequential', status='replace')
     do kk=1,nsig
	oned_len=reshape(len_field(:,:,kk),(/ni*nj/))
	call median(oned_len, ni*nj, med_len_field(kk))
	write(6,'(i2.2,e15.5)') kk,med_len_field(kk)
	write(ounit,'(i2.2,2e20.5)') kk,ds/med_len_field(kk),med_len_field(kk)/ds
     end do
     close(unit=ounit, status='keep')
   end if
end if   !  end if for sl_method
write(6,'(a)') 'End of program gen_be_stage4_regional'

end program gen_be_stage4_regional
