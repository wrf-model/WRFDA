program gen_be_stage3
  
  ! ---------------------------------------------------------------------------------
  ! Purpose : Diagnose Vertical Background Error Covariance Matrix                  !
  !           Project fields on EOF modes (eigenvectors of BV)                      !
  ! ---------------------------------------------------------------------------------
    
   use da_control, only : stderr, stdout, filename_len, vertical_ip
   use da_gen_be, only : da_eof_decomposition_test, da_eof_decomposition, &
      da_transform_vptovv, da_create_bins, update_rain_bin, da_transform_vptovv_bin7,&
      read_2d_field, read_3d_field, write_2d_field, write_3d_field
   use da_tools_serial, only : da_get_unit, da_advance_cymdh

   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name
   character(len=filename_len)        :: filename                   ! Input filename.
   character*2         :: ck                         ! Level index -> character.
   character*3         :: ce                         ! Member index -> character.
   
   integer             :: ni, nj, nk, nkdum          ! Dimensions read in.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: i, j, k, k1, k2, b, member ! Loop counters.
   integer             :: bin_type                   ! Type of bin to average over.
   integer             :: num_bins                   ! Number of bins (3D fields).
   integer             :: num_bins2d                 ! Number of bins (2D fields).
   integer             :: bin_pts_glob
     
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
   real                :: inv_nij                    ! 1 / (ni*nj).
   real                :: mean_field                 ! Mean field.
   real                :: coeffa, coeffb             ! Accumulating mean coefficients.
   
   logical             :: first_time                 ! True if first file.
   logical             :: testing_eofs               ! True if testing EOF decomposition.
   logical             :: use_global_eofs            ! True if projected data uses global EOFs.
   logical             :: data_on_levels             ! True if output level data (diagnostic).
   logical             :: twod_field                 ! True if 2D field.

   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.
   integer, allocatable:: bin_pts2d(:)               ! Number of points in bin (2D fields).
   
   real, allocatable   :: latitude(:,:)              ! Latitude (degrees, from south).
   real, allocatable   :: height(:,:,:)              ! Height field.
   real, allocatable   :: field(:,:,:)               ! Input field.
   real, allocatable   :: field_out(:,:,:)          ! Field projected into EOF space.
   real, allocatable   :: vertical_wgt(:,:,:)        ! Inner product for vertical transform.
   real, allocatable   :: bv(:,:,:)                  ! Vertical BE for this time.
   real, allocatable   :: work(:,:)                  ! EOF work array.
   real, allocatable   :: e_vec_loc(:,:,:)           ! Latitudinally varying eigenvectors.
   real, allocatable   :: e_val_loc(:,:)             ! Latitudinally varying eigenvalues.
   real, allocatable   :: e_vec(:,:)                 ! Domain-averaged eigenvectors.
   real, allocatable   :: e_val(:)                   ! Domain-averaged eigenvalues.
   real, allocatable   :: evec(:,:,:)                ! Gridpoint eigenvectors.
   real, allocatable   :: eval(:,:)                  ! Gridpoint sqrt(eigenvalues).
   real, allocatable   :: evecbin(:,:,:)             ! Binned eigenvectors.
   real, allocatable   :: evalbin(:,:)               ! Binned sqrt(eigenvalues)
  
   namelist / gen_be_stage3_nl / start_date, end_date, interval, variable, &
                                 ne, bin_type, &
                                 lat_min, lat_max, binwidth_lat, &
                                 hgt_min, hgt_max, binwidth_hgt, &
                                 testing_eofs, use_global_eofs, data_on_levels

   integer :: ounit,iunit,namelist_unit

   stderr = 0
   stdout = 6
!-----------------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!-----------------------------------------------------------------------------------------------------
   call da_get_unit(ounit)
   call da_get_unit(iunit)
   call da_get_unit(namelist_unit)


   vertical_ip = 0
   twod_field = .false.

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   variable = 'psi'
   ne = 1
   bin_type = 5
   lat_min = -90.0
   lat_max = 90.0
   binwidth_lat = 10.0
   hgt_min = 0.0
   hgt_max = 20000.0
   binwidth_hgt = 1000.0
   testing_eofs = .true.
   use_global_eofs = .true.
   data_on_levels = .false.

   open(unit=namelist_unit, file='gen_be_stage3_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage3_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing vertical error statistics for dates ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

   date = start_date
   cdate = sdate
   first_time = .true.

   if ( .not. data_on_levels ) then ! Can bypass [2] and [3] if outputing on levels.
!-----------------------------------------------------------------------------------------------------
      write(6,'(2a)')' [2] Compute vertical bv for variable ', variable
!-----------------------------------------------------------------------------------------------------
      do while ( cdate <= edate )
         do member = 1, ne
            write(6,'(5a,i4)')'    Processing data for date ', date, ', variable ', trim(variable), &
                              ' and member ', member

            write(ce,'(i3.3)')member

            ! Read dimensions:
            filename = 'psi'//'/'//date(1:10)//'.'//'psi'//'.e'//ce
            open (iunit, file = filename, form='unformatted')
            read(iunit)ni, nj, nk
	    close(iunit)
	    
            if ( first_time ) then
               write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
               allocate( bin(1:ni,1:nj,1:nk) )
               allocate( bin2d(1:ni,1:nj) )

	       filename = 'bin.data'
               open (iunit, file = filename, form='unformatted')
               read(iunit)bin_type
               read(iunit)lat_min, lat_max, binwidth_lat
               read(iunit)hgt_min, hgt_max, binwidth_hgt
               read(iunit)num_bins, num_bins2d
               read(iunit)bin(1:ni,1:nj,1:nk)
               read(iunit)bin2d(1:ni,1:nj)
               close(iunit)
           
               allocate( bin_pts2d(1:num_bins2d) )
               bin_pts2d(:) = 0
	       bin_pts_glob = 0
               inv_nij = 1.0 / real(ni*nj)
               allocate( field(1:ni,1:nj,1:nk) )
               allocate( bv(1:nk,1:nk,1:num_bins2d) )
               bv(:,:,:) = 0.0
               first_time = .false.

            end if
	    
	    if (bin_type==7) then
              call update_rain_bin(ni, nj, nk, ce, date, bin2d, bin)
  	    end if   
	    
	    if ( trim(variable) == 'ps_u' .or. trim(variable) == 'ps' ) then ! 2D field
               twod_field = .true.
               call read_2d_field(variable, date, ce, iunit, ni, nj, nkdum, field(:,:,1))
	       nk = 1
            else
      	       call read_3d_field(variable, date, ce, iunit, ni, nj, nk, field)
	    end if
	
	    if (use_global_eofs) then
            ! Compute global vertical background error matrix, store it in bin 1
            ! Remove mean field:
               do k = 1, nk
                  mean_field = sum(field(1:ni,1:nj,k)) * inv_nij
                  field(1:ni,1:nj,k) = field(1:ni,1:nj,k) - mean_field
               end do
               do j = 1, nj
                  do i = 1, ni
                     b = 1
                     bin_pts_glob = bin_pts_glob + 1
                     coeffa = 1.0 / real(bin_pts_glob)
                     coeffb = real(bin_pts_glob-1) * coeffa
                     do k1 = 1, nk
                        do k2 = 1, k1
                            bv(k1,k2,b) = coeffb * bv(k1,k2,b) + &
                            coeffa * field(i,j,k1) * field(i,j,k2)
                        end do
                     end do
                   end do
               end do
           else
           ! Compute local (2D-binned) vertical background error matrix
           	do j = 1, nj
              	   do i = 1, ni
                     b = bin2d(i,j)
                     bin_pts2d(b) = bin_pts2d(b) + 1
                     coeffa = 1.0 / real(bin_pts2d(b))
                     coeffb = real(bin_pts2d(b)-1) * coeffa
                     do k1 = 1, nk
                        do k2 = 1, k1
                           bv(k1,k2,b) = coeffb * bv(k1,k2,b) + &
                            coeffa * field(i,j,k1) * field(i,j,k2)
                        end do
                     end do
                  end do
               end do
          end if
         end do  ! End loop over ensemble members.

         ! Calculate next date:
         call da_advance_cymdh( date, interval, new_date )
         date = new_date
         read(date(1:10), fmt='(i10)')cdate
      end do     ! End loop over times.

      !  Fill in upper-right part of BE matrix by symmetry:
      do b = 1, num_bins2d
         do k1 = 1, nk
            do k2 = k1+1, nk ! Symmetry.
               bv(k1,k2,b) = bv(k2,k1,b)
            end do
         end do
      end do
      
  ! Store vertical BE matrix
  filename = 'stage3_vert_autocovariance_'//trim(variable)//'.dat'
  write(unit=6,fmt='(2a)') " Write vertical BE matrix into ", trim(filename)
  open (ounit, file = filename, form='unformatted')
  write(ounit)num_bins2d,nk
  write(ounit)bv
  close(ounit)
!-----------------------------------------------------------------------------------------------------
  write(6,'(2a)')' [3] Calculate eigenvectors and eigenvalues for variable ', variable
!-----------------------------------------------------------------------------------------------------
      allocate( work(1:nk,1:nk) )
      allocate( e_vec_loc(1:nk,1:nk,1:num_bins2d) )
      allocate( e_val_loc(1:nk,1:num_bins2d) )
      allocate( e_vec(1:nk,1:nk) )
      allocate( e_val(1:nk) )

	if (use_global_eofs) then
        !  Use domain-averaged BV stored in bin 1, compute EOF
        write(unit=6,fmt='(a)') " Use Domain-averaged BE decomposition  "
        work(1:nk,1:nk) = bv(1:nk,1:nk,1)
        call da_eof_decomposition( nk, work, e_vec, e_val )
        if ( testing_eofs ) then
           call da_eof_decomposition_test( nk, work, e_vec, e_val )
        end if
        do b = 1, num_bins2d
           e_vec_loc(1:nk,1:nk,b) = e_vec(1:nk,1:nk)
           e_val_loc(1:nk,b) = e_val(1:nk)
        end do
     else
        ! Use local (2D-binned) BV
        do b = 1, num_bins2d
           write(6,'(2(a,i6))')' Calculate eigenvectors and eigenvalues for bin ', b, &
                ' of ', num_bins2d
           work(1:nk,1:nk) = bv(1:nk,1:nk,b)
           call da_eof_decomposition(nk, work, e_vec, e_val ) 
           if ( testing_eofs ) then
              call da_eof_decomposition_test( nk, work, e_vec, e_val )
           end if
           e_vec_loc(1:nk,1:nk,b) = e_vec(1:nk,1:nk)
           e_val_loc(1:nk,b) = e_val(1:nk)
        end do
     end if

     !  Output eigenvectors, eigenvalues for use in WRF_Var:
     filename = 'gen_be_stage3.'//trim(variable)//'.dat'
     write(unit=6,fmt='(2a)') " Write eigenvec and eigenval into ", trim(filename)
     open (ounit, file = filename, form='unformatted')
     write(ounit)variable
     write(ounit)nk, num_bins2d
     write(ounit)e_vec
     write(ounit)e_val
     write(ounit)e_vec_loc
     write(ounit)e_val_loc
     close(ounit)
    
     !  Map binned eigenvectors to x, y grid, and take sqrt(this is used in WRF-Var):
     if (bin_type==7) then
        allocate( evecbin(1:num_bins2d,1:nk,1:nk) )
        allocate( evalbin(1:num_bins2d,1:nk) )
        evecbin = 0.0
        evalbin = 0.0
     else
        allocate( evec(1:nj,1:nk,1:nk) )
        allocate( eval(1:nj,1:nk) )
        ! use local decomposition (which is global if use_global_eof is .true.)
        do j = 1, nj
           do i = 1, ni
              b = bin2d(i,j)
              evec(j,1:nk,1:nk) = e_vec_loc(1:nk,1:nk,b)
              eval(j,1:nk) = sqrt(e_val_loc(1:nk,b))
           end do
        end do
     end if
  else
!-----------------------------------------------------------------------------------------------------
  write(6,'(2a)')' [3] Diagnose vertical correlation lengthscale for variable ', variable
!-----------------------------------------------------------------------------------------------------
      if ( .not. twod_field ) then
      	! compute it
      end if
  end if ! Bypass if data on levels
  !-----------------------------------------------------------------------------------------------------
   write(6,'(2a)')' [4] Transform perturbations (or not), and output.'
  !-----------------------------------------------------------------------------------------------------
   date = start_date
   cdate = sdate
   first_time = .true.

   if ( .not. twod_field ) then
      do while ( cdate <= edate )
         do member = 1, ne
            write(6,'(5a,i4)')'    Date = ', date, ', variable ', trim(variable), &
                              ' and member ', member

            write(ce,'(i3.3)')member
	    call read_3d_field(variable, date, ce, iunit, ni, nj, nk, field)	    
	    
	    if (bin_type==7) then
              call update_rain_bin(ni, nj, nk, ce, date, bin2d, bin)
  	    end if   
	    
            if ( first_time ) then
               allocate( field_out(1:ni,1:nj,1:nk) )
               allocate( vertical_wgt(1:ni,1:nj,1:nk) )
               vertical_wgt(1:ni,1:nj,1:nk) = 1.0 ! vertical_ip = 0 hardwired.
               first_time = .false.
            end if

            if ( data_on_levels ) then
               ! Keep data on vertical levels:
               field_out(:,:,:) = field(:,:,:)
            else
	    ! Redefine evec/eval in rainy cases
              if (bin_type==7) then
                 do b=1,num_bins2d
                    evecbin(b,1:nk,1:nk)=e_vec_loc(1:nk,1:nk,b)
                    evalbin(b,1:nk) = sqrt(e_val_loc(1:nk,b))
                 end do
		 ! use different routine because this is 3D
                 call da_transform_vptovv_bin7(evecbin,evalbin,vertical_wgt,field,field_out,&
                      num_bins2d,ni,nj,nk,bin2d)
              else
               ! Project fields onto vertical modes:
        	call da_transform_vptovv( evec, eval, vertical_wgt, &
                   field, field_out, nk, &
                   1, nk, & ! WRF ids, ide etc.
                   1, ni, 1, nj, 1, nk, & ! WRF ims, ime etc.
                   1, ni, 1, nj, 1, nk )  ! WRF its, ite etc.
   	      end if
            end if

            ! Output fields (split into 2D files to allow parallel horizontal treatment):
            do k = 1, nk
               write(ck,'(i2)')k
               if ( k < 10 ) ck = '0'//ck(2:2)
               ! Assumes variable directory has been created by script:
               filename = trim(variable)//'/'//date(1:10)//'.'//trim(variable)
               filename = trim(filename)//'.e'//ce//'.'//ck
               open (ounit, file = filename, form='unformatted')
               write(ounit)ni, nj, k
               write(ounit)field_out(1:ni,1:nj,k)
               close(ounit)
            end do
         end do ! End loop over members.

         ! Calculate next date:
         call da_advance_cymdh( date, interval, new_date )
         date = new_date
         read(date(1:10), fmt='(i10)')cdate
      end do
   end if
   write(6,'(a)') 'End of program gen_be_stage3'

end program gen_be_stage3
