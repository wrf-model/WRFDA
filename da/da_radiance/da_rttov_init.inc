#ifdef RTTOV
subroutine da_rttov_init(iv,ob)
!------------------------------------------------------------------------------
!  PURPOSE: interface to the initialization subroutine of RTTOV8_5.
!
!  METHOD:  1. read RTTOV coefs files; 2. allocate radiance structure
!
!  HISTORY: 07/28/2005 - Creation                               Zhiquan Liu
!           03/22/2006   add error tuning factor read in        Zhiquan Liu
!
!------------------------------------------------------------------------------

 implicit none 

#include "rttov_setup.interface"
#include "rttov_readscattcoeffs.interface"
 
 type (ob_type), intent (inout) :: iv
 type (y_type) , intent (inout) :: ob

!  local arguments
!------------------- 
 integer   :: n, k,j,bunit, iscan,ichan,npos,unit_factor_rad
 real      :: ave

!
!  input parameters of RTTOV_SETUP
!----------------------------------
  integer :: err_unit        ! Logical error unit (<0 for default)
  integer :: verbosity_level ! (<0 for default)
  integer :: nsensor
  integer, allocatable :: sensor(:,:) ! instrument id
!  integer, allocatable :: channels(:,:)   ! channel list per instrument

!  output parameters of RTTOV_SETUP
!-------------------------------------
  integer, allocatable :: errorstatus(:)  ! return code
  integer              :: coef_errorstatus, i_amsua, i_amsub
!  type( rttov_coef ), allocatable :: coefs(:)         ! coefficients
  integer     :: error
  integer , pointer   ::  nscan(:), nchanl(:)

! local variables
!----------------
 integer             :: nprofiles, nfrequencies, nchannels, nbtout
 integer , pointer   :: lprofiles(:)
 real    , pointer   :: surfem(:), emissivity(:)
 integer , pointer   :: channels (:), polarisations(:,:)
 integer             :: mxchn
 integer , pointer   :: coefs_channels (:,:)
 
 integer             :: idum, wmo_sensor_id, sensor_type, iost, iunit

! local variables for tuning error factor
 character(len=20)   ::  rttovid_string
 integer             ::  num_tot
 real                ::  joa, jo, trace, factor 
   character(len=80)   :: filename

!--------------------------------------------------------------
!  1.0 setup RTTOV instrument triplets from namelist parameter
!--------------------------------------------------------------

    mxchn           =  300
    err_unit        =  stderr
    verbosity_level =  rtminit_print
    nsensor         =  rtminit_nsensor

  allocate (errorstatus(nsensor))
  allocate (coefs(nsensor))
  allocate (sensor(3,nsensor))
  allocate (coefs_channels(mxchn,nsensor))
  allocate (nscan(nsensor))
  allocate (nchanl(nsensor))

  sensor (1,1:nsensor)  = rtminit_platform (1:nsensor) 
  sensor (2,1:nsensor)  = rtminit_satid    (1:nsensor)
  sensor (3,1:nsensor)  = rtminit_sensor   (1:nsensor)
  coefs_channels(:,:)   = 0

  print *, ' err_unit        = ', err_unit
  print *, ' verbosity_level = ', verbosity_level
  print *, ' nsensor         = ', nsensor
  print *, ' sensor (1,1:nsensor) = ', sensor (1,1:nsensor)
  print *, ' sensor (2,1:nsensor) = ', sensor (2,1:nsensor)
  print *, ' sensor (3,1:nsensor) = ', sensor (3,1:nsensor)

!----------------------------------------------------------------
!  2.0 set up some common varibles for innovation/observation structure
!----------------------------------------------------------------
  iv % num_inst = nsensor
  ob % num_inst = nsensor

  allocate (iv%instid(1:nsensor))
  allocate (ob%instid(1:nsensor))
  allocate (satinfo(1:nsensor))

  iv%instid(1:nsensor)%num_rad = 0
  ob%instid(1:nsensor)%num_rad = 0

  loop_sensor: do n = 1, nsensor

   iv%instid(n)%platform_id  = rtminit_platform(n)
   iv%instid(n)%satellite_id = rtminit_satid(n)
   iv%instid(n)%sensor_id    = rtminit_sensor(n)
 if ( rtminit_satid(n) < 10 ) then
   write(iv%instid(n)%rttovid_string, '(a,i1,a)')  &
             trim( platform_name(rtminit_platform(n)) )//'-',  &
             rtminit_satid(n),     &
             '-'//trim( inst_name(rtminit_sensor(n)) )
 else
   write(iv%instid(n)%rttovid_string, '(a,i2.2,a)')  &
             trim( platform_name(rtminit_platform(n)) )//'-',  &
             rtminit_satid(n),     &
             '-'//trim( inst_name(rtminit_sensor(n)) )
 end if

   if ( trim( inst_name(rtminit_sensor(n))) == 'msu' ) then
      nchanl(n)  = 4
      nscan(n)   = 11
   else if ( trim( inst_name(rtminit_sensor(n))) == 'hirs' ) then
      nchanl(n)  = 19
      nscan(n)   = 56
   else if ( trim( inst_name(rtminit_sensor(n))) == 'amsua' ) then
      nchanl(n)  = 15
      nscan(n)   = 30
   else if ( trim( inst_name(rtminit_sensor(n))) == 'amsub' ) then
      nchanl(n)  = 5
      nscan(n)   = 90
   else if ( trim( inst_name(rtminit_sensor(n))) == 'airs' ) then
      nchanl(n)  = 281
      nscan(n)   = 90
   else if ( trim( inst_name(rtminit_sensor(n))) == 'hsb' ) then
      nchanl(n)  = 4
      nscan(n)   = 90
   else
      write(6,*) ' unrecorgnized instrument '
      stop ' in da_rttov_init '
   end if

      iv%instid(n)%nchan  = nchanl(n)
      ob%instid(n)%nchan  = nchanl(n)

      allocate ( iv%instid(n)%ichan(1:nchanl(n)), stat = error )
      if( error /= 0 ) then
        Write( *, '( "mem allocation error to iv%instid(n)%ichan")' )
        Stop ' in da_rttov_init '
      end if

      allocate ( ob%instid(n)%ichan(1:nchanl(n)), stat = error )
      if( error /= 0 ) then
        Write( *, '( "mem allocation error to ob%instid(n)%ichan")' )
        Stop ' in da_rttov_init '
      end if

!     if (trim( inst_name(rtminit_sensor(n))) == 'airs') then

      call da_get_unit(iunit)
      filename='wrfvar_run/'//adjustl(trim(iv%instid(n)%rttovid_string))//'.info'
      open(unit=iunit,file=filename, &
         form='formatted',iostat = iost, status='old')

      if (iost /= 0) then
         message(1)="Cannot open radiance info file "//adjustl(filename)
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

  allocate ( satinfo(n) % ichan(nchanl(n)) )
  allocate ( satinfo(n) % iuse (nchanl(n)) )
  allocate ( satinfo(n) % error(nchanl(n)) )
  allocate ( satinfo(n) % polar(nchanl(n)) )

  read(iunit,*)
  do j = 1, nchanl(n)
    read(iunit,'(1x,5i5,2e18.10)')    &
                     wmo_sensor_id, &
               satinfo(n)%ichan(j), &
                       sensor_type, &
               satinfo(n)%iuse(j) , &
                              idum, &
               satinfo(n)%error(j), &
               satinfo(n)%polar(j)
    iv%instid(n)%ichan(j) = satinfo(n)%ichan(j)
    ob%instid(n)%ichan(j) = satinfo(n)%ichan(j)
! only load coefs of selected channels for AIRS
    coefs_channels(j,n)   = satinfo(n)%ichan(j) 
  end do

   close(iunit)
   call da_free_unit(iunit)

!     else
!      do j = 1, nchanl(n)
!        iv%instid(n)%ichan(j) = j
!        ob%instid(n)%ichan(j) = j
!        coefs_channels(j,n)   = j
!      end do
!     end if

  end do loop_sensor

!-----------------------------------------------------------
! 3.0 call rttov_setup for reading clear sky coefficients
!-----------------------------------------------------------

 call rttov_setup (&
      & errorstatus,      &! out
      & err_unit,         &! in
      & verbosity_level,  &! in
      & nsensor,          &! in
      & coefs,            &! out
      & sensor ) !,           &! in
!      & coefs_channels )   ! in Optional not work with amsua+b

  if( any(errorstatus(:) /= errorstatus_success )) then
     write ( 6, * ) 'rttov_setup fatal error'
     stop ' in da_rttov_init '
  endif

!-------------------------------------------------------------
!  4.0 read coef file for cloud/rain absorption/scattering
!-------------------------------------------------------------
 if (rttov_scatt) then
    i_amsua = 0
    i_amsub = 0
   do n=1,nsensor
    if ( trim(inst_name(rtminit_sensor(n))) == 'amsua' ) &
          i_amsua = n
    if ( trim(inst_name(rtminit_sensor(n))) == 'amsub' ) &
          i_amsub = n 
   end do

   if ( i_amsua /= 0 .and. i_amsub == 0 ) then
     n_scatt_coef = 1
     allocate (coefs_scatt(n_scatt_coef))
     allocate (coefs_scatt_instname(n_scatt_coef))
     coefs_scatt_instname(1) = 'amsua'
     call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsua), &
                                coefs_scatt(1))
     if(coef_errorstatus /= errorstatus_success ) then
        write ( 6, * ) 'rttov_readscattcoeffs fatal error'
        stop
     endif
   end if
   if ( i_amsua == 0 .and. i_amsub /= 0 ) then
      n_scatt_coef = 1
      allocate (coefs_scatt(n_scatt_coef))
      allocate (coefs_scatt_instname(n_scatt_coef))
      coefs_scatt_instname(1) = 'amsub'
      call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsub), &
                                 coefs_scatt(1))
      if(coef_errorstatus /= errorstatus_success ) then
        write ( 6, * ) 'rttov_readscattcoeffs fatal error'
        stop
      endif      
   end if

   if ( i_amsua /= 0 .and. i_amsub /= 0 ) then
    n_scatt_coef = 2
    allocate (coefs_scatt(n_scatt_coef))
    allocate (coefs_scatt_instname(n_scatt_coef))
    coefs_scatt_instname(1) = 'amsua'
    call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsua), &
                               coefs_scatt(1))
    if(coef_errorstatus /= errorstatus_success ) then
       write ( 6, * ) 'rttov_readscattcoeffs fatal error'
       stop
    endif
    coefs_scatt_instname(2) = 'amsub'
    call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsub), &
                               coefs_scatt(2))
    if(coef_errorstatus /= errorstatus_success ) then
       write ( 6, * ) 'rttov_readscattcoeffs fatal error'
       stop
    endif
   end if

  if ( i_amsua == 0 .and. i_amsub == 0 ) n_scatt_coef = 0

 end if

!----------------------------------------------------------------
!  5.0 set up some common varibles for innovation/observation structure
!----------------------------------------------------------------

  iv%instid(1:nsensor)%nlevels = coefs(1:nsensor)%nlevels

  write(6,*) ' Have read in the RTTOV coef files for the following sensors '

  loop_sensor2: do n = 1, nsensor

!---------------------------------------------------
!  5.1 get more information about sensor
!---------------------------------------------------
   nprofiles = 1
   write(6,'(a,2x,a,i5,2x,a,i5)') iv%instid(n)%rttovid_string, &
                        'nchanl=',nchanl(n), 'nscan=',nscan(n)

 call rttov_setupchan(nprofiles, nchanl(n), coefs(n), &    ! in
                  nfrequencies, nchannels, nbtout )        ! out

  Allocate ( lprofiles(nfrequencies) )
  Allocate ( channels (nfrequencies) )
  Allocate ( polarisations(nchannels, 3) )
  Allocate ( emissivity( nchannels ) )
  Allocate ( surfem ( nchannels ) )
  surfem(:) = 0.

 call rttov_setupindex(nchanl(n), nprofiles, nfrequencies, &    ! in
                  nchannels, nbtout, coefs(n), surfem,  &    ! in
                  lprofiles, channels, polarisations,     &    ! out
                  emissivity  )

     write (*,*) 'nprofiles = ', nprofiles, '  nchanl = ', nchanl(n), &
                 ' nfrequencies = ', nfrequencies, &
                 ' nchannels    = ', nchannels, &
                 ' nbtout       = ', nbtout
     write (*,*) 'lprofiles    = ', lprofiles
     write (*,*) 'channels     = ', channels
!     write (*,*) 'polarisations(:,1)= ', polarisations(:,1)
!     write (*,*) 'polarisations(:,2)= ', polarisations(:,2)
!     write (*,*) 'polarisations(:,3)= ', polarisations(:,3)


     deallocate ( lprofiles )
     deallocate ( channels  )
     deallocate ( polarisations )
     deallocate ( emissivity )
     deallocate ( surfem )

!-------------------------------------------------------
!  6.0 read bias correction coefs files
!-------------------------------------------------------

 if (read_biascoef) then
         call da_get_unit(bunit)
         filename='wrfvar_run/'//adjustl(trim(iv%instid(n)%rttovid_string))//'.bias'
         open(bunit,file=filename, &
            form='formatted',iostat = iost, status='old')
         if (iost /= 0) then
            call da_error(__FILE__,__LINE__, &
               (/"Cannot open radiance biascorr file "//adjustl(filename)/))
         end if

  allocate ( satinfo(n) % rms(nscan(n)+1,nchanl(n)) )
  allocate ( satinfo(n) % std(nscan(n)+1,nchanl(n)) )
  allocate ( satinfo(n) %   a(nscan(n)+1,nchanl(n)) )
  allocate ( satinfo(n) %   b(nscan(n)+1,nchanl(n)) )

   read(bunit,*)  !trim(platform),'-',trim(satid),'-',trim(instrument)
   read(bunit,*)  !trim(cdate)
   read(bunit,*)  !nchan, nscan

   read(bunit,*) !  ' Number of    total obs                    = ', num_rad
   read(bunit,*) !
   read(bunit,*) !
   read(bunit,*) !  ' Number of rejected obs by SI>3K           = ', nscat
   read(bunit,*) !  ' Number of rejected obs by CLWP>0.2mm      = ', nclwp
   read(bunit,*) !  ' Number of rejected obs by Mixture surface = ', nsurf
   read(bunit,*) !  ' Number of rejected obs by abs(IV)>20      = ', ninv(:)
   read(bunit,*) !  ' Number of rejected obs by abs(IV)>5*sigma = ', nerr(:)
   read(bunit,*) !  ' Number of     good obs                    = ', ngood(:)

   read(bunit,*) !  'scan  ', 'chan ', 'number ', 'average ', 'rms  std ', 'a ','b '

   do k = 1,nscan(n)+1
   do j = 1,nchanl(n)
      read(bunit,'(i3,i5,i8,3(2x,f8.3),2(2x,f11.3))') &
         iscan, ichan, npos,ave, &
         satinfo(n)%rms(k,j),satinfo(n)%std(k,j), &
         satinfo(n)%a(k,j),  satinfo(n)%b(k,j)
   end do
   end do

   close(bunit)
   call da_free_unit(bunit)

 end if

  end do loop_sensor2

 if (use_error_factor_rad) then

    do n=1,rtminit_nsensor
       allocate ( satinfo(n)%error_factor(1:nchanl(n)) )
       satinfo(n)%error_factor(:) = 1.0
    end do

    call da_get_unit(unit_factor_rad)
      filename='wrfvar_run/radiance_error.factor'
      open(unit_factor_rad, file=filename, &
           form='formatted',iostat = iost, status='old')    

      if (iost /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open radiance error factor file "//adjustl(filename)/))
      end if

    read(unit_factor_rad, *)
    do 
      read(unit_factor_rad,fmt='(a15,i3,i8,3f15.5,f8.3)',iostat=iost)   &
          rttovid_string, ichan, num_tot, joa,jo,trace,factor 
      if ( iost == 0 ) then
        do n=1,rtminit_nsensor
          if ( trim(rttovid_string)== trim(iv%instid(n)%rttovid_string) ) then
             satinfo(n)%error_factor(ichan) = factor
             if (print_detail_obs)  &
                write(6,'(a,i5,a,f10.3)') trim(rttovid_string)//' Channel ', ichan, ' Error Factor = ', factor
             exit
          end if
        end do
      else
         exit
      end if
    end do
    close(unit_factor_rad)
    call da_free_unit(unit_factor_rad)

 end if

  deallocate(nscan)
  deallocate(nchanl)

  allocate ( time_slots(0:num_fgat_time) )
  call da_get_time_slots(num_fgat_time,time_window_min,analysis_date, &
                         time_window_max, time_slots)

  return

end subroutine da_rttov_init
#endif
