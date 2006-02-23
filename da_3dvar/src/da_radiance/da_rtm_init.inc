!
subroutine da_rtm_init(iv,ob)
!------------------------------------------------------------------------------
!  PURPOSE: interface to the initialization subroutine of RTTOV8_5.
!
!  METHOD:  1. read RTTOV coefs files; 2. allocate radiance structure
!
!  HISTORY: 28/07/2005 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

 implicit none 

#include "rttov_setup.interface"
#include "rttov_readscattcoeffs.interface"
 
 type (ob_type), intent (inout) :: iv
 type (y_type) , intent (inout) :: ob

!  local arguments
!------------------- 
 integer   :: n, k,j, iscan,ichan,npos
 real      :: ave

!
!  input parameters of RTTOV_SETUP
!----------------------------------
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
 
 integer             :: idum, wmo_sensor_id, sensor_type, iost

!--------------------------------------------------------------
!  1.0 setup RTTOV instrument triplets from namelist parameter
!--------------------------------------------------------------

    mxchn           =  300
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

  WRITE(UNIT=stdout,FMT=*) ' verbosity_level      = ', verbosity_level
  WRITE(UNIT=stdout,FMT=*) ' nsensor              = ', nsensor
  WRITE(UNIT=stdout,FMT=*) ' sensor (1,1:nsensor) = ', sensor (1,1:nsensor)
  WRITE(UNIT=stdout,FMT=*) ' sensor (2,1:nsensor) = ', sensor (2,1:nsensor)
  WRITE(UNIT=stdout,FMT=*) ' sensor (3,1:nsensor) = ', sensor (3,1:nsensor)

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
!  iv%instid(1:nsensor)%nlevels = coefs(1:nsensor)%nlevels
!  write(UNIT=stdout,FMT=*) ' Have read in the RTTOV coef files for the following sensors '

  loop_sensor: do n = 1, nsensor

   iv%instid(n)%platform_id  = rtminit_platform(n)
   iv%instid(n)%satellite_id = rtminit_satid(n)
   iv%instid(n)%sensor_id    = rtminit_sensor(n)
 if ( rtminit_satid(n) < 10 ) then
   write(UNIT=iv%instid(n)%rttovid_string,FMT='(a,i1,a)')  &
             trim( platform_name(rtminit_platform(n)) )//'-',  &
             rtminit_satid(n),     &
             '-'//trim( inst_name(rtminit_sensor(n)) )
 else
   write(UNIT=iv%instid(n)%rttovid_string,FMT='(a,i2.2,a)')  &
             trim( platform_name(rtminit_platform(n)) )//'-',  &
             rtminit_satid(n),     &
             '-'//trim( inst_name(rtminit_sensor(n)) )
 end if
!   write(*,*)  iv%instid(n)%rttovid_string

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
    CALL wrf_error_fatal3(__FILE__,__LINE__, &
      ' unrecognized instrument '//TRIM(inst_name(rtminit_sensor(n))))
   end if

      iv%instid(n)%nchan  = nchanl(n)
      ob%instid(n)%nchan  = nchanl(n)

      allocate ( iv%instid(n)%ichan(1:nchanl(n)), stat = error )
      if( error /= 0 ) then
        CALL wrf_error_fatal3(__FILE__,__LINE__, &
          "mem allocation error to iv%instid(n)%ichan")
      end if

      allocate ( ob%instid(n)%ichan(1:nchanl(n)), stat = error )
      if( error /= 0 ) then
        CALL wrf_error_fatal3(__FILE__,__LINE__, &
          "mem allocation error to ob%instid(n)%ichan")
      end if

     if (trim( inst_name(rtminit_sensor(n))) == 'airs') then

  open(UNIT=rtm_info_unit,file=trim(iv%instid(n)%rttovid_string)//'.info', &
           form='formatted',iostat = iost, status='old')

  if (iost /= 0) then
    CALL wrf_error_fatal3(__FILE__,__LINE__, &
      'Cannot open Radiance Info FILE '// &
      TRIM(iv%instid(n)%rttovid_string)//'.info')
  end if

  allocate ( satinfo(n) % ichan(nchanl(n)) )
  allocate ( satinfo(n) % iuse (nchanl(n)) )
  allocate ( satinfo(n) % error(nchanl(n)) )
  allocate ( satinfo(n) % polar(nchanl(n)) )

  read(rtm_info_unit,*)
  do j = 1, nchanl(n)
    read(UNIT=rtm_info_unit,FMT='(1x,5i5,2e18.10)')    &
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

     else
      do j = 1, nchanl(n)
        iv%instid(n)%ichan(j) = j
        ob%instid(n)%ichan(j) = j
        coefs_channels(j,n)   = j
      end do
     end if

  end do loop_sensor

!-----------------------------------------------------------
! 3.0 call rttov_setup for reading clear sky coefficients
!-----------------------------------------------------------

 call rttov_setup (&
      & errorstatus,      &! out
      & rtm_error_unit,   &! in
      & verbosity_level,  &! in
      & nsensor,          &! in
      & coefs,            &! out
      & sensor,           &! in
      & coefs_channels )   ! in Optional

  if( any(errorstatus(:) /= errorstatus_success )) then
    CALL wrf_error_fatal3(__FILE__,__LINE__, &
     'rttov_setup failed')
  endif

!-------------------------------------------------------------
!  4.0 read coef file for cloud/rain absorption/scattering
!-------------------------------------------------------------
 if (lrttov_scatt) then
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
       CALL wrf_error_fatal3(__FILE__,__LINE__, &
         'rttov_readscattcoeffs failed')
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
       CALL wrf_error_fatal3(__FILE__,__LINE__, &
         'rttov_readscattcoeffs failed')
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
       CALL wrf_error_fatal3(__FILE__,__LINE__, &
         'rttov_readscattcoeffs failed')
    endif
    coefs_scatt_instname(2) = 'amsub'
    call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsub), &
                               coefs_scatt(2))
    if(coef_errorstatus /= errorstatus_success ) then
       CALL wrf_error_fatal3(__FILE__,__LINE__, &
         'rttov_readscattcoeffs failed')
    endif
   end if

  if ( i_amsua == 0 .and. i_amsub == 0 ) n_scatt_coef = 0

 end if

!----------------------------------------------------------------
!  5.0 set up some common varibles for innovation/observation structure
!----------------------------------------------------------------

  iv%instid(1:nsensor)%nlevels = coefs(1:nsensor)%nlevels

  write(UNIT=stdout,FMT=*) ' Have read in the RTTOV coef files for the following sensors '

  loop_sensor2: do n = 1, nsensor

!---------------------------------------------------
!  5.1 get more information about sensor
!---------------------------------------------------
   nprofiles = 1
   write(UNIT=stdout,FMT='(a,2x,a,i5,2x,a,i5)') iv%instid(n)%rttovid_string, &
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

     write (UNIT=stdout,FMT=*) 'nprofiles = ', nprofiles, '  nchanl = ', nchanl(n), &
                 ' nfrequencies = ', nfrequencies, &
                 ' nchannels    = ', nchannels, &
                 ' nbtout       = ', nbtout
     write (UNIT=stdout,FMT=*) 'lprofiles    = ', lprofiles
     write (UNIT=stdout,FMT=*) 'channels     = ', channels
!     write (UNIT=stdout,FMT=*) 'polarisations(:,1)= ', polarisations(:,1)
!     write (UNIT=stdout,FMT=*) 'polarisations(:,2)= ', polarisations(:,2)
!     write (UNIT=stdout,FMT=*) 'polarisations(:,3)= ', polarisations(:,3)


     deallocate ( lprofiles )
     deallocate ( channels  )
     deallocate ( polarisations )
     deallocate ( emissivity )
     deallocate ( surfem )

!-------------------------------------------------------
!  6.0 read bias correction coefs files
!-------------------------------------------------------

 if (lread_biascoef) then
  open(UNIT=rtm_bias_unit,file=trim(iv%instid(n)%rttovid_string)//'.bias', &
           form='formatted',iostat = iost, status='old')
  if (iost /= 0) then
    CALL wrf_error_fatal3(__FILE__,__LINE__, &
      'Cannot open radiance biascorr FILE '//TRIM(iv%instid(n)%rttovid_string)//'.bias')
  end if

  allocate ( satinfo(n) % rms(nscan(n),nchanl(n)) )
  allocate ( satinfo(n) % std(nscan(n),nchanl(n)) )
  allocate ( satinfo(n) %   a(nscan(n),nchanl(n)) )
  allocate ( satinfo(n) %   b(nscan(n),nchanl(n)) )

   read(UNIT=rtm_bias_unit,FMT=*)  !trim(platform),'-',trim(satid),'-',trim(instrument)
   read(UNIT=rtm_bias_unit,FMT=*)  !trim(cdate)
   read(UNIT=rtm_bias_unit,FMT=*)  !nchan, nscan

   read(UNIT=rtm_bias_unit,FMT=*) !  ' Number of    total obs                    = ', num_rad
   read(UNIT=rtm_bias_unit,FMT=*) !
   read(UNIT=rtm_bias_unit,FMT=*) !
   read(UNIT=rtm_bias_unit,FMT=*) !  ' Number of rejected obs by SI>3K           = ', nscat
   read(UNIT=rtm_bias_unit,FMT=*) !  ' Number of rejected obs by CLWP>0.2mm      = ', nclwp
   read(UNIT=rtm_bias_unit,FMT=*) !  ' Number of rejected obs by Mixture surface = ', nsurf
   read(UNIT=rtm_bias_unit,FMT=*) !  ' Number of rejected obs by abs(IV)>20      = ', ninv(:)
   read(UNIT=rtm_bias_unit,FMT=*) !  ' Number of rejected obs by abs(IV)>5*sigma = ', nerr(:)
   read(UNIT=rtm_bias_unit,FMT=*) !  ' Number of     good obs                    = ', ngood(:)

   read(UNIT=rtm_bias_unit,FMT=*) !  'scan  ', 'chan ', 'number ', 'average ', 'rms  std ', 'a ','b '

   do j = 1,nchanl(n)
      read(UNIT=rtm_bias_unit,FMT=*)
!     read(UNIT=rtm_bias_unit,FMT='(i3,i5,i8,2(2x,f8.3),2(2x,f11.3))') &
!                 iscan, ichan(j), bias_nc(j), bias_ave(j), &
!                 bias_std(j),bias_a(j),bias_b(j)
   end do

   do k = 1,nscan(n)
   do j = 1,nchanl(n)
      read(UNIT=rtm_bias_unit,FMT='(i3,i5,i8,3(2x,f8.3),2(2x,f11.3))') &
         iscan, ichan, npos,ave, &
         satinfo(n)%rms(k,j),satinfo(n)%std(k,j), &
         satinfo(n)%a(k,j),  satinfo(n)%b(k,j)
   end do
   end do

   close(UNIT=rtm_bias_unit)

 end if

  end do loop_sensor2

  deallocate(nscan)
  deallocate(nchanl)

  return

end subroutine da_rtm_init
