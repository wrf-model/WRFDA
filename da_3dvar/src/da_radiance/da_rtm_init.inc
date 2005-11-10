!
subroutine da_rtm_init(iv)
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

!  local arguments
 
 integer   :: n

!###################################
!  input parameters of RTTOV_SETUP
!
  integer :: err_unit        ! Logical error unit (<0 for default)
  integer :: verbosity_level ! (<0 for default)
  integer :: nsensor
  integer, allocatable :: sensor(:,:) ! instrument id
!  integer, allocatable :: channels(:,:)   ! channel list per instrument

!  output parameters of RTTOV_SETUP
!
  integer, allocatable :: errorstatus(:)  ! return code
  integer              :: coef_errorstatus, i_amsua, i_amsub
!  type( rttov_coef ), allocatable :: coefs(:)         ! coefficients
  integer     :: j, nchanl, error
  
! local variables
!----------------
 integer             :: nprofiles, nfrequencies, nchannels, nbtout
 integer , pointer   :: lprofiles(:)
 real    , pointer   :: surfem(:), emissivity(:)
 integer , pointer   :: channels (:), polarisations(:,:)
 

!  1.0 setup RTTOV instrument triplets from namelist parameter
!############################################################## 

    err_unit        =  rtminit_errunit
    verbosity_level =  rtminit_print
    nsensor         =  rtminit_nsensor

!  2.0 call rttov_setup
!
  allocate (errorstatus(nsensor))
  allocate (coefs(nsensor))
  allocate (sensor(3,nsensor))
!  allocate (channels(mxchn,nsensor))

  sensor (1,1:nsensor)  = rtminit_platform (1:nsensor) 
  sensor (2,1:nsensor)  = rtminit_satid    (1:nsensor)
  sensor (3,1:nsensor)  = rtminit_sensor   (1:nsensor)

  print *, ' err_unit        = ', err_unit
  print *, ' verbosity_level = ', verbosity_level
  print *, ' nsensor         = ', nsensor
  print *, ' sensor (1,1:nsensor) = ', sensor (1,1:nsensor)
  print *, ' sensor (2,1:nsensor) = ', sensor (2,1:nsensor)
  print *, ' sensor (3,1:nsensor) = ', sensor (3,1:nsensor)

 call rttov_setup (&
      & errorstatus,      &! out
      & err_unit,         &! in
      & verbosity_level,  &! in
      & nsensor,          &! in
      & coefs,             &! out
      & sensor )           ! in
!      & channels         ) ! in Optional

  if( any(errorstatus(:) /= errorstatus_success )) then
     write ( 6, * ) 'rttov_setup fatal error'
     stop ' in da_rtm_init '
  endif

!-------------------------------------------------------------
!  read coef file for cloud/rain absorption/scattering
!-------------------------------------------------------------
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
     Call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsua), &
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
      Call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsub), &
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
    Call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsua), &
                               coefs_scatt(1))
    if(coef_errorstatus /= errorstatus_success ) then
       write ( 6, * ) 'rttov_readscattcoeffs fatal error'
       stop
    endif
    coefs_scatt_instname(2) = 'amsub'
    Call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsub), &
                               coefs_scatt(2))
    if(coef_errorstatus /= errorstatus_success ) then
       write ( 6, * ) 'rttov_readscattcoeffs fatal error'
       stop
    endif
  end if

  if ( i_amsua == 0 .and. i_amsub == 0 ) n_scatt_coef = 0
!----------------------------------------------------------------

  iv % num_inst = nsensor

  allocate (iv%instid(1:nsensor))

  iv%instid(1:nsensor)%num_rad = 0
  iv%instid(1:nsensor)%nlevels = coefs(1:nsensor)%nlevels

  write(*, *) ' Have read in the RTTOV coef files for the following sensors '
  do n = 1, nsensor
   iv%instid(n)%platform_id  = rtminit_platform(n)
   iv%instid(n)%satellite_id = rtminit_satid(n)
   iv%instid(n)%sensor_id    = rtminit_sensor(n)
   write(iv%instid(n)%rttovid_string, '(a,i2.2,a)')  &
             trim( platform_name(rtminit_platform(n)) )//'-',  &
             rtminit_satid(n),     &
             '-'//trim( inst_name(rtminit_sensor(n)) )
   write(*,*)  iv%instid(n)%rttovid_string 

   if ( trim( inst_name(rtminit_sensor(n))) == 'msu' ) then
      nchanl  = 4
   else if ( trim( inst_name(rtminit_sensor(n))) == 'hirs' ) then
      nchanl  = 19
   else if ( trim( inst_name(rtminit_sensor(n))) == 'amsua' ) then   
      nchanl  = 15
   else if ( trim( inst_name(rtminit_sensor(n))) == 'amsub' ) then 
      nchanl  = 5
   else
      write(6,*) ' unrecorgnized instrument '
      stop ' in da_rtm_init '
   end if

!  3.0 get more information about sensor
!---------------------------------------------------
   nprofiles = 1

 call rttov_setupchan(nprofiles, nchanl, coefs(n), &       ! in
                  nfrequencies, nchannels, nbtout )        ! out

  Allocate ( lprofiles(nfrequencies) )
  Allocate ( channels (nfrequencies) )
  Allocate ( polarisations(nchannels, 3) )
  Allocate ( emissivity( nchannels ) )
  Allocate ( surfem ( nchannels ) )
  surfem(:) = 0.

 call rttov_setupindex(nchanl, nprofiles, nfrequencies, &    ! in
                  nchannels, nbtout, coefs(n), surfem,  &    ! in
                  lprofiles, channels, polarisations,     &    ! out
                  emissivity  )

     write (*,*) 'nprofiles = ', nprofiles, '  nchanl = ', nchanl, &
                 ' nfrequencies = ', nfrequencies, &
                 ' nchannels    = ', nchannels, &
                 ' nbtout       = ', nbtout
     write (*,*) 'lprofiles    = ', lprofiles
     write (*,*) 'channels     = ', channels
     write (*,*) 'polarisations(:,1)= ', polarisations(:,1)
     write (*,*) 'polarisations(:,2)= ', polarisations(:,2)
     write (*,*) 'polarisations(:,3)= ', polarisations(:,3)

  DEAllocate ( lprofiles )
  DEAllocate ( channels  )
  DEAllocate ( polarisations )
  DEAllocate ( emissivity )
  DEAllocate ( surfem )

      iv%instid(n)%nchan  = nchanl
      allocate ( iv%instid(n)%ichan(1:nchanl), stat = error )
      If( error /= 0 ) Then
        Write( *, '( "mem allocation error to iv%instid(n)%ichan")' )
        Stop ' in da_rtm_init '
      End If
      do j = 1, nchanl
        iv%instid(n)%ichan(j) = j
      end do

  end do

  return

end subroutine da_rtm_init
