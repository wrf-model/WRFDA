#ifdef CRTM
subroutine da_crtm_init(iv,ob)
!------------------------------------------------------------------------------
!  PURPOSE: interface to the initialization subroutine of RTTOV8_5.
!
!  METHOD:  1. read CRTM coefs files; 2. allocate radiance structure
!
!  HISTORY: 10/15/2006  added crtm initialization    Tomislava Vukicevic, ATOC, University of Colorado
!           11/09/2006  Updated                      Zhiquan Liu
!------------------------------------------------------------------------------

 implicit none 

 type (ob_type), intent (inout) :: iv
 type (y_type) , intent (inout) :: ob
! CRTM TYPE 
    TYPE (CRTM_ChannelInfo_type) :: ChannelInfo

!
!  local arguments
!------------------- 
 integer   :: n, k,j,bunit, iscan,ichan,npos
 real      :: ave

!
! CRTM local ---------------------------------------------------
!
  INTEGER :: Error_Status
  CHARACTER( 256 ) :: SpcCoeff_File
  CHARACTER( 256 ) :: TauCoeff_File
  CHARACTER( 256 ) :: AerosolCoeff_File
  CHARACTER( 256 ) :: CloudCoeff_File
  CHARACTER( 256 ) :: EmisCoeff_File 
!  CHARACTER( 80 ), pointer :: Sensor_Descriptor(:)
!
! end of CRTM local
!----------------------------------------------------------------
!  input parameters of RTTOV_SETUP
!----------------------------------
  integer :: err_unit        ! Logical error unit (<0 for default)
  integer :: verbosity_level ! (<0 for default)
  integer :: nsensor, unit_factor_rad
  integer, allocatable :: sensor(:,:) ! instrument id

!  output parameters of RTTOV_SETUP
!-------------------------------------
  integer, allocatable :: errorstatus(:)  ! return code
  integer              :: coef_errorstatus, i_amsua, i_amsub
  integer     :: j, error
  integer , pointer   ::  nscan(:), nchanl(:)

! local variables
!----------------
 integer             :: nprofiles, nfrequencies, nchannels, nbtout
 integer , pointer   :: lprofiles(:)
 real    , pointer   :: surfem(:), emissivity(:)
 integer , pointer   :: channels (:), polarisations(:,:)
 integer             :: mxchn
 integer             :: idum, wmo_sensor_id, sensor_type, iost

! local variables for tuning error factor
 character(len=20)   ::  rttovid_string
 integer             ::  num_tot
 real                ::  joa, jo, trace, factor 

!--------------------------------------------------------------
!  1.0 setup RTTOV instrument triplets from namelist parameter
!--------------------------------------------------------------

  call da_trace_entry("da_crtm_init")

!--------------------------------------------------------------
!  1.0 setup from namelist parameter
!--------------------------------------------------------------
  nsensor = rtminit_nsensor
  allocate (nscan(nsensor))
  allocate (nchanl(nsensor))

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
      stop ' in da_crtm_init '
   end if

   iv%instid(n)%nchan  = nchanl(n)
   ob%instid(n)%nchan  = nchanl(n)

   allocate ( iv%instid(n)%ichan(1:nchanl(n)), stat = error )
   if( error /= 0 ) then
      Write( *, '( "mem allocation error to iv%instid(n)%ichan")' )
      Stop ' in da_crtm_init '
   end if

   allocate ( ob%instid(n)%ichan(1:nchanl(n)), stat = error )
   if( error /= 0 ) then
        Write( *, '( "mem allocation error to ob%instid(n)%ichan")' )
        Stop ' in da_crtm_init '
   end if

   open(109,file=trim(iv%instid(n)%rttovid_string)//'.info', &
           form='formatted',iostat = iost, status='old')

   if (iost /= 0) then
         write(unit=*, fmt='(/A,A)') &
         'ERROR IN OPEN Radiance Info FILE ', trim(iv%instid(n)%rttovid_string)//'.info'
          stop 'CANNOT BE FOUND OR CANNOT BE OPENED in da_crtm_init '
   end if

   allocate ( satinfo(n) % ichan(nchanl(n)) )
   allocate ( satinfo(n) % iuse (nchanl(n)) )
   allocate ( satinfo(n) % error(nchanl(n)) )
   allocate ( satinfo(n) % polar(nchanl(n)) )

   read(109,*)
   do j = 1, nchanl(n)
     read(109,'(1x,5i5,2e18.10)')    &
                     wmo_sensor_id, &
               satinfo(n)%ichan(j), &
                       sensor_type, &
               satinfo(n)%iuse(j) , &
                              idum, &
               satinfo(n)%error(j), &
               satinfo(n)%polar(j)
     iv%instid(n)%ichan(j) = satinfo(n)%ichan(j)
     ob%instid(n)%ichan(j) = satinfo(n)%ichan(j)
   end do

  end do loop_sensor

!---------------------------------------------------------------------
! 3.0 get CRTM sensor descriptor to use in CRTM_Set_ChannelInfo later
!---------------------------------------------------------------------
  allocate(Sensor_Descriptor(nsensor))
  call da_crtm_sensor_descriptor(nsensor,Sensor_Descriptor)

! CRTM load coefficients
!-----------------------------------------------------------
! 3.1 call CRTM_Init to load coefficients and fill ChannelInfo structure
!-----------------------------------------------------------
  ! INPUT: 
     SpcCoeff_File     = 'CRTM_SpcCoeff.bin'
     TauCoeff_File     = 'CRTM_TauCoeff.bin'
     AerosolCoeff_File = 'CRTM_AerosolCoeff.bin'
     CloudCoeff_File   = 'CRTM_CldCoeff.bin'
     EmisCoeff_File    = 'CRTM_EmisCoeff.bin'
  !   File_Path         - path to coefficient files (if not specified current directory is used)
  ! WARNING: Coefficient file names specified in the source code for test only
  !          Must be replaced with dynamic assign of file names 
  !----------------------------------------------------------------
  ! ChannelInfo structure contains on output: 
  !
  ! n_channels - integer, total number of channels
  ! Channel_Index - integer pointer, index of the channels loaded during initialization
  ! Sensor_Channel - integer pointer, the sensor channel #
  ! Sensor_Descriptor - character pointer, character string containing satellite and sensor descr
  !                                        example: amsre_aqua (Appendix B in User guide)
  ! NCEP_Sensor_ID - integer pointer
  ! WMO_Satellite_ID - integer pointer
  ! WMO_Sensor_ID - integer pointer
  !----------------------------------------------------------------- 

     Error_Status = CRTM_Init( ChannelInfo, &
                              SpcCoeff_File = SpcCoeff_File, &
                              TauCoeff_File = TauCoeff_File, &
                              AerosolCoeff_File = AerosolCoeff_File, &
                              EmisCoeff_File = EmisCoeff_File, &
                              CloudCoeff_File = CloudCoeff_File) 

!   Error_Status = CRTM_Set_ChannelInfo(Sensor_Descriptor(1),ChannelInfo)

     IF ( Error_Status /= 0 ) THEN 
       call da_error(__FILE__,__LINE__, &
         (/"Error in initializing CRTM"/))
     END IF

!   write (6,*) 'ChannelInfo content'
!   write (6,*) 'n_channels ',ChannelInfo%n_channels
!   write (6,*) 'Channel_Index ',ChannelInfo%Channel_Index
!   write (6,*) 'Sensor_Channel ',ChannelInfo%Sensor_Channel
!   write (6,*) 'Sensor_Descriptor ',ChannelInfo%Sensor_Descriptor

!-------------------------------------------------------
!  4.0 read bias correction coefs files
!-------------------------------------------------------

 if (read_biascoef) then
   loop_sensor2: do n = 1, nsensor

   call da_get_unit(bunit)
   open(bunit,file=trim(iv%instid(n)%rttovid_string)//'.bias', &
           form='formatted',iostat = iost, status='old')
   if (iost /= 0) then
      write(unit=*, fmt='(/A,A)') &
         'ERROR IN OPEN Radiance biascorr FILE ', trim(iv%instid(n)%rttovid_string)//'.bias'
          stop 'CANNOT BE FOUND OR CANNOT BE OPENED in da_crtm_init '
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
 
 end do loop_sensor2
 end if

!-------------------------------------------------------
!  5.0 read error factor file
!-------------------------------------------------------
 if (use_error_factor_rad) then

    do n=1,rtminit_nsensor
       allocate ( satinfo(n)%error_factor(1:nchanl(n)) )
       satinfo(n)%error_factor(:) = 1.0
    end do

    call da_get_unit(unit_factor_rad)
    open(unit_factor_rad, file='radiance_error.factor', &
         form='formatted',iostat = iost, status='old')

    if (iost /= 0) then
     write(unit=*, fmt='(/A,A)') &
         'ERROR IN OPEN Radiance Error Factor FILE : radiance_error.factor'
          stop 'CANNOT BE FOUND OR CANNOT BE OPENED in da_crtm_init '
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
                write(6,'(a,i5,a,f10.3)') trim(rttovid_string)//' Channel ', ichan, 'Error Factor = ', factor
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

!---------------------------------------------
!  6.0 get FGAT time slots
!---------------------------------------------
  allocate ( time_slots(0:num_fgat_time) )
  call da_get_time_slots(num_fgat_time,time_window_min,time_analysis, &
                         time_window_max, time_slots)


  deallocate(nscan)
  deallocate(nchanl)

  call da_trace_exit("da_crtm_init")

end subroutine da_crtm_init
#endif
