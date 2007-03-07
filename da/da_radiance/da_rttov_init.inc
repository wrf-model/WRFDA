subroutine da_rttov_init(iv,ob)

   !---------------------------------------------------------------------------
   !  Purpose: interface to the initialization subroutine of RTTOV8_5.
   !
   !  METHOD:  1. read RTTOV coefs files; 2. allocate radiance structure
   !
   !  HISTORY: 07/28/2005 - Creation                               Zhiquan Liu
   !           03/22/2006   add error tuning factor read in        Zhiquan Liu
   !
   !---------------------------------------------------------------------------

   implicit none 

   type (ob_type), intent (inout) :: iv
   type (y_type) , intent (inout) :: ob

#ifdef RTTOV
#include "rttov_setup.interface"
#include "rttov_readscattcoeffs.interface"

   !  local arguments
   !------------------- 
   integer   :: i, k,j,bunit, iscan,ichan,npos,unit_factor_rad
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
   integer , allocatable ::  nscan(:), nchan(:)

   ! local variables
   !----------------
   integer             :: nprofiles, nfrequencies, nchannels, nbtout
   integer , allocatable :: lprofiles(:)
   real    , allocatable :: surfem(:), emissivity(:)
   integer , allocatable :: channels (:), polarisations(:,:)
   integer             :: mxchn
   integer , allocatable :: coefs_channels (:,:)
 
   integer             :: idum, wmo_sensor_id, sensor_type, iost, iunit

   ! local variables for tuning error factor
   character(len=20)   ::  rttovid_string
   integer             ::  num_tot
   real                ::  joa, jo, trace, factor 
   character(len=filename_len)   :: filename

   if (trace_use) call da_trace_entry("da_rttov_init")

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
   allocate (nchan(nsensor))

   sensor (1,1:nsensor)  = rtminit_platform (1:nsensor) 
   sensor (2,1:nsensor)  = rtminit_satid    (1:nsensor)
   sensor (3,1:nsensor)  = rtminit_sensor   (1:nsensor)
   coefs_channels(:,:)   = 0

   if (print_detail_radiance) then
      write(unit=message(1),fmt='(A,I5)') 'err_unit             = ', err_unit
      write(unit=message(2),fmt='(A,I5)') 'verbosity_level      = ', verbosity_level
      write(unit=message(3),fmt='(A,I5)') 'nsensor              = ', nsensor
      write(unit=message(4),fmt='(A,10I5)') 'sensor (1,1:nsensor) = ', sensor (1,1:nsensor)
      write(unit=message(5),fmt='(A,10I5)') 'sensor (2,1:nsensor) = ', sensor (2,1:nsensor)
      write(unit=message(6),fmt='(A,10I5)') 'sensor (3,1:nsensor) = ', sensor (3,1:nsensor)
      call da_message(message(1:6))
   end if

   !----------------------------------------------------------------
   ! 2.0 set up some common variables for innovation/observation structure
   !----------------------------------------------------------------

   iv % num_inst = nsensor
   ob % num_inst = nsensor

   allocate (iv%instid(1:nsensor))
   allocate (ob%instid(1:nsensor))
   allocate (satinfo(1:nsensor))

   iv%instid(1:nsensor)%num_rad = 0
   ob%instid(1:nsensor)%num_rad = 0

   loop_sensor: do i = 1, nsensor

      iv%instid(i)%platform_id  = rtminit_platform(i)
      iv%instid(i)%satellite_id = rtminit_satid(i)
      iv%instid(i)%sensor_id    = rtminit_sensor(i)
      if ( rtminit_satid(i) < 10 ) then
         write(iv%instid(i)%rttovid_string, '(a,i1,a)')  &
            trim( platform_name(rtminit_platform(i)) )//'-',  &
            rtminit_satid(i),     &
            '-'//trim( inst_name(rtminit_sensor(i)) )
      else
         write(iv%instid(i)%rttovid_string, '(a,i2.2,a)')  &
            trim( platform_name(rtminit_platform(i)) )//'-',  &
            rtminit_satid(i),     &
            '-'//trim( inst_name(rtminit_sensor(i)) )
      end if

      if ( trim( inst_name(rtminit_sensor(i))) == 'msu' ) then
         nchan(i)  = 4
         nscan(i)   = 11
      else if ( trim( inst_name(rtminit_sensor(i))) == 'hirs' ) then
         nchan(i)  = 19
         nscan(i)   = 56
      else if ( trim( inst_name(rtminit_sensor(i))) == 'amsua' ) then
         nchan(i)  = 15
         nscan(i)   = 30
      else if ( trim( inst_name(rtminit_sensor(i))) == 'amsub' ) then
         nchan(i)  = 5
         nscan(i)   = 90
      else if ( trim( inst_name(rtminit_sensor(i))) == 'airs' ) then
         nchan(i)  = 281
         nscan(i)   = 90
      else if ( trim( inst_name(rtminit_sensor(i))) == 'hsb' ) then
         nchan(i)  = 4
         nscan(i)   = 90
      else        
         call da_error(__FILE__,__LINE__,(/"Unrecognized instrument"/))
      end if

      iv%instid(i)%nchan  = nchan(i)
      ob%instid(i)%nchan  = nchan(i)

      allocate (iv%instid(i)%ichan(1:nchan(i)))
      allocate (ob%instid(i)%ichan(1:nchan(i)))

      call da_get_unit(iunit)
      filename='wrfvar_run/'//adjustl(trim(iv%instid(i)%rttovid_string))//'.info'
      open(unit=iunit,file=filename, &
         form='formatted',iostat = iost, status='old')

      if (iost /= 0) then
         message(1)="Cannot open radiance info file "//adjustl(filename)
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

      allocate (satinfo(i) % ichan(nchan(i)))
      allocate (satinfo(i) % iuse (nchan(i)))
      allocate (satinfo(i) % error(nchan(i)))
      allocate (satinfo(i) % polar(nchan(i)))

      read(unit=iunit,fmt=*)
      do j = 1, nchan(i)
         read(unit=iunit,fmt='(1x,5i5,2e18.10)')    &
                     wmo_sensor_id, &
               satinfo(i)%ichan(j), &
                       sensor_type, &
               satinfo(i)%iuse(j) , &
                              idum, &
               satinfo(i)%error(j), &
               satinfo(i)%polar(j)
         iv%instid(i)%ichan(j) = satinfo(i)%ichan(j)
         ob%instid(i)%ichan(j) = satinfo(i)%ichan(j)
         ! only load coefs of selected channels for AIRS
         coefs_channels(j,i)   = satinfo(i)%ichan(j) 
      end do

      close(iunit)
      call da_free_unit(iunit)

      ! else
      !    do j = 1, nchan(i)
      !       iv%instid(i)%ichan(j) = j
      !       ob%instid(i)%ichan(j) = j
      !       coefs_channels(j,n)   = j
      !    end do
      ! end if

   end do loop_sensor

   !-----------------------------------------------------------
   ! 3.0 call rttov_setup for reading clear sky coefficients
   !-----------------------------------------------------------

   call rttov_setup (&
      errorstatus,      &! out
      err_unit,         &! in
      verbosity_level,  &! in
      nsensor,          &! in
      coefs,            &! out
      sensor ) !,           &! in
      ! coefs_channels )   ! in Optional not work with amsua+b

   if (any(errorstatus(:) /= errorstatus_success)) then
      call da_error(__FILE__,__LINE__,(/"rttov_setup fatal error"/))
   end if

   !-------------------------------------------------------------
   ! 4.0 read coef file for cloud/rain absorption/scattering
   !-------------------------------------------------------------

   if (rttov_scatt) then
      i_amsua = 0
      i_amsub = 0
      do i=1,nsensor
         if (trim(inst_name(rtminit_sensor(i))) == 'amsua') &
            i_amsua = i
         if ( trim(inst_name(rtminit_sensor(i))) == 'amsub') &
            i_amsub = i 
      end do

      if (i_amsua /= 0 .and. i_amsub == 0) then
         n_scatt_coef = 1
         allocate (coefs_scatt(n_scatt_coef))
         allocate (coefs_scatt_instname(n_scatt_coef))
         coefs_scatt_instname(1) = 'amsua'
         call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsua), &
                                   coefs_scatt(1))
         if (coef_errorstatus /= errorstatus_success ) then
            call da_error(__FILE__,__LINE__,(/"rttov_readscattcoeffs fatal error"/))
         end if
      end if
      if (i_amsua == 0 .and. i_amsub /= 0) then
         n_scatt_coef = 1
         allocate (coefs_scatt(n_scatt_coef))
         allocate (coefs_scatt_instname(n_scatt_coef))
         coefs_scatt_instname(1) = 'amsub'
         call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsub), &
                                    coefs_scatt(1))
         if (coef_errorstatus /= errorstatus_success ) then
            call da_error(__FILE__,__LINE__,(/"rttov_readscattcoeffs fatal error"/))
         end if      
      end if

      if (i_amsua /= 0 .and. i_amsub /= 0) then
         n_scatt_coef = 2
         allocate (coefs_scatt(n_scatt_coef))
         allocate (coefs_scatt_instname(n_scatt_coef))
         coefs_scatt_instname(1) = 'amsua'
         call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsua), &
                                  coefs_scatt(1))
         if (coef_errorstatus /= errorstatus_success ) then
            call da_error(__FILE__,__LINE__,(/"rttov_readscattcoeffs fatal error"/))
         end if
         coefs_scatt_instname(2) = 'amsub'
         call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsub), &
                                  coefs_scatt(2))
         if (coef_errorstatus /= errorstatus_success ) then
            call da_error(__FILE__,__LINE__,(/"rttov_readscattcoeffs fatal error"/))
         end if
      end if

      if ( i_amsua == 0 .and. i_amsub == 0 ) n_scatt_coef = 0
   end if

   !----------------------------------------------------------------
   ! 5.0 set up some common varibles for innovation/observation structure
   !----------------------------------------------------------------

   iv%instid(1:nsensor)%nlevels = coefs(1:nsensor)%nlevels

   call da_message((/"Read in the RTTOV coef files for the following sensors"/))

   loop_sensor2: do i = 1, nsensor

      !---------------------------------------------------
      ! 5.1 get more information about sensor
      !---------------------------------------------------

      nprofiles = 1
      write(unit=message(1),fmt='(3x,2a,2x,a,i5,2x,a,i5)') "   ", &
         iv%instid(i)%rttovid_string, 'nchan=',nchan(i), 'nscan=',nscan(i)

      call rttov_setupchan(nprofiles, nchan(i), coefs(i), &    ! in
                     nfrequencies, nchannels, nbtout )        ! out

      allocate (lprofiles(nfrequencies))
      allocate (channels (nfrequencies))
      allocate (polarisations(nchannels, 3))
      allocate (emissivity( nchannels))
      allocate (surfem ( nchannels))
      surfem(:) = 0.

      call rttov_setupindex(nchan(i), nprofiles, nfrequencies, &    ! in
                     nchannels, nbtout, coefs(i), surfem,  &    ! in
                     lprofiles, channels, polarisations,     &    ! out
                     emissivity  )

      write (unit=message(2),fmt=*) &
          '   nprofiles = ', nprofiles, &
          'nchan = ', nchan(i), &
          'nfrequencies = ', nfrequencies, &
          'nchannels    = ', nchannels, &
          'nbtout       = ', nbtout
! JRB problems with PGI
!      write (unit=message(3),fmt=*) '  lprofiles    = ', lprofiles
!      write (unit=message(4),fmt=*) '  channels     = ', channels

      call da_message(message(1:2))

      deallocate (lprofiles)
      deallocate (channels)
      deallocate (polarisations)
      deallocate (emissivity)
      deallocate (surfem)

      !-------------------------------------------------------
      ! 6.0 read bias correction coefs files
      !-------------------------------------------------------

      if ( read_biascoef .and. &
           ( trim(iv%instid(i)%rttovid_string) /= 'eos-2-airs' ) &
             .and. index(iv%instid(i)%rttovid_string,'hirs') <= 0 ) then

         allocate ( satinfo(i) % scanbias  (nchan(i),nscan(i)) )
         allocate ( satinfo(i) % scanbias_b(nchan(i),nscan(i),18) )
         allocate ( satinfo(i) % bcoef     (nchan(i),4) )
         allocate ( satinfo(i) % bcoef0    (nchan(i)) )
         allocate ( satinfo(i) % error_std (nchan(i)) )

         satinfo(i) % error_std(:) = 500.
         satinfo(i) % scanbias(:,:) = 0.
         satinfo(i) % scanbias_b(:,:,:) = 0.
         satinfo(i) % bcoef(:,:) = 0.
         satinfo(i) % bcoef0(:) = 0.

  !  new bias coefs files
  !----------------------------------
    call da_read_biascoef(iv%instid(i)%rttovid_string, &
                      nchan(i),nscan(i),18,4,global, &
                      satinfo(i)%scanbias, &
                      satinfo(i)%scanbias_b, &
                      satinfo(i)%bcoef, &
                      satinfo(i)%bcoef0, &
                      satinfo(i)%error_std)

      end if

   end do loop_sensor2

   if (use_error_factor_rad) then
      do i=1,rtminit_nsensor
         allocate ( satinfo(i)%error_factor(1:nchan(i)) )
         satinfo(i)%error_factor(:) = 1.0
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
            do i=1,rtminit_nsensor
               if ( index(rttovid_string,trim(iv%instid(i)%rttovid_string))>0 ) then
                  satinfo(i)%error_factor(ichan) = factor
                  write(unit=stdout,fmt='(a,i5,a,f10.3)') &
                      trim(rttovid_string)//' Channel ', ichan, &
                      '  Error Factor = ', factor
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

   deallocate (errorstatus)
   deallocate (sensor)
   deallocate (coefs_channels)
   deallocate (nscan)
   deallocate (nchan)

   allocate (time_slots(0:num_fgat_time))
   call da_get_time_slots(num_fgat_time,time_window_min,analysis_date, &
                         time_window_max, time_slots)

   if (trace_use) call da_trace_exit("da_rttov_init")

#endif

end subroutine da_rttov_init
