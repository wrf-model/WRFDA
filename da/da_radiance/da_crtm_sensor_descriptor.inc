#ifdef CRTM
subroutine da_crtm_sensor_descriptor(nsensor,sensor_descriptor)

! n=noaa; f=dmsp; g=goes; c=npoess; eos-1/2=aqua/terra;
 character(len=8), parameter :: platform(1:20) = &
       (/ 'n       ', 'f       ', 'meteosat', 'g       ', 'gms     ', &
          'fy2     ', 'trmm    ', 'ers     ', 'eos     ', 'metop   ', &
          'envisat ', 'msg     ', 'fy1     ', 'adeos   ', 'mtsat   ', &
          'coriolis', 'c       ', 'gifts   ', 'xxxxxxxx', 'xxxxxxxx'/)

! List of instruments  !!!! HIRS is number 0
  Character (len=8), Dimension(0:34) :: sensor  =                &
       & (/ 'hirs    ', 'msu     ', 'ssu     ', 'amsua   ', 'amsub   ',  &
       &    'avhrr   ', 'ssmi    ', 'vtpr1   ', 'vtpr2   ', 'tmi     ',  &
       &    'ssmis   ', 'airs    ', 'hsb     ', 'modis   ', 'atsr    ',  &
       &    'mhs     ', 'iasi    ', 'amsr    ', 'imager  ', 'atms    ',  &
       &    'mviri   ', 'seviri  ', 'imgr    ', 'sndr    ', 'imager  ',  &
       &    'vissr   ', 'mvisr   ', 'cris    ', 'cmis    ', 'viirs   ',  &
       &    'windsat ', 'gifts   ', 'amsre   ', 'SUBSET  ', 'xxxxxxxx'   /)

 integer,        intent(in)  :: nsensor
 character(len=80), intent(out) :: sensor_descriptor(nsensor)

 integer :: i,platform_id,satellite_id,sensor_id
 character (len=80) :: crtm_sat, crtm_sensor

  do i=1,nsensor
     
     platform_id  = rtminit_platform(i)
     satellite_id = rtminit_satid(i)
     sensor_id    = rtminit_sensor(i)

     if (trim(platform(platform_id)) == 'eos') then
        if (satellite_id == 2) crtm_sat='aqua'
        if (satellite_id == 1) crtm_sat='terra'
     else
        write(crtm_sat, '(a,i2.2)')  &
             trim( platform(platform_id) ),satellite_id
     endif

     if ( trim(sensor(sensor_id)) == 'SUBSET' ) then
        crtm_sensor='airs281SUBSET'
     elseif ( trim(sensor(sensor_id)) == 'hirs' ) then
        if (satellite_id <= 14) crtm_sensor='hirs2'
        if (satellite_id >= 15 .and. satellite_id <= 17) crtm_sensor='hirs3'
        if (satellite_id == 18) crtm_sensor='hirs4'
     elseif ( trim(sensor(sensor_id)) == 'avhrr' ) then
        if (satellite_id <= 14) crtm_sensor='avhrr2'
        if (satellite_id >= 15 .and. satellite_id <= 17) crtm_sensor='avhrr3'
        if (satellite_id == 18) crtm_sensor='avhrr4'
     else
        crtm_sensor=sensor(sensor_id)
     endif

     sensor_descriptor(i)=trim(crtm_sensor)//'_'//trim(crtm_sat)

  enddo
     write(6,*) sensor_descriptor

end subroutine da_crtm_sensor_descriptor
#endif
