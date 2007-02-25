#ifdef CRTM
subroutine da_crtm_sensor_descriptor(nsensor,sensor_descriptor)

 integer,        intent(in)  :: nsensor
 character(len=80), intent(out) :: sensor_descriptor(nsensor)

 integer :: i,platform_id,satellite_id,sensor_id
 character (len=80) :: crtm_sat, crtm_sensor

  do i=1,nsensor
     
     platform_id  = rtminit_platform(i)
     satellite_id = rtminit_satid(i)
     sensor_id    = rtminit_sensor(i)

     if (trim(crtm_platform_name(platform_id)) == 'eos') then
        if (satellite_id == 2) crtm_sat='aqua'
        if (satellite_id == 1) crtm_sat='terra'
     else
        write(crtm_sat, '(a,i2.2)')  &
             trim( crtm_platform_name(platform_id) ),satellite_id
     endif

     if ( trim(crtm_sensor_name(sensor_id)) == 'airs' ) then
        crtm_sensor='airs281SUBSET'
     elseif ( trim(crtm_sensor_name(sensor_id)) == 'hirs' ) then
        if (satellite_id <= 14) crtm_sensor='hirs2'
        if (satellite_id >= 15 .and. satellite_id <= 17) crtm_sensor='hirs3'
        if (satellite_id == 18) crtm_sensor='hirs4'
     elseif ( trim(crtm_sensor_name(sensor_id)) == 'avhrr' ) then
        if (satellite_id <= 14) crtm_sensor='avhrr2'
        if (satellite_id >= 15 .and. satellite_id <= 17) crtm_sensor='avhrr3'
        if (satellite_id == 18) crtm_sensor='avhrr4'
     else
        crtm_sensor=crtm_sensor_name(sensor_id)
     endif

     sensor_descriptor(i)=trim(crtm_sensor)//'_'//trim(crtm_sat)

  enddo
     write(6,*) sensor_descriptor

end subroutine da_crtm_sensor_descriptor
#endif
