#ifdef CRTM
subroutine da_crtm_direct(  nchan, Atmosphere,   &
                            Surface,      &
                            GeometryInfo, &
                            ChannelInfo,  &
                            RTSolution)

   integer, intent(in)            :: nchan
   type (CRTM_RTSolution_type ),  intent(inout)  :: RTSolution(nchan)
   type (CRTM_ChannelInfo_type),  intent(in)  :: ChannelInfo
   type( CRTM_Atmosphere_type ),  intent(inout)  :: Atmosphere
   type( CRTM_Surface_type ),     intent(in)  :: Surface
   type( CRTM_GeometryInfo_type ),intent(inout)  :: GeometryInfo

   integer :: Error_Status

   if (trace_use) call da_trace_entry("da_crtm_direct")

         Error_Status = CRTM_Forward (Atmosphere,   &
                            Surface,      &
                            GeometryInfo, &
                            ChannelInfo,  &
                            RTSolution)

         if ( Error_Status /= 0 ) then
              call da_error(__FILE__,__LINE__, &
                 (/"Error in calling CRTM_Forward"/))
         end if

  if (trace_use) call da_trace_exit("da_crtm_direct")

end subroutine da_crtm_direct
#endif
