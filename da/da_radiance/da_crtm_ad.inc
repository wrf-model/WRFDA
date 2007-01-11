#ifdef CRTM
subroutine da_crtm_ad(  nchan, Atmosphere,   &
                            Surface,      &
                            RTSolution_AD, &
                            GeometryInfo, &
                            ChannelInfo,  &
                            Atmosphere_AD,   &
                            Surface_AD,   &
                            RTSolution)

   integer, intent(in)            :: nchan
   TYPE (CRTM_RTSolution_type ),  intent(inout)  :: RTSolution(nchan)
   TYPE (CRTM_RTSolution_type ),  intent(inout)  :: RTSolution_AD(nchan)
   TYPE (CRTM_ChannelInfo_type),  intent(in)  :: ChannelInfo
   TYPE( CRTM_Atmosphere_type ),  intent(inout)  :: Atmosphere
   TYPE( CRTM_Atmosphere_type ),  intent(inout)  :: Atmosphere_AD
   TYPE( CRTM_Surface_type ),     intent(in)  :: Surface
   TYPE( CRTM_Surface_type ),     intent(inout)  :: Surface_AD
   TYPE( CRTM_GeometryInfo_type ),intent(inout)  :: GeometryInfo

   integer :: Error_Status

   if (trace_use) call da_trace_entry("da_crtm_ad")

         Error_Status = CRTM_Adjoint(Atmosphere,   &
                            Surface,      &
                            RTSolution_AD,&
                            GeometryInfo, &
                            ChannelInfo,  &
                            Atmosphere_AD,&
                            Surface_AD,   &
                            RTSolution) !,   &
                            !Options = Options)
         if ( Error_Status /= 0 ) THEN
              call da_error(__FILE__,__LINE__, &
                 (/"Error in calling CRTM_Adjoint"/))
         endif

   if (trace_use) call da_trace_exit("da_crtm_ad")

end subroutine da_crtm_ad
#endif
