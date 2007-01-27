#ifdef CRTM
subroutine da_crtm_k(  nchan, Atmosphere,   &
                            Surface,      &
                            RTSolution_K, &
                            GeometryInfo, &
                            ChannelInfo,  &
                            Atmosphere_K,   &
                            Surface_K,   &
                            RTSolution)

   integer, intent(in)            :: nchan
   TYPE (CRTM_RTSolution_type ),  intent(inout)  :: RTSolution(nchan)
   TYPE (CRTM_RTSolution_type ),  intent(inout)  :: RTSolution_K(nchan)
   TYPE (CRTM_ChannelInfo_type),  intent(in)     :: ChannelInfo
   TYPE( CRTM_Atmosphere_type ),  intent(inout)  :: Atmosphere
   TYPE( CRTM_Atmosphere_type ),  intent(inout)  :: Atmosphere_K(nchan)
   TYPE( CRTM_Surface_type ),     intent(in)     :: Surface
   TYPE( CRTM_Surface_type ),     intent(inout)  :: Surface_K(nchan)
   TYPE( CRTM_GeometryInfo_type ),intent(inout)  :: GeometryInfo

   integer :: Error_Status

   if (trace_use) call da_trace_entry("da_crtm_k")

         Error_Status = CRTM_K_Matrix(Atmosphere,   &
                            Surface,      &
                            RTSolution_K,&
                            GeometryInfo, &
                            ChannelInfo,  &
                            Atmosphere_K,&
                            Surface_K,   &
                            RTSolution) !,   &
                            !Options = Options)
         if ( Error_Status /= 0 ) THEN
              call da_error(__FILE__,__LINE__, &
                 (/"Error in calling CRTM_K_Matrix"/))
         endif

   if (trace_use) call da_trace_exit("da_crtm_k")

end subroutine da_crtm_k
#endif
