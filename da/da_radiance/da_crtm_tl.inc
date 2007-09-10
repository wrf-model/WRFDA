#ifdef CRTM
subroutine da_crtm_tl( nsensor,  nchan, nprof, Atmosphere,   &
                            Surface,      &
                            Atmosphere_TL,&
                            Surface_TL,   &
                            GeometryInfo, &
                            ChannelInfo,  &
                            RTSolution,   &
                            RTSolution_TL)

   integer, intent(in)            :: nsensor, nchan, nprof
   TYPE (CRTM_RTSolution_type ),  intent(inout)  :: RTSolution(nchan,nprof),RTSolution_TL(nchan,nprof)
   TYPE (CRTM_ChannelInfo_type),  intent(in)  :: ChannelInfo(nsensor)
   TYPE( CRTM_Atmosphere_type ),  intent(in)  :: Atmosphere(nprof), Atmosphere_TL(nprof)
   TYPE( CRTM_Surface_type ),     intent(in)  :: Surface(nprof),Surface_TL(nprof)
   TYPE( CRTM_GeometryInfo_type ),intent(inout)  :: GeometryInfo(nprof)

   integer :: Error_Status

   if (trace_use) call da_trace_entry("da_crtm_tl")

         Error_Status = CRTM_Tangent_Linear(Atmosphere,   &
                            Surface,      &
                            Atmosphere_TL,&
                            Surface_TL,   &
                            GeometryInfo, &
                            ChannelInfo,  &
                            RTSolution,   &
                            RTSolution_TL) 

         if ( Error_Status /= 0 ) THEN
              call da_error(__FILE__,__LINE__, &
                 (/"Error in calling CRTM_Tangent_Linear"/))
         endif


   if (trace_use) call da_trace_exit("da_crtm_tl")

end subroutine da_crtm_tl
#endif
