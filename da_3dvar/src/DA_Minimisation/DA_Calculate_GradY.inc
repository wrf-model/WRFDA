      subroutine DA_Calculate_GradY( iv, re, jo_grad_y)

!-------------------------------------------------------------------------
!  HISTORY: 
!
!      Created: 02/17/2003                   S. R. H. Rizvi
!
!      Purpose:
!            
!           Does part of the obs gradient operation   
!
!-------------------------------------------------------------------------
      implicit none
      
      type (ob_type), intent(IN)    :: iv          ! Innovation vector (O-B).
      type (y_type),  intent(INOUT) :: re          ! Residual vector (O-A).
      type (y_type),  intent(OUT)   :: jo_grad_y   ! Grad_y(Jo)

!-------------------------------------------------------------------------
!     [1.0] Compute components of Grad_y(Jo):
!-------------------------------------------------------------------------

!     Radiosonde

      call DA_Calculate_GradY_Sound( iv, re,  jo_grad_y)
      call DA_Calculate_GradY_Sonde_sfc( iv, re,  jo_grad_y)

!     Surface

      call DA_Calculate_GradY_Synop( iv, re,  jo_grad_y)
                 

!     Geostationary Satellite AMVs

      call DA_Calculate_GradY_GeoAMV( iv, re,  jo_grad_y)

!     Polar orbitting Satellite AMVs

      call DA_Calculate_GradY_PolarAMV( iv, re,  jo_grad_y)


!     Airep

      call DA_Calculate_GradY_Airep( iv, re,  jo_grad_y)

!     Pilot

      call DA_Calculate_GradY_Pilot( iv, re,  jo_grad_y)
      
!     Profiler
      call DA_Calculate_GradY_Profiler( iv, re,  jo_grad_y)
      
!     Satem

      call DA_Calculate_GradY_Satem( iv, re,  jo_grad_y)

!     Metar:

      call DA_Calculate_GradY_Metar( iv, re,  jo_grad_y)

!     Ships:

      call DA_Calculate_GradY_Ships( iv, re,  jo_grad_y)

!     Buoy :

       call DA_Calculate_GradY_Buoy ( iv, re,  jo_grad_y)

!     GPSPW:

      call DA_Calculate_GradY_Gpspw( iv, re,  jo_grad_y)

!     GPSRF:

      call DA_Calculate_GradY_Gpsref( iv, re,  jo_grad_y)

!     SSMI:

      call DA_Calculate_GradY_SSMI( iv, re, jo_grad_y) 

!     SSMT1:

      call DA_Calculate_GradY_SSMT1( iv, re,  jo_grad_y)

!     SSMT2:

      call DA_Calculate_GradY_SSMT2( iv, re,  jo_grad_y)  

!     Pseudo obs:

      call DA_Calculate_GradY_Pseudo( iv, re,  jo_grad_y )    

!     TC bogus obs:

      call DA_Calculate_GradY_bogus( iv, re,  jo_grad_y )    
                  
!     Quikscat scatterometer:

      call DA_Calculate_GradY_Qscat( iv, re, jo_grad_y)
                   
!     Radar:               

      call DA_Calculate_GradY_Radar( iv, re, jo_grad_y)
                   
      end subroutine DA_Calculate_GradY
