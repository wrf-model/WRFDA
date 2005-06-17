SUBROUTINE da_setup_obs_structures( xp, ob, iv )

!------------------------------------------------------------------------------
!  PURPOSE: Allocate and read in components of observation structure.
!
!  METHOD:  Allocate and read in components of observation structure.
!
!  HISTORY: 10/05/2001 - Creation of F90 version.           Dale Barker
!           10/19/2001 - Mods for parallel implementation.  Al Bourgeois
!           02/12/2002 - Compute interplation weights.      Al Bourgeois
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xpose_type), INTENT(IN):: xp          ! Domain decomposition vars. 
   TYPE ( y_type), INTENT(OUT)  :: ob          ! Observation structure.
   TYPE (ob_type), INTENT(OUT)  :: iv          ! O-B structure.

   write (6,'(A)') ' ------------------------------'
   write (6,'(A)') ' [4.0] Set up observations (ob)'
   write (6,'(A)') ' ------------------------------'
   write (6,*)

!------------------------------------------------------------------------------      
!  [1.0] Setup and read in fields from first guess:
!------------------------------------------------------------------------------      

   iv%missing = missing
!  iv%ptop    = xb%ptop

   iv%num_sound=0
   iv%num_synop=0
   iv%num_geoamv=0
   iv%num_polaramv=0
   iv%num_airep=0
   iv%num_metar=0
   iv%num_ships=0
   iv%num_pilot=0
   iv%num_satem=0
   iv%num_gpspw=0
   iv%num_gpsref=0
   iv%num_qscat=0
   iv%num_ssmt1=0
   iv%num_ssmt2=0
   iv%num_ssmi_tb=0
   iv%num_ssmi_retrieval=0
   iv%num_buoy=0
   iv%num_profiler=0
   iv%num_radar=0
   iv%num_bogus=0

   iv%num_pseudo=num_pseudo

   if ( ob_format == 1 ) then

!     [1.1] BUFR observation input file:
      if(iv%num_pseudo > 0) then
         write(unit=*, fmt='(/6x,a/)') 'Pseudo test only.'
!--------Test pseudo_obs only.
         call DA_Allocate_Y( iv, ob )
         call da_setup_pseudo_obs( iv, ob )
         iv%pseudo(1)%loc%i=int(iv%pseudo(1)%loc%x)
         iv%pseudo(1)%loc%j=int(iv%pseudo(1)%loc%y)
         iv%pseudo(1)%loc%dx=iv%pseudo(1)%loc%x-real(iv%pseudo(1)%loc%i)
         iv%pseudo(1)%loc%dy=iv%pseudo(1)%loc%y-real(iv%pseudo(1)%loc%j)
         iv%pseudo(1)%loc%dxm=1.0-iv%pseudo(1)%loc%dx
         iv%pseudo(1)%loc%dym=1.0-iv%pseudo(1)%loc%dy
      else
         write(6,'(a)')'    Using BUFR format observation input'
         call da_setup_obs_structures_bufr( xp, ob, iv )
      endif
   else if ( ob_format == 2 ) then

!     [1.2] MM5 observation input file:
      write(6,'(a)')'    Using MM5 format observation input'
      call da_setup_obs_structures_ascii( xp, ob, iv )

   end if

!     Get horizontal interpolation weights.
      call da_setup_obs_interp_wts( xp, iv )     
END SUBROUTINE da_setup_obs_structures

