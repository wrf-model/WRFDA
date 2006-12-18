SUBROUTINE DA_Setup_FirstGuess_MM5( xb, xbx, xp)

!------------------------------------------------------------------------------
!  PURPOSE: Define/allocate components of MM5 model state.
!
!  METHOD:
!
!  HISTORY: 01/06/2000 - Creation of F90 version.           Dale Barker
!           10/19/2001 - Mods for parallel implementation.  Al Bourgeois
!           08/10/2004 - Make it work with WRF3DVAR.        Wei Huang
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(OUT)         :: xb     ! First guess state
   TYPE (xbx_type),INTENT(OUT)         :: xbx    ! Header & non-gridded vars.
   type (xpose_type), intent(in)       :: xp     ! Dimensions and xpose buffers. 
 
   TYPE (mm5_model_type)               :: xb_mm5 ! MM5 model structure.
   INTEGER                             :: ier         ! error index

   integer                             :: i, j
   integer                             :: wrf_dim1, wrf_dim2, wrf_dim3
   REAL                                :: x, y

   character(len=24) :: xb_date, an_date
   integer(kind=4)   :: flag
   integer           :: len, index, seconds

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

!---------------------------------------------------------------------------
!  [1.0] Read original MM5 format first guess:
!---------------------------------------------------------------------------

!   WRITE (0,'(/,A,I3,/,A)')' READ MM5 BACKGROUND STATE VECTOR IN UNIT ',JPIN, &
!                           ' ---------------------------------------- '

   IF ( wrf_dm_on_monitor() ) THEN
      seconds = 86400

      rewind(jpin)

      READ(jpin, iostat=ier) flag

      if( ier /= 0) then
          write(0, fmt='(a, i4, a)') &
               'Error in read from unit:', jpin, &
               'Stopped here.'
          call abort()
      end if

      READ(jpin, iostat=ier) xb_mm5 % big_header % bhi, &
                             xb_mm5 % big_header % bhr,&
                             xb_mm5 % big_header % bhic, &
                             xb_mm5 % big_header % bhrc

      REWIND(jpin)

      CALL DA_Allocate_MM5_Model( xb_mm5 )

      do
         call DA_Read_MM5 ( jpin, xb_mm5, ier)

         if( ier /= 0) then
             write(0, fmt='(a, i4, a)') &
                  'Error in read from unit:', jpin, &
                  'Stopped here.'
             call abort()
         end if

         index = xb_mm5 % big_header % bhi(1,1)

!        if(index /= 5) then
            xb_date = xb_mm5%sub_header(1)%current_date

            an_date = xb_date
   
            len = len_trim(ANALYSIS_DATE)
   
            write(an_date(1:len), fmt='(a)') trim(ANALYSIS_DATE)
   
            seconds = DA_Diff_Seconds(an_date, xb_date)
!        else
!           seconds = 0
!        end if

         if ( seconds <= ANALYSIS_ACCU ) exit
      end do
   ENDIF

   call mm5_struct_bcast( xb_mm5 )
 
!---------------------------------------------------------------------------
!  [2.0] Copy header info:
!---------------------------------------------------------------------------

   map_projection = xb_mm5%big_header%bhi(7,1)
   coarse_jy = xb_mm5%big_header%bhi(5,1)
   coarse_ix = xb_mm5%big_header%bhi(6,1)
 
   coarse_ds = xb_mm5%big_header%bhr(1,1) * 0.001

   phic = xb_mm5%big_header%bhr(2,1)
   xlonc = xb_mm5%big_header%bhr(3,1)
   cone_factor = xb_mm5%big_header%bhr(4,1)
   truelat1_3dv = xb_mm5%big_header%bhr(5,1)
   truelat2_3dv = xb_mm5%big_header%bhr(6,1)
   pole = xb_mm5%big_header%bhr(7,1)
   dsm = xb_mm5%big_header%bhr(9,1) * 0.001
   
   start_x = xb_mm5%big_header%bhr(11,1)
   start_y = xb_mm5%big_header%bhr(10,1)
   
   CALL Set_Map_Para ! set up the map background parameters

   CALL llxy( PHIC,XLONC,X,Y )
!   WRITE(0,'(''PHIC,XLONC,X,Y:'',4F10.2)') PHIC,XLONC,X,Y

!---------------------------------------------------------------------------
!  [4.0] Interpolate MM5 B-grid winds to p points of 3DVAR grid (interpolate 
!  u,v to the NORTH-EAST p point). 
!---------------------------------------------------------------------------

   xb % mix = xb_mm5 % big_header % bhi(17,1) - 1 ! Only use interior cross points.
   xb % mjy = xb_mm5 % big_header % bhi(16,1) - 1 ! Only use interior cross points.
   xb % mkz = xb_mm5 % big_header % bhi(12,5)

   mix = xb % mix
   mjy = xb % mjy
   mkz = xb % mkz
   
   wrf_dim1 = xp % ide - xp % ids + 1
   wrf_dim2 = xp % jde - xp % jds + 1
   wrf_dim3 = xp % kde - xp % kds + 1
   
   if ( xb % mix /= wrf_dim1 .or. xb % mjy /= wrf_dim2 .or. &
        xb % mkz /= wrf_dim3  ) then
        
      write(0,'(a)')' DA_Setup_FirstGuess_MM5: Inconsistent WRF/3DVAR namelist'
      write(0,'(6i6)')xb % mix, wrf_dim1, xb % mjy, wrf_dim2, xb % mkz, wrf_dim3
      stop
      
   end if
   
   call b2a(xb_mm5 % u,mjy+1,mix+1,mkz,1)
   call b2a(xb_mm5 % v,mjy+1,mix+1,mkz,0)
   
   CALL DA_Transfer_MM5ToXb( mix, mjy, mkz, &
                             xp, xb_mm5, xb, xbx )

!---------------------------------------------------------------------------
! [5.0] Deallocate MM5 model structure:
!---------------------------------------------------------------------------

   CALL DA_Deallocate_MM5_Model( xb_mm5 )
   
END SUBROUTINE DA_Setup_FirstGuess_MM5

