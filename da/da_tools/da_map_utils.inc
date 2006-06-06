!---------------------------------------------------------------------------
! Code copied from SI. See map_utils_header.inc for details
!---------------------------------------------------------------------------

SUBROUTINE da_map_init(proj)

   ! Initializes the map projection structure to missing values

   IMPLICIT NONE
   TYPE(proj_info), INTENT(INOUT)  :: proj

   proj%lat1 =    -999.9
   proj%lon1 =    -999.9
   proj%dx    =    -999.9
   proj%stdlon =   -999.9
   proj%truelat1 = -999.9
   proj%truelat2 = -999.9
   proj%hemi     = 0.0
   proj%cone     = -999.9
   proj%polei    = -999.9
   proj%polej    = -999.9
   proj%rsw      = -999.9
   proj%knowni   = -999.9
   proj%knownj   = -999.9
   proj%init     = .FALSE.

END SUBROUTINE da_map_init

  SUBROUTINE da_map_set(proj_code,lat1,lon1,knowni,knownj,dx,stdlon,truelat1,truelat2,proj)
    ! Given a partially filled proj_info structure, this routine computes
    ! polei, polej, rsw, and cone (if LC projection) to complete the 
    ! structure.  This allows us to eliminate redundant calculations when
    ! calling the coordinate conversion routines multiple times for the
    ! same map.
    ! This will generally be the first routine called when a user wants
    ! to be able to use the coordinate conversion routines, and it
    ! will call the appropriate subroutines based on the 
    ! proj%code which indicates which projection type  this is.

    IMPLICIT NONE
    
    ! Declare arguments
    INTEGER, INTENT(IN)               :: proj_code
    REAL, INTENT(IN)                  :: lat1
    REAL, INTENT(IN)                  :: lon1
    REAL, INTENT(IN)                  :: dx
    REAL, INTENT(IN)                  :: stdlon
    REAL, INTENT(IN)                  :: truelat1
    REAL, INTENT(IN)                  :: truelat2
    REAL, INTENT(IN)                  :: knowni , knownj
    TYPE(proj_info), INTENT(OUT)      :: proj

    ! Local variables


    ! Executable code

    ! First, check for validity of mandatory variables in proj
    IF ( ABS(lat1) .GT. 90. ) THEN
      CALL wrf_error_fatal3(__FILE__,__LINE__, &
        "Latitude of origin corner required as follows: -90N <= lat1 < = 90.N")
    ENDIF
    IF ( ABS(lon1) .GT. 180.) THEN
      CALL wrf_error_fatal3(__FILE__,__LINE__, &
        "Longitude of origin required as follows: -180E <= lon1 <= 180W")
    ENDIF
    IF ((dx .LE. 0.).AND.(proj_code .NE. PROJ_LATLON)) THEN
      CALL wrf_error_fatal3(__FILE__,__LINE__, &
        "Require grid spacing (dx) in meters be positive!")
    ENDIF
    IF ((ABS(stdlon) .GT. 180.).AND.(proj_code .NE. PROJ_MERC)) THEN
      CALL wrf_error_fatal3(__FILE__,__LINE__, &
        "Need orientation longitude (stdlon) as: -180E <= lon1 <= 180W") 
    ENDIF
    IF (ABS(truelat1).GT.90.) THEN
      CALL wrf_error_fatal3(__FILE__,__LINE__, &
        "Set true latitude 1 for all projections!")
    ENDIF
   
    CALL map_init(proj) 
    proj%code  = proj_code
    proj%lat1 = lat1
    proj%lon1 = lon1
    proj%knowni = knowni
    proj%knownj = knownj
    proj%dx    = dx
    proj%stdlon = stdlon
    proj%truelat1 = truelat1
    proj%truelat2 = truelat2
    IF (proj%code .NE. PROJ_LATLON) THEN
      proj%dx = dx
      IF (truelat1 .LT. 0.) THEN
        proj%hemi = -1.0 
      ELSE
        proj%hemi = 1.0
      ENDIF
      proj%rebydx = earth_radius_m / dx
    ENDIF
    pick_proj: SELECT CASE(proj%code)

      CASE(PROJ_PS)
        IF (print_detail_map) THEN
           WRITE(UNIT=stdout,FMT='(A)') 'Setting up POLAR STEREOGRAPHIC map...'
        END IF
        CALL set_ps(proj)

      CASE(PROJ_LC)
        IF (print_detail_map) THEN
           WRITE(UNIT=stdout,FMT='(A)') 'Setting up LAMBERT CONFORMAL map...'
        END IF
        IF (ABS(proj%truelat2) .GT. 90.) THEN
           IF (print_detail_map) THEN
              WRITE(UNIT=stdout,FMT='(A)') 'Second true latitude not set, assuming a tangent'
              WRITE(UNIT=stdout,FMT='(A,F10.3)') 'projection at truelat1: ', proj%truelat1
           END IF
          proj%truelat2=proj%truelat1
        ENDIF
        CALL set_lc(proj)
   
      CASE (PROJ_MERC)
        IF (print_detail_map) THEN
           WRITE(UNIT=stdout,FMT='(A)') 'Setting up MERCATOR map...'
        END IF
        CALL set_merc(proj)
   
      CASE (PROJ_LATLON)
        IF (print_detail_map) THEN
           WRITE(UNIT=stdout,FMT='(A)') 'Setting up CYLINDRICAL EQUIDISTANT LATLON map...'
        END IF
        ! Convert lon1 to 0->360 notation
        IF (proj%lon1 .LT. 0.) proj%lon1 = proj%lon1 + 360.
   
      CASE DEFAULT
        WRITE(UNIT=errmsg(1),FMT='(A,I2)') 'Unknown projection code: ', proj%code
        CALL wrf_error_fatal3(__FILE__,__LINE__,errmsg(1))
      END SELECT pick_proj
    proj%init = .TRUE.
    RETURN
  END SUBROUTINE da_map_set
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE da_latlon_to_ij(proj, lat, lon, i, j)
    ! Converts input lat/lon values to the cartesian (i,j) value
    ! for the given projection. 

    IMPLICIT NONE
    TYPE(proj_info), INTENT(IN)          :: proj
    REAL, INTENT(IN)                     :: lat
    REAL, INTENT(IN)                     :: lon
    REAL, INTENT(OUT)                    :: i
    REAL, INTENT(OUT)                    :: j

    IF (.NOT.proj%init) THEN
      CALL wrf_error_fatal3(__FILE__,__LINE__, &
        "You have not called map_set for this projection!")
    ENDIF

    SELECT CASE(proj%code)
 
      CASE(PROJ_LATLON)
        CALL llij_latlon(lat,lon,proj,i,j)

      CASE(PROJ_MERC)
        CALL llij_merc(lat,lon,proj,i,j)
        i = i + proj%knowni - 1.0
        j = j + proj%knownj - 1.0

      CASE(PROJ_PS)
        CALL llij_ps(lat,lon,proj,i,j)
      
      CASE(PROJ_LC)
        CALL llij_lc(lat,lon,proj,i,j)
        i = i + proj%knowni - 1.0
        j = j + proj%knownj - 1.0

      CASE DEFAULT
        WRITE(UNIT=errmsg(1),FMT='(A,I2)') &
          'Unrecognized map projection code: ', proj%code
        CALL wrf_error_fatal3(__FILE__,__LINE__,errmsg(1))
 
    END SELECT
    RETURN
  END SUBROUTINE da_latlon_to_ij
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE da_ij_to_latlon(proj, ii, jj, lat, lon)
    ! Computes geographical latitude and longitude for a given (i,j) point
    ! in a grid with a projection of proj

    IMPLICIT NONE
    TYPE(proj_info),INTENT(IN)          :: proj
    REAL, INTENT(IN)                    :: ii
    REAL, INTENT(IN)                    :: jj
    REAL, INTENT(OUT)                   :: lat
    REAL, INTENT(OUT)                   :: lon
    REAL                                :: i, j

    IF (.NOT.proj%init) THEN
      CALL wrf_error_fatal3(__FILE__,__LINE__, &
        "You have not called map_set for this projection!")
    ENDIF

    i = ii
    j = jj

    SELECT CASE (proj%code)

      CASE (PROJ_LATLON)
        CALL ijll_latlon(i, j, proj, lat, lon)

      CASE (PROJ_MERC)
        i = ii - proj%knowni + 1.0
        j = jj - proj%knownj + 1.0
        CALL ijll_merc(i, j, proj, lat, lon)

      CASE (PROJ_PS)
        CALL ijll_ps(i, j, proj, lat, lon)

      CASE (PROJ_LC)

        i = ii - proj%knowni + 1.0
        j = jj - proj%knownj + 1.0
        CALL ijll_lc(i, j, proj, lat, lon)

      CASE DEFAULT
        WRITE(UNIT=errmsg(1),FMT='(A,I2)') &
          "Unrecognized map projection code: ", proj%code
        CALL wrf_error_fatal3(__FILE__,__LINE__,errmsg(1))

    END SELECT
    RETURN
  END SUBROUTINE da_ij_to_latlon
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE da_set_ps(proj)
    ! Initializes a polar-stereographic map projection from the partially
    ! filled proj structure. This routine computes the radius to the
    ! southwest corner and computes the i/j location of the pole for use
    ! in llij_ps and ijll_ps.
    IMPLICIT NONE
 
    ! Declare args
    TYPE(proj_info), INTENT(INOUT)    :: proj

    ! Local vars
    REAL                              :: ala1
    REAL                              :: alo1
    REAL                              :: reflon
    REAL                              :: scale_top

    ! Executable code

! To define the cone factor for polar stereographic projection 
! (Y.-R. Guo 04/25/2005):
    proj%cone = 1.0

    reflon = proj%stdlon + 90.

    ! Compute numerator term of map scale factor
    scale_top = 1. + proj%hemi * SIN(proj%truelat1 * rad_per_deg)

    ! Compute radius to lower-left (SW) corner
    ala1 = proj%lat1 * rad_per_deg
    proj%rsw = proj%rebydx*COS(ala1)*scale_top/(1.+proj%hemi*SIN(ala1))

    ! Find the pole point
    alo1 = (proj%lon1 - reflon) * rad_per_deg
    proj%polei = proj%knowni - proj%rsw * COS(alo1)
    proj%polej = proj%knownj - proj%hemi * proj%rsw * SIN(alo1)
    IF (print_detail_map) THEN
       PRINT '(A,2F10.1)', 'Computed (I,J) of pole point: ',proj%polei,proj%polej
    END IF
    RETURN
  END SUBROUTINE da_set_ps
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE da_llij_ps(lat,lon,proj,i,j)
    ! Given latitude (-90 to 90), longitude (-180 to 180), and the
    ! standard polar-stereographic projection information via the 
    ! public proj structure, this routine returns the i/j indices which
    ! if within the domain range from 1->nx and 1->ny, respectively.

    IMPLICIT NONE

    ! Delcare input arguments
    REAL, INTENT(IN)               :: lat
    REAL, INTENT(IN)               :: lon
    TYPE(proj_info),INTENT(IN)     :: proj

    ! Declare output arguments     
    REAL, INTENT(OUT)              :: i !(x-index)
    REAL, INTENT(OUT)              :: j !(y-index)

    ! Declare local variables
    
    REAL                           :: reflon
    REAL                           :: scale_top
    REAL                           :: ala
    REAL                           :: alo
    REAL                           :: rm

    ! BEGIN CODE

  
    reflon = proj%stdlon + 90.
   
    ! Compute numerator term of map scale factor

    scale_top = 1. + proj%hemi * SIN(proj%truelat1 * rad_per_deg)

    ! Find radius to desired point
    ala = lat * rad_per_deg
    rm = proj%rebydx * COS(ala) * scale_top/(1. + proj%hemi *SIN(ala))
    alo = (lon - reflon) * rad_per_deg
    i = proj%polei + rm * COS(alo)
    j = proj%polej + proj%hemi * rm * SIN(alo)
 
    RETURN
  END SUBROUTINE da_llij_ps
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE da_ijll_ps(i, j, proj, lat, lon)

    ! This is the inverse SUBROUTINE da_of llij_ps.  It returns the 
    ! latitude and longitude of an i/j point given the projection info 
    ! structure.  

    IMPLICIT NONE

    ! Declare input arguments
    REAL, INTENT(IN)                    :: i    ! Column
    REAL, INTENT(IN)                    :: j    ! Row
    TYPE (proj_info), INTENT(IN)        :: proj
    
    ! Declare output arguments
    REAL, INTENT(OUT)                   :: lat     ! -90 -> 90 North
    REAL, INTENT(OUT)                   :: lon     ! -180 -> 180 East

    ! Local variables
    REAL                                :: reflon
    REAL                                :: scale_top
    REAL                                :: xx,yy
    REAL                                :: gi2, r2
    REAL                                :: arccos

    ! Begin Code

    ! Compute the reference longitude by rotating 90 degrees to the east
    ! to find the longitude line parallel to the positive x-axis.
    reflon = proj%stdlon + 90.
   
    ! Compute numerator term of map scale factor
    scale_top = 1. + proj%hemi * SIN(proj%truelat1 * rad_per_deg)

    ! Compute radius to point of interest
    xx = i - proj%polei
    yy = (j - proj%polej) * proj%hemi
    r2 = xx**2 + yy**2

    ! Now the magic code
    IF (r2 .EQ. 0.) THEN 
      lat = proj%hemi * 90.
      lon = reflon
    ELSE
      gi2 = (proj%rebydx * scale_top)**2.
      lat = deg_per_rad * proj%hemi * ASIN((gi2-r2)/(gi2+r2))
      arccos = ACOS(xx/SQRT(r2))
      IF (yy .GT. 0) THEN
        lon = reflon + deg_per_rad * arccos
      ELSE
        lon = reflon - deg_per_rad * arccos
      ENDIF
    ENDIF
  
    ! Convert to a -180 -> 180 East convention
    IF (lon .GT. 180.) lon = lon - 360.
    IF (lon .LT. -180.) lon = lon + 360.
    RETURN
  
  END SUBROUTINE da_ijll_ps
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE da_set_lc(proj)
    ! Initialize the remaining items in the proj structure for a
    ! lambert conformal grid.

    IMPLICIT NONE
    
    TYPE(proj_info), INTENT(INOUT)     :: proj

    REAL                               :: arg
    REAL                               :: deltalon1
    REAL                               :: tl1r
    REAL                               :: ctl1r

    ! Compute cone factor
    CALL lc_cone(proj%truelat1, proj%truelat2, proj%cone)
    IF (print_detail_map) THEN
       PRINT '(A,F8.6)', 'Computed cone factor: ', proj%cone
    END IF
    ! Compute longitude differences and ensure we stay out of the
    ! forbidden "cut zone"
    deltalon1 = proj%lon1 - proj%stdlon
    IF (deltalon1 .GT. +180.) deltalon1 = deltalon1 - 360.
    IF (deltalon1 .LT. -180.) deltalon1 = deltalon1 + 360.

    ! Convert truelat1 to radian and compute COS for later use
    tl1r = proj%truelat1 * rad_per_deg
    ctl1r = COS(tl1r)

    ! Compute the radius to our known lower-left (SW) corner
    proj%rsw = proj%rebydx * ctl1r/proj%cone * &
           (TAN((90.*proj%hemi-proj%lat1)*rad_per_deg/2.) / &
            TAN((90.*proj%hemi-proj%truelat1)*rad_per_deg/2.))**proj%cone

    ! Find pole point
    arg = proj%cone*(deltalon1*rad_per_deg)
    proj%polei = 1. - proj%hemi * proj%rsw * SIN(arg)
    proj%polej = 1. + proj%rsw * COS(arg)  
    IF (print_detail_map) THEN
       PRINT '(A,2F10.3)', 'Computed pole i/j = ', proj%polei, proj%polej
    END IF
    RETURN
  END SUBROUTINE da_set_lc                             
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE da_lc_cone(truelat1, truelat2, cone)

  ! SUBROUTINE da_to compute the cone factor of a Lambert Conformal projection

    IMPLICIT NONE
    
    ! Input Args
    REAL, INTENT(IN)             :: truelat1  ! (-90 -> 90 degrees N)
    REAL, INTENT(IN)             :: truelat2  !   "   "  "   "     "

    ! Output Args
    REAL, INTENT(OUT)            :: cone

    ! Locals

    ! BEGIN CODE

    ! First, see if this is a secant or tangent projection.  For tangent
    ! projections, truelat1 = truelat2 and the cone is tangent to the 
    ! Earth's surface at this latitude.  For secant projections, the cone
    ! intersects the Earth's surface at each of the distinctly different
    ! latitudes
    IF (ABS(truelat1-truelat2) .GT. 0.1) THEN
      cone = ALOG10(COS(truelat1*rad_per_deg)) - &
             ALOG10(COS(truelat2*rad_per_deg))
      cone = cone /(ALOG10(TAN((45.0 - ABS(truelat1)/2.0) * rad_per_deg)) - &
             ALOG10(TAN((45.0 - ABS(truelat2)/2.0) * rad_per_deg)))        
    ELSE
       cone = SIN(ABS(truelat1)*rad_per_deg )  
    ENDIF
  RETURN
  END SUBROUTINE da_lc_cone
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE da_ijll_lc( i, j, proj, lat, lon)

  ! SUBROUTINE da_to convert from the (i,j) cartesian coordinate to the 
  ! geographical latitude and longitude for a Lambert Conformal projection.

  ! History:
  ! 25 Jul 01: Corrected by B. Shaw, NOAA/FSL
  ! 
    IMPLICIT NONE

    ! Input Args
    REAL, INTENT(IN)              :: i        ! Cartesian X coordinate
    REAL, INTENT(IN)              :: j        ! Cartesian Y coordinate
    TYPE(proj_info),INTENT(IN)    :: proj     ! Projection info structure

    ! Output Args                 
    REAL, INTENT(OUT)             :: lat      ! Latitude (-90->90 deg N)
    REAL, INTENT(OUT)             :: lon      ! Longitude (-180->180 E)

    ! Locals 
    REAL                          :: inew
    REAL                          :: jnew
    REAL                          :: r
    REAL                          :: chi,chi1,chi2
    REAL                          :: r2
    REAL                          :: xx
    REAL                          :: yy

    ! BEGIN CODE

    chi1 = (90. - proj%hemi*proj%truelat1)*rad_per_deg
    chi2 = (90. - proj%hemi*proj%truelat2)*rad_per_deg

    ! See if we are in the southern hemispere and flip the indices
    ! if we are. 
    IF (proj%hemi .EQ. -1.) THEN 
      inew = -i + 2.
      jnew = -j + 2.
    ELSE
      inew = i
      jnew = j
    ENDIF

    ! Compute radius**2 to i/j location
    xx = inew - proj%polei
    yy = proj%polej - jnew
    r2 = (xx*xx + yy*yy)
    r = SQRT(r2)/proj%rebydx
   
    ! Convert to lat/lon
    IF (r2 .EQ. 0.) THEN
      lat = proj%hemi * 90.
      lon = proj%stdlon
    ELSE
       
      ! Longitude
      lon = proj%stdlon + deg_per_rad * ATAN2(proj%hemi*xx,yy)/proj%cone
      lon = MOD(lon+360., 360.)

      ! Latitude.  Latitude determined by solving an equation adapted 
      ! from:
      !  Maling, D.H., 1973: Coordinate Systems and Map Projections
      ! Equations #20 in Appendix I.  
        
      IF (chi1 .EQ. chi2) THEN
        chi = 2.0*ATAN( ( r/TAN(chi1) )**(1./proj%cone) * TAN(chi1*0.5) )
      ELSE
        chi = 2.0*ATAN( (r*proj%cone/SIN(chi1))**(1./proj%cone) * TAN(chi1*0.5)) 
      ENDIF
      lat = (90.0-chi*deg_per_rad)*proj%hemi

    ENDIF

    IF (lon .GT. +180.) lon = lon - 360.
    IF (lon .LT. -180.) lon = lon + 360.
    RETURN
    END SUBROUTINE da_ijll_lc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE da_llij_lc( lat, lon, proj, i, j)

  ! SUBROUTINE da_to compute the geographical latitude and longitude values
  ! to the cartesian x/y on a Lambert Conformal projection.
    
    IMPLICIT NONE

    ! Input Args
    REAL, INTENT(IN)              :: lat      ! Latitude (-90->90 deg N)
    REAL, INTENT(IN)              :: lon      ! Longitude (-180->180 E)
    TYPE(proj_info),INTENT(IN)      :: proj     ! Projection info structure

    ! Output Args                 
    REAL, INTENT(OUT)             :: i        ! Cartesian X coordinate
    REAL, INTENT(OUT)             :: j        ! Cartesian Y coordinate

    ! Locals 
    REAL                          :: arg
    REAL                          :: deltalon
    REAL                          :: tl1r
    REAL                          :: rm
    REAL                          :: ctl1r
    

    ! BEGIN CODE
    
    ! Compute deltalon between known longitude and standard lon and ensure
    ! it is not in the cut zone
    deltalon = lon - proj%stdlon
    IF (deltalon .GT. +180.) deltalon = deltalon - 360.
    IF (deltalon .LT. -180.) deltalon = deltalon + 360.
    
    ! Convert truelat1 to radian and compute COS for later use
    tl1r = proj%truelat1 * rad_per_deg
    ctl1r = COS(tl1r)     
   
    ! Radius to desired point
    rm = proj%rebydx * ctl1r/proj%cone * &
         (TAN((90.*proj%hemi-lat)*rad_per_deg/2.) / &
          TAN((90.*proj%hemi-proj%truelat1)*rad_per_deg/2.))**proj%cone

    arg = proj%cone*(deltalon*rad_per_deg)
    i = proj%polei + proj%hemi * rm * SIN(arg)
    j = proj%polej - rm * COS(arg)

    ! Finally, if we are in the southern hemisphere, flip the i/j
    ! values to a coordinate system where (1,1) is the SW corner
    ! (what we assume) which is different than the original NCEP
    ! algorithms which used the NE corner as the origin in the 
    ! southern hemisphere (left-hand vs. right-hand coordinate?)
    IF (proj%hemi .EQ. -1.) THEN
      i = 2. - i  
      j = 2. - j
    ENDIF
    RETURN
  END SUBROUTINE da_llij_lc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE da_set_merc(proj)
  
    ! Sets up the remaining basic elements for the mercator projection

    IMPLICIT NONE
    TYPE(proj_info), INTENT(INOUT)       :: proj
    REAL                                 :: clain


    !  Preliminary variables

    clain = COS(rad_per_deg*proj%truelat1)
    proj%dlon = proj%dx / (earth_radius_m * clain)

    ! Compute distance from equator to origin, and store in the 
    ! proj%rsw tag.

    proj%rsw = 0.
    IF (proj%lat1 .NE. 0.) THEN
      proj%rsw = (ALOG(TAN(0.5*((proj%lat1+90.)*rad_per_deg))))/proj%dlon
    ENDIF
    RETURN
  END SUBROUTINE da_set_merc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE da_llij_merc(lat, lon, proj, i, j)

    ! Compute i/j coordinate from lat lon for mercator projection
  
    IMPLICIT NONE
    REAL, INTENT(IN)              :: lat
    REAL, INTENT(IN)              :: lon
    TYPE(proj_info),INTENT(IN)    :: proj
    REAL,INTENT(OUT)              :: i
    REAL,INTENT(OUT)              :: j
    REAL                          :: deltalon

    deltalon = lon - proj%lon1
    IF (deltalon .LT. -180.) deltalon = deltalon + 360.
    IF (deltalon .GT. 180.) deltalon = deltalon - 360.
    i = 1. + (deltalon/(proj%dlon*deg_per_rad))
    j = 1. + (ALOG(TAN(0.5*((lat + 90.) * rad_per_deg)))) / &
           proj%dlon - proj%rsw

    RETURN
  END SUBROUTINE da_llij_merc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE da_ijll_merc(i, j, proj, lat, lon)

    ! Compute the lat/lon from i/j for mercator projection

    IMPLICIT NONE
    REAL,INTENT(IN)               :: i
    REAL,INTENT(IN)               :: j    
    TYPE(proj_info),INTENT(IN)    :: proj
    REAL, INTENT(OUT)             :: lat
    REAL, INTENT(OUT)             :: lon 


    lat = 2.0*ATAN(EXP(proj%dlon*(proj%rsw + j-1.)))*deg_per_rad - 90.
    lon = (i-1.)*proj%dlon*deg_per_rad + proj%lon1
    IF (lon.GT.180.) lon = lon - 360.
    IF (lon.LT.-180.) lon = lon + 360.
    RETURN
  END SUBROUTINE da_ijll_merc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE da_llij_latlon(lat, lon, proj, i, j)
 
    ! Compute the i/j location of a lat/lon on a LATLON grid.
    IMPLICIT NONE
    REAL, INTENT(IN)             :: lat
    REAL, INTENT(IN)             :: lon
    TYPE(proj_info), INTENT(IN)  :: proj
    REAL, INTENT(OUT)            :: i
    REAL, INTENT(OUT)            :: j

    REAL                         :: deltalat
    REAL                         :: deltalon
    REAL                         :: lon360
    REAL                         :: latinc
    REAL                         :: loninc

    ! Extract the latitude and longitude increments for this grid
    ! (e.g., 2.5 deg for NCEP reanalysis) from the proj structure, where
    ! loninc is saved in the stdlon tag and latinc is saved in truelat1

    latinc = proj%truelat1
    loninc = proj%stdlon

    ! Compute deltalat and deltalon as the difference between the input 
    ! lat/lon and the origin lat/lon

    deltalat = lat - proj%lat1

    ! To account for issues around the dateline, convert the incoming
    ! longitudes to be 0->360.
    IF (lon .LT. 0) THEN 
      lon360 = lon + 360. 
    ELSE 
      lon360 = lon
    ENDIF    
    deltalon = lon360 - proj%lon1      
    
    ! Compute i/j
    i = deltalon/loninc + 1.
    j = deltalat/latinc + 1.
    RETURN
    END SUBROUTINE da_llij_latlon
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE da_ijll_latlon(i, j, proj, lat, lon)
 
    ! Compute the lat/lon location of an i/j on a LATLON grid.
    IMPLICIT NONE
    REAL, INTENT(IN)             :: i
    REAL, INTENT(IN)             :: j
    TYPE(proj_info), INTENT(IN)  :: proj
    REAL, INTENT(OUT)            :: lat
    REAL, INTENT(OUT)            :: lon

    REAL                         :: deltalat
    REAL                         :: deltalon
    REAL                         :: lon360
    REAL                         :: latinc
    REAL                         :: loninc

    ! Extract the latitude and longitude increments for this grid
    ! (e.g., 2.5 deg for NCEP reanalysis) from the proj structure, where
    ! loninc is saved in the stdlon tag and latinc is saved in truelat1

    latinc = proj%truelat1
    loninc = proj%stdlon

    ! Compute deltalat and deltalon 

    deltalat = (j-1.)*latinc
    deltalon = (i-1.)*loninc
    lat = proj%lat1 + deltalat
    lon = proj%lon1 + deltalon

    IF ((ABS(lat) .GT. 90.).OR.(ABS(deltalon) .GT.360.)) THEN
      ! Off the earth for this grid
      lat = -999.
      lon = -999.
    ELSE
      lon = lon + 360.
      lon = MOD(lon,360.)
      IF (lon .GT. 180.) lon = lon -360.
    ENDIF

    RETURN
  END SUBROUTINE da_ijll_latlon
