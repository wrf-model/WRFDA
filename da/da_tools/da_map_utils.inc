!---------------------------------------------------------------------------
! Code copied from SI. See map_utils_header.inc for details
!---------------------------------------------------------------------------

subroutine da_map_init(proj)

   ! Initializes the map projection structure to missing values

   implicit none
   type(proj_info), intent(inout)  :: proj

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
   proj%init     = .false.

end subroutine da_map_init

  subroutine da_map_set(proj_code,lat1,lon1,knowni,knownj,dx,stdlon,truelat1,truelat2,proj)
    ! Given a partially filled proj_info structure, this routine computes
    ! polei, polej, rsw, and cone (if LC projection) to complete the 
    ! structure.  This allows us to eliminate redundant calculations when
    ! calling the coordinate conversion routines multiple times for the
    ! same map.
    ! This will generally be the first routine called when a user wants
    ! to be able to use the coordinate conversion routines, and it
    ! will call the appropriate subroutines based on the 
    ! proj%code which indicates which projection type  this is.

    implicit none
    
    ! Declare arguments
    integer, intent(in)               :: proj_code
    real, intent(in)                  :: lat1
    real, intent(in)                  :: lon1
    real, intent(in)                  :: dx
    real, intent(in)                  :: stdlon
    real, intent(in)                  :: truelat1
    real, intent(in)                  :: truelat2
    real, intent(in)                  :: knowni , knownj
    type(proj_info), intent(out)      :: proj

    ! Local variables


    ! Executable code

    ! First, check for validity of mandatory variables in proj
    if ( ABS(lat1) .GT. 90. ) then
      call da_error(__FILE__,__LINE__, &
        (/"Latitude of origin corner required as follows: -90N <= lat1 < = 90.N"/))
    end if
    if ( ABS(lon1) .GT. 180.) then
      call da_error(__FILE__,__LINE__, &
        (/"Longitude of origin required as follows: -180E <= lon1 <= 180W"/))
    end if
    if ((dx .LE. 0.).AND.(proj_code .NE. PROJ_LATLON)) then
      call da_error(__FILE__,__LINE__, &
        (/"Require grid spacing (dx) in meters be positive!"/))
    end if
    if ((ABS(stdlon) .GT. 180.).AND.(proj_code .NE. PROJ_MERC)) then
      call da_error(__FILE__,__LINE__, &
        (/"Need orientation longitude (stdlon) as: -180E <= lon1 <= 180W"/)) 
    end if
    if (ABS(truelat1).GT.90.) then
      call da_error(__FILE__,__LINE__, &
        (/"Set true latitude 1 for all projections!"/))
    end if
   
    call da_map_init(proj) 
    proj%code  = proj_code
    proj%lat1 = lat1
    proj%lon1 = lon1
    proj%knowni = knowni
    proj%knownj = knownj
    proj%dx    = dx
    proj%stdlon = stdlon
    proj%truelat1 = truelat1
    proj%truelat2 = truelat2
    if (proj%code .NE. PROJ_LATLON) then
      proj%dx = dx
      if (truelat1 .LT. 0.) then
        proj%hemi = -1.0 
      else
        proj%hemi = 1.0
      end if
      proj%rebydx = earth_radius_m / dx
    end if
    pick_proj: select case(proj%code)

      case(PROJ_PS)
        if (print_detail_map) then
           write(unit=stdout,fmt='(A)') 'Setting up POLAR STEREOGRAPHIC map...'
        end if
        call da_set_ps(proj)

      case(PROJ_LC)
        if (print_detail_map) then
           write(unit=stdout,fmt='(A)') 'Setting up LAMBERT CONFORMAL map...'
        end if
        if (ABS(proj%truelat2) .GT. 90.) then
           if (print_detail_map) then
              write(unit=stdout,fmt='(A)') 'Second true latitude not set, assuming a tangent'
              write(unit=stdout,fmt='(A,F10.3)') 'projection at truelat1: ', proj%truelat1
           end if
          proj%truelat2=proj%truelat1
        end if
        call da_set_lc(proj)
   
      case (PROJ_MERC)
        if (print_detail_map) then
           write(unit=stdout,fmt='(A)') 'Setting up MERCATOR map...'
        end if
        call da_set_merc(proj)
   
      case (PROJ_LATLON)
        if (print_detail_map) then
           write(unit=stdout,fmt='(A)') 'Setting up CYLinDRICAL EQUIDISTANT LATLON map...'
        end if
        ! Convert lon1 to 0->360 notation
        if (proj%lon1 .LT. 0.) proj%lon1 = proj%lon1 + 360.
   
      case default
        write(unit=message(1),fmt='(A,I2)') 'Unknown projection code: ', proj%code
        call da_error(__FILE__,__LINE__,message(1:1))
      end select pick_proj
    proj%init = .true.
    return
  end subroutine da_map_set
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine da_latlon_to_ij(proj, lat, lon, i, j)
    ! Converts input lat/lon values to the cartesian (i,j) value
    ! for the given projection. 

    implicit none
    type(proj_info), intent(in)          :: proj
    real, intent(in)                     :: lat
    real, intent(in)                     :: lon
    real, intent(out)                    :: i
    real, intent(out)                    :: j

    if (.NOT.proj%init) then
      call da_error(__FILE__,__LINE__, &
        (/"You have not called map_set for this projection!"/))
    end if

    select case(proj%code)
 
      case(PROJ_LATLON)
        call da_llij_latlon(lat,lon,proj,i,j)

      case(PROJ_MERC)
        call da_llij_merc(lat,lon,proj,i,j)
        i = i + proj%knowni - 1.0
        j = j + proj%knownj - 1.0

      case(PROJ_PS)
        call da_llij_ps(lat,lon,proj,i,j)
      
      case(PROJ_LC)
        call da_llij_lc(lat,lon,proj,i,j)
        i = i + proj%knowni - 1.0
        j = j + proj%knownj - 1.0

      case default
        write(unit=message(1),fmt='(A,I2)') &
          'Unrecognized map projection code: ', proj%code
        call da_error(__FILE__,__LINE__,message(1:1))
 
    end select
    return
  end subroutine da_latlon_to_ij
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine da_ij_to_latlon(proj, ii, jj, lat, lon)
    ! Computes geographical latitude and longitude for a given (i,j) point
    ! in a grid with a projection of proj

    implicit none
    type(proj_info),intent(in)          :: proj
    real, intent(in)                    :: ii
    real, intent(in)                    :: jj
    real, intent(out)                   :: lat
    real, intent(out)                   :: lon
    real                                :: i, j

    if (.NOT.proj%init) then
      call da_error(__FILE__,__LINE__, &
        (/"You have not called map_set for this projection!"/))
    end if

    i = ii
    j = jj

    select case (proj%code)

      case (PROJ_LATLON)
        call da_ijll_latlon(i, j, proj, lat, lon)

      case (PROJ_MERC)
        i = ii - proj%knowni + 1.0
        j = jj - proj%knownj + 1.0
        call da_ijll_merc(i, j, proj, lat, lon)

      case (PROJ_PS)
        call da_ijll_ps(i, j, proj, lat, lon)

      case (PROJ_LC)

        i = ii - proj%knowni + 1.0
        j = jj - proj%knownj + 1.0
        call da_ijll_lc(i, j, proj, lat, lon)

      case default
        write(unit=message(1),fmt='(A,I2)') &
          "Unrecognized map projection code: ", proj%code
        call da_error(__FILE__,__LINE__,message(1:1))

    end select
    return
  end subroutine da_ij_to_latlon
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine da_set_ps(proj)
    ! Initializes a polar-stereographic map projection from the partially
    ! filled proj structure. This routine computes the radius to the
    ! southwest corner and computes the i/j location of the pole for use
    ! in llij_ps and ijll_ps.
    implicit none
 
    ! Declare args
    type(proj_info), intent(inout)    :: proj

    ! Local vars
    real                              :: ala1
    real                              :: alo1
    real                              :: reflon
    real                              :: scale_top

    ! Executable code

! To define the cone factor for polar stereographic projection 
! (Y.-R. Guo 04/25/2005):
    proj%cone = 1.0

    reflon = proj%stdlon + 90.

    ! Compute numerator term of map scale factor
    scale_top = 1. + proj%hemi * Sin(proj%truelat1 * rad_per_deg)

    ! Compute radius to lower-left (SW) corner
    ala1 = proj%lat1 * rad_per_deg
    proj%rsw = proj%rebydx*COS(ala1)*scale_top/(1.+proj%hemi*Sin(ala1))

    ! Find the pole point
    alo1 = (proj%lon1 - reflon) * rad_per_deg
    proj%polei = proj%knowni - proj%rsw * COS(alo1)
    proj%polej = proj%knownj - proj%hemi * proj%rsw * Sin(alo1)
    if (print_detail_map) then
       PRinT '(A,2F10.1)', 'Computed (I,J) of pole point: ',proj%polei,proj%polej
    end if
    return
  end subroutine da_set_ps
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine da_llij_ps(lat,lon,proj,i,j)
    ! Given latitude (-90 to 90), longitude (-180 to 180), and the
    ! standard polar-stereographic projection information via the 
    ! public proj structure, this routine returns the i/j indices which
    ! if within the domain range from 1->nx and 1->ny, respectively.

    implicit none

    ! Delcare input arguments
    real, intent(in)               :: lat
    real, intent(in)               :: lon
    type(proj_info),intent(in)     :: proj

    ! Declare output arguments     
    real, intent(out)              :: i !(x-index)
    real, intent(out)              :: j !(y-index)

    ! Declare local variables
    
    real                           :: reflon
    real                           :: scale_top
    real                           :: ala
    real                           :: alo
    real                           :: rm

    ! BEGin CODE

  
    reflon = proj%stdlon + 90.
   
    ! Compute numerator term of map scale factor

    scale_top = 1. + proj%hemi * Sin(proj%truelat1 * rad_per_deg)

    ! Find radius to desired point
    ala = lat * rad_per_deg
    rm = proj%rebydx * COS(ala) * scale_top/(1. + proj%hemi *Sin(ala))
    alo = (lon - reflon) * rad_per_deg
    i = proj%polei + rm * COS(alo)
    j = proj%polej + proj%hemi * rm * Sin(alo)
 
    return
  end subroutine da_llij_ps
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine da_ijll_ps(i, j, proj, lat, lon)

    ! This is the inverse subroutine da_of llij_ps.  It returns the 
    ! latitude and longitude of an i/j point given the projection info 
    ! structure.  

    implicit none

    ! Declare input arguments
    real, intent(in)                    :: i    ! Column
    real, intent(in)                    :: j    ! Row
    type (proj_info), intent(in)        :: proj
    
    ! Declare output arguments
    real, intent(out)                   :: lat     ! -90 -> 90 North
    real, intent(out)                   :: lon     ! -180 -> 180 East

    ! Local variables
    real                                :: reflon
    real                                :: scale_top
    real                                :: xx,yy
    real                                :: gi2, r2
    real                                :: arccos

    ! Begin Code

    ! Compute the reference longitude by rotating 90 degrees to the east
    ! to find the longitude line parallel to the positive x-axis.
    reflon = proj%stdlon + 90.
   
    ! Compute numerator term of map scale factor
    scale_top = 1. + proj%hemi * Sin(proj%truelat1 * rad_per_deg)

    ! Compute radius to point of interest
    xx = i - proj%polei
    yy = (j - proj%polej) * proj%hemi
    r2 = xx**2 + yy**2

    ! Now the magic code
    if (r2 .EQ. 0.) then 
      lat = proj%hemi * 90.
      lon = reflon
    else
      gi2 = (proj%rebydx * scale_top)**2.
      lat = deg_per_rad * proj%hemi * ASin((gi2-r2)/(gi2+r2))
      arccos = ACOS(xx/sqrt(r2))
      if (yy .GT. 0) then
        lon = reflon + deg_per_rad * arccos
      else
        lon = reflon - deg_per_rad * arccos
      end if
    end if
  
    ! Convert to a -180 -> 180 East convention
    if (lon .GT. 180.) lon = lon - 360.
    if (lon .LT. -180.) lon = lon + 360.
    return
  
  end subroutine da_ijll_ps
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine da_set_lc(proj)
    ! Initialize the remaining items in the proj structure for a
    ! lambert conformal grid.

    implicit none
    
    type(proj_info), intent(inout)     :: proj

    real                               :: arg
    real                               :: deltalon1
    real                               :: tl1r
    real                               :: ctl1r

    ! Compute cone factor
    call da_lc_cone(proj%truelat1, proj%truelat2, proj%cone)
    if (print_detail_map) then
       PRinT '(A,F8.6)', 'Computed cone factor: ', proj%cone
    end if
    ! Compute longitude differences and ensure we stay out of the
    ! forbidden "cut zone"
    deltalon1 = proj%lon1 - proj%stdlon
    if (deltalon1 .GT. +180.) deltalon1 = deltalon1 - 360.
    if (deltalon1 .LT. -180.) deltalon1 = deltalon1 + 360.

    ! Convert truelat1 to radian and compute COS for later use
    tl1r = proj%truelat1 * rad_per_deg
    ctl1r = COS(tl1r)

    ! Compute the radius to our known lower-left (SW) corner
    proj%rsw = proj%rebydx * ctl1r/proj%cone * &
           (TAN((90.*proj%hemi-proj%lat1)*rad_per_deg/2.) / &
            TAN((90.*proj%hemi-proj%truelat1)*rad_per_deg/2.))**proj%cone

    ! Find pole point
    arg = proj%cone*(deltalon1*rad_per_deg)
    proj%polei = 1. - proj%hemi * proj%rsw * Sin(arg)
    proj%polej = 1. + proj%rsw * COS(arg)  
    if (print_detail_map) then
       PRinT '(A,2F10.3)', 'Computed pole i/j = ', proj%polei, proj%polej
    end if
    return
  end subroutine da_set_lc                             
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine da_lc_cone(truelat1, truelat2, cone)

  ! subroutine da_to compute the cone factor of a Lambert Conformal projection

    implicit none
    
    ! Input Args
    real, intent(in)             :: truelat1  ! (-90 -> 90 degrees N)
    real, intent(in)             :: truelat2  !   "   "  "   "     "

    ! Output Args
    real, intent(out)            :: cone

    ! Locals

    ! BEGin CODE

    ! First, see if this is a secant or tangent projection.  For tangent
    ! projections, truelat1 = truelat2 and the cone is tangent to the 
    ! Earth's surface at this latitude.  For secant projections, the cone
    ! intersects the Earth's surface at each of the distinctly different
    ! latitudes
    if (ABS(truelat1-truelat2) .GT. 0.1) then
      cone = ALOG10(COS(truelat1*rad_per_deg)) - &
             ALOG10(COS(truelat2*rad_per_deg))
      cone = cone /(ALOG10(TAN((45.0 - ABS(truelat1)/2.0) * rad_per_deg)) - &
             ALOG10(TAN((45.0 - ABS(truelat2)/2.0) * rad_per_deg)))        
    else
       cone = Sin(ABS(truelat1)*rad_per_deg )  
    end if
  return
  end subroutine da_lc_cone
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine da_ijll_lc( i, j, proj, lat, lon)

  ! subroutine da_to convert from the (i,j) cartesian coordinate to the 
  ! geographical latitude and longitude for a Lambert Conformal projection.

  ! History:
  ! 25 Jul 01: Corrected by B. Shaw, NOAA/FSL
  ! 
    implicit none

    ! Input Args
    real, intent(in)              :: i        ! Cartesian X coordinate
    real, intent(in)              :: j        ! Cartesian Y coordinate
    type(proj_info),intent(in)    :: proj     ! Projection info structure

    ! Output Args                 
    real, intent(out)             :: lat      ! Latitude (-90->90 deg N)
    real, intent(out)             :: lon      ! Longitude (-180->180 E)

    ! Locals 
    real                          :: inew
    real                          :: jnew
    real                          :: r
    real                          :: chi,chi1,chi2
    real                          :: r2
    real                          :: xx
    real                          :: yy

    ! BEGin CODE

    chi1 = (90. - proj%hemi*proj%truelat1)*rad_per_deg
    chi2 = (90. - proj%hemi*proj%truelat2)*rad_per_deg

    ! See if we are in the southern hemispere and flip the indices
    ! if we are. 
    if (proj%hemi .EQ. -1.) then 
      inew = -i + 2.
      jnew = -j + 2.
    else
      inew = i
      jnew = j
    end if

    ! Compute radius**2 to i/j location
    xx = inew - proj%polei
    yy = proj%polej - jnew
    r2 = (xx*xx + yy*yy)
    r = sqrt(r2)/proj%rebydx
   
    ! Convert to lat/lon
    if (r2 .EQ. 0.) then
      lat = proj%hemi * 90.
      lon = proj%stdlon
    else
       
      ! Longitude
      lon = proj%stdlon + deg_per_rad * ATAN2(proj%hemi*xx,yy)/proj%cone
      lon = MOD(lon+360., 360.)

      ! Latitude.  Latitude determined by solving an equation adapted 
      ! from:
      !  Maling, D.H., 1973: Coordinate Systems and Map Projections
      ! Equations #20 in Appendix I.  
        
      if (chi1 .EQ. chi2) then
        chi = 2.0*ATAN( ( r/TAN(chi1) )**(1./proj%cone) * TAN(chi1*0.5) )
      else
        chi = 2.0*ATAN( (r*proj%cone/Sin(chi1))**(1./proj%cone) * TAN(chi1*0.5)) 
      end if
      lat = (90.0-chi*deg_per_rad)*proj%hemi

    end if

    if (lon .GT. +180.) lon = lon - 360.
    if (lon .LT. -180.) lon = lon + 360.
    return
    end subroutine da_ijll_lc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine da_llij_lc( lat, lon, proj, i, j)

  ! subroutine da_to compute the geographical latitude and longitude values
  ! to the cartesian x/y on a Lambert Conformal projection.
    
    implicit none

    ! Input Args
    real, intent(in)              :: lat      ! Latitude (-90->90 deg N)
    real, intent(in)              :: lon      ! Longitude (-180->180 E)
    type(proj_info),intent(in)      :: proj     ! Projection info structure

    ! Output Args                 
    real, intent(out)             :: i        ! Cartesian X coordinate
    real, intent(out)             :: j        ! Cartesian Y coordinate

    ! Locals 
    real                          :: arg
    real                          :: deltalon
    real                          :: tl1r
    real                          :: rm
    real                          :: ctl1r
    

    ! BEGin CODE
    
    ! Compute deltalon between known longitude and standard lon and ensure
    ! it is not in the cut zone
    deltalon = lon - proj%stdlon
    if (deltalon .GT. +180.) deltalon = deltalon - 360.
    if (deltalon .LT. -180.) deltalon = deltalon + 360.
    
    ! Convert truelat1 to radian and compute COS for later use
    tl1r = proj%truelat1 * rad_per_deg
    ctl1r = COS(tl1r)     
   
    ! Radius to desired point
    rm = proj%rebydx * ctl1r/proj%cone * &
         (TAN((90.*proj%hemi-lat)*rad_per_deg/2.) / &
          TAN((90.*proj%hemi-proj%truelat1)*rad_per_deg/2.))**proj%cone

    arg = proj%cone*(deltalon*rad_per_deg)
    i = proj%polei + proj%hemi * rm * Sin(arg)
    j = proj%polej - rm * COS(arg)

    ! Finally, if we are in the southern hemisphere, flip the i/j
    ! values to a coordinate system where (1,1) is the SW corner
    ! (what we assume) which is different than the original NCEP
    ! algorithms which used the NE corner as the origin in the 
    ! southern hemisphere (left-hand vs. right-hand coordinate?)
    if (proj%hemi .EQ. -1.) then
      i = 2. - i  
      j = 2. - j
    end if
    return
  end subroutine da_llij_lc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine da_set_merc(proj)
  
    ! Sets up the remaining basic elements for the mercator projection

    implicit none
    type(proj_info), intent(inout)       :: proj
    real                                 :: clain


    !  Preliminary variables

    clain = COS(rad_per_deg*proj%truelat1)
    proj%dlon = proj%dx / (earth_radius_m * clain)

    ! Compute distance from equator to origin, and store in the 
    ! proj%rsw tag.

    proj%rsw = 0.
    if (proj%lat1 .NE. 0.) then
      proj%rsw = (ALOG(TAN(0.5*((proj%lat1+90.)*rad_per_deg))))/proj%dlon
    end if
    return
  end subroutine da_set_merc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine da_llij_merc(lat, lon, proj, i, j)

    ! Compute i/j coordinate from lat lon for mercator projection
  
    implicit none
    real, intent(in)              :: lat
    real, intent(in)              :: lon
    type(proj_info),intent(in)    :: proj
    real,intent(out)              :: i
    real,intent(out)              :: j
    real                          :: deltalon

    deltalon = lon - proj%lon1
    if (deltalon .LT. -180.) deltalon = deltalon + 360.
    if (deltalon .GT. 180.) deltalon = deltalon - 360.
    i = 1. + (deltalon/(proj%dlon*deg_per_rad))
    j = 1. + (ALOG(TAN(0.5*((lat + 90.) * rad_per_deg)))) / &
           proj%dlon - proj%rsw

    return
  end subroutine da_llij_merc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine da_ijll_merc(i, j, proj, lat, lon)

    ! Compute the lat/lon from i/j for mercator projection

    implicit none
    real,intent(in)               :: i
    real,intent(in)               :: j    
    type(proj_info),intent(in)    :: proj
    real, intent(out)             :: lat
    real, intent(out)             :: lon 


    lat = 2.0*ATAN(EXP(proj%dlon*(proj%rsw + j-1.)))*deg_per_rad - 90.
    lon = (i-1.)*proj%dlon*deg_per_rad + proj%lon1
    if (lon.GT.180.) lon = lon - 360.
    if (lon.LT.-180.) lon = lon + 360.
    return
  end subroutine da_ijll_merc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine da_llij_latlon(lat, lon, proj, i, j)
 
    ! Compute the i/j location of a lat/lon on a LATLON grid.
    implicit none
    real, intent(in)             :: lat
    real, intent(in)             :: lon
    type(proj_info), intent(in)  :: proj
    real, intent(out)            :: i
    real, intent(out)            :: j

    real                         :: deltalat
    real                         :: deltalon
    real                         :: lon360
    real                         :: latinc
    real                         :: loninc

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
    if (lon .LT. 0) then 
      lon360 = lon + 360. 
    else 
      lon360 = lon
    end if    
    deltalon = lon360 - proj%lon1      
    
    ! Compute i/j
    i = deltalon/loninc + 1.
    j = deltalat/latinc + 1.
    return
    end subroutine da_llij_latlon
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine da_ijll_latlon(i, j, proj, lat, lon)
 
    ! Compute the lat/lon location of an i/j on a LATLON grid.
    implicit none
    real, intent(in)             :: i
    real, intent(in)             :: j
    type(proj_info), intent(in)  :: proj
    real, intent(out)            :: lat
    real, intent(out)            :: lon

    real                         :: deltalat
    real                         :: deltalon
    real                         :: latinc
    real                         :: loninc

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

    if ((ABS(lat) .GT. 90.).OR.(ABS(deltalon) .GT.360.)) then
      ! Off the earth for this grid
      lat = -999.
      lon = -999.
    else
      lon = lon + 360.
      lon = MOD(lon,360.)
      if (lon .GT. 180.) lon = lon -360.
    end if

    return
  end subroutine da_ijll_latlon
