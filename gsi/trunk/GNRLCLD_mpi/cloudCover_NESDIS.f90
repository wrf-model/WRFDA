SUBROUTINE cloudCover_NESDIS(mype,nlat,nlon,nsig,xlong,xlat,t_bk,p_bk,h_bk,zh,xland, &
                        soil_tbk,q_bk,sat_ctp,sat_tem,w_frac,nlev_cld, &
                        cld_cover_3d,cld_type_3d,wthr_type)
!
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  cloudCover_NESDIS  cloud cover analysis using NESDIS cloud products
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-10
!
! ABSTRACT: 
!  This subroutine find cloud cover using NESDIS cloud products
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     mype        - processor ID
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     xlong       - 2D longitude in each grid
!     xlat        - 2D latitude in each grid
!
!     t_bk        - 3D background potentional temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!     h_bk        - 3D background height  
!     zh          - terrain
!     xland       - surface type (water, land)
!     soil_tbk    - background soil temperature
!     q_bk        - 3D mositure
!     sat_ctp     - GOES cloud top pressure in analysis grid
!     sat_tem     - GOES cloud top temperature in analysis grid
!     w_frac      - GOES cloud coverage in analysis grid
!
!   output argument list:
!     nlev_cld    - cloud status
!     cld_cover_3d- 3D cloud cover
!     cld_type_3d - 3D cloud type
!     wthr_type   - 3D weather type
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!

  use constants, only: rd_over_cp, h1000
  use constants, only: deg2rad, rad2deg, pi
  use kinds, only: r_single,i_kind
  use gridmod, only: regional_time

  implicit none

  integer(i_kind),intent(in) :: mype
  integer(i_kind),intent(in) :: nlat,nlon,nsig
!
!  surface observation
!
!
!  background
!
  real(r_single),intent(in) :: t_bk(nlon,nlat,nsig)   ! potentional temperature
  real(r_single),intent(inout) :: p_bk(nlon,nlat,nsig)   ! pressure
  real(r_single),intent(in) :: zh(nlon,nlat)           ! terrain
  real(r_single),intent(in) :: h_bk(nlon,nlat,nsig)   ! height
  real(r_single),intent(in) :: q_bk(nlon,nlat,nsig)   ! mositure
  real(i_kind),intent(in) :: xland(nlon,nlat)           ! surface
  real(i_kind),intent(in) :: xlong(nlon,nlat)           ! longitude
  real(i_kind),intent(in) :: xlat(nlon,nlat)           ! latitude
  real(i_kind),intent(in) :: soil_tbk(nlon,nlat)       ! soil tmperature
!
!
!  Variables for cloud analysis
!
  real (r_single),intent(inout) :: cld_cover_3d(nlon,nlat,nsig)
  integer(i_kind),intent(inout) :: cld_type_3d(nlon,nlat,nsig)
  integer(i_kind),intent(inout) :: wthr_type(nlon,nlat)
!
! Observation
!
  real(r_single),intent(inout) :: sat_ctp(nlon,nlat)
  real(r_single),intent(inout) :: sat_tem(nlon,nlat)
  real(r_single),intent(inout) :: w_frac(nlon,nlat)
  integer(i_kind),intent(out):: nlev_cld(nlon,nlat)
!
!-------------------------------------------------------------------------
! --- Key parameters
!     Cloud_def_p       = 0.000001      g/g cloud top threshold for model
!     Min_cloud_lev_p   = 3             Lowest model level to check for cloud
!     Rh_clear_p        = 0.80          RH to use when clearing cloud
!     Sat_cloud_pthick_p=  50.          Depth (mb) of new sat-sensed cloud layer
!     cloud_zthick_p    = 300.          Depth (m) of new cloud layer
!     Cloud_q_qvis_rat_p= 0.10          Ratio of cloud water to water/ice
!                                        saturation mixing ratio for new cloud
!     Max_cloud_top_p   = 150.          Max cloud top (mb)
!     RH_makecloud_p    = 0.90          RH threshold for making cloud if bkg
!                                         rh is at least this high at
!                                         neighboring points
!     Cloud_up_p        = 10            Pressure thickness for
!                                         Upward extrapolation of cloud
!                                        (if model level is within cloud_up_p
!                                        mb of sat cloud level)
!     min_cloud_p_p     = 960.          Max pressure at which NESDIS cloud
!                                         info is considered reliable
!                                         (i.e., not reliable at low levels)

!     zen_limit         = 0.20          Solar zenith angle - lower limit
!                                         at which sun is considered
!                                         high enough to trust the
!                                         GOES cloud data

        real    Cloud_def_p
        integer min_cloud_lev_p
        real    Rh_clear_p
        real    sat_cloud_pthick_p
        real    cloud_zthick_p
        real    Cloud_q_qvis_rat_p
        real    Max_cloud_top_p
        real    RH_makecloud_p
        real    cloud_up_p
        real    min_cloud_p_p
        real    build_cloud_frac_p
        real    clear_cloud_frac_p
        real    co2_preslim_p
        real    auto_conver
        real    zen_limit


! --- Key parameters
        data  Cloud_def_p       / 0.000001/
        data  Min_cloud_lev_p   / 1/    !  w/ sfc cld assim
        data  Rh_clear_p        / 0.80/
        data  Sat_cloud_pthick_p/ 40./
        data  cloud_zthick_p    /300./
        data  Cloud_q_qvis_rat_p/ 0.05/
        data  Max_cloud_top_p   / 150./
        data  RH_makecloud_p    / 0.90/
        data  cloud_up_p        / 0. /
        data  min_cloud_p_p     /1080./  ! w/ sfc cld assim
        data  build_cloud_frac_p / 0.80/  !PR-version
        data  clear_cloud_frac_p / 0.01/     !PR-version
        data  co2_preslim_p     / 620./
        data  auto_conver       /0.0002/
        data  zen_limit         / 0.20 /
!-----------------------------------------------------------
!
! temp.
!
  INTEGER     null_p
  REAL        spval_p
  PARAMETER ( null_p     = -1       )
  PARAMETER ( spval_p    =  99999.0 )

  INTEGER :: i,j,k,k1
  INTEGER :: i1,j1
  INTEGER :: nx_p, ny_p, nztn_p
  INTEGER :: gmt,nday,iyear,imonth,iday
  REAL  :: declin
  real  :: hrang,xxlat
  real  :: qw
  real(r_single) :: csza(nlon,nlat)
 
  INTEGER :: ndof_tot, npts_clear, npts_build, npts_bel650
  INTEGER :: npts_tskin_flag, npts_stab_flag, npts_ptly_cloudy
  real (r_single) :: tbk_k(nlon,nlat,nsig)

  INTEGER :: npts_ctp_change, npts_ctp_delete, npts_ctp_nobuddy
  INTEGER :: npts_clr_nobuddy,npts_ctp_marine_remap
  real (r_single) :: dctp, dctpabs

  INTEGER :: jp1,jm1,ip1,im1
  real    :: tsmin

  INTEGER :: kisotherm, ibuddy, ktempmin
  real    :: tempmin,dth2dp2, stab, stab_threshold

  real    :: firstcloud, pdiff,pdiffabove
!
!====================================================================
!  Begin
!
!  set constant name consitent with RUC code to resue the code
!
   nx_p=nlon
   ny_p=nlat
   nztn_p=nsig
!
!
!   calculation solar declination
!
  iyear=regional_time(1)
  imonth=regional_time(2)
  iday=regional_time(3)
  call getdays(nday,iyear,imonth,iday)
  declin=deg2rad*23.45*sin(2.0*pi*(284+nday)/365.0)
!  if(mype==0)  write(6,*) nday,iyear,imonth,iday,declin
!
!   from mb to Pa

   do k = 1,nsig
   do j = 1,nlat
   do i = 1,nlon
      qw=q_bk(i,j,k)/(1. + q_bk(i,j,k))
      tbk_k(i,j,k)=t_bk(i,j,k)*(p_bk(i,j,k)/h1000)**rd_over_cp
      p_bk(i,j,k)  = p_bk(i,j,k)*100.
      tbk_k(i,j,k)=tbk_k(i,j,k)/(1.0+0.6078*qw)
   end do
   end do
   end do

   if( p_bk(nlon/2,nlat/2,2) < 5000.0 ) then
     write(6,*) 'cloudCover_NESDIS: pressure unit check failed', p_bk(nlon/2,nlat/2,2) 
     call stop2(114)
   endif
   if( tbk_k(nlon/2,nlat/2,nsig-2) > 300 ) then
     write(6,*) 'cloudCover_NESDIS: tmperature unit check failed', tbk_k(nlon/2,nlat/2,nsig-2) 
     call stop2(114)
   endif

!
!  csza = fraction of solar constant (cos of zenith angle)
   gmt = regional_time(4)   ! UTC 
   do j=2,ny_p-1
   do i=2,nx_p-1
     hrang= (15.*gmt + xlong(i,j) - 180. )*deg2rad
     xxlat=xlat(i,j)*deg2rad
     csza(i,j)=sin(xxlat)*sin(declin)                &
           +cos(xxlat)*cos(declin)*cos(hrang)
   end do
   end do

!
!  start checking the data
!
   ndof_tot = 0   !counting total number of grids of sat info
   npts_clear = 0
   npts_build = 0
   npts_bel650 = 0
   npts_tskin_flag = 0
   npts_stab_flag = 0
   npts_ptly_cloudy = 0

   do j=2,ny_p-1
   do i=2,nx_p-1
     jp1 = min(j+1,ny_p)
     jm1 = max(j-1,1   )
     ip1 = min(i+1,nx_p)
     im1 = max(i-1,1   )
     tsmin = soil_tbk(i,j)
!  ---  Determine min skin temp in 3x3 around grid point.
!       This is to detect nearby presence of coastline.
     do j1 = jm1,jp1
     do i1 = im1,ip1
        tsmin = min(tsmin,soil_tbk(i1,j1) )
     end do
     end do

     if (      w_frac(i,j).gt.-1.               &
         .and. (sat_tem(i,j)-tsmin).gt.-2.    &
         .and. sat_ctp(i,j).gt.co2_preslim_p  &
         .and. sat_ctp(i,j).lt.1010.          &
         .and. xland(i,j).ne.0                &
         .and. p_bk(i,j,1)/100..ge.950. ) then
            w_frac(i,j) = -99999.
            sat_tem(i,j) =  99999.
            sat_ctp(i,j) =      0.
            nlev_cld(i,j) = -999
            npts_tskin_flag = npts_tskin_flag + 1
     end if
     if (w_frac(i,j).lt.clear_cloud_frac_p  .and.  &
         w_frac(i,j).gt.-1.)        then
            sat_ctp(i,j) = 1013.0
            npts_clear = npts_clear + 1
     end if
     if (w_frac(i,j).gt.clear_cloud_frac_p.and.    &
         w_frac(i,j).lt.build_cloud_frac_p) then
             w_frac(i,j) = -99999.
             sat_tem(i,j) =  99999.
             sat_ctp(i,j) =      0.
             nlev_cld(i,j) = -999
             npts_ptly_cloudy = npts_ptly_cloudy + 1
     end if
     if (w_frac(i,j).gt.build_cloud_frac_p.and.   &
         sat_ctp(i,j).lt.1050) then
             npts_build = npts_build + 1
     end if
     if (sat_ctp(i,j).gt.co2_preslim_p .and. sat_ctp(i,j).lt.1010.)  &
             npts_bel650 = npts_bel650 + 1

! -- nlev_cld = 1 if cloud info is present
! -- nlev_cld = 0 if no cloud info is at this grid point

     if(nlev_cld(i,j) .ge. 1) ndof_tot = ndof_tot + 1
   end do   ! i
   end do   ! j
!
   if(mype==0) then
   write(6,*) 'cloudCover_NESDIS: TOTAL NUMBER OF GRID pts w/ GOES CLOUD data =',ndof_tot
   write(6,*) 'cloudCover_NESDIS: CLEAR NUMBER OF GRID pts w/ GOES CLOUD data =',npts_clear
   write(6,*) 'cloudCover_NESDIS: BUILD NUMBER OF GRID pts w/ GOES CLOUD data =',npts_build 
   write(6,*) 'cloudCover_NESDIS: PTCLDY NUMBER OF GRID pts w/ GOES CLOUD data =',npts_ptly_cloudy
   write(6,*) 'cloudCover_NESDIS: > 650mb - no OF GRID pts w/ GOES CLOUD data =',npts_bel650
   write(6,*) 'cloudCover_NESDIS: Flag CTP - skin temp too close to TB, no=',npts_tskin_flag
   write(6,*) 'cloudCover_NESDIS: Clear -> cloud frac < clear frac'
   write(6,*) 'cloudCover_NESDIS: Build -> cloud frac > build frac'
   endif

!
!!
!
   npts_ctp_change = 0
   npts_ctp_delete = 0
   npts_ctp_nobuddy = 0
   npts_clr_nobuddy = 0
   npts_ctp_marine_remap = 0
   dctp = 0.
   dctpabs = 0.

! - stability threshold for building cloud - 3K / 100 mb (10000 Pa)

   stab_threshold = 3./10000.
   do j=2,ny_p-1
   do i=2,nx_p-1

! -- GOES indicates clouds in the lower troposphere
     if (sat_ctp(i,j).lt.1010. .and. sat_ctp(i,j).gt.co2_preslim_p) then
       do k=3,nztn_p-1
         if ( (sat_ctp(i,j)-p_bk(i,j,k)/100.).lt.75.) then
         if (abs(sat_tem(i,j)-tbk_k(i,j,k)).lt.1.5       &
           .and. p_bk(i,j,k)/100. .gt.co2_preslim_p ) then
              dctpabs = dctpabs + abs(sat_ctp(i,j)-p_bk(i,j,k)/100.)
              dctp = dctp+ (sat_ctp(i,j)-p_bk(i,j,k)/100.)
              k1 = k

1115          continue

! --- This stability check only for reassigining CTP using RUC bkg profile.
!      There is a later general check also.
              stab = (t_bk(i,j,k1+1)-t_bk(i,j,k1))    &
                  /(p_bk(i,j,k1)-p_bk(i,j,k1+1))
              if (stab .lt. stab_threshold) then
                   k1 = k1 + 1
                   if ((p_bk(i,j,k)-p_bk(i,j,k1)).gt.5000.) then
                     w_frac(i,j) = -99999.
                     sat_tem(i,j) =  99999.
                     sat_ctp(i,j) =  99999.
                     nlev_cld(i,j) = -999
                     npts_stab_flag = npts_stab_flag + 1
                     go to 111
                   end if
                   go to 1115
              end if

              sat_ctp(i,j) = p_bk(i,j,k)/100.
              npts_ctp_change = npts_ctp_change + 1
              go to 111
         end if
         end if
       end do  ! k

! --- Remap marine cloud to min temp level below 880 mb
!       if no matching RUC temp already found

       if (xland(i,j).eq.0. .and. sat_ctp(i,j).gt.880.)then
           tempmin = -500.

! --- Look thru lowest 15 levels for lowest temp for
!        level to put marine cloud at.
           kisotherm = 20
           ktempmin = 20
           do k=min_cloud_lev_p+2,15
             if (p_bk(i,j,k)/100. .lt. 880.) go to 1101
               dth2dp2 = t_bk(i,j,k+1)+t_bk(i,j,k-1)-2.*t_bk(i,j,k)
               if (kisotherm.eq.0 .and.                         &
                   tbk_k(i,j,k).lt.tbk_k(i,j,k+1))        &
                  kisotherm = k
               if (dth2dp2.gt.tempmin) then
                 ktempmin = k
                 tempmin = max(dth2dp2,tempmin)
               end if
           end do
1101       continue
           ktempmin = min(ktempmin,kisotherm)
           sat_ctp(i,j) = p_bk(i,j,ktempmin)/100.
           npts_ctp_marine_remap = npts_ctp_marine_remap + 1
       else
           w_frac(i,j) = -99999.
           sat_tem(i,j) =  99999.
           sat_ctp(i,j) =  99999.
           nlev_cld(i,j) = -999
           npts_ctp_delete = npts_ctp_delete + 1
       end if
       end if
111   continue
   enddo  ! i
   enddo  ! j

   if(mype==0) then
   write(6,*) 'cloudCover_NESDIS: Flag CTP - unstable w/i 50mb of CTP, no=', npts_stab_flag
   write(6,*) 'cloudCover_NESDIS: Flag CTP - can''t remap CTP,         no=', npts_ctp_delete
   write(6,*) 'cloudCover_NESDIS: Flag CTP -remap marine cloud,        no=', npts_ctp_marine_remap
   endif

   if (npts_ctp_change.gt.0) then
   if(mype==0)  write (6,1121) npts_ctp_change, dctp/float(npts_ctp_change),  &
                dctpabs/float(npts_ctp_change)
1121    format (/'No. of pts w/ cloud-top pres change = ',i6          &
              /'Mean cloud-top pres change (old-new)= ',f8.1          &
              /'Mean abs cloud-top pres change      = ',f8.1/)
   end if

! --- Make sure that any cloud point has another cloud point nearby.
!       Otherwise, get rid of it.
   do 128 j=2,ny_p-1
   do 128 i=2,nx_p-1
      if (sat_ctp(i,j).lt. 1010. .and. sat_ctp(i,j) .gt.50.) then
          ibuddy = 0
          do j1=j-1,j+1
          do i1=i-1,i+1
            if (sat_ctp(i1,j1).lt.1010. .and. sat_ctp(i1,j1).gt.50.)  &
               ibuddy = 1
          end do
          end do
          if (ibuddy.eq.0) then
               w_frac(i,j) = -99999.
               sat_tem(i,j) =  99999.
               sat_ctp(i,j) =  99999.
               nlev_cld(i,j) = -999
               npts_ctp_nobuddy = npts_ctp_nobuddy + 1
          end if
      end if
      if (sat_ctp(i,j).gt.1010. .and. sat_ctp(i,j).lt.1100.) then
          ibuddy = 0
          do j1=j-1,j+1
          do i1=i-1,i+1
            if (sat_ctp(i1,j1).gt.1010. .and. sat_ctp(i1,j1).lt.1100.) &
               ibuddy = 1
          end do
          end do
          if (ibuddy.eq.0) then
               w_frac(i,j) = -99999.
               sat_tem(i,j) =  99999.
               sat_ctp(i,j) =  99999.
               nlev_cld(i,j) = -999
               npts_clr_nobuddy = npts_clr_nobuddy + 1
          end if
      end if
128 continue

   if(mype==0) then
    write(6,*) 'cloudCover_NESDIS: Flag CTP - no contiguous points also w/ cloud, no=',  &
           npts_ctp_nobuddy

    write(6,*) 'cloudCover_NESDIS: Flag CTP - no contiguous points also w/ clear, no=', &
           npts_clr_nobuddy
    endif

!
!     *****************************************************************
!     *****************************************************************
!        Start to adjust to GOES cloud top pressure
!     *****************************************************************
!     *****************************************************************
               
!     --- clear where GOES shows clear down to the surface
!            or down to the GOES cloud top level
               
! =============================================
! - clear down to surface in fully clear column (according to GOES)
! =============================================
!    Only trust 'clear' indication under following conditions
!        - over ocean
!        - or over land only if p<620 mb overnight
!        - or at any level in daytime (zenith angle
!                      greater than zen_limit threshold)
! =============================================
    do j=2,ny_p-1
    do i=2,nx_p-1
      if (sat_ctp(i,j).ge.1010.0 .and. sat_ctp(i,j).lt.1050.) then !clear
        do k=1,nztn_p
           if (csza(i,j).lt.zen_limit                        &
              .and. p_bk(i,j,k)/100..lt.co2_preslim_p      &
               .or. xland(i,j).eq.0.                         &
               .or. csza(i,j).ge.zen_limit) then
              cld_cover_3d(i,j,k) = 0
              wthr_type(i,j) = 0
           end if
        end do
      end if
    enddo
    enddo
! =============================================
! - clearing above cloud top
! =============================================

    do 119 j=2,ny_p-1
    do 119 i=2,nx_p-1
       do k=1,nztn_p-1
! - return to previous (but experimental) version - 12 Oct 04
         if (csza(i,j).lt.zen_limit                       &
             .and. p_bk(i,j,k)/100..lt.co2_preslim_p      &
              .or. xland(i,j).eq.0.                        &
              .or. csza(i,j).ge.zen_limit) then
! --- since we set GOES to nearest RUC level, only clear at least
!       1 RUC level above cloud top
           if (sat_ctp(i,j).lt.1010. .and. sat_ctp(i,j).gt.     &
               p_bk(i,j,k)/100.) cld_cover_3d(i,j,k+1) = 0
         end if
       end do
119 continue

! =============================================
! - start building where GOES indicates so
! =============================================
    do 120 j=2,ny_p-1
    do 120 i=2,nx_p-1

      if ((w_frac(i,j).ge. build_cloud_frac_p) .and. &
          (w_frac(i,j) .lt. 99999.) )then   !Dongsoo added

! --- Cloud info below MIN_CLOUD_P not reliable
          firstcloud = 0
! - pdiff (diff between sat cloud top and model sfc pres) in mb
          do k=nztn_p-1,min_cloud_lev_p,-1
!          do k=nztn_p,min_cloud_lev_p,-1
            pdiff = (sat_ctp(i,j)-p_bk(i,j,k)/100.)
! --- set closest RUC level w/ cloud
            if (pdiff.lt.0. .and. firstcloud.eq.0) then
              pdiffabove = sat_ctp(i,j)-p_bk(i,j,k+1)/100.
              if (abs(pdiffabove).lt.abs(pdiff)) then
                if (p_bk(i,j,k)/100..lt.co2_preslim_p    &
                    .or. xland(i,j).eq.0.                   &
                    .or. csza(i,j).ge.zen_limit) then
                       cld_cover_3d(i,j,k+1)=1
                       firstcloud = 1
                end if
              end if
            end if

!     no cloud above cloud top

!
! --- Add 50mb thick (at least 1 level) of cloud where GOES
!         indicates cloud top
            if (xland(i,j).ne.0.) then
              if (sat_ctp(i,j).lt. min_cloud_p_p .and.    &
                  pdiff.le.cloud_up_p  ) then
                if (firstcloud.eq.0.or. firstcloud.eq.1   &
                    .and.pdiff .ge. -1.*sat_cloud_pthick_p) then
                    if (p_bk(i,j,k)/100..lt.co2_preslim_p ) then
                          cld_cover_3d(i,j,k) = 1
                          firstcloud = 1
                    end if
                end if
              end if
            else
              if ( pdiff.le.cloud_up_p ) then
                 if (firstcloud.eq.0.or. firstcloud.eq.1    &
                     .and.pdiff .ge. -1.*sat_cloud_pthick_p) then
! xland ==0                   if (p_bk(i,j,k)/100..lt.co2_preslim_p) then
                       cld_cover_3d(i,j,k) = 1
                       firstcloud = 1
!                     end if
                   end if
              end if
            end if

          end do
        end if

112   continue

120   continue

! go from pa  to mb
   do k = 1,nztn_p
   do j = 2,ny_p-1
   do i = 2,nx_p-1
      p_bk(i,j,k)  = p_bk(i,j,k)/100.
   end do
   end do
   end do
!
END SUBROUTINE cloudCover_NESDIS

