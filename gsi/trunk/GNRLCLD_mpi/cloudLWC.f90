SUBROUTINE cloudLWC_stratform(nlat,nlon,nsig,q_bk,t_bk,p_bk,ges_qc,ges_qi, &
                 cld_cover_3d,cld_type_3d,wthr_type,cloudlayers_i,  &
                 cldwater_3d,cldice_3d)
!
!  find cloud liquid water content
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  cloudLWC_stratform  find cloud liquid water content
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-20
!
! ABSTRACT: 
!  This subroutine calculate liquid water content for stratform cloud
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     q_bk        - 3D moisture
!     t_bk        - 3D background potentional temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!     ges_qc      - 3D background cloud water mixing ratio (g/kg)
!     ges_qi      - 3D background cloud ice mixing ratio (g/kg)
!     cld_cover_3d- 3D cloud cover
!     cld_type_3d - 3D cloud type
!     wthr_type   - 3D weather type
!     cloudlayers_i - 3D cloud layer index
!
!   output argument list:
!     cldwater_3d - 3D cloud water mixing ratio (g/kg)
!     cldice_3d   - 3D cloud ice mixing ratio  (g/kg)
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
  use kinds, only: r_single,i_kind

  implicit none

  integer(i_kind),intent(in):: nlat,nlon,nsig
!
!  surface observation
!
!
!  background
!
  real(r_single),intent(in) :: t_bk(nlon,nlat,nsig)   ! temperature
  real(r_single),intent(inout) :: q_bk(nlon,nlat,nsig)   ! height
  real(r_single),intent(in) :: p_bk(nlon,nlat,nsig)   ! pressure
  real(r_single),intent(in) :: ges_qc(nlon,nlat,nsig)   ! pressure
  real(r_single),intent(in) :: ges_qi(nlon,nlat,nsig)   ! pressure
!
!
!  Variables for cloud analysis
!
  real (r_single),intent(in) :: cld_cover_3d(nlon,nlat,nsig)
  integer(i_kind),intent(in) :: cld_type_3d(nlon,nlat,nsig)
  integer(i_kind),intent(in) :: wthr_type(nlon,nlat)
!
!  cloud layers
!
  integer(i_kind),intent(in) :: cloudlayers_i(nlon,nlat,21)  ! 5 =different layers
!                                      1= the number of layers
!                                      2,4,... bottom
!                                      3,5,... top
!
! cloud water and cloud ice
!
  real (r_single),intent(out) :: cldwater_3d(nlon,nlat,nsig)
  real (r_single),intent(out) :: cldice_3d(nlon,nlat,nsig)
  real (r_single) :: cloudtmp_3d(nlon,nlat,nsig)
!-----------------------------------------------------------
!
! temp.
!
  INTEGER :: i,j,k,ilvl,nlvl
  INTEGER :: kb,kt,k1
  real (r_single) :: p_pa_1d(nsig)
  real :: cld_base_m, cld_top_m
  real :: cld_base_qc_m, cld_top_qc_m
  real (r_single) :: cloudqvis(nlon,nlat,nsig)

! --- Key parameters
!     Rh_clear_p        = 0.80          RH to use when clearing cloud
!     Cloud_q_qvis_rat_p= 0.10          Ratio of cloud water to water/ice

  real    Cloud_q_qvis_rat_p
  real    auto_conver
  real    cloud_def_p
  real    rh_cld3_p
  real    rh_clear_p
  data  Cloud_q_qvis_rat_p/ 0.05/
  data  auto_conver       /0.0002/
  data  cloud_def_p       /0.000001/
  data  rh_cld3_p    /0.8/
  data  rh_clear_p   /0.98/

  real es0_p
  parameter (es0_p=6.1121)     ! saturation vapor pressure (mb)
  real SVP1,SVP2,SVP3
  data SVP1,SVP2,SVP3/es0_p,17.67,29.65/

  REAL stab, stab_threshold
  LOGICAL :: l_prt
  INTEGER :: iflag_slwc

  REAL :: q, Temp, tv, evs, qvs1, eis, qvi1, watwgt, qavail
!
!====================================================================
!  Begin
!
  cldwater_3d=-99999.9
  cldice_3d=-99999.9
  cloudtmp_3d=-99999.9
!-----------------------------------------------------------------------
!
!  Find Cloud Layers and Computing Output Field(s)
!  The procedure works column by column.
!
!-----------------------------------------------------------------------
!
  DO j = 2,nlat-1
  DO i = 2,nlon-1
!
    DO k = 2,nsig-1
      p_pa_1d(k) = p_bk(i,j,k)*100.0
      q = q_bk(i,j,k)/(1.+q_bk(i,j,k))     !  Q = water vapor specific humidity
                                           !  q_bk = water vapor mixing ratio
      tv = t_bk(i,j,k)*(p_bk(i,j,k)/h1000)**rd_over_cp
! now, tmperature from GSI s potential temperature
      Temp = tv
! evs, eis in mb
      evs = svp1*exp(SVP2*(Temp-273.15)/(Temp-SVP3))
      qvs1 = 0.62198*evs*100./(p_pa_1d(k)-100.*evs)
      qvs1 = qvs1/(1.0-qvs1)
      eis = svp1 *exp(22.514 - 6.15e3/Temp)
      qvi1 = 0.62198*eis*100./(p_pa_1d(k)-100.*eis)
      qvi1 = qvi1/(1.0-qvi1)
      watwgt = max(0.,min(1.,(Temp-233.15)/(263.15-233.15)))
      cloudtmp_3d(i,j,k)= Temp
      cloudqvis(i,j,k)= (watwgt*qvs1 + (1.-watwgt)*qvi1)
!      qvis(i,j,k)= (watwgt*qvs1 + (1.-watwgt)*qvi1)
    enddo
  enddo
  enddo

  stab_threshold = 3./10000.
!  DO j = 174,174
!  DO i = 272,272
  DO j = 2,nlat-1
  DO i = 2,nlon-1
    nlvl=cloudlayers_i(i,j,1)
    if(nlvl > 0 ) then
      DO ilvl = 1, nlvl          ! loop through cloud layers
        kb=cloudlayers_i(i,j,2*ilvl)
        kt=cloudlayers_i(i,j,2*ilvl+1)
        DO k = kb,kt
          stab = (t_bk(i,j,k+1)-t_bk(i,j,k-1))/(p_pa_1d(k-1)-p_pa_1d(k+1))

! -- stability check.  Use 2K/100 mb above 600 mb and
!       3K/100mb below (nearer sfc)
          if ((stab .lt. stab_threshold .and. p_pa_1d(k)/100. .gt. 600.)   &
                   .or. stab .lt. 0.66*stab_threshold )  then
          else
!dk * we need to avoid adding cloud if sat_ctp is lower than 650mb
!            if (sumq(i,j,k) .lt. cloud_def_p) then
             qavail = min(0.5*auto_conver,cloud_q_qvis_rat_p*cloudqvis(i,j,k))
             Temp = cloudtmp_3d(i,j,k)
             watwgt = max(0.,min(1.,(Temp-233.15)/(263.15-233.15)))

!    -------------------------------------------------------------------
!   - set cloud water mixing ratio  - no more than 0.1 g/kg,
!      which is the current autoconversion mixing ratio set in exmoisg
!      according to John Brown - 14 May 99
!    -------------------------------------------------------------------
             cldwater_3d(i,j,k) = watwgt*qavail*1000.0   ! g/kg
!   - set ice mixing ratio
             cldice_3d(i,j,k)= (1.-watwgt)*qavail*1000.0   ! g/kg
!            end if
          end if
        enddo   ! k
      enddo   ! ilvl
    endif   !  nlvl > 1
  enddo
  enddo
!
! moisture adjustment absed on cloud
!
  DO j = 2,nlat-1
  DO i = 2,nlon-1
!
    DO k = 2,nsig-1
      p_pa_1d(k) = p_bk(i,j,k)*100.0
!
      if(cld_cover_3d(i,j,k) <= 0.0 ) then
! adjust RH to be below 85 percent(50%?) if
!     1) cloudyn = 0
!     2) at least 100 mb above sfc
!     3) no precip from sfc obs
!make sure that clear volumes are no more than rh_clear_p RH.
        if( (ges_qc(i,j,k)+ges_qi(i,j,k)) .gt. 0.0 .and.   &
            (p_pa_1d(1) - p_pa_1d(k)) .gt. 10000. .and.  &
             wthr_type(i,j) <=0 ) then 
              q_bk(i,j,k) = min(q_bk(i,j,k), cloudqvis(i,j,k) * rh_clear_p)
        endif
!C  - moisten layers above and below cloud layer
        if(cld_cover_3d(i,j,k+1) > 0.6 .or. cld_cover_3d(i,j,k-1) > 0.6  ) then
                q_bk(i,j,k) = q_bk(i,j,k) +            &
                     0.7*(max(0.0, cloudqvis(i,j,k)-q_bk(i,j,k)))
        endif
! -- If SCT/FEW present, reduce RH only down to rh_cld3_p (0.98)
!         corresponding with cloudyn=3
      elseif(cld_cover_3d(i,j,k) > 0.0 .and. cld_cover_3d(i,j,k) < 0.6 ) then
        q_bk(i,j,k) = min(q_bk(i,j,k), cloudqvis(i,j,k) * rh_cld3_p)
!
      else   ! set qv at 102%RH
        q_bk(i,j,k) = max(q_bk(i,j,k), cloudqvis(i,j,k) * 1.02)
      endif
    enddo

  enddo
  enddo

END SUBROUTINE cloudLWC_stratform

SUBROUTINE cloudLWC_Cumulus(nlat,nlon,nsig,h_bk,t_bk,p_bk,                         &
                 cld_cover_3d,cld_type_3d,wthr_type,cloudlayers_i,  &
                 cldwater_3d,cldice_3d,cloudtmp_3d)
!
!  find cloud liquid water content
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  cloudLWC_stratform  find cloud liquid water content
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-20
!
! ABSTRACT: 
!  This subroutine calculate liquid water content for cumulus cloud
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     h_bk        - 3D height
!     t_bk        - 3D background potentional temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!     cld_cover_3d- 3D cloud cover
!     cld_type_3d - 3D cloud type
!     wthr_type   - 3D weather type
!     cloudlayers_i - 3D cloud layer index
!
!   output argument list:
!     cldwater_3d - 3D cloud water mixing ratio (g/kg)
!     cldice_3d   - 3D cloud ice mixing ratio  (g/kg)
!     cloudtmp_3d - 3D cloud temperature
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
  use kinds, only: r_single,i_kind

  implicit none
  integer(i_kind),intent(in) :: nlat,nlon,nsig
!
!  surface observation
!
!
!  background
!
  real(r_single),intent(in) :: t_bk(nlon,nlat,nsig)   ! temperature
  real(r_single),intent(in) :: h_bk(nlon,nlat,nsig)   ! height
  real(r_single),intent(in) :: p_bk(nlon,nlat,nsig)   ! pressure
!
!
!  Variables for cloud analysis
!
  real (r_single),intent(in) :: cld_cover_3d(nlon,nlat,nsig)
  integer(i_kind),intent(in) :: cld_type_3d(nlon,nlat,nsig)
  integer(i_kind),intent(in) :: wthr_type(nlon,nlat)
!
!  cloud layers
!
  integer(i_kind),intent(in) :: cloudlayers_i(nlon,nlat,21)  ! 5 =different layers
!                                      1= the number of layers
!                                      2,4,... bottom
!                                      3,5,... top
!
! cloud water and cloud ice
!
  real (r_single),intent(out) :: cldwater_3d(nlon,nlat,nsig)
  real (r_single),intent(out) :: cldice_3d(nlon,nlat,nsig)
  real (r_single),intent(out) :: cloudtmp_3d(nlon,nlat,nsig)
!-----------------------------------------------------------
!
! temp.
!
  INTEGER :: i,j,k,ilvl,nlvl
  INTEGER :: kb,kt,k1
  real (r_single) :: zs_1d(nsig)
  real (r_single) :: cv_1d(nsig)
  real (r_single) :: t_1d(nsig)
  real (r_single) :: p_pa_1d(nsig)
  real (r_single) :: p_mb_1d(nsig)
  real :: cld_base_m, cld_top_m
  real :: cld_base_qc_m, cld_top_qc_m

  real (r_single) :: slwc_1d(nsig)
  real (r_single) :: cice_1d(nsig)
  real (r_single) :: ctmp_1d(nsig)

  LOGICAL :: l_prt
  INTEGER :: iflag_slwc
!
!====================================================================
!  Begin
!
  l_prt =.false.
  iflag_slwc = 11
  cldwater_3d=-99999.9
  cldice_3d=-99999.9
  cloudtmp_3d=-99999.9
!-----------------------------------------------------------------------
!
!  Find Cloud Layers and Computing Output Field(s)
!  The procedure works column by column.
!
!-----------------------------------------------------------------------
!

!  DO j = 200,200
!  DO i = 300,300
  DO j = 2,nlat-1
  DO i = 2,nlon-1
!
    DO k = 1,nsig                      ! Initialize
      t_1d(k) = t_bk(i,j,k)*(p_bk(i,j,k)/h1000)**rd_over_cp
      zs_1d(k) = h_bk(i,j,k)
      p_pa_1d(k) = p_bk(i,j,k)*100.0
      p_mb_1d(k) = p_bk(i,j,k)
    END DO
!-----------------------------------------------------------------------
    nlvl=cloudlayers_i(i,j,1)
    if(nlvl > 0 ) then
      DO ilvl = 1, nlvl          ! loop through cloud layers

        kb=cloudlayers_i(i,j,2*ilvl)
        kt=cloudlayers_i(i,j,2*ilvl+1)

        cld_base_m = 0.5 * (zs_1d(kb-1) + zs_1d(kb))
        cld_top_m = 0.5 * (zs_1d(kt) + zs_1d(kt+1))
!
        IF(iflag_slwc /= 0) THEN
           IF(iflag_slwc < 10) THEN ! simple adiabatc scheme
                CALL get_slwc1d (nsig,cld_base_m,cld_top_m,kb,kt      &
                       ,zs_1d,t_1d,p_pa_1d,iflag_slwc,slwc_1d)

           ELSE ! iflag_slwc > 10, new Smith-Feddes scheme
                DO k1 = 1,nsig ! Initialize
                   slwc_1d(k1) = 0.0
                   cice_1d(k1) = 0.0
                   ctmp_1d(k1) = t_bk(i,j,k1)
                END DO
!
!-----------------------------------------------------------------------
!
!  QC the data going into SMF
!
!-----------------------------------------------------------------------
!
                IF(cld_top_m > zs_1d(nsig-1) - 110.) THEN
                   cld_top_qc_m = zs_1d(nsig-1) - 110.
                   cld_base_qc_m =                                   &
                              MIN(cld_base_m,cld_top_qc_m - 110.)
                ELSE ! normal case
                   cld_top_qc_m = cld_top_m
                   cld_base_qc_m = cld_base_m
                END IF
!
                CALL get_sfm_1d(nsig,cld_base_qc_m,cld_top_qc_m       &
                                ,zs_1d,p_mb_1d,t_1d                &
                                ,slwc_1d,cice_1d,ctmp_1d,l_prt)
!
           END IF ! iflag_slwc < 10
        END IF ! iflag_slwc .ne. 0
!
        DO k1 = kb,kt ! Loop through the cloud layer
           IF(iflag_slwc /= 0) THEN
             IF(slwc_1d(k1) > 0.) cldwater_3d(i,j,k1)=slwc_1d(k1)
             IF(cice_1d(k1) > 0.) cldice_3d(i,j,k1)=cice_1d(k1)
             cloudtmp_3d(i,j,k1)=ctmp_1d(k1)
           END IF ! iflag_slwc .ne. 0
        END DO ! k1

      enddo   ! ilvl

    endif

  ENDDO  ! i
  ENDDO  ! j

END SUBROUTINE cloudLWC_Cumulus
