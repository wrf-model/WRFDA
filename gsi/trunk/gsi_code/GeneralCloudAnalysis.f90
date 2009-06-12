SUBROUTINE  GeneralCloudAnalysis(mype)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  GeneralCloudAnalysis      drive of generalized cloud analysis
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-27
!
! ABSTRACT: 
!  This subroutine serves as a drive for generalized cloud analysis
!
! PROGRAM HISTORY LOG:
!    2008-12-20  Hu  Add NCO document block
!
!
!   input argument list:
!     mype     - processor ID that does this IO
!
!   output argument list:
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
! 
  use constants, only: rd, grav, half, deg2rad, pi,rad2deg
  use constants, only: zero,one,two,izero,ione
  use kinds, only: r_single,i_kind, r_kind
  use gridmod, only: pt_ll,eta1_ll,aeta1_ll, &
                  nlat,nlon,nsig,rlats,rlons
  use gridmod, only: regional, wrf_mass_regional
  use gridmod, only: regional_time
  use gridmod, only: lat2,lon2,nlat_regional,nlon_regional
  use obsmod, only: obs_setup,nsat1,ndat,dtype
  use guess_grids, only: ges_q,ges_qc,ges_qi,ges_qr,ges_qs,ges_qh
  use mpimod, only: mpi_comm_world,ierror

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype
!
! background
!
  real(r_single),allocatable:: t_bk(:,:,:)
  real(r_single),allocatable:: h_bk(:,:,:)
  real(r_single),allocatable:: p_bk(:,:,:)
  real(r_single),allocatable:: ps_bk(:,:)
  real(r_single),allocatable:: zh(:,:)
  real(r_single),allocatable:: q_bk(:,:,:)

  real(r_single),allocatable:: xlon(:,:)
  real(r_single),allocatable:: xlat(:,:)
  real(r_single),allocatable:: gsfc(:,:,:)
  real(r_single),allocatable:: xland(:,:)
  real(r_single),allocatable:: soiltbk(:,:)
  real(r_single),allocatable:: z_lcl(:,:)
  real(r_single),allocatable:: w_bk(:,:,:)
!
!  surface observation
!
  INTEGER(i_kind) :: NVARCLD_P
  PARAMETER (NVARCLD_P=13)

  INTEGER(i_kind) :: numsao
  real(r_single),allocatable  :: OI(:)
  real(r_single),allocatable  :: OJ(:)
  INTEGER(i_kind),allocatable :: OCLD(:,:)
  CHARACTER*10,allocatable    :: OWX(:)
  real(r_single),allocatable  :: Oelvtn(:)

!
!  lightning observation
!
  real(r_single),allocatable:: lightning(:,:)

!
!  radar observation
!
  real(r_kind),allocatable :: ref_mos_3d(:,:,:)
  real(r_kind),allocatable :: ref_mosaic31(:,:,:)
  INTEGER(i_kind) :: Nmsclvl_radar 
!
!  NESDIS cloud products
!
  real(r_single),allocatable:: sat_ctp(:,:)
  real(r_single),allocatable:: sat_tem(:,:)
  real(r_single),allocatable:: w_frac(:,:)
  integer(i_kind),allocatable:: nlev_cld(:,:)
!
!  grid
!
  real(r_single) :: dx,dy
!
! analysis variables
!
!  real,allocatable :: w_cld(:,:,:)
  REAL(r_single),   allocatable :: cld_cover_3d(:,:,:)  ! cloud cover
  INTEGER(i_kind),allocatable :: cld_type_3d(:,:,:)   ! cloud type
  INTEGER(i_kind),allocatable :: pcp_type_3d(:,:,:)   ! precipitation type
  INTEGER(i_kind),allocatable :: wthr_type_2d(:,:)   ! weather type type
!  INTEGER,allocatable :: icing_index_3d(:,:,:)
!  LOGICAL,allocatable :: l_mask_pcptype(:,:)
  integer(i_kind),allocatable :: cloudlayers_i(:,:,:)  
!                                      5 different layers
!                                      1= the number of layers
!                                      2,4,... bottom
!                                      3,5,... top
!
  REAL(r_single),allocatable :: cldwater_3d(:,:,:)  ! cloud water
  REAL(r_single),allocatable :: cldice_3d(:,:,:)  ! cloud ice
  REAL(r_single),allocatable :: rain_3d(:,:,:)  ! rain
  REAL(r_single),allocatable :: snow_3d(:,:,:)  ! snow
  REAL(r_single),allocatable :: hail_3d(:,:,:)  ! hail
  REAL(r_single),allocatable :: cldtmp_3d(:,:,:)  ! cloud temperature

  REAL(r_kind)    :: thunderRadius=2.5_r_kind
  INTEGER(i_kind) :: i_radius=10_i_kind
  INTEGER(i_kind) :: miss_obs_int
  REAL(r_kind)    :: miss_obs_real
  PARAMETER ( miss_obs_int = -99999_i_kind  )
  PARAMETER ( miss_obs_real = -99999.0_r_kind )

  REAL(r_kind)    :: Cloud_def_p
  data  Cloud_def_p       / 0.000001_r_kind/
  REAL(r_kind),allocatable :: sumq(:,:,:)  ! total liquid water
!
! option
!
  INTEGER(i_kind) :: cldLWopt    ! cloud water retrieve
  INTEGER(i_kind) :: cldqropt    ! precipitation retrieve
  INTEGER(i_kind) :: cldptopt    ! temperature adjustment scheme
  INTEGER(i_kind) :: istat_Surface,istat_NESDIS,istat_radar, istat_lightning
!
!  misc.
!
  INTEGER(i_kind) :: i,j,k
  REAL(r_kind) :: qrlimit
  character(10) :: obstype
  integer(i_kind) :: lunin, is
  integer(i_kind):: nreal,nchanl,ilat1s,ilon1s
  character(20) :: isis
!
!
  if(mype==0) then
  write(6,*) '========================================'
  write(6,*) 'GeneralCloudAnalysis: Start to do generalized cloud analysis '
  write(6,*) 'GeneralCloudAnalysis: test svn 2'
  write(6,*) 'GeneralCloudAnalysis: test svn 3'
  write(6,*) '========================================'
  endif

!
!  dx=9000.0
!  dy=9000.0
  dx=13545.09_r_kind
  dy=13545.09_r_kind
  cldqropt=2    ! 1=Kessler 2=Lin 3=Thompson
  cldptopt=3    ! 3=latent heat, 4,5,6 = adiabat profile
  cldLWopt=1    ! 1 = RUC layer saturation and autoconvert
                ! 2 = convective 
  
  istat_Surface=izero
  istat_NESDIS=izero
  istat_radar=izero
  istat_lightning=izero
!
!  check the consistence of the options
!

! Now either stratiform or cumulus cloud is considered in the cloud
! water calculation. This leads to a limitation for the temperature
! adjustment when stratiform cloud is choiced because adiabat profile
! scheme based on the convection. This limitation may change when 
! stratiform and cumulus cloud are considered at the same time in the future.
  if(cldLWopt == ione .and. cldptopt >= 4) then
    write(6,*) 'GeneralCloudAnalysis: unconsistent option settings for cldLWopt and cldptopt'
    write(6,*) 'GeneralCloudAnalysis: cldptopt much be set to 3 when cldLWopt =1'
    call stop2(113)
  endif
!
!----------------------------------------------
! 1. prepare background and observations  
!----------------------------------------------
!
! 1.2   background fields
!
  allocate(t_bk(lon2,lat2,nsig))
  allocate(h_bk(lon2,lat2,nsig))
  allocate(p_bk(lon2,lat2,nsig))
  allocate(ps_bk(lon2,lat2))
  allocate(zh(lon2,lat2))
  allocate(q_bk(lon2,lat2,nsig))
  allocate(w_bk(lon2,lat2,nsig))
  allocate(sumq(lon2,lat2,nsig))

  allocate(xlon(lon2,lat2))
  allocate(xlat(lon2,lat2))
  allocate(xland(lon2,lat2))
  allocate(soiltbk(lon2,lat2))
  allocate(z_lcl(lon2,lat2))

  allocate(cldwater_3d(lon2,lat2,nsig))
  allocate(cldice_3d(lon2,lat2,nsig))
  allocate(rain_3d(lon2,lat2,nsig))
  allocate(snow_3d(lon2,lat2,nsig))
  allocate(hail_3d(lon2,lat2,nsig))
  allocate(cldtmp_3d(lon2,lat2,nsig))
  cldwater_3d=miss_obs_real
  cldice_3d=miss_obs_real
  rain_3d=miss_obs_real
  snow_3d=miss_obs_real
  hail_3d=miss_obs_real
  cldtmp_3d=miss_obs_real

  allocate(ref_mos_3d(lon2,lat2,nsig))
  ref_mos_3d=miss_obs_real

  allocate(lightning(lon2,lat2))
  lightning=-9999.0_r_kind
!
!!read in background fields
!
  call BackgroundCld(mype,t_bk,p_bk,ps_bk, w_bk,     &
             q_bk,xlon,xlat,h_bk,zh,xland,soiltbk,z_lcl)
!  call BackgroundCld(mype,t_bk,p_bk,ps_bk, w_bk,     &
!             q_bk,qr_bk,qs_bk,qg_bk,qc_bk,qi_bk,     &
!             xlon,xlat,h_bk,zh,xland,soiltbk,z_lcl)
!
!
! read observations
!
  Nmsclvl_radar = -999_i_kind
  lunin=55
  open(lunin,file=obs_setup,form='unformatted')
  rewind lunin
  do is=1,ndat

    if(dtype(is) /= ' ' )then
    if(nsat1(is) > izero )then
!mhu this is a problem need to be fixed later    if(nsat1(is) > 0 )then
!
!  1.2 read in surface observations
!
       if( dtype(is).eq.'mta_cld' ) then
           numsao=nsat1(is) 
           allocate(OI(numsao))
           allocate(OJ(numsao))
           allocate(OCLD(NVARCLD_P,numsao))
           allocate(OWX(numsao))
           allocate(Oelvtn(numsao))
           call read_Surface(lunin,numsao,NVARCLD_P,numsao,OI,OJ,OCLD,OWX,Oelvtn)
           if(mype == 0) write(6,*) 'GeneralCloudAnalysis: Surface cloud observations are read in sucessfully'
           istat_Surface=ione
           nsat1(is) = izero

       elseif( dtype(is).eq.'gos_ctp' ) then 

!
!  1.4 read in NESDIS cloud products
!
           allocate(sat_ctp(lon2,lat2))
           allocate(sat_tem(lon2,lat2))
           allocate(w_frac(lon2,lat2))
           allocate(nlev_cld(lon2,lat2))
           sat_ctp=miss_obs_real
           sat_tem=miss_obs_real
           w_frac=miss_obs_real
           nlev_cld=miss_obs_int

           call read_NESDIS(lunin,nsat1(is),lon2,lat2,sat_ctp,sat_tem,w_frac,nlev_cld,mype)
           if(mype == 0) write(6,*) 'GeneralCloudAnalysis: NESDIS cloud products are read in sucessfully'
           istat_NESDIS = ione 
           nsat1(is) = izero

       elseif( dtype(is).eq.'rad_ref' ) then
!
!   1.6 read in reflectivity mosaic
!
           allocate( ref_mosaic31(lon2,lat2,31) )
           ref_mosaic31=-9999.0_r_kind

           call read_NSSL_ref(mype,lunin,lon2,lat2,Nmsclvl_radar,nsat1(is),ref_mosaic31)
           if(mype == 0) write(6,*) 'GeneralCloudAnalysis: NSSL radar reflectivity is read in sucessfully'
           istat_radar=ione
           nsat1(is) = izero

       elseif( dtype(is).eq.'lghtn' ) then
!
!   1.8 read in lightning
!
           call read_Lightning2cld(mype,lunin,lon2,lat2,nsat1(is),lightning)
           if(mype == 0) write(6,*) 'GeneralCloudAnalysis: Lightning is read in sucessfully'
           istat_lightning = ione 
           nsat1(is) = izero

       else
!
!  all other observations 
!
           read(lunin)  obstype,isis,nreal,nchanl
           read(lunin)
       endif   ! dtype

    endif
    endif
  enddo   ! is
  close(lunin)
!
!  check if data available 
!
  if( (istat_radar + istat_Surface + istat_NESDIS + istat_lightning ) == 0 ) then
!    write(6,*) ' No cloud observations available, return'
    return
  endif

!
!  vertical interpolation of radar reflectivity
!
   if(istat_radar ==  ione ) then
       call interp_NSSL_ref(mype,lon2,lat2,nsig,Nmsclvl_radar,ref_mos_3d,ref_mosaic31,h_bk,zh)
       deallocate( ref_mosaic31 )
       call build_missing_REFcone(mype,lon2,lat2,nsig,ref_mos_3d,h_bk)
   endif
!
!  convert lightning to reflectivity 
!
   if(istat_lightning ==  ione ) then
       call convert_lghtn2ref(mype,lon2,lat2,nsig,ref_mos_3d,lightning,h_bk)
   endif
!
!
!----------------------------------------------
! 2.  decide cloud cover, cloud type, precipitation type 
!----------------------------------------------
  allocate(cld_cover_3d(lon2,lat2,nsig))
  allocate(cld_type_3d(lon2,lat2,nsig))
  allocate(wthr_type_2d(lon2,lat2))
  allocate(pcp_type_3d(lon2,lat2,nsig))
  cld_cover_3d=miss_obs_real
  cld_type_3d =miss_obs_int
  wthr_type_2d =miss_obs_int
  pcp_type_3d=miss_obs_int
!
!
  if(istat_Surface ==  ione) then
    call cloudCover_surface(mype,lat2,lon2,nsig,i_radius,thunderRadius,  &
             t_bk,p_bk,q_bk,h_bk,zh,           &
             numsao,NVARCLD_P,numsao,OI,OJ,OCLD,OWX,Oelvtn,&
             cld_cover_3d,cld_type_3d,wthr_type_2d,pcp_type_3d)
    if(mype==0) write(6,*) 'GeneralCloudAnalysis: success in cloud cover analysis using surface data'
  endif

  if(istat_NESDIS == ione ) then
    call cloudCover_NESDIS(mype,lat2,lon2,nsig,xlon,xlat,t_bk,p_bk,h_bk,zh,xland, &
                        soiltbk,q_bk,sat_ctp,sat_tem,w_frac,nlev_cld, &
                        cld_cover_3d,cld_type_3d,wthr_type_2d)
    if(mype==0) write(6,*) 'GeneralCloudAnalysis: success in cloud cover analysis using NESDIS data'
  endif

  if(istat_radar == ione .or. istat_lightning == ione ) then
    call cloudCover_radar(mype,lat2,lon2,nsig,h_bk,zh,z_lcl,ref_mos_3d,        &
                          cld_cover_3d,cld_type_3d,wthr_type_2d)
    if(mype==0) write(6,*) 'GeneralCloudAnalysis: success in cloud cover analysis using radar data'
  endif

!  radar temperature tendency  for DFI
  if(istat_radar == ione .or. istat_lightning == ione ) then
    if (istat_NESDIS==ione) then
      call NSSL_ref2tten(mype,istat_radar,lon2,lat2,nsig,ref_mos_3d,cld_cover_3d, &
                        p_bk,t_bk,sat_ctp)
    else
      call NSSL_ref2tten_nosat(mype,istat_radar,lon2,lat2,nsig,ref_mos_3d,cld_cover_3d,p_bk,t_bk)
    endif
  endif

! find the cloud layers
  allocate(cloudlayers_i(lon2,lat2,21))

  call cloudLayers(lat2,lon2,nsig,h_bk,zh,z_lcl,cld_cover_3d,cld_type_3d,wthr_type_2d,  &
                   cloudlayers_i)
  if(mype==0) write(6,*) 'GeneralCloudAnalysis: success in finding cloud layers'

! decide the cloud type
  call cloudType(lat2,lon2,nsig,h_bk,t_bk,p_bk,ref_mos_3d,          &
                 cld_cover_3d,cld_type_3d,wthr_type_2d,cloudlayers_i)
  if(mype==0)  write(6,*) 'GeneralCloudAnalysis: success in deciding cloud types'

! calculate liquid water content
  if(cldLWopt == ione ) then
    call cloudLWC_stratform(lat2,lon2,nsig,q_bk,t_bk,p_bk,            &
                 ges_qc(:,:,:,1),ges_qi(:,:,:,1),                     &
                 cld_cover_3d,cld_type_3d,wthr_type_2d,cloudlayers_i, &
                 cldwater_3d,cldice_3d)
    if(mype==0) write(6,*) 'GeneralCloudAnalysis: success in claculate cloud liquid water content'

  elseif (cldLWopt == 2) then
    call cloudLWC_Cumulus(lat2,lon2,nsig,h_bk,t_bk,p_bk,              &
                 cld_cover_3d,cld_type_3d,wthr_type_2d,cloudlayers_i, &
                 cldwater_3d,cldice_3d,cldtmp_3d)
    if(mype==0) write(6,*) 'GeneralCloudAnalysis: success in claculate cloud liquid water content'
  else
    write(6,*)'GeneralCloudAnalysis: Invalid cloud water calculation option, check cldLWopt'
    call stop2(113)
  endif

  if(istat_radar == ione .or. istat_lightning == ione) then
    call PrecipType(lat2,lon2,nsig,t_bk,p_bk,q_bk,ref_mos_3d, &
                   wthr_type_2d,pcp_type_3d)
    if(mype==0) write(6,*) 'GeneralCloudAnalysis: success in deciding precipitation type'

    call PrecipMxR_radar(mype,lat2,lon2,nsig,t_bk,p_bk,ref_mos_3d,pcp_type_3d, &
                 rain_3d,snow_3d,hail_3d,cldqropt)
    if(mype==0) write(6,*) 'GeneralCloudAnalysis: success in deciding precipitation mixing ratio'
  endif
!
!  temperature adjustment
!
  call TempAdjust(mype,lat2,lon2,nsig,cldptopt, t_bk, p_bk, w_bk, q_bk, &
                   cldwater_3d,cldice_3d,cldtmp_3d)

!!
!  the final analysis or update background
!
  qrlimit=15.0_r_kind
  DO k=1,nsig
    DO j=2,lat2-1
      DO i=2,lon2-1
        sumq(i,j,k)= ges_qc(j,i,k,1) + ges_qi(j,i,k,1)
!                     ges_qr(j,i,k,1) + ges_qs(j,i,k,1) + ges_qh(j,i,k,1)
        if( cld_cover_3d(i,j,k) > -0.001_r_kind ) then 
           if( cld_cover_3d(i,j,k) > 0.6_r_kind ) then 
             if( sumq(i,j,k) .lt. cloud_def_p) then
               cldwater_3d(i,j,k) = max(0.001_r_kind*cldwater_3d(i,j,k),zero)
               cldice_3d(i,j,k) = max(0.001_r_kind*cldice_3d(i,j,k),zero)
             else   ! using background value
               cldwater_3d(i,j,k) = ges_qc(j,i,k,1)
               cldice_3d(i,j,k) = ges_qi(j,i,k,1)
             endif
           else   ! clean  cloud
             cldwater_3d(i,j,k) = zero
             cldice_3d(i,j,k) = zero
           endif
        else   ! unknown, using background values
          cldwater_3d(i,j,k) = ges_qc(j,i,k,1)
          cldice_3d(i,j,k) = ges_qi(j,i,k,1)
        endif
        if(ref_mos_3d(i,j,k) > zero ) then
          rain_3d(i,j,k) = MIN(max(0.001_r_kind*rain_3d(i,j,k),0.0),qrlimit)
          snow_3d(i,j,k) = MIN(max(0.001_r_kind*snow_3d(i,j,k),0.0),qrlimit)
          hail_3d(i,j,k) = MIN(max(0.001_r_kind*hail_3d(i,j,k),0.0),qrlimit)
        elseif(ref_mos_3d(i,j,k) <= zero .and. ref_mos_3d(i,j,k) > -100.0_r_kind ) then
          rain_3d(i,j,k) = zero
          snow_3d(i,j,k) = zero
          hail_3d(i,j,k) = zero
        else
          rain_3d(i,j,k) = ges_qr(j,i,k,1)
          snow_3d(i,j,k) = ges_qs(j,i,k,1)
          hail_3d(i,j,k) = ges_qh(j,i,k,1)
        endif
      END DO
    END DO
  END DO
!
!  check : need to be deleted later
!  hail_3d=cld_cover_3d
!  DO k=1,nsig-1
!    DO j=2,lat2-1
!      DO i=2,lon2-1
!         if(hail_3d(i,j,nsig) < hail_3d(i,j,k)) then
!           hail_3d(i,j,nsig) = hail_3d(i,j,k)
!         endif
!      END DO
!    END DO
!  END DO
!  hail_3d=ref_mos_3d
!

  deallocate(cloudlayers_i)

  deallocate(cld_cover_3d)
  deallocate(cld_type_3d)
  deallocate(wthr_type_2d)

  call save_cloudResults(mype,lat2,lon2,nsig,q_bk,rain_3d,snow_3d,hail_3d, &
                         cldwater_3d,cldice_3d,t_bk,p_bk)

! shoud adjsut q_bk somewhere

  deallocate(t_bk)
  deallocate(h_bk)
  deallocate(p_bk)
  deallocate(ps_bk)
  deallocate(zh)

  deallocate(xlon)
  deallocate(xlat)
  deallocate(xland)
  deallocate(soiltbk)

!  deallocate(qr_bk)
!  deallocate(qs_bk)
!  deallocate(qg_bk)
!  deallocate(qc_bk)
!  deallocate(qi_bk)

  deallocate(q_bk)
  deallocate(cldwater_3d)
  deallocate(cldice_3d)
  deallocate(rain_3d)
  deallocate(snow_3d)
  deallocate(hail_3d)
  deallocate(cldtmp_3d)

  if(istat_Surface ==  ione ) then
    deallocate(OI)
    deallocate(OJ)
    deallocate(OCLD)
    deallocate(OWX)
    deallocate(Oelvtn)
  endif
  if(istat_NESDIS == ione ) then
    deallocate(sat_ctp)
    deallocate(sat_tem)
    deallocate(w_frac)
    deallocate(nlev_cld)
  endif
  deallocate(ref_mos_3d)
  deallocate(pcp_type_3d)
  deallocate(lightning)

  if(mype==0) then
  write(6,*) '========================================'
  write(6,*) 'GeneralCloudAnalysis: generalized cloud analysis finished:',mype
  write(6,*) '========================================'
  endif

END SUBROUTINE GeneralCloudAnalysis
