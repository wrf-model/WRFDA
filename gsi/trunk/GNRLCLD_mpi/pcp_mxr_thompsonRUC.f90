!**********************************************************************c
 SUBROUTINE pcp_mxr_thompsonRUC(QR,QS,qg,       &
           p, tb,                      &
           refl3,nx_p,ny_p,nztn_p,cldpcp_type_3d)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  pcp_mxr calculate precipiation based on thompson microphysics scheme
!
!   PRGMMR: Ming Hu          ORG:  GSD/AMB        DATE:    2007-04-04
!
! ABSTRACT: 
!  This subroutine calculate precipiation based on thompson microphysics scheme
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     p        - 3D background pressure  (hPa)
!     tb       - 3D background potentional temperature (K)
!     refl3    - 3D reflectiviy in analysis grid (dBZ)
!     nx_p     - no. of lons on subdomain (buffer points on ends)
!     ny_p     - no. of lats on subdomain (buffer points on ends)
!     nztn_p   - no. of levels
!     cldpcp_type_3d - 3D precipitation  type
!
!   output argument list:
!     QR          - rain mixing ration (g/kg)
!     QS          - snow mixing ration (g/kg)
!     QG          - hail mixing ration (g/kg)
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!   Old document from CAPS
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! SUBPROGRAM:    CALREFL      CALCULATE Reflectivity
!
!   PRGMMR:  BENJAMIN, STAN ORG: NOAA/ESRL     DATE: 2007-02-10
!
! ABSTRACT:
!
!  Perform 3D precipitation mixing ratio (in g/kg) analysis using
!  radar reflectivity data. For rain water, using Thompson 2004
!  formulation:
!
!    from qr, qs, and qg.  
!   qr--rain water mixing ratio  (kg/kg)
!   qs--snow mixing ratio        (kg/kg)
!   qg--graupel mixing ratio     (kg/kg)

! HISTORY
! PROGRAM HISTORY LOG:
!                        
!     2007-04-04      Ming Hu
!                           
!-----------------------------------------------------------------------
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
! 

!
      use kinds, only: r_single,i_kind, r_kind
      IMPLICIT   NONE
!
!----------------------------------------------------
!   meteorological constant from METCON
!----------------------------------------------------
!
      REAL        PI_P
      REAL        R_P

      PARAMETER ( PI_P     = 3.14159265  )
      PARAMETER ( R_P     =  8.31451     )
!C   --------------------------------------------------------------------
!C   DRY AIR CONSTANTS
!C   --------------------------------------------------------------------

      REAL        MD_P
      REAL        RD_P
      PARAMETER ( MD_P     =  0.0289645          )
      PARAMETER ( RD_P     =  R_P/MD_P           )
!
!----------------------------------------------------
!   microphysics variables from PMICRPH that computered by paramr.f
!----------------------------------------------------
      REAL  PI
      REAL  R1
      REAL  RON, RON2, SON, GON
      
      REAL RON_min,const1r,const2r,qr0,delqr0
      REAL DRAIN,DSNOW,DGRAUPEL
      REAL constd,constgb,const_ng1,const_ng2
!
!
!----------------------------------------------------
!

      integer(i_kind),intent(in):: nx_p,ny_p,nztn_p

      real(r_single),intent(in) ::  p (NX_P,NY_P,nztn_p) 
      real(r_single),intent(in) :: tb(NX_P,NY_P,nztn_p)  
      REAL ::   ref_conv
      REAL(r_kind),intent(in) :: refl3(NX_P,NY_P,nztn_p)  

      REAL(r_single),intent(out) ::     &
          QR(NX_P,NY_P,nztn_p),  &
          QS(NX_P,NY_P,nztn_p),  &
          QG(NX_P,NY_P,nztn_p)   

      integer(i_kind),intent(in) ::  cldpcp_type_3d(NX_P,NY_P,nztn_p)

      integer i,j,k

      real refl_wat, refl_sno, refl_hail

      real qrain, qsnow, qgraupel

      REAL rain,ronv,snow,rhoqs,temp_c,sonv
      REAL graupel
      real alpha, rhod, bb
      real ze_s, ze_r, ze_g, ze_nc
 
      real Arain, Asnow, Ahail
      real Brain, Bsnow, Bhail
      real Hail_min, Hail_max, Hail_cfa, Hail_cfb

      real thresh_ref
      integer :: pcptype
      real :: tc, rfract

!
!  look up table for rain retrieval
!
      INTEGER :: num_tbl
      parameter ( num_tbl = 1000 )
      real :: tb_rain(num_tbl)
      real :: tb_ze_r(num_tbl)   ! note: rain reflectivity / air density ** 1.75
      real :: range_up, range_down, drange
 
      INTEGER :: ii
      
!
!  check
!
   real :: rmax
   integer :: imax,jmax,kmax
  
!------------------------------------------------------------------
      thresh_ref=10.0
!------------------------------------------------------------------

      PI = PI_P
!    Min value for hydrometeor mixing ratios
      R1 = 1.E-15

! SLOPE INTERCEPT FOR RAIN, SNOW, AND GRAUPEL

!jmb--Roy R. suggests a larger value for the slope-intercept for rain.
!      This will slow down the fall speed.--16dec98
      RON=8.E6          ! Original M-P value.
!gt   RON2=1.E10        ! GREG T.  MWR Part1 vrbl intercept
      RON2=1.E9         ! GREG T.  changed 01 Dec 2004
!     SON=2.E6          ! Original M-P value.
      SON=2.E7
!jmb--According to Roy Rasmussens data (from a QJRMS paper he was reviewing)
!      the value of the M-P slope intercept can be as large as 3.E7 for
!      graupel.  The value GON = 4.E6 as an upper bound on the intercept value
!      appears too small.  Use same value as for snow.--17oct96
!     GON=4.E6          ! Original M-P value.
!gt   GON=5.e7          ! Roy R., summer 1998, 19 Jan 00, Oct 00
      GON=4.E6          ! Original M-P value.  GREG T.  changed 01 Dec 2004

!C DENSITY OF RAIN, SNOW, AND GRAUPEL

      DRAIN=1000.
      DSNOW=100.
      DGRAUPEL=400.

! CONSTANTS FOR VARIABLE RON

!jmb  qr0 is center value of rain mixing ratio for transition from
!       M-P slope-intercept for drizzle formed by a collision-coalescence
!       process to M-P slope-intercept for traditional rain.--nov00
!jmb  delqr0 governs sharpness of transition: small delt_qr0 makes the
!      transition sharper:
!      if the rate of change of zero intercept wrt rain mixing ratio
!      were linear, with the slope at QR0 given by present tanh formula,
!      the transition would occur between qr0-delqr0 and qr0+delqr0.--nov00
!Cgt   RON_min = RON
      RON_min = 2.e7
!Cgt   qr0 = 0.0001  !  Roy R. 26oct00
      qr0 = 0.0002  !  GREG T.  01 Dec 2004
!Cgt   delqr0 = 0.25*qr0
      delqr0 = 0.5*qr0    !  GREG T.  01 Dec 2004
      const1r=(ron2-ron_min)*0.5
      const2r=(ron2+ron_min)*0.5

!CONSTANTS FOR VARIABLE GON
!     Based on Roy R s formulation, Jun 96

      constd=1./0.52
      constgb=12./13.
      const_ng1=(1.57**constd)*((pi*dgraupel)**constgb)
      const_ng2=-constgb
      Hail_max=(const_ng1/1.e4)**(1.0/constgb)
      Hail_min=(const_ng1/gon)**(1.0/constgb)
      Hail_cfa=constgb*0.75+1.75
      Hail_cfb=constgb*0.75-0.25

!-----------------------------------------------------------

      bb = 0.		!  bright band effect - yes or no (0)

      alpha = 0.224 ! = (1000kg/m^3/917kg/m^3)**2)*(0.176/0.930)
!                      1000kg/m^3 is density of liquid water
!                       917kg/m^3 is density of solid ice
!                      0.176 = dielectric factor of ice
!                      0.930 = dielectric factor of liquid water
!
!  CONSTANT for RAIN reflectivity factor
!
      Arain=720/(pi**1.75 * Drain**1.75)
      Asnow=720*alpha*sqrt(sqrt(DSNOW))/(pi**1.75 * Drain*Drain)
      Ahail=720*alpha/( (1.57**(constd*0.75)) * pi**Hail_cfa *     &
               Drain*Drain * DGRAUPEL**Hail_cfb)
!
!  generate look up table for rain
!
      range_up=8.0e-4
      range_down=4.0e-7
      drange=(range_up-range_down)/float(num_tbl-1)
      tb_rain(1)   =  range_down
      DO i=1,num_tbl-1
        tb_rain(i+1) = tb_rain(i) + drange
      ENDDO
!
      DO i=1,num_tbl
         ronv = (const1r*tanh((qr0 - tb_rain(i))/delqr0) + const2r)
         tb_ze_r(i) = Arain * tb_rain(i)**1.75 / (ronv**0.75)
      ENDDO
!-------------------
 
      Do k = 1,nztn_p
      DO J=1,ny_p
      DO I=1,nx_p

        IF (refl3(i,j,k) >= thresh_ref) THEN    ! valid radar refl.
          rhod = p(i,j,k) / (rd_p * tb(i,j,k) )
          ze_nc = 10.0**(0.1*refl3(i,j,k))/1.e18
          pcptype = cldpcp_type_3d(i,j,k)/16       ! precip. type
          tc = tb(i,j,k) - 273.15

          Brain=Arain*rhod**1.75
          Bsnow=Asnow*rhod**1.75
          Bhail=Ahail*rhod**Hail_cfa

          IF (pcptype == 1) THEN   ! rain
            ze_r =  ze_nc
            call rain_retrieval(rain, Brain, RON_min, ron2, rhod, &
                            tb_ze_r,ze_r,num_tbl,tb_rain)
            qr(i,j,k) = max(rain, r1)
          ELSE IF (pcptype == 2) THEN                   ! snow
            IF (tc <= 0.0) THEN     !dry snow
              ze_s =  ze_nc
              temp_C = min(-0.001, tc)
              sonv = (min(2.0E8, 2.0E6*exp(-0.12*temp_C)))
              snow = ( (ze_s / Bsnow)* (sonv**0.75) )**(4./7.)
              qs(i,j,k) = max(snow, r1)
            ELSE IF (tc < 5.0) THEN     !wet snow
              rfract=0.20*tc
              ze_s =  ze_nc * (1.-rfract)
              IF (tc .gt. 0)  ze_s = ze_s/(1. + 4.28*bb)
              temp_C = min(-0.001, tc)
              sonv = (min(2.0E8, 2.0E6*exp(-0.12*temp_C)))
              snow = ( (ze_s / Bsnow)* (sonv**0.75) )**(4./7.)
              qs(i,j,k) = max(snow, r1)

              ze_r =  ze_nc * rfract
              call rain_retrieval(rain, Brain, RON_min, ron2, rhod, &
                            tb_ze_r,ze_r,num_tbl,tb_rain)
              qr(i,j,k) = max(rain, r1)
            ELSE    ! rain
              ze_r =  ze_nc * rfract
              call rain_retrieval(rain, Brain, RON_min, ron2, rhod, &
                            tb_ze_r,ze_r,num_tbl,tb_rain)
              qr(i,j,k) = max(rain, r1)
            END IF
          ELSE IF (pcptype == 3) THEN   ! ZR
              ze_r =  ze_nc 
              call rain_retrieval(rain, Brain, RON_min, ron2, rhod, &
                            tb_ze_r,ze_r,num_tbl,tb_rain)
              qr(i,j,k) = max(rain, r1)
          ELSE IF (pcptype == 4) THEN   ! sleet
            IF (tc <= 0.0) THEN     ! hail category
               ze_g =  ze_nc 
               graupel = (ze_g / Bhail ) **(1/Hail_cfa)
               qg(i,j,k) = max (r1, graupel )
            ELSE IF( tc < 10. ) THEN
              rfract=0.10*tc
              ze_r =  ze_nc * rfract
              call rain_retrieval(rain, Brain, RON_min, ron2, rhod, &
                            tb_ze_r,ze_r,num_tbl,tb_rain)
              qr(i,j,k) = max(rain, r1)

              ze_g =  ze_nc * (1. - rfract)
              ze_g = ze_g/(1. + 4.28*bb)    ! For bright band
              graupel = (ze_g / Bhail ) **(1/Hail_cfa)
              qg(i,j,k) = max (r1, graupel )
            ELSE
              ze_r =  ze_nc 
              call rain_retrieval(rain, Brain, RON_min, ron2, rhod, &
                            tb_ze_r,ze_r,num_tbl,tb_rain)
              qr(i,j,k) = max(rain, r1)
            END IF
          ELSE IF (pcptype == 5) THEN   ! hail
            ze_g =  ze_nc 
            if( tc > 0 ) ze_g = ze_g/(1. + 4.28*bb)    ! For bright band
            graupel = (ze_g / Bhail ) **(1/Hail_cfa)
            qg(i,j,k) = max (r1, graupel )
          ELSE                                          ! unknown
            IF (tc <= 0.0) THEN     !dry snow
              ze_s =  ze_nc 
              temp_C = min(-0.001, tc)
              sonv = (min(2.0E8, 2.0E6*exp(-0.12*temp_C)))
              snow = ( (ze_s / Bsnow)* (sonv**0.75) )**(4./7.)
              qs(i,j,k) = max(snow, r1)
            ELSE IF ( tc < 5.0 ) THEN     !wet snow
              rfract=0.20*tc
              ze_s =  ze_nc * (1.- rfract)
              IF (tc .gt. 0)  ze_s = ze_s/(1. + 4.28*bb)
              temp_C = min(-0.001, tc)
              sonv = (min(2.0E8, 2.0E6*exp(-0.12*temp_C)))
              snow = ( (ze_s / Bsnow)* (sonv**0.75) )**(4./7.)
              qs(i,j,k) = max(snow, r1)

              ze_r =  ze_nc * rfract
              call rain_retrieval(rain, Brain, RON_min, ron2, rhod, &
                            tb_ze_r,ze_r,num_tbl,tb_rain)
              qr(i,j,k) = max(rain, r1)
            ELSE ! rain
              ze_r =  ze_nc
              call rain_retrieval(rain, Brain, RON_min, ron2, rhod, &
                            tb_ze_r,ze_r,num_tbl,tb_rain)
              qr(i,j,k) = max(rain, r1)

            END IF
          END IF
        else
          qr(i,j,k) = -999.
          qs(i,j,k) = -999.
          qg(i,j,k) = -999.
        endif
      end do
      end do
      end do

! change unit from kg/kg to g/kg

      Do k = 1,nztn_p
!      rmax=-999.0
      DO J=1,ny_p
      DO I=1,nx_p
         if( qr(i,j,k) .ge. r1 ) qr(i,j,k)=qr(i,j,k)*1000.0
         if( qs(i,j,k) .ge. r1 ) qs(i,j,k)=qs(i,j,k)*1000.0
         if( qg(i,j,k) .ge. r1 ) qg(i,j,k)=qg(i,j,k)*1000.0
!         if(rmax < qs(i,j,k)) then
!            rmax=qs(i,j,k)
!            imax=i
!            jmax=j
!            kmax=k
!         endif
      end do
      end do
      end do

      return

  END SUBROUTINE pcp_mxr_thompsonRUC

  SUBROUTINE BinSearch(ii,arry,targt,num)
!
!  doing a search using binary method
!
!  arry : array include ordered data
!  targt: the number need to find in the arrary
!  num : dimension of array
!  ii: return location of array that meet the require.
!
  implicit none
  integer :: ii
  integer :: num
  real :: targt
  real :: arry(num)

  integer :: L,R,m

  L=1
  R=num

  if( targt < arry(1) ) then
    ii=-1
  else if (targt > arry(num) ) then
    ii=-2
  else
    DO While ( L < R )
     m= (L+R)/2 
     IF( arry(m) < targt ) then
       L = m + 1
     ELSE
       R = m
     ENDIF
    ENDDO
    IF ( L==R ) then
        II=L
    ELSE
        write(6,*) 'BinSearch: Problems in binar search !'
        call stop2(114)
    ENDIF
  endif

  END SUBROUTINE BinSearch

  SUBROUTINE rain_retrieval(rain, Brain, RON_min, ron2, rhod, &
                            tb_ze_r,ze_r,num_tbl,tb_rain)
!
!  retrivel rain
!
   implicit none
!
   INTEGER :: num_tbl
   REAL :: rain
   REAL :: Brain
   REAL :: RON_min,ron2,rhod
   REAL :: ze_r
   REAL :: tb_ze_r(num_tbl)
   REAL :: tb_rain(num_tbl)

!
   INTEGER :: ii

          if( (ze_r / rhod**1.75) > tb_ze_r(num_tbl)) then
              rain = (ze_r / Brain * (RON_min**0.75) ) ** (4./7.)
          elseif ( (ze_r /rhod**1.75)  < tb_ze_r(1)) then
              rain = (ze_r / Brain * (ron2**0.75) ) ** (4./7.)
          else
              ii=0
              ze_r = ze_r/rhod**1.75
              call BinSearch(ii,tb_ze_r,ze_r,num_tbl)
              if(ii > 1 ) then
                rain=tb_rain(ii-1)
                if( (ze_r - tb_ze_r(ii-1)) > (tb_ze_r(ii) - ze_r)) &
                      rain=tb_rain(ii)
              else
                  write(6,*) ii
                  write(6,*) 'rain_retrieval: Something wrong in lookup table for rain'
                  write(6,*) 'Stop in Sub rain_retrieval'
                  call stop2(114)
              endif
          endif

  END SUBROUTINE rain_retrieval



