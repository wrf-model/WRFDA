!+ Initialize fast radiative transfer model.
!
Subroutine RTTVI(                                          &
     kerr, kppf, kpnsat, kplev, kpch, kpchus,              &
     kpnav, kpnsav, kpnssv, kpncv,                         &
     nrttovid, platform, satellite, instrument , numchans, &
     preslev, otmin, otmax, oqmin, oqmax, oozmin, oozmax,  &
     ivch, niu1)
  !
  !    This software was developed within the context of
  !    the EUMETSAT Satellite Application Facility on
  !    Numerical Weather Prediction (NWP SAF), under the
  !    Cooperation Agreement dated 25 November 1998, between
  !    EUMETSAT and the Met Office, UK, by one or more partners
  !    within the NWP SAF. The partners in the NWP SAF are
  !    the Met Office, ECMWF, KNMI and MeteoFrance.
  !
  !    Copyright 2002, EUMETSAT, All Rights Reserved.
  !
  ! Description:
  ! Initialisation for tovs rt routine, rttov.
  ! to be called before first call to rttov.
  ! allows arrays to be allocated correct size.
  ! IVCH array and numchans is normally initialised to zero
  ! in main program. It can be used to read in
  ! only those coefficients for channels you
  ! want to process by specifying valid channel
  ! numbers in this array on input. This is useful
  ! for sounders with many channels (eg AIRS)
  ! as it saves storing all coefficients (eg 2300+ for AIRS)
  ! for just a few channels required (eg ~300 for AIRS).
  ! On return IVCH either contains a list of valid channel
  ! numbers for the instrument or if non-zero input
  ! those requested.
  !
  ! Compatible with RTTOV8 library but only able to
  ! run with coefficients created on RTTOV7 43 pressure levels
  !
  ! Method
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date        Comment
  ! -------   ----        -------
  !        13/8/92.  For version 2. input sat ids required and
  !                  store only data for these.
  !        11/7/96   Modified to include meteosat & goes
  !        9/12/96   Water vapour transmittance extended to level 1
  !        6/12/97   Rose Munro - Modified for multiple satellite series,
  !        6/12/97   Rose Munro - Modified for multiple satellite series,
  !                            id's and subtypes
  !        18/3/98   Roger Saunders - Modified to generalise no. of levels
  !        18/8/98   Roger Saunders - Added key array sizes to output
  !        06/4/99   Roger Saunders - Added ssm/i
  !        01/12/99  Roger Saunders - Added variable unit number KIU1
  !        04/01/00  Roger Saunders - Added AVHRRCF+GOESIMCF
  !        01/05/2000     F90 code
  !        10/08/2000  P. Brunel    - Added GOESSNDCF
  !        21/03/2001  P, Brunel    - Unique coefficient file reading subroutine
  !        26/03/2001  P, Brunel    - New RTTOV identification numbers
  !        18/09/2001  A. Collard   - Allow a subset of channels to be initialised
  !        20/09/2001  A. Collard   - Allow coeffs file to be opened externally
  !        01/12/2002  P. Brunel    - Keep compatibility with RTTOV8
  !
  ! Code Description:
  !   Language:          Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  Use MOD_CPARAM, Only : &
       ! Imported Paramters:
       jpnsat ,&     ! Total max sats to be used
       jplev  ,&     ! No. of pressure levels
       jpnav  ,&     ! No. of profile variables
       jpnsav ,&     ! No. of surface air variables
       jpnssv ,&     ! No. of skin variables
       jpncv  ,&     ! No. of cloud variables
       jppf   ,&     ! Max no. profiles
       jpch   ,&     ! Max. no. of tovs channels
       jpchus ,&     ! Max. no. of channels used tovs
       jpchpf ,&     ! Max no. of profs * chans used
       jpcofm ,&     ! Mixed gas coeffs (max)
       jpcofw ,&     ! Water vapour coeffs (max)
       jpcofo ,&     ! Ozone coeffs (max)
       jpst   ,&     ! Max no. of surface types
       jmwcldtop ,&     ! Upper level for lwp calcs
       rcnv   ,&     ! kg/kg--> ppmv ozone
       rcnw   ,&     ! kg/kg--> ppmv water vapour
       nulout ,&       ! unit for error messages
       coef

  Use MOD_CPARAM, Only : &
       ! Imported Scalar Variables with intent (in):
       njpnsat  ,&   ! Total max sats to be used
       njplev   ,&   ! No. of pressure levels
       njpnav   ,&   ! No. of profile variables
       njpnsav  ,&   ! No. of surface air variables
       njpnssv  ,&   ! No. of skin variables
       njpncv   ,&   ! No. of cloud variables
       njppf    ,&   ! Max no. profiles
       njpch    ,&   ! Max. no. of tovs channels
       njpchus  ,&   ! Max. no. of channels used tovs
       njpchpf  ,&   ! Max no. of profs * chans used
       njpcofm  ,&   ! Mixed gas coeffs (max)
       njpcofw  ,&   ! Water vapour coeffs (max)
       njpcofo  ,&   ! Ozone coeffs (max)
       njpst    ,&   ! Max no. of surface types
       nmwcldtop     ! Upper level for lwp calcs

  Use rttov_const, Only :   &
       gas_id_watervapour  ,&
       gas_id_ozone        ,&
       sensor_id_mw
  !
  Use parkind1, Only : jpim     ,jprb
  Implicit None
  !
#include "rttov_readcoeffs.interface"
#include "rttov_initcoeffs.interface"

  ! Subroutine arguments
  ! scalar arguments with intent(in):
  Integer(Kind=jpim), Intent(in) :: nrttovid ! number of RTTOV ids  requested
  ! RTTOV id is defined by 3 numbers:
  ! platform = satellite serie (Noaa=1, Goes=4, DMSP=2...)
  ! satellite = satellite number in the serie
  !               Noaa14 = 14
  ! instrument = instrument number (HIRS=0, AMSU-A=3)

  ! Array  arguments with intent(in):
  ! ............. for each RTTOVid
  Integer(Kind=jpim), Intent(in) :: platform(*)     ! number of platform. id's
  Integer(Kind=jpim), Intent(in) :: satellite(*)    ! number of satellite. id's
  Integer(Kind=jpim), Intent(in) :: instrument(*)   ! number of instrument. id's

  ! Array  arguments with intent(inout):
  ! ............. for each RTTOVid
  Integer(Kind=jpim), Intent(inout) :: numchans(*)  ! Number of channels initialised
  Integer(Kind=jpim), Intent(inout) :: niu1(*)      ! optional unit number to read
  Integer(Kind=jpim)  :: niu2(30)      ! optional unit number to read
  ! rt_coef... files.

  !
  ! Scalar arguments with intent(out):
  Integer(Kind=jpim), Intent(out) :: kerr   ! error flag, returns kerr /= 0 if error
  ! <0 is RTTVI error
  ! >0 is RTTOVCF error
  Integer(Kind=jpim), Intent(out) :: kppf   ! max no. profiles processed in parallel
  Integer(Kind=jpim), Intent(out) :: kpnsat ! max no. of satellites
  Integer(Kind=jpim), Intent(out) :: kplev  ! no of rt levels
  Integer(Kind=jpim), Intent(out) :: kpch   ! max no. of channels
  Integer(Kind=jpim), Intent(out) :: kpchus ! max no. of channels used
  Integer(Kind=jpim), Intent(out) :: kpnav  ! max no of profile variables
  Integer(Kind=jpim), Intent(out) :: kpnsav ! max no of surface variables
  Integer(Kind=jpim), Intent(out) :: kpnssv ! max no of skin variables
  Integer(Kind=jpim), Intent(out) :: kpncv  ! max no of cloud variables

  ! Array  arguments with intent(out):
  Real(Kind=jprb), Intent(out) ::    preslev(jplev)     ! 43 pressure levels  (Pa)
  Real(Kind=jprb), Intent(out) ::    otmin(jplev)       ! min temp array (K)
  Real(Kind=jprb), Intent(out) ::    otmax(jplev)       ! max temp array (K)
  Real(Kind=jprb), Intent(out) ::    oqmin(jplev)       ! min q array   (kg/kg)
  Real(Kind=jprb), Intent(out) ::    oqmax(jplev)       ! max q array   (kg/kg)
  Real(Kind=jprb), Intent(out) ::    oozmin(jplev)      ! min ozone array   (kg/kg)
  Real(Kind=jprb), Intent(out) ::    oozmax(jplev)      ! max ozone array   (kg/kg)
  Integer(Kind=jpim), Intent(inout) :: ivch(jpch,jpnsat)  ! array of valid channel numbers
  !

  ! Local scalars:
  Integer(Kind=jpim) :: msat                 ! indice for RTTOV ids
  Integer(Kind=jpim) :: ref_ind              ! reference index for min/max

  Integer(Kind=jpim) :: ig
  Integer(Kind=jpim) :: in_inst(3)              ! instrument rttov id
  Integer(Kind=jpim) :: ich
  !- End of header ------------------------------------------------------

  !
  !     -----------------------------------------------------------------
  !*         1.   Set up profile constants.
  !               --- -- ------- ---------
  !
  kerr   = 0
  kppf   = jppf
  kpnsat = jpnsat
  kplev  = jplev
  kpch   = jpch
  kpchus = jpchus
  kpnav  = jpnav
  kpnsav = jpnsav
  kpnssv = jpnssv
  kpncv  = jpncv
  !
  njpnsat = jpnsat
  njplev  = jplev
  njpnav  = jpnav
  njpnsav = jpnsav
  njpnssv = jpnssv
  njpncv  = jpncv
  njppf   = jppf
  njpch   = jpch
  njpchus = jpchus
  njpchpf = jpchpf
  njpcofm = jpcofm
  njpcofw = jpcofw
  njpcofo = jpcofo
  njpst   = jpst
  nmwcldtop = jmwcldtop
  !ivch(:,:) = 0
  niu2(1:nrttovid)=niu1(1:nrttovid)
  !
  !*         1.   Set up data for all satellites.
  !               --- -- ---- --- --- ----------
  !
  !     1.1 Set up Sat ids FOR TOVS/METEOSAT/GOES
  !

  If (nrttovid > jpnsat) Then
     kerr=-1
     Return
  End If
  !
  !*    1.2  Set up satellite-specific data for tovs/meteosat/goes
  !
  ref_ind = 0
  Do msat = 1, nrttovid
     in_inst(1)  = platform(msat)
     in_inst(2)  = satellite(msat)
     in_inst(3)  = instrument(msat)

!  airs aqua
     if(instrument(msat) == 11)then
        in_inst(2)  = 2
        in_inst(1)  = 9
     endif

! amsua aqua
     if(instrument(msat) == 3 .and. satellite(msat) == 20) then
        in_inst(2)  = 2
        in_inst(1)  = 9
     endif

     If(numchans(msat) > 0) Then
        If( niu2(msat) >0 ) Then
           Call rttov_readcoeffs  (kerr, coef(msat), &
                &  channels = ivch(1:numchans(msat),msat), &
                &  file_id = niu2(msat) )
           Call rttov_initcoeffs  ( &
                & kerr,        &! out
                & coef(msat)  ) ! inout
        Else
           Call rttov_readcoeffs  (kerr, coef(msat), in_inst, &
                &  channels = ivch(1:numchans(msat),msat) )
           Call rttov_initcoeffs  ( &
                & kerr,        &! out
                & coef(msat)  ) ! inout
        End If
     Else
        If( niu2(msat) >0 ) Then
           Call rttov_readcoeffs  (kerr, coef(msat), &
                & file_id = niu2(msat) )
           Call rttov_initcoeffs  ( &
                & kerr,        &! out
                & coef(msat)  ) ! inout
        Else
           Call rttov_readcoeffs  (kerr, coef(msat), in_inst )
           Call rttov_initcoeffs  ( &
                & kerr,        &! out
                & coef(msat)  ) ! inout
        End If
        numchans(msat) =  coef(msat) % fmv_chn
        Do ich = 1, numchans(msat)
           If(coef(msat) % ff_val_chn(ich) /= 0) Then
              ivch(ich,msat) =  ich
           Else
              ivch(ich,msat) =  0
           Endif
        Enddo
     Endif

     If(kerr /= 0) Then
        Return
     Else
        If (ref_ind == 0) ref_ind = msat
     Endif

     If( jplev /= coef(msat) % nlevels ) Then
        kerr = -3
        Return
     Endif
  End Do
  !
  !     -----------------------------------------------------------------
  !
  !*         2.1  Set up pressure level constants and limits and output
  !               array for ifs
  !
  If (ref_ind == 0) Then
     kerr = -2
     Write(nulout,*) 'rttvi: reference profiles index zero'
     Return
  Endif
  !
  preslev(1:jplev) = coef(ref_ind) % ref_prfl_p(1:jplev)*100._JPRB    ! (Pa)
  otmin(1:jplev)   = coef(ref_ind) % lim_prfl_tmin(1:jplev)      ! (K)
  otmax(1:jplev)   = coef(ref_ind) % lim_prfl_tmax(1:jplev)      ! (K)
  ig = coef(ref_ind) % fmv_gas_pos( gas_id_watervapour )
  oqmin(1:jplev)   = coef(ref_ind) % lim_prfl_gmin(1:jplev,ig)/rcnw    ! ppmv -> kg/kg
  oqmax(1:jplev)   = coef(ref_ind) % lim_prfl_gmax(1:jplev,ig)/rcnw    ! ppmv -> kg/kg
  !
  Do msat = 1, nrttovid
    If (coef(msat) % id_sensor /= sensor_id_mw .and. in_inst(3) /= 2 ) Then   !If not MW or SSU
      ig = coef(ref_ind) % fmv_gas_pos( gas_id_ozone )
      oozmin(1:jplev)  = coef(ref_ind) % lim_prfl_gmin(1:jplev,ig)/rcnv   ! ppmv -> kg/kg
      oozmax(1:jplev)  = coef(ref_ind) % lim_prfl_gmax(1:jplev,ig)/rcnv   ! ppmv -> kg/kg
    endif
  End do
  !
  !
  !
  !
  !     -----------------------------------------------------------------
  !
  Return

End Subroutine RTTVI
