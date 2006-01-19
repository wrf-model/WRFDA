!WRF:MODEL_LAYER:DEFINE_STRUCTURES
!

! DA_Define_Structures code to define 3DVAR structures.
!---------------------------------------------------------------------------

MODULE DA_Define_Structures

    USE module_domain   ! for typedefs (vp_type, xb_type, xpose_type)
    USE module_dm       ! for rsl definitions

!------------------------------------------------------------------------------
!  PURPOSE: Collection of routines to define and allocate structures.
!
!  METHOD:  Varied.
!
!  HISTORY: 01/06/2000 - Creation of F90 version.           Dale Barker
!           10/19/2001 - Mods for parallel implementation.  Al Bourgeois
!           05/25/2004 - "gpsref" added.             W. Huang, Y.-R. Guo
!           10/08/2004 - Updated be_type             Syed  RH Rizvi         
!           03/04/2005 - CMV's from Geostationary and       Syed  RH  Rizvi
!                        Polar orbiting satellite are
!                        seperated & used as profile
!------------------------------------------------------------------------------

   USE DA_Constants

   IMPLICIT NONE
   
!-----------------------------------------------------------------------------
!  [1.0] MM5 model structure definition:
!-----------------------------------------------------------------------------

   type big_record_header
      integer(kind=4)                   :: header_flag
      integer(kind=4), dimension(50,20) :: bhi
      real(kind=4),    dimension(20,20) :: bhr
      character(80),   dimension(50,20) :: bhic
      character(80),   dimension(20,20) :: bhrc
   end type big_record_header

   type sub_record_header
      integer(kind=4)   :: ndim

      integer(kind=4), dimension(4) :: start_index, end_index

      real(kind=4)      :: current_time

      character(len= 4) :: staggering, ordering
      character(len= 8) :: char_date
      character(len= 9) :: name
      character(len=24) :: current_date
      character(len=25) :: units
      character(len=46) :: description
   end type sub_record_header

   TYPE mm5_model_type
      integer :: num_of_var

      TYPE (big_record_header)                             :: big_header

      TYPE (sub_record_header), dimension(max_num_of_var)  :: sub_header

!     Variables held on grid:
      REAL, POINTER    :: u(:,:,:)
      REAL, POINTER    :: v(:,:,:)
      REAL, POINTER    :: t(:,:,:)
      REAL, POINTER    :: q(:,:,:)
      REAL, POINTER    :: pp_c(:,:,:)
      REAL, POINTER    :: w(:,:,:)

      REAL, POINTER    :: qcw(:,:,:)
      REAL, POINTER    :: qrn(:,:,:)
      REAL, POINTER    :: qci(:,:,:)
      REAL, POINTER    :: qsn(:,:,:)
      REAL, POINTER    :: qgr(:,:,:)

      REAL, POINTER    :: psi(:,:,:)
      REAL, POINTER    :: chi(:,:,:)
      REAL, POINTER    :: blnc_p(:,:,:)
      REAL, POINTER    :: rh(:,:,:)

      REAL, POINTER    :: psac(:,:)
      REAL, POINTER    :: cori(:,:)
      REAL, POINTER    :: latc(:,:)
      REAL, POINTER    :: lonc(:,:)
      REAL, POINTER    :: latd(:,:)
      REAL, POINTER    :: lond(:,:)
      REAL, POINTER    :: msfc(:,:)
      REAL, POINTER    :: msfd(:,:)
      REAL, POINTER    :: tgrn(:,:)
      REAL, POINTER    :: terr(:,:)
      REAL, POINTER    :: lanu(:,:)
      REAL, POINTER    :: snow(:,:)
      REAL, POINTER    :: landmask(:,:)
      REAL, POINTER    :: sigmah(:)
      REAL, POINTER    :: psad(:,:)

      REAL, POINTER    ::      t_eval_glo(:)
      REAL, POINTER    ::      q_eval_glo(:)
      REAL, POINTER    ::    psi_eval_glo(:)
      REAL, POINTER    ::    chi_eval_glo(:)
      REAL, POINTER    :: blnc_p_eval_glo(:)
      REAL, POINTER    ::     rh_eval_glo(:)

      REAL, POINTER    ::      t_evec_glo(:,:)
      REAL, POINTER    ::      q_evec_glo(:,:)
      REAL, POINTER    ::    psi_evec_glo(:,:)
      REAL, POINTER    ::    chi_evec_glo(:,:)
      REAL, POINTER    :: blnc_p_evec_glo(:,:)
      REAL, POINTER    ::     rh_evec_glo(:,:)

      REAL, POINTER    ::      t_eval_loc(:,:)
      REAL, POINTER    ::      q_eval_loc(:,:)
      REAL, POINTER    ::    psi_eval_loc(:,:)
      REAL, POINTER    ::    chi_eval_loc(:,:)
      REAL, POINTER    :: blnc_p_eval_loc(:,:)
      REAL, POINTER    ::     rh_eval_loc(:,:)

      REAL, POINTER    ::      t_evec_loc(:,:,:)
      REAL, POINTER    ::      q_evec_loc(:,:,:)
      REAL, POINTER    ::    psi_evec_loc(:,:,:)
      REAL, POINTER    ::    chi_evec_loc(:,:,:)
      REAL, POINTER    :: blnc_p_evec_loc(:,:,:)
      REAL, POINTER    ::     rh_evec_loc(:,:,:)

      REAL, POINTER    ::      t_sl(:)
      REAL, POINTER    ::      q_sl(:)
      REAL, POINTER    ::    psi_sl(:)
      REAL, POINTER    ::    chi_sl(:)
      REAL, POINTER    :: blnc_p_sl(:)
      REAL, POINTER    ::     rh_sl(:)

      REAL, POINTER    :: pb_vert_reg(:,:,:)

   END TYPE mm5_model_type

!-----------------------------------------------------------------------------
!  [2.0] Background field structure definition:
!-----------------------------------------------------------------------------

!ajb Extracted non-grid variables from xb_type  
   TYPE xbx_type
      TYPE (big_record_header)                             :: big_header
      TYPE (sub_record_header), dimension(max_num_of_var)  :: sub_header

      CHARACTER (len=4):: mminlu

      INTEGER          :: fft_pad_i          ! Padding to get 2**p 3**q 5**r. (p>=1)
      INTEGER          :: fft_pad_j          ! Padding to get 2**p 3**q 5**r.

      INTEGER          :: pad_num            ! Splitted fft_pad_i on this processor.
      INTEGER          :: pad_inc            ! Pad increment (split over v2y).
      INTEGER, POINTER :: pad_loc(:)         ! pad location on this processor.
      INTEGER, POINTER :: pad_pos(:)         ! pad position beyond ide for this processor.

      INTEGER          :: fft_ix             ! x-direction FFT number, in 2**p 3**q 5**r.
      INTEGER          :: fft_jy             ! y-direction FFT number, in 2**p 3**q 5**r.

      INTEGER, POINTER :: fft_factors_x(:)   ! FFT factors in x direction.
      INTEGER, POINTER :: fft_factors_y(:)   ! FFT factors in y direction.

      REAL, POINTER    :: trig_functs_x(:)   ! Trig functions in x direction.
      REAL, POINTER    :: trig_functs_y(:)   ! Trig functions in y direction.

      REAL             :: psac_mean          ! Mean pressure.
      REAL, POINTER    :: latc_mean(:)       ! Mean latitude.

      REAL, POINTER    :: fft_coeffs(:,:)    ! FFT Coefficients

      REAL             :: fft_adjoint_factor ! FFT Adjoint factor
!  spectral transform related variables
      INTEGER          :: inc                ! Vector array increment 
      INTEGER          :: ni
      INTEGER          :: nj
      INTEGER          :: nk
      INTEGER          :: max_wavenumber
      INTEGER          :: c_cv_size
      INTEGER          :: lenr
      INTEGER          :: lensav
      INTEGER          :: lenwrk
      INTEGER          :: alp_size
   real, pointer       :: wsave(:)          ! Primes for FFT.
   real, pointer       :: lon(:)            ! Longitude (radians).
   real, pointer       :: sinlon(:)         ! sine(longitude).
   real, pointer       :: coslon(:)         ! cosine(longitude).
   real, pointer       :: lat(:)            ! Latitude (radians, from south).
   real, pointer       :: sinlat(:)         ! sine(latitude).
   real, pointer       :: coslat(:)         ! cosine(latitude).
   real, pointer       :: int_wgts(:)       ! Legendre integration weights.
   real, pointer       :: alp(:)            ! Associated Legendre Polynomial.


   END TYPE xbx_type

!-----------------------------------------------------------------------------
!  [3.0] Innovation vector structure definition:
!-----------------------------------------------------------------------------

!  [3.1] Generic sub-structures used in ob_type:

   TYPE field_type
      REAL                   :: inv             ! Innovation vector
      INTEGER                :: qc              ! Observation QC
      REAL                   :: error           ! Observational error
   END TYPE field_type

   TYPE model_loc_type
      TYPE (field_type)       :: slp            ! Pressure in Pa
!     TYPE (field_type)       :: psfc           ! Pressure in Pa
! Remove the following in future (needed now for obs i/o only):
      TYPE (field_type)       :: pw             ! Toatl precipitable water cm
!
      real                    :: x
      real                    :: y
      integer                 :: i
      integer                 :: j
      real                    :: dx
      real                    :: dxm
      real                    :: dy
      real                    :: dym
      logical                 :: proc_domain
      logical                 :: proc_domain_with_halo
      ! obs_global_index is the original index of this obs in the serial 
      ! code.  It is used to reassemble obs in serial-code-order to replicate 
      ! summation order for bitwise-exact testing of distributed-memory 
      ! parallel configurations.  
      integer                 :: obs_global_index

      integer                 :: v_interp_optn  ! 0, not specified
                                                ! 1, vertical interpolate in pressure
                                                ! 2, vertical interpolate in height

   END TYPE model_loc_type

   TYPE each_level_type
      REAL                    :: height         ! Height in m
      INTEGER                 :: height_qc      ! Height QC
      real                    :: zk             ! k-coordinates
      TYPE (field_type)       :: u              ! Wind x-component in m/s
      TYPE (field_type)       :: v              ! Wind y-component in m/s
      TYPE (field_type)       :: p              ! Pressure in Pa
      TYPE (field_type)       :: t              ! Temperature in K
      TYPE (field_type)       :: q              ! Mixing ratio (kg/kg).
      TYPE (field_type)       :: rh             ! Relative humidity (%).
      TYPE (field_type)       :: td             ! dew-point in K
      TYPE (field_type)       :: Speed          ! Wind speed m/s
   END TYPE each_level_type

   TYPE Radar_each_level_type
        REAL                   :: height         ! Height in m
        INTEGER                :: height_qc      ! Height QC
        real                   :: zk             ! MM5 k-coordinates
        TYPE (field_type)      :: rv
        TYPE (field_type)      :: rf
   END TYPE Radar_each_level_type

   TYPE info_type
        CHARACTER (LEN = 40)   :: name          ! Station name
        CHARACTER (LEN = 12)   :: platform      ! Instrument platform
        CHARACTER (LEN =  5)   :: id            ! 5 digit station identifer
        CHARACTER (LEN = 19)   :: date_char     ! CCYY-MM-DD_HH:MM:SS date
        INTEGER                :: levels        ! number of levels
        REAL                   :: lat           ! Latitude in degree
        REAL                   :: lon           ! Longitude in degree
        REAL                   :: elv           ! Elevation in m
        real                   :: pstar         ! Surface pressure
   END TYPE info_type

   TYPE stn_loc_type
      REAL                    :: lon                  ! Radar site loc
      REAL                    :: lat                  ! Radar site loc
      REAL                    :: elv                  ! Radar site loc
      REAL                    :: x                    ! Radar site loc
      REAL                    :: y                    ! Radar site loc
      REAL                    :: zk                   ! Radar site loc
   END TYPE stn_loc_type
 
   TYPE Radar_type
      TYPE (stn_loc_type)     :: stn_loc
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc

      REAL                    :: model_p(max_ob_levels)
      REAL                    :: model_rho(max_ob_levels)
      REAL                    :: model_qrn(max_ob_levels)
      REAL                    :: model_ps

      REAL                  , pointer :: height   (:) ! Height in m
      INTEGER               , pointer :: height_qc(:) ! Height QC
      REAL                  , pointer :: zk       (:) ! MM5 k-coordinates

      TYPE (field_type)     , pointer :: rv       (:) ! Radial Velocity
      TYPE (field_type)     , pointer :: rf       (:) ! Reflectivity
   END TYPE Radar_type

   TYPE multi_level_type
        TYPE (info_type)                        :: info
        TYPE (model_loc_type)                   :: loc
        TYPE (each_level_type), &
              DIMENSION (max_ob_levels)         :: each
   END TYPE multi_level_type

   TYPE Radar_stn_type
        CHARACTER (LEN = 5)    :: platform      ! Data type
        CHARACTER (LEN = 12)   :: name          ! Station name
        CHARACTER (LEN = 19)   :: date_char     ! CCYY-MM-DD_HH:MM:SS date
        INTEGER                :: numObs        ! number of Obs
        INTEGER                :: levels        ! number of levels
        REAL                   :: lat           ! Latitude in degree
        REAL                   :: lon           ! Longitude in degree
        REAL                   :: elv           ! Elevation in m
   END TYPE Radar_stn_type

   TYPE Radar_multi_level_type
        TYPE (Radar_stn_type)                   :: stn
        TYPE (info_type)                        :: info
        TYPE (model_loc_type)                   :: loc
        TYPE (Radar_each_level_type), &
              DIMENSION (max_ob_levels)         :: each
   END TYPE Radar_multi_level_type

!  [3.2] Innovation vector structure:

   TYPE airep_type
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc

      REAL                  , pointer :: h        (:) ! Height in m
      REAL                  , pointer :: p        (:) ! Height QC
      REAL                  , pointer :: zk       (:) ! k-coordinates

      TYPE (field_type)     , pointer :: u        (:) ! u-wind.
      TYPE (field_type)     , pointer :: v        (:) ! v-wind.
      TYPE (field_type)     , pointer :: t        (:) ! temperature.
   END TYPE airep_type

   TYPE pilot_type
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc

      REAL                  , pointer :: p        (:) ! Height in m
      REAL                  , pointer :: zk       (:) ! k-coordinates

      TYPE (field_type)     , pointer :: u        (:) ! u-wind.
      TYPE (field_type)     , pointer :: v        (:) ! v-wind.
   END TYPE pilot_type

   TYPE bogus_type
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc

      REAL                  , pointer :: h        (:) ! Height in m
      REAL                  , pointer :: p        (:) ! pressure.
      REAL                  , pointer :: zk       (:) ! k-coordinates

      TYPE (field_type)     , pointer :: u        (:) ! u-wind.
      TYPE (field_type)     , pointer :: v        (:) ! v-wind.
      TYPE (field_type)     , pointer :: t        (:) ! temperature.
      TYPE (field_type)     , pointer :: q        (:) ! q.
      TYPE (field_type)               :: slp          ! sea level pressure.
   END TYPE bogus_type

   TYPE satem_type
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc

      REAL                            :: ref_p        ! Reference pressure
      REAL                  , pointer :: p        (:) ! Multi-level pressure

      TYPE (field_type)     , pointer :: thickness(:) ! Thickness.
   END TYPE satem_type

   TYPE geoamv_type
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc

      REAL                  , pointer :: p        (:) ! Height in Pa
      REAL                  , pointer :: zk       (:) ! k-coordinates

      TYPE (field_type)     , pointer :: u        (:) ! u-wind.
      TYPE (field_type)     , pointer :: v        (:) ! v-wind.
   END TYPE geoamv_type

   TYPE polaramv_type
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc

      REAL                  , pointer :: p        (:) ! Height in Pa
      REAL                  , pointer :: zk       (:) ! k-coordinates

      TYPE (field_type)     , pointer :: u        (:) ! u-wind.
      TYPE (field_type)     , pointer :: v        (:) ! v-wind.
   END TYPE polaramv_type

   TYPE gpsref_type
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc

      REAL             , pointer :: h  (:)      ! Multi-level height
      REAL             , pointer :: zk (:)      ! k-coordinates

      TYPE (field_type), pointer :: ref(:)      ! GPS Refractivity
      TYPE (field_type), pointer :: p  (:)      ! Retrieved P from Ref.
      TYPE (field_type), pointer :: t  (:)      ! Retrieved T from Ref.
      TYPE (field_type), pointer :: q  (:)      ! From NCEP analysis.
   END TYPE gpsref_type

!  TYPE metar_type
!     TYPE (info_type)        :: info
!     TYPE (model_loc_type)   :: loc

!     REAL                    :: h              ! Height in m
!     REAL                    :: zk             ! k-coordinates

!     TYPE (field_type)       :: u              ! u-wind.
!     TYPE (field_type)       :: v              ! v-wind.
!     TYPE (field_type)       :: t              ! temperature.
!     TYPE (field_type)       :: p              ! pressure.
!     TYPE (field_type)       :: q              ! q.
!  END TYPE metar_type

!  TYPE ships_type
!     TYPE (info_type)        :: info
!     TYPE (model_loc_type)   :: loc

!     REAL                    :: h              ! Height in m
!     REAL                    :: zk             ! k-coordinates

!     TYPE (field_type)       :: u              ! u-wind.
!     TYPE (field_type)       :: v              ! v-wind.
!     TYPE (field_type)       :: t              ! temperature.
!     TYPE (field_type)       :: p              ! pressure.
!     TYPE (field_type)       :: q              ! q.
!  END TYPE ships_type

   TYPE synop_type
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc

      REAL                    :: h              ! Height in m
      REAL                    :: zk             ! k-coordinates

      TYPE (field_type)       :: u              ! u-wind.
      TYPE (field_type)       :: v              ! v-wind.
      TYPE (field_type)       :: t              ! temperature.
      TYPE (field_type)       :: p              ! pressure.
      TYPE (field_type)       :: q              ! q.
   END TYPE synop_type

   TYPE sound_type
      TYPE (info_type)      :: info
      TYPE (model_loc_type) :: loc

      real                  , pointer :: h        (:) ! Height in m
      real                  , pointer :: p        (:) ! pressure.
      real                  , pointer :: zk       (:) ! k-coordinates

      TYPE (field_type)     , pointer :: u        (:) ! u-wind.
      TYPE (field_type)     , pointer :: v        (:) ! v-wind.
      TYPE (field_type)     , pointer :: t        (:) ! temperature.
      TYPE (field_type)     , pointer :: q        (:) ! q.
   END TYPE sound_type

   TYPE gpspw_type
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc

      TYPE (field_type)       :: tpw  ! Toatl precipitable water cm from GPS
   END TYPE gpspw_type

   TYPE ssmi_retrieval_type
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc

      TYPE (field_type)       :: Speed          ! Wind speed in m/s
      TYPE (field_type)       :: tpw            ! Toatl precipitable water cm
   END TYPE ssmi_retrieval_type

   TYPE ssmi_tb_type
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc

      TYPE (field_type)       :: tb19v          ! Brightness T (k) 19V
      TYPE (field_type)       :: tb19h          ! Brightness T (k) 19H
      TYPE (field_type)       :: tb22v          ! Brightness T (k) 22V
      TYPE (field_type)       :: tb37v          ! Brightness T (k) 37V
      TYPE (field_type)       :: tb37h          ! Brightness T (k) 37H
      TYPE (field_type)       :: tb85v          ! Brightness T (k) 85V
      TYPE (field_type)       :: tb85h          ! Brightness T (k) 85H
   END TYPE ssmi_tb_type
   
   TYPE ssmt1_type
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc
      
      REAL                  , pointer :: h        (:) ! Height in m
      REAL                  , pointer :: p        (:) ! Pressure in Pa.
      real                  , pointer :: zk       (:) ! k-coordinates

      TYPE (field_type)     , pointer :: t        (:) ! temperature.
   END TYPE ssmt1_type

   TYPE ssmt2_type
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc
      
      REAL                  , pointer :: h        (:) ! Height in m
      REAL                  , pointer :: p        (:) ! Pressure in Pa.
      real                  , pointer :: zk       (:) ! k-coordinates

      TYPE (field_type)     , pointer :: rh       (:) ! Relative humidity.
   END TYPE ssmt2_type

   TYPE pseudo_type
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc

!     REAL                    :: h              ! Height in m
      REAL                    :: zk             ! k-coordinates

      TYPE (field_type)       :: u              ! u-wind.
      TYPE (field_type)       :: v              ! v-wind.
      TYPE (field_type)       :: t              ! Temperature.
      TYPE (field_type)       :: p              ! Pressure.
      TYPE (field_type)       :: q              ! Specific Humidity.
   END TYPE pseudo_type

   TYPE qscat_type
      TYPE (info_type)        :: info
      TYPE (model_loc_type)   :: loc

      REAL                    :: h              ! Height in m
      REAL                    :: zk             ! k-coordinates

      TYPE (field_type)       :: u              ! u-wind.
      TYPE (field_type)       :: v              ! v-wind.
   END TYPE qscat_type

   TYPE ob_numb_type
      INTEGER :: total, &
                 synop, & 
                 sound, &
                 geoamv,&
                 polaramv,&
                 pilot, &
                 bogus, &
                 satem, &
                 airep, &
                 metar, &
                 ships, &
                 gpspw, &
                 gpsref, &
                 ssmi_tb, &
                 ssmi_retrieval, &
                 ssmt1, &
                 ssmt2, &
                 pseudo, &
                 qscat, &
                 profiler, &
                 buoy, &
                 Radar
   END TYPE ob_numb_type

   TYPE ob_type
      TYPE(ob_numb_type), DIMENSION(0:max_fgat_time) :: ob_numb

      INTEGER :: current_ob_time

      INTEGER :: total_obs, num_synop, &
                 num_sound, num_geoamv, num_polaramv, &
                 num_pilot, num_satem, &
                 num_airep, num_metar, &
                 num_ships, num_gpspw, &
                 num_ssmi_tb, num_ssmi_retrieval, &
                 num_ssmt1, num_ssmt2, num_pseudo, &
                 num_qscat, num_profiler, num_buoy, &
                 num_Radar, num_gpsref, num_bogus

      INTEGER :: num_synop_glo, &
                 num_sound_glo, num_geoamv_glo, num_polaramv_glo, &
                 num_pilot_glo, num_satem_glo, &
                 num_airep_glo, num_metar_glo, &
                 num_ships_glo, num_gpspw_glo, &
                 num_ssmi_tb_glo, num_ssmi_retrieval_glo, &
                 num_ssmt1_glo, num_ssmt2_glo, num_pseudo_glo, &
                 num_qscat_glo, num_profiler_glo, num_buoy_glo, &
                 num_Radar_glo, num_gpsref_glo, num_bogus_glo

      real    :: synop_ef_u, synop_ef_v, synop_ef_t, synop_ef_p, synop_ef_q
      real    :: metar_ef_u, metar_ef_v, metar_ef_t, metar_ef_p, metar_ef_q
      real    :: ships_ef_u, ships_ef_v, ships_ef_t, ships_ef_p, ships_ef_q
      real    :: geoamv_ef_u, geoamv_ef_v
      real    :: polaramv_ef_u, polaramv_ef_v
      real    :: gpspw_ef_tpw
      real    :: sound_ef_u, sound_ef_v, sound_ef_t, sound_ef_p, sound_ef_q
      real    :: airep_ef_u, airep_ef_v, airep_ef_t
      real    :: pilot_ef_u, pilot_ef_v
      real    :: ssmir_ef_speed, ssmir_ef_tpw
      real    :: satem_ef_thickness, ssmt1_ef_t, ssmt2_ef_rh
      real    :: gpsref_ef_ref, gpsref_ef_p, gpsref_ef_t, gpsref_ef_q
      real    :: qscat_ef_u, qscat_ef_v
      real    :: profiler_ef_u, profiler_ef_v
      real    :: buoy_ef_u, buoy_ef_v, buoy_ef_t, buoy_ef_p, buoy_ef_q
      real    :: Radar_ef_rv, Radar_ef_rf
      real    :: bogus_ef_u, bogus_ef_v, bogus_ef_t, bogus_ef_p, bogus_ef_q, bogus_ef_slp

      TYPE (sound_type)         , pointer :: sound(:)
      TYPE (synop_type)         , pointer :: sonde_sfc(:)
      TYPE (airep_type)         , pointer :: airep(:)
      TYPE (pilot_type)         , pointer :: pilot(:)
      TYPE (satem_type)         , pointer :: satem(:)
      TYPE (geoamv_type)        , pointer :: geoamv(:)
      TYPE (polaramv_type)        , pointer :: polaramv(:)
      TYPE (synop_type)         , pointer :: synop(:)
      TYPE (synop_type)         , pointer :: metar(:)
      TYPE (synop_type)         , pointer :: ships(:)
      TYPE (gpspw_type)         , pointer :: gpspw(:)
      TYPE (gpsref_type)        , pointer :: gpsref(:)
      TYPE (ssmi_tb_type)       , pointer :: ssmi_tb(:)
      TYPE (ssmi_retrieval_type), pointer :: ssmi_retrieval(:)
      TYPE (ssmt1_type)         , pointer :: ssmt1(:)
      TYPE (ssmt2_type)         , pointer :: ssmt2(:)
      TYPE (pseudo_type)        , pointer :: pseudo(:)
      TYPE (qscat_type)         , pointer :: qscat(:)
      TYPE (synop_type)         , pointer :: buoy(:)
      TYPE (pilot_type)         , pointer :: profiler(:)
      TYPE (bogus_type)         , pointer :: bogus(:)
      TYPE (Radar_type)         , pointer :: Radar(:)

      REAL :: missing
      REAL :: ptop

   END TYPE ob_type

!  [3.3] Where are these used:?

   TYPE number
      integer                    :: bad
      integer                    :: miss
      integer                    :: use
   END TYPE number

   TYPE bad_info
      type (number)              :: num
      integer, dimension(100000) :: nn
      integer, dimension(100000) :: kk
   END TYPE bad_info

   TYPE  bad_data_type
      type (bad_info)       :: u
      type (bad_info)       :: v
      type (bad_info)       :: t
      type (bad_info)       :: p
      type (bad_info)       :: q
      type (bad_info)       :: tpw
      type (bad_info)       :: Speed
      type (bad_info)       :: gpsref
      type (bad_info)       :: thickness
      type (bad_info)       :: rh
      type (bad_info)       :: rv
      type (bad_info)       :: rf
      type (bad_info)       :: slp
   END TYPE bad_data_type

   TYPE count_obs_number_type
        integer                                 :: num_used
        integer                                 :: num_outside_iyjx
        integer                                 :: num_max_err_chk
        integer                                 :: num_missing
   END TYPE count_obs_number_type
 
   TYPE count_obs_type

        TYPE (count_obs_number_type)  :: total_obs, num_synop, &
                                         num_sound, num_geoamv, num_polaramv,&
                                         num_pilot, num_satem, &
                                         num_airep, num_metar, &
                                         num_ships, num_gpspw, &
                                         num_gpsref, &
                                         num_ssmi_retrieval,   &
                                         num_ssmi_tb, &
                                         num_ssmt1, num_ssmt2, &
                                         num_qscat, &
                                         num_profiler, &
                                         num_buoy, &
                                         num_Radar, num_bogus, &
                                         num_other  

   END TYPE count_obs_type

!-----------------------------------------------------------------------------
!  [3.0] Observation/residual structure definition:
!-----------------------------------------------------------------------------

   TYPE residual_synop_type
      REAL :: u                                 ! u-wind.
      REAL :: v                                 ! v-wind.
      REAL :: t                                 ! temperature.
      REAL :: p                                 ! pressure.
      REAL :: q                                 ! q.
   END TYPE residual_synop_type

   TYPE residual_qscat_type
      REAL :: u                                 ! u-wind.
      REAL :: v                                 ! v-wind.
   END TYPE residual_qscat_type

   TYPE residual_geoamv_type
      REAL, POINTER :: u(:)                     ! u-wind.
      REAL, POINTER :: v(:)                     ! v-wind.
   END TYPE residual_geoamv_type

   TYPE residual_polaramv_type
      REAL, POINTER :: u(:)                     ! u-wind.
      REAL, POINTER :: v(:)                     ! v-wind.
   END TYPE residual_polaramv_type

   TYPE residual_gpspw_type
      REAL :: tpw                               ! Total precipitable water.
   END TYPE residual_gpspw_type

   TYPE residual_sound_type
      REAL, POINTER :: u(:)                     ! u-wind.
      REAL, POINTER :: v(:)                     ! v-wind.
      REAL, POINTER :: t(:)                     ! temperature.
      REAL, POINTER :: q(:)                     ! specific humidity.
   END TYPE residual_sound_type

   TYPE residual_airep_type
      REAL, POINTER :: u(:)                     ! u-wind.
      REAL, POINTER :: v(:)                     ! v-wind.
      REAL, POINTER :: t(:)                     ! temperature.
   END TYPE residual_airep_type

   TYPE residual_pilot_type
      REAL, POINTER :: u(:)                     ! u-wind.
      REAL, POINTER :: v(:)                     ! v-wind.
   END TYPE residual_pilot_type

   TYPE residual_bogus_type
      REAL, POINTER :: u(:)                     ! u-wind.
      REAL, POINTER :: v(:)                     ! v-wind.
      REAL, POINTER :: t(:)                     ! temperature.
      REAL, POINTER :: q(:)                     ! specific humidity.
      REAL          :: slp                      ! sea-level pressure.
   END TYPE residual_bogus_type

   TYPE residual_satem_type
      REAL, POINTER :: thickness(:)             ! Thickness.
   END TYPE residual_satem_type

   TYPE residual_gpsref_type
      REAL, POINTER :: ref(:)         ! GPS Refractivity
      REAL, POINTER :: p  (:)         ! GPS Retrived p from Refractivity
      REAL, POINTER :: t  (:)         ! GPS Retrived t from Refractivity
      REAL, POINTER :: q  (:)         ! q from NCEP used by CDAAC in retrieval
   END TYPE residual_gpsref_type

   TYPE residual_ssmi_retrieval_type
        REAL                    :: tpw      ! Toatl precipitable water cm
        REAL                    :: Speed    ! Wind speed m/s
   END TYPE residual_ssmi_retrieval_type

   TYPE residual_ssmi_tb_type
        REAL                    :: tb19v          ! Brightness T (k) 19V
        REAL                    :: tb19h          ! Brightness T (k) 19H
        REAL                    :: tb22v          ! Brightness T (k) 22V
        REAL                    :: tb37v          ! Brightness T (k) 37V
        REAL                    :: tb37h          ! Brightness T (k) 37H
        REAL                    :: tb85v          ! Brightness T (k) 85V
        REAL                    :: tb85h          ! Brightness T (k) 85H
   END TYPE residual_ssmi_tb_type
   
   TYPE residual_ssmt1_type
      REAL, POINTER :: t(:)                       ! temperature.
   END TYPE residual_ssmt1_type
   
   TYPE residual_ssmt2_type
      REAL, POINTER :: rh(:)                      ! Relative Humidity.
   END TYPE residual_ssmt2_type

   TYPE residual_pseudo_type
      REAL :: u                                   ! u-wind.
      REAL :: v                                   ! v-wind.
      REAL :: t                                   ! temperature.
      REAL :: p                                   ! pressure.
      REAL :: q                                   ! specific humidity.
   END TYPE residual_pseudo_type

   TYPE residual_Radar_type
      REAL, POINTER :: rv(:)                    ! rv
      REAL, POINTER :: rf(:)                    ! rf
   END TYPE residual_Radar_type

   TYPE y_type
        TYPE(ob_numb_type) :: ob_numb

        INTEGER :: total_obs, num_synop, &
                   num_sound, num_geoamv, num_polaramv, &
                   num_pilot, num_satem, &
                   num_airep, num_metar, &
                   num_ships, num_gpspw, &
                   num_ssmi_tb, num_ssmi_retrieval, &
                   num_ssmt1, num_ssmt2, num_pseudo, &
                   num_qscat, num_profiler, num_buoy, &
                   num_Radar, num_gpsref, num_bogus

        TYPE (residual_synop_type), POINTER :: synop(:)
        TYPE (residual_synop_type), POINTER :: metar(:) ! Same as synop type
        TYPE (residual_synop_type), POINTER :: ships(:) ! Same as synop type
        TYPE (residual_geoamv_type), POINTER :: geoamv(:)
        TYPE (residual_polaramv_type), POINTER :: polaramv(:)
        TYPE (residual_gpspw_type ), POINTER :: gpspw (:)
        TYPE (residual_gpsref_type), POINTER :: gpsref(:)
        TYPE (residual_sound_type), POINTER :: sound(:)
        TYPE (residual_bogus_type), POINTER :: bogus(:)
        TYPE (residual_synop_type), POINTER :: sonde_sfc(:) ! Same as synop type
        TYPE (residual_airep_type), POINTER :: airep(:)
        TYPE (residual_pilot_type), POINTER :: pilot(:)
        TYPE (residual_satem_type), POINTER :: satem(:)
        TYPE (residual_ssmi_tb_type), POINTER        :: ssmi_tb(:)
        TYPE (residual_ssmi_retrieval_type), POINTER :: ssmi_retrieval(:)
        TYPE (residual_ssmt1_type), POINTER :: ssmt1(:)
        TYPE (residual_ssmt2_type), POINTER :: ssmt2(:)
        TYPE (residual_pseudo_type), POINTER:: pseudo(:)
        TYPE (residual_qscat_type), POINTER :: qscat(:)
        TYPE (residual_synop_type),  POINTER :: buoy(:) ! Same as synop type
        TYPE (residual_pilot_type), POINTER :: profiler(:) ! Same as pilot type
        TYPE (residual_Radar_type), POINTER :: Radar(:)
   END TYPE y_type

!-----------------------------------------------------------------------------
!  [4.0] Control variable structure:
!-----------------------------------------------------------------------------

! Max/Min type:

   TYPE maxmin_type
        REAL                       :: value
        INTEGER                    :: n, l
   END TYPE maxmin_type

!-----------------------------------------------------------------------------
!  [5.0] Control variable structure:
!-----------------------------------------------------------------------------

   type jo_type
      real                :: total
      real                :: synop_u, synop_v, synop_t, synop_p, synop_q
      real                :: metar_u, metar_v, metar_t, metar_p, metar_q
      real                :: ships_u, ships_v, ships_t, ships_p, ships_q
      real                :: geoamv_u, geoamv_v
      real                :: polaramv_u, polaramv_v
      real                :: gpspw_tpw, satem_thickness, gpsref_ref
      real                :: sound_u, sound_v, sound_t, sound_q
      real                :: sonde_sfc_u, sonde_sfc_v, sonde_sfc_t, sonde_sfc_p, sonde_sfc_q
      real                :: airep_u, airep_v, airep_t
      real                :: pilot_u, pilot_v
      real                :: ssmir_speed, ssmir_tpw
      real                :: ssmi_tb19v, ssmi_tb19h, ssmi_tb22v, ssmi_tb37v, &
                             ssmi_tb37h, ssmi_tb85v, ssmi_tb85h
      real                :: ssmt1_t, ssmt2_rh
      real                :: pseudo_u, pseudo_v, pseudo_t, pseudo_p, pseudo_q
      real                :: qscat_u, qscat_v
      real                :: profiler_u, profiler_v
      real                :: buoy_u, buoy_v, buoy_t, buoy_p, buoy_q
      real                :: Radar_rv, Radar_rf
      real                :: bogus_u, bogus_v, bogus_t, bogus_q, bogus_slp
   end type jo_type

   type j_type
      real             :: total
      real             :: jb
      type (jo_type)   :: jo
   end type j_type

   TYPE cv_type
      REAL             :: j_grad_norm_target ! Target norm(Grad_v J) 

      INTEGER          :: option             ! 1 = use u, v,t,q,p, 2 = use psi, chi etc.
      INTEGER          :: size               ! Total size of control variable.
      INTEGER          :: size1              ! Size of CV array of 1st variable error.
      INTEGER          :: size2              ! Size of CV array of 2nd variable error.
      INTEGER          :: size3              ! Size of CV array of 3rd variable error.
      INTEGER          :: size4              ! Size of CV array of 4th variable error.
      INTEGER          :: size5              ! Size of CV array of 5th variable error.
      real, POINTER    :: array(:)           ! Control variables v.

   END TYPE cv_type

   TYPE be_subtype
      INTEGER           :: mz                ! Vertical truncation of errors.
      CHARACTER*5       :: name              ! Variable name.
      REAL, POINTER     :: rf_alpha(:)       ! RF scale length.
      REAL, POINTER     :: val(:,:)          ! LOcal Standard dev./sqrt(eigenvalue).
      REAL, POINTER     :: evec(:,:,:)       ! LOcal Vertical eigenvectors.
      REAL, POINTER     :: val_g(:)          ! Global Standard dev./sqrt(eigenvalue).
      REAL, POINTER     :: evec_g(:,:)       ! Global Vertical eigenvectors.
      REAL, POINTER     :: power(:,:)        ! Power spectrum
   END TYPE be_subtype

   TYPE be_type
!     Grid point errors:
      INTEGER           :: mix
      INTEGER           :: mjy
      TYPE (be_subtype) :: v1
      TYPE (be_subtype) :: v2
      TYPE (be_subtype) :: v3
      TYPE (be_subtype) :: v4
      TYPE (be_subtype) :: v5
      REAL, POINTER     :: pb_vert_reg(:,:,:)
!
!     Control variable space errors:
      TYPE (cv_type)    :: cv

!-----For cv option 3:
      INTEGER          :: ndeg,nta
      REAL             :: swidth
      REAL, POINTER    :: be(:)
      REAL, POINTER    :: rate(:)
      REAL, POINTER    :: table(:,:)
      REAL, POINTER    :: agvz(:,:,:,:)
      REAL, POINTER    :: bvz(:,:,:)
      REAL, POINTER    :: wgvz(:,:,:)
      REAL, POINTER    :: slix(:,:,:,:)
      REAL, POINTER    :: slipx(:,:)
      REAL, POINTER    :: sljy(:,:,:,:)
      REAL, POINTER    :: sljpy(:,:)
      REAL, POINTER    :: vz(:,:,:,:)
      REAL, POINTER    :: corz(:,:,:,:)
      REAL, POINTER    :: corp(:,:)
!-----For cv option 4 and 5:
      REAL, POINTER     :: reg_chi(:,:)
      REAL, POINTER     :: reg_t  (:,:,:)
      REAL, POINTER     :: reg_ps (:,:)
   END TYPE be_type

!  Analysis_Stats maximum-minumum structure.

   TYPE maxmin_field_type
      REAL                         :: value
      INTEGER                      :: i, j
   END TYPE maxmin_field_type


!  vp_type is defined in the Registry
!  x_type  is defined in the Registry
!  The framework allocates the (local-grid) xa structure.
!  The framework allocates the (local-grid) xb structure.
!  The framework (de)allocates the vv structure.
!  The framework (de)allocates the vp structure.

CONTAINS

#include "DA_Allocate_Background_Errors.inc"
#include "DA_Allocate_MM5_Model.inc"
#include "DA_Allocate_Observations.inc"
#include "DA_Allocate_Y.inc"
#include "DA_Deallocate_Background_Errors.inc"
#include "DA_Deallocate_MM5_Model.inc"
#include "DA_Deallocate_Observations.inc"
#include "DA_Deallocate_Y.inc"
#include "DA_Zero_X.inc"
#include "DA_Zero_Y.inc"
#include "DA_Zero_vp_type.inc"
#include "da_allocate_cv.inc"
#include "da_deallocate_cv.inc"
#include "da_gauss_noise.inc"

END MODULE DA_Define_Structures

