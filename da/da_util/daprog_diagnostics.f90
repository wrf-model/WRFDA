program da_diagnostics

   implicit none

   integer, parameter            :: y_unit = 50
   integer, parameter            :: unit1 = 35
   integer, parameter            :: unit2 = 36
   integer, parameter            :: unit3 = 37
   integer, parameter            :: unit4 = 38
   integer, parameter            :: unit5 = 39

   integer, parameter            :: obs_qc_pointer = 0   
   real, parameter               :: missing_r = -888888.0

   character*200                 :: filename
   integer                       :: n, k, ounit, current_time

   type info_type
      character*5                :: id
      integer                    :: time
      real                       :: lat
      real                       :: lon
   end type info_type
   
   type field_type
      real                       :: yo
      real                       :: omb
      integer                    :: qc
      real                       :: err
      real                       :: oma
   end type field_type

   type surfc_type
      type (info_type)           :: info
      real                       :: pressure
      type (field_type)          :: u
      type (field_type)          :: v
      type (field_type)          :: t
      type (field_type)          :: p
      type (field_type)          :: q
   end type surfc_type
   
   type qscat_type
      type (info_type)           :: info
      real                       :: height
      type (field_type)          :: u
      type (field_type)          :: v
   end type qscat_type

   type geamv_type
      type (info_type)           :: info
      real                       :: height
      type (field_type)          :: u
      type (field_type)          :: v
   end type geamv_type

   type poamv_type
      type (info_type)           :: info
      character*2                :: channel 
      character*1                :: landmask
      real                       :: pressure
      type (field_type)          :: u
      type (field_type)          :: v
   end type poamv_type
   
   type gpspw_type
      type (info_type)           :: info
      type (field_type)          :: tpw
   end type gpspw_type

   type sound_type
      type (info_type)           :: info
      integer                    :: numlevs
      real, pointer              :: pressure(:)
      type (field_type), pointer :: u(:)
      type (field_type), pointer :: v(:)
      type (field_type), pointer :: t(:)
      type (field_type), pointer :: p(:)
      type (field_type), pointer :: q(:)
   end type sound_type
   
   type airep_type
      type (info_type)           :: info
      integer                    :: numlevs
      real, pointer              :: pressure(:)
      type (field_type), pointer :: u(:)
      type (field_type), pointer :: v(:)
      type (field_type), pointer :: t(:)
   end type airep_type

   type pilot_type
      type (info_type)           :: info
      integer                    :: numlevs
      real, pointer              :: height(:)
      type (field_type), pointer :: u(:)
      type (field_type), pointer :: v(:)
   end type pilot_type

   type ssmir_type
      type (info_type)           :: info
      type (field_type) :: speed
      type (field_type) :: tpw
   end type ssmir_type
   
   type satem_type
      type (info_type)           :: info
      integer                    :: numlevs
      real, pointer              :: pressure(:)
      type (field_type), pointer :: thickness(:)
   end type satem_type

   type ssmt1_type
      type (info_type)           :: info
      integer                    :: numlevs
      real, pointer              :: height(:)
      type (field_type), pointer :: t(:)
   end type ssmt1_type
   
   type ssmt2_type
      type (info_type)           :: info
      integer                    :: numlevs
      real, pointer              :: height(:)
      type (field_type), pointer :: rh(:)
   end type ssmt2_type

   type ob_type
      integer                    :: num_synop, num_metar, num_ships, &
                                    num_poamv, num_qscat, num_geamv, num_gpspw, num_sound, &
                                    num_airep, num_pilot, num_ssmir, &
                                    num_satem, num_ssmt1, num_ssmt2

      type (surfc_type), pointer :: synop(:)
      type (surfc_type), pointer :: metar(:)
      type (surfc_type), pointer :: ships(:)
      type (poamv_type), pointer :: poamv(:)
      type (geamv_type), pointer :: geamv(:)
      type (qscat_type), pointer :: qscat(:)
      type (gpspw_type), pointer :: gpspw(:)
      type (sound_type), pointer :: sound(:)
      type (airep_type), pointer :: airep(:)
      type (pilot_type), pointer :: pilot(:)
      type (ssmir_type), pointer :: ssmir(:)
      type (satem_type), pointer :: satem(:)
      type (ssmt1_type), pointer :: ssmt1(:)
      type (ssmt2_type), pointer :: ssmt2(:)
   end type ob_type

   type (ob_type)       :: ob
   
!--------------------------------------------------------------------------
!  [1.0] Count total number of observations and allocate arrays:
!--------------------------------------------------------------------------

   filename = 'daprog_diagnostics.in'
   open( y_unit, file = filename, status = 'unknown' )

   call da_count_obs( y_unit, ob )

!--------------------------------------------------------------------------
!  [2.0] Read in observation data:
!--------------------------------------------------------------------------

   call da_read_y( y_unit, ob )

!--------------------------------------------------------------------------
!  [3.0] Perform basic statistics for each ob type:
!--------------------------------------------------------------------------

   if ( ob % num_synop > 0 ) then
      call da_calc_stats( 'synop u  ', ob % num_synop, ob % synop % u )
      call da_calc_stats( 'synop v  ', ob % num_synop, ob % synop % v )
      call da_calc_stats( 'synop t  ', ob % num_synop, ob % synop % t )
      call da_calc_stats( 'synop p  ', ob % num_synop, ob % synop % p )
      call da_calc_stats( 'synop q  ', ob % num_synop, ob % synop % q )
      write(6,*)
   end if
   
   if ( ob % num_metar > 0 ) then
      call da_calc_stats( 'metar u  ', ob % num_metar, ob % metar % u )
      call da_calc_stats( 'metar v  ', ob % num_metar, ob % metar % v )
      call da_calc_stats( 'metar t  ', ob % num_metar, ob % metar % t )
      call da_calc_stats( 'metar p  ', ob % num_metar, ob % metar % p )
      call da_calc_stats( 'metar q  ', ob % num_metar, ob % metar % q )
      write(6,*)
   end if

   if ( ob % num_ships > 0 ) then
      call da_calc_stats( 'ships u  ', ob % num_ships, ob % ships % u )
      call da_calc_stats( 'ships v  ', ob % num_ships, ob % ships % v )
      call da_calc_stats( 'ships t  ', ob % num_ships, ob % ships % t )
      call da_calc_stats( 'ships p  ', ob % num_ships, ob % ships % p )
      call da_calc_stats( 'ships q  ', ob % num_ships, ob % ships % q )
      write(6,*)
   end if
   
   if ( ob % num_poamv > 0 ) then
      call da_calc_stats( 'poamv u  ', ob % num_poamv, ob % poamv % u )
      call da_calc_stats( 'poamv v  ', ob % num_poamv, ob % poamv % v )
      write(6,*)
   end if

   if ( ob % num_geamv > 0 ) then
      call da_calc_stats( 'geamv u  ', ob % num_geamv, ob % geamv % u )
      call da_calc_stats( 'geamv v  ', ob % num_geamv, ob % geamv % v )
      write(6,*)
   end if

   if ( ob % num_qscat > 0 ) then
      call da_calc_stats( 'qscat u  ', ob % num_qscat, ob % qscat % u )
      call da_calc_stats( 'qscat v  ', ob % num_qscat, ob % qscat % v )
      write(6,*)
   end if

   if ( ob % num_gpspw > 0 ) then
      call da_calc_stats( 'gpspw tpw', ob % num_gpspw, ob % gpspw % tpw)
      write(6,*)
   end if

   if ( ob % num_ssmir > 0 ) then
      call da_calc_stats( 'ssmir spd', ob % num_ssmir, ob % ssmir % speed)
      call da_calc_stats( 'ssmir tpw', ob % num_ssmir, ob % ssmir % tpw)
      write(6,*)
   end if
   
   if ( ob % num_sound > 0 ) call da_calc_stats_sound( ob % num_sound, ob % sound )
   if ( ob % num_airep > 0 ) call da_calc_stats_airep( ob % num_airep, ob % airep ) 
   if ( ob % num_pilot > 0 ) call da_calc_stats_pilot( ob % num_pilot, ob % pilot )
   if ( ob % num_satem > 0 ) call da_calc_stats_satem( ob % num_satem, ob % satem )
   if ( ob % num_ssmt1 > 0 ) call da_calc_stats_ssmt1( ob % num_ssmt1, ob % ssmt1 )
   if ( ob % num_ssmt2 > 0 ) call da_calc_stats_ssmt2( ob % num_ssmt2, ob % ssmt2 )

!--------------------------------------------------------------------------
!  [4.0] Write data for post-processing:
!--------------------------------------------------------------------------

!  [4.1] Sonde O-B:

   open( unit1, file = 'soundu_omb.dat', status = 'unknown' )
   open( unit2, file = 'soundv_omb.dat', status = 'unknown' )
   open( unit3, file = 'soundt_omb.dat', status = 'unknown' )
   open( unit4, file = 'soundp_omb.dat', status = 'unknown' )
   open( unit5, file = 'soundq_omb.dat', status = 'unknown' )

   current_time = 1
   do n = 1, ob % num_sound
      do k = 1, ob % sound(n) % numlevs
         call da_write_data( k, current_time, ob % num_sound, unit1, &
                             ob % sound(n) % info, ob % sound(n) % pressure(k), &
                             ob % sound(n) % u(k) )
         call da_write_data( k, current_time, ob % num_sound, unit2, &
                             ob % sound(n) % info, &
                             ob % sound(n) % pressure(k), ob % sound(n) % v(k) )
         call da_write_data( k, current_time, ob % num_sound, unit3, &
                             ob % sound(n) % info, &
                             ob % sound(n) % pressure(k), ob % sound(n) % t(k) )
         call da_write_data( k, current_time, ob % num_sound, unit4, &
                             ob % sound(n) % info, &
                             ob % sound(n) % pressure(k), ob % sound(n) % p(k) )
         call da_write_data( k, current_time, ob % num_sound, unit5, &
                             ob % sound(n) % info, &
                             ob % sound(n) % pressure(k), ob % sound(n) % q(k) )

      end do
      current_time = ob % sound(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit3,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit4,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit5,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 ); close( unit3 ); close( unit4 ); close( unit5 )

!  [4.2] Synop O-B:

   open( unit1, file = 'synopu_omb.dat', status = 'unknown' )
   open( unit2, file = 'synopv_omb.dat', status = 'unknown' )
   open( unit3, file = 'synopt_omb.dat', status = 'unknown' )
   open( unit4, file = 'synopp_omb.dat', status = 'unknown' )
   open( unit5, file = 'synopq_omb.dat', status = 'unknown' )

   current_time = 1
   do n = 1, ob % num_synop
      call da_write_data( 1, current_time, ob % num_synop, unit1, &
                          ob % synop(n) % info, &
                          ob % synop(n) % pressure, ob % synop(n) % u )
      call da_write_data( 1, current_time, ob % num_synop, unit2, &
                          ob % synop(n) % info, &
                          ob % synop(n) % pressure, ob % synop(n) % v )
      call da_write_data( 1, current_time, ob % num_synop, unit3, &
                          ob % synop(n) % info, &
                          ob % synop(n) % pressure, ob % synop(n) % t )
      call da_write_data( 1, current_time, ob % num_synop, unit4, &
                          ob % synop(n) % info, &
                          ob % synop(n) % pressure, ob % synop(n) % p )
      call da_write_data( 1, current_time, ob % num_synop, unit5, &
                          ob % synop(n) % info, &
                          ob % synop(n) % pressure, ob % synop(n) % q )
      current_time = ob % synop(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit3,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit4,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit5,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 ); close( unit3 ); close( unit4 ); close( unit5 )

!  [4.3] Metar O-B:

   open( unit1, file = 'metaru_omb.dat', status = 'unknown' )
   open( unit2, file = 'metarv_omb.dat', status = 'unknown' )
   open( unit3, file = 'metart_omb.dat', status = 'unknown' )
   open( unit4, file = 'metarp_omb.dat', status = 'unknown' )
   open( unit5, file = 'metarq_omb.dat', status = 'unknown' )

!--------------------------------------------------------------------------
!  [6.0] Write metar data for post-processing:
!--------------------------------------------------------------------------

   current_time = 1
   do n = 1, ob % num_metar
      call da_write_data( 1, current_time, ob % num_metar, unit1, &
                          ob % metar(n) % info, &
                          ob % metar(n) % pressure, ob % metar(n) % u )
      call da_write_data( 1, current_time, ob % num_metar, unit2, &
                          ob % metar(n) % info, &
                          ob % metar(n) % pressure, ob % metar(n) % v )
      call da_write_data( 1, current_time, ob % num_metar, unit3, &
                          ob % metar(n) % info, &
                          ob % metar(n) % pressure, ob % metar(n) % t )
      call da_write_data( 1, current_time, ob % num_metar, unit4, &
                          ob % metar(n) % info, &
                          ob % metar(n) % pressure, ob % metar(n) % p )
      call da_write_data( 1, current_time, ob % num_metar, unit5, &
                          ob % metar(n) % info, &
                          ob % metar(n) % pressure, ob % metar(n) % q )
      current_time = ob % metar(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit3,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit4,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit5,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 ); close( unit3 ); close( unit4 ); close( unit5 )

!  [4.4] Polar AMV O-B:

   open( unit1, file = 'poamvu_omb.dat', status = 'unknown' )
   open( unit2, file = 'poamvv_omb.dat', status = 'unknown' )

   current_time = 1
   do n = 1, ob % num_poamv
      call da_write_data( 1, current_time, ob % num_poamv, unit1, &
                          ob % poamv(n) % info, &
                          ob % poamv(n) % pressure, ob % poamv(n) % u )
      call da_write_data( 1, current_time, ob % num_poamv, unit2, &
                          ob % poamv(n) % info, &
                          ob % poamv(n) % pressure, ob % poamv(n) % v )
      current_time = ob % poamv(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 )

contains

subroutine da_write_data( lev, current_time, num_obs, ounit, info, p, field )

   implicit none

   integer, intent(in)           :: lev
   integer, intent(in)           :: current_time
   integer, intent(in)           :: num_obs
   integer, intent(in)           :: ounit
   type (info_type), intent(in)  :: info
   real, intent(in)              :: p
   type (field_type), intent(in) :: field

   if ( info % time == current_time ) then  ! New ob at same time:
      write(ounit,'(a5,2f9.3,3f17.7,i8)') info % id, info % lat, info % lon, &
                                          p, field % omb, field % err, field % qc
   else
      if ( lev == 1)write(ounit,'(a5,2f9.3,3f17.7,i8)')'*****', 0., 0., 0., 0., 0., 0
      write(ounit,'(a5,2f9.3,3f17.7,i8)') info % id, info % lat, info % lon, &
                                          p, field % omb, field % err, field % qc
   end if
   
end subroutine da_write_data

!--------------------------------------------------------------------------

subroutine da_count_obs( y_unit, ob )

   implicit none
   
   integer, intent(in)               :: y_unit
   type (ob_type), intent(inout)     :: ob

   character*5          :: ob_name
   integer              :: num_obs, num_times

!  [1] Initialize ob numbers:

   ob % num_synop = 0
   ob % num_metar = 0
   ob % num_ships = 0
   ob % num_poamv = 0
   ob % num_geamv = 0
   ob % num_qscat = 0
   ob % num_gpspw = 0
   ob % num_sound = 0
   ob % num_airep = 0
   ob % num_pilot = 0
   ob % num_ssmir = 0
   ob % num_satem = 0
   ob % num_ssmt1 = 0
   ob % num_ssmt2 = 0
   num_times = 0
      
!  [2] Loop through input file to count number of obs:

   do       ! loop over entire input file: (ends in *end*)
      do    ! loop over particular time (ends in *****)

         read(y_unit,'(a5,i8)')ob_name, num_obs
 
         if ( trim(ob_name) == '*****' .or. trim(ob_name) == '*end*' ) exit

         if ( trim(ob_name) == 'synop' ) then
            ob % num_synop = ob % num_synop + num_obs
         else if ( trim(ob_name) == 'metar' ) then
            ob % num_metar = ob % num_metar + num_obs
         else if ( trim(ob_name) == 'ships' ) then
            ob % num_ships = ob % num_ships + num_obs
         else if ( trim(ob_name) == 'poamv' ) then
            ob % num_poamv = ob % num_poamv + num_obs
         else if ( trim(ob_name) == 'geamv' ) then
            ob % num_geamv = ob % num_geamv + num_obs
         else if ( trim(ob_name) == 'qscat' ) then
            ob % num_qscat = ob % num_qscat + num_obs
         else if ( trim(ob_name) == 'gpspw' ) then
            ob % num_gpspw = ob % num_gpspw + num_obs
         else if ( trim(ob_name) == 'sound' ) then
            ob % num_sound = ob % num_sound + num_obs
         else if ( trim(ob_name) == 'airep' ) then
             ob % num_airep = ob % num_airep + num_obs
         else if ( trim(ob_name) == 'pilot' ) then
             ob % num_pilot = ob % num_pilot + num_obs
         else if ( trim(ob_name) == 'ssmir' ) then
             ob % num_ssmir = ob % num_ssmir + num_obs
         else if ( trim(ob_name) == 'satem' ) then
             ob % num_satem = ob % num_satem + num_obs
         else if ( trim(ob_name) == 'ssmt1' ) then
             ob % num_ssmt1 = ob % num_ssmt1 + num_obs
         else if ( trim(ob_name) == 'ssmt2' ) then
             ob % num_ssmt2 = ob % num_ssmt2 + num_obs
         end if
      end do
      
      if ( trim(ob_name) == '*end*' ) then
         exit
      else
         num_times = num_times + 1
      end if
   end do

   write(6,'(a,i8)')' Number of times read= ', num_times

!  [3] Allocate ob structures where obs exist:

   if ( ob % num_synop > 0 ) then
      allocate( ob % synop(1:ob % num_synop) )
      write(6,'(a,i8)')' Number of synop obs = ', ob % num_synop
   end if
   
   if ( ob % num_metar > 0 ) then
      allocate( ob % metar(1:ob % num_metar) )
      write(6,'(a,i8)')' Number of metar obs = ', ob % num_metar
   end if
   
   if ( ob % num_ships > 0 ) then
      allocate( ob % ships(1:ob % num_ships) )
      write(6,'(a,i8)')' Number of ships obs = ', ob % num_ships
   end if
   
   if ( ob % num_poamv > 0 ) then
      allocate( ob % poamv(1:ob % num_poamv) )
      write(6,'(a,i8)')' Number of poamv obs = ', ob % num_poamv
   end if
   
   if ( ob % num_geamv > 0 ) then
      allocate( ob % geamv(1:ob % num_geamv) )
      write(6,'(a,i8)')' Number of geamv obs = ', ob % num_geamv
   end if

   if ( ob % num_qscat > 0 ) then
      allocate( ob % qscat(1:ob % num_qscat) )
      write(6,'(a,i8)')' Number of qscat obs = ', ob % num_qscat
   end if

   if ( ob % num_gpspw > 0 ) then
      allocate( ob % gpspw(1:ob % num_gpspw) )
      write(6,'(a,i8)')' Number of gpspw obs = ', ob % num_gpspw
   end if
   
   if ( ob % num_sound > 0 ) then
      allocate( ob % sound(1:ob % num_sound) )
      write(6,'(a,i8)')' Number of sound obs = ', ob % num_sound
   end if
   
   if ( ob % num_airep > 0 ) then
      allocate( ob % airep(1:ob % num_airep) )
      write(6,'(a,i8)')' Number of airep obs = ', ob % num_airep
   end if
   
   if ( ob % num_pilot > 0 ) then
      allocate( ob % pilot(1:ob % num_pilot) )
      write(6,'(a,i8)')' Number of pilot obs = ', ob % num_pilot
   end if
   
   if ( ob % num_ssmir > 0 ) then
      allocate( ob % ssmir(1:ob % num_ssmir) )
      write(6,'(a,i8)')' Number of ssmir obs = ', ob % num_ssmir
   end if
   
   if ( ob % num_satem > 0 ) then
      allocate( ob % satem(1:ob % num_satem) )
      write(6,'(a,i8)')' Number of satem obs = ', ob % num_satem
   end if
   
   if ( ob % num_ssmt1 > 0 ) then
      allocate( ob % ssmt1(1:ob % num_ssmt1) )
      write(6,'(a,i8)')' Number of ssmt1 obs = ', ob % num_ssmt1
   end if
   
   if ( ob % num_ssmt2 > 0 ) then
      allocate( ob % ssmt2(1:ob % num_ssmt2) )
      write(6,'(a,i8)')' Number of ssmt2 obs = ', ob % num_ssmt2
   end if
 
  write(6,*)
 
end subroutine da_count_obs

!--------------------------------------------------------------------------

subroutine da_read_y( y_unit, ob )

   implicit none
   
   integer, intent(in)               :: y_unit
   type (ob_type), intent(inout)     :: ob

   character*5          :: ob_name
   integer              :: n, ndum, k, kdum, num_obs, num_levs
   integer              :: num_obs_synop, num_obs_metar, num_obs_ships
   integer              :: num_obs_qscat,  num_obs_poamv, num_obs_geamv
   integer              :: num_obs_gpspw, num_obs_sound, num_obs_airep, num_obs_pilot
   integer              :: num_obs_ssmir, num_obs_satem, num_obs_ssmt1, num_obs_ssmt2
   integer              :: synopt, metart, shipst, poamvt, geamvt, qscatt, gpspwt, soundt
   integer              :: airept, pilott, ssmirt, satemt, ssmt1t, ssmt2t
   real                 :: rdum

   rewind (y_unit)
   num_obs_synop = 0; num_obs_metar = 0; num_obs_ships = 0
   num_obs_poamv = 0; num_obs_geamv = 0; num_obs_qscat = 0
   num_obs_gpspw = 0; num_obs_sound = 0; num_obs_airep = 0; num_obs_pilot = 0
   num_obs_ssmir = 0; num_obs_satem = 0; num_obs_ssmt1 = 0; num_obs_ssmt2 = 0
   synopt = 0; metart = 0; shipst = 0; poamvt = 0; geamvt = 0; qscatt = 0
   gpspwt = 0; soundt = 0; airept = 0; pilott = 0
   ssmirt = 0; satemt = 0; ssmt1t = 0; ssmt2t = 0

   do

      read(y_unit,'(a5,i8)')ob_name, num_obs
      
      if ( trim(ob_name) == 'synop' ) then
         synopt = synopt + 1
         do n = num_obs_synop + 1, num_obs_synop + num_obs
            read(y_unit,'(2i8,a5,2f9.3,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % synop(n) % info % id, &        ! Station
                        ob % synop(n) % info % lat, &       ! Latitude
                        ob % synop(n) % info % lon, &       ! Longitude
                        ob % synop(n) % pressure, &         ! Obs height
                        ob % synop(n) % u, &                ! O, O-B, O-A
                        ob % synop(n) % v, &                ! O, O-B, O-A
                        ob % synop(n) % t, &                ! O, O-B, O-A
                        ob % synop(n) % p, &                ! O, O-B, O-A
                        ob % synop(n) % q                   ! O, O-B, O-A
            ob % synop(n) % info % time = synopt
         end do
         num_obs_synop = num_obs_synop + num_obs

      else if ( trim(ob_name) == 'metar' ) then
         metart = metart + 1
         do n = num_obs_metar + 1, num_obs_metar + num_obs
            read(y_unit,'(2i8,a5,2f9.3,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % metar(n) % info % id, &        ! Station
                        ob % metar(n) % info % lat, &       ! Latitude
                        ob % metar(n) % info % lon, &       ! Longitude
                        ob % metar(n) % pressure, &         ! Obs height
                        ob % metar(n) % u, &                ! O, O-B, O-A
                        ob % metar(n) % v, &                ! O, O-B, O-A
                        ob % metar(n) % t, &                ! O, O-B, O-A
                        ob % metar(n) % p, &                ! O, O-B, O-A
                        ob % metar(n) % q                   ! O, O-B, O-A
            ob % metar(n) % info % time = metart
         end do
         num_obs_metar = num_obs_metar + num_obs

      else if ( trim(ob_name) == 'ships' ) then
         shipst = shipst + 1
         do n = num_obs_ships + 1, num_obs_ships + num_obs
            read(y_unit,'(2i8,a5,2f9.3,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % ships(n) % info % id, &        ! Station
                        ob % ships(n) % info % lat, &       ! Latitude
                        ob % ships(n) % info % lon, &       ! Longitude
                        ob % ships(n) % pressure, &         ! Obs height
                        ob % ships(n) % u, &                ! O, O-B, O-A
                        ob % ships(n) % v, &                ! O, O-B, O-A
                        ob % ships(n) % t, &                ! O, O-B, O-A
                        ob % ships(n) % p, &                ! O, O-B, O-A
                        ob % ships(n) % q                   ! O, O-B, O-A
            ob % ships(n) % info % time = shipst
         end do
         num_obs_ships = num_obs_ships + num_obs
         
      else if ( trim(ob_name) == 'poamv' ) then
         poamvt = poamvt + 1
         do n = num_obs_poamv + 1, num_obs_poamv + num_obs
            read(y_unit,'(2i8,a5,2f9.3,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % poamv(n) % info % id, &        ! Station
                        ob % poamv(n) % info % lat, &       ! Latitude
                        ob % poamv(n) % info % lon, &       ! Longitude
                        ob % poamv(n) % pressure, &         ! Obs pressure
                        ob % poamv(n) % u, &                ! O, O-B, O-A
                        ob % poamv(n) % v
            ob % poamv(n) % info % time = poamvt
            ob % poamv(n) % channel = ob % poamv(n) % info % id(3:4)
            ob % poamv(n) % landmask = ob % poamv(n) % info % id(5:5)
         end do
         num_obs_poamv = num_obs_poamv + num_obs

      else if ( trim(ob_name) == 'geamv' ) then
         geamvt = geamvt + 1
         do n = num_obs_geamv + 1, num_obs_geamv + num_obs
            read(y_unit,'(2i8,a5,2f9.3,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % geamv(n) % info % id, &        ! Station
                        ob % geamv(n) % info % lat, &       ! Latitude
                        ob % geamv(n) % info % lon, &       ! Longitude
                        ob % geamv(n) % height, &           ! Obs height
                        ob % geamv(n) % u, &                ! O, O-B, O-A
                        ob % geamv(n) % v
            ob % geamv(n) % info % time = geamvt
         end do
         num_obs_geamv = num_obs_geamv + num_obs

      else if ( trim(ob_name) == 'qscat' ) then
         qscatt = qscatt + 1
         do n = num_obs_qscat + 1, num_obs_qscat + num_obs
            read(y_unit,'(2i8,a5,2f9.3,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % qscat(n) % info % id, &        ! Station
                        ob % qscat(n) % info % lat, &       ! Latitude
                        ob % qscat(n) % info % lon, &       ! Longitude
                        ob % qscat(n) % height, &           ! Obs height
                        ob % qscat(n) % u, &                ! O, O-B, O-A
                        ob % qscat(n) % v
            ob % qscat(n) % info % time = qscatt
         end do
         num_obs_qscat = num_obs_qscat + num_obs
         
      else if ( trim(ob_name) == 'gpspw' ) then
         gpspwt = gpspwt + 1
         do n = num_obs_gpspw + 1, num_obs_gpspw + num_obs
            read(y_unit,'(2i8,a5,2f9.3,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % gpspw(n) % info % id, &        ! Station
                        ob % gpspw(n) % info % lat, &       ! Latitude
                        ob % gpspw(n) % info % lon, &       ! Longitude
                        rdum, &                             ! Obs height
                        ob % gpspw(n) % tpw
            ob % gpspw(n) % info % time = gpspwt
         end do
         num_obs_gpspw = num_obs_gpspw + num_obs

      else if ( trim(ob_name) == 'sound' ) then
         soundt = soundt + 1
         do n = num_obs_sound + 1, num_obs_sound + num_obs
            read(y_unit,'(i8)')num_levs
            ob % sound(n) % numlevs = num_levs
            allocate( ob % sound(n) % pressure(1:num_levs) )
            allocate( ob % sound(n) % u(1:num_levs) )
            allocate( ob % sound(n) % v(1:num_levs) )
            allocate( ob % sound(n) % t(1:num_levs) )
            allocate( ob % sound(n) % p(1:num_levs) )
            allocate( ob % sound(n) % q(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.3,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % sound(n) % info % id, &     ! Station
                        ob % sound(n) % info % lat, &       ! Latitude
                        ob % sound(n) % info % lon, &       ! Longitude
                        ob % sound(n) % pressure(k), &      ! Obs height
                        ob % sound(n) % u(k), &             ! O, O-B, O-A
                        ob % sound(n) % v(k), &             ! O, O-B, O-A
                        ob % sound(n) % t(k), &             ! O, O-B, O-A
                        ob % sound(n) % p(k), &             ! O, O-B, O-A
                        ob % sound(n) % q(k)                ! O, O-B, O-A
            end do
            ob % sound(n) % info % time = soundt
         end do
         num_obs_sound = num_obs_sound + num_obs

      else if ( trim(ob_name) == 'airep' ) then
         airept = airept + 1
         do n = num_obs_airep + 1, num_obs_airep + num_obs
            read(y_unit,'(i8)')num_levs
            ob % airep(n) % numlevs = num_levs
            allocate( ob % airep(n) % pressure(1:num_levs) )
            allocate( ob % airep(n) % u(1:num_levs) )
            allocate( ob % airep(n) % v(1:num_levs) )
            allocate( ob % airep(n) % t(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.3,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % airep(n) % info % id, &     ! Station
                        ob % airep(n) % info % lat, &       ! Latitude
                        ob % airep(n) % info % lon, &       ! Longitude
                        ob % airep(n) % pressure(k), &      ! Obs height
                        ob % airep(n) % u(k), &             ! O, O-B, O-A
                        ob % airep(n) % v(k), &             ! O, O-B, O-A
                        ob % airep(n) % t(k)                ! O, O-B, O-A
            end do
            ob % airep(n) % info % time = airept
         end do
         num_obs_airep = num_obs_airep + num_obs

      else if ( trim(ob_name) == 'pilot' ) then
         pilott = pilott + 1
         do n = num_obs_pilot + 1, num_obs_pilot + num_obs
            read(y_unit,'(i8)')num_levs
            ob % pilot(n) % numlevs = num_levs
            allocate( ob % pilot(n) % height(1:num_levs) )
            allocate( ob % pilot(n) % u(1:num_levs) )
            allocate( ob % pilot(n) % v(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.3,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % pilot(n) % info % id, &     ! Station
                        ob % pilot(n) % info % lat, &       ! Latitude
                        ob % pilot(n) % info % lon, &       ! Longitude
                        ob % pilot(n) % height(k), &        ! Obs height
                        ob % pilot(n) % u(k), &             ! O, O-B, O-A
                        ob % pilot(n) % v(k)
            end do
            ob % pilot(n) % info % time = pilott
         end do
         num_obs_pilot = num_obs_pilot + num_obs

      else if ( trim(ob_name) == 'ssmir' ) then
         ssmirt = ssmirt + 1
         do n = num_obs_ssmir + 1, num_obs_ssmir + num_obs
            read(y_unit,'(2i8,a5,2f9.3,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % ssmir(n) % info % id, &     ! Station
                        ob % ssmir(n) % info % lat, &       ! Latitude
                        ob % ssmir(n) % info % lon, &       ! Longitude
                        rdum, &                             ! Obs height
                        ob % ssmir(n) % speed, &            ! O, O-B, O-A
                        ob % ssmir(n) % tpw
            ob % ssmir(n) % info % time = ssmirt
         end do
         num_obs_ssmir = num_obs_ssmir + num_obs

      else if ( trim(ob_name) == 'satem' ) then
         satemt = satemt + 1
         do n = num_obs_satem + 1, num_obs_satem + num_obs
            read(y_unit,'(i8)')num_levs
            ob % satem(n) % numlevs = num_levs
            allocate( ob % satem(n) % pressure(1:num_levs) )
            allocate( ob % satem(n) % thickness(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.3,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % satem(n) % info % id, &     ! Station
                        ob % satem(n) % info % lat, &       ! Latitude
                        ob % satem(n) % info % lon, &       ! Longitude
                        ob % satem(n) % pressure(k), &      ! Obs height
                        ob % satem(n) % thickness(k)        ! O, O-B, O-A
            end do
             ob % satem(n) % info % time = satemt
         end do
         num_obs_satem = num_obs_satem + num_obs

      else if ( trim(ob_name) == 'ssmt1' ) then
         ssmt1t = ssmt1t + 1
         do n = num_obs_ssmt1 + 1, num_obs_ssmt1 + num_obs
            read(y_unit,'(i8)')num_levs
            ob % ssmt1(n) % numlevs = num_levs
            allocate( ob % ssmt1(n) % height(1:num_levs) )
            allocate( ob % ssmt1(n) % t(1:num_levs) )
            
            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.3,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % ssmt1(n) % info % id, &     ! Station
                        ob % ssmt1(n) % info % lat, &       ! Latitude
                        ob % ssmt1(n) % info % lon, &       ! Longitude
                        ob % ssmt1(n) % height(k), &        ! Obs height
                        ob % ssmt1(n) % t(k)                ! O, O-B, O-A
            end do
            ob % ssmt1(n) % info % time = ssmt1t
         end do
         num_obs_ssmt1 = num_obs_ssmt1 + num_obs

      else if ( trim(ob_name) == 'ssmt2' ) then
         ssmt2t = ssmt2t + 1
         do n = num_obs_ssmt2 + 1, num_obs_ssmt2 + num_obs
            read(y_unit,'(i8)')num_levs
            ob % ssmt2(n) % numlevs = num_levs
            allocate( ob % ssmt2(n) % height(1:num_levs) )
            allocate( ob % ssmt2(n) % rh(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.3,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % ssmt2(n) % info % id, &     ! Station
                        ob % ssmt2(n) % info % lat, &       ! Latitude
                        ob % ssmt2(n) % info % lon, &       ! Longitude
                        ob % ssmt2(n) % height(k), &        ! Obs height
                        ob % ssmt2(n) % rh(k)               ! O, O-B, O-A
            end do
            ob % ssmt2(n) % info % time = ssmt2t
         end do
         num_obs_ssmt2 = num_obs_ssmt2 + num_obs

      else if ( trim(ob_name) == '*end*' ) then
         exit
      end if

   end do
   
end subroutine da_read_y

!--------------------------------------------------------------------------

subroutine da_calc_stats( ob_name, num_obs, field )


   implicit none

   character*9, intent(in)       :: ob_name ! Ob name   
   integer, intent(in)           :: num_obs ! Number of observations
   type (field_type), intent(in) :: field(:)! Obs data.

   integer                       :: n, count
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then
      count = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0

      do n = 1, num_obs   
         call da_increment_stats( field(n), count, &
                                  sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                  mean_omb, mean_oma, stdv_omb, stdv_oma )
      end do

      write(6,'(1x,a9,i8,a,i8,a,4f10.4)')ob_name, count, '/', num_obs, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

   end if

end subroutine da_calc_stats

!--------------------------------------------------------------------------

subroutine da_calc_stats_sound( num_obs, sound )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (sound_type), intent(in) :: sound(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then
 
      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, sound(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( sound(n) % u(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i8,a,i8,a,4f10.4)')'sound u  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, sound(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( sound(n) % v(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i8,a,i8,a,4f10.4)')'sound v  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, sound(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( sound(n) % t(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i8,a,i8,a,4f10.4)')'sound t  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, sound(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( sound(n) % p(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i8,a,i8,a,4f10.4)')'sound p  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, sound(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( sound(n) % q(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i8,a,i8,a,4f10.4)')'sound q  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if

end subroutine da_calc_stats_sound

subroutine da_calc_stats_airep( num_obs, airep )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (airep_type), intent(in) :: airep(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, airep(n) % numlevs
           count1 = count1 + 1
           call da_increment_stats( airep(n) % u(k), count, &
                                    sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                    mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i5,a,i5,a,4f10.4)')'airep u  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, airep(n) % numlevs
           count1 = count1 + 1
           call da_increment_stats( airep(n) % v(k), count, &
                                    sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                    mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i8,a,i8,a,4f10.4)')'airep v  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, airep(n) % numlevs
           count1 = count1 + 1
           call da_increment_stats( airep(n) % t(k), count, &
                                    sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                    mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i8,a,i8,a,4f10.4)')'airep t  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if

end subroutine da_calc_stats_airep

subroutine da_calc_stats_pilot( num_obs, pilot )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (pilot_type), intent(in) :: pilot(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, pilot(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( pilot(n) % u(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i8,a,i8,a,4f10.4)')'pilot u  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, pilot(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( pilot(n) % v(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i8,a,i8,a,4f10.4)')'pilot v  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if
end subroutine da_calc_stats_pilot

subroutine da_calc_stats_satem( num_obs, satem )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (satem_type), intent(in) :: satem(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, satem(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( satem(n) % thickness(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i8,a,i8,a,4f10.4)')'satem thk', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if

end subroutine da_calc_stats_satem

subroutine da_calc_stats_ssmt1( num_obs, ssmt1 )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (ssmt1_type), intent(in) :: ssmt1(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, ssmt1(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( ssmt1(n) % t(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i8,a,i8,a,4f10.4)')'ssmt1 t  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if

end subroutine da_calc_stats_ssmt1
 
subroutine da_calc_stats_ssmt2( num_obs, ssmt2 )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (ssmt2_type), intent(in) :: ssmt2(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, ssmt2(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( ssmt2(n) % rh(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i8,a,i8,a,4f10.4)')'ssmt2 rh ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if

end subroutine da_calc_stats_ssmt2

subroutine da_increment_stats( field, count, &
                               sum_omb, sum_oma, sum_omb2, sum_oma2, &
                               mean_omb, mean_oma, stdv_omb, stdv_oma )

   implicit none

   type (field_type), intent(in) :: field
   integer, intent(inout)        :: count
   real, intent(inout)           :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real, intent(out)             :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( field % qc >= obs_qc_pointer ) then
      count = count + 1
      sum_omb = sum_omb + field % omb
      sum_oma = sum_oma + field % oma
      sum_omb2 = sum_omb2 + field % omb**2
      sum_oma2 = sum_oma2 + field % oma**2
   end if

   if ( count > 0 ) then
      mean_omb = sum_omb / real(count)
      mean_oma = sum_oma / real(count)
      stdv_omb = sqrt( sum_omb2/ real(count) - mean_omb**2 )
      stdv_oma = sqrt( sum_oma2/ real(count) - mean_oma**2 )
   end if

end subroutine da_increment_stats

end program da_diagnostics
