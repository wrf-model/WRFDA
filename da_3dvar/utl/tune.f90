program tune

   implicit none

   integer, parameter            :: rand_unit = 45
   integer, parameter            :: yp_unit   = 46
   integer, parameter            :: y_unit    = 47
   integer, parameter            :: jo_unit   = 48
   integer, parameter            :: in_unit   = 49
   real, parameter               :: missing_r = -888888.0

   integer                       :: n
   
   type field_type
      real                       :: yp
      real                       :: y
      real                       :: error
      real                       :: pert
   end type field_type
   
   type surfc_type
      type (field_type)          :: u
      type (field_type)          :: v
      type (field_type)          :: t
      type (field_type)          :: p
      type (field_type)          :: q
   end type surfc_type
   
   type satob_type
      type (field_type)          :: u
      type (field_type)          :: v
   end type satob_type
   
   type gpspw_type
      type (field_type)          :: tpw
   end type gpspw_type

   type sound_type
      integer                    :: numlevs
      type (field_type), pointer :: u(:)
      type (field_type), pointer :: v(:)
      type (field_type), pointer :: t(:)
      type (field_type), pointer :: p(:)
      type (field_type), pointer :: q(:)
   end type sound_type
   
   type airep_type
      integer                    :: numlevs
      type (field_type), pointer :: u(:)
      type (field_type), pointer :: v(:)
      type (field_type), pointer :: t(:)
   end type airep_type

   type pilot_type
      integer                    :: numlevs
      type (field_type), pointer :: u(:)
      type (field_type), pointer :: v(:)
   end type pilot_type

   type ssmir_type
      type (field_type) :: speed
      type (field_type) :: tpw
   end type ssmir_type
   
   type satem_type
      integer                    :: numlevs
      type (field_type), pointer :: thickness(:)
   end type satem_type

   type ssmt1_type
      integer                    :: numlevs
      type (field_type), pointer :: t(:)
   end type ssmt1_type
   
   type ssmt2_type
      integer                    :: numlevs
      type (field_type), pointer :: rh(:)
   end type ssmt2_type

   type ob_type
      integer                    :: total_obs
      integer                    :: num_synop, num_metar, num_ships, &
                                    num_satob, num_gpspw, num_sound, &
                                    num_airep, num_pilot, num_ssmir, &
                                    num_satem, num_ssmt1, num_ssmt2
      integer                    :: num_synop_tot, num_metar_tot, num_ships_tot, &
                                    num_satob_tot, num_gpspw_tot, num_sound_tot, &
                                    num_airep_tot, num_pilot_tot, num_ssmir_tot, &
                                    num_satem_tot, num_ssmt1_tot, num_ssmt2_tot
      real                       :: trace_total
      real                       :: trace_synop, trace_metar, trace_ships, &
                                    trace_satob, trace_gpspw, trace_sound, &
                                    trace_airep, trace_pilot, trace_ssmir, &
                                    trace_satem, trace_ssmt1, trace_ssmt2
      real                       :: jo_synop_u, jo_synop_v, jo_synop_t, jo_synop_p, jo_synop_q
      real                       :: jo_metar_u, jo_metar_v, jo_metar_t, jo_metar_p, jo_metar_q
      real                       :: jo_ships_u, jo_ships_v, jo_ships_t, jo_ships_p, jo_ships_q
      real                       :: jo_satob_u, jo_satob_v, jo_gpspw_tpw
      real                       :: jo_sound_u, jo_sound_v, jo_sound_t, jo_sound_p, jo_sound_q
      real                       :: jo_airep_u, jo_airep_v, jo_airep_t
      real                       :: jo_pilot_u, jo_pilot_v, jo_pilot_t, jo_pilot_p, jo_pilot_q
      real                       :: jo_ssmir_speed, jo_ssmir_tpw, jo_satem_thickness
      real                       :: jo_ssmt1_t, jo_ssmt2_rh
      
      real                       :: joa_synop_u, joa_synop_v, joa_synop_t, joa_synop_p, joa_synop_q
      real                       :: joa_metar_u, joa_metar_v, joa_metar_t, joa_metar_p, joa_metar_q
      real                       :: joa_ships_u, joa_ships_v, joa_ships_t, joa_ships_p, joa_ships_q
      real                       :: joa_satob_u, joa_satob_v, joa_gpspw_tpw
      real                       :: joa_sound_u, joa_sound_v, joa_sound_t, joa_sound_p, joa_sound_q
      real                       :: joa_airep_u, joa_airep_v, joa_airep_t
      real                       :: joa_pilot_u, joa_pilot_v, joa_pilot_t, joa_pilot_p, joa_pilot_q
      real                       :: joa_ssmir_speed, joa_ssmir_tpw, joa_satem_thickness
      real                       :: joa_ssmt1_t, joa_ssmt2_rh

      real                       :: ef_synop_u, ef_synop_v, ef_synop_t, ef_synop_p, ef_synop_q
      real                       :: ef_metar_u, ef_metar_v, ef_metar_t, ef_metar_p, ef_metar_q
      real                       :: ef_ships_u, ef_ships_v, ef_ships_t, ef_ships_p, ef_ships_q
      real                       :: ef_satob_u, ef_satob_v, ef_gpspw_tpw
      real                       :: ef_sound_u, ef_sound_v, ef_sound_t, ef_sound_p, ef_sound_q
      real                       :: ef_airep_u, ef_airep_v, ef_airep_t
      real                       :: ef_pilot_u, ef_pilot_v
      real                       :: ef_ssmir_speed, ef_ssmir_tpw, ef_satem_thickness
      real                       :: ef_ssmt1_t, ef_ssmt2_rh

      type (surfc_type), pointer :: synop(:)
      type (surfc_type), pointer :: metar(:)
      type (surfc_type), pointer :: ships(:)
      type (satob_type), pointer :: satob(:)
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

   call da_count_obs( y_unit, ob )

!--------------------------------------------------------------------------
!  [2.0] Read in UNperturbed y = H(x_inc) from each ob type:
!--------------------------------------------------------------------------

   call da_read_y( y_unit, ob )
   
!--------------------------------------------------------------------------
!  [3.0] Read in perturbed yp = H(x_inc_p) from each ob type:
!--------------------------------------------------------------------------
 
   call da_read_yp( yp_unit, ob )

!--------------------------------------------------------------------------
!  [4.0] Read in perturbations and errors from each ob type:
!--------------------------------------------------------------------------

   call da_read_obs_rand( rand_unit, ob )

!--------------------------------------------------------------------------
!  [5.0] Calculate expected cost function J values:
!--------------------------------------------------------------------------

   call da_calc_jo_expected( ob )

!--------------------------------------------------------------------------
!  [6.0] Read actual cost function J and error tuning factors used:
!--------------------------------------------------------------------------

   call da_read_jo_actual( ob )

!--------------------------------------------------------------------------
!  [7.0] Calculate observation error tuning factors:
!--------------------------------------------------------------------------

   call da_calc_new_factors( ob )

!--------------------------------------------------------------------------
!  [8.0] Calculate observation error tuning factors:
!--------------------------------------------------------------------------

   call da_get_j( ob )

contains

!--------------------------------------------------------------------------

subroutine da_count_obs( y_unit, ob )

   implicit none
   
   integer, intent(in)               :: y_unit
   type (ob_type), intent(inout)     :: ob

   character*5          :: ob_name
   integer              :: num_obs

!  [1] Initialize ob numbers:

   ob % num_synop = 0
   ob % num_metar = 0
   ob % num_ships = 0
   ob % num_satob = 0
   ob % num_gpspw = 0
   ob % num_sound = 0
   ob % num_airep = 0
   ob % num_pilot = 0
   ob % num_ssmir = 0
   ob % num_satem = 0
   ob % num_ssmt1 = 0
   ob % num_ssmt2 = 0
   
!  [2] Loop through input file to count number of obs:

   do
   
      read(y_unit,'(a5,i8)')ob_name, num_obs
      
      if ( trim(ob_name) == 'synop' ) then
         ob % num_synop = ob % num_synop + num_obs
      else if ( trim(ob_name) == 'metar' ) then
         ob % num_metar = ob % num_metar + num_obs
      else if ( trim(ob_name) == 'ships' ) then
         ob % num_ships = ob % num_ships + num_obs
      else if ( trim(ob_name) == 'satob' ) then
         ob % num_satob = ob % num_satob + num_obs
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
      else if ( trim(ob_name) == '*****' ) then
         exit
      end if
   end do

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
   
   if ( ob % num_satob > 0 ) then
      allocate( ob % satob(1:ob % num_satob) )
      write(6,'(a,i8)')' Number of satob obs = ', ob % num_satob
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
   
end subroutine da_count_obs

!--------------------------------------------------------------------------

subroutine da_read_y( y_unit, ob )

   implicit none
   
   integer, intent(in)               :: y_unit
   type (ob_type), intent(inout)     :: ob

   character*5          :: ob_name
   integer              :: n, ndum, k, kdum, num_obs, num_levs   
   integer              :: isynop, imetar, iships, isatob, igpspw, isound, &
                           iairep, ipilot, issmir, isatem, issmt1, issmt2

   rewind (y_unit)

   isynop = 0
   imetar = 0
   iships = 0
   isatob = 0
   igpspw = 0
   isound = 0
   iairep = 0
   ipilot = 0
   issmir = 0
   isatem = 0
   issmt1 = 0
   issmt2 = 0

   do
   
      read(y_unit,'(a5,i8)')ob_name, num_obs
      
      if ( trim(ob_name) == 'synop' ) then
         do n = 1, num_obs
            isynop = isynop + 1
            read(y_unit,'(2i8,10e15.7)')ndum, kdum, ob % synop(isynop) % u % y, &
                                        ob % synop(isynop) % v % y, &
                                        ob % synop(isynop) % t % y, &
                                        ob % synop(isynop) % p % y, &
                                        ob % synop(isynop) % q % y
         end do
      else if ( trim(ob_name) == 'metar' ) then
         do n = 1, num_obs
            imetar = imetar + 1
            read(y_unit,'(2i8,10e15.7)')ndum, kdum, ob % metar(imetar) % u % y, &
                                        ob % metar(imetar) % v % y, &
                                        ob % metar(imetar) % t % y, &
                                        ob % metar(imetar) % p % y, &
                                        ob % metar(imetar) % q % y
         end do
      else if ( trim(ob_name) == 'ships' ) then
         do n = 1, num_obs
            iships = iships + 1
            read(y_unit,'(2i8,10e15.7)')ndum, kdum, ob % ships(iships) % u % y, &
                                        ob % ships(iships) % v % y, &
                                        ob % ships(iships) % t % y, &
                                        ob % ships(iships) % p % y, &
                                        ob % ships(iships) % q % y
         end do

      else if ( trim(ob_name) == 'satob' ) then

         do n = 1, num_obs
            isatob = isatob + 1
            read(y_unit,'(2i8,10e15.7)')ndum, kdum, &
                                        ob % satob(isatob) % u % y, &
                                        ob % satob(isatob) % v % y
         end do

      else if ( trim(ob_name) == 'gpspw' ) then

         do n = 1, num_obs
            igpspw = igpspw + 1
            read(y_unit,'(2i8,10e15.7)')ndum, kdum, &
                                        ob % gpspw(igpspw) % tpw % y
         end do


      else if ( trim(ob_name) == 'sound' ) then
         do n = 1, num_obs
            isound = isound + 1
            read(y_unit,'(i8)')num_levs
            ob % sound(isound) % numlevs = num_levs
            allocate( ob % sound(isound) % u(1:num_levs) )
            allocate( ob % sound(isound) % v(1:num_levs) )
            allocate( ob % sound(isound) % t(1:num_levs) )
            allocate( ob % sound(isound) % p(1:num_levs) )
            allocate( ob % sound(isound) % q(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,10e15.7)')ndum, kdum, &
                                           ob % sound(isound) % u(k) % y, &
                                           ob % sound(isound) % v(k) % y, &
                                           ob % sound(isound) % t(k) % y, &
                                           ob % sound(isound) % p(k) % y, &
                                           ob % sound(isound) % q(k) % y
            end do
         end do
      else if ( trim(ob_name) == 'airep' ) then
         do n = 1, num_obs
            iairep = iairep + 1
            read(y_unit,'(i8)')num_levs
            ob % airep(iairep) % numlevs = num_levs
            allocate( ob % airep(iairep) % u(1:num_levs) )
            allocate( ob % airep(iairep) % v(1:num_levs) )
            allocate( ob % airep(iairep) % t(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,10e15.7)')ndum, kdum, &
                                           ob % airep(iairep) % u(k) % y, &
                                           ob % airep(iairep) % v(k) % y, &
                                           ob % airep(iairep) % t(k) % y
            end do
         end do
      else if ( trim(ob_name) == 'pilot' ) then
         do n = 1, num_obs
            ipilot = ipilot + 1
            read(y_unit,'(i8)')num_levs
            ob % pilot(ipilot) % numlevs = num_levs
            allocate( ob % pilot(ipilot) % u(1:num_levs) )
            allocate( ob % pilot(ipilot) % v(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,10e15.7)')ndum, kdum, &
                                           ob % pilot(ipilot) % u(k) % y, &
                                           ob % pilot(ipilot) % v(k) % y
            end do
         end do
      else if ( trim(ob_name) == 'ssmir' ) then
         do n = 1, num_obs
            issmir = issmir + 1
            read(y_unit,'(2i8,10e15.7)')ndum, kdum, &
                                        ob % ssmir(issmir) % speed % y, &
                                        ob % ssmir(issmir) % tpw % y
         end do
      else if ( trim(ob_name) == 'satem' ) then
         do n = 1, num_obs
            isatem = isatem + 1
            read(y_unit,'(i8)')num_levs
            ob % satem(isatem) % numlevs = num_levs
            allocate( ob % satem(isatem) % thickness(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,10e15.7)')ndum, kdum, &
                                           ob % satem(isatem) % thickness(k) % y
            end do
         end do
      else if ( trim(ob_name) == 'ssmt1' ) then
         do n = 1, num_obs
            issmt1 = issmt1 + 1
            read(y_unit,'(i8)')num_levs
            ob % ssmt1(issmt1) % numlevs = num_levs
            allocate( ob % ssmt1(issmt1) % t(1:num_levs) )
            
            do k = 1, num_levs
               read(y_unit,'(2i8,10e15.7)')ndum, kdum, &
                                           ob % ssmt1(issmt1) % t(k) % y
            end do
         end do
      else if ( trim(ob_name) == 'ssmt2' ) then
         do n = 1, num_obs
            issmt2 = issmt2 + 1
            read(y_unit,'(i8)')num_levs
            ob % ssmt2(issmt2) % numlevs = num_levs
            allocate( ob % ssmt2(issmt2) % rh(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,10e15.7)')ndum, kdum, &
                                           ob % ssmt2(issmt2) % rh(k) % y
            end do
         end do

      else if ( trim(ob_name) == '*****' ) then
         exit
      end if

   end do
   
end subroutine da_read_y

!--------------------------------------------------------------------------

subroutine da_read_yp( yp_unit, ob )

   implicit none
   
   integer, intent(in)               :: yp_unit
   type (ob_type), intent(inout)     :: ob

   character*5          :: ob_name
   integer              :: n, ndum, k, kdum, num_obs, num_levs   
   integer              :: isynop, imetar, iships, isatob, igpspw, isound, &
                           iairep, ipilot, issmir, isatem, issmt1, issmt2

   rewind (yp_unit)

   isynop = 0
   imetar = 0
   iships = 0
   isatob = 0
   igpspw = 0
   isound = 0
   iairep = 0
   ipilot = 0
   issmir = 0
   isatem = 0
   issmt1 = 0
   issmt2 = 0

   do
   
      read(yp_unit,'(a5,i8)')ob_name, num_obs
      
      if ( trim(ob_name) == 'synop' ) then
         do n = 1, num_obs
            isynop = isynop + 1
            read(yp_unit,'(2i8,10e15.7)')ndum, kdum, ob % synop(isynop) % u % yp, &
                                        ob % synop(isynop) % v % yp, &
                                        ob % synop(isynop) % t % yp, &
                                        ob % synop(isynop) % p % yp, &
                                        ob % synop(isynop) % q % yp
         end do
      else if ( trim(ob_name) == 'metar' ) then
         do n = 1, num_obs
            imetar = imetar + 1
            read(yp_unit,'(2i8,10e15.7)')ndum, kdum, ob % metar(imetar) % u % yp, &
                                        ob % metar(imetar) % v % yp, &
                                        ob % metar(imetar) % t % yp, &
                                        ob % metar(imetar) % p % yp, &
                                        ob % metar(imetar) % q % yp
         end do
      else if ( trim(ob_name) == 'ships' ) then
         do n = 1, num_obs
            iships = iships + 1
            read(yp_unit,'(2i8,10e15.7)')ndum, kdum, ob % ships(iships) % u % yp, &
                                        ob % ships(iships) % v % yp, &
                                        ob % ships(iships) % t % yp, &
                                        ob % ships(iships) % p % yp, &
                                        ob % ships(iships) % q % yp
         end do
      else if ( trim(ob_name) == 'satob' ) then
         do n = 1, num_obs
            isatob = isatob + 1
            read(yp_unit,'(2i8,10e15.7)')ndum, kdum, &
                                        ob % satob(isatob) % u % yp, &
                                        ob % satob(isatob) % v % yp
         end do

      else if ( trim(ob_name) == 'gpspw' ) then

         do n = 1, num_obs
            igpspw = igpspw + 1
            read(yp_unit,'(2i8,10e15.7)')ndum, kdum, &
                                        ob % gpspw(igpspw) % tpw % yp
         end do
         
      else if ( trim(ob_name) == 'sound' ) then
         do n = 1, num_obs
            isound = isound + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,10e15.7)')ndum, kdum, &
                                           ob % sound(isound) % u(k) % yp, &
                                           ob % sound(isound) % v(k) % yp, &
                                           ob % sound(isound) % t(k) % yp, &
                                           ob % sound(isound) % p(k) % yp, &
                                           ob % sound(isound) % q(k) % yp
            end do
         end do
      else if ( trim(ob_name) == 'airep' ) then
         do n = 1, num_obs
            iairep = iairep + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,10e15.7)')ndum, kdum, &
                                           ob % airep(iairep) % u(k) % yp, &
                                           ob % airep(iairep) % v(k) % yp, &
                                           ob % airep(iairep) % t(k) % yp
            end do
         end do
      else if ( trim(ob_name) == 'pilot' ) then
         do n = 1, num_obs
            ipilot = ipilot + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,10e15.7)')ndum, kdum, &
                                           ob % pilot(ipilot) % u(k) % yp, &
                                           ob % pilot(ipilot) % v(k) % yp
            end do
         end do
      else if ( trim(ob_name) == 'ssmir' ) then
         do n = 1, num_obs
            issmir = issmir + 1
            read(yp_unit,'(2i8,10e15.7)')ndum, kdum, &
                                        ob % ssmir(issmir) % speed % yp, &
                                        ob % ssmir(issmir) % tpw % yp
         end do
      else if ( trim(ob_name) == 'satem' ) then
         do n = 1, num_obs
            isatem = isatem + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,10e15.7)')ndum, kdum, &
                                           ob % satem(isatem) % thickness(k) % yp
            end do
         end do
      else if ( trim(ob_name) == 'ssmt1' ) then
         do n = 1, num_obs
            issmt1 = issmt1 + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,10e15.7)')ndum, kdum, &
                                           ob % ssmt1(issmt1) % t(k) % yp
            end do
         end do
      else if ( trim(ob_name) == 'ssmt2' ) then
         do n = 1, num_obs
            issmt2 = issmt2 + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,10e15.7)')ndum, kdum, &
                                           ob % ssmt2(issmt2) % rh(k) % yp
            end do
         end do

      else if ( trim(ob_name) == '*****' ) then
         exit
      end if

   end do
   
end subroutine da_read_yp
  
!--------------------------------------------------------------------------

subroutine da_read_obs_rand( rand_unit, ob )

   implicit none
   
   integer, intent(in)               :: rand_unit
   type (ob_type), intent(inout)     :: ob

   character*5          :: ob_name
   integer              :: n, ndum, k, kdum, num_obs, num_levs
   integer              :: isynop, imetar, iships, isatob, igpspw, isound, &
                           iairep, ipilot, issmir, isatem, issmt1, issmt2

   isynop = 0
   imetar = 0
   iships = 0
   isatob = 0
   igpspw = 0
   isound = 0
   iairep = 0
   ipilot = 0
   issmir = 0
   isatem = 0
   issmt1 = 0
   issmt2 = 0   

   rewind( rand_unit )

   do
   
      read(rand_unit,'(a5,i8)')ob_name, num_obs
      
      if ( trim(ob_name) == 'synop' ) then

         do n = 1, num_obs
            isynop = isynop + 1
            read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
            ob % synop(isynop) % u % error, ob % synop(isynop) % u % pert, &
            ob % synop(isynop) % v % error, ob % synop(isynop) % v % pert, &
            ob % synop(isynop) % t % error, ob % synop(isynop) % t % pert, &
            ob % synop(isynop) % p % error, ob % synop(isynop) % p % pert, &
            ob % synop(isynop) % q % error, ob % synop(isynop) % q % pert
         end do
         
      else if ( trim(ob_name) == 'metar' ) then
         
         do n = 1, num_obs
            imetar = imetar + 1
            read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
            ob % metar(imetar) % u % error, ob % metar(imetar) % u % pert, &
            ob % metar(imetar) % v % error, ob % metar(imetar) % v % pert, &
            ob % metar(imetar) % t % error, ob % metar(imetar) % t % pert, &
            ob % metar(imetar) % p % error, ob % metar(imetar) % p % pert, &
            ob % metar(imetar) % q % error, ob % metar(imetar) % q % pert
         end do
         
      else if ( trim(ob_name) == 'ships' ) then
         
         do n = 1, num_obs
            iships = iships + 1
            read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
            ob % ships(iships) % u % error, ob % ships(iships) % u % pert, &
            ob % ships(iships) % v % error, ob % ships(iships) % v % pert, &
            ob % ships(iships) % t % error, ob % ships(iships) % t % pert, &
            ob % ships(iships) % p % error, ob % ships(iships) % p % pert, &
            ob % ships(iships) % q % error, ob % ships(iships) % q % pert
         end do

      else if ( trim(ob_name) == 'satob' ) then
         
         do n = 1, num_obs
            isatob = isatob + 1
            read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
            ob % satob(isatob) % u % error, ob % satob(isatob) % u % pert, &
            ob % satob(isatob) % v % error, ob % satob(isatob) % v % pert
         end do

      else if ( trim(ob_name) == 'gpspw' ) then
         
         do n = 1, num_obs
            igpspw = igpspw + 1
            read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
            ob % gpspw(igpspw) % tpw % error, ob % gpspw(igpspw) % tpw % pert
         end do

      else if ( trim(ob_name) == 'sound' ) then
         
         do n = 1, num_obs
            isound = isound + 1
            read(rand_unit,'(i8)')num_levs
            
            do k = 1, num_levs
               read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
               ob % sound(isound) % u(k) % error, ob % sound(isound) % u(k) % pert, &
               ob % sound(isound) % v(k) % error, ob % sound(isound) % v(k) % pert, &
               ob % sound(isound) % t(k) % error, ob % sound(isound) % t(k) % pert, &
               ob % sound(isound) % p(k) % error, ob % sound(isound) % p(k) % pert, &
               ob % sound(isound) % q(k) % error, ob % sound(isound) % q(k) % pert
            end do
         end do

      else if ( trim(ob_name) == 'airep' ) then
         
         do n = 1, num_obs
            iairep = iairep + 1
            read(rand_unit,'(i8)')num_levs
            
            do k = 1, num_levs
               read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
               ob % airep(iairep) % u(k) % error, ob % airep(iairep) % u(k) % pert, &
               ob % airep(iairep) % v(k) % error, ob % airep(iairep) % v(k) % pert, &
               ob % airep(iairep) % t(k) % error, ob % airep(iairep) % t(k) % pert
            end do
         end do

      else if ( trim(ob_name) == 'pilot' ) then
         
         do n = 1, num_obs
            ipilot = ipilot + 1
            read(rand_unit,'(i8)')num_levs
            
            do k = 1, num_levs
               read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
               ob % pilot(ipilot) % u(k) % error, ob % pilot(ipilot) % u(k) % pert, &
               ob % pilot(ipilot) % v(k) % error, ob % pilot(ipilot) % v(k) % pert
            end do
         end do
         
      else if ( trim(ob_name) == 'ssmir' ) then

         do n = 1, num_obs
            issmir = issmir + 1
            read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
            ob % ssmir(issmir) % speed % error, ob % ssmir(issmir) % speed % pert, &
            ob % ssmir(issmir) % tpw % error, ob % ssmir(issmir) % tpw % pert
         end do
      
      else if ( trim(ob_name) == 'satem' ) then
    
         do n = 1, num_obs
            isatem = isatem + 1
            read(rand_unit,'(i8)')num_levs

            do k = 1, num_levs
               read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
               ob % satem(isatem) % thickness(k) % error, ob % satem(isatem) % thickness(k) % pert
            end do
         end do
         
      else if ( trim(ob_name) == 'ssmt1' ) then
         
         do n = 1, num_obs
            issmt1 = issmt1 + 1
            read(rand_unit,'(i8)')num_levs

            do k = 1, num_levs
               read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
               ob % ssmt1(issmt1) % t % error, ob % ssmt1(issmt1) % t % pert
            end do
         end do

      else if ( trim(ob_name) == 'ssmt2' ) then
         
         do n = 1, num_obs
            issmt2 = issmt2 + 1
            read(rand_unit,'(i8)')num_levs

            do k = 1, num_levs
               read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
               ob % ssmt2(issmt2) % rh % error, ob % ssmt2(issmt2) % rh % pert
            end do
         end do

      else if ( trim(ob_name) == '*****' ) then
         exit
      end if
   end do
   
end subroutine da_read_obs_rand

!--------------------------------------------------------------------------

subroutine da_calc_jo_expected( ob )

   implicit none

   type (ob_type), intent(inout)     :: ob

   integer              :: n, k
   integer              :: count1, count2, count3, count4, count5
   real                 :: trace1, trace2, trace3, trace4, trace5
   real                 :: factor
   
   ob % trace_total = 0
   
   if ( ob % num_synop > 0 ) then
      trace1 = 0.0; trace2 = 0.0; trace3 = 0.0; trace4 = 0.0; trace5 = 0.0
      count1 = 0; count2 = 0; count3 = 0; count4 = 0; count5 = 0
 
      do n = 1, ob % num_synop
         call da_calc_trace_single( ob % synop(n) % u, count1, trace1 )
         call da_calc_trace_single( ob % synop(n) % v, count2, trace2 )
         call da_calc_trace_single( ob % synop(n) % t, count3, trace3 )
         call da_calc_trace_single( ob % synop(n) % p, count4, trace4 )
         call da_calc_trace_single( ob % synop(n) % q, count5, trace5 )
      end do
      
      ob % jo_synop_u = 0.5 * ( count1 - trace1 )
      ob % jo_synop_v = 0.5 * ( count2 - trace2 )
      ob % jo_synop_t = 0.5 * ( count3 - trace3 )
      ob % jo_synop_p = 0.5 * ( count4 - trace4 )
      ob % jo_synop_q = 0.5 * ( count5 - trace5 )
      ob % trace_synop   = trace1 + trace2 + trace3 + trace4 + trace5

      if ( ob % trace_synop < 0.0 ) &
      write(6,'(a,f15.5)')' Warning: ob % trace_synop < 0 = ', ob % trace_synop

      ob % trace_total   = ob % trace_total + ob % trace_synop
      ob % num_synop_tot = count1 + count2 + count3 + count4 + count5
   end if

   if ( ob % num_metar > 0 ) then
      trace1 = 0.0; trace2 = 0.0; trace3 = 0.0; trace4 = 0.0; trace5 = 0.0
      count1 = 0; count2 = 0; count3 = 0; count4 = 0; count5 = 0
 
      do n = 1, ob % num_metar
         call da_calc_trace_single( ob % metar(n) % u, count1, trace1 )
         call da_calc_trace_single( ob % metar(n) % v, count2, trace2 )
         call da_calc_trace_single( ob % metar(n) % t, count3, trace3 )
         call da_calc_trace_single( ob % metar(n) % p, count4, trace4 )
         call da_calc_trace_single( ob % metar(n) % q, count5, trace5 )
      end do
      
      ob % jo_metar_u = 0.5 * ( count1 - trace1 )
      ob % jo_metar_v = 0.5 * ( count2 - trace2 )
      ob % jo_metar_t = 0.5 * ( count3 - trace3 )
      ob % jo_metar_p = 0.5 * ( count4 - trace4 )
      ob % jo_metar_q = 0.5 * ( count5 - trace5 )
      ob % trace_metar   = trace1 + trace2 + trace3 + trace4 + trace5

      if ( ob % trace_metar < 0.0 ) &
      write(6,'(a,f15.5)')' Warning: ob % trace_metar < 0 = ', ob % trace_metar

      ob % trace_total = ob % trace_total + ob % trace_metar
      ob % num_metar_tot = count1 + count2 + count3 + count4 + count5

   end if

   if ( ob % num_ships > 0 ) then
      trace1 = 0.0; trace2 = 0.0; trace3 = 0.0; trace4 = 0.0; trace5 = 0.0
      count1 = 0; count2 = 0; count3 = 0; count4 = 0; count5 = 0
 
      do n = 1, ob % num_ships
         call da_calc_trace_single( ob % ships(n) % u, count1, trace1 )
         call da_calc_trace_single( ob % ships(n) % v, count2, trace2 )
         call da_calc_trace_single( ob % ships(n) % t, count3, trace3 )
         call da_calc_trace_single( ob % ships(n) % p, count4, trace4 )
         call da_calc_trace_single( ob % ships(n) % q, count5, trace5 )
      end do
      
      ob % jo_ships_u = 0.5 * ( count1 - trace1 )
      ob % jo_ships_v = 0.5 * ( count2 - trace2 )
      ob % jo_ships_t = 0.5 * ( count3 - trace3 )
      ob % jo_ships_p = 0.5 * ( count4 - trace4 )
      ob % jo_ships_q = 0.5 * ( count5 - trace5 )
      ob % trace_ships   = trace1 + trace2 + trace3 + trace4 + trace5

      if ( ob % trace_ships < 0.0 ) &
      write(6,'(a,f15.5)')' Warning: ob % trace_ships < 0 = ', ob % trace_ships

      ob % trace_total = ob % trace_total + ob % trace_ships
      ob % num_ships_tot = count1 + count2 + count3 + count4 + count5

   end if

   if ( ob % num_satob > 0 ) then
      trace1 = 0.0; trace2 = 0.0
      count1 = 0; count2 = 0

      do n = 1, ob % num_satob
         call da_calc_trace_single( ob % satob(n) % u, count1, trace1 )
         call da_calc_trace_single( ob % satob(n) % v, count2, trace2 )
      end do
      
      ob % jo_satob_u = 0.5 * ( count1 - trace1 )
      ob % jo_satob_v = 0.5 * ( count2 - trace2 )
      ob % trace_satob   = trace1 + trace2

      if ( ob % trace_satob < 0.0 ) &
      write(6,'(a,f15.5)')' Warning: ob % trace_satob < 0 = ', ob % trace_satob

      ob % trace_total = ob % trace_total + ob % trace_satob
      ob % num_satob_tot = count1 + count2

   end if

   if ( ob % num_gpspw > 0 ) then
      trace1 = 0.0
      count1 = 0
 
      do n = 1, ob % num_gpspw
         call da_calc_trace_single( ob % gpspw(n) % tpw, count1, trace1 )
      end do

      ob % jo_gpspw_tpw = 0.5 * ( count1 - trace1 )
      ob % trace_gpspw = trace1

      if ( ob % trace_gpspw < 0.0 ) &
      write(6,'(a,f15.5)')' Warning: ob % trace_gpspw < 0 = ', ob % trace_gpspw

      ob % trace_total = ob % trace_total + ob % trace_gpspw
      ob % num_gpspw_tot = count1

   end if

   if ( ob % num_sound > 0 ) then
      trace1 = 0.0; trace2 = 0.0; trace3 = 0.0; trace4 = 0.0; trace5 = 0.0
      count1 = 0; count2 = 0; count3 = 0; count4 = 0; count5 = 0

      do n = 1, ob % num_sound
         do k = 1, ob % sound(n) % numlevs
            call da_calc_trace_single( ob % sound(n) % u(k), count1, trace1 )
            call da_calc_trace_single( ob % sound(n) % v(k), count2, trace2 )
            call da_calc_trace_single( ob % sound(n) % t(k), count3, trace3 )
            call da_calc_trace_single( ob % sound(n) % p(k), count4, trace4 )
            call da_calc_trace_single( ob % sound(n) % q(k), count5, trace5 )
         end do
      end do
      
      ob % jo_sound_u = 0.5 * ( count1 - trace1 )
      ob % jo_sound_v = 0.5 * ( count2 - trace2 )
      ob % jo_sound_t = 0.5 * ( count3 - trace3 )
      ob % jo_sound_p = 0.5 * ( count4 - trace4 )
      ob % jo_sound_q = 0.5 * ( count5 - trace5 )
      ob % trace_sound = trace1 + trace2 + trace3 + trace4 + trace5

      if ( ob % trace_sound < 0.0 ) &
      write(6,'(a,f15.5)')' Warning: ob % trace_sound < 0 = ', ob % trace_sound

      ob % trace_total = ob % trace_total + ob % trace_sound
      ob % num_sound_tot = count1 + count2 + count3 + count4 + count5

   end if
   
   if ( ob % num_airep > 0 ) then
      trace1 = 0.0; trace2 = 0.0; trace3 = 0.0; trace4 = 0.0
      count1 = 0; count2 = 0; count3 = 0; count4 = 0

      do n = 1, ob % num_airep
         do k = 1, ob % airep(n) % numlevs
            call da_calc_trace_single( ob % airep(n) % u(k), count1, trace1 )
            call da_calc_trace_single( ob % airep(n) % v(k), count2, trace2 )
            call da_calc_trace_single( ob % airep(n) % t(k), count3, trace3 )
         end do
      end do
      
      ob % jo_airep_u = 0.5 * ( count1 - trace1 )
      ob % jo_airep_v = 0.5 * ( count2 - trace2 )
      ob % jo_airep_t = 0.5 * ( count3 - trace3 )
      ob % trace_airep   = trace1 + trace2 + trace3

      if ( ob % trace_airep < 0.0 ) &
      write(6,'(a,f15.5)')' Warning: ob % trace_airep < 0 = ', ob % trace_airep

      ob % trace_total = ob % trace_total + ob % trace_airep
      ob % num_airep_tot = count1 + count2 + count3

   end if
   
   if ( ob % num_pilot > 0 ) then
      trace1 = 0.0; trace2 = 0.0
      count1 = 0; count2 = 0

      do n = 1, ob % num_pilot
         do k = 1, ob % pilot(n) % numlevs
            call da_calc_trace_single( ob % pilot(n) % u(k), count1, trace1 )
            call da_calc_trace_single( ob % pilot(n) % v(k), count2, trace2 )
         end do
      end do

      ob % jo_pilot_u = 0.5 * ( count1 - trace1 )
      ob % jo_pilot_v = 0.5 * ( count2 - trace2 )
      ob % trace_pilot = trace1 + trace2

      if ( ob % trace_pilot < 0.0 ) &
      write(6,'(a,f15.5)')' Warning: ob % trace_pilot < 0 = ', ob % trace_pilot

      ob % trace_total = ob % trace_total + ob % trace_pilot
      ob % num_pilot_tot = count1 + count2

   end if
   
   if ( ob % num_ssmir > 0 ) then
      trace1 = 0.0; trace2 = 0.0
      count1= 0; count2 = 0

      do n = 1, ob % num_ssmir
         call da_calc_trace_single( ob % ssmir(n) % speed, count1, trace1 )
         call da_calc_trace_single( ob % ssmir(n) % tpw,   count2, trace2 )
      end do
   
      ob % jo_ssmir_speed = 0.5 * ( count1 - trace1 )
      ob % jo_ssmir_tpw   = 0.5 * ( count2 - trace2 )
      ob % trace_ssmir    = trace1 + trace2

      if ( ob % trace_ssmir < 0.0 ) &
      write(6,'(a,f15.5)')' Warning: ob % trace_ssmir < 0 = ', ob % trace_ssmir

      ob % trace_total    = ob % trace_total + ob % trace_ssmir
      ob % num_ssmir_tot  = count1 + count2

   end if
   
   if ( ob % num_satem > 0 ) then
      trace1 = 0.0
      count1 = 0

      do n = 1, ob % num_satem
         do k = 1, ob % satem(n) % numlevs
           call da_calc_trace_single( ob % satem(n) % thickness(k), count1, trace1 )
         end do
      end do
   
      ob % jo_satem_thickness = 0.5 * ( count1 - trace1 )
      ob % trace_satem = trace1

      if ( ob % trace_satem< 0.0 ) &
      write(6,'(a,f15.5)')' Warning: ob % trace_satem < 0 = ', ob % trace_satem

      ob % trace_total = ob % trace_total + ob % trace_satem
      ob % num_satem_tot = count1

   end if
   
   if ( ob % num_ssmt1 > 0 ) then
      trace1 = 0.0
      count1 = 0

      do n = 1, ob % num_ssmt1
         do k = 1, ob % ssmt1(n) % numlevs
            call da_calc_trace_single( ob % ssmt1(n) % t(k), count1, trace1 )
         end do
      end do
   
      ob % jo_ssmt1_t  = 0.5 * ( count1 - trace1 )
      ob % trace_ssmt1 = trace1

      if ( ob % trace_ssmt1< 0.0 ) &
      write(6,'(a,f15.5)')' Warning: ob % trace_ssmt1 < 0 = ', ob % trace_ssmt1

      ob % trace_total = ob % trace_total + ob % trace_ssmt1
      ob % num_ssmt1_tot = count1

   end if
   
   if ( ob % num_ssmt2 > 0 ) then
      trace1 = 0.0
      count1 = 0

      do n = 1, ob % num_ssmt2
         do k = 1, ob % ssmt2(n) % numlevs
            call da_calc_trace_single( ob % ssmt2(n) % rh(k), count1, trace1 )
         end do
      end do
   
      ob % jo_ssmt2_rh = 0.5 * ( count1 - trace1 )
      ob % trace_ssmt2    = trace1

      if ( ob % trace_ssmt2< 0.0 ) &
      write(6,'(a,f15.5)')' Warning: ob % trace_ssmt2 < 0 = ', ob % trace_ssmt2

      ob % trace_total = ob % trace_total + ob % trace_ssmt2
      ob % num_ssmt2_tot = count1

   end if
   
   ob % total_obs = ob % num_synop_tot + ob % num_metar_tot + ob % num_ships_tot + &
                    ob % num_satob_tot + ob % num_gpspw_tot + ob % num_sound_tot + &
                    ob % num_airep_tot + ob % num_pilot_tot + ob % num_ssmir_tot + &
                    ob % num_satem_tot + ob % num_ssmt1_tot + ob % num_ssmt2_tot

end subroutine da_calc_jo_expected

!--------------------------------------------------------------------------

subroutine da_calc_trace_single( field, count, trace )

   implicit none
   
   type (field_type), intent(in)   :: field
   integer, intent(inout)          :: count
   real, intent(inout)             :: trace
      
   if ( field % yp /= missing_r .and. field % y /= missing_r ) then
      count = count + 1
      trace = trace + ( field % yp  - field % y ) * field % pert / field % error
   end if
   
end subroutine da_calc_trace_single

!--------------------------------------------------------------------------

subroutine da_read_jo_actual( ob )

   implicit none

   type (ob_type), intent(inout) :: ob
   
   character (len=46)            :: str
   character (len=12)            :: string
   character (len=15)            :: str1, str2, str3, str4, str5
   character (len=15)            :: str6, str7, str8, str9, str10
   character (len=5)             :: ob_name
   
   real                          :: dum1, dum2, dum3, dum4, dum5

   integer                       :: isynop, imetar, iships, isatob, igpspw, isound, &
                                    iairep, ipilot, issmir, isatem, issmt1, issmt2

   isynop = 0
   imetar = 0
   iships = 0
   isatob = 0
   igpspw = 0
   isound = 0
   iairep = 0
   ipilot = 0
   issmir = 0
   isatem = 0
   issmt1 = 0
   issmt2 = 0

   ob % joa_synop_u = 0.0
   ob % joa_synop_v = 0.0
   ob % joa_synop_t = 0.0
   ob % joa_synop_p = 0.0
   ob % joa_synop_q = 0.0
   ob % joa_metar_u = 0.0
   ob % joa_metar_v = 0.0
   ob % joa_metar_t = 0.0
   ob % joa_metar_p = 0.0
   ob % joa_metar_q = 0.0
   ob % joa_ships_u = 0.0
   ob % joa_ships_v = 0.0
   ob % joa_ships_t = 0.0
   ob % joa_ships_p = 0.0
   ob % joa_ships_q = 0.0
   ob % joa_satob_u = 0.0
   ob % joa_satob_v = 0.0
   ob % joa_gpspw_tpw = 0.0
   ob % joa_sound_u = 0.0
   ob % joa_sound_v = 0.0
   ob % joa_sound_t = 0.0
   ob % joa_sound_p = 0.0
   ob % joa_sound_q = 0.0
   ob % joa_airep_u = 0.0
   ob % joa_airep_v = 0.0
   ob % joa_airep_t = 0.0
   ob % joa_pilot_u = 0.0
   ob % joa_pilot_v = 0.0
   ob % joa_ssmir_speed = 0.0
   ob % joa_ssmir_tpw = 0.0
   ob % joa_satem_thickness = 0.0
   ob % joa_ssmt1_t = 0.0
   ob % joa_ssmt2_rh = 0.0

   rewind(jo_unit)
   
   do

      read(jo_unit,'(a46,10a15)')str, str1, str2, str3, str4, str5, &
                                 str6, str7, str8, str9, str10
      ob_name = str(5:9)
      string = str(16:27)
     
      if ( ob_name == 'synop' .and. string == 'Jo (actual) ' ) then

            call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                     str6, str7, str8, str9, str10, &
                                     ob % joa_synop_u, ob % joa_synop_v, &
                                     ob % joa_synop_t, ob % joa_synop_p, &
                                     ob % joa_synop_q )

      else if ( ob_name == 'metar' .and. string == 'Jo (actual) ' ) then

            call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                     str6, str7, str8, str9, str10, &
                                     ob % joa_metar_u, ob % joa_metar_v, &
                                     ob % joa_metar_t, ob % joa_metar_p, &
                                     ob % joa_metar_q )

      else if ( ob_name == 'ships' .and. string == 'Jo (actual) ' ) then

         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_ships_u, ob % joa_ships_v, &
                                  ob % joa_ships_t, ob % joa_ships_p, &
                                  ob % joa_ships_q )

      else if ( ob_name == 'satob' .and. string == 'Jo (actual) ' ) then

         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_satob_u, ob % joa_satob_v, &
                                  dum3, dum4, dum5 )

      else if ( ob_name == 'gpspw' .and. string == 'Jo (actual) ' ) then

         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_gpspw_tpw, dum2, dum3, dum4, dum5 )

      else if ( ob_name == 'sound' .and. string == 'Jo (actual) ' ) then

         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_sound_u, ob % joa_sound_v, ob % joa_sound_t, &
                                  ob % joa_sound_p, ob % joa_sound_q )

      else if ( ob_name == 'airep' .and. string == 'Jo (actual) ' ) then

         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_airep_u, ob % joa_airep_v, &
                                  ob % joa_airep_t, dum1, dum2 )

      else if ( ob_name == 'pilot' .and. string == 'Jo (actual) ' ) then

         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_pilot_u, ob % joa_pilot_v, &
                                  dum1, dum2, dum3 )

      else if ( ob_name == 'ssmir' .and. string == 'Jo (actual) ' ) then

         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_ssmir_speed, ob % joa_ssmir_tpw, &
                                  dum1, dum2, dum3 )

      else if ( ob_name == 'satem' .and. string == 'Jo (actual) ' ) then

         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_satem_thickness, dum1, dum2, &
                                  dum3, dum4 )

      else if ( ob_name == 'ssmt1' .and. string == 'Jo (actual) ' ) then

         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_ssmt1_t, dum1, dum2, dum3, dum4 )

      else if ( ob_name == 'ssmt2' .and. string == 'Jo (actual) ' ) then

         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_ssmt2_rh, dum1, dum2, dum3, dum4 )

      else if ( str(1:5) == '*****' ) then
         exit
      end if

   end do
            
end subroutine da_read_jo_actual

!--------------------------------------------------------------------------

subroutine da_read_jo_actual1( str1, str2, str3, str4, str5, &
                               str6, str7, str8, str9, str10, &
                               j1, j2, j3, j4, j5 )
                            
   implicit none
         
   character (len=15), intent(in) :: str1, str2, str3, str4, str5
   character (len=15), intent(in) :: str6, str7, str8, str9, str10
   real, intent(inout)            :: j1, j2, j3, j4, j5
   
   real                           :: j1n, j2n, j3n, j4n, j5n
   real                           :: f1n, f2n, f3n, f4n, f5n

   read(str1,*)j1n
   read(str2,*)f1n
   read(str3,*)j2n
   read(str4,*)f2n
   read(str5,*)j3n
   read(str6,*)f3n
   read(str7,*)j4n
   read(str8,*)f4n
   read(str9,*)j5n
   read(str10,*)f5n

!  Jo(actual) is scaled by the square of the input error factor used.

   j1 = j1 + f1n**2 * j1n
   j2 = j2 + f2n**2 * j2n
   j3 = j3 + f3n**2 * j3n
   j4 = j4 + f4n**2 * j4n
   j5 = j5 + f5n**2 * j5n

end subroutine da_read_jo_actual1

!--------------------------------------------------------------------------

subroutine da_calc_new_factors( ob )

   implicit none

   type (ob_type), intent(inout)     :: ob

   write(6,*)

   if ( ob % num_synop > 0 ) then
      call da_calc_new_factors1( 'synop', ob % num_synop, ob % num_synop_tot, &
                            ob % jo_synop_u, ob % jo_synop_v, ob % jo_synop_t, ob % jo_synop_p, ob % jo_synop_q, &
                            ob % joa_synop_u, ob % joa_synop_v, ob % joa_synop_t, ob % joa_synop_p, ob % joa_synop_q )
   end if

   if ( ob % num_metar > 0 ) then
      call da_calc_new_factors1( 'metar', ob % num_metar, ob % num_metar_tot, &
                            ob % jo_metar_u, ob % jo_metar_v, ob % jo_metar_t, ob % jo_metar_p, ob % jo_metar_q, &
                            ob % joa_metar_u, ob % joa_metar_v, ob % joa_metar_t, ob % joa_metar_p, ob % joa_metar_q )

   end if

   if ( ob % num_ships > 0 ) then
      call da_calc_new_factors1( 'ships', ob % num_ships, ob % num_ships_tot, &
                            ob % jo_ships_u, ob % jo_ships_v, ob % jo_ships_t, ob % jo_ships_p, ob % jo_ships_q, &
                            ob % joa_ships_u, ob % joa_ships_v, ob % joa_ships_t, ob % joa_ships_p, ob % joa_ships_q )

   end if

   if ( ob % num_satob > 0 ) then
      call da_calc_new_factors1( 'satob', ob % num_satob, ob % num_satob_tot, &
                            ob % jo_satob_u, ob % jo_satob_v, 0.0, 0.0, 0.0, &
                            ob % joa_satob_u, ob % joa_satob_v, 0.0, 0.0, 0.0 )

   end if

   if ( ob % num_gpspw > 0 ) then
      call da_calc_new_factors1( 'gpspw', ob % num_gpspw, ob % num_gpspw_tot, &
                            ob % jo_gpspw_tpw, 0.0, 0.0, 0.0, 0.0, &
                            ob % joa_gpspw_tpw, 0.0, 0.0, 0.0, 0.0 )

   end if

   if ( ob % num_sound > 0 ) then
      call da_calc_new_factors1( 'sound', ob % num_sound, ob % num_sound_tot, &
                            ob % jo_sound_u, ob % jo_sound_v, ob % jo_sound_t, ob % jo_sound_p, ob % jo_sound_q, &
                            ob % joa_sound_u, ob % joa_sound_v, ob % joa_sound_t, ob % joa_sound_p, ob % joa_sound_q )

   end if
   
   if ( ob % num_airep > 0 ) then
      call da_calc_new_factors1( 'airep', ob % num_airep, ob % num_airep_tot, &
                            ob % jo_airep_u, ob % jo_airep_v, ob % jo_airep_t, 0.0, 0.0, &
                            ob % joa_airep_u, ob % joa_airep_v, ob % joa_airep_t, 0.0, 0.0 )

   end if
   
   if ( ob % num_pilot > 0 ) then
      call da_calc_new_factors1( 'pilot', ob % num_pilot, ob % num_pilot_tot, &
                            ob % jo_pilot_u, ob % jo_pilot_v, 0.0, 0.0, 0.0, &
                            ob % joa_pilot_u, ob % joa_pilot_v, 0.0, 0.0, 0.0 )

   end if
   
   if ( ob % num_ssmir > 0 ) then
      call da_calc_new_factors1( 'ssmir', ob % num_ssmir, ob % num_ssmir_tot, &
                            ob % jo_ssmir_speed, ob % jo_ssmir_tpw, 0.0, 0.0, 0.0, &
                            ob % joa_ssmir_speed, ob % joa_ssmir_tpw, 0.0, 0.0, 0.0 )

   end if
   
   if ( ob % num_satem > 0 ) then
      call da_calc_new_factors1( 'satem', ob % num_satem, ob % num_satem_tot, &
                            ob % jo_satem_thickness, 0.0, 0.0, 0.0, 0.0, &
                            ob % joa_satem_thickness, 0.0, 0.0, 0.0, 0.0 )

   end if
   
   if ( ob % num_ssmt1 > 0 ) then
      call da_calc_new_factors1( 'ssmt1', ob % num_ssmt1, ob % num_ssmt1_tot, &
                            ob % jo_ssmt1_t, 0.0, 0.0, 0.0, 0.0, &
                            ob % joa_ssmt1_t, 0.0, 0.0, 0.0, 0.0 )
   end if
   
   if ( ob % num_ssmt2 > 0 ) then
      call da_calc_new_factors1( 'ssmt2', ob % num_ssmt2, ob % num_ssmt2_tot, &
                            ob % jo_ssmt2_rh, 0.0, 0.0, 0.0, 0.0, &
                            ob % joa_ssmt2_rh, 0.0, 0.0, 0.0, 0.0 )
   end if

end subroutine da_calc_new_factors

!--------------------------------------------------------------------------

subroutine da_calc_new_factors1( ob_name, ob_num, ob_num_tot, &
                                 j1e, j2e, j3e, j4e, j5e, &
                                 j1a, j2a, j3a, j4a, j5a )
                             
   implicit none
         
   character (len=5), intent(in)  :: ob_name
   integer, intent(in)            :: ob_num, ob_num_tot
   real, intent(in)               :: j1e, j2e, j3e, j4e, j5e
   real, intent(in)               :: j1a, j2a, j3a, j4a, j5a

   real                           :: j1, j2, j3, j4, j5   
   real                           :: f1, f2, f3, f4, f5
         
   f1 = 1.0; f2 = 1.0; f3 = 1.0; f4 = 1.0; f5 = 1.0

   if ( j1e > 0.0 ) f1 = sqrt( j1a / j1e )
   if ( j2e > 0.0 ) f2 = sqrt( j2a / j2e )
   if ( j3e > 0.0 ) f3 = sqrt( j3a / j3e )
   if ( j4e > 0.0 ) f4 = sqrt( j4a / j4e )
   if ( j5e > 0.0 ) f5 = sqrt( j5a / j5e )
   
   write(6,'(x,a5,a21,2i8,6f15.5)')ob_name, ' obs, Jo (expected)= ', &
                                   ob_num, ob_num_tot, &
                                   j1e, j2e, j3e, j4e, j5e

   write(6,'(x,a5,a21,2i8,6f15.5)')ob_name, ' obs, Jo (actual)  = ', &
                                   ob_num, ob_num_tot, &
                                   j1a, j2a, j3a, j4a, j5a

   write(6,'(x,a5,a21,2i8,6f15.5)')ob_name, ' obs, Error Factor = ', &
                                   ob_num, ob_num_tot, f1, f2, f3, f4, f5
   write(6,*)
   
end subroutine da_calc_new_factors1

!--------------------------------------------------------------------------

subroutine da_get_j( ob )

   implicit none

   type (ob_type), intent(in)    :: ob
   
   character (len=30)            :: str
   character (len=15)            :: str1
   character (len=26)            :: string
   integer                       :: icount
   real                          :: j, jo, jb, jn, jon, jbn, j_e, jo_e, jb_e
   real                          :: jb_factor_old, jb_factor_oldn, jb_factor_new

   rewind(in_unit)

   j = 0.0; jo = 0.0; jb = 0.0; j_e = 0.0; jo_e = 0.0; jb_e = 0.0
   icount = 0 
     
   do

      read(in_unit,'(a30,a15)')str, str1
      string = str(5:30)
      
      if ( string == 'Final 3DVAR value of J  = ' ) then
         read(str1,*)jn
         j = j + jn
      else if ( string == 'Final 3DVAR value of Jo = ' ) then
         read(str1,*)jon
         jo = jo + jon
      else if ( string == 'Final 3DVAR value of Jb = ' ) then
         read(str1,*)jbn
         jb = jb + jbn
      else if ( string == '3DVAR Jb factor used (1)= ' ) then
         read(str1,*)jb_factor_oldn
         jb_factor_old = jb_factor_old + jb_factor_oldn
         icount = icount + 1
      else if ( str(1:5) == '*****' ) then
         exit
      end if
      
   end do

   jb_factor_old = jb_factor_old / real(icount)

   write(6,'(/a,i8)')    ' Total number of obs.    = ', ob % total_obs
   write(6,'(/a,i8)')    ' Total number of cases   = ', icount
   j_e  = 0.5 * ob % total_obs
   jo_e = 0.5 * ( ob % total_obs -  ob % trace_total )
   jb_e = 0.5 * ob % trace_total

   write(6,'(a,f15.5)')' Total J (actual)        = ', j
   write(6,'(a,f15.5)')' Total J (expected)      = ', j_e
   write(6,*)

   write(6,'(a,f15.5)')' Total Jo(actual)        = ', jo
   write(6,'(a,f15.5)')' Total Jo(expected)      = ', jo_e
   write(6,'(a,f15.5)')' Total Jo factor         = ', sqrt(jo/jo_e)

   write(6,*)
   write(6,'(a,f15.5)')' Total Jb(actual)        = ', jb
   write(6,'(a,f15.5)')' Total Jb(expected)      = ', jb_e
   write(6,'(a,f15.5)')' Total Jb factor (old)   = ', jb_factor_old

   if ( jb_e < 0.0 ) then
      write(6,'(a)')' Warning: Tr(HK) < 0.0! Too small a sample?'
      stop
   end if
   jb_factor_new = sqrt(jb/jb_e) * jb_factor_old
   write(6,'(a,f15.5)')' Total Jb factor (new)   = ', jb_factor_new
   write(6,*)
   
end subroutine da_get_j

!--------------------------------------------------------------------------

end program tune
