!WRF:DRIVER_LAYER:DOMAIN_OBJECT
!
!  Following are the routines contained within this MODULE:

!  alloc_and_configure_domain        1. Allocate the space for a single domain (constants
!                                       and null terminate pointers).
!                                    2. Connect the domains as a linked list.
!                                    3. Store all of the domain constants.
!                                    4. CALL alloc_space_field.

!  alloc_space_field                 1. Allocate space for the gridded data required for
!                                       each domain.

!  dealloc_space_domain              1. Reconnect linked list nodes since the current
!                                       node is removed.
!                                    2. CALL dealloc_space_field.
!                                    3. Deallocate single domain.

!  dealloc_space_field               1. Deallocate each of the fields for a particular
!                                       domain.

!  first_loc_integer                 1. Find the first incidence of a particular
!                                       domain identifier from an array of domain
!                                       identifiers.

MODULE module_domain1

   USE module_driver_constants
   USE module_machine
   USE module_state_description
   USE module_configure
   USE module_wrf_error
   USE module_utility

   CHARACTER (LEN=80) program_name

   !  An entire domain.  This contains multiple meteorological fields by having
   !  arrays (such as "data_3d") of pointers for each field.  Also inside each
   !  domain is a link to a couple of other domains, one is just the "next"
   !  domain that is to be stored, the other is the next domain which happens to
   !  also be on the "same_level".

   TYPE domain_ptr
      TYPE(domain), POINTER :: ptr
   END TYPE domain_ptr

   INTEGER, PARAMETER :: HISTORY_ALARM=1, AUXHIST1_ALARM=2, AUXHIST2_ALARM=3,     &
                         AUXHIST3_ALARM=4, AUXHIST4_ALARM=5, AUXHIST5_ALARM=6,    &
                         AUXINPUT1_ALARM=7, AUXINPUT2_ALARM=8, AUXINPUT3_ALARM=9, &
                         AUXINPUT4_ALARM=10, AUXINPUT5_ALARM=11,                  &
                         RESTART_ALARM=12, BOUNDARY_ALARM=13, INPUTOUT_ALARM=14,  &  ! for outputing input (e.g. for 3dvar)
                         ALARM_SUBTIME=15,                                        &
#ifdef MOVE_NESTS
                         COMPUTE_VORTEX_CENTER_ALARM=16,                          &
#endif
                         MAX_WRF_ALARMS=20  ! WARNING:  MAX_WRF_ALARMS must be 
                                            ! large enough to include all of 
                                            ! the alarms declared above.  

#include <state_subtypes.inc>

   TYPE domain

! SEE THE INCLUDE FILE FOR DEFINITIONS OF STATE FIELDS WITHIN THE DOMAIN DATA STRUCTURE
#include <state_struct.inc>

      INTEGER                                             :: comms( max_comms ), shift_x, shift_y

      INTEGER                                             :: id
      INTEGER                                             :: domdesc
      INTEGER                                             :: communicator
      INTEGER                                             :: iocommunicator
      INTEGER,POINTER                                     :: mapping(:,:)
      INTEGER,POINTER                                     :: i_start(:),i_end(:)
      INTEGER,POINTER                                     :: j_start(:),j_end(:)
      INTEGER                                             :: max_tiles
      INTEGER                                             :: num_tiles        ! taken out of namelist 20000908
      INTEGER                                             :: num_tiles_x      ! taken out of namelist 20000908
      INTEGER                                             :: num_tiles_y      ! taken out of namelist 20000908
      INTEGER                                             :: num_tiles_spec   ! place to store number of tiles computed from 
                                                                              ! externally specified params

      TYPE(domain_ptr) , DIMENSION( : ) , POINTER         :: parents                            
      TYPE(domain_ptr) , DIMENSION( : ) , POINTER         :: nests                            
      TYPE(domain) , POINTER                              :: sibling ! overlapped domains at same lev
      TYPE(domain) , POINTER                              :: intermediate_grid
      INTEGER                                             :: num_parents, num_nests, num_siblings
      INTEGER      , DIMENSION( max_parents )             :: child_of_parent
      INTEGER      , DIMENSION( max_nests )               :: active

      INTEGER      , DIMENSION(0:5)                       :: nframes          ! frames per outfile for history 
                                                                              ! streams (0 is main history)                  

      TYPE(domain) , POINTER                              :: next
      TYPE(domain) , POINTER                              :: same_level

      LOGICAL      , DIMENSION ( 4 )                      :: bdy_mask         ! which boundaries are on processor

      LOGICAL                                             :: first_force

#ifdef MOVE_NESTS
      REAL         :: xi , xj
      REAL         :: vc_i, vc_j    ! vortex center i and j
#endif

      ! domain dimensions

      INTEGER    :: sd31,   ed31,   sd32,   ed32,   sd33,   ed33,         &
                    sd21,   ed21,   sd22,   ed22,                         &
                    sd11,   ed11

      INTEGER    :: sp31,   ep31,   sp32,   ep32,   sp33,   ep33,         &
                    sp21,   ep21,   sp22,   ep22,                         &
                    sp11,   ep11,                                         &
                    sm31,   em31,   sm32,   em32,   sm33,   em33,         &
                    sm21,   em21,   sm22,   em22,                         &
                    sm11,   em11,                                         &
                    sp31x,  ep31x,  sp32x,  ep32x,  sp33x,  ep33x,        &
                    sp21x,  ep21x,  sp22x,  ep22x,                        &
                    sm31x,  em31x,  sm32x,  em32x,  sm33x,  em33x,        &
                    sm21x,  em21x,  sm22x,  em22x,                        &
                    sp31y,  ep31y,  sp32y,  ep32y,  sp33y,  ep33y,        &
                    sp21y,  ep21y,  sp22y,  ep22y,                        &
                    sm31y,  em31y,  sm32y,  em32y,  sm33y,  em33y,        &
                    sm21y,  em21y,  sm22y,  em22y
      Type(WRF_UTIL_Clock), POINTER                           :: domain_clock
      Type(WRF_UTIL_Time)                                     :: start_time, stop_time, current_time
      Type(WRF_UTIL_Time)                                     :: start_subtime, stop_subtime
      Type(WRF_UTIL_Time)                                     :: this_bdy_time, next_bdy_time
      Type(WRF_UTIL_Time)                                     :: this_emi_time, next_emi_time
      Type(WRF_UTIL_TimeInterval) :: step_time
      Type(WRF_UTIL_Alarm), pointer :: alarms(:)
! This awful hackery accounts for the fact that ESMF2.1.0+ alarms cannot tell 
! us if they have ever been created or not.  So, we have to keep track of this 
! ourselves to avoid destroying an alarm that has never been created!  Rip this 
! out once ESMF has useful introspection here...  
      LOGICAL, pointer :: alarms_created(:)

   END TYPE domain

   !  Now that a "domain" TYPE exists, we can use it to store a few pointers
   !  to this type.  These are primarily for use in traversing the linked list.
   !  The "head_grid" is always the pointer to the first domain that is
   !  allocated.  This is available and is not to be changed.  The others are
   !  just temporary pointers.

   TYPE(domain) , POINTER :: head_grid , new_grid , next_grid , old_grid

   !  To facilitate an easy integration of each of the domains that are on the
   !  same level, we have an array for the head pointer for each level.  This
   !  removed the need to search through the linked list at each time step to
   !  find which domains are to be active.

   TYPE domain_levels
      TYPE(domain) , POINTER                              :: first_domain
   END TYPE domain_levels

   TYPE(domain_levels) , DIMENSION(max_levels)            :: head_for_each_level

END MODULE module_domain1
