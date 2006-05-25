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

MODULE module_domain2

   USE module_domain1
   USE module_domain3

CONTAINS

   SUBROUTINE alloc_and_configure_domain ( domain_id , grid , parent, kid )

!<DESCRIPTION>
! This subroutine is used to allocate a domain data structure of
! TYPE(DOMAIN) pointed to by the argument <em>grid</em>, link it into the
! nested domain hierarchy, and set it's configuration information from
! the appropriate settings in the WRF namelist file. Specifically, if the
! domain being allocated and configured is nest, the <em>parent</em>
! argument will point to the already existing domain data structure for
! the parent domain and the <em>kid</em> argument will be set to an
! integer indicating which child of the parent this grid will be (child
! indices start at 1).  If this is the top-level domain, the parent and
! kid arguments are ignored.  <b>WRF domains may have multiple children
! but only ever have one parent.</b>
!
! The <em>domain_id</em> argument is the
! integer handle by which this new domain will be referred; it comes from
! the grid_id setting in the namelist, and these grid ids correspond to
! the ordering of settings in the namelist, starting with 1 for the
! top-level domain. The id of 1 always corresponds to the top-level
! domain.  and these grid ids correspond to the ordering of settings in
! the namelist, starting with 1 for the top-level domain.
! 
! Model_data_order is provide by USE association of
! module_driver_constants and is set from dimspec entries in the
! Registry.
! 
! The allocation of the TYPE(DOMAIN) itself occurs in this routine.
! However, the numerous multi-dimensional arrays that make up the members
! of the domain are allocated in the call to alloc_space_field, after
! wrf_patch_domain has been called to determine the dimensions in memory
! that should be allocated.  It bears noting here that arrays and code
! that indexes these arrays are always global, regardless of how the
! model is decomposed over patches. Thus, when arrays are allocated on a
! given process, the start and end of an array dimension are the global
! indices of the start and end of that process's subdomain.
! 
! Configuration information for the domain (that is, information from the
! namelist) is added by the call to <a href=med_add_config_info_to_grid.html>med_add_config_info_to_grid</a>, defined
! in share/mediation_wrfmain.F. 
!</DESCRIPTION>

      
      IMPLICIT NONE

      !  Input data.

      INTEGER , INTENT(IN)                           :: domain_id
      TYPE( domain ) , POINTER                       :: grid
      TYPE( domain ) , POINTER                       :: parent
      INTEGER , INTENT(IN)                           :: kid    ! which kid of parent am I?

      !  Local data.
      INTEGER                     :: sd1 , ed1 , sp1 , ep1 , sm1 , em1
      INTEGER                     :: sd2 , ed2 , sp2 , ep2 , sm2 , em2
      INTEGER                     :: sd3 , ed3 , sp3 , ep3 , sm3 , em3

      INTEGER                     :: sd1x , ed1x , sp1x , ep1x , sm1x , em1x
      INTEGER                     :: sd2x , ed2x , sp2x , ep2x , sm2x , em2x
      INTEGER                     :: sd3x , ed3x , sp3x , ep3x , sm3x , em3x

      INTEGER                     :: sd1y , ed1y , sp1y , ep1y , sm1y , em1y
      INTEGER                     :: sd2y , ed2y , sp2y , ep2y , sm2y , em2y
      INTEGER                     :: sd3y , ed3y , sp3y , ep3y , sm3y , em3y

      TYPE(domain) , POINTER      :: new_grid
      INTEGER                     :: i
      INTEGER                     :: parent_id , parent_domdesc , new_domdesc
      INTEGER                     :: bdyzone_x , bdyzone_y
      INTEGER                     :: nx, ny


! This next step uses information that is listed in the registry as namelist_derived
! to properly size the domain and the patches; this in turn is stored in the new_grid
! data structure


      data_ordering : SELECT CASE ( model_data_order )
        CASE  ( DATA_ORDER_XYZ )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_YXZ )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed1-sd1+1

        CASE  ( DATA_ORDER_ZXY )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_ZYX )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_XZY )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_YZX )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed1-sd1+1

      END SELECT data_ordering


#ifdef RSL
! Check domain size to be sure it is within RSLs limit
      IF ( nx .GE. 1024 .OR. ny .GE. 1024 ) THEN
        WRITE ( wrf_err_message , * ) 'domain too large for RSL. Use RSL_LITE or other comm package.'
        CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
      ENDIF

#endif

      IF ( num_time_levels > 3 ) THEN
        WRITE ( wrf_err_message , * ) 'alloc_and_configure_domain: Incorrect value for num_time_levels ', &
                                       num_time_levels
        CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
      ENDIF

      IF (ASSOCIATED(parent)) THEN
        parent_id = parent%id
        parent_domdesc = parent%domdesc
      ELSE
        parent_id = -1
        parent_domdesc = -1
      ENDIF

! provided by application, WRF defines in share/module_bc.F
      CALL get_bdyzone_x( bdyzone_x )
      CALL get_bdyzone_y( bdyzone_y )

      ALLOCATE ( new_grid )
      ALLOCATE ( new_grid%parents( max_parents ) )
      ALLOCATE ( new_grid%nests( max_nests ) )
      NULLIFY( new_grid%sibling )
      DO i = 1, max_nests
         NULLIFY( new_grid%nests(i)%ptr )
      ENDDO
      NULLIFY  (new_grid%next)
      NULLIFY  (new_grid%same_level)
      NULLIFY  (new_grid%i_start)
      NULLIFY  (new_grid%j_start)
      NULLIFY  (new_grid%i_end)
      NULLIFY  (new_grid%j_end)
      ALLOCATE( new_grid%domain_clock )
      ALLOCATE( new_grid%alarms( MAX_WRF_ALARMS ) )    ! initialize in setup_timekeeping
      ALLOCATE( new_grid%alarms_created( MAX_WRF_ALARMS ) )
      DO i = 1, MAX_WRF_ALARMS
        new_grid%alarms_created( i ) = .FALSE.
      ENDDO

#ifdef MOVE_NESTS
      new_grid%xi = -1.0
      new_grid%xj = -1.0
      new_grid%vc_i = -1.0
      new_grid%vc_j = -1.0
#endif

      ! set up the pointers that represent the nest hierarchy
      ! set this up *prior* to calling the patching or allocation
      ! routines so that implementations of these routines can
      ! traverse the nest hierarchy (through the root head_grid)
      ! if they need to 

 
      IF ( domain_id .NE. 1 ) THEN
         new_grid%parents(1)%ptr => parent
         new_grid%num_parents = 1
         parent%nests(kid)%ptr => new_grid
         new_grid%child_of_parent(1) = kid    ! note assumption that nest can have only 1 parent
         parent%num_nests = parent%num_nests + 1
      END IF
      new_grid%id = domain_id                 ! this needs to be assigned prior to calling wrf_patch_domain

      CALL wrf_patch_domain( domain_id  , new_domdesc , parent, parent_id, parent_domdesc , &

                             sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &     ! z-xpose dims
                             sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &     ! (standard)
                             sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &

                                     sp1x , ep1x , sm1x , em1x , &     ! x-xpose dims
                                     sp2x , ep2x , sm2x , em2x , &
                                     sp3x , ep3x , sm3x , em3x , &

                                     sp1y , ep1y , sm1y , em1y , &     ! y-xpose dims
                                     sp2y , ep2y , sm2y , em2y , &
                                     sp3y , ep3y , sm3y , em3y , &

                         bdyzone_x  , bdyzone_y , new_grid%bdy_mask &
      ) 


      new_grid%domdesc = new_domdesc
      new_grid%num_nests = 0
      new_grid%num_siblings = 0
      new_grid%num_parents = 0
      new_grid%max_tiles   = 0
      new_grid%num_tiles_spec   = 0
      new_grid%nframes   = 0         ! initialize the number of frames per file (array assignment)

      CALL alloc_space_field ( new_grid, domain_id , 3 , 3 , .FALSE. ,      &
                               sd1, ed1, sd2, ed2, sd3, ed3,       &
                               sm1,  em1,  sm2,  em2,  sm3,  em3,  &
                               sm1x, em1x, sm2x, em2x, sm3x, em3x, &   ! x-xpose
                               sm1y, em1y, sm2y, em2y, sm3y, em3y  &   ! y-xpose
      )

      new_grid%sd31                            = sd1 
      new_grid%ed31                            = ed1
      new_grid%sp31                            = sp1 
      new_grid%ep31                            = ep1 
      new_grid%sm31                            = sm1 
      new_grid%em31                            = em1
      new_grid%sd32                            = sd2 
      new_grid%ed32                            = ed2
      new_grid%sp32                            = sp2 
      new_grid%ep32                            = ep2 
      new_grid%sm32                            = sm2 
      new_grid%em32                            = em2
      new_grid%sd33                            = sd3 
      new_grid%ed33                            = ed3
      new_grid%sp33                            = sp3 
      new_grid%ep33                            = ep3 
      new_grid%sm33                            = sm3 
      new_grid%em33                            = em3

      new_grid%sp31x                           = sp1x
      new_grid%ep31x                           = ep1x
      new_grid%sm31x                           = sm1x
      new_grid%em31x                           = em1x
      new_grid%sp32x                           = sp2x
      new_grid%ep32x                           = ep2x
      new_grid%sm32x                           = sm2x
      new_grid%em32x                           = em2x
      new_grid%sp33x                           = sp3x
      new_grid%ep33x                           = ep3x
      new_grid%sm33x                           = sm3x
      new_grid%em33x                           = em3x

      new_grid%sp31y                           = sp1y
      new_grid%ep31y                           = ep1y
      new_grid%sm31y                           = sm1y
      new_grid%em31y                           = em1y
      new_grid%sp32y                           = sp2y
      new_grid%ep32y                           = ep2y
      new_grid%sm32y                           = sm2y
      new_grid%em32y                           = em2y
      new_grid%sp33y                           = sp3y
      new_grid%ep33y                           = ep3y
      new_grid%sm33y                           = sm3y
      new_grid%em33y                           = em3y

      SELECT CASE ( model_data_order )
         CASE  ( DATA_ORDER_XYZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YXZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_ZXY )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_ZYX )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_XZY )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YZX )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
      END SELECT

      CALL med_add_config_info_to_grid ( new_grid )           ! this is a mediation layer routine

! Some miscellaneous state that is in the Registry but not namelist data

      new_grid%tiled                           = .false.
      new_grid%patched                         = .false.
      NULLIFY(new_grid%mapping)

! This next set of includes causes all but the namelist_derived variables to be
! properly assigned to the new_grid record

      grid => new_grid

#ifdef DM_PARALLEL
      CALL wrf_get_dm_communicator ( grid%communicator )
      CALL wrf_dm_define_comms( grid )
#endif

   END SUBROUTINE alloc_and_configure_domain

!

!  This routine ALLOCATEs the required space for the meteorological fields
!  for a specific domain.  The fields are simply ALLOCATEd as an -1.  They
!  are referenced as wind, temperature, moisture, etc. in routines that are
!  below this top-level of data allocation and management (in the solve routine
!  and below).

   SUBROUTINE alloc_space_field ( grid,   id, setinitval ,  tl_in , inter_domain_in ,   &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )

      
      USE module_configure
      IMPLICIT NONE
 

      !  Input data.

      TYPE(domain)               , POINTER          :: grid
      INTEGER , INTENT(IN)            :: id
      INTEGER , INTENT(IN)            :: setinitval   ! 3 = everything, 1 = arrays only, 0 = none
      INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y

      ! this argument is a bitmask. First bit is time level 1, second is time level 2, and so on.
      ! e.g. to set both 1st and second time level, use 3
      !      to set only 1st                        use 1
      !      to set only 2st                        use 2
      INTEGER , INTENT(IN)            :: tl_in
  
      ! true if the allocation is for an intermediate domain (for nesting); only certain fields allocated
      ! false otherwise (all allocated, modulo tl above)
      LOGICAL , INTENT(IN)            :: inter_domain_in

      !  Local data.
      INTEGER dyn_opt, idum1, idum2, spec_bdy_width
      INTEGER num_bytes_allocated
      REAL    initial_data_value
      CHARACTER (LEN=256) message
      INTEGER tl
      LOGICAL inter_domain

      !declare ierr variable for error checking ALLOCATE calls
      INTEGER ierr

      INTEGER                              :: loop

#if 1
      tl = tl_in
      inter_domain = inter_domain_in
#else
      tl = 3
      inter_domain = .FALSE.
#endif

#if ( RWORDSIZE == 8 )
      initial_data_value = 0.
#else
      CALL get_initial_data_value ( initial_data_value )
#endif

      CALL nl_get_dyn_opt( 1, dyn_opt )
      CALL nl_get_spec_bdy_width( 1, spec_bdy_width )

      CALL set_scalar_indices_from_config( id , idum1 , idum2 )

      num_bytes_allocated = 0 


      IF ( dyn_opt == DYN_NODYN ) THEN

        IF ( grid%id .EQ. 1 ) CALL wrf_message ( 'DYNAMICS OPTION: dynamics disabled ' )

#if ALLOW_NODYN
# include <nodyn_allocs.inc>
#else
        CALL wrf_error_fatal( &
          "To run the the NODYN option, recompile -DALLOW_NODYN in ARCHFLAGS settings of configure.wrf") 
#endif

write (message,*) "dyn_opt,DYN_EM,mod(dyn_opt, 100)",dyn_opt,DYN_EM,mod(dyn_opt, 100)
      CALL  wrf_debug( 1, message )

write (message,*) "grid%id",grid%id
      CALL  wrf_debug( 1, message )
#if (EM_CORE == 1)
      ELSE IF ( mod(dyn_opt, 100) == DYN_EM ) THEN
        IF ( grid%id .EQ. 1 ) CALL wrf_message ( 'DYNAMICS OPTION: Eulerian Mass Coordinate ')
# include <em_allocs.inc>
#endif
#if (NMM_CORE == 1)
      ELSE IF ( dyn_opt == DYN_NMM ) THEN
        IF ( grid%id .EQ. 1 ) CALL wrf_message ( 'DYNAMICS OPTION: nmm dyncore' )
# include <nmm_allocs.inc>
#endif
#if (COAMPS_CORE == 1)
      ELSE IF ( dyn_opt == DYN_COAMPS ) THEN
        IF ( grid%id .EQ. 1 ) CALL wrf_message ( 'DYNAMICS OPTION: coamps dyncore' )
# include <coamps_allocs.inc>
#endif
#if (EXP_CORE==1)
      ELSE IF ( dyn_opt == DYN_EXP ) THEN
        IF ( grid%id .EQ. 1 ) CALL wrf_message ( 'DYNAMICS OPTION: experimental dyncore' )
# include <exp_allocs.inc>
#endif

      ELSE
!JRB      
!        WRITE( wrf_err_message , * )'Invalid specification of dynamics: dyn_opt = ',dyn_opt
!        CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
      ENDIF

      WRITE(message,*)'alloc_space_field: domain ',id,' ',num_bytes_allocated
      CALL  wrf_debug( 1, message )

   END SUBROUTINE alloc_space_field

!

!  This routine is used to DEALLOCATE space for a single domain.  First
!  the pointers in the linked list are fixed (so the one in the middle can
!  be removed).  Second, the field data are all removed through a CALL to 
!  the dealloc_space_domain routine.  Finally, the pointer to the domain
!  itself is DEALLOCATEd.

   SUBROUTINE dealloc_space_domain ( id )
      
      IMPLICIT NONE

      !  Input data.

      INTEGER , INTENT(IN)            :: id

      !  Local data.

      TYPE(domain) , POINTER          :: grid
      LOGICAL                         :: found
      INTEGER                         :: alarmid

      !  Initializations required to start the routine.

      grid => head_grid
      old_grid => head_grid
      found = .FALSE.

      !  The identity of the domain to delete is based upon the "id".
      !  We search all of the possible grids.  It is required to find a domain
      !  otherwise it is a fatal error.  

      find_grid : DO WHILE ( ASSOCIATED(grid) ) 
         IF ( grid%id == id ) THEN
            found = .TRUE.
            old_grid%next => grid%next
            CALL dealloc_space_field ( grid )
            DEALLOCATE( grid%parents )
            DEALLOCATE( grid%nests )
            ! clean up time manager bits
            CALL WRF_UTIL_ClockDestroy( grid%domain_clock )
            DO alarmid = 1, MAX_WRF_ALARMS
              IF ( grid%alarms_created( alarmid ) ) THEN
                CALL WRF_UTIL_AlarmDestroy( grid%alarms( alarmid ) )
                grid%alarms_created( alarmid ) = .FALSE.
              ENDIF
            ENDDO
            DEALLOCATE( grid%alarms )
            DEALLOCATE( grid%alarms_created )
            DEALLOCATE( grid%domain_clock )
            DEALLOCATE(grid)
            EXIT find_grid
         END IF
         old_grid => grid
         grid     => grid%next
      END DO find_grid

      IF ( .NOT. found ) THEN
         WRITE ( wrf_err_message , * ) 'module_domain: dealloc_space_domain: Could not de-allocate grid id ',id
         CALL wrf_error_fatal ( TRIM( wrf_err_message ) ) 
      END IF

   END SUBROUTINE dealloc_space_domain

!

!  This routine DEALLOCATEs each gridded field for this domain.  For each type of
!  different array (1d, 2d, 3d, etc.), the space for each pointer is DEALLOCATEd
!  for every -1 (i.e., each different meteorological field).

   SUBROUTINE dealloc_space_field ( grid )
      
      IMPLICIT NONE

      !  Input data.

      TYPE(domain)              , POINTER :: grid

      !  Local data.

      INTEGER                             :: dyn_opt, ierr

      CALL nl_get_dyn_opt( 1, dyn_opt )

      IF      ( .FALSE. )           THEN

#if (EM_CORE == 1)
      ELSE IF ( dyn_opt == DYN_EM ) THEN
# include <em_deallocs.inc>
#endif
#if (NMM_CORE == 1)
      ELSE IF ( dyn_opt == DYN_NMM ) THEN
# include <nmm_deallocs.inc>
#endif
#if (COAMPS_CORE == 1)
      ELSE IF ( dyn_opt == DYN_COAMPS ) THEN
# include <coamps_deallocs.inc>
#endif
#if (EXP_CORE==1)
      ELSE IF ( dyn_opt == DYN_EXP ) THEN
# include <exp_deallocs.inc>
#endif
      ELSE
        WRITE( wrf_err_message , * )'dealloc_space_field: Invalid specification of dynamics: dyn_opt = ',dyn_opt
        CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
      ENDIF

   END SUBROUTINE dealloc_space_field

!
!
   RECURSIVE SUBROUTINE find_grid_by_id ( id, in_grid, result_grid )
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: id
      TYPE(domain), POINTER     :: in_grid 
      TYPE(domain), POINTER     :: result_grid
! <DESCRIPTION>
! This is a recursive subroutine that traverses the domain hierarchy rooted
! at the input argument <em>in_grid</em>, a pointer to TYPE(domain), and returns
! a pointer to the domain matching the integer argument <em>id</em> if it exists.
!
! </DESCRIPTION>
      TYPE(domain), POINTER     :: grid_ptr
      INTEGER                   :: kid
      LOGICAL                   :: found
      found = .FALSE.
      IF ( ASSOCIATED( in_grid ) ) THEN
      IF ( in_grid%id .EQ. id ) THEN
         result_grid => in_grid
      ELSE
         grid_ptr => in_grid
         DO WHILE ( ASSOCIATED( grid_ptr ) .AND. .NOT. found )
            DO kid = 1, max_nests
               IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) .AND. .NOT. found ) THEN
                  CALL find_grid_by_id ( id, grid_ptr%nests(kid)%ptr, result_grid )
                  IF ( ASSOCIATED( result_grid ) ) THEN
                    IF ( result_grid%id .EQ. id ) found = .TRUE.
                  ENDIF
               ENDIF
            ENDDO
            IF ( .NOT. found ) grid_ptr => grid_ptr%sibling
         ENDDO
      ENDIF
      ENDIF
      RETURN
   END SUBROUTINE find_grid_by_id


   FUNCTION first_loc_integer ( array , search ) RESULT ( loc ) 
 
      IMPLICIT NONE

      !  Input data.

      INTEGER , INTENT(IN) , DIMENSION(:) :: array
      INTEGER , INTENT(IN)                :: search

      !  Output data.

      INTEGER                             :: loc

!<DESCRIPTION>
!  This routine is used to find a specific domain identifier in an array
!  of domain identifiers.
!
!</DESCRIPTION>
      
      !  Local data.

      INTEGER :: loop

      loc = -1
      find : DO loop = 1 , SIZE(array)
         IF ( search == array(loop) ) THEN         
            loc = loop
            EXIT find
         END IF
      END DO find

   END FUNCTION first_loc_integer
!
   SUBROUTINE init_module_domain
   END SUBROUTINE init_module_domain

END MODULE module_domain2
