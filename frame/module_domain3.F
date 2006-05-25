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

MODULE module_domain3

   USE module_domain1

CONTAINS

   SUBROUTINE adjust_domain_dims_for_move( grid , dx, dy )
    IMPLICIT NONE

    TYPE( domain ), POINTER   :: grid
    INTEGER, INTENT(IN) ::  dx, dy

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
            grid%sm31  =             grid%sm31 + dx
            grid%em31  =             grid%em31 + dx
            grid%sm32  =             grid%sm32 + dy
            grid%em32  =             grid%em32 + dy
            grid%sp31  =             grid%sp31 + dx
            grid%ep31  =             grid%ep31 + dx
            grid%sp32  =             grid%sp32 + dy
            grid%ep32  =             grid%ep32 + dy
            grid%sd31  =             grid%sd31 + dx
            grid%ed31  =             grid%ed31 + dx
            grid%sd32  =             grid%sd32 + dy
            grid%ed32  =             grid%ed32 + dy

       CASE  ( DATA_ORDER_YXZ )
            grid%sm31  =             grid%sm31 + dy
            grid%em31  =             grid%em31 + dy
            grid%sm32  =             grid%sm32 + dx
            grid%em32  =             grid%em32 + dx
            grid%sp31  =             grid%sp31 + dy
            grid%ep31  =             grid%ep31 + dy
            grid%sp32  =             grid%sp32 + dx
            grid%ep32  =             grid%ep32 + dx
            grid%sd31  =             grid%sd31 + dy
            grid%ed31  =             grid%ed31 + dy
            grid%sd32  =             grid%sd32 + dx
            grid%ed32  =             grid%ed32 + dx

       CASE  ( DATA_ORDER_ZXY )
            grid%sm32  =             grid%sm32 + dx
            grid%em32  =             grid%em32 + dx
            grid%sm33  =             grid%sm33 + dy
            grid%em33  =             grid%em33 + dy
            grid%sp32  =             grid%sp32 + dx
            grid%ep32  =             grid%ep32 + dx
            grid%sp33  =             grid%sp33 + dy
            grid%ep33  =             grid%ep33 + dy
            grid%sd32  =             grid%sd32 + dx
            grid%ed32  =             grid%ed32 + dx
            grid%sd33  =             grid%sd33 + dy
            grid%ed33  =             grid%ed33 + dy

       CASE  ( DATA_ORDER_ZYX )
            grid%sm32  =             grid%sm32 + dy
            grid%em32  =             grid%em32 + dy
            grid%sm33  =             grid%sm33 + dx
            grid%em33  =             grid%em33 + dx
            grid%sp32  =             grid%sp32 + dy
            grid%ep32  =             grid%ep32 + dy
            grid%sp33  =             grid%sp33 + dx
            grid%ep33  =             grid%ep33 + dx
            grid%sd32  =             grid%sd32 + dy
            grid%ed32  =             grid%ed32 + dy
            grid%sd33  =             grid%sd33 + dx
            grid%ed33  =             grid%ed33 + dx

       CASE  ( DATA_ORDER_XZY )
            grid%sm31  =             grid%sm31 + dx
            grid%em31  =             grid%em31 + dx
            grid%sm33  =             grid%sm33 + dy
            grid%em33  =             grid%em33 + dy
            grid%sp31  =             grid%sp31 + dx
            grid%ep31  =             grid%ep31 + dx
            grid%sp33  =             grid%sp33 + dy
            grid%ep33  =             grid%ep33 + dy
            grid%sd31  =             grid%sd31 + dx
            grid%ed31  =             grid%ed31 + dx
            grid%sd33  =             grid%sd33 + dy
            grid%ed33  =             grid%ed33 + dy

       CASE  ( DATA_ORDER_YZX )
            grid%sm31  =             grid%sm31 + dy
            grid%em31  =             grid%em31 + dy
            grid%sm33  =             grid%sm33 + dx
            grid%em33  =             grid%em33 + dx
            grid%sp31  =             grid%sp31 + dy
            grid%ep31  =             grid%ep31 + dy
            grid%sp33  =             grid%sp33 + dx
            grid%ep33  =             grid%ep33 + dx
            grid%sd31  =             grid%sd31 + dy
            grid%ed31  =             grid%ed31 + dy
            grid%sd33  =             grid%sd33 + dx
            grid%ed33  =             grid%ed33 + dx

    END SELECT data_ordering

#if 0
    CALL dealloc_space_field ( grid )

    CALL alloc_space_field ( grid, grid%id , 1 , 2 , .FALSE. ,     &
                             grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                             grid%sm31,  grid%em31,  grid%sm32,  grid%em32,  grid%sm33,  grid%em33, &
                             grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &   ! x-xpose
                             grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y  &   ! y-xpose
      )
#endif

    RETURN

   END SUBROUTINE adjust_domain_dims_for_move


   SUBROUTINE get_ijk_from_grid (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe    )

    IMPLICIT NONE

    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           ids             = grid%sd31 
           ide             = grid%ed31 
           jds             = grid%sd32 
           jde             = grid%ed32 
           kds             = grid%sd33 
           kde             = grid%ed33 
           ims             = grid%sm31 
           ime             = grid%em31 
           jms             = grid%sm32 
           jme             = grid%em32 
           kms             = grid%sm33 
           kme             = grid%em33 
           ips             = grid%sp31 
           ipe             = grid%ep31 
           jps             = grid%sp32 
           jpe             = grid%ep32 
           kps             = grid%sp33 
           kpe             = grid%ep33 

       CASE  ( DATA_ORDER_YXZ )
           ids             = grid%sd32 
           ide             = grid%ed32 
           jds             = grid%sd31 
           jde             = grid%ed31 
           kds             = grid%sd33 
           kde             = grid%ed33 
           ims             = grid%sm32 
           ime             = grid%em32 
           jms             = grid%sm31 
           jme             = grid%em31 
           kms             = grid%sm33 
           kme             = grid%em33 
           ips             = grid%sp32 
           ipe             = grid%ep32 
           jps             = grid%sp31 
           jpe             = grid%ep31 
           kps             = grid%sp33 
           kpe             = grid%ep33 

       CASE  ( DATA_ORDER_ZXY )
           ids             = grid%sd32 
           ide             = grid%ed32 
           jds             = grid%sd33 
           jde             = grid%ed33 
           kds             = grid%sd31 
           kde             = grid%ed31 
           ims             = grid%sm32 
           ime             = grid%em32 
           jms             = grid%sm33 
           jme             = grid%em33 
           kms             = grid%sm31 
           kme             = grid%em31 
           ips             = grid%sp32 
           ipe             = grid%ep32 
           jps             = grid%sp33 
           jpe             = grid%ep33 
           kps             = grid%sp31 
           kpe             = grid%ep31 

       CASE  ( DATA_ORDER_ZYX )
           ids             = grid%sd33 
           ide             = grid%ed33 
           jds             = grid%sd32 
           jde             = grid%ed32 
           kds             = grid%sd31 
           kde             = grid%ed31 
           ims             = grid%sm33 
           ime             = grid%em33 
           jms             = grid%sm32 
           jme             = grid%em32 
           kms             = grid%sm31 
           kme             = grid%em31 
           ips             = grid%sp33 
           ipe             = grid%ep33 
           jps             = grid%sp32 
           jpe             = grid%ep32 
           kps             = grid%sp31 
           kpe             = grid%ep31 

       CASE  ( DATA_ORDER_XZY )
           ids             = grid%sd31 
           ide             = grid%ed31 
           jds             = grid%sd33 
           jde             = grid%ed33 
           kds             = grid%sd32 
           kde             = grid%ed32 
           ims             = grid%sm31 
           ime             = grid%em31 
           jms             = grid%sm33 
           jme             = grid%em33 
           kms             = grid%sm32 
           kme             = grid%em32 
           ips             = grid%sp31 
           ipe             = grid%ep31 
           jps             = grid%sp33 
           jpe             = grid%ep33 
           kps             = grid%sp32 
           kpe             = grid%ep32 

       CASE  ( DATA_ORDER_YZX )
           ids             = grid%sd33 
           ide             = grid%ed33 
           jds             = grid%sd31 
           jde             = grid%ed31 
           kds             = grid%sd32 
           kde             = grid%ed32 
           ims             = grid%sm33 
           ime             = grid%em33 
           jms             = grid%sm31 
           jme             = grid%em31 
           kms             = grid%sm32 
           kme             = grid%em32 
           ips             = grid%sp33 
           ipe             = grid%ep33 
           jps             = grid%sp31 
           jpe             = grid%ep31 
           kps             = grid%sp32 
           kpe             = grid%ep32 

    END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid

! Default version ; Otherwise module containing interface to DM library will provide

   SUBROUTINE wrf_patch_domain( id , domdesc , parent, parent_id , parent_domdesc , &
                            sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                            sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                            sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                        sp1x , ep1x , sm1x , em1x , &
                                        sp2x , ep2x , sm2x , em2x , &
                                        sp3x , ep3x , sm3x , em3x , &
                                        sp1y , ep1y , sm1y , em1y , &
                                        sp2y , ep2y , sm2y , em2y , &
                                        sp3y , ep3y , sm3y , em3y , &
                            bdx , bdy , bdy_mask )
!<DESCRIPTION>
! Wrf_patch_domain is called as part of the process of initiating a new
! domain.  Based on the global domain dimension information that is
! passed in it computes the patch and memory dimensions on this
! distributed-memory process for parallel compilation when DM_PARALLEL is
! defined in configure.wrf.  In this case, it relies on an external
! communications package-contributed routine, wrf_dm_patch_domain. For
! non-parallel compiles, it returns the patch and memory dimensions based
! on the entire domain. In either case, the memory dimensions will be
! larger than the patch dimensions, since they allow for distributed
! memory halo regions (DM_PARALLEL only) and for boundary regions around
! the domain (used for idealized cases only).  The width of the boundary
! regions to be accommodated is passed in as bdx and bdy.
! 
! The bdy_mask argument is a four-dimensional logical array, each element
! of which is set to true for any boundaries that this process's patch
! contains (all four are true in the non-DM_PARALLEL case) and false
! otherwise. The indices into the bdy_mask are defined in
! frame/module_state_description.F. P_XSB corresponds boundary that
! exists at the beginning of the X-dimension; ie. the western boundary;
! P_XEB to the boundary that corresponds to the end of the X-dimension
! (east). Likewise for Y (south and north respectively).
! 
! The correspondence of the first, second, and third dimension of each
! set (domain, memory, and patch) with the coordinate axes of the model
! domain is based on the setting of the variable model_data_order, which
! comes into this routine through USE association of
! module_driver_constants in the enclosing module of this routine,
! module_domain.  Model_data_order is defined by the Registry, based on
! the dimspec entries which associate dimension specifiers (e.g. 'k') in
! the Registry with a coordinate axis and specify which dimension of the
! arrays they represent. For WRF, the sd1 , ed1 , sp1 , ep1 , sm1 , and
! em1 correspond to the starts and ends of the global, patch, and memory
! dimensions in X; those with 2 specify Z (vertical); and those with 3
! specify Y.  Note that the WRF convention is to overdimension to allow
! for staggered fields so that sd<em>n</em>:ed<em>n</em> are the starts
! and ends of the staggered domains in X.  The non-staggered grid runs
! sd<em>n</em>:ed<em>n</em>-1. The extra row or column on the north or
! east boundaries is not used for non-staggered fields.
! 
! The domdesc and parent_domdesc arguments are for external communication
! packages (e.g. RSL) that establish and return to WRF integer handles
! for referring to operations on domains.  These descriptors are not set
! or used otherwise and they are opaque, which means they are never
! accessed or modified in WRF; they are only only passed between calls to
! the external package.
!</DESCRIPTION>

   USE module_machine
   IMPLICIT NONE
   LOGICAL, DIMENSION(4), INTENT(OUT)  :: bdy_mask
   INTEGER, INTENT(IN)   :: sd1 , ed1 , sd2 , ed2 , sd3 , ed3 , bdx , bdy
   INTEGER, INTENT(OUT)  :: sp1  , ep1  , sp2  , ep2  , sp3  , ep3  , &  ! z-xpose (std)
                            sm1  , em1  , sm2  , em2  , sm3  , em3
   INTEGER, INTENT(OUT)  :: sp1x , ep1x , sp2x , ep2x , sp3x , ep3x , &  ! x-xpose
                            sm1x , em1x , sm2x , em2x , sm3x , em3x
   INTEGER, INTENT(OUT)  :: sp1y , ep1y , sp2y , ep2y , sp3y , ep3y , &  ! y-xpose
                            sm1y , em1y , sm2y , em2y , sm3y , em3y
   INTEGER, INTENT(IN)   :: id , parent_id , parent_domdesc
   INTEGER, INTENT(INOUT)  :: domdesc
   TYPE(domain), POINTER :: parent

!local data

   INTEGER spec_bdy_width

   CALL nl_get_spec_bdy_width( 1, spec_bdy_width )

#ifndef DM_PARALLEL

   bdy_mask = .true.     ! only one processor so all 4 boundaries are there

! this is a trivial version -- 1 patch per processor; 
! use version in module_dm to compute for DM
   sp1 = sd1 ; sp2 = sd2 ; sp3 = sd3
   ep1 = ed1 ; ep2 = ed2 ; ep3 = ed3
   SELECT CASE ( model_data_order )
      CASE ( DATA_ORDER_XYZ )
         sm1  = sp1 - bdx ; em1 = ep1 + bdx
         sm2  = sp2 - bdy ; em2 = ep2 + bdy
         sm3  = sp3       ; em3 = ep3
      CASE ( DATA_ORDER_YXZ )
         sm1 = sp1 - bdy ; em1 = ep1 + bdy
         sm2 = sp2 - bdx ; em2 = ep2 + bdx
         sm3 = sp3       ; em3 = ep3
      CASE ( DATA_ORDER_ZXY )
         sm1 = sp1       ; em1 = ep1
         sm2 = sp2 - bdx ; em2 = ep2 + bdx
         sm3 = sp3 - bdy ; em3 = ep3 + bdy
      CASE ( DATA_ORDER_ZYX )
         sm1 = sp1       ; em1 = ep1
         sm2 = sp2 - bdy ; em2 = ep2 + bdy
         sm3 = sp3 - bdx ; em3 = ep3 + bdx
      CASE ( DATA_ORDER_XZY )
         sm1 = sp1 - bdx ; em1 = ep1 + bdx
         sm2 = sp2       ; em2 = ep2
         sm3 = sp3 - bdy ; em3 = ep3 + bdy
      CASE ( DATA_ORDER_YZX )
         sm1 = sp1 - bdy ; em1 = ep1 + bdy
         sm2 = sp2       ; em2 = ep2
         sm3 = sp3 - bdx ; em3 = ep3 + bdx
   END SELECT
   sm1x = sm1       ; em1x = em1    ! just copy
   sm2x = sm2       ; em2x = em2
   sm3x = sm3       ; em3x = em3
   sm1y = sm1       ; em1y = em1    ! just copy
   sm2y = sm2       ; em2y = em2
   sm3y = sm3       ; em3y = em3
! assigns mostly just to suppress warning messages that INTENT OUT vars not assigned
   sp1x = sp1 ; ep1x = ep1 ; sp2x = sp2 ; ep2x = ep2 ; sp3x = sp3 ; ep3x = ep3
   sp1y = sp1 ; ep1y = ep1 ; sp2y = sp2 ; ep2y = ep2 ; sp3y = sp3 ; ep3y = ep3

#else
! This is supplied by the package specific version of module_dm, which
! is supplied by the external package and copied into the src directory
! when the code is compiled. The cp command will be found in the externals
! target of the configure.wrf file for this architecture.  Eg: for RSL
! routine is defined in external/RSL/module_dm.F .
! Note, it would be very nice to be able to pass parent to this routine;
! however, there doesn't seem to be a way to do that in F90. That is because
! to pass a pointer to a domain structure, this call requires an interface
! definition for wrf_dm_patch_domain (otherwise it will try to convert the
! pointer to something). In order to provide an interface definition, we
! would need to either USE module_dm or use an interface block. In either
! case it generates a circular USE reference, since module_dm uses
! module_domain.  JM 20020416

   CALL wrf_dm_patch_domain( id , domdesc , parent_id , parent_domdesc , &
                             sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                             sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                             sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                         sp1x , ep1x , sm1x , em1x , &
                                         sp2x , ep2x , sm2x , em2x , &
                                         sp3x , ep3x , sm3x , em3x , &
                                         sp1y , ep1y , sm1y , em1y , &
                                         sp2y , ep2y , sm2y , em2y , &
                                         sp3y , ep3y , sm3y , em3y , &
                             bdx , bdy )

   SELECT CASE ( model_data_order )
      CASE ( DATA_ORDER_XYZ )
   bdy_mask( P_XSB ) = ( sp1 <= sd1 .AND. sd1 <= ep1 .AND. sp1 <= sd1+spec_bdy_width-1 .AND. sd1+spec_bdy_width-1 <= ep1 )
   bdy_mask( P_YSB ) = ( sp2 <= sd2 .AND. sd2 <= ep2 .AND. sp2 <= sd2+spec_bdy_width-1 .AND. sd2+spec_bdy_width-1 <= ep2 )
   bdy_mask( P_XEB ) = ( sp1 <= ed1 .AND. ed1 <= ep1 .AND. sp1 <= ed1-spec_bdy_width-1 .AND. ed1-spec_bdy_width-1 <= ep1 )
   bdy_mask( P_YEB ) = ( sp2 <= ed2 .AND. ed2 <= ep2 .AND. sp2 <= ed2-spec_bdy_width-1 .AND. ed2-spec_bdy_width-1 <= ep2 )
      CASE ( DATA_ORDER_YXZ )
   bdy_mask( P_XSB ) = ( sp2 <= sd2 .AND. sd2 <= ep2 .AND. sp2 <= sd2+spec_bdy_width-1 .AND. sd2+spec_bdy_width-1 <= ep2 )
   bdy_mask( P_YSB ) = ( sp1 <= sd1 .AND. sd1 <= ep1 .AND. sp1 <= sd1+spec_bdy_width-1 .AND. sd1+spec_bdy_width-1 <= ep1 )
   bdy_mask( P_XEB ) = ( sp2 <= ed2 .AND. ed2 <= ep2 .AND. sp2 <= ed2-spec_bdy_width-1 .AND. ed2-spec_bdy_width-1 <= ep2 )
   bdy_mask( P_YEB ) = ( sp1 <= ed1 .AND. ed1 <= ep1 .AND. sp1 <= ed1-spec_bdy_width-1 .AND. ed1-spec_bdy_width-1 <= ep1 )
      CASE ( DATA_ORDER_ZXY )
   bdy_mask( P_XSB ) = ( sp2 <= sd2 .AND. sd2 <= ep2 .AND. sp2 <= sd2+spec_bdy_width-1 .AND. sd2+spec_bdy_width-1 <= ep2 )
   bdy_mask( P_YSB ) = ( sp3 <= sd3 .AND. sd3 <= ep3 .AND. sp3 <= sd3+spec_bdy_width-1 .AND. sd3+spec_bdy_width-1 <= ep3 )
   bdy_mask( P_XEB ) = ( sp2 <= ed2 .AND. ed2 <= ep2 .AND. sp2 <= ed2-spec_bdy_width-1 .AND. ed2-spec_bdy_width-1 <= ep2 )
   bdy_mask( P_YEB ) = ( sp3 <= ed3 .AND. ed3 <= ep3 .AND. sp3 <= ed3-spec_bdy_width-1 .AND. ed3-spec_bdy_width-1 <= ep3 )
      CASE ( DATA_ORDER_ZYX )
   bdy_mask( P_XSB ) = ( sp3 <= sd3 .AND. sd3 <= ep3 .AND. sp3 <= sd3+spec_bdy_width-1 .AND. sd3+spec_bdy_width-1 <= ep3 )
   bdy_mask( P_YSB ) = ( sp2 <= sd2 .AND. sd2 <= ep2 .AND. sp2 <= sd2+spec_bdy_width-1 .AND. sd2+spec_bdy_width-1 <= ep2 )
   bdy_mask( P_XEB ) = ( sp3 <= ed3 .AND. ed3 <= ep3 .AND. sp3 <= ed3-spec_bdy_width-1 .AND. ed3-spec_bdy_width-1 <= ep3 )
   bdy_mask( P_YEB ) = ( sp2 <= ed2 .AND. ed2 <= ep2 .AND. sp2 <= ed2-spec_bdy_width-1 .AND. ed2-spec_bdy_width-1 <= ep2 )
      CASE ( DATA_ORDER_XZY )
   bdy_mask( P_XSB ) = ( sp1 <= sd1 .AND. sd1 <= ep1 .AND. sp1 <= sd1+spec_bdy_width-1 .AND. sd1+spec_bdy_width-1 <= ep1 )
   bdy_mask( P_YSB ) = ( sp3 <= sd3 .AND. sd3 <= ep3 .AND. sp3 <= sd3+spec_bdy_width-1 .AND. sd3+spec_bdy_width-1 <= ep3 )
   bdy_mask( P_XEB ) = ( sp1 <= ed1 .AND. ed1 <= ep1 .AND. sp1 <= ed1-spec_bdy_width-1 .AND. ed1-spec_bdy_width-1 <= ep1 )
   bdy_mask( P_YEB ) = ( sp3 <= ed3 .AND. ed3 <= ep3 .AND. sp3 <= ed3-spec_bdy_width-1 .AND. ed3-spec_bdy_width-1 <= ep3 )
      CASE ( DATA_ORDER_YZX )
   bdy_mask( P_XSB ) = ( sp3 <= sd3 .AND. sd3 <= ep3 .AND. sp3 <= sd3+spec_bdy_width-1 .AND. sd3+spec_bdy_width-1 <= ep3 )
   bdy_mask( P_YSB ) = ( sp1 <= sd1 .AND. sd1 <= ep1 .AND. sp1 <= sd1+spec_bdy_width-1 .AND. sd1+spec_bdy_width-1 <= ep1 )
   bdy_mask( P_XEB ) = ( sp3 <= ed3 .AND. ed3 <= ep3 .AND. sp3 <= ed3-spec_bdy_width-1 .AND. ed3-spec_bdy_width-1 <= ep3 )
   bdy_mask( P_YEB ) = ( sp1 <= ed1 .AND. ed1 <= ep1 .AND. sp1 <= ed1-spec_bdy_width-1 .AND. ed1-spec_bdy_width-1 <= ep1 )
   END SELECT

#endif

   RETURN
   END SUBROUTINE wrf_patch_domain

END MODULE module_domain3
