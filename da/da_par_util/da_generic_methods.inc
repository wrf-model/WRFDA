

SUBROUTINE da_res_generic_set_info( res_generic, proc_domain, &
                                      obs_global_index )

!------------------------------------------------------------------------------
! PURPOSE:  Eliminate redundant code for many obs types.  
!
! METHOD:   Set common fields in res_generic.
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (residual_generic_type), INTENT(INOUT) :: res_generic  ! generic type
   LOGICAL,                      INTENT(IN   ) :: proc_domain
   INTEGER,                      INTENT(IN   ) :: obs_global_index

   res_generic%proc_domain = proc_domain
   res_generic%obs_global_index = obs_global_index
END SUBROUTINE da_res_generic_set_info



SUBROUTINE da_res_sound_create_template( template )

!------------------------------------------------------------------------------
! PURPOSE:  Return storage template for specific type stored as 
!           residual_generic_type.
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (residual_template_type), INTENT(INOUT) :: template

   ! only vector data for this type
   template%lbnd = 1
   template%ubnd = 4

END SUBROUTINE da_res_sound_create_template




SUBROUTINE da_res_sound_to_generic( res_specific, iv_num_levels, &
                                      res_generic )

!------------------------------------------------------------------------------
! PURPOSE:  Eliminate redundant code for many obs types.  
!
! METHOD:   Specific type res_specific is translated to generic type 
!           res_generic.  Pointer manipulation is used for vector data, no 
!           vector data is copied.  Scalar data is copied.  This routine 
!           allocates memory for res_generic.  The caller must ensure that 
!           memory is deallocated later.  
!           iv_num_levels is used as a sanity check and is ignored for 
!           generic types that do not contain vector data. 
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (residual_sound_type),   INTENT(IN   ) :: res_specific ! specific type
   INTEGER,                      INTENT(IN   ) :: iv_num_levels ! levels
   TYPE (residual_generic_type), INTENT(INOUT) :: res_generic  ! generic type
   ! Local declarations
   TYPE (residual_template_type) :: template
   INTEGER :: n

   CALL da_res_sound_create_template( template )
   ALLOCATE( res_generic%values(template%lbnd:template%ubnd) )
   ! only vector data for this type
   ! store references to vector data
   res_generic%values(1)%ptr => res_specific%u
   res_generic%values(2)%ptr => res_specific%v
   res_generic%values(3)%ptr => res_specific%t
   res_generic%values(4)%ptr => res_specific%q
   ! ASSERTION
   DO n=1,4
!TBH:  NOTE:  We could handle iv_num_levels < SIZE(res_generic%values(n)%ptr) 
!TBH:         HOWEVER, we would have to add "num_levels" state to 
!TBH:         residual_generic_type AND send this around.  Better to fix 
!TBH:         allocation in specific types to avoid wasting memory!  
     IF ( SIZE(res_generic%values(n)%ptr) /= iv_num_levels ) THEN
       CALL wrf_error_fatal( 'residual_sound_to_generic:  mismatched levels' )
     ENDIF
   ENDDO

END SUBROUTINE da_res_sound_to_generic



SUBROUTINE da_res_sound_from_generic( res_generic, res_specific )

!------------------------------------------------------------------------------
! PURPOSE:  Eliminate redundant code for many obs types.  
!
! METHOD:   Generic type res_generic is translated to specific type 
!           res_specific.  Pointer manipulation is used for vector data, no 
!           vector data is copied.  Scalar data is copied. 
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (residual_generic_type), INTENT(IN   ) :: res_generic  ! generic type
   TYPE (residual_sound_type),   INTENT(INOUT) :: res_specific ! specific type

   ! only vector data for this type
   ! store references to vector data
   res_specific%u => res_generic%values(1)%ptr
   res_specific%v => res_generic%values(2)%ptr
   res_specific%t => res_generic%values(3)%ptr
   res_specific%q => res_generic%values(4)%ptr

END SUBROUTINE da_res_sound_from_generic



SUBROUTINE da_res_synop_create_template( template )

!------------------------------------------------------------------------------
! PURPOSE:  Return storage template for specific type stored as 
!           residual_generic_type.  
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (residual_template_type), INTENT(INOUT) :: template

   ! only scalar data for this type
   template%lbnd = 0
   template%ubnd = 0

END SUBROUTINE da_res_synop_create_template




SUBROUTINE da_res_synop_to_generic( res_specific, iv_num_levels, &
                                      res_generic )

!------------------------------------------------------------------------------
! PURPOSE:  Eliminate redundant code for many obs types.  
!
! METHOD:   Specific type res_specific is translated to generic type 
!           res_generic.  Pointer manipulation is used for vector data, no 
!           vector data is copied.  Scalar data is copied.  This routine 
!           allocates memory for res_generic.  The caller must ensure that 
!           memory is deallocated later.  
!           iv_num_levels is used as a sanity check and is ignored for 
!           generic types that do not contain vector data.
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (residual_synop_type),   INTENT(IN   ) :: res_specific ! specific type
   INTEGER,                      INTENT(IN   ) :: iv_num_levels ! levels
   TYPE (residual_generic_type), INTENT(INOUT) :: res_generic  ! generic type
   ! Local declarations
   TYPE (residual_template_type) :: template

   ! Use to avoid compiler warning
   IF  (iv_num_levels==0) THEN
   END IF

   CALL da_res_synop_create_template( template )
   ALLOCATE( res_generic%values(template%lbnd:template%ubnd) )
   ! only scalar data for this type
   ALLOCATE( res_generic%values(0)%ptr(5) )
   ! copy scalar data
   res_generic%values(0)%ptr(1) = res_specific%u
   res_generic%values(0)%ptr(2) = res_specific%v
   res_generic%values(0)%ptr(3) = res_specific%t
   res_generic%values(0)%ptr(4) = res_specific%p
   res_generic%values(0)%ptr(5) = res_specific%q

END SUBROUTINE da_res_synop_to_generic




SUBROUTINE da_res_synop_from_generic( res_generic, res_specific )

!------------------------------------------------------------------------------
! PURPOSE:  Eliminate redundant code for many obs types.  
!
! METHOD:   Generic type res_generic is translated to specific type 
!           res_specific.  Pointer manipulation is used for vector data, no 
!           vector data is copied.  Scalar data is copied.
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (residual_generic_type), INTENT(IN   ) :: res_generic  ! generic type
   TYPE (residual_synop_type),   INTENT(INOUT) :: res_specific ! specific type

   ! only scalar data for this type
   ! copy scalar data
   res_specific%u = res_generic%values(0)%ptr(1)
   res_specific%v = res_generic%values(0)%ptr(2)
   res_specific%t = res_generic%values(0)%ptr(3)
   res_specific%p = res_generic%values(0)%ptr(4)
   res_specific%q = res_generic%values(0)%ptr(5)

END SUBROUTINE da_res_synop_from_generic




SUBROUTINE da_y_facade_create( slice, num_obs, num_obs_glo, template )

!------------------------------------------------------------------------------
! PURPOSE:  Create a y_facade_type containing specified number of 
!           residual_generic_type objects.  
!
! METHOD:   Allocate memory and call residual_generic_create.  
!           Call y_facade_free() to deallocate memory allocated here.  
!           If template is not present, residual_generic_type objects will be 
!           left uninitialized.  If template is present, then 
!           residual_generic_type objects will be allocated but no values 
!           will be copied into them.  
!
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (y_facade_type),          INTENT(INOUT) :: slice ! selected residual obs
   INTEGER,                       INTENT(IN   ) :: num_obs
   INTEGER,                       INTENT(IN   ) :: num_obs_glo
   TYPE (residual_template_type), OPTIONAL, INTENT(IN   ) :: template
   ! Local declarations
   INTEGER :: n

   slice%num_obs     = num_obs
   slice%num_obs_glo = num_obs_glo
   ALLOCATE( slice%obs(slice%num_obs) )
   DO n=1, slice%num_obs
     CALL da_res_generic_create( slice%obs(n), template=template )
   ENDDO

END SUBROUTINE da_y_facade_create




SUBROUTINE da_res_generic_create( res_generic, template )
!------------------------------------------------------------------------------
! PURPOSE:  Create a residual_generic_type object.  This must be called 
!           before any other operation is performed on a 
!           residual_generic_type object.  Do not pass an already-allocated 
!           object into this routine as res_generic or you will cause a 
!           memory leak!  
!
! METHOD:   If template is not present, create an empty object.  
!           If template is present, create an uninitialized object with 
!             space allocated to match template.  Caller is responsible 
!             for deallocation via call to residual_generic_free().  Note 
!             that this is *NOT* a copy-constructor because values are not 
!             copied.  Also, proc_domain and obs_global_index fields are 
!             not copied from the template.  Finally, memory is not allocated 
!             for vector or scalar data, these pointers 
!             (res_generic%values(:)%ptr) are nullified.  
!
!  HISTORY:
!           09/23/2005 - Creation.                          Tom Henderson
!
! PARENT_MODULE: da_par_util
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (residual_generic_type), INTENT(INOUT) :: res_generic  ! generic type
   TYPE (residual_template_type), OPTIONAL, INTENT(IN   ) :: template
   ! Local declarations
   INTEGER :: i

   NULLIFY( res_generic%values )
   IF ( PRESENT( template ) ) THEN
     ALLOCATE( res_generic%values(template%lbnd:template%ubnd) )
     DO i=template%lbnd, template%ubnd
       NULLIFY( res_generic%values(i)%ptr )
     ENDDO
   ENDIF
END SUBROUTINE da_res_generic_create




SUBROUTINE da_res_generic_alloc_and_set( res_generic, val_index, values )
! TODO:  replace this with a full-blown copy-constructor!  
!------------------------------------------------------------------------------
! PURPOSE:  Allocates and initializes one of the values (either scalar or 
!           vector) in an already-created residual_generic_type object.  
!
! METHOD:   Allocate pointer and copy data from values.  Note that a call
!           to residual_generic_free() will deallocate scalar data but leave
!           vector data alone.  It is still the callers responsibility to
!           deallocate pointers to vector data.  In this particular case, 
!           any vector data allocated here is later passed by reference to 
!           a global specific object via call to residual_*_from_generic() 
!           (called from y_type_insert_sound_global()) and later explictly 
!           deallocated via call to free_global_*().  
!
!  HISTORY:
!           09/23/2005 - Creation.                          Tom Henderson
!
! PARENT_MODULE: da_par_util
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (residual_generic_type), INTENT(INOUT) :: res_generic  ! generic type
   INTEGER,                      INTENT(IN   ) :: val_index    ! which value
   REAL,                         INTENT(IN   ) :: values(:)    ! values

   IF ( ( val_index < LBOUND(res_generic%values,1) ) .OR. &
        ( val_index > UBOUND(res_generic%values,1) ) ) THEN
     CALL wrf_error_fatal( 'residual_generic_alloc_and_set:  bad val_index' )
   ENDIF
   ALLOCATE( res_generic%values(val_index)%ptr(SIZE(values)) )
   res_generic%values(val_index)%ptr(:) = values(:)
END SUBROUTINE da_res_generic_alloc_and_set




LOGICAL FUNCTION da_res_generic_has_vector( res_generic )
!------------------------------------------------------------------------------
! PURPOSE:  Returns .TRUE. iff res_generic stores vector values.  
!
! METHOD:   Trivial.  
!
!  HISTORY:
!           09/23/2005 - Creation.                          Tom Henderson
!
! PARENT_MODULE: da_par_util
!------------------------------------------------------------------------------
   IMPLICIT NONE
   TYPE (residual_generic_type), INTENT(IN) :: res_generic  ! generic type
   da_res_generic_has_vector = ( UBOUND(res_generic%values,1) > 0 )
END FUNCTION da_res_generic_has_vector



LOGICAL FUNCTION da_res_generic_has_scalar( res_generic )
!------------------------------------------------------------------------------
! PURPOSE:  Returns .TRUE. iff res_generic stores scalar values. 
!------------------------------------------------------------------------------
   IMPLICIT NONE
   TYPE (residual_generic_type), INTENT(IN) :: res_generic  ! generic type
   da_res_generic_has_scalar = ( LBOUND(res_generic%values,1) == 0 )
END FUNCTION da_res_generic_has_scalar



SUBROUTINE da_res_generic_free( res_generic )
!------------------------------------------------------------------------------
! PURPOSE:  Free memory for residual_generic_type object.  
!
! METHOD:   POINTERs to vector values are assumed to point to memory allocated 
!           by others and are not deallocated.  
!           This will fail if called for a residual_generic_type object that 
!           has never been created.  
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (residual_generic_type), INTENT(INOUT) :: res_generic  ! generic type
   ! Local declarations
   LOGICAL :: oktofreevalues

   oktofreevalues = .FALSE.
   IF ( da_res_generic_has_scalar( res_generic) ) THEN
     ! free scalar memory
     DEALLOCATE( res_generic%values(0)%ptr )
     oktofreevalues = .TRUE.
   ENDIF
   IF ( da_res_generic_has_vector( res_generic) ) THEN
     oktofreevalues = .TRUE.
   ENDIF
   IF ( oktofreevalues ) THEN
     DEALLOCATE( res_generic%values )
   ENDIF
END SUBROUTINE da_res_generic_free



SUBROUTINE da_y_facade_free( slice )
!------------------------------------------------------------------------------
! PURPOSE:  Free memory for y_facade_type object.  
!
! METHOD:   Brute force.  May want to make this smarter... 
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (y_facade_type), INTENT(INOUT) :: slice
   ! Local declarations
   INTEGER :: n
   IF ( ASSOCIATED( slice%obs ) ) THEN
     DO n=1, SIZE(slice%obs)
       CALL da_res_generic_free( slice%obs(n) )
     ENDDO
     DEALLOCATE( slice%obs )
   ENDIF
END SUBROUTINE da_y_facade_free


