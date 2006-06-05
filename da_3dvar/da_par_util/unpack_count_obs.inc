SUBROUTINE unpack_count_obs ( num_obs, offset, value )

!------------------------------------------------------------------------------
!  PURPOSE: Unpack the 4 integer values starting at value(offset) into the
!           num_obs structure.
!
!------------------------------------------------------------------------------

   TYPE (count_obs_number_type), INTENT(OUT)    :: num_obs
   INTEGER                     , INTENT(INOUT)  :: offset
   INTEGER                     , INTENT(IN)     :: value(*)

   num_obs % num_used         = value(offset)
   num_obs % num_outside_iyjx = value(offset+1)
   num_obs % num_max_err_chk  = value(offset+2)
   num_obs % num_missing      = value(offset+3)
   offset = offset + 4

END SUBROUTINE unpack_count_obs

