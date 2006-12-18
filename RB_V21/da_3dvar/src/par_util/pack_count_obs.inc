SUBROUTINE pack_count_obs ( num_obs, offset, value )

!------------------------------------------------------------------------------
!  PURPOSE: Pack the 4 integer num_obs values into value(offset) to 
!           value(offset+3).
!
!------------------------------------------------------------------------------

   TYPE (count_obs_number_type), INTENT(IN)     :: num_obs
   INTEGER                     , INTENT(INOUT)  :: offset
   INTEGER                     , INTENT(INOUT)  :: value(*)

   value(offset)   = num_obs % num_used
   value(offset+1) = num_obs % num_outside_iyjx
   value(offset+2) = num_obs % num_max_err_chk
   value(offset+3) = num_obs % num_missing
   offset = offset + 4

END SUBROUTINE pack_count_obs

