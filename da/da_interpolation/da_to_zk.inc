subroutine da_to_zk(obs_v, mdl_v, xp, v_interp_optn, zk)

   implicit none

   integer,                        intent(in)  :: v_interp_optn
   type(xpose_type),               intent(in)  :: xp!Domain decomposition vars.
   real,                           intent(in)  :: obs_v
   real, dimension(xp%kms:xp%kme), intent(in)  :: mdl_v
   real,                           intent(out) :: zk

   integer                :: k

   zk = missing_r

   if(v_interp_optn == v_interp_p) then
     if (obs_v > mdl_v(xp%kts) .or. obs_v < mdl_v(xp%kte)) then
        if( analysis_type(1:6) == "VERIFY" .or.  &
            analysis_type(1:6) == "verify") then
          if (obs_v > mdl_v(xp%kts)) then
            p_below = p_below + 1
            zk = real(xp%kts+1) - &
               (obs_v-mdl_v(xp%kts+1))/(mdl_v(xp%kts)-mdl_v(xp%kts+1))
          else if (obs_v < mdl_v(xp%kte)) then
            p_above = p_above + 1
            zk = real(xp%kte-1) + &
               (obs_v-mdl_v(xp%kte-1))/(mdl_v(xp%kte)-mdl_v(xp%kte-1))
          endif
        else
          return
        endif

     else
        do k = xp%kts,xp%kte-1
          if(obs_v <= mdl_v(k) .and. obs_v >= mdl_v(k+1)) then
            zk = real(k) + (mdl_v(k) - obs_v)/(mdl_v(k) - mdl_v(k+1))
            exit
          endif
        enddo
     endif

   else if(v_interp_optn == v_interp_h) then
     if (obs_v < mdl_v(xp%kts) .or. obs_v > mdl_v(xp%kte)) then
       if( analysis_type(1:6) == "VERIFY" .or.  &
           analysis_type(1:6) == "verify") then
          if (obs_v < mdl_v(xp%kts)) then
            h_below = h_below + 1
            zk = real(xp%kts+1) - &
               (obs_v-mdl_v(xp%kts+1))/(mdl_v(xp%kts)-mdl_v(xp%kts+1))
          else if (obs_v > mdl_v(xp%kte)) then
            h_above = h_above + 1
            zk = real(xp%kte-1) + &
               (obs_v-mdl_v(xp%kte-1))/(mdl_v(xp%kte)-mdl_v(xp%kte-1))
          endif
       else
         return
       endif
     else
       do k = xp%kts,xp%kte-1
         if(obs_v >= mdl_v(k) .and. obs_v <= mdl_v(k+1)) then
            zk = real(k) + (mdl_v(k) - obs_v)/(mdl_v(k) - mdl_v(k+1))
            exit
         endif
       enddo
     endif

   endif

end subroutine da_to_zk

