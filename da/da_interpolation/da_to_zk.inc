subroutine da_to_zk(obs_v, mdl_v, xp, v_interp_optn, zk)

   implicit none

   integer,                        intent(in)  :: v_interp_optn
   type(xpose_type),               intent(in)  :: xp!Domain decomposition vars.
   real,                           intent(in)  :: obs_v
   real, dimension(xp%kms:xp%kme), intent(in)  :: mdl_v
   real,                           intent(out) :: zk

   integer                :: k

   if (trace_use_frequent) call da_trace_entry("da_to_zk")

   zk = missing_r

   if (v_interp_optn == v_interp_p) then
      if (obs_v > mdl_v(xp%kts) .or. obs_v < mdl_v(xp%kte)) then
         if (anal_type_verify) then
            ! Guo (02/06/2006), for VERifY, allow the extrapolation to obtain the zk:
            if (obs_v > mdl_v(xp%kts)) then
               ! below the lowest level:
               zk = real(xp%kts+1) - &
                  (obs_v-mdl_v(xp%kts+1))/(mdl_v(xp%kts)-mdl_v(xp%kts+1))
            else if (obs_v < mdl_v(xp%kte)) then
               ! above the highest level:
               zk = real(xp%kte-1) + &
                  (obs_v-mdl_v(xp%kte-1))/(mdl_v(xp%kte)-mdl_v(xp%kte-1))
            end if
         else
            if (trace_use) call da_trace_exit("da_to_zk")
            return
         end if
      else
         do k = xp%kts,xp%kte-1
            if(obs_v <= mdl_v(k) .and. obs_v >= mdl_v(k+1)) then
               zk = real(k) + (mdl_v(k) - obs_v)/(mdl_v(k) - mdl_v(k+1))
               exit
            end if
         end do
      end if
   else if(v_interp_optn == v_interp_h) then
      if (obs_v < mdl_v(xp%kts) .or. obs_v > mdl_v(xp%kte)) then
         if (anal_type_verify) then
            ! Guo (02/06/2006), for VERifY, allow the extrapolation to obtain the zk:
            if (obs_v < mdl_v(xp%kts)) then
               ! below the lowest level:
               zk = real(xp%kts+1) - &
                  (obs_v-mdl_v(xp%kts+1))/(mdl_v(xp%kts)-mdl_v(xp%kts+1))
            else if (obs_v > mdl_v(xp%kte)) then
               ! above the highest level:
               zk = real(xp%kte-1) + &
                  (obs_v-mdl_v(xp%kte-1))/(mdl_v(xp%kte)-mdl_v(xp%kte-1))
            end if
         else
            if (trace_use) call da_trace_exit("da_to_zk")
            return
         end if
      else
         do k = xp%kts,xp%kte-1
            if(obs_v >= mdl_v(k) .and. obs_v <= mdl_v(k+1)) then
               zk = real(k) + (mdl_v(k) - obs_v)/(mdl_v(k) - mdl_v(k+1))
               exit
            end if
         end do
      end if
   end if

   if (trace_use_frequent) call da_trace_exit("da_to_zk")

end subroutine da_to_zk

