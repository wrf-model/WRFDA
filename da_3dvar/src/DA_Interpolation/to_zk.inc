subroutine to_zk(obs_v, mdl_v, xp, v_interp_optn, zk)

   implicit none

   integer,                        intent(in)  :: v_interp_optn
   type(xpose_type),               intent(in)  :: xp      ! Domain decomposition vars.
   real,                           intent(in)  :: obs_v
   real, dimension(xp%kms:xp%kme), intent(in)  :: mdl_v
   real,                           intent(out) :: zk

   integer                :: k

   zk = missing_r

   if(v_interp_optn == v_interp_p) then
      if (obs_v > mdl_v(xp%kts) .or. obs_v < mdl_v(xp%kte)) return

      do k = xp%kts,xp%kte-1
         if(obs_v <= mdl_v(k) .and. obs_v >= mdl_v(k+1)) then
            zk = real(k) + (mdl_v(k) - obs_v)/(mdl_v(k) - mdl_v(k+1))
            exit
         endif
      enddo
   else if(v_interp_optn == v_interp_h) then
      if (obs_v < mdl_v(xp%kts) .or. obs_v > mdl_v(xp%kte)) return

      do k = xp%kts,xp%kte-1
         if(obs_v >= mdl_v(k) .and. obs_v <= mdl_v(k+1)) then
            zk = real(k) + (mdl_v(k) - obs_v)/(mdl_v(k) - mdl_v(k+1))
            exit
         endif
      enddo
   endif

end subroutine to_zk

