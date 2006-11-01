subroutine da_to_zk_new(obs_v, mdl_v, xp, v_interp_optn, zk,num,nlevels)

   implicit none

   integer,          intent(in)  :: v_interp_optn
   type(xpose_type), intent(in)  :: xp!Domain decomposition vars.
   real,             intent(in)  :: obs_v(num)
   real,             intent(in)  :: mdl_v(num,xp%kms:xp%kme)
   real,             intent(out) :: zk(:,:)
   integer,          intent(in)  :: num
   integer,          intent(in)  :: nlevels

   integer                :: k,n,kk
   real    :: r_kts, r_ktsplus, r_kteminus

   if (trace_use) call da_trace_entry("da_to_zk_new")

   zk(:,:) = missing_r

   r_kts      = real(xp%kts)
   r_ktsplus  = real(xp%kts+1)
   r_kteminus = real(xp%kte-1)

   if (v_interp_optn == v_interp_p) then
      if (anal_type_verify) then
         do n=1,num
            do k=1,nlevels
               if (obs_v(k) > mdl_v(n,(xp%kts))) then
                  ! below the lowest level:
                  zk(k,n) = r_ktsplus - &
                     (obs_v(k)-mdl_v(n,(xp%kts+1)))/(mdl_v(n,(xp%kts))-mdl_v(n,(xp%kts+1)))
               else if (obs_v(k) < mdl_v(n,(xp%kts))) then
                  ! above the highest level:
                  zk(k,n) = r_kteminus + &
                     (obs_v(k)-mdl_v(n,(xp%kte-1)))/(mdl_v(n,(xp%kte))-mdl_v(n,(xp%kte-1)))
               else
                  do kk = xp%kts,xp%kte-1
                     if (obs_v(k) <= mdl_v(n,kk) .and. obs_v(k) >= mdl_v(n,kk+1)) then
                        zk(k,n) = real(kk) + (mdl_v(n,kk) - obs_v(k))/(mdl_v(n,kk) - mdl_v(n,kk+1))
                        exit
                     end if
                  end do
               end if
            end do
         end do
      end if
   else if(v_interp_optn == v_interp_h) then
      if (anal_type_verify) then
         do n=1,num
            do k=1,nlevels
               if (obs_v(k) < mdl_v(n,(xp%kts))) then
                   ! below the lowest level:
                   zk(k,n) = r_ktsplus - &
                     (obs_v(k)-mdl_v(n,(xp%kts+1)))/(mdl_v(n,(xp%kts))-mdl_v(n,(xp%kts+1)))
               else if (obs_v(k) > mdl_v(n,(xp%kts))) then
                  ! above the highest level:
                  zk(k,n) = r_kteminus + &
                     (obs_v(k)-mdl_v(n,(xp%kte-1)))/(mdl_v(n,(xp%kte))-mdl_v(n,(xp%kte-1)))
               else
                  do kk = xp%kts,xp%kte-1
                     if (obs_v(k) >= mdl_v(n,kk) .and. obs_v(k) <= mdl_v(n,kk+1)) then
                        zk(k,n) = real(kk) + (mdl_v(n,kk) - obs_v(k))/(mdl_v(n,kk) - mdl_v(n,kk+1))
                        exit
                     end if
                  end do
               end if
            end do
         end do
      end if
   end if

   if (trace_use) call da_trace_exit("da_to_zk_new")

end subroutine da_to_zk_new

