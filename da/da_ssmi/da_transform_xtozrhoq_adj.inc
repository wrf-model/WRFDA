subroutine da_transform_xtozrhoq_adj(xb, xa, i, j, zh, zf, adj_zrhom)

   implicit none
   
   integer,        intent(in)      :: i, j
   type (xb_type), intent(in)      :: xb         ! first guess state.
   type (x_type) , intent(inout)   :: xa         ! increment
   real,           intent(in)      :: ADJ_zrhom  ! gridded height weighted moisture
   real,           intent(in),  &
                   dimension(mkz)  :: zh
   real,           intent(in),  &
                   dimension(mkz+1):: zf
   integer                         :: k

   if (trace_use) call da_trace_entry("da_transform_xtozrhoq_adj")

   do k = 1,mkz
      xa%rho(i,j,k) = (zf(k)-zf(k+1))*zh(k)*xb%q(i,j,k)*ADJ_zrhom   + xa%rho(i,j,k)
      xa%q(i,j,k)   = (zf(k)-zf(k+1))*zh(k)*ADJ_zrhom*xb%rho(i,j,k) + xa%q(i,j,k)
   end do

   if (trace_use) call da_trace_exit("da_transform_xtozrhoq_adj")
 
end subroutine da_transform_xtozrhoq_adj


