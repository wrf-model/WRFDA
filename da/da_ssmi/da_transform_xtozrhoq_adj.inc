SUBROUTINE DA_Transform_XToZRhoQ_Adj( xb, xa, i, j, zh, zf, ADJ_zrhom)

   implicit none
   
   INTEGER,        intent(in)      :: i, j
   type (xb_type), intent(in)      :: xb         ! first guess state.
   type (x_type) , intent(inout)   :: xa         ! increment
   real,           intent(in)      :: ADJ_zrhom  ! gridded height weighted moisture
   real,           intent(in),  &
                   dimension(mkz)  :: zh
   real,           intent(in),  &
                   dimension(mkz+1):: zf
   integer                         :: k

   DO k = 1,mkz
      xa%rho(i,j,k) = (zf(k)-zf(k+1))*zh(k)*xb%q(i,j,k)*ADJ_zrhom   + xa%rho(i,j,k)
      xa%q(i,j,k)   = (zf(k)-zf(k+1))*zh(k)*ADJ_zrhom*xb%rho(i,j,k) + xa%q(i,j,k)

   END DO
 
END SUBROUTINE DA_Transform_XToZRhoQ_Adj

