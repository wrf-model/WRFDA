SUBROUTINE DA_Transform_XToZRhoQ_Lin( xb, xa, i, j, zh, zf, TGL_zrhom)

   implicit none
   
   INTEGER,        intent(in)      :: i, j
   type (xb_type), intent(in)      :: xb         ! first guess state.
   type (x_type) , intent(in)      :: xa         ! increment
   real,           intent(out)     :: TGL_zrhom  ! gridded height weighted moisture
   real,           intent(in),  &
                   dimension(mkz)  :: zh
   real,           intent(in),  &
                   dimension(mkz+1):: zf
   integer                         :: k

   TGL_zrhom = 0.

   DO k = 1,mkz
      TGL_zrhom = (zf(k)-zf(k+1))*zh(k)*(xb%q(i,j,k)*xa%rho(i,j,k) + &
                                         xa%q(i,j,k)*xb%rho(i,j,k)   &
                                        ) + TGL_zrhom
   END DO
 
END SUBROUTINE DA_Transform_XToZRhoQ_Lin

