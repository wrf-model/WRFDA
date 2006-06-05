SUBROUTINE DA_Transform_XToZRhoQ( xb, i, j, zh, zf, zrhom)

   implicit none
   
   INTEGER,        intent(in)      :: i, j
   type (xb_type), intent(in)      :: xb         ! first guess state.
   real,           intent(out)     :: zrhom      ! gridded height weighted moisture
   real,           intent(in),  &
                   dimension(mkz) :: zh
   real,           intent(in),  &
           dimension(mkz+1)       :: zf
   integer                         :: k
   
   zrhom = 0.

   DO k = 1,mkz
      zrhom = (zf(k)-zf(k+1))*zh(k)*(xb%q(i,j,k)*xb%rho(i,j,k))+zrhom
   END DO
 
END SUBROUTINE DA_Transform_XToZRhoQ

