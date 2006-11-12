module da_module_couple_uv

contains

!------------------------------------------------------------------------

subroutine da_couple_uv ( u,  v,  mu, mub,  msfu, msfv, &
                       ids, ide, jds, jde, kds, kde )

   implicit none

!--Input data.

   integer, intent(in) :: ids, ide, jds, jde, kds, kde

   real, dimension(ids:ide+1, jds:jde  , kds:kde), intent(inout) :: u
   real, dimension(ids:ide  , jds:jde+1, kds:kde), intent(inout) :: v

   real, dimension(ids:ide+1, jds:jde  ),          intent(in   ) :: msfu
   real, dimension(ids:ide  , jds:jde+1),          intent(in   ) :: msfv

   real, dimension(ids:ide  , jds:jde  ),          intent(in   ) :: mu, mub

   real, allocatable, dimension(:, :) :: muu, muv

   allocate(muu(ids:ide+1, jds:jde  ))
   allocate(muv(ids:ide  , jds:jde+1))

!--couple variables u, v

   call da_calc_mu_uv ( mu, mub, muu, muv, &
                     ids, ide, jds, jde )

   call da_couple ( muu, u, msfu, &
                 ids, ide+1, jds, jde, kds, kde )

   call da_couple ( muv, v, msfv, &
                 ids, ide, jds, jde+1, kds, kde )

   deallocate(muu)
   deallocate(muv)

end subroutine da_couple_uv

!-------------------------------------------------------------------------------

subroutine da_calc_mu_uv ( mu, mub, muu, muv, &
                        ids, ide, jds, jde )

   implicit none

!--Input data

   integer, intent(in) :: ids, ide, jds, jde

   real, dimension(ids:ide, jds:jde),     intent(in   ) :: mu,  mub

   real, dimension(ids:ide+1, jds:jde  ), intent(  out) :: muu
   real, dimension(ids:ide  , jds:jde+1), intent(  out) :: muv

   real, dimension(ids-1:ide+1, jds-1:jde+1) :: mut

   integer :: i, j

   DO j=jds,jde
      DO i=ids,ide
         mut(i,j) = mu(i,j)+mub(i,j)
      endDO

      mut(ids-1,j) = mut(ids,j)
      mut(ide+1,j) = mut(ide,j)
   endDO

   DO i=ids-1,ide+1
      mut(i,jds-1)=mut(i,jds)
      mut(i,jde+1)=mut(i,jde)
   endDO

   DO j=jds,jde
   DO i=ids,ide+1
      muu(i,j) = 0.5*(mut(i,j)+mut(i-1,j))
   endDO
   endDO

   DO j=jds,jde+1
   DO i=ids,ide
      muv(i,j) = 0.5*(mut(i,j)+mut(i,j-1))
   endDO
   endDO

end subroutine da_calc_mu_uv

!-------------------------------------------------------------------------------

subroutine da_couple ( mut, field, msf, &
                    ids, ide, jds, jde, kds, kde )

   implicit none

!--Input data

   integer, intent(in) :: ids, ide, jds, jde, kds, kde

   real, dimension(ids:ide, jds:jde),          intent(in   ) :: mut, msf
  
   real, dimension(ids:ide, jds:jde, kds:kde), intent(inout) :: field
  
!--Local data
  
   integer :: i, j, k
  
   DO j=jds,jde
   DO k=kds,kde
   DO i=ids,ide
      field(i,j,k)=field(i,j,k)*mut(i,j)/msf(i,j)
   endDO
   endDO
   endDO

end subroutine da_couple

end module da_module_couple_uv

