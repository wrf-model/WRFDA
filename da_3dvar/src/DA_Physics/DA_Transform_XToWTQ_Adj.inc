Subroutine DA_Transform_XToWTQ_adj ( xp, xb, jo_grad_x )

   IMPLICIT NONE

   TYPE (xpose_type), intent(in)    :: xp          ! Domain decomposition vars.
   TYPE (xb_type),    INTENT(IN)    :: xb          ! first guess state.
   TYPE (x_type),     INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

! ---------------------------------------------------------------------------

   integer :: i, j, kz, is, js, ie, je
! ---------------------------------------------------------------------------

   kz = xp%kts

   is = xb%its
   js = xb%jts

   ie = xb%ite
   je = xb%jte

   if ( Testing_3DVAR ) then
      is = xb%its-1
      js = xb%jts-1

      ie = xb%ite+1
      je = xb%jte+1

      if ( is < xb%ids ) is = xb%ids
      if ( js < xb%jds ) js = xb%jds

      if ( ie > xb%ide ) ie = xb%ide
      if ( je > xb%jde ) je = xb%jde
   endif

! ---------------------------------------------------------------------------

!--Adjoint from Gridded 10-m wind and 2-m moisture and temperature
!  to the model adjoint variables

   do j=js, je
   do i=is, ie
      jo_grad_x%tgrn(i,j)=0.0
      call sfc_wtq_Adj(xb%psfc(i,j), xb%tgrn(i,j), &
                       xb%p(i,j,kz), xb%t(i,j,kz), xb%q(i,j,kz), &
                       xb%u(i,j,kz), xb%v(i,j,kz), &
                       xb%regime(i,j),  &
                       jo_grad_x%psfc(i,j), jo_grad_x%tgrn(i,j), &
                       jo_grad_x%p(i,j,kz), jo_grad_x%t(i,j,kz), jo_grad_x%q(i,j,kz), &
                       jo_grad_x%u(i,j,kz), jo_grad_x%v(i,j,kz), &
                       xb%h(i,j,kz), xb%rough(i,j), xb%xland(i,j), &
                       jo_grad_x%u10(i,j),jo_grad_x%v10(i,j), &
                       jo_grad_x%t2 (i,j),jo_grad_x%q2 (i,j))
    
      jo_grad_x%tgrn(i,j)=0.0
   enddo
    
   enddo

END Subroutine DA_Transform_XToWTQ_adj

