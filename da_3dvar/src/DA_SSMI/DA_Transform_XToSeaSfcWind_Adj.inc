SUBROUTINE DA_Transform_XToSeaSfcWind_Adj( xa, xb )

!------------------------------------------------------------------------------!
!  Convert (U-V in m/s) components into wind speed (Speed in m/s)
!------------------------------------------------------------------------------!

   IMPLICIT NONE

   type (x_type),  intent(inout) :: xa   ! model space analysis (local).
   type (xb_type), intent(in)    :: xb   ! first guess.

   real :: const, rgh_fac, var
   integer :: i, j, k, is, js, ie, je

!------------------------------------------------------------------------------!

   const = log(10./0.0001)
   k = xb%kts

!------------------------------------------------------------------------

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

!------------------------------------------------------------------------

   do j=js, je
   do i=is, ie
      rgh_fac = const/log(xb%h(i,j,k)/0.0001) ! roughness = 0.0001

      var = rgh_fac*rgh_fac/xb%speed(i,j)

      xa%u(i,j,k)=xa%u(i,j,k)+var*xa%speed(i,j)*xb%u(i,j,k)
      xa%v(i,j,k)=xa%v(i,j,k)+var*xa%speed(i,j)*xb%v(i,j,k)
   enddo
   enddo

END SUBROUTINE DA_Transform_XToSeaSfcWind_Adj

