SUBROUTINE DA_TRH_To_TD (RH, T, TD, xp)
!!!---------------------------------------------------------------------CCC
!!!
!!!                       FUNCTION F_TD_FROM_RH
!!!                     **************************
!!!
!!!  PURPOSE:
!!!  -------
!!!     COMPUTE DEW POINT FROM TEMPERATURE AND RELATIVE HUMIDITY
!!!
!!   METHOD:
!!   ------
!!     INVERT THE RELATION
!!
!!     RH = 100. * EXP ( L_over_Rv * (1./T - 1./TD) )
!!
!!   INPUT:
!!   -----
!!      T_K:   TEMPERATURE       in K
!!      RH:    RELAITVE HUMIDITY in %
!!
!!   OUTPUT:
!!   ------
!!      TD:    DEW POINT IN K
!!
!!   REFERENCES:
!!   -----------
!!    R. R. ROGERS AND M. K. YAU, 1989: A SHORT COURSE IN CLOUD PHYSICS,
!!                                      3ND EDITION, PERGAMON PRESS, PAGE 14-19.
!!
!!   VERIFICATION SET:
!!   -----------------
!!    T_K  = 268.15 K,  
!!    TD_K = 262.55 K
!!    RH   = 65 %, 
!!    P_PA = 80000  Pa, 
!!    QV   = 2.11E-03 kg/kg,
!!
!!  MODIFICATIONS:
!!   ------------
!!    parallel implementation. -Al Bourgeois
!! 
!!-------------------------------------------------------------------------CC

   IMPLICIT NONE

   type (xpose_type), intent(in)      :: xp    ! Domain decomposition vars.

   real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), intent(inout) :: rh ! Relative Humidity.
   real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), intent(in)    ::  t ! Temperature.
   real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), intent(out)   :: td ! Dew Point in K.

!---------------------------------------------------------------------------C

   INTEGER :: I, J, K

   REAL    :: INVDIFFTD, INVTD

   integer :: is,ie,js,je,ks,ke

!---------------------------------------------------------------------------C

   is = xp%its
   ie = xp%ite
   js = xp%jts
   je = xp%jte
   ks = xp%kts
   ke = xp%kte

   do j=js,je
   do k=ks,ke
   do i=is,ie
      if(RH(i,j,k) < 10.0) then
         RH(i,j,k) = 10.0
      else if(RH(i,j,k) > 105.0) then
         RH(i,j,k) = 105.0
      end if

      INVDIFFTD = LOG (RH(i,j,k)/100.) / L_over_Rv

      INVTD = 1/T(i,j,k)  - INVDIFFTD

      TD(i,j,k)  = 1. / INVTD

      if(TD(i,j,k) > T(i,j,k)) &
         TD(i,j,k) = T(i,j,k)
   end do
   end do
   end do

END SUBROUTINE DA_TRH_To_TD

