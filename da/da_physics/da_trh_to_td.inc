subroutine da_trh_to_td (RH, T, TD, xp)

   !---------------------------------------------------------------------
   !
   !                       function F_TD_FROM_RH
   !                     **************************
   !
   !  PURPOSE:
   !  -------
   !     COMPUTE DEW POinT FROM TEMPERATURE AND RELATIVE HUMIDITY
   !
   !   METHOD:
   !   ------
   !     inVERT THE RELATION
   !
   !     RH = 100. * EXP ( L_over_Rv * (1./T - 1./TD) )
   !
   !   inPUT:
   !   -----
   !      T_K:   TEMPERATURE       in K
   !      RH:    RELAITVE HUMIDITY in %
   !
   !   outPUT:
   !   ------
   !      TD:    DEW POinT in K
   !
   !   REFERENCES:
   !   -----------
   !    R. R. ROGERS AND M. K. YAU, 1989: A SHORT COURSE in CLOUD PHYSICS,
   !                                   3ND EDITION, PERGAMON PRESS, PAGE 14-19.
   !
   !   VERifICATION SET:
   !   -----------------
   !    T_K  = 268.15 K,  
   !    TD_K = 262.55 K
   !    RH   = 65 %, 
   !    P_PA = 80000  Pa, 
   !    QV   = 2.11E-03 kg/kg,
   !
   !  MODifICATIONS:
   !   ------------
   !    parallel implementation. -Al Bourgeois
   ! 
   !-------------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in)      :: xp    ! Domain decomposition vars.

   real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), &
      intent(inout) :: rh ! Relative Humidity.
   real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), &
      intent(in)    ::  t ! Temperature.
   real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), &
      intent(out)   :: td ! Dew Point in K.

   integer :: I, J, K

   real    :: inVDifFTD, inVTD

   integer :: is,ie,js,je,ks,ke

   if (trace_use) call da_trace_entry("da_trh_to_td")

   is = xp%its
   ie = xp%ite
   js = xp%jts
   je = xp%jte
   ks = xp%kts
   ke = xp%kte

   do j=js,je
      do k=ks,ke
         do i=is,ie
            if (RH(i,j,k) < 10.0) then
               RH(i,j,k) = 10.0
            else if(RH(i,j,k) > 105.0) then
               RH(i,j,k) = 105.0
            end if

            inVDifFTD = LOG (RH(i,j,k)/100.) / L_over_Rv

            inVTD = 1/T(i,j,k)  - inVDifFTD

            TD(i,j,k)  = 1. / inVTD

            if (TD(i,j,k) > T(i,j,k)) &
               TD(i,j,k) = T(i,j,k)
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_trh_to_td")

end subroutine da_trh_to_td


