subroutine da_uvprho_to_w_adj(xb, xa, xp)

   !---------------------------------------------------------------------------
   ! Purpose: Calculates vertical velocity increments from Richardson's Eq.
   !
   ! Method: Richardson's Eq., which
   !         combines continuity Eq., thermodynamic Eq. and hrdrostatic Eq.
   !---------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)       :: xb           ! First guess structure.
   type (x_type), intent(inout)     :: xa           ! Increment structure.
   type (xpose_type), intent(inout) :: xp           ! Domain decomposition vars.

   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.

   integer                       :: I,J,K

   real, dimension(ims:ime,jms:jme,kms:kme) :: URHO, VRHO
   real, dimension(ims:ime,jms:jme,kms:kme) :: DIV, WZ
   real                                     :: TERM3

   !Initialize to zero for some variables because of the adjoint requirements.

   WZ(:,:,:)=0.
   URHO(:,:,:)=0.0
   VRHO(:,:,:)=0.0
   DIV(:,:,:)=0.0
   TERM3=0.0

   ! Integration to calculate the vertical velocity

   do J=jts,jte
      do I=its,ite
         do K=kts,kte
            xa%w(I,J,K+1)=xa%w(I,J,K+1)+xa%w(I,J,K)
            WZ(I,J,K)=xa%w(I,J,K)*(xb%hf(I,J,K)-xb%hf(I,J,K+1))
            xa%w(I,J,K)=0.0
         end do
         xa%w(I,J,kte+1)=0.0
      end do
   end do

   call da_w_adjustment_adj(xb,WZ)

   ! Divide by constant

   do K=kts,kte
      do J=jts,jte
         do I=its,ite
            WZ(I,J,K)=WZ(I,J,K)/(GAMMA*xb%p(I,J,K))
         end do
      end do
   end do

   ! Term 4: Derivative of basic vertical velocity with respect to z.

   do J=jts,jte
      do I=its,ite
         do K=kte,kts,-1
            xa%p(I,J,K)=xa%p(I,J,K)-WZ(I,J,K)*GAMMA*  &
                     (xb%w(I,J,K+1)-xb%w(I,J,K))/  &
                     (xb%hf(I,J,K+1)-xb%hf(I,J,K))
         end do
      end do
   end do

   ! Term 3.2: Vertical integration of the basic mass divergence

   do J=jts,jte
      do I=its,ite
         do K=kts,kte-1
            TERM3=TERM3+WZ(I,J,K)
            DIV(I,J,K+1)=DIV(I,J,K+1)+  &
                      TERM3*GRAVITY*0.5*(xb%h(I,J,K+1)-xb%h(I,J,K))
            DIV(I,J,K)  =DIV(I,J,K)+  &
                      TERM3*GRAVITY*0.5*(xb%h(I,J,K+1)-xb%h(I,J,K))
         end do
        TERM3=0.0
      end do
   end do

   call da_uv_to_divergence_adj(xp, xb, URHO,VRHO, DIV)

   ! Computation to check for edge of domain:
   if (Testing_WRFVAR) then
      is = its-1; ie = ite+1; js = jts-1; je = jte+1
      if (its == ids) is = ids; if (ite == ide) ie = ide
      if (jts == jds) js = jds; if (jte == jde) je = jde
   else
      is = its
      ie = ite
      js = jts
      je = jte
   end if

   do K=kts,kte
      do J=js,je
         do I=is,ie
            xa%rho(I,J,K)=xa%rho(I,J,K)+VRHO(I,J,K)*xb%v(I,J,K)
            xa%rho(I,J,K)=xa%rho(I,J,K)+URHO(I,J,K)*xb%u(I,J,K)
         end do
      end do
   end do

   URHO(:,:,:)=0.0
   VRHO(:,:,:)=0.0

   ! Term 3.1: Vertical integration of the perturbed mass divergence

   do J=jts,jte
      do I=its,ite
         do K=kts,kte-1
            TERM3=TERM3+WZ(I,J,K)
            DIV(I,J,K+1)=DIV(I,J,K+1)+  &
                      TERM3*GRAVITY*0.5*(xb%h(I,J,K+1)-xb%h(I,J,K))
            DIV(I,J,K)  =DIV(I,J,K)+  &
                      TERM3*GRAVITY*0.5*(xb%h(I,J,K+1)-xb%h(I,J,K))
         end do
         TERM3=0.0
      end do
   end do

   call da_uv_to_divergence_adj(xp, xb, URHO,VRHO, DIV)

   do K=kts,kte
      do J=js,je
         do I=is,ie
            xa%v(I,J,K)=xa%v(I,J,K)+VRHO(I,J,K)*xb%rho(I,J,K)
            xa%u(I,J,K)=xa%u(I,J,K)+URHO(I,J,K)*xb%rho(I,J,K)
         end do
      end do
   end do

   URHO(:,:,:)=0.0
   VRHO(:,:,:)=0.0

   ! Term 2.2: Divergence term from basic wind

   call da_uv_to_divergence(xb, xb%u,xb%v, DIV)

   do K=kts,kte
      do J=jts,jte
         do I=its,ite
           xa%p(I,J,K)=xa%p(I,J,K)-WZ(I,J,K)*GAMMA*DIV(I,J,K)
         end do
      end do
   end do

   ! Term 2.1: Divergence term from perturbed wind

   do K=kts,kte   
      do J=jts,jte
         do I=its,ite
            DIV(I,J,K)=-WZ(I,J,K)*GAMMA*xb%p(I,J,K)  ! DIV redefined
         end do
      end do
   end do

   call da_uv_to_divergence_adj(xp, xb, xa%u,xa%v, DIV)

   ! Computation to check for edge of domain:
   is = its
   ie = ite
   js = jts
   je = jte
   if (its == ids) is = ids+1
   if (ite == ide) ie = ide-1
   if (jts == jds) js = jds+1
   if (jte == jde) je = jde-1

   ! Term 1.2: Basic pressure advection along the perturbed wind

   if (jte == jde) then
      j = jte
      do K=kts,kte
         do I=its, ite
            WZ(I,J-1,K)=WZ(I,J-1,K)+WZ(I,J,K)
         end do
      end do
   end if

   if (jts == jds) then
      j = jts
      do K=kts,kte
         do I=its, ite
            WZ(I,J+1,K)=WZ(I,J+1,K)+WZ(I,J,K)
         end do
      end do
   end if

   if (ite == ide) then
      i = ite
      do K=kts,kte
         do J=js,je
            WZ(I-1,J,K)=WZ(I-1,J,K)+WZ(I,J,K)
         end do
      end do
   end if

   if (its == ids) then
      i = its
      do K=kts,kte
         do J=js,je
            WZ(I+1,J,K)=WZ(I+1,J,K)+WZ(I,J,K)
         end do
      end do
   end if

   do K=kts,kte
      do J=js,je
         do I=is,ie
            xa%v(I,J,K)=xa%v(I,J,K)-WZ(I,J,K)*  &
                     (xb%p(I,J+1,K)-xb%p(I,J-1,K))*xb%coefy(I,J)
            xa%u(I,J,K)=xa%u(I,J,K)-WZ(I,J,K)*  &
                     (xb%p(I+1,J,K)-xb%p(I-1,J,K))*xb%coefx(I,J)
         end do
      end do
   end do

   !-------------------------------------------------------------------------
   ! Computation to check for edge of domain:
   ! This is only for adjoint, as we have to cross the processor boundary
   ! to get the contribution.

   is = its - 1
   ie = ite + 1
   js = jts - 1
   je = jte + 1

   xp%v1z(its:ite, jts:jte, kts:kte) = wz(its:ite, jts:jte, kts:kte)

   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id2)

   if (its == ids) then
      is = ids+1
   else
      wz(is, js:je, kts:kte) = xp%v1z(is, js:je, kts:kte)
   end if

   if (ite == ide) then
      ie = ide-1
   else
      wz(ie, js:je, kts:kte) = xp%v1z(ie, js:je, kts:kte)
   end if

   if (jts == jds) then
      js = jds+1
   else
      wz(is:ie, js, kts:kte) = xp%v1z(is:ie, js, kts:kte)
   end if

   if (jte == jde) then
      je = jde-1
   else
      wz(is:ie, je, kts:kte) = xp%v1z(is:ie, je, kts:kte)
   end if

   ! Term 1.1: Perturbed pressure advection along the basic wind

   do K=kts,kte
      do J=js,je
         do I=is,ie
            xa%p(I,J+1,K)=xa%p(I,J+1,K)-WZ(I,J,K)*xb%v(I,J,K)*xb%coefy(I,J)
            xa%p(I,J-1,K)=xa%p(I,J-1,K)+WZ(I,J,K)*xb%v(I,J,K)*xb%coefy(I,J)
            xa%p(I+1,J,K)=xa%p(I+1,J,K)-WZ(I,J,K)*xb%u(I,J,K)*xb%coefx(I,J)
            xa%p(I-1,J,K)=xa%p(I-1,J,K)+WZ(I,J,K)*xb%u(I,J,K)*xb%coefx(I,J)
         end do
      end do
   end do

   WZ(:,:,:) = 0.0

end subroutine da_uvprho_to_w_adj


