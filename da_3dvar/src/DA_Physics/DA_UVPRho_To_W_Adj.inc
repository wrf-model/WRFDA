SUBROUTINE DA_UVPRho_To_W_Adj( xb, xa, xp,                 &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte)

!------------------------------------------------------------------------------
!  PURPOSE: Calculates vertical velocity increments from Richardson's Eq.
!
!  METHOD: Richardson's Eq., which
!          combines continuity Eq., thermodynamic Eq. and hrdrostatic Eq.
!
!  HISTORY: 08/21/2002 - Creation of F90 version.  Qingnong Xiao
!  HISTORY: 04/15/2004 - Modified of F90 version.  Jianfeng Gu
!           02/02/2005 - Map factor fix and global update      Syed RH Rizvi
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)       :: xb           ! First guess structure.
   TYPE (x_type), INTENT(INOUT)     :: xa           ! Increment structure.
   type (xpose_type), intent(inout) :: xp           ! Domain decomposition vars.

   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.

   integer                       :: ids,ide, jds,jde, kds,kde
   integer                       :: ims,ime, jms,jme, kms,kme
   integer                       :: its,ite, jts,jte, kts,kte

   integer                       :: I,J,K

   real, dimension(ims:ime,jms:jme,kms:kme) :: URHO, VRHO
   real, dimension(ims:ime,jms:jme,kms:kme) :: DIV, WZ
   real                                     :: TERM3

!------------------------------------------------------------------------------

!  Initialize to zero for some variables because of the adjoint requirements.

   WZ(:,:,:)=0.
   URHO(:,:,:)=0.; VRHO(:,:,:)=0.; DIV(:,:,:)=0.; TERM3=0.

! Integration to calculate the vertical velocity

   DO J=jts,jte
   DO I=its,ite

      DO K=kts,kte
         xa%w(I,J,K+1)=xa%w(I,J,K+1)+xa%w(I,J,K)
         WZ(I,J,K)=xa%w(I,J,K)*(xb%hf(I,J,K)-xb%hf(I,J,K+1))
         xa%w(I,J,K)=0.
      ENDDO
      xa%w(I,J,kte+1)=0.

   ENDDO
   ENDDO

   CALL AADJUST(xb,WZ,                     &
                ids,ide, jds,jde, kds,kde, &
                ims,ime, jms,jme, kms,kme, &
                its,ite, jts,jte, kts,kte )

! Divided by constant

   DO K=kts,kte
   DO J=jts,jte
   DO I=its,ite
      WZ(I,J,K)=WZ(I,J,K)/(GAMMA*xb%p(I,J,K))
   ENDDO
   ENDDO
   ENDDO

! Term 4: Derivative of basic vertical velocity with respect to z.

   DO J=jts,jte
   DO I=its,ite

      DO K=kte,kts,-1
         xa%p(I,J,K)=xa%p(I,J,K)-WZ(I,J,K)*GAMMA*  &
                     (xb%w(I,J,K+1)-xb%w(I,J,K))/  &
                     (xb%hf(I,J,K+1)-xb%hf(I,J,K))
      ENDDO

   ENDDO
   ENDDO

! Term 3.2: Vertical integration of the basic mass divergence

   DO J=jts,jte
   DO I=its,ite

      DO K=kts,kte-1
         TERM3=TERM3+WZ(I,J,K)
         DIV(I,J,K+1)=DIV(I,J,K+1)+  &
                      TERM3*GRAVITY*0.5*(xb%h(I,J,K+1)-xb%h(I,J,K))
         DIV(I,J,K)  =DIV(I,J,K)+  &
                      TERM3*GRAVITY*0.5*(xb%h(I,J,K+1)-xb%h(I,J,K))
      ENDDO
      TERM3=0.

   ENDDO
   ENDDO

   CALL DA_UV_To_Divergence_Adj(xp, xb, URHO,VRHO, DIV, &
                                ids,ide, jds,jde, kds,kde,         &
                                ims,ime, jms,jme, kms,kme,         &
                                its,ite, jts,jte, kts,kte )

!  Computation to check for edge of domain:
   if(Testing_3DVAR) then
      is = its-1; ie = ite+1; js = jts-1; je = jte+1
      if ( its == ids ) is = ids; if ( ite == ide ) ie = ide
      if ( jts == jds ) js = jds; if ( jte == jde ) je = jde
   else
      is = its; ie = ite; js = jts; je = jte
   endif

   DO K=kts,kte
      DO J=js,je
      DO I=is,ie
         xa%rho(I,J,K)=xa%rho(I,J,K)+VRHO(I,J,K)*xb%v(I,J,K)
         xa%rho(I,J,K)=xa%rho(I,J,K)+URHO(I,J,K)*xb%u(I,J,K)
      ENDDO
      ENDDO
   ENDDO

   URHO(:,:,:)=0.
   VRHO(:,:,:)=0.

! Term 3.1: Vertical integration of the perturbed mass divergence

   DO J=jts,jte
   DO I=its,ite

      DO K=kts,kte-1
         TERM3=TERM3+WZ(I,J,K)
         DIV(I,J,K+1)=DIV(I,J,K+1)+  &
                      TERM3*GRAVITY*0.5*(xb%h(I,J,K+1)-xb%h(I,J,K))
         DIV(I,J,K)  =DIV(I,J,K)+  &
                      TERM3*GRAVITY*0.5*(xb%h(I,J,K+1)-xb%h(I,J,K))
      ENDDO
      TERM3=0.

   ENDDO
   ENDDO

   CALL DA_UV_To_Divergence_Adj(xp, xb, URHO,VRHO, DIV, &
                                ids,ide, jds,jde, kds,kde,         &
                                ims,ime, jms,jme, kms,kme,         &
                                its,ite, jts,jte, kts,kte )

   DO K=kts,kte
      DO J=js,je
      DO I=is,ie
         xa%v(I,J,K)=xa%v(I,J,K)+VRHO(I,J,K)*xb%rho(I,J,K)
         xa%u(I,J,K)=xa%u(I,J,K)+URHO(I,J,K)*xb%rho(I,J,K)
      ENDDO
      ENDDO
   ENDDO

   URHO(:,:,:)=0.
   VRHO(:,:,:)=0.

! Term 2.2: Divergence term from basic wind

   CALL DA_UV_To_Divergence(xb, xb%u,xb%v, DIV, &
                            ids,ide, jds,jde, kds,kde,         &
                            ims,ime, jms,jme, kms,kme,         &
                            its,ite, jts,jte, kts,kte )

   DO K=kts,kte
   DO J=jts,jte
   DO I=its,ite
      xa%p(I,J,K)=xa%p(I,J,K)-WZ(I,J,K)*GAMMA*DIV(I,J,K)
   ENDDO
   ENDDO
   ENDDO

! Term 2.1: Divergence term from perturbed wind

   DO K=kts,kte
   DO J=jts,jte
   DO I=its,ite
      DIV(I,J,K)=-WZ(I,J,K)*GAMMA*xb%p(I,J,K)  ! DIV redefined
   ENDDO
   ENDDO
   ENDDO

   CALL DA_UV_To_Divergence_adj(xp, xb, xa%u,xa%v, DIV, &
                                ids,ide, jds,jde, kds,kde,         &
                                ims,ime, jms,jme, kms,kme,         &
                                its,ite, jts,jte, kts,kte )

!  Computation to check for edge of domain:
    is = its; ie = ite; js = jts; je = jte
    if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
    if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1

! Term 1.2: Basic pressure advection along the perturbed wind

   if ( jte == jde ) then
      j = jte
      DO K=kts,kte
      DO I=its, ite
         WZ(I,J-1,K)=WZ(I,J-1,K)+WZ(I,J,K)
      ENDDO
      ENDDO
   end if

   if ( jts == jds ) then
      j = jts
      DO K=kts,kte
      DO I=its, ite
         WZ(I,J+1,K)=WZ(I,J+1,K)+WZ(I,J,K)
      ENDDO
      ENDDO
   end if

   if ( ite == ide ) then
      i = ite
      DO K=kts,kte
      DO J=js,je
         WZ(I-1,J,K)=WZ(I-1,J,K)+WZ(I,J,K)
      ENDDO
      ENDDO
   end if

   if ( its == ids ) then
      i = its
      DO K=kts,kte
      DO J=js,je
         WZ(I+1,J,K)=WZ(I+1,J,K)+WZ(I,J,K)
      ENDDO
      ENDDO
   end if

   DO K=kts,kte
      DO J=js,je
      DO I=is,ie

         xa%v(I,J,K)=xa%v(I,J,K)-WZ(I,J,K)*  &
                     (xb%p(I,J+1,K)-xb%p(I,J-1,K))*xb%coefy(I,J)
         xa%u(I,J,K)=xa%u(I,J,K)-WZ(I,J,K)*  &
                     (xb%p(I+1,J,K)-xb%p(I-1,J,K))*xb%coefx(I,J)

      ENDDO
      ENDDO
   ENDDO

!-------------------------------------------------------------------------
!--Computation to check for edge of domain:
!  This is only for adjoint, as we have to cross the processor boundary
!  to get the contribution.

      is = its - 1; ie = ite + 1; js = jts - 1; je = jte + 1

      xp%v1z(its:ite, jts:jte, kts:kte) = wz(its:ite, jts:jte, kts:kte)
      call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id2)

      if ( its == ids ) then
         is = ids+1
      else
         wz(is, js:je, kts:kte) = xp%v1z(is, js:je, kts:kte)
      endif

      if ( ite == ide ) then
         ie = ide-1
      else
         wz(ie, js:je, kts:kte) = xp%v1z(ie, js:je, kts:kte)
      endif

      if ( jts == jds ) then
         js = jds+1
      else
         wz(is:ie, js, kts:kte) = xp%v1z(is:ie, js, kts:kte)
      endif

      if ( jte == jde ) then
         je = jde-1
      else
         wz(is:ie, je, kts:kte) = xp%v1z(is:ie, je, kts:kte)
      endif
!-------------------------------------------------------------------------

! Term 1.1: Perturbed pressure advection along the basic wind

   DO K=kts,kte
      DO J=js,je
      DO I=is,ie

         xa%p(I,J+1,K)=xa%p(I,J+1,K)-WZ(I,J,K)*xb%v(I,J,K)*xb%coefy(I,J)
         xa%p(I,J-1,K)=xa%p(I,J-1,K)+WZ(I,J,K)*xb%v(I,J,K)*xb%coefy(I,J)
         xa%p(I+1,J,K)=xa%p(I+1,J,K)-WZ(I,J,K)*xb%u(I,J,K)*xb%coefx(I,J)
         xa%p(I-1,J,K)=xa%p(I-1,J,K)+WZ(I,J,K)*xb%u(I,J,K)*xb%coefx(I,J)

      ENDDO
      ENDDO
   ENDDO

    WZ(:,:,:) = 0.0

END SUBROUTINE DA_UVPRho_To_W_Adj
