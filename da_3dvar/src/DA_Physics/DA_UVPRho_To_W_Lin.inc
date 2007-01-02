SUBROUTINE DA_UVPRho_To_W_Lin( xb, xa, xp,                 &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Calculates vertical velocity increments from Richardson's Eq.
!
!  METHOD: Richardson's Eq., which
!          combines continuity Eq., thermodynamic Eq. and hrdrostatic Eq.
!
!  HISTORY: 07/21/2002 - Creation of F90 version. Qingnong Xiao
!  HISTORY: 04/15/2004 - Modified of F90 version. Jianfeng Gu
!           02/02/2005 - Map factor fix and global update      Syed RH Rizvi 
!           06/02/2005 - fixed the bug and update the DIV.  Yong-Run Guo
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)       :: xb        ! First guess structure.
   TYPE (x_type), INTENT(INOUT)     :: xa        ! Increment structure.
   TYPE (xpose_type), INTENT(INOUT) :: xp        ! Domain decomposition vars.

   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.
   integer                       :: ks, ke       ! 3rd dim. end points.

   integer                       :: ids,ide, jds,jde, kds,kde
   integer                       :: ims,ime, jms,jme, kms,kme
   integer                       :: its,ite, jts,jte, kts,kte

   integer                       :: I,J,K

   real, dimension(ims:ime,jms:jme,kms:kme) :: URHO, VRHO
   real, dimension(ims:ime,jms:jme,kms:kme) :: DIV, WZ
   real                                     :: TERM3

   
!------------------------------------------------------------------------------

!  Computation to check for edge of domain:
   is = its; ie = ite; js = jts; je = jte
   if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
   if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1

    WZ(:,:,:) = 0.0
! Term 1.1: perturbed pressure advection along the basic wind
   DO K=kts,kte
      DO J=js,je
      DO I=is,ie
         WZ(I,J,K)=WZ(I,J,K)-xb%u(I,J,K)*(xa%p(I+1,J,K)-xa%p(I-1,J,K))*xb%coefx(I,J)
         WZ(I,J,K)=WZ(I,J,K)-xb%v(I,J,K)*(xa%p(I,J+1,K)-xa%p(I,J-1,K))*xb%coefy(I,J)
      ENDDO
      ENDDO
   ENDDO
! Term 1.2: Basic pressure advection along the perturbed wind

   DO K=kts,kte
      DO J=js,je
      DO I=is,ie
         WZ(I,J,K)=WZ(I,J,K)-xa%u(I,J,K)*(xb%p(I+1,J,K)-xb%p(I-1,J,K))*xb%coefx(I,J)
         WZ(I,J,K)=WZ(I,J,K)-xa%v(I,J,K)*(xb%p(I,J+1,K)-xb%p(I,J-1,K))*xb%coefy(I,J)
      ENDDO
      ENDDO
   ENDDO

!  Dealing the laterial boundary because of the advection.
!  boundary too simple? (It is the same as fill in interpf, fill can be used)

   if ( its == ids ) then
      i = its
      DO K=kts,kte
      DO J=js,je
         WZ(I,J,K)=WZ(I+1,J,K)
      ENDDO
      ENDDO
   end if

   if ( ite == ide ) then
      i = ite
      DO K=kts,kte
      DO J=js,je
         WZ(I,J,K)=WZ(I-1,J,K)
      ENDDO
      ENDDO
   end if

   if ( jts == jds ) then
      j = jts
      DO K=kts,kte
      DO I=its, ite
         WZ(I,J,K)=WZ(I,J+1,K)       
      ENDDO
      ENDDO
   end if

   if ( jte == jde ) then
      j = jte
      DO K=kts,kte
      DO I=its, ite
         WZ(I,J,K)=WZ(I,J-1,K)
      ENDDO
      ENDDO
   end if

! Term 2.1: Divergence term from perturbed wind

   CALL DA_UV_To_Divergence(xb, xa%u, xa%v, DIV, &
                            ids,ide, jds,jde, kds,kde,         &
                            ims,ime, jms,jme, kms,kme,         &
                            its,ite, jts,jte, kts,kte )

   DO K=kts,kte
   DO J=jts,jte
   DO I=its,ite
      WZ(I,J,K)=WZ(I,J,K)-GAMMA*xb%p(I,J,K)*DIV(I,J,K)
   ENDDO
   ENDDO
   ENDDO

! Term 2.2: Divergence term from basic wind

   CALL DA_UV_To_Divergence(xb, xb%u, xb%v, DIV, &
                            ids,ide, jds,jde, kds,kde,         &
                            ims,ime, jms,jme, kms,kme,         &
                            its,ite, jts,jte, kts,kte )

   DO K=kts,kte
   DO J=jts,jte
   DO I=its,ite
      WZ(I,J,K)=WZ(I,J,K)-GAMMA*xa%p(I,J,K)*DIV(I,J,K)
   ENDDO
   ENDDO
   ENDDO

!  Computation to check for edge of domain:
   is = its-1; ie = ite+1; js = jts-1; je = jte+1
   if ( its == ids ) is = ids; if ( ite == ide ) ie = ide
   if ( jts == jds ) js = jds; if ( jte == jde ) je = jde

! Term 3.1: Vertical integration of the perturbed mass divergence

   DO K=kts,kte
      DO J=js,je
      DO I=is,ie
         URHO(I,J,K)=xb%rho(I,J,K)*xa%u(I,J,K)
         VRHO(I,J,K)=xb%rho(I,J,K)*xa%v(I,J,K)
      ENDDO
      ENDDO
   ENDDO

   CALL DA_UV_To_Divergence(xb, URHO, VRHO, DIV, &
                            ids,ide, jds,jde, kds,kde,         &
                            ims,ime, jms,jme, kms,kme,         &
                            its,ite, jts,jte, kts,kte )

   DO J=jts,jte
   DO I=its,ite
      TERM3=0.

      DO K=kte-1,kts,-1
         TERM3=TERM3+GRAVITY*(DIV(I,J,K+1)+DIV(I,J,K))*0.5  &
                *(xb%h(I,J,K+1)-xb%h(I,J,K))
         WZ(I,J,K)=WZ(I,J,K)+TERM3
      ENDDO

   ENDDO
   ENDDO

! Term 3.2: Vertical integration of the basic mass divergence

   DO K=kts,kte
      DO J=js,je
      DO I=is,ie
         URHO(I,J,K)=xa%rho(I,J,K)*xb%u(I,J,K)
         VRHO(I,J,K)=xa%rho(I,J,K)*xb%v(I,J,K)
      ENDDO
      ENDDO
   ENDDO

   CALL DA_UV_To_Divergence(xb, URHO, VRHO, DIV, &
                            ids,ide, jds,jde, kds,kde,         &
                            ims,ime, jms,jme, kms,kme,         &
                            its,ite, jts,jte, kts,kte )

   DO J=jts,jte
   DO I=its,ite
      TERM3=0.

      DO K=kte-1,kts,-1
         TERM3=TERM3+GRAVITY*(DIV(I,J,K+1)+DIV(I,J,K))*0.5  &
                *(xb%h(I,J,K+1)-xb%h(I,J,K))
         WZ(I,J,K)=WZ(I,J,K)+TERM3
      ENDDO

   ENDDO
   ENDDO

! Term 4: Derivative of basic vertical velocity with respect to z.

   DO J=jts,jte
   DO I=its,ite

         DO K=kts,kte
            WZ(I,J,K)=WZ(I,J,K)-GAMMA*xa%p(I,J,K)*(xb%w(I,J,K+1)-xb%w(I,J,K))/  &
                      (xb%hf(I,J,K+1)-xb%hf(I,J,K))
         ENDDO

   ENDDO
   ENDDO

! Divided by constant

   DO K=kts,kte
   DO J=jts,jte
   DO I=its,ite
      WZ(I,J,K)=WZ(I,J,K)/(GAMMA*xb%p(I,J,K))
   ENDDO
   ENDDO
   ENDDO

! Integration to calculate the vertical velocity 

   CALL LADJUST(xb,xa%w,WZ,                  &
                ids,ide, jds,jde, kds,kde,   &
                ims,ime, jms,jme, kms,kme,   &
                its,ite, jts,jte, kts,kte )

   DO J=jts,jte
   DO I=its,ite

      xa%w(I,J,kte+1)=0.
      DO K=kte,kts,-1
         xa%w(I,J,K)=xa%w(I,J,K+1)+   &
                     WZ(I,J,K)*(xb%hf(I,J,K)-xb%hf(I,J,K+1))
      ENDDO

   ENDDO
   ENDDO

!------------------------------------------------------

END SUBROUTINE DA_UVPRho_To_W_Lin
