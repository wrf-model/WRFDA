
SUBROUTINE AADJUST(INV_2DS,xb,WZ_a,                   &
                      ids,ide, jds,jde, kds,kde,      &
                      ims,ime, jms,jme, kms,kme,      &
                      its,ite, jts,jte, kts,kte )
!------------------------------------------------------------------------------
!  PURPOSE: Adjust vertical velocity increments
!
!  ASSUMPTIONS: 1) Model level stored top down.
!
!  HISTORY: 09/21/2002 - Creation of F90 version. Qingnong Xiao
!  HISTORY: 04/15/2004 - Modified of F90 version. Jianfeng Gu
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)    :: xb                ! First guess structure.

   integer, intent(in):: ids,ide, jds,jde, kds,kde    ! domain dims.
   integer, intent(in):: ims,ime, jms,jme, kms,kme    ! memory dims.
   integer, intent(in):: its,ite, jts,jte, kts,kte    ! tile   dims.

   real, dimension(ims:ime,jms:jme,kms:kme), intent(inout) :: WZ_a

   real, intent(in) :: INV_2DS

   integer :: I,J,K

   real, dimension(ims:ime,jms:jme,kms:kme)   :: URHO, VRHO, WZ_b

   real :: EBXL1,EBXL2
   real :: EBXL19,EBXL29

   CALL WZ_BASE(xb,WZ_b,INV_2DS,                     &
                ids,ide, jds,jde, kds,kde,           &
                ims,ime, jms,jme, kms,kme,           &
                its,ite, jts,jte, kts,kte )

   DO J=jts,jte
   DO I=its,ite

      EBXL19=0.
      EBXL29=0.

      DO K=kte,kts,-1
         EBXL19=EBXL19+WZ_b(I,J,K)*(xb%hf(I,J,K)-xb%hf(I,J,K+1))
         EBXL29=EBXL29+ABS(WZ_b(I,J,K))*(xb%hf(I,J,K)-xb%hf(I,J,K+1))
      ENDDO

      EBXL1=0.
      EBXL2=0.

      DO K=kts,kte
         EBXL1=EBXL1-WZ_a(I,J,K)*ABS(WZ_b(I,J,K))/EBXL29
         EBXL2=EBXL2-WZ_a(I,J,K)*   &
               ABS(WZ_b(I,J,K))*(-EBXL19)/EBXL29**2
         WZ_a(I,J,K)=WZ_a(I,J,K)*(1.-EBXL19/EBXL29   &
                                *SIGN(1.,WZ_b(I,J,K)))
      ENDDO

      DO K=kte,kts,-1
         WZ_a(I,J,K)=WZ_a(I,J,K)+EBXL2*(xb%hf(I,J,K)-xb%hf(I,J,K+1))   &
                       *SIGN(1.,WZ_b(I,J,K))
         WZ_a(I,J,K)=WZ_a(I,J,K)+EBXL1*(xb%hf(I,J,K)-xb%hf(I,J,K+1))
      ENDDO
   ENDDO
   ENDDO

END SUBROUTINE AADJUST



SUBROUTINE WZ_BASE(xb,WZ_b,INV_2DS,                        &
                      ids,ide, jds,jde, kds,kde,           &
                      ims,ime, jms,jme, kms,kme,           &
                      its,ite, jts,jte, kts,kte )

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)    :: xb                ! First guess structure.

   real, intent(in) ::  INV_2DS

   integer, intent(in):: ids,ide, jds,jde, kds,kde    ! domain dims.
   integer, intent(in):: ims,ime, jms,jme, kms,kme    ! memory dims.
   integer, intent(in):: its,ite, jts,jte, kts,kte    ! tile   dims.

   real, dimension(ims:ime,jms:jme,kms:kme), intent(inout) :: WZ_b

   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.
   integer                       :: ks, ke       ! 3rd dim. end points.

   integer                       :: I,J,K

   real  ::  TERM3

   real, dimension(ims:ime,jms:jme)         :: CRSDOT_X
   real, dimension(ims:ime,jms:jme,kms:kme) :: URHO, VRHO
   real, dimension(ims:ime,jms:jme,kms:kme) :: DIV

!  Computation to check for edge of domain:

   is = its; ie = ite; js = jts; je = jte
   if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
   if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1

   DO K=kts,kte
      DO J=js,je
      DO I=is,ie
         WZ_b(I,J,K)=-xb%u(I,J,K)*(xb%p(I+1,J,K)-xb%p(I-1,J,K))*INV_2DS
      ENDDO
      ENDDO

      DO J=js,je
      DO I=is,ie
         WZ_b(I,J,K)=WZ_b(I,J,K)-xb%v(I,J,K)*(xb%p(I,J+1,K)-xb%p(I,J-1,K))*INV_2DS
      ENDDO
      ENDDO
   ENDDO

   if ( its == ids ) then
      i = its
      DO K=kts,kte
      DO J=js,je
         WZ_b(I,J,K)=WZ_b(I+1,J,K)
      ENDDO
      ENDDO
   end if

   if ( ite == ide ) then
      i = ite
      DO K=kts,kte
      DO J=js,je
         WZ_b(I,J,K)=WZ_b(I-1,J,K)
      ENDDO
      ENDDO
   end if

   if ( jts == jds ) then
      j = jts
      DO K=kts,kte
      DO I=its, ite
         WZ_b(I,J,K)=WZ_b(I,J+1,K)
      ENDDO
      ENDDO
   end if

   if ( jte == jde ) then
      j = jte
      DO K=kts,kte
      DO I=its, ite
         WZ_b(I,J,K)=WZ_b(I,J-1,K)
      ENDDO
      ENDDO
   end if


   CALL DA_UV_To_Divergence(xb%ds,xb%map_factor,xb%u,xb%v,DIV, &
                            ids,ide, jds,jde, kds,kde,         &
                            ims,ime, jms,jme, kms,kme,         &
                            its,ite, jts,jte, kts,kte)

   DO K=kts,kte
   DO J=jts,jte
   DO I=its,ite
      WZ_b(I,J,K)=WZ_b(I,J,K)-GAMMA*xb%p(I,J,K)*DIV(I,J,K)
   ENDDO
   ENDDO
   ENDDO


!  Computation to check for edge of domain:
   is = its-1; ie = ite+1; js = jts-1; je = jte+1
   if ( its == ids ) is = ids; if ( ite == ide ) ie = ide
   if ( jts == jds ) js = jds; if ( jte == jde ) je = jde

   DO K=kts,kte
      DO J=js,je
      DO I=is,ie
         URHO(I,J,K)=xb%rho(I,J,K)*xb%u(I,J,K)
         VRHO(I,J,K)=xb%rho(I,J,K)*xb%v(I,J,K)
      ENDDO
      ENDDO
   ENDDO

   CALL DA_UV_To_Divergence(xb%ds,xb%map_factor,URHO,VRHO,DIV, &
                            ids,ide, jds,jde, kds,kde,         &
                            ims,ime, jms,jme, kms,kme,         &
                            its,ite, jts,jte, kts,kte)

   DO J=jts,jte
   DO I=its,ite
      TERM3=0.

         DO K=kte-1,kts,-1
            TERM3=TERM3+GRAVITY*(DIV(I,J,K+1)+DIV(I,J,K))*0.5  &
                       *(xb%h(I,J,K+1)-xb%h(I,J,K))
            WZ_b(I,J,K)=WZ_b(I,J,K)+TERM3
         ENDDO

   ENDDO
   ENDDO


   DO K=kts,kte
   DO J=jts,jte
   DO I=its,ite
      WZ_b(I,J,K)=WZ_b(I,J,K)/(GAMMA*xb%p(I,J,K))
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE WZ_BASE
