SUBROUTINE AADJUST(xb,WZ_a,                      &
                      ids,ide, jds,jde, kds,kde, &
                      ims,ime, jms,jme, kms,kme, &
                      its,ite, jts,jte, kts,kte )
!------------------------------------------------------------------------------
!  PURPOSE: Adjust vertical velocity increments
!
!  ASSUMPTIONS: 1) Model level stored top down.
!
!  HISTORY: 09/21/2002 - Creation of F90 version. Qingnong Xiao
!  HISTORY: 04/15/2004 - Modified of F90 version. Jianfeng Gu
!           02/02/2005 - Global update            Syed RH Rizvi 
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)    :: xb                ! First guess structure.

   integer, intent(in):: ids,ide, jds,jde, kds,kde    ! domain dims.
   integer, intent(in):: ims,ime, jms,jme, kms,kme    ! memory dims.
   integer, intent(in):: its,ite, jts,jte, kts,kte    ! tile   dims.

   real, dimension(ims:ime,jms:jme,kms:kme), intent(inout) :: WZ_a

   integer :: I,J,K

   real, dimension(ims:ime,jms:jme,kms:kme)   :: URHO, VRHO, WZ_b

   real :: EBXL1,EBXL2
   real :: EBXL19,EBXL29

   CALL WZ_BASE(xb,WZ_b,                    &
                ids,ide, jds,jde, kds,kde,  &
                ims,ime, jms,jme, kms,kme,  &
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
