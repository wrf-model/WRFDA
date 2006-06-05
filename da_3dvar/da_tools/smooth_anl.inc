SUBROUTINE SMOOTH_ANL(SLAB,IMX,JMX,KX,NPASS,ICRSDOT)
 
!  SECTION  TOOLS
!  PURPOSE  SPATIALLY SMOOTH (USUALLY SLAB) TO REMOVE HIGH
!  FREQUENCY WAVES

   IMPLICIT NONE
   
   REAL, INTENT(INOUT)                :: SLAB(:,:,:)
   INTEGER, INTENT(IN)                :: imx, jmx, kx
   INTEGER, INTENT(IN)                :: npass
   INTEGER, INTENT(IN)                :: icrsdot
   
   REAL, ALLOCATABLE                  :: SLABNEW(:,:)
   REAL                               :: XNU(1:2)
   INTEGER                            :: iost, ie, je, k 
   INTEGER                            :: loop, n, i, j
   
   ALLOCATE (SLABNEW(IMX,JMX))

   IE=IMX-1-ICRSDOT
   JE=JMX-1-ICRSDOT
   XNU(1)=0.50
   XNU(2)=-0.52
   DO K=1,KX
      DO LOOP=1,NPASS*2
         N=2-MOD(LOOP,2)
 
!        FIRST SMOOTH IN THE IMX DIRECTION
 
         DO I=2,IE
            DO J=2,JE
               SLABNEW(I,J)=SLAB(I,J,K)+XNU(N) * &
               ((SLAB(I,J+1,K)+SLAB(I,J-1,K))*0.5-SLAB(I,J,K))
            END DO
         END DO
         DO I=2,IE
            DO J=2,JE
               SLAB(I,J,K)=SLABNEW(I,J)
            END DO
         END DO
 
!        NOW SMOOTH IN THE JMX DIRECTION
 
         DO J=2,JE
            DO I=2,IE
               SLABNEW(I,J)=SLAB(I,J,K)+XNU(N) * &
               ((SLAB(I+1,J,K)+SLAB(I-1,J,K))*0.5-SLAB(I,J,K))
            END DO
         END DO

         DO I=2,IE
            DO J=2,JE
               SLAB(I,J,K)=SLABNEW(I,J)
            END DO
         END DO
      END DO
   END DO

   DEALLOCATE (SLABNEW)

END SUBROUTINE SMOOTH_ANL
