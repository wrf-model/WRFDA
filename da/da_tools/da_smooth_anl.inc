subroutine da_smooth_anl(SLAB,IMX,JMX,KX,NPASS,ICRSdoT)
 
   ! SECTION  TOOLS
   ! PURPOSE  SPATIALLY SMOOTH (USUALLY SLAB) TO REMOVE HIGH
   ! FREQUENCY WAVES

   implicit none
   
   real, intent(inout)                :: SLAB(:,:,:)
   integer, intent(in)                :: imx, jmx, kx
   integer, intent(in)                :: npass
   integer, intent(in)                :: icrsdot
   
   real, allocatable                  :: SLABNEW(:,:)
   real                               :: XNU(1:2)
   integer                            :: ie, je, k 
   integer                            :: loop, n, i, j
   
   allocate (SLABNEW(IMX,JMX))

   IE=IMX-1-ICRSdoT
   JE=JMX-1-ICRSdoT
   XNU(1)=0.50
   XNU(2)=-0.52
   do K=1,KX
      do LOOP=1,NPASS*2
         N=2-MOD(LOOP,2)
 
         ! FIRST SMOOTH in THE IMX DIRECTION
 
         do I=2,IE
            do J=2,JE
               SLABNEW(I,J)=SLAB(I,J,K)+XNU(N) * &
               ((SLAB(I,J+1,K)+SLAB(I,J-1,K))*0.5-SLAB(I,J,K))
            end do
         end do
         do I=2,IE
            do J=2,JE
               SLAB(I,J,K)=SLABNEW(I,J)
            end do
         end do
 
         ! NOW SMOOTH in THE JMX DIRECTION
 
         do J=2,JE
            do I=2,IE
               SLABNEW(I,J)=SLAB(I,J,K)+XNU(N) * &
               ((SLAB(I+1,J,K)+SLAB(I-1,J,K))*0.5-SLAB(I,J,K))
            end do
         end do

         do I=2,IE
            do J=2,JE
               SLAB(I,J,K)=SLABNEW(I,J)
            end do
         end do
      end do
   end do

   deallocate (SLABNEW)

end subroutine da_smooth_anl
