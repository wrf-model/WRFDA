      SUBROUTINE DA_Zero_X ( x )
!
!  05/27/2004       GPS Refractivity "ref" added.          Y.-R. Guo
!

      IMPLICIT NONE

      TYPE (x_type), INTENT(OUT)        :: x      ! Analysis incrs structure.
   
      x % u(:,:,:) = 0.0
      x % v(:,:,:) = 0.0
      x % w(:,:,:) = 0.0
      x % t(:,:,:) = 0.0
      x % q(:,:,:) = 0.0
      x % p(:,:,:) = 0.0
      x % rh(:,:,:) = 0.0
      x % wh(:,:,:) = 0.0
      x % rho(:,:,:) = 0.0
      x % ref(:,:,:) = 0.0

      x % qcw(:,:,:) = 0.0
      x % qrn(:,:,:) = 0.0
      x % qt (:,:,:) = 0.0
      x % qci(:,:,:) = 0.0
      x % qsn(:,:,:) = 0.0
      x % qgr(:,:,:) = 0.0

      x % tgrn(:,:) = 0.0
      x % psfc(:,:) = 0.0
      x % u10(:,:) = 0.0
      x % v10(:,:) = 0.0
      x % t2(:,:) = 0.0
      x % q2(:,:) = 0.0
   
      x % tpw(:,:) = 0.0
      x % speed(:,:) = 0.0
      x % tb19v(:,:) = 0.0
      x % tb19h(:,:) = 0.0
      x % tb22v(:,:) = 0.0
      x % tb37v(:,:) = 0.0
      x % tb37h(:,:) = 0.0
      x % tb85v(:,:) = 0.0
      x % tb85h(:,:) = 0.0
    
      END SUBROUTINE DA_Zero_X

