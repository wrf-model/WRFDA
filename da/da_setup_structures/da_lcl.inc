subroutine da_lcl( P0, Z0, T0, Q0, PLCL, ZLCL, TLCL, QLCL )

   implicit none

   real, intent(IN)   :: P0, Z0, T0, Q0
   real, intent(OUT)  :: PLCL, ZLCL, TLCL, QLCL

   real   :: DP, QS, EPS

   DP=5.
   PLCL=300.

   DO
      TLCL=T0*((PLCL/P0)**0.286)

      call da_qfrmrh( PLCL, TLCL, 100., QS )

      EPS=QS-Q0

      IF (EPS >= 0.) THEN
         ZLCL=(1004./9.81)*(T0-TLCL)+Z0
         QLCL=QS
         RETURN
      ELSE
         PLCL=PLCL+DP

         IF ( PLCL >= P0 ) THEN
            ZLCL=Z0
            QLCL=Q0
            PLCL=P0
            RETURN
        END IF
      END IF
   ENDDO

end subroutine da_lcl


