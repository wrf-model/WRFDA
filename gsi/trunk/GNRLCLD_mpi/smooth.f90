!C$$$  SUBPROGRAM DOCUMENTATION BLOCK 
!C                .      .    .                                       . 
!C SUBPROGRAM:    SMOOTH      SMOOTH A METEOROLOGICAL FIELD
!C   PRGMMR: STAN BENJAMIN    ORG: FSL/PROFS  DATE: 90-06-15 
!C 
!C ABSTRACT: SHAPIRO SMOOTHER. 
!C 
!C PROGRAM HISTORY LOG: 
!C   85-12-09  S. BENJAMIN   ORIGINAL VERSION
!C 
!C USAGE:    CALL SMOOTH (FIELD,HOLD,IX,IY,SMTH) 
!C   INPUT ARGUMENT LIST: 
!C     FIELD    - REAL ARRAY  FIELD(IX,IY)
!C                            METEOROLOGICAL FIELD
!C     HOLD     - REAL ARRAY  HOLD(IX,2)
!C                            HOLDING THE VALUE FOR FIELD
!C     IX       - INTEGER     X COORDINATES OF FIELD
!C     IY       - INTEGER     Y COORDINATES OF FIELD
!C     SMTH     - REAL      
!C
!C   OUTPUT ARGUMENT LIST:   
!C     FIELD    - REAL ARRAY  FIELD(IX,IY)
!C                            SMOOTHED METEOROLOGICAL FIELD
!C 
!C REMARKS: REFERENCE: SHAPIRO, 1970: "SMOOTHING, FILTERING, AND
!C   BOUNDARY EFFECTS", REV. GEOPHYS. SP. PHYS., 359-387.
!C   THIS FILTER IS OF THE TYPE 
!C         Z(I) = (1-S)Z(I) + S(Z(I+1)+Z(I-1))/2
!C   FOR A FILTER WHICH IS SUPPOSED TO DAMP 2DX WAVES COMPLETELY
!C   BUT LEAVE 4DX AND LONGER WITH LITTLE DAMPING,
!C   IT SHOULD BE RUN WITH 2 PASSES USING SMTH (OR S) OF 0.5
!C   AND -0.5.
!C   
!C ATTRIBUTES: 
!C   LANGUAGE: FORTRAN-77 + EXTENSIONS
!C   MACHINE:  NAS-9000, VAX, UNIX
!C$$$ 
!C**********************************************************************
!C**********************************************************************

      SUBROUTINE SMOOTH (FIELD,HOLD,IX,IY,SMTH)

!C**********************************************************************
!C**********************************************************************

      REAL      FIELD(IX,IY), HOLD (IX,2)
      SMTH1 = 0.25 * SMTH * SMTH
      SMTH2 = 0.5  * SMTH * (1.-SMTH)
      SMTH3 = (1.-SMTH) * (1.-SMTH)
      SMTH4 = (1.-SMTH)
      SMTH5 = 0.5 * SMTH
      I1 = 2
      I2 = 1
       DO 30 J=2,IY-1
        IT = I1
        I1 = I2
        I2 = IT
         DO 10 I = 2,IX-1
           SUM1 = FIELD (I-1,J+1) + FIELD (I-1,J-1)  &
         + FIELD (I+1,J+1) + FIELD (I+1,J-1)
          SUM2 = FIELD (I  ,J+1) + FIELD (I+1,J  )   &
         + FIELD (I  ,J-1) + FIELD (I-1,J  )
          HOLD(I,I1) = SMTH1*SUM1 + SMTH2*SUM2 + SMTH3*FIELD(I,J)
10      CONTINUE
        IF (J.EQ.2) GO TO 200
         DO 20 I=2,IX-1
          FIELD(I,J-1) = HOLD(I,I2)
20      CONTINUE
200     CONTINUE
30     CONTINUE


       DO 40 I = 2,IX-1
        FIELD (I,IY-1) = HOLD(I,I1)
40    CONTINUE

       DO 50 I = 2,IX-1
        FIELD(I,1) = SMTH4* FIELD(I,1)   &
             + SMTH5 * (FIELD(I-1,1) + FIELD(I+1,1))
        FIELD(I,IY) = SMTH4* FIELD(I,IY) &
             + SMTH5 * (FIELD(I-1,IY) + FIELD(I+1,IY))
50    CONTINUE

       DO 60 J = 2,IY-1
        FIELD(1,J) = SMTH4* FIELD(1,J)   &
             + SMTH5 * (FIELD(1,J-1) + FIELD(1,J+1))
        FIELD(IX,J) = SMTH4* FIELD(IX,J) &
             + SMTH5 * (FIELD(IX,J-1) + FIELD(IX,J+1))
60      CONTINUE

      RETURN
       END
