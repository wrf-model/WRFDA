REAL FUNCTION UNIFVA (KDUM) 

!-----------------------------------------------------------------------
 
!  PURPOSE :
!  -------
!  Minimal random number generator of Park and Miller with 
!  Bays-Durham shuffle and added safeguards.
!  Returns a uniform random deviate between 0.0. and 1.0 (exclusive 
!  of the endpoint values). Call with kdum a negative integer to 
!  initialize; thereafter, do not alter kdum between successive 
!  deviates in sequence. rnmx should approximate the largest 
!  floating value less than 1. 
 
!  See descripiton of function 'ran1', pg. 271.
   
!-----------------------------------------------------------------------
 
   IMPLICIT NONE
 
   INTEGER ::   KDUM

   INTEGER JPIA,JPIM,JPIQ,JPIR,JPNTAB,JPNDIV
   REAL PPAM,PPEPS,PPRNMX

   PARAMETER(JPIA=16807,JPIM=2147483647,JPIQ=127773,JPIR=2836, &
             JPNTAB=32,JPNDIV=1+(JPIM-1)/JPNTAB, &
             PPAM=1./JPIM,PPEPS=1.2E-07,PPRNMX=1.-PPEPS)

   INTEGER JJ
   INTEGER IJJ,IK
 
   INTEGER NIV(JPNTAB),NIY
   SAVE NIV,NIY
   DATA NIV /JPNTAB*0/, NIY /0/

!  BEGIN MAIN
!  ----------

   IF ( (KDUM.LE.0).OR.(NIY.EQ.0) ) THEN
      KDUM = MAX( -KDUM , 1 )

       DO JJ = JPNTAB+8,1,-1
          IK   = KDUM/JPIQ
          KDUM = JPIA*( KDUM - IK*JPIQ ) - JPIR*IK
 
          IF (KDUM.lt.0) KDUM = KDUM + JPIM
          IF (JJ.LE.JPNTAB) NIV(JJ) = KDUM
 
       END DO

       NIY = NIV(1)
   ENDIF
  
   IK   = KDUM/JPIQ
   KDUM = JPIA*( KDUM - IK*JPIQ ) - JPIR*IK
     
   IF (KDUM.LT.0) KDUM = KDUM + JPIM

   IJJ      = 1 + NIY/JPNDIV
   NIY      = NIV(IJJ)
   NIV(IJJ) = KDUM
   UNIFVA   = MIN( PPAM*NIY , PPRNMX )

END FUNCTION UNIFVA
