SUBROUTINE da_julian_day(NY,NM,ND,JD,METHOD)   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
!                                  C        
!   METHOD = 1: INPUT ---- NY, NM, ND.  OUTPUT ---- JD                 C        
!   METHOD = 2: INPUT ---- NY, JD.      OUTPUT ---- NM, ND             C        
!                                  C        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
   IMPLICIT NONE

   integer,  intent(IN)    :: METHOD, NY
   integer,  intent(INOUT) :: NM, ND, JD

   integer                 :: LOOP
   integer, DIMENSION(12)  :: MDAY = (/31,28,31,30,31,30,31,31,30,31,30,31/)

   IF(MOD(NY,4) == 0) then
      MDAY(2)=29      
      IF(MOD(NY,100) == 0) then
         MDAY(2)=28
         IF(MOD(NY,400) == 0) then
            MDAY(2)=29
         ENDIF
      ENDIF
   ENDIF

   IF(METHOD == 1) THEN                  
      JD=ND
      JuDAY: DO LOOP=1,NM-1                  
         JD=JD+MDAY(LOOP)                
      ENDDO JuDAY                           
   ELSE IF(METHOD == 2) THEN             
      NM=1                               
      ND=JD
      NYEAR: DO LOOP=1,11                    
         IF(ND <= MDAY(LOOP)) EXIT NYEAR

         ND=ND-MDAY(LOOP)     
         NM=NM+1                      
      ENDDO NYEAR                           
   END IF                                

END subroutine da_julian_day

