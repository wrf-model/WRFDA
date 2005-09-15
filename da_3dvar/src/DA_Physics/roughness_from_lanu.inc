subroutine roughness_from_lanu(ltbl, mminlu, date_mm5, xp, lanu, rough)

  implicit none

  integer             ,   intent(in)    :: ltbl
  character (len=4)   ,   intent(in)    :: mminlu
  character (len=19)  ,   intent(in)    :: date_mm5
  TYPE (xpose_type),      intent(in)    :: xp  ! Domain decomposition vars.
  real, dimension(xp%ims:xp%ime,xp%jms:xp%jme),   intent(in)    :: lanu 
  real, dimension(xp%ims:xp%ime,xp%jms:xp%jme),   intent(out)   :: rough 

  integer                               :: LS, LC, LI, LUCATS, LUSEAS, &
                                           LUMATCH, year, month, day,  &
                                           julday, Ict, Isn, io_error, &
                                           m1, m2, n1, n2 
  real                                  :: albd, slmo, sfem
  real(kind=4), dimension(50,2)         :: sfz0
  character (len=4)                     :: LUTYPE
  logical                               :: exist

  read(date_mm5,'(I4,1x,I2,1X,I2)') year, month, day
  call Julian_DAY(year,month,day,Julday, 1)
  Isn = 1
  IF(JULDAY < 105 .OR. JULDAY > 288) Isn=2
 
  INQUIRE (FILE = 'LANDUSE.TBL', EXIST = exist )

  if (exist) then
    OPEN (UNIT = ltbl, FILE = 'LANDUSE.TBL', FORM='formatted', &
                     ACTION = 'read', IOSTAT = io_error )
  else
    WRITE(unit=0, fmt='(a)') 'No file: LANDUSE.TBL for conversion of roughness'
    WRITE(unit=*, fmt='(a)') 'No file: LANDUSE.TBL for conversion of roughness'
    CALL wrf_shutdown
    stop 22222
  endif

  LUMATCH=0  

  DO
      READ (ltbl,'(A4)', IOSTAT=io_error) LUTYPE
      if (io_error /= 0) exit
      READ (ltbl,*, IOSTAT=io_error) LUCATS,LUSEAS

      IF(LUTYPE == MMINLU) LUMATCH=1 

      DO LS=1,LUSEAS 
         READ (ltbl,*)  
         DO LC=1,LUCATS 
            IF(LUTYPE == MMINLU)THEN 
               READ (ltbl,*) LI, ALBD, SLMO, SFEM, SFZ0(LC,LS)
               IF(LC /= LI) STOP 'MISSING LANDUSE: LC'   
            ELSE 
               READ (ltbl,*) 
            ENDIF 
         ENDDO 
      ENDDO
   ENDDO

   close (ltbl)

   IF(LUMATCH == 0)THEN
      CALL wrf_shutdown
      PRINT *,'LANDUSE IN INPUT FILE DOES NOT MATCH LUTABLE' 
      STOP 'INCONSISTENT OR MISSING LUTABLE FILE' 
   ENDIF   

   m1 = xp % its
   m2 = xp % ite
   n1 = xp % jts
   n2 = xp % jte

!  write(0,'(''dimensions of lanu: m1,m2,n1,n2='',4I4)') m1,m2,n1,n2

   do lc = m1,m2
   do ls = n1,n2
      Li = int(lanu(lc,ls)+0.001)
      rough(lc,ls) =  sfz0(Li,Isn)/100.
   enddo
   enddo

end subroutine roughness_from_lanu

SUBROUTINE Julian_DAY(NY,NM,ND,JD,METHOD)   
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

END subroutine Julian_DAY

