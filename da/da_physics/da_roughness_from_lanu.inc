subroutine da_roughness_from_lanu(ltbl, mminlu, date_mm5, xp, lanu, rough)

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

  read(UNIT=date_mm5,FMT='(I4,1x,I2,1X,I2)') year, month, day
  call da_julian_day(year,month,day,Julday, 1)
  Isn = 1
  IF(JULDAY < 105 .OR. JULDAY > 288) Isn=2
 
  INQUIRE (FILE = 'LANDUSE.TBL', EXIST = exist )

  if (exist) then
    OPEN (UNIT = ltbl, FILE = 'LANDUSE.TBL', FORM='formatted', &
                     ACTION = 'read', IOSTAT = io_error )
  else
    CALL wrf_error(__FILE__,__LINE__,&
      "Cannot open file LANDUSE.TBL for conversion of roughness")
  endif

  LUMATCH=0  

  DO
      READ (UNIT=ltbl,FMT='(A4)', IOSTAT=io_error) LUTYPE
      if (io_error /= 0) exit
      READ (UNIT=ltbl,FMT=*, IOSTAT=io_error) LUCATS,LUSEAS

      IF(LUTYPE == MMINLU) LUMATCH=1 

      DO LS=1,LUSEAS 
         READ (UNIT=ltbl,FMT=*)  
         DO LC=1,LUCATS 
            IF(LUTYPE == MMINLU)THEN 
               READ (UNIT=ltbl,FMT=*) LI, ALBD, SLMO, SFEM, SFZ0(LC,LS)
               IF(LC /= LI) THEN
                 CALL wrf_error(__FILE__,__LINE__, &
                   "MISSING LANDUSE: LC")
               END IF
            ELSE 
               READ (UNIT=ltbl,FMT=*) 
            ENDIF 
         ENDDO 
      ENDDO
   ENDDO

   close (UNIT=ltbl)

   IF(LUMATCH == 0)THEN
    CALL wrf_error(__FILE__,__LINE__,&
      "LANDUSE IN INPUT FILE DOES NOT MATCH LUTABLE")
   ENDIF   

   m1 = xp % its
   m2 = xp % ite
   n1 = xp % jts
   n2 = xp % jte

!  write(UNIT=stderr,FMT='(''dimensions of lanu: m1,m2,n1,n2='',4I4)') m1,m2,n1,n2

   do lc = m1,m2
   do ls = n1,n2
      Li = int(lanu(lc,ls)+0.001)
      rough(lc,ls) =  sfz0(Li,Isn)/100.
   enddo
   enddo

end subroutine da_roughness_from_lanu
