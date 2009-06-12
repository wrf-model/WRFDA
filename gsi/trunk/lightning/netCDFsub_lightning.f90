!
Subroutine  ifexist_file(mosaicfile,STATUS)
!
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

   CHARACTER*120 ::  mosaicfile
   integer :: NCID
   integer :: status, status1
!
   STATUS=NF_OPEN(trim(mosaicfile),0,NCID)
   IF(STATUS .EQ. NF_NOERR) STATUS1=NF_CLOSE(NCID)
!
end Subroutine ifexist_file

Subroutine  GET_lightning_NLDN(lightningsngle,numStrike,xlon,ylat,time,strike)
!
!  Author: Ming Hu, ESRL/GSD
!  
!  First written: 01/02/2008.
!
!  IN:
!     lightningsngle
!     numStrike
!  out:
!     xlon
!     ylat
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER*120    lightningsngle
  INTEGER ::   numStrike

  INTEGER ::  NCID, STATUS

  INTEGER ::   NDIMS
  PARAMETER (NDIMS=1)                  ! number of dimensions
  INTEGER START(NDIMS), COUNT(NDIMS)

  INTEGER ::   VARID
  REAL ::   xlon(numStrike)
  REAL ::   ylat(numStrike)
  integer   ::   time(numStrike)
  integer   ::   strike(numStrike)

  START(1)=1
  COUNT(1)=numStrike

  STATUS = NF_OPEN(trim(lightningsngle), 0, NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_lightning(STATUS)

  STATUS = NF_INQ_VARID (NCID, 'lat', VARID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_lightning(STATUS)
  STATUS = NF_GET_VARA_REAL (NCID, VARID, START, COUNT, ylat)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_lightning(STATUS)
!
  STATUS = NF_INQ_VARID (NCID, 'lon', VARID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_lightning(STATUS)
  STATUS = NF_GET_VARA_REAL (NCID, VARID, START, COUNT, xlon)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_lightning(STATUS)
!
  STATUS = NF_INQ_VARID (NCID, 'time', VARID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_lightning(STATUS)
  STATUS = NF_GET_VARA_INT (NCID, VARID, START, COUNT, time)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_lightning(STATUS)
!
  STATUS = NF_INQ_VARID (NCID, 'mult', VARID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_lightning(STATUS)
  STATUS = NF_GET_VARA_INT (NCID, VARID, START, COUNT, strike)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_lightning(STATUS)

 STATUS = NF_CLOSE(NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_lightning(STATUS)

end subroutine GET_lightning_NLDN 

Subroutine  GET_DIM_ATT_NLDN(lightningsngle,numStrike)
!
!  Author: Ming Hu, ESRL/GSD                      
!  
!  First written: 01/02/2008.
!
!  IN:
!     lightningsngle : name of the file
!  OUT
!     numStrike: strike number
!
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER*120    lightningsngle

  INTEGER ::   numStrike   ! number of strike in one file
  INTEGER ::  NCID, STATUS
  INTEGER ::  ATT_ID

  STATUS = NF_OPEN(trim(lightningsngle), 0, NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_lightning(STATUS)

  STATUS = NF_INQ_DIMID(NCID, 'strikeNum', ATT_ID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_lightning(STATUS)
  STATUS = NF_INQ_DIMLEN(NCID, ATT_ID, numStrike)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_lightning(STATUS)

  STATUS = NF_CLOSE(NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_lightning(STATUS) 

END SUBROUTINE GET_DIM_ATT_NLDN

SUBROUTINE HANDLE_ERR_lightning(STATUS)
     INCLUDE 'netcdf.inc'
     INTEGER STATUS
     IF (STATUS .NE. NF_NOERR) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
     ENDIF
END SUBROUTINE HANDLE_ERR_lightning
