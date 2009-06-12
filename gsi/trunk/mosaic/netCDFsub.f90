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

Subroutine  OPEN_Mosaic(mosaicfile,NCID)
!
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

   CHARACTER*120 ::  mosaicfile
   integer :: NCID
   integer :: status
!
   STATUS=NF_OPEN(trim(mosaicfile),0,NCID)
!
   IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

end Subroutine OPEN_Mosaic
!
Subroutine  CLOSE_Mosaic(NCID)
! 
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

   integer :: NCID
   integer :: status
! 
   STATUS=NF_CLOSE(NCID)
! 
   IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

end Subroutine CLOSE_Mosaic


Subroutine  GET_Mosaic_sngl_Mosaic(NCID,mscNlon,mscNlat,mscNlev,mscValue)
!
!  Author: Ming Hu, CAPS. University of Oklahma.
!  
!  First written: 04/06/2006.
!
!  IN:
!     mscNlon
!     mscNlan
!     mscNlev
!     NCID
!  out:
!     mscValue
!
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data
  INTEGER ::   mscNlev   ! number of level of mosaic data

  INTEGER ::  NCID, STATUS, MSID

  INTEGER ::   NDIMS
  PARAMETER (NDIMS=4)                  ! number of dimensions
  INTEGER START(NDIMS), COUNT(NDIMS)

  REAL ::   mscValue(mscNlon,mscNlat,1,1)
  INTEGER :: i,j

  START(1)=1
  START(2)=1
  START(3)=mscNlev
  START(4)=1
  COUNT(1)=mscNlon
  COUNT(2)=mscNlat
  COUNT(3)=1
  COUNT(4)=1

  STATUS = NF_INQ_VARID (NCID, 'mrefl_mosaic', MSID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_VARA_REAL (NCID, MSID, START, COUNT, mscValue)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

end subroutine GET_Mosaic_sngl_Mosaic


Subroutine  Check_DIM_ATT_Mosaic8(NCID,mscNlon,mscNlat,mscNlev, &
                   lonMin,latMin,lonMax,latMax,dlon,dlat)
!
!  Author: Ming Hu, CAPS. University of Oklahma.
!  
!  First written: 04/06/2006.
!
!  IN:
!     mscNlon
!     mscNlan
!     mscNlev
!     lonMin,latMin,lonMax,latMax,dlon,dlat
!
!  out:
!     NCID
!
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data
  INTEGER ::   mscNlev   ! number of level of mosaic data
  REAL    ::   lonMin,latMin,lonMax,latMax,dlon,dlat

  INTEGER ::  NCID, STATUS
  INTEGER ::  LONID, LATID, LEVID
  INTEGER ::  LONLEN, LATLEN, LEVLEN

  REAL ::   lonMinVal
  REAL ::   latMinVal
  REAL ::   lonMaxVal
  REAL ::   latMaxVal
  REAL ::   dlonVal
  REAL ::   dlatVal

  STATUS = NF_INQ_DIMID(NCID, 'Lon', LONID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMID(NCID, 'Lat', LATID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMID(NCID, 'Ht', LEVID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

  STATUS = NF_INQ_DIMLEN(NCID, LONID, LONLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMLEN(NCID, LATID, LATLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMLEN(NCID, LEVID, LEVLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

  if( mscNlon.ne.LONLEN .or. mscNlat.ne.LATLEN .or. mscNlev.ne.LEVLEN) then
    write(*,*) 'Dimension is inconsistent'
    write(*,*) 'INPUT:', mscNlon,mscNlat,mscNlev
    write(*,*) 'Decoding', LONLEN,LATLEN,LEVLEN
    stop 123
  endif

  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'Longitude', lonMinVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'Latitude', latMaxVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'LonGridSpacing', dlonVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'LatGridSpacing', dlatVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

  if( abs(lonMin-lonMinVal) > 0.00001 .or. &
      abs(latMax-latMaxVal) > 0.00001 .or. &
      abs(dlon-dlonVal) > 0.00001 .or. &
      abs(dlat-dlatVal) > 0.00001) then
    write(*,*) ' Attributes are inconsistent. Check'
    stop 123
  endif

  write(*,*) 'PASS check'

END SUBROUTINE Check_DIM_ATT_Mosaic8

Subroutine  Check_DIM_ATT_Mosaic(NCID,mscNlon,mscNlat,mscNlev, &
                   lonMin,latMin,lonMax,latMax,dlon,dlat)
!
!  Author: Ming Hu, CAPS. University of Oklahma.
!  
!  First written: 04/06/2006.
!
!  IN:
!     mscNlon
!     mscNlan
!     mscNlev
!     lonMin,latMin,lonMax,latMax,dlon,dlat
!
!  out:
!     NCID
!
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data
  INTEGER ::   mscNlev   ! number of level of mosaic data
  REAL    ::   lonMin,latMin,lonMax,latMax,dlon,dlat

  INTEGER ::  NCID, STATUS
  INTEGER ::  LONID, LATID, LEVID
  INTEGER ::  LONLEN, LATLEN, LEVLEN

  REAL ::   lonMinVal
  REAL ::   latMinVal
  REAL ::   lonMaxVal
  REAL ::   latMaxVal
  REAL ::   dlonVal
  REAL ::   dlatVal

  STATUS = NF_INQ_DIMID(NCID, 'x', LONID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMID(NCID, 'y', LATID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMID(NCID, 'mrefl_mosaic_levels', LEVID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

  STATUS = NF_INQ_DIMLEN(NCID, LONID, LONLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMLEN(NCID, LATID, LATLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMLEN(NCID, LEVID, LEVLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

  if( mscNlon.ne.LONLEN .or. mscNlat.ne.LATLEN .or. mscNlev.ne.LEVLEN) then
    write(*,*) 'Dimension is inconsistent'
    write(*,*) 'INPUT:', mscNlon,mscNlat,mscNlev
    write(*,*) 'Decoding', LONLEN,LATLEN,LEVLEN
    stop 123
  endif

  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'xMin', lonMinVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'yMin', latMinVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'xMax', lonMaxVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'yMax', latMaxVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'dx', dlonVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'dy', dlatVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

  if( abs(lonMin-lonMinVal) > 0.00001 .or. &
      abs(lonMax-lonMaxVal) > 0.00001 .or. &
      abs(latMin-latMinVal) > 0.00001 .or. &
      abs(latMax-latMaxVal) > 0.00001 .or. &
      abs(dlon-dlonVal) > 0.00001 .or. &
      abs(dlat-dlatVal) > 0.00001) then
    write(*,*) ' Attributes are inconsistent. Check'
    stop 123
  endif

  write(*,*) 'PASS check'

END SUBROUTINE Check_DIM_ATT_Mosaic

Subroutine  GET_DIM_ATT_Mosaic8(mosaicsngle,mscNlon,mscNlat,mscNlev, &
                   lonMin,latMin,lonMax,latMax,dlon,dlat)
!
!  Author: Ming Hu, CAPS. University of Oklahma.
!  
!  First written: 04/06/2006.
!
!   New verison of Mosaic file that has 8 tiles
!
!  IN:
!     mosaicPath  : path of mosaic file
!     mosaicsngle : name of mosaic file
!  OUT
!     mscNlon
!     mscNlan
!     mscNlev
!     lonMin,latMin,lonMax,latMax,dlon,dlat
!
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER*120    mosaicsngle

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data
  INTEGER ::   mscNlev   ! number of level of mosaic data
  REAL    ::   lonMin,latMin,lonMax,latMax,dlon,dlat

  INTEGER ::  NCID, STATUS
  INTEGER ::  LONID, LATID, LEVID
  INTEGER ::  LONLEN, LATLEN, LEVLEN

  REAL ::   lonMinVal
  REAL ::   latMinVal
  REAL ::   lonMaxVal
  REAL ::   latMaxVal
  REAL ::   dlonVal
  REAL ::   dlatVal

  STATUS = NF_OPEN(trim(mosaicsngle), 0, NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

  STATUS = NF_INQ_DIMID(NCID, 'Lon', LONID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMID(NCID, 'Lat', LATID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMID(NCID, 'Ht', LEVID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

  STATUS = NF_INQ_DIMLEN(NCID, LONID, LONLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMLEN(NCID, LATID, LATLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMLEN(NCID, LEVID, LEVLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

  mscNlon=LONLEN
  mscNlat=LATLEN
  mscNlev=LEVLEN


  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'Longitude', lonMinVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'Latitude', latMaxVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'LatGridSpacing', dlonVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'LatGridSpacing', dlatVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

  lonMin=lonMinVal
  latMax=latMaxVal
  dlon=dlonVal
  dlat=dlatVal
  lonMax=lonMinVal+dlon*mscNlon
  latMin=latMaxVal-dlat*mscNlat

  STATUS = NF_CLOSE(NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS) 

END SUBROUTINE GET_DIM_ATT_Mosaic8

Subroutine  GET_DIM_ATT_Mosaic(mosaicsngle,mscNlon,mscNlat,mscNlev, &
                   lonMin,latMin,lonMax,latMax,dlon,dlat)
!
!  Author: Ming Hu, CAPS. University of Oklahma.
!  
!  First written: 04/06/2006.
!
!  IN:
!     mosaicPath  : path of mosaic file
!     mosaicsngle : name of mosaic file
!  OUT
!     mscNlon
!     mscNlan
!     mscNlev
!     lonMin,latMin,lonMax,latMax,dlon,dlat
!
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER*120    mosaicsngle

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data
  INTEGER ::   mscNlev   ! number of level of mosaic data
  REAL    ::   lonMin,latMin,lonMax,latMax,dlon,dlat

  INTEGER ::  NCID, STATUS
  INTEGER ::  LONID, LATID, LEVID
  INTEGER ::  LONLEN, LATLEN, LEVLEN

  REAL ::   lonMinVal
  REAL ::   latMinVal
  REAL ::   lonMaxVal
  REAL ::   latMaxVal
  REAL ::   dlonVal
  REAL ::   dlatVal

  STATUS = NF_OPEN(trim(mosaicsngle), 0, NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

  STATUS = NF_INQ_DIMID(NCID, 'x', LONID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMID(NCID, 'y', LATID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMID(NCID, 'mrefl_mosaic_levels', LEVID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

  STATUS = NF_INQ_DIMLEN(NCID, LONID, LONLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMLEN(NCID, LATID, LATLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_INQ_DIMLEN(NCID, LEVID, LEVLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

  mscNlon=LONLEN
  mscNlat=LATLEN
  mscNlev=LEVLEN


  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'xMin', lonMinVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'yMin', latMinVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'xMax', lonMaxVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'yMax', latMaxVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'dx', dlonVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)
  STATUS = NF_GET_ATT_REAL (NCID, NF_GLOBAL, 'dy', dlatVal)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS)

  lonMin=lonMinVal
  lonMax=lonMaxVal
  latMin=latMinVal
  latMax=latMaxVal
  dlon=dlonVal
  dlat=dlatVal

  STATUS = NF_CLOSE(NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_Mosaic(STATUS) 

END SUBROUTINE GET_DIM_ATT_Mosaic
!
SUBROUTINE HANDLE_ERR_Mosaic(STATUS)
     INCLUDE 'netcdf.inc'
     INTEGER STATUS
     IF (STATUS .NE. NF_NOERR) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
     ENDIF
END SUBROUTINE HANDLE_ERR_Mosaic
