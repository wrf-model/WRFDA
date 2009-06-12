program process_Lightning
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2008-01-02
!
! ABSTRACT: 
!     This routine read in lightning data and 
!     map them into GSI mass grid
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  NLDN and Alasks lightning data
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  wJET
!
!$$$
!
!_____________________________________________________________________
!
  use mpi
  use map_utils
  use misc_definitions_module , only : PROJ_LC
  use constants_module ,only : EARTH_RADIUS_M
  use kinds, only: r_kind,i_kind

  implicit none
  INCLUDE 'netcdf.inc'
!
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

  real     :: rad2deg = 180.0/3.1415926
!
  character*256 output_file
!
!  grid and map
  integer :: nlon,nlat
  CHARACTER*180   geofile

  real ::  userDX, userDY, CEN_LAT, CEN_LON
  real ::  userTRUELAT1,userTRUELAT2,MOAD_CEN_LAT,STAND_LON
  integer :: MAP_PROJ

  type (proj_info) :: proj_stack
  REAL :: truelat1, truelat2, stdlon, lat1, lon1, r_earth
  REAL :: knowni, knownj, dx

!
!  For lightning data
!
  INTEGER ::    numStrike
  CHARACTER*180   lightsngle
  real,allocatable:: llon(:)    !
  real,allocatable:: llat(:)    !
  integer,allocatable:: ltime(:)   !
  integer,allocatable:: lstrike(:) !
  character*21,allocatable:: ctime(:)   !
  real :: rtmp
  integer,allocatable:: lquality(:) !

  REAL, allocatable :: lightning(:,:)   ! lightning  strakes
  REAL(r_kind), allocatable :: lightning_out(:,:)   ! lightning  strakes

  integer :: numNLDN_all, numNLDN_used
  integer :: numAlaska_all, numAlask_used
!
!! Declare namelists 
!
! SETUP (general control namelist) :
!
  character*10 :: analysis_time
  integer      :: NLDN_filenum
  logical      :: IfAlaska
  namelist/setup/analysis_time, NLDN_filenum, IfAlaska
!
!  ** misc
      
  CHARACTER*180   workpath

  real :: LAT_LL_P,LON_LL_P
  real :: user_known_x, user_known_y
  real :: XC,YC

  integer i,j,igrid,jgrid,nt

  integer :: NCID, istatus
  real,allocatable:: xlon(:,:)    !
  real,allocatable:: ylat(:,:)    !

  integer :: numlightning


!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror) 
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

write(*,*) mype, 'deal with lightning'
  numNLDN_all=0
  numNLDN_used=0
  numAlaska_all=0
  numAlask_used=0

  read(5,setup)

!
! set geogrid fle name
!
  workPath='./'
  write(geofile,'(a,a)') trim(workPath), 'geo_em.d01.nc'

  write(*,*) 'geofile', trim(geofile)
  call GET_DIM_ATT_geo(geofile,NLON,NLAT)
  write(*,*) 'NLON,NLAT',NLON,NLAT

  call GET_MAP_ATT_geo(geofile, userDX, userDY, CEN_LAT, CEN_LON, &
                userTRUELAT1,userTRUELAT2,MOAD_CEN_LAT,STAND_LON,MAP_PROJ)
  write(*,*) userDX, userDY, CEN_LAT, CEN_LON
  write(*,*) userTRUELAT1,userTRUELAT2,MOAD_CEN_LAT,STAND_LON,MAP_PROJ
!
!   setup  map
!

  user_known_x = (NLON+1)/2.0
  user_known_y = (NLAT+1)/2.0
  call map_init(proj_stack)

  if (MAP_PROJ == PROJ_LC) then
     call map_set(MAP_PROJ, proj_stack, &
                  truelat1=userTRUELAT1, &
                  truelat2=userTRUELAT2, &
                  stdlon=STAND_LON, &
                  lat1=CEN_LAT, &
                  lon1=CEN_LON, &
                  knowni=user_known_x, &
                  knownj=user_known_y, &
                  dx=userDX, &
                  r_earth=earth_radius_m)
  else
     write(*,*) 'Wrong map projection: only lambert works now'
     stop 123
  endif

!
!  get GSI horizontal grid in latitude and longitude
!
  allocate(xlon(nlon,nlat))
  allocate(ylat(nlon,nlat))

  call OPEN_geo(geofile, NCID)
  call GET_geo_sngl_geo(NCID,Nlon,Nlat,ylat,xlon)
  call CLOSE_geo(NCID)

  allocate(lightning(nlon,nlat))
  lightning=0

!
!  process NLDN data
!
  DO nt=1,NLDN_filenum
    write(lightsngle,'(a,I1)') 'NLDN_lightning_',nt
    write(*,*) trim(lightsngle)
    call ifexist_file(trim(lightsngle),istatus)
    if (ISTATUS .NE. NF_NOERR) CYCLE

    call GET_DIM_ATT_NLDN(lightsngle,numStrike)
    write(*,*) 'number of strikes=', nt, numStrike

    numNLDN_all=numNLDN_all+numStrike

    allocate(llon(numStrike))
    allocate(llat(numStrike))
    allocate(ltime(numStrike))
    allocate(lStrike(numStrike))

    call GET_lightning_NLDN(lightsngle,numStrike,llon,llat,ltime,lStrike)
!  do i=1,numStrike
!     write(*,*) i, llon(i),llat(i),ltime(i),lStrike(i)
!  enddo
!
!  check quality
!
    allocate(lquality(numStrike))
    lquality = 0    ! 0 good data,  > 0 bad data
    call Check_NLDN(numStrike,llon,llat,ltime,lstrike,lquality)

    do i=1,numStrike

      if(lquality(i) == 0 ) then
        call latlon_to_ij(proj_stack, llat(i), llon(i), xc, yc)
        igrid = int(XC+0.5)
        jgrid = int(YC+0.5)
        if( (igrid > 0 .and. igrid< nlon).and.  &
            (jgrid > 0 .and. jgrid< nlat)) then 
            lightning(igrid,jgrid) = lightning(igrid,jgrid) + 1
            numNLDN_used=numNLDN_used+1
        endif
      endif

    enddo

    deallocate(llon)
    deallocate(llat)
    deallocate(ltime)
    deallocate(lStrike)
    deallocate(lquality)
  enddo ! nt
!
!  process Alaska lightning data
!
  if(IfAlaska) then
  numStrike=0
  lightsngle='ALSKA_lightning'
  OPEN( 12, file=trim(lightsngle),status='old',form='formatted', err=200)
99  read(12,*,end=100) 
    numStrike=numStrike+1 
    go to 99
100 continue

    numAlaska_all=numAlaska_all+numStrike

    allocate(llon(numStrike))
    allocate(llat(numStrike))
    allocate(ctime(numStrike))
    allocate(lStrike(numStrike))
    rewind(12)
    DO i=1,numStrike
      read(12,*) ctime(i),llat(i),llon(i),rtmp,lStrike(i)
    ENDDO
  close(12)
!
!  quality control
!
  allocate(lquality(numStrike))
  lquality = 0    ! 0 good data,  > 0 bad data
  call Check_Alaska(numStrike,llon,llat,ctime,lstrike,lquality,analysis_time)
!  do i=1,20
!    write(*,*) i, llon(i),llat(i),ctime(i),lstrike(i),lquality(i)
!  enddo

  do i=1,numStrike

    if(lquality(i) == 0 ) then
      call latlon_to_ij(proj_stack, llat(i), llon(i), xc, yc)
      igrid = int(XC+0.5)
      jgrid = int(YC+0.5)
      if( (igrid > 0 .and. igrid< nlon).and.  &
          (jgrid > 0 .and. jgrid< nlat)) then 
          lightning(igrid,jgrid) = lightning(igrid,jgrid) + 1
          numAlask_used=numAlask_used+1
      endif
    endif

  enddo

  deallocate(llon)
  deallocate(llat)
  deallocate(ctime)
  deallocate(lStrike)
  deallocate(lquality)
200 continue
  endif
!
!  statistic
!
  write(*,*) ' The total number of NLDN data is:', numNLDN_all
  write(*,*) ' The number of NLDN data used is:', numNLDN_used
  write(*,*) ' The total number of Alasks data is:', numAlaska_all
  write(*,*) ' The number of Alasks data used is:', numAlask_used

!
!     Find max reflectivity in each column
!
   allocate(lightning_out(3,nlon*nlat))
   numlightning=0
   DO j=1,nlat
   DO i=1,nlon
     if(lightning(i,j) > 0 ) then
       numlightning=numlightning+1
       lightning_out(1,numlightning)=float(i)
       lightning_out(2,numlightning)=float(j)
       lightning_out(3,numlightning)=lightning(i,j)
     endif
   ENDDO
   ENDDO
     write(*,*) 'Dump out results',numlightning,'out of',nlon*nlat
     OPEN(10,file=trim(workPath)//'LightningInGSI.dat',form='unformatted')
      write(10) 3,nlon,nlat,numlightning,1,2
      write(10) ((lightning_out(i,j),i=1,3),j=1,numlightning)
     close(10)
!
   write(6,*) ' write lightning in BUFR'
   call write_bufr_lightning(1,nlon,nlat,numlightning,lightning_out)

  call MPI_FINALIZE(ierror)
!
end program process_Lightning 
!
