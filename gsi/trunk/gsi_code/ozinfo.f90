module ozinfo 
!$$$   module documentation block
!                .      .    .                                       .
! module:    ozinfo
!   prgmmr: treadon          org: np23                date: 2004-04-10
!
! abstract:  This module contains variables and routines related
!            to the assimilation of ozone observations (presently,
!            satellite based ozone observations)
!
! program history log:
!   2004-04-10  treadon - original code
!   2004-05-13  kleist  - original documentation
!   2004-06-16  treadon - update documentation
!   2004-12-22  treadon - rename logical "idiag_ozone" to "diag_ozone"
!   2005-09-28  derber  - Modify for new ozinfo file, add var qc parameters
!   2006-02-03  derber  - modify for new obs control and obs count
!   2007-06-29  Zhou    - change total number of ozone enteries (jpch_oz) from
!                         53 (version 6 SBUV/2) to 67 (version 8 SBUV/2)
!
! Subroutines Included:
!   sub init_oz       - set ozone related variables to defaults
!   sub ozinfo_read   - read in ozone info
!
! Functions Included:
!
! Variable Definitions:
!   def diag_ozone     - logical to turn off or on the diagnostic ozone file (true=on)
!   def jpch_oz        - number of (levels+1) * number of satellites
!   def mype_oz        - task id for writing out radiance diagnostics
!   def pob_oz         - pressure level of observation (hPa)
!   def gross_oz       - gross error limit
!   def error_oz       - observation error
!   def nusis_oz       - sensor/intrument/satellite id (14=NOAA-14, 15=NOAA-15, 16=NOAA-16, etc)
!   def nulev          - integer level of ozone observation
!   def iuse_oz        - integer flag to control usage of ozone data (-1=don't use, 1=use)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only:r_kind,i_kind

  logical diag_ozone
  integer(i_kind) mype_oz,jpch_oz
  real(r_kind),allocatable,dimension(:)::pob_oz,gross_oz,error_oz,pg_oz,b_oz
  integer(i_kind),allocatable,dimension(:):: nulev,iuse_oz
  character(len=20),allocatable,dimension(:):: nusis_oz

contains
  
  subroutine init_oz
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_oz     initialize parameters for ozone data
!     prgmmr:    treadon     org: np23                date: 2004-04-10
!
! abstract:  This routine sets default values for variables used in 
!            the ozone processing routines
!
! program history log:
!   2004-04-10  treadon
!   2004-06-16  treadon, documentation
!   2005-07-28  treadon - increase jpch_oz from 52 to 53 (add omi data)
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use mpimod, only: npe       ! contains the number of mpi tasks, variable "npe"

    jpch_oz = 0                 ! jpch_oz is total number of enteries in the
                                ! the fixed file (global_ozinfo.txt) read by 
                                ! ozinfo_read
    diag_ozone = .true.         ! default is to generate ozone diagnostic file
    mype_oz     = max(0,npe-6)  ! mpi task to write ozone summary report

  end subroutine init_oz
  

  subroutine ozinfo_read(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ozinfo_read      read ozone information file
!     prgmmr:    treadon     org: np23                date: 2004-04-10
!
! abstract:  This routine reads the ozone information file, global_ozinfo.txt
!
! program history log:
!   2004-04-10  treadon
!   2004-06-16  treadon, documentation
!   2005-10-11  treadon - change ozinfo read to free format
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_kind,i_kind
    use obsmod, only: iout_oz
    implicit none

    character(len=1):: cflg
    character(len=120) crecord
    integer(i_kind) lunin,mype,i,j
    data lunin / 47 /


!   Determine number of entries in ozone information file
    open(lunin,file='ozinfo',form='formatted')
    j=0
    do i=1,5000
       read(lunin,100,end=110,err=200) cflg,crecord
       if (cflg == '!') cycle
       j=j+1
    end do
110 continue
    jpch_oz = j
    

!   Allocate arrays to receive ozone information
    allocate(nusis_oz(jpch_oz),nulev(jpch_oz),iuse_oz(jpch_oz), &
         pob_oz(jpch_oz),gross_oz(jpch_oz),error_oz(jpch_oz), &
         pg_oz(jpch_oz),b_oz(jpch_oz))


!   All mpi tasks open and read ozone information file.
!   Task mype_oz writes information to ozone runtime file
  
    rewind(lunin)
    if (mype==mype_oz) open(iout_oz)
    j=0
    do i=1,5000
       read(lunin,100,end=120,err=200) cflg,crecord
       if (cflg == '!') cycle
       j=j+1
       read(crecord,*) nusis_oz(j),&
            nulev(j),iuse_oz(j),pob_oz(j),gross_oz(j),error_oz(j), &
            b_oz(j),pg_oz(j)
       if (mype==mype_oz) write(iout_oz,130) j,nusis_oz(j),nulev(j),&
               iuse_oz(j),pob_oz(j),gross_oz(j),error_oz(j),b_oz(j), &
               pg_oz(j)
    end do
120 continue
    close(lunin)
    if (mype==mype_oz) close(iout_oz)

100 format(a1,a120)
130 format(i3,1x,a20,' lev = ',i4,' use = ',i2,' pob = ',f9.3,&
         ' gross = ',f7.3,' error = ',f7.3,' b_oz = ',f7.3,' pg_oz = ',f7.3)



!   Successful read, return to calling routine
    return

!   In case of error reading file, jump to here.
200 continue
    write(6,*)'OZINFO_READ:  ***ERROR*** error reading ozinfo ',mype
    close(lunin)
    if (mype==mype_oz) close(iout_oz)
    write(6,*)'OZINFO_READ:  stop program execution'
    call stop2(79)
    
    return
  end subroutine ozinfo_read
  
end module ozinfo
