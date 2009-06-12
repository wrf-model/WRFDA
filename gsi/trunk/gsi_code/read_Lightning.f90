subroutine read_lightning(nread,ndata,infile,obstype,lunout,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_lightning          Reading in lightning data  
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2008-03-27
!
! ABSTRACT: 
!     This routine reads in lightning data. The lightning data has already  
!          been interpolated into analysis grid and in form of BUFR.
!
! PROGRAM HISTORY LOG:
!    2008-12-20  Hu  make it read in BUFR form lightning data
!
!
!   input argument list:
!     infile   - unit from which to read mosaic information file
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     mype     - processor ID that does this IO
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!
! USAGE:
!   INPUT FILES:  lghtInGSI
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,r_double,i_kind
  use constants, only: zero,one,izero,ione
  use convinfo, only: nconvtype,ctwind,cgross,cermax,cermin,cvar_b,cvar_pg, &
        ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype

  implicit none
!
  
  integer(i_kind),intent(in):: mype
  character(10),intent(in):: infile,obstype
  integer(i_kind),intent(in):: lunout
  integer(i_kind),intent(inout):: nread,ndata
!
!  For lightning
!
  integer(i_kind) nreal,nchanl,ilat,ilon
  character(20):: sis

  integer(i_kind) ifn,i,j
 
  logical :: lightningobs

!
!  for read in bufr
!
    real(r_kind) :: hdr(5),obs(1,35)
    character(80):: hdrstr='SID XOB YOB DHR TYP'
    character(80):: obsstr='POB'

    REAL(i_kind),PARAMETER ::  MXBF = 160000_i_kind
    INTEGER(i_kind) :: ibfmsg = MXBF/4_i_kind

    character(8) subset,sid
    integer(i_kind) :: lunin,idate

    INTEGER(i_kind)  ::  maxlvl,nlon,nlat
    INTEGER(i_kind)  ::  numlvl,numlight
    INTEGER(i_kind)  ::  n,k,iret
    INTEGER(i_kind),PARAMETER  ::  nmsgmax=100000_i_kind
    INTEGER(i_kind)  ::  nmsg,ntb
    INTEGER(i_kind)  ::  nrep(nmsgmax)

    REAL(r_kind),allocatable :: lightning_in(:,:)   ! 3D reflectivity in column

    REAL * 8 :: rid
    EQUIVALENCE (sid,rid)

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
   lightningobs = .false.
   do i=1,nconvtype
       if(trim(obstype) == trim(ioctype(i)) .and. icuse(i)== 1) lightningobs =.true.
   end do

   nchanl= izero
   nread = izero
   ndata = izero
   ifn = 15_i_kind
   sis='lghtn'
!
   if(lightningobs) then
      lunin = 10            
      OPEN  ( UNIT = lunin, FILE = trim(infile),form='unformatted',err=200)
      CALL OPENBF  ( lunin, 'IN', lunin )
      CALL DATELEN  ( 10 )

      call readmg(lunin,subset,idate,iret)

      nmsg= ione
      ntb = izero

      loop_report: do
         call readsb(lunin,iret)
         if(iret/=0) then
                call readmg(lunin,subset,idate,iret)
                if(iret/=0) then
                        exit loop_report ! end of file
                else
                  nmsg=nmsg+1
                  if (nmsg>nmsgmax) then
                        write(6,*)'read_lightning: messages exceed maximum ',nmsgmax
                        call stop2(112)
                  endif
                endif
                cycle loop_report
         else
                nrep(nmsg)=nrep(nmsg)+ione  ! count reports per message 
         endif
!    Extract type, date, and location information
         call ufbint(lunin,hdr,5,1,iret,hdrstr)
         if( ntb == izero ) then
           call ufbint(lunin,obs,1,35,iret,obsstr)
           numlvl=iret
         endif

        ntb = ntb+ione
        rid=hdr(1)
      enddo loop_report

      write(6,*)'READ_LIGHTNING: messages/reports = ',nmsg,'/',ntb
      write(6,*)'READ_LIGHTNING: levels = ',numlvl

      numlight=ntb
      maxlvl=numlvl
     if( numlight > izero ) then
      allocate(lightning_in(numlvl+2,ntb))

      call closbf(lunin)
      OPEN  ( UNIT = lunin, FILE = trim(infile),form='unformatted',err=200)
      CALL OPENBF  ( lunin, 'IN', lunin )
      CALL DATELEN  ( 10 )

      call readmg(lunin,subset,idate,iret)

      nmsg= ione
      ntb = izero
      loop_sb: do

         call readsb(lunin,iret)
         if(iret/=izero) then
                call readmg(lunin,subset,idate,iret)
                if(iret/=izero) then
                        exit loop_sb ! end of file
                else
                  nmsg=nmsg+ione
                  if (nmsg>nmsgmax) then
                        write(6,*)'read_lightning: messages exceed maximum ',nmsgmax
                        call stop2(112)
                  endif
                endif
                cycle loop_sb
         else
                nrep(nmsg)=nrep(nmsg)+ione  ! count reports per message
         endif

!    Extract type, date, and location information
         call ufbint(lunin,hdr,5,1,iret,hdrstr)
         call ufbint(lunin,obs,1,35,iret,obsstr)
         numlvl=iret

         ntb = ntb+ione
         lightning_in(1,ntb)=hdr(2)*10.0_r_kind
         lightning_in(2,ntb)=hdr(3)*10.0_r_kind

         do k=1,numlvl
           lightning_in(2+k,ntb)=obs(1,k)
         enddo

      enddo loop_sb
!
      ilon=ione
      ilat=2
      nread=numlight
      ndata=numlight
      nreal=maxlvl+2
      if(numlight > izero ) then
        write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
        write(lunout) lightning_in
        deallocate(lightning_in)
      endif
     endif
    endif
!
    return
200 continue
    write(6,*) 'read_lightning, Warning : cannot find lightning data file'

end subroutine read_lightning
