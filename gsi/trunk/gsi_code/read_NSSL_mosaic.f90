subroutine read_NSSL_mosaic(nread,ndata,infile,obstype,lunout)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_lightning          Reading in NSSL reflectiivty mosaic 
!
!   PRGMMR: Ming Hu          ORG: NP22        DATE: 2006-03-27
!
! ABSTRACT: 
!     This routine read in NSSL reflectiivty mosaic data.  The data has already
!          been interpolated into analysis grid and in form of BUFR.
!
! PROGRAM HISTORY LOG:
!    2008-12-20  Hu  make it read in BUFR form reflectivity  data
!
!   input argument list:
!     infile   - unit from which to read mosaic information file
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!
! USAGE:
!   INPUT FILES:  refInGSI
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  Linux cluster(Wjet)
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

  character(10),intent(in):: infile,obstype
  integer(i_kind),intent(in):: lunout
  integer(i_kind),intent(inout):: nread,ndata
!
!  For reflectiivty mosaic
!
  integer(i_kind) nreal,nchanl
  character(20):: sis

  integer(i_kind) ifn,i,j
 
  real(r_kind)  :: maxref
  integer(i_kind) :: ilon,ilat

  logical :: nsslrefobs
!
!  for read in bufr 
!
    real(r_kind) :: hdr(5),obs(1,35)
    character(80):: hdrstr='SID XOB YOB DHR TYP'
    character(80):: obsstr='HREF'

    REAL(i_kind),PARAMETER ::  MXBF = 160000_i_kind
    INTEGER(i_kind) :: ibfmsg = MXBF/4_i_kind

    character(8) subset,sid
!    character(80) infilebufr
    integer(i_kind) :: lunin,idate

    INTEGER(i_kind)  ::  maxlvl,nlon,nlat
    INTEGER(i_kind)  ::  numlvl,numref
    INTEGER(i_kind)  ::  n,k,iret
    INTEGER(i_kind),PARAMETER  ::  nmsgmax=100000_i_kind
    INTEGER(i_kind)  ::  nmsg,ntb
    INTEGER(i_kind)  ::  nrep(nmsgmax)

    REAL(r_kind),allocatable :: ref3d_column(:,:)   ! 3D reflectivity in column

    REAL * 8 :: rid
    EQUIVALENCE (sid,rid)

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
   nsslrefobs = .false.
   do i=1,nconvtype
       if(trim(obstype) == trim(ioctype(i)) .and. icuse(i)== 1) nsslrefobs=.true.
   end do

   nread=izero
   ndata=izero
   nchanl=izero
   ifn = 15_i_kind
   sis='rad_ref'

   if(nsslrefobs) then
      lunin = 10            
      OPEN  ( UNIT = lunin, FILE = trim(infile),form='unformatted',err=200)
      CALL OPENBF  ( lunin, 'IN', lunin )
      CALL DATELEN  ( 10 )

      call readmg(lunin,subset,idate,iret)

      nmsg= ione
      ntb = izero

      loop_report: do
         call readsb(lunin,iret)
         if(iret/=izero) then
                call readmg(lunin,subset,idate,iret)
                if(iret/=izero) then
                        exit loop_report ! end of file
                else
                  nmsg=nmsg+ione
                  if (nmsg>nmsgmax) then
                        write(6,*)'read_NSSL_mosaic: messages exceed maximum ',nmsgmax
                        call stop2(111)
                  endif
                endif
                cycle loop_report
         else
                nrep(nmsg)=nrep(nmsg)+ione  ! count reports per message 
         endif
!    Extract type, date, and location information
         call ufbint(lunin,hdr,5,1,iret,hdrstr)
         if( ntb .eq. izero ) then
           call ufbint(lunin,obs,1,35,iret,obsstr)
           numlvl=iret
         endif

        ntb = ntb+ione
        rid=hdr(1)
      enddo loop_report

      write(6,*)'read_NSSL_mosaic: messages/reports = ',nmsg,'/',ntb
      write(6,*)'read_NSSL_mosaic: levels = ',numlvl

      numref=ntb
      maxlvl=numlvl
      allocate(ref3d_column(numlvl+2,ntb))

      call closbf(lunin)
      OPEN  ( UNIT = lunin, FILE = trim(infile),form='unformatted',err=200)
      CALL OPENBF  ( lunin, 'IN', lunin )
      CALL DATELEN  ( 10 )

      call readmg(lunin,subset,idate,iret)

      nmsg=ione
      ntb =izero
      loop_sb: do

         call readsb(lunin,iret)
         if(iret/=izero) then
                call readmg(lunin,subset,idate,iret)
                if(iret/=izero) then
                        exit loop_sb ! end of file
                else
                  nmsg=nmsg+ione
                  if (nmsg>nmsgmax) then
                        write(6,*)'read_NSSL_mosaic: messages exceed maximum ',nmsgmax
                        call stop2(111)
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
         ref3d_column(1,ntb)=hdr(2)*10.0_r_kind
         ref3d_column(2,ntb)=hdr(3)*10.0_r_kind

         do k=1,numlvl
           ref3d_column(2+k,ntb)=obs(1,k)
         enddo

      enddo loop_sb

!
!  covert BUFR value of missing (-64) and no echo (-63) to cloud analysis
!  value of missing (-999.0) and no echo (-99.0)
!
      DO i=1,numref
      DO k=1,maxlvl
        if( abs(ref3d_column(k+2,i)+64.0_r_kind) .le. 0.00001_r_kind) then
            ref3d_column(k+2,i)=-999.0_r_kind
        elseif( abs(ref3d_column(k+2,i)+63.0_r_kind) .le. 0.00001_r_kind) then
            ref3d_column(k+2,i)=-99.0_r_kind
        endif
      enddo
      enddo

      ilon=ione
      ilat=2
      nread=numref
      ndata=numref
      nreal=maxlvl+2
      if(numref > izero ) then
        write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
        write(lunout) ref3d_column
        deallocate(ref3d_column)
      endif
    endif
!
    return
200 continue
    write(6,*) 'read_NSSL_mosaic, Warning : cannot find radar data file'

end subroutine read_NSSL_mosaic
!
!
