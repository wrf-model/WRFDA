PROGRAM readbufr

    use constants, only: zero, one
    use kinds, only: r_kind,i_kind
    implicit none

    real(r_kind) :: hdr(5),obs(1,35)
    character(80):: hdrstr='SID XOB YOB DHR TYP'
    character(80):: obsstr='HREF'

    REAL(i_kind),PARAMETER ::  MXBF = 160000_i_kind
    INTEGER(i_kind) :: ibfmsg = MXBF/4_i_kind

    character(8) subset,sid
    character(80) infile
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

!*-----------------------------------------------------------------------

  lunin = 23
  infile='bufr_nsslref'
  infile='NSSLRefInGSI.bufr'
  OPEN  ( UNIT = lunin, FILE = trim(infile),form='unformatted' )
  CALL OPENBF  ( lunin, 'IN', lunin )

!*	Specify that we would like IDATE values returned using 10 digits
!*	(i.e. YYYYMMDDHH ).
  CALL DATELEN  ( 10 )

  call readmg(lunin,subset,idate,iret)
  write(*,*) subset,idate,iret

  nmsg=1
  ntb = 0

  loop_report: do
         call readsb(lunin,iret)
         if(iret/=0) then
                call readmg(lunin,subset,idate,iret)
                if(iret/=0) then
                        exit loop_report ! end of file
                else
                  nmsg=nmsg+1
                  if (nmsg>nmsgmax) then
                        write(6,*)'READ_PREPBUFR: messages exceed maximum ',nmsgmax
                        stop
                  endif
                endif
                cycle loop_report
         else
                nrep(nmsg)=nrep(nmsg)+1  ! count reports per message 
         endif
!    Extract type, date, and location information
         call ufbint(lunin,hdr,5,1,iret,hdrstr)
         if( ntb .eq. 0 ) then
           call ufbint(lunin,obs,1,35,iret,obsstr)
           numlvl=iret
         endif

        ntb = ntb+1
        rid=hdr(1)
!        write(*,*) ntb, sid, (hdr(k),k=2,5)
  enddo loop_report
 
  write(6,*)'READ_RADARBUFR: messages/reports = ',nmsg,'/',ntb
  write(6,*)'READ_RADARBUFR: levels = ',numlvl
  allocate(ref3d_column(numlvl+2,ntb))

  call closbf(lunin)
  OPEN  ( UNIT = lunin, FILE = trim(infile),form='unformatted' )
  CALL OPENBF  ( lunin, 'IN', lunin )
  CALL DATELEN  ( 10 )

  call readmg(lunin,subset,idate,iret)

  nmsg=1
  ntb = 0
  loop_sb: do
         call readsb(lunin,iret)
         if(iret/=0) then
                call readmg(lunin,subset,idate,iret)
                if(iret/=0) then
                        exit loop_sb ! end of file
                else
                  nmsg=nmsg+1
                  if (nmsg>nmsgmax) then
                        write(6,*)'READ_PREPBUFR: messages exceed maximum ',nmsgmax
                        stop
                  endif
                endif
                cycle loop_sb
         else
                nrep(nmsg)=nrep(nmsg)+1  ! count reports per message
         endif
!    Extract type, date, and location information
         call ufbint(lunin,hdr,5,1,iret,hdrstr)
         call ufbint(lunin,obs,1,35,iret,obsstr)
         numlvl=iret

         ntb = ntb+1
         ref3d_column(1,ntb)=hdr(2)*10.0
         ref3d_column(2,ntb)=hdr(3)*10.0

         do k=1,numlvl
           ref3d_column(2+k,ntb)=obs(1,k)
         enddo

  enddo loop_sb


  write(*,*) (ref3d_column(k,100),k=1,numlvl+2)
  write(*,*) (ref3d_column(k,500),k=1,numlvl+2)
  write(*,*) (ref3d_column(k,1000),k=1,numlvl+2)
  END

