  subroutine write_bufr_lightning(maxlvl,nlon,nlat,numref,lightning_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_bufr_lightning
!   prgmmr: hu           org: essl/gsd                date: 2008-12-01
!   
! abstract: write NSSL mosaic reflectivity in RR grid into bufr
!   
! program history log:
!   2008-12-01  Hu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  linux 
!
!$$$
    use constants, only: zero, one
    use kinds, only: r_kind,i_kind
    implicit none

    REAL(r_kind) :: lightning_out(maxlvl+2,nlon*nlat)   
    real(r_kind) :: hdr(5),obs(1,35)
    character(80):: hdrstr='SID XOB YOB DHR TYP'
    character(80):: obsstr='POB'

    REAL(i_kind),PARAMETER ::  MXBF = 160000_i_kind
    INTEGER(i_kind) :: ibfmsg = MXBF/4_i_kind

    character(8) subset,sid
    integer(i_kind) :: ludx,lendian_in,idate

    INTEGER(i_kind)  ::  maxlvl,nlon,nlat
    INTEGER(i_kind)  ::  numlvl,numref
    INTEGER(i_kind)  ::  i,n,k,iret


    idate=2008120100
    subset='ADPUPA'
    sid='LIGHTNI'
    ludx=22
    lendian_in=10

    open(ludx,file='prepobs_prep.bufrtable',action='read')
    open(lendian_in,file='LightningInGSI.bufr',action='write',form='unformatted')

    call datelen(10)
    call openbf(lendian_in,'OUT',ludx)
    do n=1,numref
      hdr(1)=transfer(sid,hdr(1))
      hdr(2)=lightning_out(1,n)/10.0_r_kind
      hdr(3)=lightning_out(2,n)/10.0_r_kind
      hdr(4)=0
      hdr(5)=500

      do k=1,maxlvl
        obs(1,k)=lightning_out(2+k,n)
      enddo
      call openmb(lendian_in,subset,idate)
      call ufbint(lendian_in,hdr,5,   1,iret,hdrstr)
      call ufbint(lendian_in,obs,1,maxlvl,iret,obsstr)
      call writsb(lendian_in,ibfmsg,iret)
!      write(6,*) 'write_bufr_nsslref,1st: write BUFR message ibfmsg(1:',iret,') to local system'
    enddo
    call closbf(lendian_in)
    write(6,*) 'write_bufr_nsslref, DONE: write columns:',numref

end subroutine write_bufr_lightning
