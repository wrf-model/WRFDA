program write_bufr_test
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_bufr_nsslref
!   prgmmr: hu           org: essl/gsd                date: 2008-12-01
!   
! abstract: write NSSL mosaic reflectivity in RR grid into bufr
!   
! program history log:
!   2008-12-01  kleist
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
    use kinds, only: r_kind,i_kind
    implicit none

    INTEGER(i_kind), PARAMETER ::  maxlvl=31
    INTEGER(i_kind), PARAMETER ::  nlon=4
    INTEGER(i_kind), PARAMETER ::  nlat=3
    INTEGER(i_kind), PARAMETER ::  numref=10
    INTEGER(i_kind)  ::  i,k

    REAL(r_kind) :: ref3d_column(maxlvl+2,nlon*nlat)   


    ref3d_column=0
    DO i=1,numref
       ref3d_column(1,i)=1.0*10.0 + i
       ref3d_column(2,i)=2.0*10.0 + i
       DO k=1,maxlvl
         ref3d_column(k+2,i) = k*100.0 + i
       ENDDO
    ENDDO
    call write_bufr_nsslref(maxlvl,nlon,nlat,numref,ref3d_column)
    write(*,*) ' End of build bufr'

end
