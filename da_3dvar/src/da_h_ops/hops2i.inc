!						*************************
!						*  module_hops2i.f90 	*
!						*  PURSER 2001          *
! 						*************************
!
!============================================================================
!  R. J. Purser, jpurser@ncep.noaa.gov
!  Routines to fill horizontal haloes in various ways
!
!  Let "s" signify start of tile, "e" end of tile.
!  hase.. : fill halo at s from interior data at e assuming cyclic conditions
!  haes.. : fill halo at e from interior data at s assuming cyclic conditions
!  hass.. : fill halo at s from interior data at s assuming extrapolation
!  haps.. : fill provided array p from extrapolated interior data at s
!  haee.. : fill halo at e from interior data at e assuming extrapolation
!  hape.. : fill provided array p from extrapolated interior data at e
!  hasp.. : fill halo at s from a provided array p
!  haep.. : fill halo at e from a provided array p
!  hasz.. : fill halo at s with zeroes
!  haez.. : fill halo at e with zeroes
!============================================================================
!MODULE MODULE_hops2i
!CONTAINS

!============================================================================
SUBROUTINE hase2i(a, nhalo,                                                 &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: nhalo
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
!----------------------------------------------------------------------------
  INTEGER                                          :: i,j
!============================================================================
IF(nhalo > its-ims)STOP 'In hase2i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO i=1,nhalo
   a(its-i,j)=a(ite+1-i,j)
ENDDO
ENDDO
END SUBROUTINE hase2i
!============================================================================
SUBROUTINE haes2i(a, nhalo,                                                 &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: nhalo
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
!----------------------------------------------------------------------------
  INTEGER                                          :: i,j
!============================================================================
IF(nhalo > ime-ite)STOP 'In haes2i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO i=1,nhalo
   a(ite+i,j)=a(its-1+i,j)
ENDDO
ENDDO
END SUBROUTINE haes2i

!============================================================================
SUBROUTINE hass2i(a, wext,                                                  &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,                          INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                          INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                          INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),INTENT(INOUT) :: a

  REAL, DIMENSION(:,:),             INTENT(IN   ) :: wext
!----------------------------------------------------------------------------
  INTEGER                                         :: in,im,j
  INTEGER                                         :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
IF(nhalo > its-ims)STOP 'In hass2i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO in=1,nhalo
   a(its-in,j)=0.
   DO im=1,mhalo
      a(its-in,j)=a(its-in,j)+wext(in,im)*a(its-1+im,j)
   ENDDO
ENDDO
ENDDO
END SUBROUTINE hass2i

!============================================================================
SUBROUTINE haps2i(a,p, wext,                                                &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  
  INTEGER,                          INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                          INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                          INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),INTENT(IN   ) :: a
  REAL, DIMENSION(:,       jms:   ),INTENT(OUT  ) :: p
  REAL, DIMENSION(:,:),             INTENT(IN   ) :: wext
!----------------------------------------------------------------------------
  INTEGER                                         :: in,im,j
  INTEGER                                         :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
DO j=jts,jte
DO in=1,nhalo
   p(in,j)=0.
   DO im=1,mhalo
      p(in,j)=p(in,j)+wext(in,im)*a(its-1+im,j)
   ENDDO
ENDDO
ENDDO
END SUBROUTINE haps2i

!============================================================================
SUBROUTINE haee2i(a, wext,                                                  &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,                          INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                          INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                          INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),INTENT(INOUT) :: a

  REAL,DIMENSION(:,:),              INTENT(IN   ) :: wext
!----------------------------------------------------------------------------
  INTEGER                                         :: in,im,j
  INTEGER                                         :: nhalo,mhalo  
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
IF(nhalo > ime-ite)STOP 'In haee2i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO in=1,nhalo
   a(ite+in,j)=0.
   DO im=1,mhalo
      a(ite+in,j)=a(ite+in,j)+wext(in,im)*a(ite+1-im,j)
   ENDDO
ENDDO
ENDDO
END SUBROUTINE haee2i

!============================================================================
SUBROUTINE hape2i(a,p, wext,                                                &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,                          INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                          INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                          INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),INTENT(IN   ) :: a
  REAL, DIMENSION(:,       jms:   ),INTENT(OUT  ) :: p

  REAL, DIMENSION(:,:),             INTENT(IN   ) :: wext
!----------------------------------------------------------------------------
  INTEGER                                         :: in,im,j
  INTEGER                                         :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
DO j=jts,jte
DO in=1,nhalo
   p(in,j)=0.
   DO im=1,mhalo
      p(in,j)=p(in,j)+wext(in,im)*a(ite+1-im,j)
   ENDDO
ENDDO
ENDDO
END SUBROUTINE hape2i

!============================================================================
SUBROUTINE hasp2i(a,p,                                                      &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(:,       jms:   ), INTENT(IN   ) :: p
!----------------------------------------------------------------------------
  INTEGER                                          :: i,j
  INTEGER                                          :: nhalo
!============================================================================
nhalo=SIZE(p,1)
IF(nhalo > its-ims)STOP 'In hasp2i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO i=1,nhalo
   a(its-i,j)=p(i,j)
ENDDO
ENDDO
END SUBROUTINE hasp2i

!============================================================================
SUBROUTINE ha11sp2i(a,p,lq1,kq1,                                            &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(1,       jms:jme), INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,kq1
!----------------------------------------------------------------------------
  INTEGER                                          :: j
!============================================================================
IF(1 > its-ims)STOP 'In ha11sp2i; nhalo wider than memory dimension allows'
DO j=jts,jte
   a(its-1,j)=p(1    ,j)
   p(1    ,j)=a(its  ,j)-lq1*p(1,j)
   a(its-1,j)=a(its-1,j)+kq1*p(1,j)
ENDDO
END SUBROUTINE ha11sp2i

!============================================================================
SUBROUTINE ha12sp2i(a,p,lq1,kq1,kq2,                                        &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(1,       jms:jme), INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                                          :: j
!============================================================================
IF(1 > its-ims)STOP 'In ha12sp2i; nhalo wider than memory dimension allows'
DO j=jts,jte
   a(its-1,j)=p(1,    j)
   p(1,    j)=a(its,  j)-lq1*p(1,j)
   a(its-1,j)=a(its-1,j)+kq1*p(1,j)
   p(1,    j)=a(its+1,j)-lq1*p(1,j)
   a(its-1,j)=a(its-1,j)+kq2*p(1,j)
ENDDO
END SUBROUTINE ha12sp2i

!============================================================================
SUBROUTINE ha21sp2i(a,p,lq1,lq2,kq1,                                        &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(2,       jms:jme), INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,lq2,kq1
!----------------------------------------------------------------------------
  INTEGER                                          :: j
!============================================================================
IF(2 > its-ims)STOP 'In ha21sp2i; nhalo wider than memory dimension allows'
DO j=jts,jte
   a(its-2,j)=p(2    ,j)+kq1*p(1,j)
   a(its-1,j)=p(1    ,j)
   p(2    ,j)=a(its  ,j)-lq1*p(1,j)-lq2*p(2,j)
   a(its-1,j)=a(its-1,j)+kq1*p(2,j)
ENDDO
END SUBROUTINE ha21sp2i

!============================================================================
SUBROUTINE ha22sp2i(a,p,lq1,lq2,kq1,kq2,                                    &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(2,       jms:jme), INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,lq2,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                                          :: j
!============================================================================
IF(2 > its-ims)STOP 'In ha22sp2i; nhalo wider than memory dimension allows'
DO j=jts,jte
   a(its-2,j)=p(2    ,j)+kq1*p(1,j)
   a(its-1,j)=p(1    ,j)
   p(2    ,j)=a(its  ,j)-lq1*p(1,j)-lq2*p(2,j)
   a(its-2,j)=a(its-2,j)+kq2*p(2,j)
   a(its-1,j)=a(its-1,j)+kq1*p(2,j)
   p(1    ,j)=a(its+1,j)-lq1*p(2,j)-lq2*p(1,j)
   a(its-1,j)=a(its-1,j)+kq2*p(1,j)
ENDDO
END SUBROUTINE ha22sp2i

!============================================================================
SUBROUTINE haep2i(a,p,                                                      &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(:,       jms:   ), INTENT(IN   ) :: p
!----------------------------------------------------------------------------
  INTEGER                                          :: i,j
  INTEGER                                          :: nhalo
!============================================================================
nhalo=SIZE(p,1)
IF(nhalo > ime-ite)STOP 'In haep2i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO i=1,nhalo
   a(ite+i,j)=p(i,j)
ENDDO
ENDDO
END SUBROUTINE haep2i

!============================================================================
SUBROUTINE ha11ep2i(a,p,lq1,kq1,                                            &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(1,       jms:jme), INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,kq1
!----------------------------------------------------------------------------
  INTEGER                                          :: j
!============================================================================
IF(1 > ime-ite)STOP 'In ha11ep2i; nhalo wider than memory dimension allows'
DO j=jts,jte
   a(ite+1,j)=p(1    ,j)
   p(1    ,j)=a(ite  ,j)-lq1*p(1,j)
   a(ite+1,j)=a(ite+1,j)+kq1*p(1,j)
ENDDO
END SUBROUTINE ha11ep2i

!============================================================================
SUBROUTINE ha12ep2i(a,p,lq1,kq1,kq2,                                        &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(1,       jms:jme), INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                                          :: j
!============================================================================
IF(1 > ime-ite)STOP 'In ha12ep2i; nhalo wider than memory dimension allows'
DO j=jts,jte
   a(ite+1,j)=p(1    ,j)
   p(1    ,j)=a(ite  ,j)-lq1*p(1,j)
   a(ite+1,j)=a(ite+1,j)+kq1*p(1,j)
   p(1    ,j)=a(ite-1,j)-lq1*p(1,j)
   a(ite+1,j)=a(ite+1,j)+kq2*p(1,j)
ENDDO
END SUBROUTINE ha12ep2i

!============================================================================
SUBROUTINE ha21ep2i(a,p,lq1,lq2,kq1,                                        &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(2,       jms:jme), INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,lq2,kq1
!----------------------------------------------------------------------------
  INTEGER                                          :: j
!============================================================================
IF(2 > ime-ite)STOP 'In ha21ep2i; nhalo wider than memory dimension allows'
DO j=jts,jte
   a(ite+2,j)=p(2    ,j)+kq1*p(1,j)
   a(ite+1,j)=p(1    ,j)
   p(2    ,j)=a(ite  ,j)-lq1*p(1,j)-lq2*p(2,j)
   a(ite+1,j)=a(ite+1,j)+kq1*p(2,j)
ENDDO
END SUBROUTINE ha21ep2i

!============================================================================
SUBROUTINE ha22ep2i(a,p,lq1,lq2,kq1,kq2,                                    &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(2,       jms:jme), INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,lq2,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                                          :: j
!============================================================================
IF(2 > ime-ite)STOP 'In ha22ep2i; nhalo wider than memory dimension allows'
DO j=jts,jte
   a(ite+2,j)=p(2    ,j)+kq1*p(1,j)
   a(ite+1,j)=p(1    ,j)
   p(2    ,j)=a(ite  ,j)-lq1*p(1,j)-lq2*p(2,j)
   a(ite+2,j)=a(ite+2,j)+kq2*p(2,j)
   a(ite+1,j)=a(ite+1,j)+kq1*p(2,j)
   p(1    ,j)=a(ite-1,j)-lq1*p(2,j)-lq2*p(1,j)
   a(ite+1,j)=a(ite+1,j)+kq2*p(1,j)
ENDDO
END SUBROUTINE ha22ep2i

!============================================================================
SUBROUTINE hasz2i(a,  nhalo,                                                &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: nhalo
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
!----------------------------------------------------------------------------
  INTEGER                                          :: i,j
!============================================================================
IF(nhalo > its-ims)STOP 'In hasz2i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO i=1,nhalo
   a(its-i,j)=0.
ENDDO
ENDDO
END SUBROUTINE hasz2i

!============================================================================
SUBROUTINE haez2i(a,  nhalo,                                                &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: nhalo
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
!----------------------------------------------------------------------------
  INTEGER                                          :: i,j
!============================================================================
IF(nhalo > ime-ite)STOP 'In haez2i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO i=1,nhalo
   a(ite+i,j)=0.
ENDDO
ENDDO
END SUBROUTINE haez2i
!============================================================================

!============================================================================
!  R. J. Purser, jpurser@ncep.noaa.gov
!============================================================================
! Horizontal basic numerical operations for 2-dimensional arrays.
! These are the basic building blocks for horizontal compact and
! conventional differencing, quadrature, midpoint interpolation
! and filtering.

!############# 2-dimensions, index-i active:
!============================================================================
SUBROUTINE ud1c2i(c,d,bnd1,                                                 &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(IN   )    :: c                   

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(OUT  )    :: d    
               
  REAL,         INTENT(IN   )    :: bnd1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   d(i,j)=bnd1*(c(i+1,j)-c(i-1,j))
ENDDO
ENDDO
END SUBROUTINE ud1c2i

!============================================================================
SUBROUTINE ud2c2i(c,d,bnd1,bnd2,                                            &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(IN   )    :: c                   

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(OUT  )    :: d    
               
  REAL,         INTENT(IN   )    :: bnd1,bnd2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   d(i,j)=bnd1*(c(i+1,j)-c(i-1,j))+bnd2*(c(i+2,j)-c(i-2,j))
ENDDO
ENDDO
END SUBROUTINE ud2c2i

!============================================================================
SUBROUTINE sa1a2i(a,bnm1,                                                   &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: bnm1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=bnm1*(a(i,j)+a(i+1,j))
ENDDO
ENDDO
END SUBROUTINE sa1a2i

!============================================================================
SUBROUTINE sq1a2i(a,bnq,                                                    &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: bnq
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=a(i-1,j)+bnq*a(i,j)
ENDDO
ENDDO
END SUBROUTINE sq1a2i

!============================================================================
SUBROUTINE sd1b2i(a,bnqi,                                                   &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: bnqi
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=ite,its,-1
   a(i,j)=bnqi*(a(i,j)-a(i-1,j))
ENDDO
ENDDO
END SUBROUTINE sd1b2i

!============================================================================
SUBROUTINE mf01a2i(a,knq1,                                                  &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=a(i,j)+knq1*a(i+1,j)
ENDDO
ENDDO
END SUBROUTINE mf01a2i

!============================================================================
SUBROUTINE mf02a2i(a,knq1,knq2,                                             &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=a(i,j)+knq1*a(i+1,j)+knq2*a(i+2,j)
ENDDO
ENDDO
END SUBROUTINE mf02a2i

!============================================================================
SUBROUTINE mf10a2i(a,lnq1,                                                  &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=a(i,j)-lnq1*a(i-1,j)
ENDDO
ENDDO
END SUBROUTINE mf10a2i

!============================================================================
SUBROUTINE mf11a2i(a,lnq1,knq1,                                             &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=a(i,j)-lnq1*a(i-1,j)+knq1*a(i+1,j)
ENDDO
ENDDO
END SUBROUTINE mf11a2i

!============================================================================
SUBROUTINE mf12a2i(a,lnq1,knq1,knq2,                                        &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=a(i,j)-lnq1*a(i-1,j)  &
                +knq1*a(i+1,j)+knq2*a(i+2,j)
ENDDO
ENDDO
END SUBROUTINE mf12a2i

!============================================================================
SUBROUTINE mf20a2i(a,lnq1,lnq2,                                             &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=a(i,j)-lnq1*a(i-1,j)-lnq2*a(i-2,j)
ENDDO
ENDDO
END SUBROUTINE mf20a2i

!============================================================================
SUBROUTINE mf21a2i(a,lnq1,lnq2,knq1,                                        &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2,knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=a(i,j)-lnq1*a(i-1,j)-lnq2*a(i-2,j)+knq1*a(i+1,j)
ENDDO
ENDDO
END SUBROUTINE mf21a2i

!============================================================================
SUBROUTINE mf22a2i(a,lnq1,lnq2,knq1,knq2,                                   &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2,knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=a(i,j)-lnq1*a(i-1,j)-lnq2*a(i-2,j) &
                +knq1*a(i+1,j)+knq2*a(i+2,j)
ENDDO
ENDDO
END SUBROUTINE mf22a2i

!============================================================================
SUBROUTINE tf1a2i(a,lnq1,                                                   &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=a(i,j)+lnq1*(a(i+1,j)-a(i-1,j))
ENDDO
ENDDO
END SUBROUTINE tf1a2i

!============================================================================
SUBROUTINE tf2a2i(a,lnq1,lnq2,                                              &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=a(i,j)+lnq1*(a(i+1,j)-a(i-1,j))+lnq2*(a(i+2,j)-a(i-2,j))
ENDDO
ENDDO
END SUBROUTINE tf2a2i

!============================================================================
SUBROUTINE ma10a2i(a,p,lnq1,                                                &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(IN   )    :: a

  REAL, DIMENSION(1, jms:jme),         &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   p(1,j)=a(i,j)-lnq1*p(1,j)
ENDDO
ENDDO
END SUBROUTINE ma10a2i

!============================================================================
SUBROUTINE ma20a2i(a,p,lnq1,lnq2,                                           &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(IN   )    :: a   

  REAL, DIMENSION(2, jms:jme),         &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite,2
   p(2,j)=a(i  ,j)-lnq1*p(1,j)-lnq2*p(2,j)
   p(1,j)=a(i+1,j)-lnq1*p(2,j)-lnq2*p(1,j)
ENDDO
ENDDO
END SUBROUTINE ma20a2i

!============================================================================
SUBROUTINE mf01b2i(a,knq1,                                                  &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=ite,its,-1
   a(i,j)=a(i,j)+knq1*a(i-1,j)
ENDDO
ENDDO
END SUBROUTINE mf01b2i

!============================================================================
SUBROUTINE mf02b2i(a,knq1,knq2,                                             &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=ite,its,-1
   a(i,j)=a(i,j)+knq1*a(i-1,j)+knq2*a(i-2,j)
ENDDO
ENDDO
END SUBROUTINE mf02b2i

!============================================================================
SUBROUTINE mf10b2i(a,lnq1,                                                  &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=ite,its,-1
   a(i,j)=a(i,j)-lnq1*a(i+1,j)
ENDDO
ENDDO
END SUBROUTINE mf10b2i

!============================================================================
SUBROUTINE mf11b2i(a,lnq1,knq1,                                             &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=ite,its,-1
   a(i,j)=a(i,j)-lnq1*a(i+1,j)+knq1*a(i-1,j)
ENDDO
ENDDO
END SUBROUTINE mf11b2i

!============================================================================
SUBROUTINE mf12b2i(a,lnq1,knq1,knq2,                                        &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=ite,its,-1
   a(i,j)=a(i,j)-lnq1*a(i+1,j)  &
                +knq1*a(i-1,j)+knq2*a(i-2,j)
ENDDO
ENDDO
END SUBROUTINE mf12b2i

!============================================================================
SUBROUTINE mf20b2i(a,lnq1,lnq2,                                             &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=ite,its,-1
   a(i,j)=a(i,j)-lnq1*a(i+1,j)-lnq2*a(i+2,j)
ENDDO
ENDDO
END SUBROUTINE mf20b2i

!============================================================================
SUBROUTINE mf21b2i(a,lnq1,lnq2,knq1,                                        &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2,knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=ite,its,-1
   a(i,j)=a(i,j)-lnq1*a(i+1,j)-lnq2*a(i+2,j)+knq1*a(i-1,j)
ENDDO
ENDDO
END SUBROUTINE mf21b2i

!============================================================================
SUBROUTINE mf22b2i(a,lnq1,lnq2,knq1,knq2,                                   &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2,knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=ite,its,-1
   a(i,j)=a(i,j)-lnq1*a(i+1,j)-lnq2*a(i+2,j) &
                +knq1*a(i-1,j)+knq2*a(i-2,j)
ENDDO
ENDDO
END SUBROUTINE mf22b2i

!============================================================================
SUBROUTINE tf1b2i(a,lnq1,                                                   &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=ite,its,-1
   a(i,j)=a(i,j)+lnq1*(a(i-1,j)-a(i+1,j))
ENDDO
ENDDO
END SUBROUTINE tf1b2i

!============================================================================
SUBROUTINE tf2b2i(a,lnq1,lnq2,                                              &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=ite,its,-1
   a(i,j)=a(i,j)+lnq1*(a(i-1,j)-a(i+1,j))+lnq2*(a(i-2,j)-a(i+2,j))
ENDDO
ENDDO
END SUBROUTINE tf2b2i

!============================================================================
SUBROUTINE ma10b2i(a,p,lnq1,                                                &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(IN   )    :: a                    

  REAL, DIMENSION(1, jms:jme),         &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=ite,its,-1
   p(1,j)=a(i,j)-lnq1*p(1,j)
ENDDO
ENDDO
END SUBROUTINE ma10b2i

!============================================================================
SUBROUTINE ma20b2i(a,p,lnq1,lnq2,                                           &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),   &
                INTENT(IN   )    :: a 

  REAL, DIMENSION(2, jms:jme),         &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=ite,its,-2
   p(2,j)=a(i  ,j)-lnq1*p(1,j)-lnq2*p(2,j)
   p(1,j)=a(i-1,j)-lnq1*p(2,j)-lnq2*p(1,j)
ENDDO
ENDDO
END SUBROUTINE ma20b2i
!============================================================================

!============================================================================
!  R. J. Purser, jpurser@ncep.noaa.gov
! Intermediate level routines performing various compact operations
! for horizontally cyclic domains.
!============================================================================
!============================================================================
SUBROUTINE hc4m2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "4m" scheme, for 2-dimensional array, active index i
! (4th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fl4m)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sa1a2i(c,b4m,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl4m)THEN
p=0.
ELSE
CALL haps2i(c,p, w_l_4m,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma10a2i(c,p,l4m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL hasp2i(c,p,                          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10a2i(c,  l4m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl4m)THEN
p=0.
ELSE
CALL hape2i(c,p, w_l_4m,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma10b2i(c,p,l4m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL haep2i(c,p,                          &  
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10b2i(c,  l4m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc4m2i 

!============================================================================
SUBROUTINE hc6m2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "6m" scheme, for 2-dimensional array, active index i
! (6th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fl6m)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sa1a2i(c,b6m,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haes2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fl6m)THEN
p=0.
ELSE
CALL haps2i(c,p, w_l_6m,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma10a2i(c,p,l6m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL ha11sp2i(c,p,l6m1,k6m1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11a2i(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl6m)THEN
p=0.
ELSE
CALL hape2i(c,p, w_l_6m,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma10b2i(c,p,l6m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL ha11ep2i(c,p,l6m1,k6m1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11b2i(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc6m2i 

!============================================================================
SUBROUTINE hc8m2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "8m" scheme, for 2-dimensional array, active index i
! (8th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fl8m/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sa1a2i(c,b8m,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haes2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fl8m)THEN
p=0.
ELSE
CALL haps2i(c,p, w_l_8m,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma20a2i(c,p,l8m1,l8m2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL ha21sp2i(c,p,l8m1,l8m2,k8m1,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21a2i(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl8m)THEN
p=0.
ELSE
CALL hape2i(c,p, w_l_8m,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma20b2i(c,p,l8m1,l8m2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL ha21ep2i(c,p,l8m1,l8m2,k8m1,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21b2i(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc8m2i

!============================================================================
SUBROUTINE hc10m2i(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "10m" scheme, for 2-dimensional array, active index i
! (10th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fl10m/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sa1a2i(c,b10m,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haes2i(c, 2,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fl10m)THEN
p=0.
ELSE
CALL haps2i(c,p, w_l_10m,                 &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma20a2i(c,p,l10m1,l10m2,             &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL ha22sp2i(c,p,l10m1,l10m2,k10m1,k10m2,&
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22a2i(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl10m)THEN
p=0.
ELSE
CALL hape2i(c,p, w_l_10m,                 &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma20b2i(c,p,l10m1,l10m2,             &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL ha22ep2i(c,p,l10m1,l10m2,k10m1,k10m2,&
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22b2i(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc10m2i

!============================================================================
SUBROUTINE hc4d2i(c,d,                                                      &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "4d" scheme, for 2-dimensional array, active index i
! (4th-order differencing on a simple grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(INOUT) :: c
  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(OUT  ) :: d
!----------------------------------------------------------------------------
  REAL, DIMENSION(1, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fl4d)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL haes2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL ud1c2i(c,d,b4d,                      &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl4d)THEN
p=0.
ELSE
CALL haps2i(d,p, w_l_4d,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma10a2i(d,p,l4d1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL hasp2i(d,p,                          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10a2i(d,  l4d1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl4d)THEN
p=0.
ELSE
CALL hape2i(d,p, w_l_4d,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma10b2i(d,p,l4d1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL haep2i(d,p,                          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10b2i(d,  l4d1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc4d2i 

!============================================================================
SUBROUTINE hc8d2i(c,d,                                                      &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "8d" scheme, for 2-dimensional array, active index i
! (8th-order differencing on a simple grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(INOUT) :: c
  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(OUT  ) :: d
!----------------------------------------------------------------------------
  REAL, DIMENSION(2, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fl8d/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL haes2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL ud1c2i(c,d,b8d,                      &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haes2i(d, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fl8d)THEN
p=0.
ELSE
CALL haps2i(d,p, w_l_8d,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma20a2i(d,p,l8d1,l8d2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL ha21sp2i(d,p,l8d1,l8d2,k8d1,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21a2i(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl8d)THEN
p=0.
ELSE
CALL hape2i(d,p, w_l_8d,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma20b2i(d,p,l8d1,l8d2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL ha21ep2i(d,p,l8d1,l8d2,k8d1,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21b2i(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc8d2i

!============================================================================
SUBROUTINE hc4qi2i(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "4qi" scheme, for 1-dimensional array, active index i
! (4th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fl4q)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sd1b2i(c,b4qi,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl4q)THEN
p=0.
ELSE
CALL haps2i(c,p, w_l_4q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma10a2i(c,p,l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL hasp2i(c,p,                          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10a2i(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl4q)THEN
p=0.
ELSE
CALL hape2i(c,p, w_l_4q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma10b2i(c,p,l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL haep2i(c,p,                          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10b2i(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc4qi2i

!============================================================================
SUBROUTINE hc4q2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "4q" scheme, for 2-dimensional array, active index i
! (4th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haes2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf01a2i(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hase2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf01b2i(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hasz2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2i(c,b4q,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc4q2i

!============================================================================
SUBROUTINE hc6qi2i(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "6qi" scheme, for 2-dimensional array, active index i
! (6th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fl6q)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sd1b2i(c,b6qi,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haes2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fl6q)THEN
p=0.
ELSE
CALL haps2i(c,p, w_l_6q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma10a2i(c,p,l6q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL ha11sp2i(c,p,l6q1,k6q1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11a2i(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl6q)THEN
p=0.
ELSE
CALL hape2i(c,p, w_l_6q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma10b2i(c,p,l6q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL ha11ep2i(c,p,l6q1,k6q1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11b2i(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc6qi2i

!============================================================================
SUBROUTINE hc6q2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "6q" scheme, for 2-dimensional array, active index i
! (6th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),         &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1, jms:jme)                &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fk6q)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fk6q)THEN
p=0.
ELSE
CALL haps2i(c,p, w_k_6q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma10a2i(c,p,k6q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL ha11sp2i(c,p,k6q1,l6q1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11a2i(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fk6q)THEN
p=0.
ELSE
CALL hape2i(c,p, w_k_6q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma10b2i(c,p,k6q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL ha11ep2i(c,p,k6q1,l6q1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11b2i(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hasz2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2i(c,b6q,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc6q2i

!============================================================================
SUBROUTINE hc8qi2i(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "8qi" scheme, for 2-dimensional array, active index i
! (8th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme), &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2, jms:jme)        &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fl8q/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sd1b2i(c,b8qi,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haes2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fl8q)THEN
p=0.
ELSE
CALL haps2i(c,p, w_l_8q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma20a2i(c,p,l8q1,l8q2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL ha21sp2i(c,p,l8q1,l8q2,k8q1,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21a2i(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl8q)THEN
p=0.
ELSE
CALL hape2i(c,p, w_l_8q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma20b2i(c,p,l8q1,l8q2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL ha21ep2i(c,p,l8q1,l8q2,k8q1,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21b2i(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc8qi2i

!============================================================================
SUBROUTINE hc8q2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "8q" scheme, for 2-dimensional array, active index i
! (8th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme), &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1, jms:jme)        &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fk8q)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes2i(c, 2,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fk8q)THEN
p=0.
ELSE
CALL haps2i(c,p, w_k_8q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma10a2i(c,p, k8q1,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL ha12sp2i(c,p,k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf12a2i(c,   k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hase2i(c, 1,                         & ! (widen existing halo)
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its-1,ite-1, jts,jte)   

IF(f==fk8q)THEN
p=0.
ELSE
CALL hape2i(c,p, w_k_8q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma10b2i(c,p,k8q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL ha12ep2i(c,p,k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf12b2i(c,  k8q1,l8q1,l8q2,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hasz2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2i(c,b8q,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc8q2i

!============================================================================
SUBROUTINE hc10qi2i(c,                                                      &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "10qi" scheme, for 2-dimensional array, active index i
! (10th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fl10q/2)
itfs=ite+1-f
itfe=its-1+f
CALL hase2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sd1b2i(c,b10qi,                      &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

!------------------
CALL haes2i(c, 2,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fl10q)THEN
p=0.
ELSE
CALL haps2i(c,p, w_l_10q,                 &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma20a2i(c,p,l10q1,l10q2,             &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL ha22sp2i(c,p,l10q1,l10q2,k10q1,k10q2,&
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22a2i(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl10q)THEN
p=0.
ELSE
CALL hape2i(c,p, w_l_10q,                 &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma20b2i(c,p,l10q1,l10q2,             &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL ha22ep2i(c,p,l10q1,l10q2,k10q1,k10q2,&
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22b2i(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc10qi2i

!============================================================================
SUBROUTINE hc10q2i(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "10q" scheme, for 2-dimensional array, active index i
! (10th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme), &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2, jms:jme)        &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fk10q/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes2i(c, 2,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fk10q)THEN
p=0.
ELSE
CALL haps2i(c,p, w_k_10q,                 &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma20a2i(c,p,k10q1,k10q2,             &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL ha22sp2i(c,p,k10q1,k10q2,l10q1,l10q2,&
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22a2i(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fk10q)THEN
p=0.
ELSE
CALL hape2i(c,p, w_k_10q,                 &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma20b2i(c,p,k10q1,k10q2,             &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL ha22ep2i(c,p,k10q1,k10q2,l10q1,l10q2,&
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22b2i(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hasz2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2i(c,b10q,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc10q2i

!============================================================================
SUBROUTINE hc4t2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "4t" scheme, for 2-dimensional array, active index i
! (4th-order invertible translation for A to C grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1, jms:jme)                      &
                         :: p
  INTEGER                :: fl,fk,itfs,itfe
!============================================================================
fl=MIN(ite+1-its, fl4t)
fk=MIN(ite+1-its, fk4t)
itfs=ite+1-fl
itfe=its-1+fk
!------------------
CALL haes2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(fl==fl4t)THEN
p=0.
ELSE
CALL haps2i(c,p, w_l_4t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma10a2i(c,p,l4t1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL ha11sp2i(c,p,l4t1,l4t1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1a2i(c,  l4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(fk==fk4t)THEN
p=0.
ELSE
CALL hape2i(c,p, w_k_4t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma10b2i(c,p,k4t1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL ha11ep2i(c,p,k4t1,k4t1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1b2i(c,  k4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc4t2i

!============================================================================
SUBROUTINE hc4ti2i(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "4ti" scheme, for 2-dimensional array, active index i
! (4th-order invertible translation for C to A grid; inverse of 4t)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1, jms:jme)                      &
                         :: p
  INTEGER                :: fl,fk,itfs,itfe
!============================================================================
fl=MIN(ite+1-its, fl4t)
fk=MIN(ite+1-its, fk4t)
itfs=ite+1-fk
itfe=its-1+fl
!------------------
CALL haes2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(fk==fk4t)THEN
p=0.
ELSE
CALL haps2i(c,p, w_k_4t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma10a2i(c,p,k4t1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL ha11sp2i(c,p,k4t1,k4t1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1a2i(c,  k4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(fl==fl4t)THEN
p=0.
ELSE
CALL hape2i(c,p, w_l_4t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma10b2i(c,p,l4t1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL ha11ep2i(c,p,l4t1,l4t1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1b2i(c,  l4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc4ti2i

!============================================================================
SUBROUTINE hc8t2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "8t" scheme, for 2-dimensional array, active index i
! (8th-order invertible translation for A to C grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2, jms:jme)                      &
                         :: p
  INTEGER                :: fl,fk,itfs,itfe
!============================================================================
fl=2*MIN((ite+1-its)/2, fl8t/2)
fk=2*MIN((ite+1-its)/2, fk8t/2)
itfs=ite+1-fl
itfe=its-1+fk
!------------------
CALL haes2i(c, 2,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(fl==fl8t)THEN
p=0.
ELSE
CALL haps2i(c,p, w_l_8t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma20a2i(c,p,l8t1,l8t2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL ha22sp2i(c,p,l8t1,l8t2,l8t1,l8t2,    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2a2i(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(fk==fk8t)THEN
p=0.
ELSE
CALL hape2i(c,p, w_k_8t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma20b2i(c,p,k8t1,k8t2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL ha22ep2i(c,p,k8t1,k8t2,k8t1,k8t2,    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2b2i(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc8t2i

!============================================================================
SUBROUTINE hc8ti2i(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "8ti" scheme, for 2-dimensional array, active index i
! (4th-order invertible translation for C to A grid; inverse of 8t)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2, jms:jme)                      &
                         :: p
  INTEGER                :: fl,fk,itfs,itfe
!============================================================================
fl=2*MIN((ite+1-its)/2, fl8t/2)
fk=2*MIN((ite+1-its)/2, fk8t/2)
itfs=ite+1-fk
itfe=its-1+fl
!------------------
CALL haes2i(c, 2,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(fk==fk8t)THEN
p=0.
ELSE
CALL haps2i(c,p, w_k_8t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   
ENDIF

CALL ma20a2i(c,p,k8t1,k8t2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     itfs,ite, jts,jte)   

CALL ha22sp2i(c,p,k8t1,k8t2,k8t1,k8t2,    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2a2i(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(fl==fl8t)THEN
p=0.
ELSE
CALL hape2i(c,p, w_l_8t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   
ENDIF

CALL ma20b2i(c,p,l8t1,l8t2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,itfe, jts,jte)   

CALL ha22ep2i(c,p,l8t1,l8t2,l8t1,l8t2,    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2b2i(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc8ti2i

!============================================================================
SUBROUTINE hc1rf2i(c, al1,frf,amp,        &       
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)
!============================================================================
! Horizontal cyclic "1rf" scheme, for 2-dimensional array, active index i
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c

  REAL,    INTENT(IN   ) :: al1, amp

  INTEGER, INTENT(IN   ) :: frf
!----------------------------------------------------------------------------
  REAL, DIMENSION(1, jms:jme)             &
                         :: p
  REAL                   :: beta
  INTEGER                :: i,j,f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, frf)
itfs=ite+1-f
itfe=its-1+f
beta=(1.+al1)*amp
!------------------
DO j=its,ite
DO i=its,ite
   c(i,j)=beta*c(i,j)
ENDDO
ENDDO

p=0
CALL ma10a2i(c,p,al1,                     &  
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       itfs,ite, jts,jte)

CALL hasp2i(c,p,                          &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)

CALL mf10a2i(c,  al1,                     &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)

p=0
CALL ma10b2i(c,p,al1,                     &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,itfe, jts,jte)

CALL haep2i(c,p,                          &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)

CALL mf10b2i(c,  al1,                     &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)
END SUBROUTINE hc1rf2i

!============================================================================
SUBROUTINE hc2rf2i(c,al1,al2,frf,amp,     &       
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)
!============================================================================
! Horizontal cyclic "2rf" scheme, for 2-dimensional array, active index i
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c

  REAL,    INTENT(IN   ) :: al1,al2, amp

  INTEGER, INTENT(IN   ) :: frf
!----------------------------------------------------------------------------
  REAL, DIMENSION(2, jms:jme) &
                         :: p
  REAL                   :: beta
  INTEGER                :: i,j,f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, frf)
itfs=ite+1-f
itfe=its-1+f
beta=(1.+al1+al2)*amp
!------------------
DO j=its,ite
DO i=its,ite
   c(i,j)=beta*c(i,j)
ENDDO
ENDDO

p=0
CALL ma20a2i(c,p,al1,al2,                 &  
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       itfs,ite, jts,jte)

CALL hasp2i(c,p,                          &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)

CALL mf20a2i(c,  al1,al2,                 &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)

p=0
CALL ma20b2i(c,p,al1,al2,                 &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,itfe, jts,jte)

CALL haep2i(c,p,                          &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)

CALL mf20b2i(c,  al1,al2,                 &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)
END SUBROUTINE hc2rf2i


!============================================================================
!
!============================================================================
!  R. J. Purser, jpurser@ncep.noaa.gov
! Intermediate level routines performing various compact operations
! for horizontally bounded domains.
!============================================================================
!============================================================================
SUBROUTINE hg4m2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "4m" scheme, for 2-dimensional array, active index i
! (4th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
!------------------
CALL sa1a2i(c,b4m,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hass2i(c, w_l_4m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10a2i(c,  l4m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wll_4m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10b2i(c,  l4m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg4m2i 

!============================================================================
SUBROUTINE hg6m2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "6m" scheme, for 2-dimensional array, active index i
! (6th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sa1a2i(c,b6m,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wk__6m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2i(c, w_lk6m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11a2i(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wllk6m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11b2i(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg6m2i 

!============================================================================
SUBROUTINE hg8m2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "8m" scheme, for 2-dimensional array, active index i
! (8th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sa1a2i(c,b8m,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wk__8m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2i(c, w_lk8m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21a2i(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wllk8m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21b2i(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg8m2i

!============================================================================
SUBROUTINE hg10m2i(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "10m" scheme, for 2-dimensional array, active index i
! (10th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sa1a2i(c,b10m,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wk__10m,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2i(c, w_lk10m,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22a2i(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wllk10m,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22b2i(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg10m2i

!============================================================================
SUBROUTINE hg4d2i(c,d,                                                      &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "4d" scheme, for 2-dimensional array, active index i
! (4th-order differencing on a simple grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(OUT  ) :: d
!----------------------------------------------------------------------------
!============================================================================
CALL hass2i(c, wi__4d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL haee2i(c, wi__4d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL ud1c2i(c,d,b4d,                      &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hass2i(d, w_l_4d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10a2i(d,  l4d1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(d, wll_4d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10b2i(d,  l4d1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg4d2i 

!============================================================================
SUBROUTINE hg8d2i(c,d,                                                      &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "8d" scheme, for 2-dimensional array, active index i
! (8th-order differencing on a simple grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(OUT  ) :: d
!----------------------------------------------------------------------------
!============================================================================
CALL hass2i(c, wi__8d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL haee2i(c, wi__8d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL ud1c2i(c,d,b8d,                      &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(d, wk__8d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2i(d, w_lk8d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21a2i(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(d, wllk8d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21b2i(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg8d2i

!============================================================================
SUBROUTINE hg4qi2i(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "4qi" scheme, for 2-dimensional array, active index i
! (4th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sd1b2i(c,b4qi,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hass2i(c, w_l_4q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10a2i(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wll_4q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10b2i(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg4qi2i

!============================================================================
SUBROUTINE hg4q2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "4q" scheme, for 2-dimensional array, active index i
! (4th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee2i(c, wl__4q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf01a2i(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hass2i(c, w___4q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf01b2i(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hasz2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2i(c,b4q,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg4q2i

!============================================================================
SUBROUTINE hg6qi2i(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "6qi" scheme, for 2-dimensional array, active index i
! (6th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sd1b2i(c,b6qi,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wk__6q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2i(c, w_lk6q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11a2i(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wllk6q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11b2i(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg6qi2i

!============================================================================
SUBROUTINE hg6q2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "6q" scheme, for 2-dimensional array, active index i
! (6th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee2i(c, wl__6q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2i(c, w_kl6q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11a2i(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wkkl6q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11b2i(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hasz2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2i(c,b6q,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg6q2i

!============================================================================
SUBROUTINE hg8qi2i(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "8qi" scheme, for 2-dimensional array, active index i
! (8th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sd1b2i(c,b8qi,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wk__8q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2i(c, w_lk8q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21a2i(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wllk8q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21b2i(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg8qi2i

!============================================================================
SUBROUTINE hg8q2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "8q" scheme, for 2-dimensional array, active index i
! (8th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee2i(c, wl__8q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2i(c, w_kl8q,                    & ! (2-wide halo for later b-filter) 
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf12a2i(c,   k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wkkl8q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf12b2i(c,  k8q1,l8q1,l8q2,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hasz2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2i(c,b8q,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg8q2i

!============================================================================
SUBROUTINE hg10qi2i(c,                                                      &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "10qi" scheme, for 2-dimensional array, active index i
! (10th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sd1b2i(c,b10qi,                      &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wk__10q,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2i(c, w_lk10q,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22a2i(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wllk10q,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22b2i(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg10qi2i

!============================================================================
SUBROUTINE hg10q2i(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "10q" scheme, for 2-dimensional array, active index i
! (10th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee2i(c, wl__10q,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2i(c, w_kl10q,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22a2i(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wkkl10q,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22b2i(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hasz2i(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2i(c,b10q,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg10q2i

!============================================================================
SUBROUTINE hg4t2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "4t" scheme, for 2-dimensional array, active index i
! (4th-order invertible translation for A to C grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee2i(c, wk__4t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2i(c, w_ll4t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1a2i(c,  l4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wlkk4t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1b2i(c,  k4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg4t2i

!============================================================================
SUBROUTINE hg4ti2i(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "4ti" scheme, for 2-dimensional array, active index i
! (4th-order invertible translation for C to A grid; inverse of 4t)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee2i(c, wl__4t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2i(c, w_kk4t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1a2i(c,  k4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wkll4t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1b2i(c,  l4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg4ti2i

!============================================================================
SUBROUTINE hg8t2i(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "8t" scheme, for 2-dimensional array, active index i
! (8th-order invertible translation for A to C grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee2i(c, wk__8t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2i(c, w_ll8t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2a2i(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wlkk8t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2b2i(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg8t2i

!============================================================================
SUBROUTINE hg8ti2i(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "8ti" scheme, for 2-dimensional array, active index i
! (4th-order invertible translation for C to A grid; inverse of 8t)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),      &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee2i(c, wl__8t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2i(c, w_kk8t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2a2i(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2i(c, wkll8t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2b2i(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg8ti2i

!END MODULE MODULE_hops2i
