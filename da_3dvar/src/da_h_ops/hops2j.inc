!                                               **************************
!                                               *  module_hops2j.f90     *
!                                               *  PURSER 2001           *
!                                               **************************
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
!MODULE MODULE_hops2j
!CONTAINS
!============================================================================
SUBROUTINE hase2j(a, nhalo,                                                 &
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
IF(nhalo > jts-jms)STOP 'In hase2j; nhalo wider than memory dimension allows'
DO j=1,nhalo
DO i=its,ite
   a(i,jts-j)=a(i,jte+1-j)
ENDDO
ENDDO
END SUBROUTINE hase2j

!============================================================================
SUBROUTINE haes2j(a, nhalo,                                                 &
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
IF(nhalo > jme-jte)STOP 'In haes2j; nhalo wider than memory dimension allows'
DO j=1,nhalo
DO i=its,ite
   a(i,jte+j)=a(i,jts-1+j)
ENDDO
ENDDO
END SUBROUTINE haes2j

!============================================================================
SUBROUTINE hass2j(a, wext,                                                  &
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
  INTEGER                                         :: i,jn,jm
  INTEGER                                         :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
IF(nhalo > jts-jms)STOP 'In hass2j; nhalo wider than memory dimension allows'
DO jn=1,nhalo
   DO i=its,ite
      a(i,jts-jn)=0.
   ENDDO
   DO jm=1,mhalo
   DO i=its,ite
      a(i,jts-jn)=a(i,jts-jn)+wext(jn,jm)*a(i,jts-1+jm)
   ENDDO
   ENDDO
ENDDO
END SUBROUTINE hass2j

!============================================================================
SUBROUTINE haps2j(a,p, wext,                                                &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,                          INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                          INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                          INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),INTENT(IN   ) :: a
  REAL, DIMENSION(ims:, : ),        INTENT(OUT  ) :: p
  REAL, DIMENSION(:,:),             INTENT(IN   ) :: wext
!----------------------------------------------------------------------------
  INTEGER                                         :: i,jn,jm
  INTEGER                                         :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
DO jn=1,nhalo
   DO i=its,ite
      p(i,jn)=0.
   ENDDO
   DO jm=1,mhalo
   DO i=its,ite
      p(i,jn)=p(i,jn)+wext(jn,jm)*a(i,jts-1+jm)
   ENDDO
   ENDDO
ENDDO
END SUBROUTINE haps2j

!============================================================================
SUBROUTINE haee2j(a, wext,                                                  &
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
  INTEGER                                         :: i,jn,jm
  INTEGER                                         :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
IF(nhalo > jme-jte)STOP 'In haee2j; nhalo wider than memory dimension allows'
DO jn=1,nhalo
   DO i=its,ite
      a(i,jte+jn)=0.
   ENDDO
   DO jm=1,mhalo
   DO i=its,ite
      a(i,jte+jn)=a(i,jte+jn)+wext(jn,jm)*a(i,jte+1-jm)
   ENDDO
   ENDDO
ENDDO
END SUBROUTINE haee2j

!============================================================================
SUBROUTINE hape2j(a,p, wext,                                                &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,                          INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                          INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                          INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),INTENT(IN   ) :: a
  REAL, DIMENSION(ims:, : ),        INTENT(OUT  ) :: p

  REAL,DIMENSION(:,:),              INTENT(IN   ) :: wext
!----------------------------------------------------------------------------
  INTEGER                                         :: i,jn,jm
  INTEGER                                         :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
DO jn=1,nhalo
   DO i=its,ite
      p(i,jn)=0.
   ENDDO
   DO jm=1,mhalo
   DO i=its,ite
      p(i,jn)=p(i,jn)+wext(jn,jm)*a(i,jte+1-jm)
   ENDDO
   ENDDO
ENDDO
END SUBROUTINE hape2j

!============================================================================
SUBROUTINE hasp2j(a,p,                                                      &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(ims:, :),          INTENT(IN   ) :: p
!----------------------------------------------------------------------------
  INTEGER                                          :: i,j
  INTEGER                                          :: nhalo
!============================================================================
nhalo=SIZE(p,2)
IF(nhalo > jts-jms)STOP 'In hasp2j; nhalo wider than memory dimension allows'
DO j=1,nhalo
DO i=its,ite
   a(i,jts-j)=p(i,j)
ENDDO
ENDDO
END SUBROUTINE hasp2j

!============================================================================
SUBROUTINE ha11sp2j(a,p,lq1,kq1,                                            &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, 1),       INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,kq1
!----------------------------------------------------------------------------
  INTEGER                                          :: i
!============================================================================
IF(1 > jts-jms)STOP 'In ha11sp2j; nhalo wider than memory dimension allows'
DO i=its,ite
   a(i,jts-1)=p(i,1    )
   p(i,1    )=a(i,jts  )-lq1*p(i,1)
   a(i,jts-1)=a(i,jts-1)+kq1*p(i,1)
ENDDO
END SUBROUTINE ha11sp2j

!============================================================================
SUBROUTINE ha12sp2j(a,p,lq1,kq1,kq2,                                        &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, 1),       INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                                          :: i
!============================================================================
IF(1 > jts-jms)STOP 'In ha12sp2j; nhalo wider than memory dimension allows'
DO i=its,ite
   a(i,jts-1)=p(i,1    )
   p(i,1    )=a(i,jts  )-lq1*p(i,1)
   a(i,jts-1)=a(i,jts-1)+kq1*p(i,1)
   p(i,1    )=a(i,jts+1)-lq1*p(i,1)
   a(i,jts-1)=a(i,jts-1)+kq2*p(i,1)
ENDDO
END SUBROUTINE ha12sp2j

!============================================================================
SUBROUTINE ha21sp2j(a,p,lq1,lq2,kq1,                                        &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, 2),       INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,lq2,kq1
!----------------------------------------------------------------------------
  INTEGER                                          :: i
!============================================================================
IF(2 > jts-jms)STOP 'In ha21sp2j; nhalo wider than memory dimension allows'
DO i=its,ite
   a(i,jts-2)=p(i,2    )+kq1*p(i,1)
   a(i,jts-1)=p(i,1    )
   p(i,2    )=a(i,jts  )-lq1*p(i,1)-lq2*p(i,2)
   a(i,jts-1)=a(i,jts-1)+kq1*p(i,2)
ENDDO
END SUBROUTINE ha21sp2j

!============================================================================
SUBROUTINE ha22sp2j(a,p,lq1,lq2,kq1,kq2,                                    &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, 2),       INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,lq2,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                                          :: i
!============================================================================
IF(2 > jts-jms)STOP 'In ha22sp2j; nhalo wider than memory dimension allows'
DO i=its,ite
   a(i,jts-2)=p(i,2    )+kq1*p(i,1)
   a(i,jts-1)=p(i,1    )
   p(i,2    )=a(i,jts  )-lq1*p(i,1)-lq2*p(i,2)
   a(i,jts-2)=a(i,jts-2)+kq2*p(i,2)
   a(i,jts-1)=a(i,jts-1)+kq1*p(i,2)
   p(i,1    )=a(i,jts+1)-lq1*p(i,2)-lq2*p(i,1)
   a(i,jts-1)=a(i,jts-1)+kq2*p(i,1)
ENDDO
END SUBROUTINE ha22sp2j

!============================================================================
SUBROUTINE haep2j(a,p,                                                      &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(ims:, :),          INTENT(IN   ) :: p
!----------------------------------------------------------------------------
  INTEGER                                          :: i,j
  INTEGER                                          :: nhalo
!============================================================================
nhalo=SIZE(p,2)
IF(nhalo > jme-jte)STOP 'In haep2j; nhalo wider than memory dimension allows'
DO j=1,nhalo
DO i=its,ite
   a(i,jte+j)=p(i,j)
ENDDO
ENDDO
END SUBROUTINE haep2j

!============================================================================
SUBROUTINE ha11ep2j(a,p,lq1,kq1,                                            &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, 1),       INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,kq1
!----------------------------------------------------------------------------
  INTEGER                                          :: i
!============================================================================
IF(1 > jme-jte)STOP 'In ha11ep2j; nhalo wider than memory dimension allows'
DO i=its,ite
   a(i,jte+1)=p(i,1    )
   p(i,1    )=a(i,jte  )-lq1*p(i,1)
   a(i,jte+1)=a(i,jte+1)+kq1*p(i,1)
ENDDO
END SUBROUTINE ha11ep2j

!============================================================================
SUBROUTINE ha12ep2j(a,p,lq1,kq1,kq2,                                        &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, 1),       INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                                          :: i
!============================================================================
IF(1 > jme-jte)STOP 'In ha12ep2j; nhalo wider than memory dimension allows'
DO i=its,ite
   a(i,jte+1)=p(i,1    )
   p(i,1    )=a(i,jte  )-lq1*p(i,1)
   a(i,jte+1)=a(i,jte+1)+kq1*p(i,1)
   p(i,1    )=a(i,jte-1)-lq1*p(i,1)
   a(i,jte+1)=a(i,jte+1)+kq2*p(i,1)
ENDDO
END SUBROUTINE ha12ep2j

!============================================================================
SUBROUTINE ha21ep2j(a,p,lq1,lq2,kq1,                                        &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, 2),       INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,lq2,kq1
!----------------------------------------------------------------------------
  INTEGER                                          :: i
!============================================================================
IF(2 > jme-jte)STOP 'In ha21ep2j; nhalo wider than memory dimension allows'
DO i=its,ite
   a(i,jte+2)=p(i,2    )+kq1*p(i,1)
   a(i,jte+1)=p(i,1    )
   p(i,2    )=a(i,jte  )-lq1*p(i,1)-lq2*p(i,2)
   a(i,jte+1)=a(i,jte+1)+kq1*p(i,2)
ENDDO
END SUBROUTINE ha21ep2j

!============================================================================
SUBROUTINE ha22ep2j(a,p,lq1,lq2,kq1,kq2,                                    &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE
  INTEGER,                           INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER,                           INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER,                           INTENT(IN   ) :: its,ite, jts,jte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, 2),       INTENT(INOUT) :: p
  REAL,                              INTENT(IN   ) :: lq1,lq2,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                                          :: i
!============================================================================
IF(2 > jme-jte)STOP 'In ha22ep2j; nhalo wider than memory dimension allows'
DO i=its,ite
   a(i,jte+2)=p(i,2    )+kq1*p(i,1)
   a(i,jte+1)=p(i,1    )
   p(i,2    )=a(i,jte  )-lq1*p(i,1)-lq2*p(i,2)
   a(i,jte+2)=a(i,jte+2)+kq2*p(i,2)
   a(i,jte+1)=a(i,jte+1)+kq1*p(i,2)
   p(i,1    )=a(i,jte-1)-lq1*p(i,2)-lq2*p(i,1)
   a(i,jte+1)=a(i,jte+1)+kq2*p(i,1)
ENDDO
END SUBROUTINE ha22ep2j

!============================================================================
SUBROUTINE hasz2j(a,  nhalo,                                                &
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
IF(nhalo > jts-jms)STOP 'In hasz2j; nhalo wider than memory dimension allows'
DO j=1,nhalo
DO i=its,ite
   a(i,jts-j)=0.
ENDDO
ENDDO
END SUBROUTINE hasz2j 

!============================================================================
SUBROUTINE haez2j(a,  nhalo,                                                &
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
IF(nhalo > jme-jte)STOP 'In haez2j; nhalo wider than memory dimension allows'
DO j=1,nhalo
DO i=its,ite
   a(i,jte+j)=0.
ENDDO
ENDDO
END SUBROUTINE haez2j 
!============================================================================

!============================================================================
! Horizontal basic numerical operations for 2-dimensional arrays.
! These are the basic building blocks for horizontal compact and
! conventional differencing, quadrature, midpoint interpolation
! and filtering.
!############# 2-dimensions, index-j active:
!============================================================================
SUBROUTINE ud1c2j(c,d,bnd1,                                                 &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme), &
                INTENT(IN   )    :: c                   

  REAL, DIMENSION(ims:ime, jms:jme), &
                INTENT(OUT  )    :: d    
               
  REAL,         INTENT(IN   )    :: bnd1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   d(i,j)=bnd1*(c(i,j+1)-c(i,j-1))
ENDDO
ENDDO
END SUBROUTINE ud1c2j

!============================================================================
SUBROUTINE ud2c2j(c,d,bnd1,bnd2,                                            &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(IN   )    :: c                   

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(OUT  )    :: d    
               
  REAL,         INTENT(IN   )    :: bnd1,bnd2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   d(i,j)=bnd1*(c(i,j+1)-c(i,j-1))+bnd2*(c(i,j+2)-c(i,j-2))
ENDDO
ENDDO
END SUBROUTINE ud2c2j

!============================================================================
SUBROUTINE sa1a2j(a,bnm1,                                                   &
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

  REAL,         INTENT(IN   )    :: bnm1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=bnm1*(a(i,j)+a(i,j+1))
ENDDO
ENDDO
END SUBROUTINE sa1a2j

!============================================================================
SUBROUTINE sq1a2j(a,bnq,                                                    &
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

  REAL,         INTENT(IN   )    :: bnq
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=a(i,j-1)+bnq*a(i,j)
ENDDO
ENDDO
END SUBROUTINE sq1a2j

!============================================================================
SUBROUTINE sd1b2j(a,bnqi,                                                   &
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

  REAL,         INTENT(IN   )    :: bnqi
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jte,jts-1,-1
DO i=its,ite
   a(i,j)=bnqi*(a(i,j)-a(i,j-1))
ENDDO
ENDDO
END SUBROUTINE sd1b2j

!============================================================================
SUBROUTINE mf01a2j(a,knq1,                                                  &
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

  REAL,         INTENT(IN   )    :: knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=a(i,j)+knq1*a(i,j+1)
ENDDO
ENDDO
END SUBROUTINE mf01a2j

!============================================================================
SUBROUTINE mf02a2j(a,knq1,knq2,                                             &
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

  REAL,         INTENT(IN   )    :: knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=a(i,j)+knq1*a(i,j+1)+knq2*a(i,j+2)
ENDDO
ENDDO
END SUBROUTINE mf02a2j

!============================================================================
SUBROUTINE mf10a2j(a,lnq1,                                                  &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime , jms:jme), &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte
DO i=its,ite
   a(i,j)=a(i,j)-lnq1*a(i,j-1)
ENDDO
ENDDO
END SUBROUTINE mf10a2j

!============================================================================
SUBROUTINE mf11a2j(a,lnq1,knq1,                                             &
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
   a(i,j)=a(i,j)-lnq1*a(i,j-1)+knq1*a(i,j+1)
ENDDO
ENDDO
END SUBROUTINE mf11a2j

!============================================================================
SUBROUTINE mf12a2j(a,lnq1,knq1,knq2,                                        &
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
   a(i,j)=a(i,j)-lnq1*a(i,j-1)  &
                +knq1*a(i,j+1)+knq2*a(i,j+2)
ENDDO
ENDDO
END SUBROUTINE mf12a2j

!============================================================================
SUBROUTINE mf20a2j(a,lnq1,lnq2,                                             &
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
   a(i,j)=a(i,j)-lnq1*a(i,j-1)-lnq2*a(i,j-2)
ENDDO
ENDDO
END SUBROUTINE mf20a2j

!============================================================================
SUBROUTINE mf21a2j(a,lnq1,lnq2,knq1,                                        &
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
   a(i,j)=a(i,j)-lnq1*a(i,j-1)-lnq2*a(i,j-2)+knq1*a(i,j+1)
ENDDO
ENDDO
END SUBROUTINE mf21a2j

!============================================================================
SUBROUTINE mf22a2j(a,lnq1,lnq2,knq1,knq2,                                   &
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
   a(i,j)=a(i,j)-lnq1*a(i,j-1)-lnq2*a(i,j-2) &
                +knq1*a(i,j+1)+knq2*a(i,j+2)
ENDDO
ENDDO
END SUBROUTINE mf22a2j

!============================================================================
SUBROUTINE tf1a2j(a,lnq1,                                                   &
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
   a(i,j)=a(i,j)+lnq1*(a(i,j+1)-a(i,j-1))
ENDDO
ENDDO
END SUBROUTINE tf1a2j

!============================================================================
SUBROUTINE tf2a2j(a,lnq1,lnq2,                                              &
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
   a(i,j)=a(i,j)+lnq1*(a(i,j+1)-a(i,j-1))+lnq2*(a(i,j+2)-a(i,j-2))
ENDDO
ENDDO
END SUBROUTINE tf2a2j

!============================================================================
SUBROUTINE ma10a2j(a,p,lnq1,                                                &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(IN   )    :: a

  REAL, DIMENSION(ims:ime, 1),        &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte,2
DO i=its,ite
   p(i,1)=a(i,j)-lnq1*p(i,1)
ENDDO
ENDDO
END SUBROUTINE ma10a2j

!============================================================================
SUBROUTINE ma20a2j(a,p,lnq1,lnq2,                                           &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(IN   )    :: a   

  REAL, DIMENSION(ims:ime, 2),        &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jts,jte,2
DO i=its,ite
   p(i,2)=a(i,j  )-lnq1*p(i,1)-lnq2*p(i,2)
   p(i,1)=a(i,j+1)-lnq1*p(i,2)-lnq2*p(i,1)
ENDDO
ENDDO
END SUBROUTINE ma20a2j

!============================================================================
SUBROUTINE mf01b2j(a,knq1,                                                  &
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

  REAL,         INTENT(IN   )    :: knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jte,jts-1,-1
DO i=its,ite
   a(i,j)=a(i,j)+knq1*a(i,j-1)
ENDDO
ENDDO
END SUBROUTINE mf01b2j

!============================================================================
SUBROUTINE mf02b2j(a,knq1,knq2,                                             &
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

  REAL,         INTENT(IN   )    :: knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jte,jts,-1
DO i=its,ite
   a(i,j)=a(i,j)+knq1*a(i,j-1)+knq2*a(i,j-2)
ENDDO
ENDDO
END SUBROUTINE mf02b2j

!============================================================================
SUBROUTINE mf10b2j(a,lnq1,                                                  &
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
DO j=jte,jts,-1
DO i=its,ite
   a(i,j)=a(i,j)-lnq1*a(i,j+1)
ENDDO
ENDDO
END SUBROUTINE mf10b2j

!============================================================================
SUBROUTINE mf11b2j(a,lnq1,knq1,                                             &
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
DO j=jte,jts,-1
DO i=its,ite
   a(i,j)=a(i,j)-lnq1*a(i,j+1)+knq1*a(i,j-1)
ENDDO
ENDDO
END SUBROUTINE mf11b2j

!============================================================================
SUBROUTINE mf12b2j(a,lnq1,knq1,knq2,                                        &
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
DO j=jte,jts,-1
DO i=its,ite
   a(i,j)=a(i,j)-lnq1*a(i,j+1) &
                +knq1*a(i,j-1)+knq2*a(i,j-2)
ENDDO
ENDDO
END SUBROUTINE mf12b2j

!============================================================================
SUBROUTINE mf20b2j(a,lnq1,lnq2,                                             &
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
DO j=jte,jts,-1
DO i=its,ite
   a(i,j)=a(i,j)-lnq1*a(i,j+1)-lnq2*a(i,j+2)
ENDDO
ENDDO
END SUBROUTINE mf20b2j

!============================================================================
SUBROUTINE mf21b2j(a,lnq1,lnq2,knq1,                                        &
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
DO j=jte,jts,-1
DO i=its,ite
   a(i,j)=a(i,j)-lnq1*a(i,j+1)-lnq2*a(i,j+2)+knq1*a(i,j-1)
ENDDO
ENDDO
END SUBROUTINE mf21b2j

!============================================================================
SUBROUTINE mf22b2j(a,lnq1,lnq2,knq1,knq2,                                   &
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
DO j=jte,jts,-1
DO i=its,ite
   a(i,j)=a(i,j)-lnq1*a(i,j+1)-lnq2*a(i,j+2) &
                +knq1*a(i,j-1)+knq2*a(i,j-2)
ENDDO
ENDDO
END SUBROUTINE mf22b2j

!============================================================================
SUBROUTINE tf1b2j(a,lnq1,                                                   &
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
DO j=jte,jts,-1
DO i=its,ite
   a(i,j)=a(i,j)+lnq1*(a(i,j-1)-a(i,j+1))
ENDDO
ENDDO
END SUBROUTINE tf1b2j

!============================================================================
SUBROUTINE tf2b2j(a,lnq1,lnq2,                                              &
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
DO j=jte,jts,-1
DO i=its,ite
   a(i,j)=a(i,j)+lnq1*(a(i,j-1)-a(i,j+1))+lnq2*(a(i,j-2)-a(i,j+2))
ENDDO
ENDDO
END SUBROUTINE tf2b2j

!============================================================================
SUBROUTINE ma10b2j(a,p,lnq1,                                                &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(IN   )    :: a                    

  REAL, DIMENSION(ims:ime, 1),        &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jte,jts,-1
DO i=its,ite
   p(i,1)=a(i,j)-lnq1*p(i,1)
ENDDO
ENDDO
END SUBROUTINE ma10b2j

!============================================================================
SUBROUTINE ma20b2j(a,p,lnq1,lnq2,                                           &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),  &
                INTENT(IN   )    :: a                    

  REAL, DIMENSION(ims:ime, 2),        &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j
!============================================================================
DO j=jte,jts,-2
DO i=its,ite
   p(i,2)=a(i,j  )-lnq1*p(i,1)-lnq2*p(i,2)
   p(i,1)=a(i,j-1)-lnq1*p(i,2)-lnq2*p(i,1)
ENDDO
ENDDO
END SUBROUTINE ma20b2j   

!============================================================================
! R. J. Purser, jpurser@ncep.noaa.gov
! Intermediate level routines performing various compact operations
! for horizontally cyclic domains.
!============================================================================
SUBROUTINE hc4m2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "4m" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 1)             & 
                         :: p 
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, fl4m)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL haes2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sa1a2j(c,b4m,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl4m)THEN
p=0.
ELSE
CALL haps2j(c,p, w_l_4m,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF
CALL ma10a2j(c,p,l4m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL hasp2j(c,p,                          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10a2j(c,  l4m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl4m)THEN
p=0.
ELSE
CALL hape2j(c,p, w_l_4m,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF
CALL ma10b2j(c,p,l4m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL haep2j(c,p,                          &  
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10b2j(c,  l4m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc4m2j 

!============================================================================
SUBROUTINE hc6m2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "6m" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 1)             & 
                         :: p 
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, fl6m)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL haes2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sa1a2j(c,b6m,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haes2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fl6m)THEN
p=0.
ELSE
CALL haps2j(c,p, w_l_6m,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma10a2j(c,p,l6m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL ha11sp2j(c,p,l6m1,k6m1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11a2j(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl6m)THEN
p=0.
ELSE
CALL hape2j(c,p, w_l_6m,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma10b2j(c,p,l6m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL ha11ep2j(c,p,l6m1,k6m1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11b2j(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc6m2j 

!============================================================================
SUBROUTINE hc8m2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "8m" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 2)             & 
                         :: p 
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=2*MIN((jte+1-jts)/2, fl8m/2)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL haes2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sa1a2j(c,b8m,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haes2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fl8m)THEN
p=0.
ELSE
CALL haps2j(c,p, w_l_8m,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma20a2j(c,p,l8m1,l8m2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL ha21sp2j(c,p,l8m1,l8m2,k8m1,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21a2j(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl8m)THEN
p=0.
ELSE
CALL hape2j(c,p, w_l_8m,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma20b2j(c,p,l8m1,l8m2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL ha21ep2j(c,p,l8m1,l8m2,k8m1,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21b2j(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc8m2j

!============================================================================
SUBROUTINE hc10m2j(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "10m" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 2)             & 
                         :: p 
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=2*MIN((jte+1-jts)/2, fl10m/2)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL haes2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sa1a2j(c,b10m,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haes2j(c, 2,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fl10m)THEN
p=0.
ELSE
CALL haps2j(c,p, w_l_10m,                 &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma20a2j(c,p,l10m1,l10m2,             &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL ha22sp2j(c,p,l10m1,l10m2,k10m1,k10m2,&
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22a2j(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl10m)THEN
p=0.
ELSE
CALL hape2j(c,p, w_l_10m,                 &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma20b2j(c,p,l10m1,l10m2,             &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL ha22ep2j(c,p,l10m1,l10m2,k10m1,k10m2,&
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22b2j(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc10m2j

!============================================================================
SUBROUTINE hc4d2j(c,d,                                                      &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "4d" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 1)             & 
                         :: p 
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, fl4d)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL hase2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL haes2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL ud1c2j(c,d,b4d,                      &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl4d)THEN
p=0.
ELSE
CALL haps2j(d,p, w_l_4d,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma10a2j(d,p,l4d1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL hasp2j(d,p,                          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10a2j(d,  l4d1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl4d)THEN
p=0.
ELSE
CALL hape2j(d,p, w_l_4d,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma10b2j(d,p,l4d1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL haep2j(d,p,                          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10b2j(d,  l4d1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc4d2j 

!============================================================================
SUBROUTINE hc8d2j(c,d,                                                      &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "8d" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 2)             & 
                         :: p 
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=2*MIN((jte+1-jts)/2, fl8d/2)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL hase2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL haes2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL ud1c2j(c,d,b8d,                      &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haes2j(d, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fl8d)THEN
p=0.
ELSE
CALL haps2j(d,p, w_l_8d,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma20a2j(d,p,l8d1,l8d2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL ha21sp2j(d,p,l8d1,l8d2,k8d1,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21a2j(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl8d)THEN
p=0.
ELSE
CALL hape2j(d,p, w_l_8d,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma20b2j(d,p,l8d1,l8d2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL ha21ep2j(d,p,l8d1,l8d2,k8d1,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21b2j(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc8d2j

!============================================================================
SUBROUTINE hc4qi2j(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "4qi" scheme, for 1-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 1)             & 
                         :: p 
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, fl4q)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL hase2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sd1b2j(c,b4qi,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl4q)THEN
p=0.
ELSE
CALL haps2j(c,p, w_l_4q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma10a2j(c,p,l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL hasp2j(c,p,                          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10a2j(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl4q)THEN
p=0.
ELSE
CALL hape2j(c,p, w_l_4q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma10b2j(c,p,l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL haep2j(c,p,                          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10b2j(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc4qi2j

!============================================================================
SUBROUTINE hc4q2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "4q" scheme, for 2-dimensional array, active index j
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
CALL haes2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf01a2j(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hase2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf01b2j(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hasz2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2j(c,b4q,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc4q2j

!============================================================================
SUBROUTINE hc6qi2j(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "6qi" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 1)             & 
                         :: p 
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, fl6q)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL hase2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sd1b2j(c,b6qi,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haes2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fl6q)THEN
p=0.
ELSE
CALL haps2j(c,p, w_l_6q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma10a2j(c,p,l6q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL ha11sp2j(c,p,l6q1,k6q1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11a2j(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl6q)THEN
p=0.
ELSE
CALL hape2j(c,p, w_l_6q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma10b2j(c,p,l6q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL ha11ep2j(c,p,l6q1,k6q1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11b2j(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc6qi2j

!============================================================================
SUBROUTINE hc6q2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "6q" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 1)             & 
                         :: p 
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, fk6q)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL haes2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fk6q)THEN
p=0.
ELSE
CALL haps2j(c,p, w_k_6q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma10a2j(c,p,k6q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL ha11sp2j(c,p,k6q1,l6q1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11a2j(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fk6q)THEN
p=0.
ELSE
CALL hape2j(c,p, w_k_6q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma10b2j(c,p,k6q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL ha11ep2j(c,p,k6q1,l6q1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11b2j(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hasz2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2j(c,b6q,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc6q2j

!============================================================================
SUBROUTINE hc8qi2j(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "8qi" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 2)             & 
                         :: p 
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=2*MIN((jte+1-jts)/2, fl8q/2)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL hase2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sd1b2j(c,b8qi,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haes2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fl8q)THEN
p=0.
ELSE
CALL haps2j(c,p, w_l_8q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma20a2j(c,p,l8q1,l8q2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL ha21sp2j(c,p,l8q1,l8q2,k8q1,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21a2j(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl8q)THEN
p=0.
ELSE
CALL hape2j(c,p, w_l_8q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma20b2j(c,p,l8q1,l8q2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL ha21ep2j(c,p,l8q1,l8q2,k8q1,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21b2j(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc8qi2j

!============================================================================
SUBROUTINE hc8q2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "8q" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 1)                      & 
                         :: p 
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, fk8q)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL haes2j(c, 2,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fk8q)THEN
p=0.
ELSE
CALL haps2j(c,p, w_k_8q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma10a2j(c,p, k8q1,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL ha12sp2j(c,p,k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf12a2j(c,   k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hase2j(c, 1,                         & ! (widen existing halo)
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,jts-1,jte-1)   

IF(f==fk8q)THEN
p=0.
ELSE
CALL hape2j(c,p, w_k_8q,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma10b2j(c,p,k8q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL ha12ep2j(c,p,k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf12b2j(c,  k8q1,l8q1,l8q2,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hasz2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2j(c,b8q,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc8q2j

!============================================================================
SUBROUTINE hc10qi2j(c,                                                      &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "10qi" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 2)                      & 
                         :: p 
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=2*MIN((jte+1-jts)/2, fl10q/2)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL hase2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sd1b2j(c,b10qi,                      &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haes2j(c, 2,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fl10q)THEN
p=0.
ELSE
CALL haps2j(c,p, w_l_10q,                 &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma20a2j(c,p,l10q1,l10q2,             &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL ha22sp2j(c,p,l10q1,l10q2,k10q1,k10q2,&
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22a2j(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fl10q)THEN
p=0.
ELSE
CALL hape2j(c,p, w_l_10q,                 &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma20b2j(c,p,l10q1,l10q2,             &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL ha22ep2j(c,p,l10q1,l10q2,k10q1,k10q2,&
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22b2j(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc10qi2j

!============================================================================
SUBROUTINE hc10q2j(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "10q" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 2)                      & 
                         :: p 
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=2*MIN((jte+1-jts)/2, fk10q/2)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL haes2j(c, 2,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(f==fk10q)THEN
p=0.
ELSE
CALL haps2j(c,p, w_k_10q,                 &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma20a2j(c,p,k10q1,k10q2,             &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL ha22sp2j(c,p,k10q1,k10q2,l10q1,l10q2,&
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22a2j(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(f==fk10q)THEN
p=0.
ELSE
CALL hape2j(c,p, w_k_10q,                 &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma20b2j(c,p,k10q1,k10q2,             &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL ha22ep2j(c,p,k10q1,k10q2,l10q1,l10q2,&
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22b2j(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hasz2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2j(c,b10q,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc10q2j

!============================================================================
SUBROUTINE hc4t2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "4t" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 1)                      & 
                         :: p 
  INTEGER                :: fl,fk,jtfs,jtfe
!============================================================================
fl=MIN(jte+1-jts, fl4t)
fk=MIN(jte+1-jts, fk4t)
jtfs=jte+1-fl
jtfe=jts-1+fk
!------------------
CALL haes2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(fl==fl4t)THEN
p=0.
ELSE
CALL haps2j(c,p, w_l_4t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma10a2j(c,p,l4t1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL ha11sp2j(c,p,l4t1,l4t1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1a2j(c,  l4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(fk==fk4t)THEN
p=0.
ELSE
CALL hape2j(c,p, w_k_4t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma10b2j(c,p,k4t1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL ha11ep2j(c,p,k4t1,k4t1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1b2j(c,  k4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc4t2j

!============================================================================
SUBROUTINE hc4ti2j(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "4ti" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 1)                      & 
                         :: p 
  INTEGER                :: fl,fk,jtfs,jtfe
!============================================================================
fl=MIN(jte+1-jts, fl4t)
fk=MIN(jte+1-jts, fk4t)
jtfs=jte+1-fk
jtfe=jts-1+fl
!------------------
CALL haes2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(fk==fk4t)THEN
p=0.
ELSE
CALL haps2j(c,p, w_k_4t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma10a2j(c,p,k4t1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL ha11sp2j(c,p,k4t1,k4t1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1a2j(c,  k4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(fl==fl4t)THEN
p=0.
ELSE
CALL hape2j(c,p, w_l_4t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma10b2j(c,p,l4t1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL ha11ep2j(c,p,l4t1,l4t1,              &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1b2j(c,  l4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc4ti2j

!============================================================================
SUBROUTINE hc8t2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "8t" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 2)                      & 
                         :: p 
  INTEGER                :: fl,fk,jtfs,jtfe
!============================================================================
fl=2*MIN((jte+1-jts)/2, fl8t/2)
fk=2*MIN((jte+1-jts)/2, fk8t/2)
jtfs=jte+1-fl
jtfe=jts-1+fk
!------------------
CALL haes2j(c, 2,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(fl==fl8t)THEN
p=0.
ELSE
CALL haps2j(c,p, w_l_8t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma20a2j(c,p,l8t1,l8t2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL ha22sp2j(c,p,l8t1,l8t2,l8t1,l8t2,    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2a2j(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(fk==fk8t)THEN
p=0.
ELSE
CALL hape2j(c,p, w_k_8t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma20b2j(c,p,k8t1,k8t2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL ha22ep2j(c,p,k8t1,k8t2,k8t1,k8t2,    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2b2j(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc8t2j

!============================================================================
SUBROUTINE hc8ti2j(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal cyclic "8ti" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 2)                      & 
                         :: p 
  INTEGER                :: fl,fk,jtfs,jtfe
!============================================================================
fl=2*MIN((jte+1-jts)/2, fl8t/2)
fk=2*MIN((jte+1-jts)/2, fk8t/2)
jtfs=jte+1-fk
jtfe=jts-1+fl
!------------------
CALL haes2j(c, 2,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

IF(fk==fk8t)THEN
p=0.
ELSE
CALL haps2j(c,p, w_k_8t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   
ENDIF

CALL ma20a2j(c,p,k8t1,k8t2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite, jtfs,jte)   

CALL ha22sp2j(c,p,k8t1,k8t2,k8t1,k8t2,    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2a2j(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
IF(fl==fl8t)THEN
p=0.
ELSE
CALL hape2j(c,p, w_l_8t,                  &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   
ENDIF

CALL ma20b2j(c,p,l8t1,l8t2,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jtfe)   

CALL ha22ep2j(c,p,l8t1,l8t2,l8t1,l8t2,    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2b2j(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hc8ti2j

!============================================================================
SUBROUTINE hc1rf2j(c, al1,frf,amp,        &       
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)
!============================================================================
! Horizontal cyclic "1rf" scheme, for 2-dimensional array, active index j
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme), &
           INTENT(INOUT) :: c

  REAL,    INTENT(IN   ) :: al1, amp

  INTEGER, INTENT(IN   ) :: frf
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, 1)        &
                         :: p
  REAL                   :: beta
  INTEGER                :: i,j,f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, frf)
jtfs=jte+1-f
jtfe=jts-1+f
beta=(1.+al1)*amp
!------------------
DO j=its,ite
DO i=its,ite
   c(i,j)=beta*c(i,j)
ENDDO
ENDDO

p=0
CALL ma10a2j(c,p,al1,                     &  
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jtfs,jte)

CALL hasp2j(c,p,                          &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)

CALL mf10a2j(c,  al1,                     &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)

p=0
CALL ma10b2j(c,p,al1,                     &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jtfe)

CALL haep2j(c,p,                          &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)

CALL mf10b2j(c,  al1,                     &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)
END SUBROUTINE hc1rf2j

!============================================================================
SUBROUTINE hc2rf2j(c,al1,al2,frf,amp,     &       
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)
!============================================================================
! Horizontal cyclic "2rf" scheme, for 2-dimensional array, active index j
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
  REAL, DIMENSION(ims:ime, 2) &
                         :: p
  REAL                   :: beta
  INTEGER                :: i,j,f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, frf)
jtfs=jte+1-f
jtfe=jts-1+f
beta=(1.+al1+al2)*amp
!------------------
DO j=its,ite
DO i=its,ite
   c(i,j)=beta*c(i,j)
ENDDO
ENDDO

p=0
CALL ma20a2j(c,p,al1,al2,                 &  
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jtfs,jte)
   
CALL hasp2j(c,p,                          &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)

CALL mf20a2j(c,  al1,al2,                 &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)

p=0
CALL ma20b2j(c,p,al1,al2,                 &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jtfe)

CALL haep2j(c,p,                          &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)

CALL mf20b2j(c,  al1,al2,                 &
       ids,ide, jds,jde,                  &
       ims,ime, jms,jme,                  &
       its,ite, jts,jte)
END SUBROUTINE hc2rf2j

!============================================================================
!  R. J. Purser, jpurser@ncep.noaa.gov
! Intermediate level routines performing various compact operations
! for horizontally bounded domains.
!============================================================================
SUBROUTINE hg4m2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "4m" scheme, for 2-dimensional array, active index j
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
!============================================================================
!------------------
CALL sa1a2j(c,b4m,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hass2j(c, w_l_4m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10a2j(c,  l4m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wll_4m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10b2j(c,  l4m1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg4m2j 

!============================================================================
SUBROUTINE hg6m2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "6m" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL sa1a2j(c,b6m,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wk__6m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2j(c, w_lk6m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11a2j(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wllk6m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11b2j(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg6m2j 

!============================================================================
SUBROUTINE hg8m2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "8m" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL sa1a2j(c,b8m,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wk__8m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2j(c, w_lk8m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21a2j(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wllk8m,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21b2j(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg8m2j

!============================================================================
SUBROUTINE hg10m2j(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "10m" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL sa1a2j(c,b10m,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wk__10m,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2j(c, w_lk10m,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22a2j(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wllk10m,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22b2j(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg10m2j

!============================================================================
SUBROUTINE hg4d2j(c,d,                                                      &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "4d" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL hass2j(c, wi__4d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL haee2j(c, wi__4d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL ud1c2j(c,d,b4d,                      &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hass2j(d, w_l_4d,           &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10a2j(d,  l4d1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(d, wll_4d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10b2j(d,  l4d1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg4d2j 

!============================================================================
SUBROUTINE hg8d2j(c,d,                                                      &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "8d" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL hass2j(c, wi__8d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL haee2j(c, wi__8d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL ud1c2j(c,d,b8d,                      &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(d, wk__8d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2j(d, w_lk8d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21a2j(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(d, wllk8d,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21b2j(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg8d2j

!============================================================================
SUBROUTINE hg4qi2j(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "4qi" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL sd1b2j(c,b4qi,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hass2j(c, w_l_4q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10a2j(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wll_4q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf10b2j(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg4qi2j

!============================================================================
SUBROUTINE hg4q2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "4q" scheme, for 2-dimensional array, active index j
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
CALL haee2j(c, wl__4q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf01a2j(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hass2j(c, w___4q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf01b2j(c,  l4q1,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hasz2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2j(c,b4q,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg4q2j

!============================================================================
SUBROUTINE hg6qi2j(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "6qi" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL sd1b2j(c,b6qi,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wk__6q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2j(c, w_lk6q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11a2j(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wllk6q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11b2j(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg6qi2j

!============================================================================
SUBROUTINE hg6q2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "6q" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL haee2j(c, wl__6q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2j(c, w_kl6q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11a2j(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wkkl6q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf11b2j(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hasz2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2j(c,b6q,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg6q2j

!============================================================================
SUBROUTINE hg8qi2j(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "8qi" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL sd1b2j(c,b8qi,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wk__8q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2j(c, w_lk8q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21a2j(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wllk8q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf21b2j(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg8qi2j

!============================================================================
SUBROUTINE hg8q2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "8q" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL haee2j(c, wl__8q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2j(c, w_kl8q,                    & ! (2-wide halo for later b-filter) 
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf12a2j(c,   k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wkkl8q,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf12b2j(c,  k8q1,l8q1,l8q2,          &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hasz2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2j(c,b8q,                        &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg8q2j

!============================================================================
SUBROUTINE hg10qi2j(c,                                                      &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "10qi" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL sd1b2j(c,b10qi,                      &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wk__10q,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2j(c, w_lk10q,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22a2j(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wllk10q,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22b2j(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg10qi2j

!============================================================================
SUBROUTINE hg10q2j(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "10q" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL haee2j(c, wl__10q,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2j(c, w_kl10q,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22a2j(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wkkl10q,                   &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL mf22b2j(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL hasz2j(c, 1,                         &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL sq1a2j(c,b10q,                       &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg10q2j

!============================================================================
SUBROUTINE hg4t2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "4t" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL haee2j(c, wk__4t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2j(c, w_ll4t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1a2j(c,  l4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wlkk4t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1b2j(c,  k4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg4t2j

!============================================================================
SUBROUTINE hg4ti2j(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "4ti" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL haee2j(c, wl__4t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2j(c, w_kk4t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1a2j(c,  k4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wkll4t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf1b2j(c,  l4t1,                     &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg4ti2j

!============================================================================
SUBROUTINE hg8t2j(c,                                                        &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "8t" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL haee2j(c, wk__8t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2j(c, w_ll8t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2a2j(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wlkk8t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2b2j(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg8t2j

!============================================================================
SUBROUTINE hg8ti2j(c,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal "8ti" scheme, for 2-dimensional array, active index j
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
!============================================================================
CALL haee2j(c, wl__8t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL hass2j(c, w_kk8t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2a2j(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
!------------------
CALL haee2j(c, wkll8t,                    &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   

CALL tf2b2j(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,                   &
     ims,ime,  jms,jme,                   &
     its,ite,  jts,jte)   
END SUBROUTINE hg8ti2j

!END MODULE MODULE_hops2j
