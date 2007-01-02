!						*************************
!						*  module_hops3j.f90	*
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

!MODULE MODULE_hops3j
!CONTAINS

!============================================================================
SUBROUTINE hase3j(a, nhalo,                                                 &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE
  INTEGER,    INTENT(IN   ) :: nhalo
  INTEGER,    INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,    INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,    INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
              INTENT(INOUT) :: a
!----------------------------------------------------------------------------
  INTEGER                   :: i,k,j
!============================================================================
IF(nhalo > jts-jms)STOP 'In hase3j; nhalo wider than memory dimension allows'
DO j=1,nhalo
DO k=kts,kte
DO i=its,ite
   a(i,k,jts-j)=a(i,k,jte+1-j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE hase3j

!============================================================================
SUBROUTINE haes3j(a, nhalo,                                                 &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE
  INTEGER,    INTENT(IN   ) :: nhalo
  INTEGER,    INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,    INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,    INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
              INTENT(INOUT) :: a
!----------------------------------------------------------------------------
  INTEGER                   :: i,k,j
!============================================================================
IF(nhalo > jme-jte)STOP 'In haes3j; nhalo wider than memory dimension allows'
DO j=1,nhalo
DO k=kts,kte
DO i=its,ite
   a(i,k,jte+j)=a(i,k,jts-1+j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE haes3j

!============================================================================
SUBROUTINE hass3j(a, wext,                                                  &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),      &
                INTENT(INOUT)    :: a                    

  REAL, DIMENSION(:,:),                    &      
                INTENT(IN   )    :: wext
!----------------------------------------------------------------------------
  INTEGER                        :: i,jn,jm,k
  INTEGER                        :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
IF(nhalo > jts-jms)STOP 'In hass3j; nhalo wider than memory dimension allows'
DO jn=1,nhalo
   DO k=kts,kte
   DO i=its,ite
      a(i,k,jts-jn)=0.
   ENDDO
   DO jm=1,mhalo
   DO i=its,ite
      a(i,k,jts-jn)=a(i,k,jts-jn)+wext(jn,jm)*a(i,k,jts-1+jm)
   ENDDO
   ENDDO
   ENDDO
ENDDO
END SUBROUTINE hass3j

!============================================================================
SUBROUTINE haps3j(a,p, wext,                                                &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),      &
                INTENT(IN   )    :: a                    
  REAL, DIMENSION(ims:   , kms:   , :      ),      &
                INTENT(OUT  )    :: p

  REAL, DIMENSION(:,:),                            &      
                INTENT(IN   )    :: wext
!----------------------------------------------------------------------------
  INTEGER                        :: i,jn,jm,k
  INTEGER                        :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
DO jn=1,nhalo
   DO k=kts,kte
   DO i=its,ite
      p(i,k,jn)=0.
   ENDDO
   DO jm=1,mhalo
   DO i=its,ite
      p(i,k,jn)=p(i,k,jn)+wext(jn,jm)*a(i,k,jts-1+jm)
   ENDDO
   ENDDO
   ENDDO
ENDDO
END SUBROUTINE haps3j

!============================================================================
SUBROUTINE haee3j(a, wext,                                                  &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),      &
                INTENT(INOUT)    :: a                    
  REAL,DIMENSION(:,:),                             &
                INTENT(IN   )    :: wext
!----------------------------------------------------------------------------
  INTEGER                        :: i,jn,jm,k
  INTEGER                        :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
IF(nhalo > jme-jte)STOP 'In haee3j; nhalo wider than memory dimension allows'
DO jn=1,nhalo
   DO k=kts,kte
   DO i=its,ite
      a(i,k,jte+jn)=0.
   ENDDO
   DO jm=1,mhalo
   DO i=its,ite
      a(i,k,jte+jn)=a(i,k,jte+jn)+wext(jn,jm)*a(i,k,jte+1-jm)
   ENDDO
   ENDDO
   ENDDO
ENDDO
END SUBROUTINE haee3j

!============================================================================
SUBROUTINE hape3j(a,p, wext,                                                &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),      &
                INTENT(IN   )    :: a                    
  REAL, DIMENSION(ims:, kms:, :),                  &
                INTENT(OUT  )    :: p

  REAL,DIMENSION(:,:),                             &
                INTENT(IN   )    :: wext
!----------------------------------------------------------------------------
  INTEGER                        :: i,jn,jm,k
  INTEGER                        :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
DO jn=1,nhalo
   DO k=kts,kte
   DO i=its,ite
      p(i,k,jn)=0.
   ENDDO
   DO jm=1,mhalo
   DO i=its,ite
      p(i,k,jn)=p(i,k,jn)+wext(jn,jm)*a(i,k,jte+1-jm)
   ENDDO
   ENDDO
   ENDDO
ENDDO
END SUBROUTINE hape3j

!============================================================================
SUBROUTINE hasp3j(a,p,                                                      &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE
  INTEGER,    INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,    INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,    INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
              INTENT(INOUT) :: a
  REAL, DIMENSION(ims:, kms:, :),             &
              INTENT(IN   ) :: p
!----------------------------------------------------------------------------
  INTEGER                   :: i,k,j
  INTEGER                   :: nhalo
!============================================================================
nhalo=SIZE(p,3)
IF(nhalo > jts-jms)STOP 'In hasp3j; nhalo wider than memory dimension allows'
DO j=1,nhalo
DO k=kts,kte
DO i=its,ite
   a(i,k,jts-j)=p(i,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE hasp3j

!============================================================================
SUBROUTINE ha11sp3j(a,p,lq1,kq1,                                            &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE
  INTEGER,    INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,    INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,    INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
              INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, kms:kme, 1),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,kq1
!----------------------------------------------------------------------------
  INTEGER                   :: i,k
!============================================================================
IF(1 > jts-jms)STOP 'In ha11sp3j; nhalo wider than memory dimension allows'
DO k=kts,kte
DO i=its,ite
   a(i,k,jts-1)=p(i,k,1    )
   p(i,k,1    )=a(i,k,jts  )-lq1*p(i,k,1)
   a(i,k,jts-1)=a(i,k,jts-1)+kq1*p(i,k,1)
ENDDO
ENDDO
END SUBROUTINE ha11sp3j

!============================================================================
SUBROUTINE ha12sp3j(a,p,lq1,kq1,kq2,                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE
  INTEGER,    INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,    INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,    INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
              INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, kms:kme, 1),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                   :: i,k
!============================================================================
IF(1 > jts-jms)STOP 'In ha12sp3j; nhalo wider than memory dimension allows'
DO k=kts,kte
DO i=its,ite
   a(i,k,jts-1)=p(i,k,1    )
   p(i,k,1    )=a(i,k,jts  )-lq1*p(i,k,1)
   a(i,k,jts-1)=a(i,k,jts-1)+kq1*p(i,k,1)
   p(i,k,1    )=a(i,k,jts+1)-lq1*p(i,k,1)
   a(i,k,jts-1)=a(i,k,jts-1)+kq2*p(i,k,1)
ENDDO
ENDDO
END SUBROUTINE ha12sp3j

!============================================================================
SUBROUTINE ha21sp3j(a,p,lq1,lq2,kq1,                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE
  INTEGER,    INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,    INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,    INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
              INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, kms:kme, 2),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,lq2,kq1
!----------------------------------------------------------------------------
  INTEGER                   :: i,k
!============================================================================
IF(2 > jts-jms)STOP 'In ha21sp3j; nhalo wider than memory dimension allows'
DO k=kts,kte
DO i=its,ite
   a(i,k,jts-2)=p(i,k,2    )+kq1*p(i,k,1)
   a(i,k,jts-1)=p(i,k,1    )
   p(i,k,2    )=a(i,k,jts  )-lq1*p(i,k,1)-lq2*p(i,k,2)
   a(i,k,jts-1)=a(i,k,jts-1)+kq1*p(i,k,2)
ENDDO
ENDDO
END SUBROUTINE ha21sp3j

!============================================================================
SUBROUTINE ha22sp3j(a,p,lq1,lq2,kq1,kq2,                                    &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE
  INTEGER,    INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,    INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,    INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
              INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, kms:kme, 2),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,lq2,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                   :: i,k
!============================================================================
IF(2 > jts-jms)STOP 'In ha22sp3j; nhalo wider than memory dimension allows'
DO k=kts,kte
DO i=its,ite
   a(i,k,jts-2)=p(i,k,2    )+kq1*p(i,k,1)
   a(i,k,jts-1)=p(i,k,1    )
   p(i,k,2    )=a(i,k,jts  )-lq1*p(i,k,1)-lq2*p(i,k,2)
   a(i,k,jts-2)=a(i,k,jts-2)+kq2*p(i,k,2)
   a(i,k,jts-1)=a(i,k,jts-1)+kq1*p(i,k,2)
   p(i,k,1    )=a(i,k,jts+1)-lq1*p(i,k,2)-lq2*p(i,k,1)
   a(i,k,jts-1)=a(i,k,jts-1)+kq2*p(i,k,1)
ENDDO
ENDDO
END SUBROUTINE ha22sp3j

!============================================================================
SUBROUTINE haep3j(a,p,                                                      &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE
  INTEGER,    INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,    INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,    INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
              INTENT(INOUT) :: a
  REAL, DIMENSION(ims:   , kms:, :),          &
              INTENT(IN   ) :: p
!----------------------------------------------------------------------------
  INTEGER                   :: i,k,j
  INTEGER                   :: nhalo
!============================================================================
nhalo=SIZE(p,3)
IF(nhalo > jme-jte)STOP 'In haep3j; nhalo wider than memory dimension allows'
DO j=1,nhalo
DO k=kts,kte
DO i=its,ite
   a(i,k,jte+j)=p(i,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE haep3j

!============================================================================
SUBROUTINE ha11ep3j(a,p,lq1,kq1,                                            &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE
  INTEGER,    INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,    INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,    INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
              INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, kms:kme, 1),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,kq1
!----------------------------------------------------------------------------
  INTEGER                   :: i,k
!============================================================================
IF(1 > jme-jte)STOP 'In ha11ep3j; nhalo wider than memory dimension allows'
DO k=kts,kte
DO i=its,ite
   a(i,k,jte+1)=p(i,k,1    )
   p(i,k,    1)=a(i,k,  jte)-lq1*p(i,k,1)
   a(i,k,jte+1)=a(i,k,jte+1)+kq1*p(i,k,1)
ENDDO
ENDDO
END SUBROUTINE ha11ep3j

!============================================================================
SUBROUTINE ha12ep3j(a,p,lq1,kq1,kq2,                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE
  INTEGER,    INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,    INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,    INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
              INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, kms:kme, 1),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                   :: i,k
!============================================================================
IF(1 > jme-jte)STOP 'In ha12ep3j; nhalo wider than memory dimension allows'
DO k=kts,kte
DO i=its,ite
   a(i,k,jte+1)=p(i,k,1    )
   p(i,k,1    )=a(i,k,jte  )-lq1*p(i,k,1)
   a(i,k,jte+1)=a(i,k,jte+1)+kq1*p(i,k,1)
   p(i,k,1    )=a(i,k,jte-1)-lq1*p(i,k,1)
   a(i,k,jte+1)=a(i,k,jte+1)+kq2*p(i,k,1)
ENDDO
ENDDO
END SUBROUTINE ha12ep3j

!============================================================================
SUBROUTINE ha21ep3j(a,p,lq1,lq2,kq1,                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE
  INTEGER,    INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,    INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,    INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
              INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, kms:kme, 2),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,lq2,kq1
!----------------------------------------------------------------------------
  INTEGER                   :: i,k
!============================================================================
IF(2 > jme-jte)STOP 'In ha21ep3j; nhalo wider than memory dimension allows'
DO k=kts,kte
DO i=its,ite
   a(i,k,jte+2)=p(i,k,    2)+kq1*p(i,k,1)
   a(i,k,jte+1)=p(i,k,    1)
   p(i,k,    2)=a(i,k,  jte)-lq1*p(i,k,1)-lq2*p(i,k,2)
   a(i,k,jte+1)=a(i,k,jte+1)+kq1*p(i,k,1)
ENDDO
ENDDO
END SUBROUTINE ha21ep3j

!============================================================================
SUBROUTINE ha22ep3j(a,p,lq1,lq2,kq1,kq2,                                    &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE
  INTEGER,    INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,    INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,    INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
              INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, kms:kme, 2),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,lq2,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                   :: i,k
!============================================================================
IF(2 > jme-jte)STOP 'In ha22ep3j; nhalo wider than memory dimension allows'
DO k=kts,kte
DO i=its,ite
   a(i,k,jte+2)=p(i,k,  2)+kq1*p(i,k,1)
   a(i,k,jte+1)=p(i,k,  1)
   p(i,k,    2)=a(i,k,jte  )-lq1*p(i,k,1)-lq2*p(i,k,2)
   a(i,k,jte+2)=a(i,k,jte+2)+kq2*p(i,k,2)
   a(i,k,jte+1)=a(i,k,jte+1)+kq1*p(i,k,2)
   p(i,k,    1)=a(i,k,jte-1)-lq1*p(i,k,2)-lq2*p(i,k,1)
   a(i,k,jte+1)=a(i,k,jte+1)+kq2*p(i,k,1)
ENDDO
ENDDO
END SUBROUTINE ha22ep3j

!============================================================================
SUBROUTINE hasz3j(a,  nhalo,                                                &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE
  INTEGER,    INTENT(IN   ) :: nhalo
  INTEGER,    INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,    INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,    INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
              INTENT(INOUT) :: a
!----------------------------------------------------------------------------
  INTEGER                   :: i,k,j
!============================================================================
IF(nhalo > jts-jms)STOP 'In hasz3j; nhalo wider than memory dimension allows'
DO j=1,nhalo
DO k=kts,kte
DO i=its,ite
   a(i,k,jts-j)=0.
ENDDO
ENDDO
ENDDO
END SUBROUTINE hasz3j

!============================================================================
SUBROUTINE haez3j(a,  nhalo,                                                &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE
  INTEGER,    INTENT(IN   ) :: nhalo
  INTEGER,    INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,    INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,    INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
              INTENT(INOUT) :: a
!----------------------------------------------------------------------------
  INTEGER                   :: i,k,j
!============================================================================
IF(nhalo > jme-jte)STOP 'In haez3j; nhalo wider than memory dimension allows'
DO j=1,nhalo
DO k=kts,kte
DO i=its,ite
   a(i,k,jte+j)=0.
ENDDO
ENDDO
ENDDO
END SUBROUTINE haez3j
!
!============================================================================
!  R. J. Purser, jpurser@ncep.noaa.gov
!============================================================================
! Horizontal basic numerical operations for 3-dimensional arrays.
! These are the basic building blocks for horizontal compact and
! conventional differencing, quadrature, midpoint interpolation
! and filtering.

!############# 3-dimensions, index-j active:
!============================================================================
SUBROUTINE ud1c3j(c,d,bnd1,                                                 &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(IN   )    :: c                   

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(OUT  )    :: d    
               
  REAL,         INTENT(IN   )    :: bnd1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   d(i,k,j)=bnd1*(c(i,k,j+1)-c(i,k,j-1))
ENDDO
ENDDO
ENDDO
END SUBROUTINE ud1c3j

!============================================================================
SUBROUTINE ud2c3j(c,d,bnd1,bnd2,                                            &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(IN   )    :: c                   

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(OUT  )    :: d    
               
  REAL,         INTENT(IN   )    :: bnd1,bnd2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   d(i,k,j)=bnd1*(c(i,k,j+1)-c(i,k,j-1))+bnd2*(c(i,k,j+2)-c(i,k,j-2))
ENDDO
ENDDO
ENDDO
END SUBROUTINE ud2c3j

!============================================================================
SUBROUTINE sa1a3j(a,bnm1,                                                   &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: bnm1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=bnm1*(a(i,k,j)+a(i,k,j+1))
ENDDO
ENDDO
ENDDO
END SUBROUTINE sa1a3j

!============================================================================
SUBROUTINE sq1a3j(a,bnq,                                                    &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: bnq
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j-1)+bnq*a(i,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE sq1a3j

!============================================================================
SUBROUTINE sd1b3j(a,bnqi,                                                   &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: bnqi
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jte,jts-1,-1
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=bnqi*(a(i,k,j)-a(i,k,j-1))
ENDDO
ENDDO
ENDDO
END SUBROUTINE sd1b3j

!============================================================================
SUBROUTINE mf01a3j(a,knq1,                                                  &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)+knq1*a(i,k,j+1)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf01a3j

!============================================================================
SUBROUTINE mf02a3j(a,knq1,knq2,                                             &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)+knq1*a(i,k,j+1)+knq2*a(i,k,j+2)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf02a3j

!============================================================================
SUBROUTINE mf10a3j(a,lnq1,                                                  &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)-lnq1*a(i,k,j-1)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf10a3j

!============================================================================
SUBROUTINE mf11a3j(a,lnq1,knq1,                                             &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)-lnq1*a(i,k,j-1)+knq1*a(i,k,j+1)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf11a3j

!============================================================================
SUBROUTINE mf12a3j(a,lnq1,knq1,knq2,                                        &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)-lnq1*a(i,k,j-1) &
                    +knq1*a(i,k,j+1)+knq2*a(i,k,j+2)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf12a3j

!============================================================================
SUBROUTINE mf20a3j(a,lnq1,lnq2,                                             &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)-lnq1*a(i,k,j-1)-lnq2*a(i,k,j-2)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf20a3j

!============================================================================
SUBROUTINE mf21a3j(a,lnq1,lnq2,knq1,                                        &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2,knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)-lnq1*a(i,k,j-1)-lnq2*a(i,k,j-2)+knq1*a(i,k,j+1)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf21a3j

!============================================================================
SUBROUTINE mf22a3j(a,lnq1,lnq2,knq1,knq2,                                   &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2,knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)-lnq1*a(i,k,j-1)-lnq2*a(i,k,j-2) &
                    +knq1*a(i,k,j+1)+knq2*a(i,k,j+2)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf22a3j

!============================================================================
SUBROUTINE tf1a3j(a,lnq1,                                                   &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)+lnq1*(a(i,k,j+1)-a(i,k,j-1))
ENDDO
ENDDO
ENDDO
END SUBROUTINE tf1a3j

!============================================================================
SUBROUTINE tf2a3j(a,lnq1,lnq2,                                              &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)+lnq1*(a(i,k,j+1)-a(i,k,j-1)) &
                    +lnq2*(a(i,k,j+2)-a(i,k,j-2))
ENDDO
ENDDO
ENDDO
END SUBROUTINE tf2a3j

!============================================================================
SUBROUTINE ma10a3j(a,p,lnq1,                                                &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(IN   )    :: a

  REAL, DIMENSION(ims:ime, kms:kme, 1),        &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte,2
DO k=kts,kte
DO i=its,ite
   p(i,k,1)=a(i,k,j)-lnq1*p(i,k,1)
ENDDO
ENDDO
ENDDO
END SUBROUTINE ma10a3j

!============================================================================
SUBROUTINE ma20a3j(a,p,lnq1,lnq2,                                           &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(IN   )    :: a   

  REAL, DIMENSION(ims:ime, kms:kme, 2),        &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte,2
DO k=kts,kte
DO i=its,ite
   p(i,k,2)=a(i,k,j  )-lnq1*p(i,k,1)-lnq2*p(i,k,2)
   p(i,k,1)=a(i,k,j+1)-lnq1*p(i,k,2)-lnq2*p(i,k,1)
ENDDO
ENDDO
ENDDO
END SUBROUTINE ma20a3j

!============================================================================
SUBROUTINE mf01b3j(a,knq1,                                                  &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jte,jts-1,-1
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)+knq1*a(i,k,j-1)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf01b3j

!============================================================================
SUBROUTINE mf02b3j(a,knq1,knq2,                                             &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jte,jts,-1
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)+knq1*a(i,k,j-1)+knq2*a(i,k,j-2)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf02b3j

!============================================================================
SUBROUTINE mf10b3j(a,lnq1,                                                  &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jte,jts,-1
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)-lnq1*a(i,k,j+1)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf10b3j

!============================================================================
SUBROUTINE mf11b3j(a,lnq1,knq1,                                             &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jte,jts,-1
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)-lnq1*a(i,k,j+1)+knq1*a(i,k,j-1)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf11b3j

!============================================================================
SUBROUTINE mf12b3j(a,lnq1,knq1,knq2,                                        &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jte,jts,-1
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)-lnq1*a(i,k,j+1) &
                    +knq1*a(i,k,j-1)+knq2*a(i,k,j-2)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf12b3j

!============================================================================
SUBROUTINE mf20b3j(a,lnq1,lnq2,                                             &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jte,jts,-1
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)-lnq1*a(i,k,j+1)-lnq2*a(i,k,j+2)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf20b3j

!============================================================================
SUBROUTINE mf21b3j(a,lnq1,lnq2,knq1,                                        &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2,knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jte,jts,-1
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)-lnq1*a(i,k,j+1)-lnq2*a(i,k,j+2)+knq1*a(i,k,j-1)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf21b3j

!============================================================================
SUBROUTINE mf22b3j(a,lnq1,lnq2,knq1,knq2,                                   &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2,knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jte,jts,-1
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)-lnq1*a(i,k,j+1)-lnq2*a(i,k,j+2) &
                    +knq1*a(i,k,j-1)+knq2*a(i,k,j-2)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf22b3j

!============================================================================
SUBROUTINE tf1b3j(a,lnq1,                                                   &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jte,jts,-1
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)+lnq1*(a(i,k,j-1)-a(i,k,j+1))
ENDDO
ENDDO
ENDDO
END SUBROUTINE tf1b3j

!============================================================================
SUBROUTINE tf2b3j(a,lnq1,lnq2,                                              &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jte,jts,-1
DO k=kts,kte
DO i=its,ite
   a(i,k,j)=a(i,k,j)+lnq1*(a(i,k,j-1)-a(i,k,j+1)) &
                    +lnq2*(a(i,k,j-2)-a(i,k,j+2))
ENDDO
ENDDO
ENDDO
END SUBROUTINE tf2b3j

!============================================================================
SUBROUTINE ma10b3j(a,p,lnq1,                                                &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(IN   )    :: a                    

  REAL, DIMENSION(ims:ime, kms:kme, 1),        &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jte,jts,-1
DO k=kts,kte
DO i=its,ite
   p(i,k,1)=a(i,k,j)-lnq1*p(i,k,1)
ENDDO
ENDDO
ENDDO
END SUBROUTINE ma10b3j

!============================================================================
SUBROUTINE ma20b3j(a,p,lnq1,lnq2,                                           &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(IN   )    :: a                    

  REAL, DIMENSION(ims:ime, kms:kme, 2),        &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jte,jts,-2
DO k=kts,kte
DO i=its,ite
   p(i,k,2)=a(i,k,j  )-lnq1*p(i,k,1)-lnq2*p(i,k,2)
   p(i,k,1)=a(i,k,j-1)-lnq1*p(i,k,2)-lnq2*p(i,k,1)
ENDDO
ENDDO
ENDDO
END SUBROUTINE ma20b3j 

!============================================================================
!  R. J. Purser, jpurser@ncep.noaa.gov
! Intermediate level routines performing various compact operations
! for horizontally cyclic domains.
!============================================================================
SUBROUTINE hc4m3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "4m" scheme, for 3-dimensional array, active index j
! (4th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 1)                      &
                         :: p
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, fl4m)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL haes3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sa1a3j(c,b4m,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl4m)THEN
p=0.
ELSE
CALL haps3j(c,p, w_l_4m,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma10a3j(c,p,l4m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL hasp3j(c,p,                          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10a3j(c,  l4m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl4m)THEN
p=0.
ELSE
CALL hape3j(c,p, w_l_4m,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma10b3j(c,p,l4m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL haep3j(c,p,                          &  
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10b3j(c,  l4m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc4m3j 

!============================================================================
SUBROUTINE hc6m3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "6m" scheme, for 3-dimensional array, active index j
! (6th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 1)                      &
                         :: p
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, fl6m)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL haes3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sa1a3j(c,b6m,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haes3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fl6m)THEN
p=0.
ELSE
CALL haps3j(c,p, w_l_6m,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma10a3j(c,p,l6m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL ha11sp3j(c,p,l6m1,k6m1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11a3j(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl6m)THEN
p=0.
ELSE
CALL hape3j(c,p, w_l_6m,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma10b3j(c,p,l6m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL ha11ep3j(c,p,l6m1,k6m1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11b3j(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc6m3j 

!============================================================================
SUBROUTINE hc8m3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "8m" scheme, for 3-dimensional array, active index j
! (8th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 2)                      &
                         :: p
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=2*MIN((jte+1-jts)/2, fl8m/2)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL haes3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sa1a3j(c,b8m,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haes3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fl8m)THEN
p=0.
ELSE
CALL haps3j(c,p, w_l_8m,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma20a3j(c,p,l8m1,l8m2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL ha21sp3j(c,p,l8m1,l8m2,k8m1,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21a3j(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl8m)THEN
p=0.
ELSE
CALL hape3j(c,p, w_l_8m,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma20b3j(c,p,l8m1,l8m2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL ha21ep3j(c,p,l8m1,l8m2,k8m1,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21b3j(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc8m3j

!============================================================================
SUBROUTINE hc10m3j(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "10m" scheme, for 3-dimensional array, active index j
! (10th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 2)                      &
                         :: p
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=2*MIN((jte+1-jts)/2, fl10m/2)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL haes3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sa1a3j(c,b10m,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haes3j(c, 2,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fl10m)THEN
p=0.
ELSE
CALL haps3j(c,p, w_l_10m,                 &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma20a3j(c,p,l10m1,l10m2,             &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL ha22sp3j(c,p,l10m1,l10m2,k10m1,k10m2,&
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22a3j(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl10m)THEN
p=0.
ELSE
CALL hape3j(c,p, w_l_10m,                 &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma20b3j(c,p,l10m1,l10m2,             &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL ha22ep3j(c,p,l10m1,l10m2,k10m1,k10m2,&
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22b3j(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc10m3j

!============================================================================
SUBROUTINE hc4d3j(c,d,                                                      &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "4d" scheme, for 3-dimensional array, active index j
! (4th-order differencing on a simple grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(OUT  ) :: d
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 1)                      &
                         :: p
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, fl4d)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL hase3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL haes3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL ud1c3j(c,d,b4d,                      &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl4d)THEN
p=0.
ELSE
CALL haps3j(d,p, w_l_4d,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma10a3j(d,p,l4d1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL hasp3j(d,p,                          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10a3j(d,  l4d1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl4d)THEN
p=0.
ELSE
CALL hape3j(d,p, w_l_4d,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma10b3j(d,p,l4d1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL haep3j(d,p,                          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10b3j(d,  l4d1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc4d3j 

!============================================================================
SUBROUTINE hc8d3j(c,d,                                                      &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "8d" scheme, for 3-dimensional array, active index j
! (8th-order differencing on a simple grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(OUT  ) :: d
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 2)                      &
                         :: p
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=2*MIN((jte+1-jts)/2, fl8d/2)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL hase3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL haes3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL ud1c3j(c,d,b8d,                      &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haes3j(d, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fl8d)THEN
p=0.
ELSE
CALL haps3j(d,p, w_l_8d,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma20a3j(d,p,l8d1,l8d2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL ha21sp3j(d,p,l8d1,l8d2,k8d1,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21a3j(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl8d)THEN
p=0.
ELSE
CALL hape3j(d,p, w_l_8d,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma20b3j(d,p,l8d1,l8d2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL ha21ep3j(d,p,l8d1,l8d2,k8d1,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21b3j(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc8d3j

!============================================================================
SUBROUTINE hc4qi3j(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "4qi" scheme, for 1-dimensional array, active index j
! (4th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 1)                      &
                         :: p
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, fl4q)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL hase3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sd1b3j(c,b4qi,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl4q)THEN
p=0.
ELSE
CALL haps3j(c,p, w_l_4q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma10a3j(c,p,l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL hasp3j(c,p,                          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10a3j(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl4q)THEN
p=0.
ELSE
CALL hape3j(c,p, w_l_4q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma10b3j(c,p,l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL haep3j(c,p,                          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10b3j(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc4qi3j

!============================================================================
SUBROUTINE hc4q3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "4q" scheme, for 3-dimensional array, active index j
! (4th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haes3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf01a3j(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hase3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf01b3j(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hasz3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3j(c,b4q,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc4q3j

!============================================================================
SUBROUTINE hc6qi3j(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "6qi" scheme, for 3-dimensional array, active index j
! (6th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 1)                      &
                         :: p
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, fl6q)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL hase3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sd1b3j(c,b6qi,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haes3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fl6q)THEN
p=0.
ELSE
CALL haps3j(c,p, w_l_6q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma10a3j(c,p,l6q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL ha11sp3j(c,p,l6q1,k6q1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11a3j(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl6q)THEN
p=0.
ELSE
CALL hape3j(c,p, w_l_6q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma10b3j(c,p,l6q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL ha11ep3j(c,p,l6q1,k6q1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11b3j(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc6qi3j

!============================================================================
SUBROUTINE hc6q3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "6q" scheme, for 3-dimensional array, active index j
! (6th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),         &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 1)                      &
                         :: p
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, fk6q)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL haes3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fk6q)THEN
p=0.
ELSE
CALL haps3j(c,p, w_k_6q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma10a3j(c,p,k6q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL ha11sp3j(c,p,k6q1,l6q1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11a3j(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fk6q)THEN
p=0.
ELSE
CALL hape3j(c,p, w_k_6q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma10b3j(c,p,k6q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL ha11ep3j(c,p,k6q1,l6q1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11b3j(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hasz3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3j(c,b6q,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc6q3j

!============================================================================
SUBROUTINE hc8qi3j(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "8qi" scheme, for 3-dimensional array, active index j
! (8th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 2)                      &
                         :: p
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=2*MIN((jte+1-jts)/2, fl8q/2)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL hase3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sd1b3j(c,b8qi,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haes3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fl8q)THEN
p=0.
ELSE
CALL haps3j(c,p, w_l_8q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma20a3j(c,p,l8q1,l8q2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL ha21sp3j(c,p,l8q1,l8q2,k8q1,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21a3j(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl8q)THEN
p=0.
ELSE
CALL hape3j(c,p, w_l_8q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma20b3j(c,p,l8q1,l8q2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL ha21ep3j(c,p,l8q1,l8q2,k8q1,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21b3j(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc8qi3j

!============================================================================
SUBROUTINE hc8q3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "8q" scheme, for 3-dimensional array, active index j
! (8th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 1)                      &
                         :: p
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, fk8q)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL haes3j(c, 2,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fk8q)THEN
p=0.
ELSE
CALL haps3j(c,p, w_k_8q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma10a3j(c,p, k8q1,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL ha12sp3j(c,p,k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf12a3j(c,   k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hase3j(c, 1,                         & ! (widen existing halo)
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,jts-1,jte-1,kts,kte)      

IF(f==fk8q)THEN
p=0.
ELSE
CALL hape3j(c,p, w_k_8q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma10b3j(c,p,k8q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL ha12ep3j(c,p,k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf12b3j(c,  k8q1,l8q1,l8q2,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hasz3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3j(c,b8q,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc8q3j

!============================================================================
SUBROUTINE hc10qi3j(c,                                                      &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "10qi" scheme, for 3-dimensional array, active index j
! (10th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 2)                      &
                         :: p
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=2*MIN((jte+1-jts)/2, fl10q/2)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL hase3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sd1b3j(c,b10qi,                      &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

!------------------
CALL haes3j(c, 2,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fl10q)THEN
p=0.
ELSE
CALL haps3j(c,p, w_l_10q,                 &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma20a3j(c,p,l10q1,l10q2,             &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL ha22sp3j(c,p,l10q1,l10q2,k10q1,k10q2,&
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22a3j(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl10q)THEN
p=0.
ELSE
CALL hape3j(c,p, w_l_10q,                 &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma20b3j(c,p,l10q1,l10q2,             &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL ha22ep3j(c,p,l10q1,l10q2,k10q1,k10q2,&
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22b3j(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc10qi3j

!============================================================================
SUBROUTINE hc10q3j(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "10q" scheme, for 3-dimensional array, active index j
! (10th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 2)                      &
                         :: p
  INTEGER                :: f,jtfs,jtfe
!============================================================================
f=2*MIN((jte+1-jts)/2, fk10q/2)
jtfs=jte+1-f
jtfe=jts-1+f
!------------------
CALL haes3j(c, 2,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fk10q)THEN
p=0.
ELSE
CALL haps3j(c,p, w_k_10q,                 &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma20a3j(c,p,k10q1,k10q2,             &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL ha22sp3j(c,p,k10q1,k10q2,l10q1,l10q2,&
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22a3j(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fk10q)THEN
p=0.
ELSE
CALL hape3j(c,p, w_k_10q,                 &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma20b3j(c,p,k10q1,k10q2,             &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL ha22ep3j(c,p,k10q1,k10q2,l10q1,l10q2,&
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22b3j(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hasz3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3j(c,b10q,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc10q3j

!============================================================================
SUBROUTINE hc4t3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "4t" scheme, for 3-dimensional array, active index j
! (4th-order invertible translation for A to C grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 1)                      &
                         :: p
  INTEGER                :: fl,fk,jtfs,jtfe
!============================================================================
fl=MIN(jte+1-jts, fl4t)
fk=MIN(jte+1-jts, fk4t)
jtfs=jte+1-fl
jtfe=jts-1+fk
!------------------
CALL haes3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(fl==fl4t)THEN
p=0.
ELSE
CALL haps3j(c,p, w_l_4t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma10a3j(c,p,l4t1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL ha11sp3j(c,p,l4t1,l4t1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1a3j(c,  l4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(fk==fk4t)THEN
p=0.
ELSE
CALL hape3j(c,p, w_k_4t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma10b3j(c,p,k4t1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL ha11ep3j(c,p,k4t1,k4t1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1b3j(c,  k4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc4t3j

!============================================================================
SUBROUTINE hc4ti3j(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "4ti" scheme, for 3-dimensional array, active index j
! (4th-order invertible translation for C to A grid; inverse of 4t)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 1)                      &
                         :: p
  INTEGER                :: fl,fk,jtfs,jtfe
!============================================================================
fl=MIN(jte+1-jts, fl4t)
fk=MIN(jte+1-jts, fk4t)
jtfs=jte+1-fk
jtfe=jts-1+fl
!------------------
CALL haes3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(fk==fk4t)THEN
p=0.
ELSE
CALL haps3j(c,p, w_k_4t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma10a3j(c,p,k4t1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL ha11sp3j(c,p,k4t1,k4t1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1a3j(c,  k4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(fl==fl4t)THEN
p=0.
ELSE
CALL hape3j(c,p, w_l_4t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma10b3j(c,p,l4t1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL ha11ep3j(c,p,l4t1,l4t1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1b3j(c,  l4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc4ti3j

!============================================================================
SUBROUTINE hc8t3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "8t" scheme, for 3-dimensional array, active index j
! (8th-order invertible translation for A to C grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 2)                      &
                         :: p
  INTEGER                :: fl,fk,jtfs,jtfe
!============================================================================
fl=2*MIN((jte+1-jts)/2, fl8t/2)
fk=2*MIN((jte+1-jts)/2, fk8t/2)
jtfs=jte+1-fl
jtfe=jts-1+fk
!------------------
CALL haes3j(c, 2,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(fl==fl8t)THEN
p=0.
ELSE
CALL haps3j(c,p, w_l_8t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma20a3j(c,p,l8t1,l8t2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL ha22sp3j(c,p,l8t1,l8t2,l8t1,l8t2,    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2a3j(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(fk==fk8t)THEN
p=0.
ELSE
CALL hape3j(c,p, w_k_8t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma20b3j(c,p,k8t1,k8t2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL ha22ep3j(c,p,k8t1,k8t2,k8t1,k8t2,    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2b3j(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc8t3j

!============================================================================
SUBROUTINE hc8ti3j(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "8ti" scheme, for 3-dimensional array, active index j
! (4th-order invertible translation for C to A grid; inverse of 8t)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 2)                      &
                         :: p
  INTEGER                :: fl,fk,jtfs,jtfe
!============================================================================
fl=2*MIN((jte+1-jts)/2, fl8t/2)
fk=2*MIN((jte+1-jts)/2, fk8t/2)
jtfs=jte+1-fk
jtfe=jts-1+fl
!------------------
CALL haes3j(c, 2,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(fk==fk8t)THEN
p=0.
ELSE
CALL haps3j(c,p, w_k_8t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      
ENDIF

CALL ma20a3j(c,p,k8t1,k8t2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jtfs,jte, kts,kte)      

CALL ha22sp3j(c,p,k8t1,k8t2,k8t1,k8t2,    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2a3j(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(fl==fl8t)THEN
p=0.
ELSE
CALL hape3j(c,p, w_l_8t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      
ENDIF

CALL ma20b3j(c,p,l8t1,l8t2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jtfe, kts,kte)      

CALL ha22ep3j(c,p,l8t1,l8t2,l8t1,l8t2,    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2b3j(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc8ti3j

!============================================================================
SUBROUTINE hc1rf3j(c, al1,frf,amp,        &       
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!============================================================================
! Horizontal cyclic "1rf" scheme, for 3-dimensional array, active index j
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
           INTENT(INOUT) :: c

  REAL,    INTENT(IN   ) :: al1, amp

  INTEGER, INTENT(IN   ) :: frf
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 1):: p
  REAL                   :: beta
  INTEGER                :: i,j,k,f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, frf)
jtfs=jte+1-f
jtfe=jts-1+f
beta=(1.+al1)*amp
!------------------
DO j=its,ite
DO k=kts,kte
DO i=its,ite
   c(i,k,j)=beta*c(i,k,j)
ENDDO
ENDDO
ENDDO

p=0
CALL ma10a3j(c,p,al1,                     &  
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jtfs,jte, kts,kte)  

CALL hasp3j(c,p,                          &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  

CALL mf10a3j(c,  al1,                     &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  

p=0
CALL ma10b3j(c,p,al1,                     &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jtfe, kts,kte)  

CALL haep3j(c,p,                          &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  

CALL mf10b3j(c,  al1,                     &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  
END SUBROUTINE hc1rf3j

!============================================================================
SUBROUTINE hc2rf3j(c,al1,al2,frf,amp,     &       
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!============================================================================
! Horizontal cyclic "2rf" scheme, for 3-dimensional array, active index j
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
           INTENT(INOUT) :: c

  REAL,    INTENT(IN   ) :: al1,al2, amp

  INTEGER, INTENT(IN   ) :: frf
!----------------------------------------------------------------------------
  REAL, DIMENSION(ims:ime, kms:kme, 2)        &
                         :: p
  REAL                   :: beta
  INTEGER                :: i,j,k,f,jtfs,jtfe
!============================================================================
f=MIN(jte+1-jts, frf)
jtfs=jte+1-f
jtfe=jts-1+f
beta=(1.+al1+al2)*amp
!------------------
DO j=its,ite
DO k=kts,kte
DO i=its,ite
   c(i,k,j)=beta*c(i,k,j)
ENDDO
ENDDO
ENDDO

p=0
CALL ma20a3j(c,p,al1,al2,                 &  
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jtfs,jte, kts,kte)  

CALL hasp3j(c,p,                          &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  

CALL mf20a3j(c,  al1,al2,                 &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  

p=0
CALL ma20b3j(c,p,al1,al2,                 &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jtfe, kts,kte)  

CALL haep3j(c,p,                          &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  

CALL mf20b3j(c,  al1,al2,                 &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  
END SUBROUTINE hc2rf3j


!============================================================================
!  R. J. Purser, jpurser@ncep.noaa.gov
! Intermediate level routines performing various compact operations
! for horizontally bounded domains.
!============================================================================
SUBROUTINE hg4m3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "4m" scheme, for 3-dimensional array, active index j
! (4th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
!------------------
CALL sa1a3j(c,b4m,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hass3j(c, w_l_4m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10a3j(c,  l4m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wll_4m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10b3j(c,  l4m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg4m3j 

!============================================================================
SUBROUTINE hg6m3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "6m" scheme, for 3-dimensional array, active index j
! (6th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sa1a3j(c,b6m,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wk__6m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3j(c, w_lk6m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11a3j(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wllk6m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11b3j(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg6m3j 

!============================================================================
SUBROUTINE hg8m3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "8m" scheme, for 3-dimensional array, active index j
! (8th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sa1a3j(c,b8m,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wk__8m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3j(c, w_lk8m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21a3j(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wllk8m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21b3j(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg8m3j

!============================================================================
SUBROUTINE hg10m3j(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "10m" scheme, for 3-dimensional array, active index j
! (10th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sa1a3j(c,b10m,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wk__10m,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3j(c, w_lk10m,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22a3j(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wllk10m,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22b3j(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg10m3j

!============================================================================
SUBROUTINE hg4d3j(c,d,                                                      &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "4d" scheme, for 3-dimensional array, active index j
! (4th-order differencing on a simple grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(OUT  ) :: d
!----------------------------------------------------------------------------
!============================================================================
CALL hass3j(c, wi__4d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL haee3j(c, wi__4d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL ud1c3j(c,d,b4d,                      &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hass3j(d, w_l_4d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10a3j(d,  l4d1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(d, wll_4d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10b3j(d,  l4d1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg4d3j 

!============================================================================
SUBROUTINE hg8d3j(c,d,                                                      &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "8d" scheme, for 3-dimensional array, active index j
! (8th-order differencing on a simple grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(OUT  ) :: d
!----------------------------------------------------------------------------
!============================================================================
CALL hass3j(c, wi__8d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL haee3j(c, wi__8d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL ud1c3j(c,d,b8d,                      &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(d, wk__8d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3j(d, w_lk8d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21a3j(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(d, wllk8d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21b3j(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg8d3j

!============================================================================
SUBROUTINE hg4qi3j(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "4qi" scheme, for 3-dimensional array, active index j
! (4th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sd1b3j(c,b4qi,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hass3j(c, w_l_4q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10a3j(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wll_4q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10b3j(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg4qi3j

!============================================================================
SUBROUTINE hg4q3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "4q" scheme, for 3-dimensional array, active index j
! (4th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee3j(c, wl__4q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf01a3j(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hass3j(c, w___4q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf01b3j(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hasz3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3j(c,b4q,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg4q3j

!============================================================================
SUBROUTINE hg6qi3j(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "6qi" scheme, for 3-dimensional array, active index j
! (6th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sd1b3j(c,b6qi,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wk__6q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3j(c, w_lk6q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11a3j(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wllk6q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11b3j(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg6qi3j

!============================================================================
SUBROUTINE hg6q3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "6q" scheme, for 3-dimensional array, active index j
! (6th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),         &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee3j(c, wl__6q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3j(c, w_kl6q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11a3j(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wkkl6q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11b3j(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hasz3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3j(c,b6q,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg6q3j

!============================================================================
SUBROUTINE hg8qi3j(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "8qi" scheme, for 3-dimensional array, active index j
! (8th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sd1b3j(c,b8qi,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wk__8q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3j(c, w_lk8q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21a3j(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wllk8q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21b3j(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg8qi3j

!============================================================================
SUBROUTINE hg8q3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "8q" scheme, for 3-dimensional array, active index j
! (8th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee3j(c, wl__8q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3j(c, w_kl8q,                    & ! (2-wide halo for later b-filter) 
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf12a3j(c,   k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wkkl8q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf12b3j(c,  k8q1,l8q1,l8q2,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hasz3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3j(c,b8q,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg8q3j

!============================================================================
SUBROUTINE hg10qi3j(c,                                                      &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "10qi" scheme, for 3-dimensional array, active index j
! (10th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sd1b3j(c,b10qi,                      &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wk__10q,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3j(c, w_lk10q,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22a3j(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wllk10q,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22b3j(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg10qi3j

!============================================================================
SUBROUTINE hg10q3j(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "10q" scheme, for 3-dimensional array, active index j
! (10th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee3j(c, wl__10q,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3j(c, w_kl10q,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22a3j(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wkkl10q,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22b3j(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hasz3j(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3j(c,b10q,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg10q3j

!============================================================================
SUBROUTINE hg4t3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "4t" scheme, for 3-dimensional array, active index j
! (4th-order invertible translation for A to C grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee3j(c, wk__4t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3j(c, w_ll4t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1a3j(c,  l4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wlkk4t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1b3j(c,  k4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg4t3j

!============================================================================
SUBROUTINE hg4ti3j(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "4ti" scheme, for 3-dimensional array, active index j
! (4th-order invertible translation for C to A grid; inverse of 4t)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee3j(c, wl__4t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3j(c, w_kk4t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1a3j(c,  k4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wkll4t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1b3j(c,  l4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg4ti3j

!============================================================================
SUBROUTINE hg8t3j(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "8t" scheme, for 3-dimensional array, active index j
! (8th-order invertible translation for A to C grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee3j(c, wk__8t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3j(c, w_ll8t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2a3j(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wlkk8t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2b3j(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg8t3j

!============================================================================
SUBROUTINE hg8ti3j(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "8ti" scheme, for 3-dimensional array, active index j
! (4th-order invertible translation for C to A grid; inverse of 8t)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee3j(c, wl__8t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3j(c, w_kk8t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2a3j(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3j(c, wkll8t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2b3j(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg8ti3j

!END MODULE MODULE_hops3j
