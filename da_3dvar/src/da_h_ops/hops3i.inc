!						**********************
!						*  module_hops3i.f90 *
!						*  PURSER 2001       *
! 						**********************
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
!MODULE MODULE_hops3i
!CONTAINS
!============================================================================
SUBROUTINE hase3i(a, nhalo,                                                 &
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
IF(nhalo > its-ims)STOP 'In hase3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
DO i=1,nhalo
   a(its-i,k,j)=a(ite+1-i,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE hase3i

!============================================================================
SUBROUTINE haes3i(a, nhalo,                                                 &
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
IF(nhalo > ime-ite)STOP 'In haes3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
DO i=1,nhalo
   a(ite+i,k,j)=a(its-1+i,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE haes3i

!============================================================================
SUBROUTINE hass3i(a, wext,                                                  &
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
  INTEGER                        :: in,im,j,k
  INTEGER                        :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
IF(nhalo > its-ims)STOP 'In hass3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
DO in=1,nhalo
   a(its-in,k,j)=0.
   DO im=1,mhalo
      a(its-in,k,j)=a(its-in,k,j)+wext(in,im)*a(its-1+im,k,j)
   ENDDO
ENDDO
ENDDO
ENDDO
END SUBROUTINE hass3i

!============================================================================
SUBROUTINE haps3i(a,p, wext,                                                &
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
  REAL, DIMENSION(:,       kms:,    jms:   ),      &
                INTENT(OUT  )    :: p

  REAL,DIMENSION(:,:),                             &
                INTENT(IN   )    :: wext
!----------------------------------------------------------------------------
  INTEGER                        :: in,im,j,k
  INTEGER                        :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
DO j=jts,jte
DO k=kts,kte
DO in=1,nhalo
   p(in,k,j)=0.
   DO im=1,mhalo
      p(in,k,j)=p(in,k,j)+wext(in,im)*a(its-1+im,k,j)
   ENDDO
ENDDO
ENDDO
ENDDO
END SUBROUTINE haps3i

!============================================================================
SUBROUTINE haee3i(a, wext,                                                  &
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
  INTEGER                        :: in,im,j,k
  INTEGER                        :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
IF(nhalo > ime-ite)STOP 'In haee3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
DO in=1,nhalo
   a(ite+in,k,j)=0.
   DO im=1,mhalo
      a(ite+in,k,j)=a(ite+in,k,j)+wext(in,im)*a(ite+1-im,k,j)
   ENDDO
ENDDO
ENDDO
ENDDO
END SUBROUTINE haee3i

!============================================================================
SUBROUTINE hape3i(a,p, wext,                                                &
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
  REAL, DIMENSION(:,       kms:,    jms:   ),      &
                INTENT(OUT  )    :: p

  REAL,DIMENSION(:,:),                             &
                INTENT(IN   )    :: wext
!----------------------------------------------------------------------------
  INTEGER                        :: in,im,j,k
  INTEGER                        :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
DO j=jts,jte
DO k=kts,kte
DO in=1,nhalo
   p(in,k,j)=0.
   DO im=1,mhalo
      p(in,k,j)=p(in,k,j)+wext(in,im)*a(ite+1-im,k,j)
   ENDDO
ENDDO
ENDDO
ENDDO
END SUBROUTINE hape3i

!============================================================================
SUBROUTINE hasp3i(a,p,                                                      & 
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
  REAL, DIMENSION(:,       kms:   , jms:   ), &
              INTENT(IN   ) :: p
!----------------------------------------------------------------------------
  INTEGER                   :: i,k,j
  INTEGER                   :: nhalo
!============================================================================
nhalo=SIZE(p,1)
IF(nhalo > its-ims)STOP 'In hasp3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
DO i=1,nhalo
   a(its-i,k,j)=p(i,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE hasp3i

!============================================================================
SUBROUTINE ha11sp3i(a,p,lq1,kq1,                                            &
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
  REAL, DIMENSION(1, kms:kme, jms:jme),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,kq1
!----------------------------------------------------------------------------
  INTEGER                   :: j,k
!============================================================================
IF(1 > its-ims)STOP 'In ha11sp3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
   a(its-1,k,j)=p(1    ,k,j)
   p(1    ,k,j)=a(its  ,k,j)-lq1*p(1,k,j)
   a(its-1,k,j)=a(its-1,k,j)+kq1*p(1,k,j)
ENDDO
ENDDO
END SUBROUTINE ha11sp3i

!============================================================================
SUBROUTINE ha12sp3i(a,p,lq1,kq1,kq2,                                        &
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
  REAL, DIMENSION(1, kms:kme, jms:jme),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                   :: j,k
!============================================================================
IF(1 > its-ims)STOP 'In ha12sp3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
   a(its-1,k,j)=p(1    ,k,j)
   p(1    ,k,j)=a(its  ,k,j)-lq1*p(1,k,j)
   a(its-1,k,j)=a(its-1,k,j)+kq1*p(1,k,j)
   p(1    ,k,j)=a(its+1,k,j)-lq1*p(1,k,j)
   a(its-1,k,j)=a(its-1,k,j)+kq2*p(1,k,j)
ENDDO
ENDDO
END SUBROUTINE ha12sp3i

!============================================================================
SUBROUTINE ha21sp3i(a,p,lq1,lq2,kq1,                                        &
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
  REAL, DIMENSION(2, kms:kme, jms:jme),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,lq2,kq1
!----------------------------------------------------------------------------
  INTEGER                   :: j,k
!============================================================================
IF(2 > its-ims)STOP 'In ha21sp3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
   a(its-2,k,j)=p(2    ,k,j)+kq1*p(1,k,j)
   a(its-1,k,j)=p(1    ,k,j)
   p(2    ,k,j)=a(its  ,k,j)-lq1*p(1,k,j)-lq2*p(2,k,j)
   a(its-1,k,j)=a(its-1,k,j)+kq1*p(2,k,j)
ENDDO
ENDDO
END SUBROUTINE ha21sp3i

!============================================================================
SUBROUTINE ha22sp3i(a,p,lq1,lq2,kq1,kq2,                                    &
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
  REAL, DIMENSION(2, kms:kme, jms:jme),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,lq2,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                   :: j,k
!============================================================================
IF(2 > its-ims)STOP 'In ha22sp3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
   a(its-2,k,j)=p(2    ,k,j)+kq1*p(1,k,j)
   a(its-1,k,j)=p(1    ,k,j)
   p(2    ,k,j)=a(its  ,k,j)-lq1*p(1,k,j)-lq2*p(2,k,j)
   a(its-2,k,j)=a(its-2,k,j)+kq2*p(2,k,j)
   a(its-1,k,j)=a(its-1,k,j)+kq1*p(2,k,j)
   p(1    ,k,j)=a(its+1,k,j)-lq1*p(2,k,j)-lq2*p(1,k,j)
   a(its-1,k,j)=a(its-1,k,j)+kq2*p(1,k,j)
ENDDO
ENDDO
END SUBROUTINE ha22sp3i

!============================================================================
SUBROUTINE haep3i(a,p,                                                      &
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
  REAL, DIMENSION(:,       kms:   , jms:   ), &
              INTENT(IN   ) :: p
!----------------------------------------------------------------------------
  INTEGER                   :: i,k,j
  INTEGER                   :: nhalo
!============================================================================
nhalo=SIZE(p,1)
IF(nhalo > ime-ite)STOP 'In haep3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
DO i=1,nhalo
   a(ite+i,k,j)=p(i,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE haep3i

!============================================================================
SUBROUTINE ha11ep3i(a,p,lq1,kq1,                                            &
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
  REAL, DIMENSION(1, kms:kme, jms:jme),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,kq1
!----------------------------------------------------------------------------
  INTEGER                   :: j,k
!============================================================================
IF(1 > ime-ite)STOP 'In ha11ep3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
   a(ite+1,k,j)=p(1    ,k,j)
   p(1    ,k,j)=a(ite  ,k,j)-lq1*p(1,k,j)
   a(ite+1,k,j)=a(ite+1,k,j)+kq1*p(1,k,j)
ENDDO
ENDDO
END SUBROUTINE ha11ep3i

!============================================================================
SUBROUTINE ha12ep3i(a,p,lq1,kq1,kq2,                                        &
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
  REAL, DIMENSION(1, kms:kme, jms:jme),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                   :: j,k
!============================================================================
IF(1 > ime-ite)STOP 'In ha12ep3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
   a(ite+1,k,j)=p(1    ,k,j)
   p(1    ,k,j)=a(ite  ,k,j)-lq1*p(1,k,j)
   a(ite+1,k,j)=a(ite+1,k,j)+kq1*p(1,k,j)
   p(1    ,k,j)=a(ite-1,k,j)-lq1*p(1,k,j)
   a(ite+1,k,j)=a(ite+1,k,j)+kq2*p(1,k,j)
ENDDO
ENDDO
END SUBROUTINE ha12ep3i

!============================================================================
SUBROUTINE ha21ep3i(a,p,lq1,lq2,kq1,                                        &
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
  REAL, DIMENSION(2, kms:kme, jms:jme),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,lq2,kq1
!----------------------------------------------------------------------------
  INTEGER                   :: j,k
!============================================================================
IF(2 > ime-ite)STOP 'In ha21ep3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
   a(ite+2,k,j)=p(2    ,k,j)+kq1*p(1,k,j)
   a(ite+1,k,j)=p(1    ,k,j)
   p(2    ,k,j)=a(ite  ,k,j)-lq1*p(1,k,j)-lq2*p(2,k,j)
   a(ite+1,k,j)=a(ite+1,k,j)+kq1*p(2,k,j)
ENDDO
ENDDO
END SUBROUTINE ha21ep3i

!============================================================================
SUBROUTINE ha22ep3i(a,p,lq1,lq2,kq1,kq2,                                    &
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
  REAL, DIMENSION(2, kms:kme, jms:jme),       &
              INTENT(INOUT) :: p
  REAL,       INTENT(IN   ) :: lq1,lq2,kq1,kq2
!----------------------------------------------------------------------------
  INTEGER                   :: j,k
!============================================================================
IF(2 > ime-ite)STOP 'In ha22ep3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
   a(ite+2,k,j)=p(2    ,k,j)+kq1*p(1,k,j)
   a(ite+1,k,j)=p(1    ,k,j)
   p(2    ,k,j)=a(ite  ,k,j)-lq1*p(1,k,j)-lq2*p(2,k,j)
   a(ite+2,k,j)=a(ite+2,k,j)+kq2*p(2,k,j)
   a(ite+1,k,j)=a(ite+1,k,j)+kq1*p(2,k,j)
   p(1    ,k,j)=a(ite-1,k,j)-lq1*p(2,k,j)-lq2*p(1,k,j)
   a(ite+1,k,j)=a(ite+1,k,j)+kq2*p(1,k,j)
ENDDO
ENDDO
END SUBROUTINE ha22ep3i

!============================================================================
SUBROUTINE hasz3i(a,  nhalo,                                                & 
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
IF(nhalo > its-ims)STOP 'In hasz3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
DO i=1,nhalo
   a(its-i,k,j)=0.
ENDDO
ENDDO
ENDDO
END SUBROUTINE hasz3i 

!============================================================================
SUBROUTINE haez3i(a,  nhalo,                                                & 
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
IF(nhalo > ime-ite)STOP 'In haez3i; nhalo wider than memory dimension allows'
DO j=jts,jte
DO k=kts,kte
DO i=1,nhalo
   a(ite+i,k,j)=0.
ENDDO
ENDDO
ENDDO
END SUBROUTINE haez3i 
!---------------------------------------------------------------------------
!
!============================================================================
!  R. J. Purser, jpurser@ncep.noaa.gov
!============================================================================
! Horizontal basic numerical operations for 3-dimensional arrays.
! These are the basic building blocks for horizontal compact and
! conventional differencing, quadrature, midpoint interpolation
! and filtering.
!############# 3-dimensions, index-i active:

!============================================================================
SUBROUTINE ud1c3i(c,d,bnd1,                                                 &
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
               
  REAL,         INTENT(IN   )    ::  bnd1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   d(i,k,j)=bnd1*(c(i+1,k,j)-c(i-1,k,j))
ENDDO
ENDDO
ENDDO
END SUBROUTINE ud1c3i

!============================================================================
SUBROUTINE ud2c3i(c,d,bnd1,bnd2,                                            &
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
   d(i,k,j)=bnd1*(c(i+1,k,j)-c(i-1,k,j))+bnd2*(c(i+2,k,j)-c(i-2,k,j))
ENDDO
ENDDO
ENDDO
END SUBROUTINE ud2c3i

!============================================================================
SUBROUTINE sa1a3i(a,bnm1,                                                   &
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
   a(i,k,j)=bnm1*(a(i,k,j)+a(i+1,k,j))
ENDDO
ENDDO
ENDDO
END SUBROUTINE sa1a3i

!============================================================================
SUBROUTINE sq1a3i(a,bnq,                                                    &
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
   a(i,k,j)=a(i-1,k,j)+bnq*a(i,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE sq1a3i

!============================================================================
SUBROUTINE sd1b3i(a,bnqi,                                                   &
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
DO j=jts,jte
DO k=kts,kte
DO i=ite,its,-1
   a(i,k,j)=bnqi*(a(i,k,j)-a(i-1,k,j))
ENDDO
ENDDO
ENDDO
END SUBROUTINE sd1b3i

!============================================================================
SUBROUTINE mf01a3i(a,knq1,                                                  &
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
   a(i,k,j)=a(i,k,j)+knq1*a(i+1,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf01a3i

!============================================================================
SUBROUTINE mf02a3i(a,knq1,knq2,                                             &
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
   a(i,k,j)=a(i,k,j)+knq1*a(i+1,k,j)+knq2*a(i+2,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf02a3i

!============================================================================
SUBROUTINE mf10a3i(a,lnq1,                                                  &
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
   a(i,k,j)=a(i,k,j)-lnq1*a(i-1,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf10a3i

!============================================================================
SUBROUTINE mf11a3i(a,lnq1,knq1,                                             &
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
   a(i,k,j)=a(i,k,j)-lnq1*a(i-1,k,j)+knq1*a(i+1,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf11a3i

!============================================================================
SUBROUTINE mf12a3i(a,lnq1,knq1,knq2,                                        &
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
   a(i,k,j)=a(i,k,j)-lnq1*a(i-1,k,j) &
                    +knq1*a(i+1,k,j)+knq2*a(i+2,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf12a3i

!============================================================================
SUBROUTINE mf20a3i(a,lnq1,lnq2,                                             &
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
   a(i,k,j)=a(i,k,j)-lnq1*a(i-1,k,j)-lnq2*a(i-2,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf20a3i

!============================================================================
SUBROUTINE mf21a3i(a,lnq1,lnq2,knq1,                                        &
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
   a(i,k,j)=a(i,k,j)-lnq1*a(i-1,k,j)-lnq2*a(i-2,k,j)+knq1*a(i+1,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf21a3i

!============================================================================
SUBROUTINE mf22a3i(a,lnq1,lnq2,knq1,knq2,                                   &
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
   a(i,k,j)=a(i,k,j)-lnq1*a(i-1,k,j)-lnq2*a(i-2,k,j) &
                    +knq1*a(i+1,k,j)+knq2*a(i+2,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf22a3i

!============================================================================
SUBROUTINE tf1a3i(a,lnq1,                                                   &
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
   a(i,k,j)=a(i,k,j)+lnq1*(a(i+1,k,j)-a(i-1,k,j))
ENDDO
ENDDO
ENDDO
END SUBROUTINE tf1a3i

!============================================================================
SUBROUTINE tf2a3i(a,lnq1,lnq2,                                              &
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
   a(i,k,j)=a(i,k,j)+lnq1*(a(i+1,k,j)-a(i-1,k,j)) &
                    +lnq2*(a(i+2,k,j)-a(i-2,k,j))
ENDDO
ENDDO
ENDDO
END SUBROUTINE tf2a3i

!============================================================================
SUBROUTINE ma10a3i(a,p,lnq1,                                                &
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

  REAL, DIMENSION(1,       kms:kme, jms:jme),  &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite
   p(1,k,j)=a(i,k,j)-lnq1*p(1,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE ma10a3i

!============================================================================
SUBROUTINE ma20a3i(a,p,lnq1,lnq2,                                           &
     ids,ide, jds,jde, kds,kde,                                             &
     ims,ime, jms,jme, kms,kme,                                             &
     its,ite, jts,jte, kts,kte                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  &
                INTENT(IN   )    ::  a   

  REAL, DIMENSION(2,       kms:kme, jms:jme),  &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=its,ite,2
   p(2,k,j)=a(i,  k,j)-lnq1*p(1,k,j)-lnq2*p(2,k,j)
   p(1,k,j)=a(i+1,k,j)-lnq1*p(2,k,j)-lnq2*p(1,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE ma20a3i

!============================================================================
SUBROUTINE mf01b3i(a,knq1,                                                  &
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
DO i=ite,its,-1
   a(i,k,j)=a(i,k,j)+knq1*a(i-1,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf01b3i

!============================================================================
SUBROUTINE mf02b3i(a,knq1,knq2,                                             &
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
DO i=ite,its,-1
   a(i,k,j)=a(i,k,j)+knq1*a(i-1,k,j)+knq2*a(i-2,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf02b3i

!============================================================================
SUBROUTINE mf10b3i(a,lnq1,                                                  &
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
DO i=ite,its,-1
   a(i,k,j)=a(i,k,j)-lnq1*a(i+1,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf10b3i

!============================================================================
SUBROUTINE mf11b3i(a,lnq1,knq1,                                             &
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
DO i=ite,its,-1
   a(i,k,j)=a(i,k,j)-lnq1*a(i+1,k,j)+knq1*a(i-1,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf11b3i

!============================================================================
SUBROUTINE mf12b3i(a,lnq1,knq1,knq2,                                        &
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
DO i=ite,its,-1
   a(i,k,j)=a(i,k,j)-lnq1*a(i+1,k,j) &
                    +knq1*a(i-1,k,j)+knq2*a(i-2,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf12b3i

!============================================================================
SUBROUTINE mf20b3i(a,lnq1,lnq2,                                             &
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
DO i=ite,its,-1
   a(i,k,j)=a(i,k,j)-lnq1*a(i+1,k,j)-lnq2*a(i+2,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf20b3i

!============================================================================
SUBROUTINE mf21b3i(a,lnq1,lnq2,knq1,                                        &
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
DO i=ite,its,-1
   a(i,k,j)=a(i,k,j)-lnq1*a(i+1,k,j)-lnq2*a(i+2,k,j)+knq1*a(i-1,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf21b3i

!============================================================================
SUBROUTINE mf22b3i(a,lnq1,lnq2,knq1,knq2,                                   &
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
DO i=ite,its,-1
   a(i,k,j)=a(i,k,j)-lnq1*a(i+1,k,j)-lnq2*a(i+2,k,j) &
                    +knq1*a(i-1,k,j)+knq2*a(i-2,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE mf22b3i

!============================================================================
SUBROUTINE tf1b3i(a,lnq1,                                                   &
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
DO i=ite,its,-1
   a(i,k,j)=a(i,k,j)+lnq1*(a(i-1,k,j)-a(i+1,k,j))
ENDDO
ENDDO
ENDDO
END SUBROUTINE tf1b3i

!============================================================================
SUBROUTINE tf2b3i(a,lnq1,lnq2,                                              &
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
DO i=ite,its,-1
   a(i,k,j)=a(i,k,j)+lnq1*(a(i-1,k,j)-a(i+1,k,j)) &
                    +lnq2*(a(i-2,k,j)-a(i+2,k,j))
ENDDO
ENDDO
ENDDO
END SUBROUTINE tf2b3i

!============================================================================
SUBROUTINE ma10b3i(a,p,lnq1,                                                &
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

  REAL, DIMENSION(1, kms:kme, jms:jme),        &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=ite,its,-1
   p(1,k,j)=a(i,k,j)-lnq1*p(1,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE ma10b3i

!============================================================================
SUBROUTINE ma20b3i(a,p,lnq1,lnq2,                                           &
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

  REAL, DIMENSION(2, kms:kme, jms:jme),        &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i,j,k
!============================================================================
DO j=jts,jte
DO k=kts,kte
DO i=ite,its,-2
   p(2,k,j)=a(i  ,k,j)-lnq1*p(1,k,j)-lnq2*p(2,k,j)
   p(1,k,j)=a(i-1,k,j)-lnq1*p(2,k,j)-lnq2*p(1,k,j)
ENDDO
ENDDO
ENDDO
END SUBROUTINE ma20b3i 

!===========================================================================
!
!============================================================================
!  R. J. Purser, jpurser@ncep.noaa.gov
!Intermediate level routines performing various compact operations
! for horizontally cyclic domains.
!============================================================================
SUBROUTINE hc4m3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "4m" scheme, for 3-dimensional array, active index i
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
  REAL, DIMENSION(1, kms:kme, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fl4m)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sa1a3i(c,b4m,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl4m)THEN
p=0.
ELSE
CALL haps3i(c,p, w_l_4m,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF
CALL ma10a3i(c,p,l4m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL hasp3i(c,p,                          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10a3i(c,  l4m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl4m)THEN
p=0.
ELSE
CALL hape3i(c,p, w_l_4m,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma10b3i(c,p,l4m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL haep3i(c,p,                          &  
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10b3i(c,  l4m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc4m3i 

!============================================================================
SUBROUTINE hc6m3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "6m" scheme, for 3-dimensional array, active index i
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
  REAL, DIMENSION(1, kms:kme, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fl6m)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sa1a3i(c,b6m,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haes3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fl6m)THEN
p=0.
ELSE
CALL haps3i(c,p, w_l_6m,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma10a3i(c,p,l6m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL ha11sp3i(c,p,l6m1,k6m1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11a3i(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl6m)THEN
p=0.
ELSE
CALL hape3i(c,p, w_l_6m,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma10b3i(c,p,l6m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL ha11ep3i(c,p,l6m1,k6m1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11b3i(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc6m3i 

!============================================================================
SUBROUTINE hc8m3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "8m" scheme, for 3-dimensional array, active index i
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
  REAL, DIMENSION(2, kms:kme, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fl8m/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sa1a3i(c,b8m,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haes3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fl8m)THEN
p=0.
ELSE
CALL haps3i(c,p, w_l_8m,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma20a3i(c,p,l8m1,l8m2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL ha21sp3i(c,p,l8m1,l8m2,k8m1,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21a3i(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl8m)THEN
p=0.
ELSE
CALL hape3i(c,p, w_l_8m,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma20b3i(c,p,l8m1,l8m2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL ha21ep3i(c,p,l8m1,l8m2,k8m1,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21b3i(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc8m3i

!============================================================================
SUBROUTINE hc10m3i(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "10m" scheme, for 3-dimensional array, active index i
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
  REAL, DIMENSION(2, kms:kme, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fl10m/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sa1a3i(c,b10m,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haes3i(c, 2,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fl10m)THEN
p=0.
ELSE
CALL haps3i(c,p, w_l_10m,                 &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma20a3i(c,p,l10m1,l10m2,             &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL ha22sp3i(c,p,l10m1,l10m2,k10m1,k10m2,&
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22a3i(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl10m)THEN
p=0.
ELSE
CALL hape3i(c,p, w_l_10m,                 &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma20b3i(c,p,l10m1,l10m2,             &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL ha22ep3i(c,p,l10m1,l10m2,k10m1,k10m2,&
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22b3i(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc10m3i

!============================================================================
SUBROUTINE hc4d3i(c,d,                                                      &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "4d" scheme, for 3-dimensional array, active index i
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
  REAL, DIMENSION(1, kms:kme, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fl4d)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL haes3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL ud1c3i(c,d,b4d,                      &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl4d)THEN
p=0.
ELSE
CALL haps3i(d,p, w_l_4d,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma10a3i(d,p,l4d1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL hasp3i(d,p,                          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10a3i(d,  l4d1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl4d)THEN
p=0.
ELSE
CALL hape3i(d,p, w_l_4d,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma10b3i(d,p,l4d1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL haep3i(d,p,                          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10b3i(d,  l4d1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc4d3i 

!============================================================================
SUBROUTINE hc8d3i(c,d,                                                      &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "8d" scheme, for 3-dimensional array, active index i
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
  REAL, DIMENSION(2, kms:kme, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fl8d/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL haes3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL ud1c3i(c,d,b8d,                      &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haes3i(d, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fl8d)THEN
p=0.
ELSE
CALL haps3i(d,p, w_l_8d,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma20a3i(d,p,l8d1,l8d2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL ha21sp3i(d,p,l8d1,l8d2,k8d1,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21a3i(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl8d)THEN
p=0.
ELSE
CALL hape3i(d,p, w_l_8d,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma20b3i(d,p,l8d1,l8d2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL ha21ep3i(d,p,l8d1,l8d2,k8d1,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21b3i(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc8d3i

!============================================================================
SUBROUTINE hc4qi3i(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "4qi" scheme, for 1-dimensional array, active index i
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
  REAL, DIMENSION(1, kms:kme, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fl4q)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sd1b3i(c,b4qi,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl4q)THEN
p=0.
ELSE
CALL haps3i(c,p, w_l_4q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma10a3i(c,p,l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL hasp3i(c,p,                          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10a3i(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl4q)THEN
p=0.
ELSE
CALL hape3i(c,p, w_l_4q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma10b3i(c,p,l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL haep3i(c,p,                          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10b3i(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc4qi3i

!============================================================================
SUBROUTINE hc4q3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "4q" scheme, for 3-dimensional array, active index i
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
CALL haes3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf01a3i(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hase3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf01b3i(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hasz3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3i(c,b4q,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc4q3i

!============================================================================
SUBROUTINE hc6qi3i(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "6qi" scheme, for 3-dimensional array, active index i
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
  REAL, DIMENSION(1, kms:kme, jms:jme)                      &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fl6q)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sd1b3i(c,b6qi,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haes3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fl6q)THEN
p=0.
ELSE
CALL haps3i(c,p, w_l_6q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma10a3i(c,p,l6q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL ha11sp3i(c,p,l6q1,k6q1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11a3i(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl6q)THEN
p=0.
ELSE
CALL hape3i(c,p, w_l_6q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma10b3i(c,p,l6q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL ha11ep3i(c,p,l6q1,k6q1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11b3i(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc6qi3i

!============================================================================
SUBROUTINE hc6q3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "6q" scheme, for 3-dimensional array, active index i
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
  REAL, DIMENSION(1, kms:kme, jms:jme)                &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fk6q)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fk6q)THEN
p=0.
ELSE
CALL haps3i(c,p, w_k_6q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma10a3i(c,p,k6q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL ha11sp3i(c,p,k6q1,l6q1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11a3i(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fk6q)THEN
p=0.
ELSE
CALL hape3i(c,p, w_k_6q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma10b3i(c,p,k6q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL ha11ep3i(c,p,k6q1,l6q1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11b3i(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hasz3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3i(c,b6q,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc6q3i

!============================================================================
SUBROUTINE hc8qi3i(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "8qi" scheme, for 3-dimensional array, active index i
! (8th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),         &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2, kms:kme, jms:jme)                &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fl8q/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sd1b3i(c,b8qi,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haes3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fl8q)THEN
p=0.
ELSE
CALL haps3i(c,p, w_l_8q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma20a3i(c,p,l8q1,l8q2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL ha21sp3i(c,p,l8q1,l8q2,k8q1,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21a3i(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl8q)THEN
p=0.
ELSE
CALL hape3i(c,p, w_l_8q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma20b3i(c,p,l8q1,l8q2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL ha21ep3i(c,p,l8q1,l8q2,k8q1,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21b3i(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc8qi3i

!============================================================================
SUBROUTINE hc8q3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "8q" scheme, for 3-dimensional array, active index i
! (8th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),         &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1, kms:kme, jms:jme)                &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fk8q)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes3i(c, 2,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fk8q)THEN
p=0.
ELSE
CALL haps3i(c,p, w_k_8q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma10a3i(c,p, k8q1,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL ha12sp3i(c,p,k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf12a3i(c,   k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hase3i(c, 1,                         & ! (widen existing halo)
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its-1,ite-1,jts,jte,kts,kte)      

IF(f==fk8q)THEN
p=0.
ELSE
CALL hape3i(c,p, w_k_8q,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma10b3i(c,p,k8q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL ha12ep3i(c,p,k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf12b3i(c,  k8q1,l8q1,l8q2,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hasz3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3i(c,b8q,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc8q3i

!============================================================================
SUBROUTINE hc10qi3i(c,                                                      &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "10qi" scheme, for 3-dimensional array, active index i
! (10th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),         &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2, kms:kme, jms:jme)                &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fl10q/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sd1b3i(c,b10qi,                      &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haes3i(c, 2,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fl10q)THEN
p=0.
ELSE
CALL haps3i(c,p, w_l_10q,                 &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma20a3i(c,p,l10q1,l10q2,             &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL ha22sp3i(c,p,l10q1,l10q2,k10q1,k10q2,&
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22a3i(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fl10q)THEN
p=0.
ELSE
CALL hape3i(c,p, w_l_10q,                 &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma20b3i(c,p,l10q1,l10q2,             &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL ha22ep3i(c,p,l10q1,l10q2,k10q1,k10q2,&
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22b3i(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc10qi3i

!============================================================================
SUBROUTINE hc10q3i(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "10q" scheme, for 3-dimensional array, active index i
! (10th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),         &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2, kms:kme, jms:jme)                &
                         :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fk10q/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes3i(c, 2,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(f==fk10q)THEN
p=0.
ELSE
CALL haps3i(c,p, w_k_10q,                 &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma20a3i(c,p,k10q1,k10q2,             &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL ha22sp3i(c,p,k10q1,k10q2,l10q1,l10q2,&
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22a3i(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(f==fk10q)THEN
p=0.
ELSE
CALL hape3i(c,p, w_k_10q,                 &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma20b3i(c,p,k10q1,k10q2,             &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL ha22ep3i(c,p,k10q1,k10q2,l10q1,l10q2,&
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22b3i(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hasz3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3i(c,b10q,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc10q3i

!============================================================================
SUBROUTINE hc4t3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "4t" scheme, for 3-dimensional array, active index i
! (4th-order invertible translation for A to C grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),         &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1, kms:kme, jms:jme)                &
                         :: p
  INTEGER                :: fl,fk,itfs,itfe
!============================================================================
fl=MIN(ite+1-its, fl4t)
fk=MIN(ite+1-its, fk4t)
itfs=ite+1-fl
itfe=its-1+fk
!------------------
CALL haes3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(fl==fl4t)THEN
p=0.
ELSE
CALL haps3i(c,p, w_l_4t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma10a3i(c,p,l4t1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL ha11sp3i(c,p,l4t1,l4t1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1a3i(c,  l4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(fk==fk4t)THEN
p=0.
ELSE
CALL hape3i(c,p, w_k_4t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma10b3i(c,p,k4t1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL ha11ep3i(c,p,k4t1,k4t1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1b3i(c,  k4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc4t3i

!============================================================================
SUBROUTINE hc4ti3i(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "4ti" scheme, for 3-dimensional array, active index i
! (4th-order invertible translation for C to A grid; inverse of 4t)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),         &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1, kms:kme, jms:jme)                &
                         :: p
  INTEGER                :: fl,fk,itfs,itfe
!============================================================================
fl=MIN(ite+1-its, fl4t)
fk=MIN(ite+1-its, fk4t)
itfs=ite+1-fk
itfe=its-1+fl
!------------------
CALL haes3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(fk==fk4t)THEN
p=0.
ELSE
CALL haps3i(c,p, w_k_4t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma10a3i(c,p,k4t1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL ha11sp3i(c,p,k4t1,k4t1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1a3i(c,  k4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(fl==fl4t)THEN
p=0.
ELSE
CALL hape3i(c,p, w_l_4t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma10b3i(c,p,l4t1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL ha11ep3i(c,p,l4t1,l4t1,              &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1b3i(c,  l4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc4ti3i

!============================================================================
SUBROUTINE hc8t3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "8t" scheme, for 3-dimensional array, active index i
! (8th-order invertible translation for A to C grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),         &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2, kms:kme, jms:jme)                &
                         :: p
  INTEGER                :: fl,fk,itfs,itfe
!============================================================================
fl=2*MIN((ite+1-its)/2, fl8t/2)
fk=2*MIN((ite+1-its)/2, fk8t/2)
itfs=ite+1-fl
itfe=its-1+fk
!------------------
CALL haes3i(c, 2,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(fl==fl8t)THEN
p=0.
ELSE
CALL haps3i(c,p, w_l_8t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma20a3i(c,p,l8t1,l8t2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL ha22sp3i(c,p,l8t1,l8t2,l8t1,l8t2,    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2a3i(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(fk==fk8t)THEN
p=0.
ELSE
CALL hape3i(c,p, w_k_8t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma20b3i(c,p,k8t1,k8t2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL ha22ep3i(c,p,k8t1,k8t2,k8t1,k8t2,    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2b3i(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc8t3i

!============================================================================
SUBROUTINE hc8ti3i(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal cyclic "8ti" scheme, for 3-dimensional array, active index i
! (4th-order invertible translation for C to A grid; inverse of 8t)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),         &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2, kms:kme, jms:jme)                &
                         :: p
  INTEGER                :: fl,fk,itfs,itfe
!============================================================================
fl=2*MIN((ite+1-its)/2, fl8t/2)
fk=2*MIN((ite+1-its)/2, fk8t/2)
itfs=ite+1-fk
itfe=its-1+fl
!------------------
CALL haes3i(c, 2,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

IF(fk==fk8t)THEN
p=0.
ELSE
CALL haps3i(c,p, w_k_8t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      
ENDIF

CALL ma20a3i(c,p,k8t1,k8t2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     itfs,ite, jts,jte,  kts,kte)      

CALL ha22sp3i(c,p,k8t1,k8t2,k8t1,k8t2,    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2a3i(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
IF(fl==fl8t)THEN
p=0.
ELSE
CALL hape3i(c,p, w_l_8t,                  &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      
ENDIF

CALL ma20b3i(c,p,l8t1,l8t2,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,itfe, jts,jte,  kts,kte)      

CALL ha22ep3i(c,p,l8t1,l8t2,l8t1,l8t2,    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2b3i(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hc8ti3i

!============================================================================
SUBROUTINE hc1rf3i(c, al1,frf,amp,        &       
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!============================================================================
! Horizontal cyclic "1rf" scheme, for 3-dimensional array, active index i
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
  REAL, DIMENSION(1, kms:kme, jms:jme) &
                         :: p
  REAL                   :: beta
  INTEGER                :: i,j,k,f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, frf)
itfs=ite+1-f
itfe=its-1+f
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
CALL ma10a3i(c,p,al1,                     &  
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       itfs,ite, jts,jte, kts,kte)  

CALL hasp3i(c,p,                          &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  

CALL mf10a3i(c,  al1,                     &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  

p=0
CALL ma10b3i(c,p,al1,                     &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,itfe, jts,jte, kts,kte)  

CALL haep3i(c,p,                          &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  

CALL mf10b3i(c,  al1,                     &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  
END SUBROUTINE hc1rf3i

!============================================================================
SUBROUTINE hc2rf3i(c,al1,al2,frf,amp,     &       
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!============================================================================
! Horizontal cyclic "2rf" scheme, for 3-dimensional array, active index i
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
  REAL, DIMENSION(2, kms:kme, jms:jme)    &
                         :: p
  REAL                   :: beta
  INTEGER                :: i,j,k,f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, frf)
itfs=ite+1-f
itfe=its-1+f
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
CALL ma20a3i(c,p,al1,al2,                 &  
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       itfs,ite, jts,jte, kts,kte)  

CALL hasp3i(c,p,                          &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  

CALL mf20a3i(c,  al1,al2,                 &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  

p=0
CALL ma20b3i(c,p,al1,al2,                 &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,itfe, jts,jte, kts,kte)  

CALL haep3i(c,p,                          &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  

CALL mf20b3i(c,  al1,al2,                 &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)  
END SUBROUTINE hc2rf3i


!============================================================================
!  R. J. Purser, jpurser@ncep.noaa.gov
! Intermediate level routines performing various compact operations
! for horizontally bounded domains.
!============================================================================
SUBROUTINE hg4m3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "4m" scheme, for 3-dimensional array, active index i
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
CALL sa1a3i(c,b4m,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hass3i(c, w_l_4m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10a3i(c,  l4m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wll_4m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10b3i(c,  l4m1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg4m3i 

!============================================================================
SUBROUTINE hg6m3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "6m" scheme, for 3-dimensional array, active index i
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
CALL sa1a3i(c,b6m,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wk__6m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3i(c, w_lk6m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11a3i(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wllk6m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11b3i(c,  l6m1,k6m1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg6m3i 

!============================================================================
SUBROUTINE hg8m3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "8m" scheme, for 3-dimensional array, active index i
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
CALL sa1a3i(c,b8m,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wk__8m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3i(c, w_lk8m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21a3i(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wllk8m,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21b3i(c,  l8m1,l8m2,k8m1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg8m3i

!============================================================================
SUBROUTINE hg10m3i(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "10m" scheme, for 3-dimensional array, active index i
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
CALL sa1a3i(c,b10m,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wk__10m,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3i(c, w_lk10m,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22a3i(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wllk10m,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22b3i(c,  l10m1,l10m2,k10m1,k10m2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg10m3i

!============================================================================
SUBROUTINE hg4d3i(c,d,                                                      &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "4d" scheme, for 3-dimensional array, active index i
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
CALL hass3i(c, wi__4d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL haee3i(c, wi__4d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL ud1c3i(c,d,b4d,                      &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hass3i(d, w_l_4d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10a3i(d,  l4d1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(d, wll_4d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10b3i(d,  l4d1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg4d3i 

!============================================================================
SUBROUTINE hg8d3i(c,d,                                                      &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "8d" scheme, for 3-dimensional array, active index i
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
CALL hass3i(c, wi__8d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL haee3i(c, wi__8d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL ud1c3i(c,d,b8d,                      &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(d, wk__8d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3i(d, w_lk8d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21a3i(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(d, wllk8d,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21b3i(d,  l8d1,l8d2,k8d1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg8d3i

!============================================================================
SUBROUTINE hg4qi3i(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "4qi" scheme, for 3-dimensional array, active index i
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
CALL sd1b3i(c,b4qi,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hass3i(c, w_l_4q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10a3i(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wll_4q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf10b3i(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg4qi3i

!============================================================================
SUBROUTINE hg4q3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "4q" scheme, for 3-dimensional array, active index i
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
CALL haee3i(c, wl__4q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf01a3i(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hass3i(c, w___4q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf01b3i(c,  l4q1,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hasz3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3i(c,b4q,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg4q3i

!============================================================================
SUBROUTINE hg6qi3i(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "6qi" scheme, for 3-dimensional array, active index i
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
CALL sd1b3i(c,b6qi,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wk__6q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3i(c, w_lk6q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11a3i(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wllk6q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11b3i(c,  l6q1,k6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg6qi3i

!============================================================================
SUBROUTINE hg6q3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "6q" scheme, for 3-dimensional array, active index i
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
CALL haee3i(c, wl__6q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3i(c, w_kl6q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11a3i(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wkkl6q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf11b3i(c,  k6q1,l6q1,               &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hasz3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3i(c,b6q,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg6q3i

!============================================================================
SUBROUTINE hg8qi3i(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "8qi" scheme, for 3-dimensional array, active index i
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
CALL sd1b3i(c,b8qi,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wk__8q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3i(c, w_lk8q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21a3i(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wllk8q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf21b3i(c,  l8q1,l8q2,k8q1,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg8qi3i

!============================================================================
SUBROUTINE hg8q3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "8q" scheme, for 3-dimensional array, active index i
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
CALL haee3i(c, wl__8q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3i(c, w_kl8q,                    & ! (2-wide halo for later b-filter) 
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf12a3i(c,   k8q1,l8q1,l8q2,         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wkkl8q,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf12b3i(c,  k8q1,l8q1,l8q2,          &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hasz3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3i(c,b8q,                        &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg8q3i

!============================================================================
SUBROUTINE hg10qi3i(c,                                                      &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "10qi" scheme, for 3-dimensional array, active index i
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
CALL sd1b3i(c,b10qi,                      &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wk__10q,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3i(c, w_lk10q,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22a3i(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wllk10q,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22b3i(c,  l10q1,l10q2,k10q1,k10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg10qi3i

!============================================================================
SUBROUTINE hg10q3i(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "10q" scheme, for 3-dimensional array, active index i
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
CALL haee3i(c, wl__10q,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3i(c, w_kl10q,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22a3i(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wkkl10q,                   &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL mf22b3i(c,  k10q1,k10q2,l10q1,l10q2, &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL hasz3i(c, 1,                         &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL sq1a3i(c,b10q,                       &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg10q3i

!============================================================================
SUBROUTINE hg4t3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "4t" scheme, for 3-dimensional array, active index i
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
CALL haee3i(c, wk__4t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3i(c, w_ll4t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1a3i(c,  l4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wlkk4t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1b3i(c,  k4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg4t3i

!============================================================================
SUBROUTINE hg4ti3i(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "4ti" scheme, for 3-dimensional array, active index i
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
CALL haee3i(c, wl__4t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3i(c, w_kk4t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1a3i(c,  k4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wkll4t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf1b3i(c,  l4t1,                     &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg4ti3i

!============================================================================
SUBROUTINE hg8t3i(c,                                                        &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "8t" scheme, for 3-dimensional array, active index i
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
CALL haee3i(c, wk__8t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3i(c, w_ll8t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2a3i(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wlkk8t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2b3i(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg8t3i

!============================================================================
SUBROUTINE hg8ti3i(c,                                                       &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal "8ti" scheme, for 3-dimensional array, active index i
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
CALL haee3i(c, wl__8t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL hass3i(c, w_kk8t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2a3i(c,  k8t1,k8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
!------------------
CALL haee3i(c, wkll8t,                    &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      

CALL tf2b3i(c,  l8t1,l8t2,                &
     ids,ide,  jds,jde,  kds,kde,         &
     ims,ime,  jms,jme,  kms,kme,         &
     its,ite,  jts,jte,  kts,kte)      
END SUBROUTINE hg8ti3i
!END MODULE MODULE_hops3i
