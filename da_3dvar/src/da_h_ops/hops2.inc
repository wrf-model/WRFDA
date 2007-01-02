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

