!						**********************
!						*  module_hops1i.f90 *
!						*  PURSER 2001       *
! 						**********************
!
!============================================================================
!  R. J. Purser, jpurser@ncep.noaa.gov
!  Routines associated with compact horizontal operators for 1-dimensional
!  arrays, index i active.
!
!  Halo routines (ha....) and other low-level filtering operations 
!  are followed by intermediate-level routines for operations on cyclic grid
!  (hc....) and bounded grid (hg....) domains.
!
!
!  Routines to fill lateral haloes in various ways
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
!MODULE MODULE_hops1i
!CONTAINS

!============================================================================
SUBROUTINE hase1i(a, nhalo,                                                 &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE
  INTEGER,                  INTENT(IN   ) :: nhalo
  INTEGER,                  INTENT(IN   ) :: ids,ide
  INTEGER,                  INTENT(IN   ) :: ims,ime
  INTEGER,                  INTENT(IN   ) :: its,ite
  REAL, DIMENSION(ims:ime), INTENT(INOUT) :: a
!----------------------------------------------------------------------------
  INTEGER                                 :: i
!============================================================================
IF(nhalo > its-ims)STOP 'In hase1i; nhalo wider than memory dimension allows'
DO i=1,nhalo
   a(its-i)=a(ite+1-i)
ENDDO
END SUBROUTINE hase1i

!============================================================================
SUBROUTINE haes1i(a, nhalo,                                                 &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE
  INTEGER,                  INTENT(IN   ) :: nhalo
  INTEGER,                  INTENT(IN   ) :: ids,ide
  INTEGER,                  INTENT(IN   ) :: ims,ime
  INTEGER,                  INTENT(IN   ) :: its,ite
  REAL, DIMENSION(ims:ime), INTENT(INOUT) :: a
!----------------------------------------------------------------------------
  INTEGER                                 :: i
!============================================================================
IF(nhalo > ime-ite)STOP 'In haes1i; nhalo wider than memory dimension allows'
DO i=1,nhalo
   a(ite+i)=a(its-1+i)
ENDDO
END SUBROUTINE haes1i

!============================================================================
SUBROUTINE hass1i(a, wext,                                                  &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,                    INTENT(IN   ) :: ids,ide
  INTEGER,                    INTENT(IN   ) :: ims,ime
  INTEGER,                    INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),   INTENT(INOUT) :: a

  REAL, DIMENSION(:,:),       INTENT(IN   ) :: wext
!----------------------------------------------------------------------------
  INTEGER                                   :: in,im
  INTEGER                                   :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
IF(nhalo > its-ims)STOP 'In hass1i; nhalo wider than memory dimension allows'
DO in=1,nhalo
   a(its-in)=0.
   DO im=1,mhalo
      a(its-in)=a(its-in)+wext(in,im)*a(its-1+im)
   ENDDO
ENDDO
! i.e., a(its-1:its-nhalo:-1)=matmul(wext,a(its:its-1+mhalo)
END SUBROUTINE hass1i

!============================================================================
SUBROUTINE haps1i(a,p,  wext,                                               &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,                    INTENT(IN   ) :: ids,ide
  INTEGER,                    INTENT(IN   ) :: ims,ime
  INTEGER,                    INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),   INTENT(IN   ) :: a
  REAL, DIMENSION(:),         INTENT(OUT  ) :: p

  REAL, DIMENSION(:,:),       INTENT(IN   ) :: wext
!----------------------------------------------------------------------------
  INTEGER                                   :: in,im
  INTEGER                                   :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
DO in=1,nhalo
   p(in)=0.
   DO im=1,mhalo
      p(in)=p(in)+wext(in,im)*a(its-1+im)
   ENDDO
ENDDO
END SUBROUTINE haps1i

!============================================================================
SUBROUTINE haee1i(a ,wext,                                                  &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,                    INTENT(IN   ) :: ids,ide
  INTEGER,                    INTENT(IN   ) :: ims,ime
  INTEGER,                    INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),   INTENT(INOUT) :: a

  REAL, DIMENSION(:,:),       INTENT(IN   ) :: wext

!----------------------------------------------------------------------------
  INTEGER                                   :: in,im
  INTEGER                                   :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
IF(nhalo > ime-ite)STOP 'In haee1i; nhalo wider than memory dimension allows'
DO in=1,nhalo
   a(ite+in)=0.
   DO im=1,mhalo
      a(ite+in)=a(ite+in)+wext(in,im)*a(ite+1-im)
   ENDDO
ENDDO
! i.e., a(ite+1:ite+nhalo)=matmul(wext,a(ite:ite+1-mhalo:-1)
END SUBROUTINE haee1i

!============================================================================
SUBROUTINE hape1i(a,p, wext,                                                &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,                    INTENT(IN   ) :: ids,ide
  INTEGER,                    INTENT(IN   ) :: ims,ime
  INTEGER,                    INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),   INTENT(IN   ) :: a
  REAL, DIMENSION(:),         INTENT(OUT  ) :: p

  REAL,DIMENSION(:,:),        INTENT(IN   ) :: wext

!----------------------------------------------------------------------------
  INTEGER                                   :: in,im
  INTEGER                                   :: nhalo,mhalo
!============================================================================
nhalo=SIZE(wext,1)
mhalo=SIZE(wext,2)
DO in=1,nhalo
   p(in)=0.
   DO im=1,mhalo
      p(in)=p(in)+wext(in,im)*a(ite+1-im)
   ENDDO
ENDDO
END SUBROUTINE hape1i

!============================================================================
SUBROUTINE hasp1i(a,p,                                                      &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE
  INTEGER,                  INTENT(IN   ) :: ids,ide
  INTEGER,                  INTENT(IN   ) :: ims,ime
  INTEGER,                  INTENT(IN   ) :: its,ite
  REAL, DIMENSION(ims:ime), INTENT(INOUT) :: a
  REAL, DIMENSION(:),       INTENT(IN   ) :: p
!----------------------------------------------------------------------------
  INTEGER                                 :: i
  INTEGER                                 :: nhalo
!============================================================================
nhalo=SIZE(p)
IF(nhalo > its-ims)STOP 'In hasp1i; nhalo wider than memory dimension allows'
DO i=1,nhalo
   a(its-i)=p(i)
ENDDO
END SUBROUTINE hasp1i

!============================================================================
SUBROUTINE ha11sp1i(a,p,lq1,kq1,                                            &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE
  INTEGER,                  INTENT(IN   ) :: ids,ide
  INTEGER,                  INTENT(IN   ) :: ims,ime
  INTEGER,                  INTENT(IN   ) :: its,ite
  REAL, DIMENSION(ims:ime), INTENT(INOUT) :: a
  REAL, DIMENSION(1),       INTENT(INOUT) :: p
  REAL,                     INTENT(IN   ) :: lq1,kq1
!============================================================================
IF(1 > its-ims)STOP 'In ha11sp1i; nhalo wider than memory dimension allows'
a(its-1)=p(1)
p(1    )=a(its  )-lq1*p(1)
a(its-1)=a(its-1)+kq1*p(1)
END SUBROUTINE ha11sp1i

!============================================================================
SUBROUTINE ha12sp1i(a,p,lq1,kq1,kq2,                                        &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE
  INTEGER,                  INTENT(IN   ) :: ids,ide
  INTEGER,                  INTENT(IN   ) :: ims,ime
  INTEGER,                  INTENT(IN   ) :: its,ite
  REAL, DIMENSION(ims:ime), INTENT(INOUT) :: a
  REAL, DIMENSION(1),       INTENT(INOUT) :: p
  REAL,                     INTENT(IN   ) :: lq1,kq1,kq2
!============================================================================
IF(1 > its-ims)STOP 'In ha12sp1i; nhalo wider than memory dimension allows'
a(its-1)=p(1)
p(1    )=a(its  )-lq1*p(1)
a(its-1)=a(its-1)+kq1*p(1)
p(1    )=a(its+1)-lq1*p(1)
a(its-1)=a(its-1)+kq2*p(1)
END SUBROUTINE ha12sp1i

!============================================================================
SUBROUTINE ha21sp1i(a,p,lq1,lq2,kq1,                                        &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE
  INTEGER,                  INTENT(IN   ) :: ids,ide
  INTEGER,                  INTENT(IN   ) :: ims,ime
  INTEGER,                  INTENT(IN   ) :: its,ite
  REAL, DIMENSION(ims:ime), INTENT(INOUT) :: a
  REAL, DIMENSION(2),       INTENT(INOUT) :: p
  REAL,                     INTENT(IN   ) :: lq1,lq2,kq1
!============================================================================
IF(2 > its-ims)STOP 'In ha21sp1i; nhalo wider than memory dimension allows'
a(its-2)=p(2    )+kq1*p(1)
a(its-1)=p(1    )
p(2    )=a(its  )-lq1*p(1)-lq2*p(2)
a(its-1)=a(its-1)+kq1*p(2)
END SUBROUTINE ha21sp1i

!============================================================================
SUBROUTINE ha22sp1i(a,p,lq1,lq2,kq1,kq2,                                    &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE
  INTEGER,                  INTENT(IN   ) :: ids,ide
  INTEGER,                  INTENT(IN   ) :: ims,ime
  INTEGER,                  INTENT(IN   ) :: its,ite
  REAL, DIMENSION(ims:ime), INTENT(INOUT) :: a
  REAL, DIMENSION(2),       INTENT(INOUT) :: p
  REAL,                     INTENT(IN   ) :: lq1,lq2,kq1,kq2
!============================================================================
IF(2 > its-ims)STOP 'In ha22sp1i; nhalo wider than memory dimension allows'
a(its-2)=p(2    )+kq1*p(1)
a(its-1)=p(1    )
p(2    )=a(its  )-lq1*p(1)-lq2*p(2)
a(its-2)=a(its-2)+kq2*p(2)
a(its-1)=a(its-1)+kq1*p(2)
p(1    )=a(its+1)-lq1*p(2)-lq2*p(1)
a(its-1)=a(its-1)+kq2*p(1)
END SUBROUTINE ha22sp1i

!============================================================================
SUBROUTINE haep1i(a,p,                                                      &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE
  INTEGER,                  INTENT(IN   ) :: ids,ide
  INTEGER,                  INTENT(IN   ) :: ims,ime
  INTEGER,                  INTENT(IN   ) :: its,ite
  REAL, DIMENSION(ims:ime), INTENT(INOUT) :: a
  REAL, DIMENSION(:),       INTENT(IN   ) :: p
!----------------------------------------------------------------------------
  INTEGER                                 :: i
  INTEGER                                 :: nhalo
!============================================================================
nhalo=SIZE(p)
IF(nhalo > ime-ite)STOP 'In haep1i; nhalo wider than memory dimension allows'
DO i=1,nhalo
   a(ite+i)=p(i)
ENDDO
END SUBROUTINE haep1i

!============================================================================
SUBROUTINE ha11ep1i(a,p,lq1,kq1,                                            &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE
  INTEGER,                  INTENT(IN   ) :: ids,ide
  INTEGER,                  INTENT(IN   ) :: ims,ime
  INTEGER,                  INTENT(IN   ) :: its,ite
  REAL, DIMENSION(ims:ime), INTENT(INOUT) :: a
  REAL, DIMENSION(1),       INTENT(INOUT) :: p
  REAL,                     INTENT(IN   ) :: lq1,kq1
!============================================================================
IF(1 > ime-ite)STOP 'In ha11ep1i; nhalo wider than memory dimension allows'
a(ite+1)=p(1)
p(1    )=a(ite  )-lq1*p(1)
a(ite+1)=a(ite+1)+kq1*p(1)
END SUBROUTINE ha11ep1i

!============================================================================
SUBROUTINE ha12ep1i(a,p,lq1,kq1,kq2,                                        &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE
  INTEGER,                  INTENT(IN   ) :: ids,ide
  INTEGER,                  INTENT(IN   ) :: ims,ime
  INTEGER,                  INTENT(IN   ) :: its,ite
  REAL, DIMENSION(ims:ime), INTENT(INOUT) :: a
  REAL, DIMENSION(1),       INTENT(INOUT) :: p
  REAL,                     INTENT(IN   ) :: lq1,kq1,kq2
!============================================================================
IF(1 > ime-ite)STOP 'In ha12ep1i; nhalo wider than memory dimension allows'
a(ite+1)=p(1)
p(1    )=a(ite  )-lq1*p(1)
a(ite+1)=a(ite+1)+kq1*p(1)
p(1    )=a(ite-1)-lq1*p(1)
a(ite+1)=a(ite+1)+kq2*p(1)
END SUBROUTINE ha12ep1i

!============================================================================
SUBROUTINE ha21ep1i(a,p,lq1,lq2,kq1,                                        &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE
  INTEGER,                  INTENT(IN   ) :: ids,ide
  INTEGER,                  INTENT(IN   ) :: ims,ime
  INTEGER,                  INTENT(IN   ) :: its,ite
  REAL, DIMENSION(ims:ime), INTENT(INOUT) :: a
  REAL, DIMENSION(2),       INTENT(INOUT) :: p
  REAL,                     INTENT(IN   ) :: lq1,lq2,kq1
!============================================================================
IF(2 > ime-ite)STOP 'In ha21ep1i; nhalo wider than memory dimension allows'
a(ite+2)=p(2    )+kq1*p(1)
a(ite+1)=p(1    )
p(2    )=a(ite  )-lq1*p(1)-lq2*p(2)
a(ite+1)=a(ite+1)+kq1*p(2)
END SUBROUTINE ha21ep1i

!============================================================================
SUBROUTINE ha22ep1i(a,p,lq1,lq2,kq1,kq2,                                    &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE
  INTEGER,                  INTENT(IN   ) :: ids,ide
  INTEGER,                  INTENT(IN   ) :: ims,ime
  INTEGER,                  INTENT(IN   ) :: its,ite
  REAL, DIMENSION(ims:ime), INTENT(INOUT) :: a
  REAL, DIMENSION(2),       INTENT(INOUT) :: p
  REAL,                     INTENT(IN   ) :: lq1,lq2,kq1,kq2
!============================================================================
IF(2 > ime-ite)STOP 'In ha22ep1i; nhalo wider than memory dimension allows'
a(ite+2)=p(2    )+kq1*p(1)
a(ite+1)=p(1    )
p(2    )=a(ite  )-lq1*p(1)-lq2*p(2)
a(ite+2)=a(ite+2)+kq2*p(2)
a(ite+1)=a(ite+1)+kq1*p(2)
p(1    )=a(ite-1)-lq1*p(2)-lq2*p(1)
a(ite+1)=a(ite+1)+kq2*p(1)
END SUBROUTINE ha22ep1i

!============================================================================
SUBROUTINE hasz1i(a,  nhalo,                                                &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE
  INTEGER,                  INTENT(IN   ) :: nhalo
  INTEGER,                  INTENT(IN   ) :: ids,ide
  INTEGER,                  INTENT(IN   ) :: ims,ime
  INTEGER,                  INTENT(IN   ) :: its,ite
  REAL, DIMENSION(ims:ime), INTENT(INOUT) :: a
!----------------------------------------------------------------------------
  INTEGER                                 :: i
!============================================================================
IF(nhalo > its-ims)STOP 'In hasz1i; nhalo wider than memory dimension allows'
DO i=1,nhalo
   a(its-i)=0.
ENDDO
END SUBROUTINE hasz1i 

!============================================================================
SUBROUTINE haez1i(a,  nhalo,                                                &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE
  INTEGER,                  INTENT(IN   ) :: nhalo
  INTEGER,                  INTENT(IN   ) :: ids,ide
  INTEGER,                  INTENT(IN   ) :: ims,ime
  INTEGER,                  INTENT(IN   ) :: its,ite
  REAL, DIMENSION(ims:ime), INTENT(INOUT) :: a
!----------------------------------------------------------------------------
  INTEGER                                 :: i
!============================================================================
IF(nhalo > ime-ite)STOP 'In haez1i; nhalo wider than memory dimension allows'
DO i=1,nhalo
   a(ite+i)=0.
ENDDO
END SUBROUTINE haez1i 
!============================================================================

!============================================================================
!  R. J. Purser, jpurser@ncep.noaa.gov
! Horizontal basic filtering numerical operations for 1-dimensional arrays.
! These are the building blocks for compact and conventional 
! horizontal differencing, quadrature, midpoint interpolation
! and filtering.
!============================================================================
!============================================================================
SUBROUTINE ud1c1i(c,d,bnd1,                                                 &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(IN   )    :: c                   

  REAL, DIMENSION(ims:ime),       &
                INTENT(OUT  )    :: d    
               
  REAL,         INTENT(IN   )    :: bnd1
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite
   d(i)=bnd1*(c(i+1)-c(i-1))
ENDDO
END SUBROUTINE ud1c1i

!============================================================================
SUBROUTINE ud2c1i(c,d,bnd1,bnd2,                                            &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(IN   )    :: c                   

  REAL, DIMENSION(ims:ime),       &
                INTENT(OUT  )    :: d    
               
  REAL,         INTENT(IN   )    :: bnd1,bnd2
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite
   d(i)=bnd1*(c(i+1)-c(i-1))+bnd2*(c(i+2)-c(i-2))
ENDDO
END SUBROUTINE ud2c1i

!============================================================================
SUBROUTINE sa1a1i(a,bnm1,                                                   &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: bnm1
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite
   a(i)=bnm1*(a(i)+a(i+1))
ENDDO
END SUBROUTINE sa1a1i

!============================================================================
SUBROUTINE sq1a1i(a,bnq,                                                    &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: bnq
!---------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite
   a(i)=a(i-1)+bnq*a(i)
ENDDO
END SUBROUTINE sq1a1i

!============================================================================
SUBROUTINE sd1b1i(a,bnqi,                                                   &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: bnqi
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=ite,its,-1
   a(i)=bnqi*(a(i)-a(i-1))
ENDDO
END SUBROUTINE sd1b1i

!============================================================================
SUBROUTINE mf01a1i(a,knq1,                                                  &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite
   a(i)=a(i)+knq1*a(i+1)
ENDDO
END SUBROUTINE mf01a1i

!============================================================================
SUBROUTINE mf02a1i(a,knq1,knq2,                                             &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite
   a(i)=a(i)+knq1*a(i+1)+knq2*a(i+2)
ENDDO
END SUBROUTINE mf02a1i

!============================================================================
SUBROUTINE mf10a1i(a,lnq1,                                                  &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite
   a(i)=a(i)-lnq1*a(i-1)
ENDDO
END SUBROUTINE mf10a1i

!============================================================================
SUBROUTINE mf11a1i(a,lnq1,knq1,                                             &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite
   a(i)=a(i)-lnq1*a(i-1)+knq1*a(i+1)
ENDDO
END SUBROUTINE mf11a1i

!============================================================================
SUBROUTINE mf12a1i(a,lnq1,knq1,knq2,                                        &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite
   a(i)=a(i)-lnq1*a(i-1)    &
            +knq1*a(i+1)+knq2*a(i+2)
ENDDO
END SUBROUTINE mf12a1i

!============================================================================
SUBROUTINE mf20a1i(a,lnq1,lnq2,                                             &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite
   a(i)=a(i)-lnq1*a(i-1)-lnq2*a(i-2)
ENDDO
END SUBROUTINE mf20a1i

!============================================================================
SUBROUTINE mf21a1i(a,lnq1,lnq2,knq1,                                        &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2,knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite
   a(i)=a(i)-lnq1*a(i-1)-lnq2*a(i-2)+knq1*a(i+1)
ENDDO
END SUBROUTINE mf21a1i

!============================================================================
SUBROUTINE mf22a1i(a,lnq1,lnq2,knq1,knq2,                                   &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2,knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite
   a(i)=a(i)-lnq1*a(i-1)-lnq2*a(i-2) &
            +knq1*a(i+1)+knq2*a(i+2)
ENDDO
END SUBROUTINE mf22a1i

!============================================================================
SUBROUTINE tf1a1i(a,lnq1,                                                   &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite
   a(i)=a(i)+lnq1*(a(i+1)-a(i-1))
ENDDO
END SUBROUTINE tf1a1i

!============================================================================
SUBROUTINE tf2a1i(a,lnq1,lnq2,                                              &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite
   a(i)=a(i)+lnq1*(a(i+1)-a(i-1))+lnq2*(a(i+2)-a(i-2))
ENDDO
END SUBROUTINE tf2a1i

!============================================================================
SUBROUTINE ma10a1i(a,p,lnq1,                                                &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(IN   )    :: a

  REAL, DIMENSION(1),             &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite
   p(1)=a(i)-lnq1*p(1)
ENDDO
END SUBROUTINE ma10a1i

!============================================================================
SUBROUTINE ma20a1i(a,p,lnq1,lnq2,                                           &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(IN   )    :: a   

  REAL, DIMENSION(2),             &    
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=its,ite,2
   p(2)=a(i  )-lnq1*p(1)-lnq2*p(2)
   p(1)=a(i+1)-lnq1*p(2)-lnq2*p(1)
ENDDO
END SUBROUTINE ma20a1i

!============================================================================
SUBROUTINE mf01b1i(a,knq1,                                                  &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=ite,its,-1
   a(i)=a(i)+knq1*a(i-1)
ENDDO
END SUBROUTINE mf01b1i

!============================================================================
SUBROUTINE mf02b1i(a,knq1,knq2,                                             &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=ite,its,-1
   a(i)=a(i)+knq1*a(i-1)+knq2*a(i-2)
ENDDO
END SUBROUTINE mf02b1i

!============================================================================
SUBROUTINE mf10b1i(a,lnq1,                                                  &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=ite,its,-1
   a(i)=a(i)-lnq1*a(i+1)
ENDDO
END SUBROUTINE mf10b1i

!============================================================================
SUBROUTINE mf11b1i(a,lnq1,knq1,                                             &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=ite,its,-1
   a(i)=a(i)-lnq1*a(i+1)+knq1*a(i-1)
ENDDO
END SUBROUTINE mf11b1i

!============================================================================
SUBROUTINE mf12b1i(a,lnq1,knq1,knq2,                                        &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a

  REAL,         INTENT(IN   )    :: lnq1,knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=ite,its,-1
   a(i)=a(i)-lnq1*a(i+1)    &
            +knq1*a(i-1)+knq2*a(i-2)
ENDDO
END SUBROUTINE mf12b1i

!============================================================================
SUBROUTINE mf20b1i(a,lnq1,lnq2,                                             &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=ite,its,-1
   a(i)=a(i)-lnq1*a(i+1)-lnq2*a(i+2)
ENDDO
END SUBROUTINE mf20b1i

!============================================================================
SUBROUTINE mf21b1i(a,lnq1,lnq2,knq1,                                        &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2,knq1
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=ite,its,-1
   a(i)=a(i)-lnq1*a(i+1)-lnq2*a(i+2)+knq1*a(i-1)
ENDDO
END SUBROUTINE mf21b1i

!============================================================================
SUBROUTINE mf22b1i(a,lnq1,lnq2,knq1,knq2,                                   &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2,knq1,knq2
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=ite,its,-1
   a(i)=a(i)-lnq1*a(i+1)-lnq2*a(i+2) &
            +knq1*a(i-1)+knq2*a(i-2)
ENDDO
END SUBROUTINE mf22b1i

!============================================================================
SUBROUTINE tf1b1i(a,lnq1,                                                   &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------  
  INTEGER                        :: i
!============================================================================  
DO i=ite,its,-1
   a(i)=a(i)+lnq1*(a(i-1)-a(i+1))
ENDDO
END SUBROUTINE tf1b1i

!============================================================================
SUBROUTINE tf2b1i(a,lnq1,lnq2,                                              &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(INOUT)    :: a                    

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=ite,its,-1
   a(i)=a(i)+lnq1*(a(i-1)-a(i+1))+lnq2*(a(i-2)-a(i+2))
ENDDO
END SUBROUTINE tf2b1i

!============================================================================
SUBROUTINE ma10b1i(a,p,lnq1,                                                &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(IN   )    :: a                    

  REAL, DIMENSION(1),             &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=ite,its,-1
   p(1)=a(i)-lnq1*p(1)
ENDDO
END SUBROUTINE ma10b1i

!============================================================================
SUBROUTINE ma20b1i(a,p,lnq1,lnq2,                                           &
     ids,ide,                                                               &
     ims,ime,                                                               &
     its,ite                                                                )
!============================================================================
  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    :: ids,ide
  INTEGER,      INTENT(IN   )    :: ims,ime
  INTEGER,      INTENT(IN   )    :: its,ite

  REAL, DIMENSION(ims:ime),       &
                INTENT(IN   )    :: a                    

  REAL, DIMENSION(2),             &
                INTENT(INOUT)    :: p

  REAL,         INTENT(IN   )    :: lnq1,lnq2
!----------------------------------------------------------------------------
  INTEGER                        :: i
!============================================================================
DO i=ite,its,-2
   p(2)=a(i  )-lnq1*p(1)-lnq2*p(2)
   p(1)=a(i-1)-lnq1*p(2)-lnq2*p(1)
ENDDO
END SUBROUTINE ma20b1i
!============================================================================

!============================================================================
! R. J. Purser, jpurser@ncep.noaa.gov
! Intermediate level routines performing various compact operations
! for horizontally cyclic domains.
!============================================================================
SUBROUTINE hc4m1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "4m" scheme, for 1-dimensional array, active index i
! (4th-order midpoint interpolation)
!============================================================================
  !!USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1)     :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fl4m)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL sa1a1i(c,b4m,                        &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(f==fl4m)THEN
p=0.
ELSE
CALL haps1i(c,p, w_l_4m,                  &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF
CALL ma10a1i(c,p,l4m1,                    &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL hasp1i(c,p,                          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf10a1i(c,  l4m1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(f==fl4m)THEN
p=0.
ELSE
CALL hape1i(c,p, w_l_4m,                  &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF
CALL ma10b1i(c,p,l4m1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL haep1i(c,p,                          &  
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf10b1i(c,  l4m1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hc4m1i 

!============================================================================
SUBROUTINE hc6m1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "6m" scheme, for 1-dimensional array, active index i
! (6th-order midpoint interpolation)
!============================================================================
  !!USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1)     :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fl6m)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL sa1a1i(c,b6m,                        &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haes1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

IF(f==fl6m)THEN
p=0.
ELSE
CALL haps1i(c,p, w_l_6m,                  &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF

CALL ma10a1i(c,p,l6m1,                    &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL ha11sp1i(c,p,l6m1,k6m1,              &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf11a1i(c,  l6m1,k6m1,               &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(f==fl6m)THEN
p=0.
ELSE
CALL hape1i(c,p, w_l_6m,                  &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma10b1i(c,p,l6m1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL ha11ep1i(c,p,l6m1,k6m1,              &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf11b1i(c,  l6m1,k6m1,               &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hc6m1i 

!============================================================================
SUBROUTINE hc8m1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "8m" scheme, for 1-dimensional array, active index i
! (8th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2)     :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fl8m/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL sa1a1i(c,b8m,                        &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haes1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

IF(f==fl8m)THEN
p=0.
ELSE
CALL haps1i(c,p, w_l_8m,                  &
       ids,ide,                           &
       ims,ime,                           & 
       itfs,ite) 
ENDIF

CALL ma20a1i(c,p,l8m1,l8m2,               &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL ha21sp1i(c,p,l8m1,l8m2,k8m1,         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf21a1i(c,  l8m1,l8m2,k8m1,          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(f==fl8m)THEN
p=0.
ELSE
CALL hape1i(c,p, w_l_8m,                  &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma20b1i(c,p,l8m1,l8m2,               &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL ha21ep1i(c,p,l8m1,l8m2,k8m1,         &
       ids,ide,                           &
       ims,ime,                           & 
       its,ite)

CALL mf21b1i(c,  l8m1,l8m2,k8m1,          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hc8m1i

!============================================================================
SUBROUTINE hc10m1i(c,                                                       &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "10m" scheme, for 1-dimensional array, active index i
! (10th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2)     :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fl10m/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)    

CALL sa1a1i(c,b10m,                       &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)   
!------------------
CALL haes1i(c, 2,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

IF(f==fl10m)THEN
p=0.
ELSE
CALL haps1i(c,p,  w_l_10m,                &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF

CALL ma20a1i(c,p,l10m1,l10m2,             &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL ha22sp1i(c,p,l10m1,l10m2,k10m1,k10m2,&
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf22a1i(c,  l10m1,l10m2,k10m1,k10m2, &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(f==fl10m)THEN
p=0.
ELSE
CALL hape1i(c,p, w_l_10m,                 &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma20b1i(c,p,l10m1,l10m2,             &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL ha22ep1i(c,p,l10m1,l10m2,k10m1,k10m2,&
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf22b1i(c,  l10m1,l10m2,k10m1,k10m2, &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hc10m1i

!============================================================================
SUBROUTINE hc4d1i(c,d,                                                      &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "4d" scheme, for 1-dimensional array, active index i
! (4th-order differencing on a simple grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
  REAL, DIMENSION(ims:ime),               &
           INTENT(OUT  ) :: d
!----------------------------------------------------------------------------
  REAL, DIMENSION(1)     :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fl4d)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL haes1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL ud1c1i(c,d,b4d,                      &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(f==fl4d)THEN
p=0.
ELSE
CALL haps1i(d,p, w_l_4d,                  &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF

CALL ma10a1i(d,p,l4d1,                    &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL hasp1i(d,p,                          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf10a1i(d,  l4d1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(f==fl4d)THEN
p=0.
ELSE
CALL hape1i(d,p, w_l_4d,                  &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma10b1i(d,p,l4d1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL haep1i(d,p,                          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf10b1i(d,  l4d1,                    &
       ids,ide,                           &
       ims,ime,                           & 
       its,ite)
END SUBROUTINE hc4d1i 

!============================================================================
SUBROUTINE hc8d1i(c,d,                                                      &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "8d" scheme, for 1-dimensional array, active index i
! (8th-order differencing on a simple grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
  REAL, DIMENSION(ims:ime),               &
           INTENT(OUT  ) :: d
!----------------------------------------------------------------------------
  REAL, DIMENSION(2)     :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fl8d/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL haes1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL ud1c1i(c,d,b8d,                      &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haes1i(d, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

IF(f==fl8d)THEN
p=0.
ELSE
CALL haps1i(d,p, w_l_8d,                  &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF

CALL ma20a1i(d,p,l8d1,l8d2,               &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL ha21sp1i(d,p,l8d1,l8d2,k8d1,         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf21a1i(d,  l8d1,l8d2,k8d1,          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(f==fl8d)THEN
p=0.
ELSE
CALL hape1i(d,p, w_l_8d,                  &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma20b1i(d,p,l8d1,l8d2,               &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL ha21ep1i(d,p,l8d1,l8d2,k8d1,         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf21b1i(d,  l8d1,l8d2,k8d1,          &
       ids,ide,                           &
       ims,ime,                           & 
       its,ite)
END SUBROUTINE hc8d1i

!============================================================================
SUBROUTINE hc4qi1i(c,                                                       &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "4qi" scheme, for 1-dimensional array, active index i
! (4th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1)     :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fl4q)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)     

CALL sd1b1i(c,b4qi,                       &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)     
!------------------
IF(f==fl4q)THEN
p=0.
ELSE
CALL haps1i(c,p, w_l_4q,                  &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF

CALL ma10a1i(c,p,l4q1,                    &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 

CALL hasp1i(c,p,                          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)   

CALL mf10a1i(c,  l4q1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)    
!------------------
IF(f==fl4q)THEN
p=0.
ELSE
CALL hape1i(c,p, w_l_4q,                  &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma10b1i(c,p,l4q1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 

CALL haep1i(c,p,                          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)    

CALL mf10b1i(c,  l4q1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)      
END SUBROUTINE hc4qi1i

!============================================================================
SUBROUTINE hc4q1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "4q" scheme, for 1-dimensional array, active index i
! (4th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haes1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf01a1i(c,  l4q1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL hase1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf01b1i(c,  l4q1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL hasz1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL sq1a1i(c,b4q,                        &
       ids,ide,                           &
       ims,ime,                           & 
       its,ite)
END SUBROUTINE hc4q1i

!============================================================================
SUBROUTINE hc6qi1i(c,                                                       &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "6qi" scheme, for 1-dimensional array, active index i
! (6th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1)     :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fl6q)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL sd1b1i(c,b6qi,                       &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haes1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

IF(f==fl6q)THEN
p=0.
ELSE
CALL haps1i(c,p, w_l_6q,                  &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF

CALL ma10a1i(c,p,l6q1,                    &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL ha11sp1i(c,p,l6q1,k6q1,              &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf11a1i(c,  l6q1,k6q1,               &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(f==fl6q)THEN
p=0.
ELSE
CALL hape1i(c,p, w_l_6q,                  &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma10b1i(c,p,l6q1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL ha11ep1i(c,p,l6q1,k6q1,              &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf11b1i(c,  l6q1,k6q1,               &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hc6qi1i

!============================================================================
SUBROUTINE hc6q1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "6q" scheme, for 1-dimensional array, active index i
! (6th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1)     :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fk6q)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

IF(f==fk6q)THEN
p=0.
ELSE
CALL haps1i(c,p, w_k_6q,                  &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF

CALL ma10a1i(c,p,k6q1,                    &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL ha11sp1i(c,p,k6q1,l6q1,              &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf11a1i(c,  k6q1,l6q1,               &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(f==fk6q)THEN
p=0.
ELSE
CALL hape1i(c,p, w_k_6q,                  &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma10b1i(c,p,k6q1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL ha11ep1i(c,p,k6q1,l6q1,              &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf11b1i(c,  k6q1,l6q1,               &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL hasz1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL sq1a1i(c,b6q,                        &
       ids,ide,                           &
       ims,ime,                           & 
       its,ite)
END SUBROUTINE hc6q1i

!============================================================================
SUBROUTINE hc8qi1i(c,                                                       &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "8qi" scheme, for 1-dimensional array, active index i
! (8th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2)     :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fl8q/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL sd1b1i(c,b8qi,                       &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haes1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

IF(f==fl8q)THEN
p=0.
ELSE
CALL haps1i(c,p, w_l_8q,                  &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF

CALL ma20a1i(c,p,l8q1,l8q2,               &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL ha21sp1i(c,p,l8q1,l8q2,k8q1,         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf21a1i(c,  l8q1,l8q2,k8q1,          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(f==fl8q)THEN
p=0.
ELSE
CALL hape1i(c,p, w_l_8q,                  &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma20b1i(c,p,l8q1,l8q2,               &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL ha21ep1i(c,p,l8q1,l8q2,k8q1,         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf21b1i(c,  l8q1,l8q2,k8q1,          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hc8qi1i

!============================================================================
SUBROUTINE hc8q1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "8q" scheme, for 1-dimensional array, active index i
! (8th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1)     :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, fk8q)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes1i(c, 2,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

IF(f==fk8q)THEN
p=0.
ELSE
CALL haps1i(c,p, w_k_8q,                  &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF

CALL ma10a1i(c,p, k8q1,                   &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL ha12sp1i(c,p,k8q1,l8q1,l8q2,         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf12a1i(c,   k8q1,l8q1,l8q2,         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL hase1i(c, 1,                         & ! (widen existing halo)
       ids,ide,                           &
       ims,ime,                           &
       its-1,ite-1)

IF(f==fk8q)THEN
p=0.
ELSE
CALL hape1i(c,p, w_k_8q,                  &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma10b1i(c,p,k8q1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL ha12ep1i(c,p,k8q1,l8q1,l8q2,         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf12b1i(c,  k8q1,l8q1,l8q2,          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL hasz1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL sq1a1i(c,b8q,                        &
       ids,ide,                           &
       ims,ime,                           & 
       its,ite)
END SUBROUTINE hc8q1i

!============================================================================
SUBROUTINE hc10qi1i(c,                                                      &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "10qi" scheme, for 1-dimensional array, active index i
! (10th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2)     :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fl10q/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL hase1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL sd1b1i(c,b10qi,                      &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haes1i(c, 2,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

IF(f==fl10q)THEN
p=0.
ELSE
CALL haps1i(c,p, w_l_10q,                 &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF

CALL ma20a1i(c,p,l10q1,l10q2,             &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL ha22sp1i(c,p,l10q1,l10q2,k10q1,k10q2,&
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf22a1i(c,  l10q1,l10q2,k10q1,k10q2, &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(f==fl10q)THEN
p=0.
ELSE
CALL hape1i(c,p, w_l_10q,                 &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma20b1i(c,p,l10q1,l10q2,             &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL ha22ep1i(c,p,l10q1,l10q2,k10q1,k10q2,&
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf22b1i(c,  l10q1,l10q2,k10q1,k10q2, &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hc10qi1i

!============================================================================
SUBROUTINE hc10q1i(c,                                                       &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "10q" scheme, for 1-dimensional array, active index i
! (10th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2)     :: p
  INTEGER                :: f,itfs,itfe
!============================================================================
f=2*MIN((ite+1-its)/2, fk10q/2)
itfs=ite+1-f
itfe=its-1+f
!------------------
CALL haes1i(c, 2,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

IF(f==fk10q)THEN
p=0.
ELSE
CALL haps1i(c,p, w_k_10q,                 &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF

CALL ma20a1i(c,p,k10q1,k10q2,             &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL ha22sp1i(c,p,k10q1,k10q2,l10q1,l10q2,&
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf22a1i(c,  k10q1,k10q2,l10q1,l10q2, &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(f==fk10q)THEN
p=0.
ELSE
CALL hape1i(c,p, w_k_10q,                 &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma20b1i(c,p,k10q1,k10q2,             &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL ha22ep1i(c,p,k10q1,k10q2,l10q1,l10q2,&
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf22b1i(c,  k10q1,k10q2,l10q1,l10q2, &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL hasz1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL sq1a1i(c,b10q,                       &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hc10q1i

!============================================================================
SUBROUTINE hc4t1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "4t" scheme, for 1-dimensional array, active index i
! (4th-order invertible translation for A to C grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1)     :: p
  INTEGER                :: fl,fk,itfs,itfe
!============================================================================
fl=MIN(ite+1-its, fl4t)
fk=MIN(ite+1-its, fk4t)
itfs=ite+1-fl
itfe=its-1+fk
!------------------
CALL haes1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

IF(fl==fl4t)THEN
p=0.
ELSE
CALL haps1i(c,p, w_l_4t,                  &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF

CALL ma10a1i(c,p,l4t1,                    &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL ha11sp1i(c,p,l4t1,l4t1,              &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL tf1a1i(c,  l4t1,                     &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(fk==fk4t)THEN
p=0.
ELSE
CALL hape1i(c,p, w_k_4t,                  &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma10b1i(c,p,k4t1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL ha11ep1i(c,p,k4t1,k4t1,              &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL tf1b1i(c,  k4t1,                     &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hc4t1i

!============================================================================
SUBROUTINE hc4ti1i(c,                                                       &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "4ti" scheme, for 1-dimensional array, active index i
! (4th-order invertible translation for C to A grid; inverse of 4t)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(1)     :: p
  INTEGER                :: fl,fk,itfs,itfe
!============================================================================
fl=MIN(ite+1-its, fl4t)
fk=MIN(ite+1-its, fk4t)
itfs=ite+1-fk
itfe=its-1+fl
!------------------
CALL haes1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

IF(fk==fk4t)THEN
p=0.
ELSE
CALL haps1i(c,p, w_k_4t,                  &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF

CALL ma10a1i(c,p,k4t1,                    &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL ha11sp1i(c,p,k4t1,k4t1,              &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL tf1a1i(c,  k4t1,                     &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(fl==fl4t)THEN
p=0.
ELSE
CALL hape1i(c,p, w_l_4t,                  &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma10b1i(c,p,l4t1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL ha11ep1i(c,p,l4t1,l4t1,              &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL tf1b1i(c,  l4t1,                     &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hc4ti1i

!============================================================================
SUBROUTINE hc8t1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "8t" scheme, for 1-dimensional array, active index i
! (8th-order invertible translation for A to C grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2)     :: p
  INTEGER                :: fl,fk,itfs,itfe
!============================================================================
fl=2*MIN((ite+1-its)/2, fl8t/2)
fk=2*MIN((ite+1-its)/2, fk8t/2)
itfs=ite+1-fl
itfe=its-1+fk
!------------------
CALL haes1i(c, 2,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

IF(fl==fl8t)THEN
p=0.
ELSE
CALL haps1i(c,p, w_l_8t,                  &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF

CALL ma20a1i(c,p,l8t1,l8t2,               &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL ha22sp1i(c,p,l8t1,l8t2,l8t1,l8t2,    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL tf2a1i(c,  l8t1,l8t2,                &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(fk==fk8t)THEN
p=0.
ELSE
CALL hape1i(c,p, w_k_8t,                  &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma20b1i(c,p,k8t1,k8t2,               &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL ha22ep1i(c,p,k8t1,k8t2,k8t1,k8t2,    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL tf2b1i(c,  k8t1,k8t2,                &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hc8t1i

!============================================================================
SUBROUTINE hc8ti1i(c,                                                       &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal cyclic "8ti" scheme, for 1-dimensional array, active index i
! (4th-order invertible translation for C to A grid; inverse of 8t)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
  REAL, DIMENSION(2)     :: p
  INTEGER                :: fl,fk,itfs,itfe
!============================================================================
fl=2*MIN((ite+1-its)/2, fl8t/2)
fk=2*MIN((ite+1-its)/2, fk8t/2)
itfs=ite+1-fk
itfe=its-1+fl
!------------------
CALL haes1i(c, 2,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

IF(fk==fk8t)THEN
p=0.
ELSE
CALL haps1i(c,p, w_k_8t,                  &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite) 
ENDIF

CALL ma20a1i(c,p,k8t1,k8t2,               &
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL ha22sp1i(c,p,k8t1,k8t2,k8t1,k8t2,    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL tf2a1i(c,  k8t1,k8t2,                &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
IF(fl==fl8t)THEN
p=0.
ELSE
CALL hape1i(c,p, w_l_8t,                  &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe) 
ENDIF

CALL ma20b1i(c,p,l8t1,l8t2,               &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL ha22ep1i(c,p,l8t1,l8t2,l8t1,l8t2,    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL tf2b1i(c,  l8t1,l8t2,                &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hc8ti1i

!============================================================================
SUBROUTINE hc1rf1i(c, al1,frf,amp,        &       
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!============================================================================
! Horizontal cyclic "1rf" scheme, for 1-dimensional array, active index i
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime), &
           INTENT(INOUT) :: c

  REAL,    INTENT(IN   ) :: al1, amp

  INTEGER, INTENT(IN   ) :: frf
!----------------------------------------------------------------------------
  REAL, DIMENSION(1)     :: p
  REAL                   :: beta
  INTEGER                :: i,f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, frf)
itfs=ite+1-f
itfe=its-1+f
beta=(1.+al1)*amp
!------------------
DO i=its,ite
   c(i)=beta*c(i)
ENDDO

p=0
CALL ma10a1i(c,p,al1,                     &  
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL hasp1i(c,p,                          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf10a1i(c,  al1,                     &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

p=0
CALL ma10b1i(c,p,al1,                     &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL haep1i(c,p,                          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf10b1i(c,  al1,                     &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hc1rf1i

!============================================================================
SUBROUTINE hc2rf1i(c,al1,al2,frf,amp,     &       
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!============================================================================
! Horizontal cyclic "2rf" scheme, for 1-dimensional array, active index i
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime), &
           INTENT(INOUT) :: c

  REAL,    INTENT(IN   ) :: al1,al2, amp

  INTEGER, INTENT(IN   ) :: frf
!----------------------------------------------------------------------------
  REAL, DIMENSION(2)     :: p
  REAL                   :: beta
  INTEGER                :: i,f,itfs,itfe
!============================================================================
f=MIN(ite+1-its, frf)
itfs=ite+1-f
itfe=its-1+f
beta=(1.+al1+al2)*amp
!------------------
DO i=its,ite
   c(i)=beta*c(i)
ENDDO

p=0
CALL ma20a1i(c,p,al1,al2,                 &  
       ids,ide,                           &
       ims,ime,                           &
       itfs,ite)

CALL hasp1i(c,p,                          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf20a1i(c,  al1,al2,                 &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

p=0
CALL ma20b1i(c,p,al1,al2,                 &
       ids,ide,                           &
       ims,ime,                           &
       its,itfe)

CALL haep1i(c,p,                          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf20b1i(c,  al1,al2,                 &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hc2rf1i


!============================================================================
! R. J. Purser, jpurser@ncep.noaa.gov
! Intermediate level routines performing various compact operations
! for horizontally bounded domains.
!============================================================================
!============================================================================
SUBROUTINE hg4m1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "4m" scheme, for 1-dimensional array, active index i
! (4th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
!------------------
CALL sa1a1i(c,b4m,                        &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL hass1i(c, w_l_4m,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf10a1i(c,  l4m1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wll_4m,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf10b1i(c,  l4m1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hg4m1i 

!============================================================================
SUBROUTINE hg6m1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "6m" scheme, for 1-dimensional array, active index i
! (6th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sa1a1i(c,b6m,                        &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wk__6m,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL hass1i(c, w_lk6m,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf11a1i(c,  l6m1,k6m1,               &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wllk6m,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf11b1i(c,  l6m1,k6m1,               &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hg6m1i 

!============================================================================
SUBROUTINE hg8m1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "8m" scheme, for 1-dimensional array, active index i
! (8th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sa1a1i(c,b8m,                        &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wk__8m,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL hass1i(c, w_lk8m,                    &
       ids,ide,                           &
       ims,ime,                           & 
       its,ite) 

CALL mf21a1i(c,  l8m1,l8m2,k8m1,          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wllk8m,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf21b1i(c,  l8m1,l8m2,k8m1,          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hg8m1i

!============================================================================
SUBROUTINE hg10m1i(c,                                                       &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "10m" scheme, for 1-dimensional array, active index i
! (10th-order midpoint interpolation)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sa1a1i(c,b10m,                       &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)   

!------------------
CALL haee1i(c, wk__10m,                   &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL hass1i(c, w_lk10m,                   &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf22a1i(c,  l10m1,l10m2,k10m1,k10m2, &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wllk10m,                   &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf22b1i(c,  l10m1,l10m2,k10m1,k10m2, &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hg10m1i

!============================================================================
SUBROUTINE hg4d1i(c,d,                                                      &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "4d" scheme, for 1-dimensional array, active index i
! (4th-order differencing on a simple grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
  REAL, DIMENSION(ims:ime),               &
           INTENT(OUT  ) :: d
!----------------------------------------------------------------------------
!============================================================================
CALL hass1i(c, wi__4d,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL haee1i(c, wi__4d,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL ud1c1i(c,d,b4d,                      &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

!------------------
CALL hass1i(d, w_l_4d,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf10a1i(d,  l4d1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(d, wll_4d,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf10b1i(d,  l4d1,                    &
       ids,ide,                           &
       ims,ime,                           & 
       its,ite)
END SUBROUTINE hg4d1i 

!============================================================================
SUBROUTINE hg8d1i(c,d,                                                      &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "8d" scheme, for 1-dimensional array, active index i
! (8th-order differencing on a simple grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
  REAL, DIMENSION(ims:ime),               &
           INTENT(OUT  ) :: d
!----------------------------------------------------------------------------
!============================================================================
CALL hass1i(c, wi__8d,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL haee1i(c, wi__8d,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL ud1c1i(c,d,b8d,                      &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(d, wk__8d,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL hass1i(d, w_lk8d,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf21a1i(d,  l8d1,l8d2,k8d1,          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(d, wllk8d,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf21b1i(d,  l8d1,l8d2,k8d1,          &
       ids,ide,                           &
       ims,ime,                           & 
       its,ite)
END SUBROUTINE hg8d1i

!============================================================================
SUBROUTINE hg4qi1i(c,                                                       &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "4qi" scheme, for 1-dimensional array, active index i
! (4th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sd1b1i(c,b4qi,                       &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)     
!------------------
CALL hass1i(c, w_l_4q,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf10a1i(c,  l4q1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)    
!------------------
CALL haee1i(c, wll_4q,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf10b1i(c,  l4q1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)      
END SUBROUTINE hg4qi1i

!============================================================================
SUBROUTINE hg4q1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "4q" scheme, for 1-dimensional array, active index i
! (4th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee1i(c, wl__4q,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf01a1i(c,  l4q1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL hass1i(c, w___4q,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf01b1i(c,  l4q1,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL hasz1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL sq1a1i(c,b4q,                        &
       ids,ide,                           &
       ims,ime,                           & 
       its,ite)
END SUBROUTINE hg4q1i

!============================================================================
SUBROUTINE hg6qi1i(c,                                                       &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "6qi" scheme, for 1-dimensional array, active index i
! (6th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sd1b1i(c,b6qi,                       &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wk__6q,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL hass1i(c, w_lk6q,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf11a1i(c,  l6q1,k6q1,               &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wllk6q,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL mf11b1i(c,  l6q1,k6q1,               &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hg6qi1i

!============================================================================
SUBROUTINE hg6q1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "6q" scheme, for 1-dimensional array, active index i
! (6th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee1i(c,wl__6q,                     &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL hass1i(c, w_kl6q,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf11a1i(c,  k6q1,l6q1,               &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wkkl6q,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf11b1i(c,  k6q1,l6q1,               &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL hasz1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL sq1a1i(c,b6q,                        &
       ids,ide,                           &
       ims,ime,                           & 
       its,ite)
END SUBROUTINE hg6q1i

!============================================================================
SUBROUTINE hg8qi1i(c,                                                       &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "8qi" scheme, for 1-dimensional array, active index i
! (8th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sd1b1i(c,b8qi,                       &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wk__8q,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL hass1i(c, w_lk8q,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf21a1i(c,  l8q1,l8q2,k8q1,          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wllk8q,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf21b1i(c,  l8q1,l8q2,k8q1,          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hg8qi1i

!============================================================================
SUBROUTINE hg8q1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "8q" scheme, for 1-dimensional array, active index i
! (8th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee1i(c, wl__8q,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL hass1i(c, w_kl8q,                    & ! (2-wide halo for later b-filter) 
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf12a1i(c,   k8q1,l8q1,l8q2,         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wkkl8q,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf12b1i(c,  k8q1,l8q1,l8q2,          &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL hasz1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL sq1a1i(c,b8q,                        &
       ids,ide,                           &
       ims,ime,                           & 
       its,ite)
END SUBROUTINE hg8q1i

!============================================================================
SUBROUTINE hg10qi1i(c,                                                      &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "10qi" scheme, for 1-dimensional array, active index i
! (10th-order quadrature-inversion, or staggered differencing)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL sd1b1i(c,b10qi,                      &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wk__10q,                   &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL hass1i(c, w_lk10q,                   &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf22a1i(c,  l10q1,l10q2,k10q1,k10q2, &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wllk10q,                   &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf22b1i(c,  l10q1,l10q2,k10q1,k10q2, &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hg10qi1i

!============================================================================
SUBROUTINE hg10q1i(c,                                                       &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "10q" scheme, for 1-dimensional array, active index i
! (10th-order quadrature)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee1i(c, wl__10q,                   &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL hass1i(c, w_kl10q,                   &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf22a1i(c,  k10q1,k10q2,l10q1,l10q2, &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wkkl10q,                   &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL mf22b1i(c,  k10q1,k10q2,l10q1,l10q2, &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL hasz1i(c, 1,                         &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL sq1a1i(c,b10q,                       &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hg10q1i

!============================================================================
SUBROUTINE hg4t1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "4t" scheme, for 1-dimensional array, active index i
! (4th-order invertible translation for A to C grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee1i(c, wk__4t,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL hass1i(c, w_ll4t,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL tf1a1i(c,  l4t1,                     &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wlkk4t,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL tf1b1i(c,  k4t1,                     &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hg4t1i

!============================================================================
SUBROUTINE hg4ti1i(c,                                                       &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "4ti" scheme, for 1-dimensional array, active index i
! (4th-order invertible translation for C to A grid; inverse of 4t)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee1i(c, wl__4t,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL hass1i(c, w_kk4t,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL tf1a1i(c,  k4t1,                     &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wkll4t,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL tf1b1i(c,  l4t1,                     &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hg4ti1i

!============================================================================
SUBROUTINE hg8t1i(c,                                                        &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "8t" scheme, for 1-dimensional array, active index i
! (8th-order invertible translation for A to C grid)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee1i(c, wk__8t,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL hass1i(c, w_ll8t,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL tf2a1i(c,  l8t1,l8t2,                &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wlkk8t,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL tf2b1i(c,  k8t1,k8t2,                &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hg8t1i

!============================================================================
SUBROUTINE hg8ti1i(c,                                                       &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal "8ti" scheme, for 1-dimensional array, active index i
! (4th-order invertible translation for C to A grid; inverse of 8t)
!============================================================================
  !USE MODULE_cpt_consts
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),               &
           INTENT(INOUT) :: c
!----------------------------------------------------------------------------
!============================================================================
CALL haee1i(c, wl__8t,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL hass1i(c, w_kk8t,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite) 

CALL tf2a1i(c,  k8t1,k8t2,                &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
!------------------
CALL haee1i(c, wkll8t,                    &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)

CALL tf2b1i(c,  l8t1,l8t2,                &
       ids,ide,                           &
       ims,ime,                           &
       its,ite)
END SUBROUTINE hg8ti1i

!END MODULE MODULE_hops1i

