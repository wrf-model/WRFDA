!                                               ***************************
!                                               *  module_set_vops.f90    *
!                                               *  PURSER 2001            *
!                                               ***************************
!MODULE MODULE_set_vops
!-----------------------------------------------------------------------------
! Constants required by various compact numerical schemes along the vertical. 
! These are all initialized by a call to subroutine set_vrt_consts
! These are for standard precision;
! The following compact schemes are accommodated:
!     4m  : 4th-order midpoint interpolation
!     6m  : 6th-order midpoint interpolation
!     8m  : 8th-order midpoint interpolation
!     10m : 10th-ordr midpoint interpolation
!     4d  : 4th-order same-grid differencing
!     8d  : 8th-order same-grid differencing
!     4qi : 4th-order staggered differencing (inverse of 4q)
!     4q  : 4th-order invertible quadrature
!     6qi : 6th-order staggered differencing (inverse of 6q)
!     6q  : 6th-order invertible quadrature
!     8qi : 8th-order staggered differencing (inverse of 8q)
!     8q  : 8th-order invertible quadrature
!     10qi: 10th-ordr staggered differencing (inverse of 10q)
!     10q : 10th-ordr invertible quadrature
!-----------------------------------------------------------------------------
!USE MODULE_pmat2, only: ldlb,udub,ldlb_d,udub_d
!IMPLICIT NONE
!CONTAINS

!=============================================================================
SUBROUTINE set_vrt_consts(n,ch)
!=============================================================================
INTEGER,                INTENT(IN)  :: n
INTEGER                             :: allocatestatus
CHARACTER(len=*),       INTENT(IN)  :: ch
!=============================================================================
SELECT CASE(ch)
CASE('4q')                                                            
 allocate(vl4q1(n),vd4q(n),vd4qi(n), stat = allocatestatus)

 PRINT *, 'setting up for cases 4q and 4qi; allocatestatus=',allocatestatus 
 CALL set_vr4q(n,vl4q1,vd4q,vd4qi)


CASE('6q')
 allocate(vl6q1(n),vd6q(n),vd6qi(n),vk6q1(n),               &
          ve6q(n),ve6qi(n),vm6q1(n),vm6q2(n),               &
                           stat = allocatestatus)
 PRINT *, 'setting up for cases 6q and 6qi; allocatestatus=',allocatestatus
 CALL set_vr6q(n,vl6q1,vd6q,vd6qi,vk6q1,ve6q,ve6qi,vm6q1,vm6q2)

CASE('8q')
 allocate(vl8q1(n),vl8q2(n),vd8q(n),vd8qi(n),               &
          vk8q1(n),ve8q(n),ve8qi(n),vm8q1(n),vm8q2(n),      &
                                   stat = allocatestatus)

 PRINT *, 'setting up for cases 8q and 8qi; allocatestatus=',allocatestatus
 CALL set_vr8q(n,vl8q1,vl8q2,vd8q,vd8qi,vk8q1,ve8q,ve8qi,vm8q1,vm8q2)

CASE('10q')
 allocate(vl10q1(n),vl10q2(n),vd10q(n),vd10qi(n),vk10q1(n),  &
          vk10q2(n),ve10q(n),ve10qi(n),vm10q1(n),vm10q2(n),  &
          vm10q3(n),                 stat = allocatestatus)  

 PRINT *, 'setting up for cases 10q and 10qi; allocatestatus=',allocatestatus
 CALL set_vr10q(n,vl10q1,vl10q2,vd10q,vd10qi,vk10q1,vk10q2,  &
                  ve10q,ve10qi,vm10q1,vm10q2,vm10q3)
 
CASE('4m')
 allocate(vl4m1(n),vd4mi(n),                                 &
                                   stat = allocatestatus)

 PRINT *, 'setting up for case 4m; allocatestatus=',allocatestatus
 CALL set_vr4m(n,vl4m1,vd4mi)

CASE('6m')
 allocate(vl6m1(n),vd6mi(n),vk6m1(n),ve6m(n),                &
          vm6m1(n),vm6m2(n),                                 &
                                   stat = allocatestatus)

 PRINT *, 'setting up for case 6m; allocatestatus=',allocatestatus
 CALL set_vr6m(n,vl6m1,vd6mi,vk6m1,ve6m,vm6m1,vm6m2)

CASE('8m')
 allocate(vl8m1(n),vl8m2(n),vd8mi(n),vk8m1(n),ve8m(n),        &
          vm8m1(n),vm8m2(n),                                  &
                                   stat = allocatestatus)
 PRINT *, 'setting up for case 8m; allocatestatus=',allocatestatus
 CALL set_vr8m(n,vl8m1,vl8m2,vd8mi,vk8m1,ve8m,vm8m1,vm8m2)

CASE('10m')
 allocate(vl10m1(n),vl10m2(n),vd10mi(n),vk10m1(n),vk10m2(n),  &
          ve10m(n),vm10m1(n),vm10m2(n),vm10m3(n),             &
                                   stat = allocatestatus)
 PRINT *, 'setting up for case 10m; allocatestatus=',allocatestatus
 CALL set_vr10m(n,vl10m1,vl10m2,vd10mi,vk10m1,vk10m2,ve10m,   &
                       vm10m1,vm10m2,vm10m3)

CASE('4d')
 allocate(vl4d1(n),vd4d(n),                                   &
                                   stat = allocatestatus)

 PRINT *, 'setting up for case 4d; allocatestatus=',allocatestatus
 CALL set_vr4d(n,vl4d1,vd4d)

CASE('8d')
 allocate(vl8d1(n),vl8d2(n),vd8d(n),                          &
                                   stat = allocatestatus)
 PRINT *, 'setting up for case 8d; allocatestatus=',allocatestatus
 CALL set_vr8d(n,vl8d1,vl8d2,vd8d)

END SELECT

END SUBROUTINE set_vrt_consts

!=============================================================================
SUBROUTINE set_vr4m(n,l4m1,d4mi)
!=============================================================================
INTEGER,                INTENT(IN)  :: n
REAL,   DIMENSION(0:n), INTENT(OUT) :: l4m1,d4mi
!-----------------------------------------------------------------------------
INTEGER                             :: i
REAL(8)                             :: beta,a0,a1
REAL(8),DIMENSION(0:n,-1:1)         :: ad
REAL(8),DIMENSION(0:n,-1:0)         :: ld
REAL(8),DIMENSION(0:n)              :: dd
!=============================================================================
beta=4
a0=6/beta
a1=1/beta
ad=0
DO i=0,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
ENDDO
ad(0,0) =ad(0,0)/2 ; ad(0,-1)=0
ad(n,0)=ad(n,0)/2  ; ad(n,1) =0
CALL ldlb_d(ad,ld,dd,n+1,1)
DO i=0,n
   d4mi(i)=dd(i)   
   IF(i>0)l4m1(i)=ld(i,-1)
ENDDO
l4m1(0)=0
END SUBROUTINE set_vr4m

!=============================================================================
SUBROUTINE set_vr6m(n,l6m1,d6mi,k6m1,e6m,m6m1,m6m2)
!=============================================================================
INTEGER,            INTENT(IN) :: n
REAL,DIMENSION(0:n),INTENT(OUT):: l6m1,d6mi,k6m1,e6m,m6m1,m6m2
!-----------------------------------------------------------------------------
INTEGER                        :: i
REAL(8)                        :: beta,a0,a1,b0,b1
REAL(8),DIMENSION(0:n,-1:1)    :: ad
REAL(8),DIMENSION(0:n, 0:1)    :: ld
REAL(8),DIMENSION(0:n,-1:1)    :: bd
REAL(8),DIMENSION(0:n,-1:0)    :: kd
REAL(8),DIMENSION(0:n)         :: dd,ed
!=============================================================================
beta=8
a0=10/beta
a1=3 /beta
ad=0
DO i=0,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
ENDDO
ad(0,0)=ad(0,0)/2 ;  ad(0,-1)=0
ad(n,0)=ad(n,0)/2  ; ad(n,1) =0
CALL udub_d(ad,ld,dd,n+1,1)
DO i=0,n
   d6mi(i)=dd(i)   
   IF(i < n)l6m1(i+1)=ld(i,1)
ENDDO
l6m1(0)=0

beta=16
b1=1/beta
b0=15/beta - b1
bd=0
DO i=0,n
   bd(i,0) =b0
   bd(i,-1)=b1
   bd(i,1) =b1
ENDDO
bd(0,0)=bd(0,0)/2 ; bd(0,-1)=0
bd(n,0)=bd(n,0)/2 ; bd(n,1) =0
CALL ldlb_d(bd,kd,ed,n+1,1)
DO i=0,n
   e6m(i) =1./ed(i)
   IF(i > 0)k6m1(i)=kd(i,-1)
ENDDO
e6m(n)=2*e6m(n)
k6m1(0)=0
m6m1=0; m6m1(1)=2+k6m1(1); m6m1(2:n-1)=1+k6m1(2:n-1); m6m1(n)=1+2*k6m1(n)
m6m2=0; m6m2(2:n)=k6m1(1:n-1)
END SUBROUTINE set_vr6m

!=============================================================================
SUBROUTINE set_vr8m(n,l8m1,l8m2,d8mi,k8m1,e8m,m8m1,m8m2)
!=============================================================================
INTEGER,            INTENT(IN) :: n
REAL,DIMENSION(0:n),INTENT(OUT):: l8m1,l8m2,d8mi,k8m1,e8m,m8m1,m8m2
!-----------------------------------------------------------------------------
INTEGER                        :: i
REAL(8)                        :: beta,a0,a1,a2,b0,b1
REAL(8),DIMENSION(0:n,-2:2)    :: ad
REAL(8),DIMENSION(0:n, 0:2)    :: ld
REAL(8),DIMENSION(0:n,-1:1)    :: bd
REAL(8),DIMENSION(0:n,-1:0)    :: kd
REAL(8),DIMENSION(0:n)         :: dd,ed
!=============================================================================
beta=64
a0=70/beta
a1=28/beta
a2=1 /beta
ad=0
DO i=0,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
   ad(i,-2)=a2
   ad(i,2) =a2
ENDDO
ad(0,0)  =ad(0,0)/2 ;          ad(0,-1) =0; ad(0,-2)=0
ad(1,0)  =ad(1,0)+ad(1,-2);    ad(1,-2) =0
ad(n-1,0)=ad(n-1,0)+ad(n-1,2); ad(n-1,2)=0
ad(n,0)  =ad(n,0)/2  ;         ad(n,1)  =0; ad(n,2)=0
CALL udub_d(ad,ld,dd,n+1,2)
DO i=0,n
   d8mi(i)=dd(i)   
   IF(i < n  )l8m1(i+1)=ld(i,1)
   if(i < n-1)l8m2(i+2)=ld(i,2)
ENDDO
l8m1(0)=0; l8m2(0)=0; l8m2(1)=0

beta=8
b1=1/beta
b0=7/beta - b1
bd=0
DO i=0,n
   bd(i,0) =b0
   bd(i,-1)=b1
   bd(i,1) =b1
ENDDO
bd(0,0)=bd(0,0)/2 ; bd(0,-1)=0
bd(n,0)=bd(n,0)/2 ; bd(n,1) =0
CALL ldlb_d(bd,kd,ed,n+1,1)
DO i=0,n
   e8m(i) =1./ed(i)
   IF(i > 0)k8m1(i)=kd(i,-1)
ENDDO
e8m(n)=2*e8m(n)
k8m1(0)=0
m8m1=0; m8m1(1)=2+k8m1(1); m8m1(2:n-1)=1+k8m1(2:n-1); m8m1(n)=1+2*k8m1(n)
m8m2=0; m8m2(2:n)=k8m1(1:n-1)
END SUBROUTINE set_vr8m

!=============================================================================
SUBROUTINE set_vr10m(n,l10m1,l10m2,d10mi,k10m1,k10m2,e10m, &
                       m10m1,m10m2,m10m3)
!=============================================================================
INTEGER,            INTENT(IN) :: n
REAL,DIMENSION(0:n),INTENT(OUT):: l10m1,l10m2,d10mi,k10m1,k10m2,e10m, &
                                  m10m1,m10m2,m10m3
!-----------------------------------------------------------------------------
INTEGER                        :: i
REAL(8)                        :: beta,a0,a1,a2,b0,b1,b2
REAL(8),DIMENSION(0:n,-2:2)    :: ad
REAL(8),DIMENSION(0:n, 0:2)    :: ld
REAL(8),DIMENSION(0:n,-2:2)    :: bd
REAL(8),DIMENSION(0:n,-2:0)    :: kd
REAL(8),DIMENSION(0:n)         :: dd,ed
!=============================================================================
beta=128
a0=126/beta
a1=60/beta
a2=5 /beta
ad=0
DO i=0,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
   ad(i,-2)=a2
   ad(i,2) =a2
ENDDO
ad(0,0)  =ad(0,0)/2 ;          ad(0,-1) =0; ad(0,-2)=0
ad(1,0)  =ad(1,0)  +ad(1, -2); ad(1,-2) =0
ad(n-1,0)=ad(n-1,0)+ad(n-1,2); ad(n-1,2)=0
ad(n,0)  =ad(n,0)/2  ;         ad(n,1)  =0; ad(n,2)=0
CALL udub_d(ad,ld,dd,n+1,2)
DO i=0,n
   d10mi(i)=dd(i)   
   IF(i < n  )l10m1(i+1)=ld(i,1)
   if(i < n-1)l10m2(i+2)=ld(i,2)
ENDDO
l10m1(0)=0; l10m2(0)=0; l10m2(1)=0

beta=256
b2=1/beta
b1=45/beta  - b2
b0=210/beta - b1
bd=0
DO i=0,n
   bd(i,0) =b0
   bd(i,-1)=b1
   bd(i,1) =b1
   bd(i,-2)=b2
   bd(i,2) =b2
ENDDO
bd(0,0)  =bd(0,0)/2 ;          bd(0,-1) =0; bd(0,-2)=0
bd(1,0)  =bd(1,0)  +bd(1, -2); bd(1,-2) =0
bd(n-1,0)=bd(n-1,0)+bd(n-1,2); bd(n-1,2)=0
bd(n,0)  =bd(n,0)/2  ;         bd(n,1)  =0; bd(n,2)=0
CALL ldlb_d(bd,kd,ed,n+1,2)
DO i=0,n
   e10m(i) =1./ed(i)
   IF(i > 0)k10m1(i)=kd(i,-1)
   if(i > 1)k10m2(i)=kd(i,-2)
ENDDO
e10m(n)=2*e10m(n)
k10m1(0)=0; k10m2(0)=0; m10m2(1)=0
m10m1=0; m10m1(1)    =2+k10m1(1); 
         m10m1(2:n-1)=1+k10m1(2:n-1); 
         m10m1(n)    =1+2*k10m1(n)
m10m2=0; m10m2(2:n-1)=k10m1(1:n-2)+k10m2(2:n-1);
         m10m2(n)    =k10m1(n-1)  +2*k10m2(n)
m10m3=0; m10m3(3:n)  =k10m2(2:n-1)
END SUBROUTINE set_vr10m

!=============================================================================
SUBROUTINE set_vr4d(n,l4d1,d4d)
!=============================================================================
  INTEGER,             INTENT(IN)  :: n
  REAL,   DIMENSION(n),INTENT(OUT) :: l4d1,d4d
!-----------------------------------------------------------------------------
  INTEGER                          :: i
  REAL(8)                          :: beta,a0,a1
  REAL(8),DIMENSION(n,-1:1)        :: ad
  REAL(8),DIMENSION(n,-1:0)        :: ld
  REAL(8),DIMENSION(n)             :: dd
!=============================================================================
beta=6
a0=4/beta
a1=1/beta
ad=0
DO i=1,n
   ad(i, 0)=a0
   ad(i,-1)=a1
   ad(i, 1)=a1
ENDDO
ad(1,0)=ad(1,0)-ad(1,-1); ad(1,-1)=0
ad(n,0)=ad(n,0)-ad(n,1);  ad(n,1) =0
CALL ldlb_d(ad,ld,dd,n,1)
DO i=1,n
   d4d(i)=dd(i)
   IF(i > 1)l4d1(i)=ld(i,-1)
ENDDO
l4d1(1)=0
END SUBROUTINE set_vr4d

!=============================================================================
SUBROUTINE set_vr8d(n,l8d1,l8d2,d8d)
!=============================================================================
INTEGER,              INTENT(IN) :: n
REAL,   DIMENSION(n), INTENT(OUT):: l8d1,l8d2,d8d
!-----------------------------------------------------------------------------
INTEGER                          :: i
REAL(8)                          :: beta,a0,a1,a2
REAL(8),DIMENSION(n,-2:2)        :: ad
REAL(8),DIMENSION(n,-2:0)        :: ld
REAL(8),DIMENSION(n)             :: dd
!=============================================================================
beta=70
a0=36/beta
a1=16/beta
a2=1 /beta
ad=0
DO i=1,n
   ad(i, 0)=a0
   ad(i,-1)=a1
   ad(i, 1)=a1
   ad(i,-2)=a2
   ad(i, 2)=a2
ENDDO
ad(1,0)  =ad(1,0)  -ad(1,-1);  ad(1,-1) =0.
ad(1,1)  =ad(1,1)  -ad(1,-2);  ad(1,-2) =0.
ad(2,-1) =ad(2,-1) -ad(2,-2);  ad(2,-2) =0.
ad(n-1,1)=ad(n-1,1)-ad(n-1,2); ad(n-1,2)=0.
ad(n,0)  =ad(n,0)  -ad(n,1);   ad(n,1)  =0.
ad(n,-1) =ad(n,-1) -ad(n,2);   ad(n,2)  =0.
CALL ldlb_d(ad,ld,dd,n,2)
DO i=1,n
   d8d(i)=dd(i)   
   IF(i > 1)l8d1(i)=ld(i,-1)
   IF(i > 2)l8d2(i)=ld(i,-2)
ENDDO
l8d1(1)  =0
l8d2(1:2)=0
END SUBROUTINE set_vr8d

!=============================================================================
SUBROUTINE set_vr4q(n,l4q1,d4q,d4qi)
!=============================================================================
INTEGER,              INTENT(IN)  :: n
REAL,   DIMENSION(n), INTENT(OUT) :: l4q1,d4q,d4qi
!-----------------------------------------------------------------------------
INTEGER                           :: i
REAL(8)                           :: beta,a0,a1
REAL(8),DIMENSION(n,-1:1)         :: ad
REAL(8),DIMENSION(n,0:1)          :: ld
REAL(8),DIMENSION(n)              :: dd
!=============================================================================
beta=24
a0=22/beta
a1=1 /beta
ad=0
DO i=1,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
ENDDO
ad(1,0)=ad(1,0)-ad(1,-1); ad(1,-1)=0
ad(n,0)=ad(n,0)-ad(n,1);  ad(n,1) =0
CALL udub_d(ad,ld,dd,n,1)
DO i=1,n
   d4qi(i)=dd(i)   
   d4q(i)=1/d4qi(i)
   IF(i < n)l4q1(i+1)=ld(i,1)
ENDDO
l4q1(1)=0
END SUBROUTINE set_vr4q

!=============================================================================
SUBROUTINE set_vr6q(n,l6q1,d6q,d6qi,k6q1,e6q,e6qi,m6q1,m6q2)
!=============================================================================
INTEGER,          INTENT(IN) :: n
REAL,DIMENSION(n),INTENT(OUT):: l6q1,d6q,d6qi,k6q1,e6q,e6qi,m6q1,m6q2
!-----------------------------------------------------------------------------
INTEGER                      :: i
REAL(8)                      :: beta,a0,a1,a2,b0,b1
REAL(8),DIMENSION(n,-1:1)    :: ad
REAL(8),DIMENSION(n,-1:0)    :: ld
REAL(8),DIMENSION(n,-1:1)    :: bd
REAL(8),DIMENSION(n, 0:1)    :: kd
REAL(8),DIMENSION(n)         :: dd,ed
!=============================================================================
beta=80
a0=62/beta
a1=9 /beta
ad=0
DO i=1,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
ENDDO
ad(1,0)  =ad(1,0)  -ad(1,-1);  ad(1,-1)=0.
ad(n,0)  =ad(n,0)  -ad(n,1);   ad(n,1)=0.
CALL ldlb_d(ad,ld,dd,n,1)
DO i=1,n
   d6qi(i)=dd(i)   
   d6q(i)=1/d6qi(i)
   IF(i > 1)l6q1(i)=ld(i,-1)
ENDDO
l6q1(1)=0

b1=17/(3*beta)
b0=63/beta + b1
bd=0
DO i=1,n
   bd(i,0) =b0
   bd(i,-1)=b1
   bd(i,1) =b1
ENDDO
bd(1,0)=bd(1,0)-bd(1,-1); bd(1,-1)=0.
bd(n,0)=bd(n,0)-bd(n,1);  bd(n,1)=0.
CALL udub_d(bd,kd,ed,n,1)
DO i=1,n
   e6q(i) =ed(i)
   e6qi(i)=1/e6q(i)
   IF(i < n)k6q1(i+1)=kd(i,1)
ENDDO
k6q1(1)=0
m6q1=0; m6q1(2:n)=-1.       +k6q1(2:n)
m6q2=0; m6q2(3:n)=-k6q1(3:n)
END SUBROUTINE set_vr6q

!=============================================================================
SUBROUTINE set_vr8q(n,l8q1,l8q2,d8q,d8qi,k8q1,e8q,e8qi,m8q1,m8q2)
!=============================================================================
INTEGER,          INTENT(IN) :: n
REAL,DIMENSION(n),INTENT(OUT):: l8q1,l8q2,d8q,d8qi,k8q1,e8q,e8qi,m8q1,m8q2
!-----------------------------------------------------------------------------
INTEGER                      :: i
REAL(8)                      :: beta,a0,a1,a2,b0,b1
REAL(8),DIMENSION(n,-2:2)    :: ad
REAL(8),DIMENSION(n,-2:0)    :: ld
REAL(8),DIMENSION(n,-1:1)    :: bd
REAL(8),DIMENSION(n, 0:1)    :: kd
REAL(8),DIMENSION(n)         :: dd,ed
!=============================================================================
beta=76160
a0=51338/beta
a1=12228/beta
a2=183  /beta
ad=0
DO i=1,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
   ad(i,-2)=a2
   ad(i, 2)=a2
ENDDO
ad(1,0)  =ad(1,0)  -ad(1,-1);  ad(1,-1) =0.
ad(1,1)  =ad(1,1)  -ad(1,-2);  ad(1,-2) =0.
ad(2,-1) =ad(2,-1) -ad(2,-2);  ad(2,-2) =0.
ad(n-1,1)=ad(n-1,1)-ad(n-1,2); ad(n-1,2)=0.
ad(n,0)  =ad(n,0)  -ad(n,1);   ad(n,1)  =0.
ad(n,-1) =ad(n,-1) -ad(n,2);   ad(n,2)  =0.
CALL ldlb_d(ad,ld,dd,n,2)
DO i=1,n
   d8qi(i)=dd(i)   
   d8q(i)=1/d8qi(i)
   IF(i > 1)l8q1(i)=ld(i,-1)
   IF(i > 2)l8q2(i)=ld(i,-2)
ENDDO
l8q1(1)  =0
l8q2(1:2)=0

b1=29360/(3*beta)
b0=46800/beta + b1
bd=0
DO i=1,n
   bd(i,0) =b0
   bd(i,-1)=b1
   bd(i,1) =b1
ENDDO
bd(1,0)=bd(1,0)-bd(1,-1); bd(1,-1)=0.
bd(n,0)=bd(n,0)-bd(n,1);  bd(n,1)=0.
CALL udub_d(bd,kd,ed,n,1)
DO i=1,n
   e8q(i)=ed(i)
   e8qi(i)=1/e8q(i)
   IF(i < n)k8q1(i+1)=kd(i,1)
ENDDO
k8q1(1)=0
m8q1=0; m8q1(2:n)=-1.       +k8q1(2:n)
m8q2=0; m8q2(3:n)=-k8q1(3:n)
END SUBROUTINE set_vr8q

!=============================================================================
SUBROUTINE set_vr10q(n,l10q1,l10q2,d10q,d10qi,k10q1,k10q2,e10q,e10qi, &
                       m10q1,m10q2,m10q3)
!=============================================================================
INTEGER,          INTENT(IN) :: n
REAL,DIMENSION(n),INTENT(OUT):: l10q1,l10q2,d10q,d10qi, &
                                k10q1,k10q2,e10q,e10qi, &
                                m10q1,m10q2,m10q3
!-----------------------------------------------------------------------------
INTEGER                      :: i
REAL(8)                      :: beta,a0,a1,a2,b0,b1,b2
REAL(8),DIMENSION(n,-2:2)    :: ad
REAL(8),DIMENSION(n,-2:0)    :: ld
REAL(8),DIMENSION(n,-2:2)    :: bd
REAL(8),DIMENSION(n, 0:2)    :: kd
REAL(8),DIMENSION(n)         :: dd,ed
!=============================================================================
beta=2951424
a0=1731174/beta
a1=581100 /beta
a2=29025  /beta
ad=0
DO i=1,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
   ad(i,-2)=a2
   ad(i, 2)=a2
ENDDO
ad(1,0)  =ad(1,0)  -ad(1,-1);  ad(1,-1) =0.
ad(1,1)  =ad(1,1)  -ad(1,-2);  ad(1,-2) =0.
ad(2,-1) =ad(2,-1) -ad(2,-2);  ad(2,-2) =0.
ad(n-1,1)=ad(n-1,1)-ad(n-1,2); ad(n-1,2)=0.
ad(n,0)  =ad(n,0)  -ad(n,1);   ad(n,1)  =0.
ad(n,-1) =ad(n,-1) -ad(n,2);   ad(n,2)  =0.
CALL ldlb_d(ad,ld,dd,n,2)
DO i=1,n
   d10qi(i)=dd(i)   
   d10q(i)=1/d10qi(i)
   IF(i > 1)l10q1(i)=ld(i,-1)
   IF(i > 2)l10q2(i)=ld(i,-2)
ENDDO
l10q1(1)  =0
l10q2(1:2)=0

b2=69049  /(5*beta)
b1=1515525/(3*beta) + b2
b0=1366850/beta     + b1
bd=0
DO i=1,n
   bd(i,0) =b0
   bd(i,-1)=b1
   bd(i,1) =b1
   bd(i,-2)=b2
   bd(i,2) =b2
ENDDO
bd(1,0)  =bd(1,0)  -bd(1,-1);  bd(1,-1) =0.
bd(1,1)  =bd(1,1)  -bd(1,-2);  bd(1,-2) =0.
bd(2,-1) =bd(2,-1) -bd(2,-2);  bd(2,-2) =0.
bd(n-1,1)=bd(n-1,1)-bd(n-1,2); bd(n-1,2)=0.
bd(n,0)  =bd(n,0)  -bd(n,1);   bd(n,1)  =0.
bd(n,-1) =bd(n,-1) -bd(n,2);   bd(n,2)  =0.
CALL udub_d(bd,kd,ed,n,2)
DO i=1,n
   e10q(i)=ed(i)
   e10qi(i)=1/e10q(i)
   IF(i < n)  k10q1(i+1)=kd(i,1)
   IF(i < n-1)k10q2(i+2)=kd(i,2)
ENDDO
k10q1(1)  =0
k10q2(1:2)=0
m10q1=0; m10q1(2:n)=-1.        +k10q1(2:n)
m10q2=0; m10q2(3:n)=-k10q1(3:n)+k10q2(3:n)
m10q3=0; m10q3(4:n)=-k10q2(4:n)
END SUBROUTINE set_vr10q

!=============================================================================
SUBROUTINE set_vr4m_d(n,l4m1,d4mi)
!=============================================================================
INTEGER,                INTENT(IN)  :: n
REAL(8),DIMENSION(0:n), INTENT(OUT) :: l4m1,d4mi
!-----------------------------------------------------------------------------
INTEGER                             :: i
REAL(8)                             :: beta,a0,a1
REAL(8),DIMENSION(0:n,-1:1)         :: ad
REAL(8),DIMENSION(0:n,-1:0)         :: ld
REAL(8),DIMENSION(0:n)              :: dd
!=============================================================================
beta=4
a0=6/beta
a1=1/beta
ad=0
DO i=0,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
ENDDO
ad(0,0) =ad(0,0)/2 ; ad(0,-1)=0
ad(n,0)=ad(n,0)/2  ; ad(n,1) =0
CALL ldlb_d(ad,ld,dd,n+1,1)
DO i=0,n
   d4mi(i)=dd(i)   
   IF(i>0)l4m1(i)=ld(i,-1)
ENDDO
l4m1(0)=0
END SUBROUTINE set_vr4m_d

!=============================================================================
SUBROUTINE set_vr6m_d(n,l6m1,d6mi,k6m1,e6m,m6m1,m6m2)
!=============================================================================
INTEGER,               INTENT(IN) :: n
REAL(8),DIMENSION(0:n),INTENT(OUT):: l6m1,d6mi,k6m1,e6m,m6m1,m6m2
!-----------------------------------------------------------------------------
INTEGER                           :: i
REAL(8)                           :: beta,a0,a1,b0,b1
REAL(8),DIMENSION(0:n,-1:1)       :: ad
REAL(8),DIMENSION(0:n, 0:1)       :: ld
REAL(8),DIMENSION(0:n,-1:1)       :: bd
REAL(8),DIMENSION(0:n,-1:0)       :: kd
REAL(8),DIMENSION(0:n)            :: dd,ed
!=============================================================================
beta=8
a0=10/beta
a1=3 /beta
ad=0
DO i=0,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
ENDDO
ad(0,0)=ad(0,0)/2 ;  ad(0,-1)=0
ad(n,0)=ad(n,0)/2  ; ad(n,1) =0
CALL udub_d(ad,ld,dd,n+1,1)
DO i=0,n
   d6mi(i)=dd(i)   
   IF(i < n)l6m1(i+1)=ld(i,1)
ENDDO
l6m1(0)=0

beta=16
b1=1/beta
b0=15/beta - b1
bd=0
DO i=0,n
   bd(i,0) =b0
   bd(i,-1)=b1
   bd(i,1) =b1
ENDDO
bd(0,0)=bd(0,0)/2 ; bd(0,-1)=0
bd(n,0)=bd(n,0)/2 ; bd(n,1) =0
CALL ldlb_d(bd,kd,ed,n+1,1)
DO i=0,n
   e6m(i) =1./ed(i)
   IF(i > 0)k6m1(i)=kd(i,-1)
ENDDO
e6m(n)=2*e6m(n)
k6m1(0)=0
m6m1=0; m6m1(1)=2+k6m1(1); m6m1(2:n-1)=1+k6m1(2:n-1); m6m1(n)=1+2*k6m1(n)
m6m2=0; m6m2(2:n)=k6m1(1:n-1)
END SUBROUTINE set_vr6m_d

!=============================================================================
SUBROUTINE set_vr8m_d(n,l8m1,l8m2,d8mi,k8m1,e8m,m8m1,m8m2)
!=============================================================================
INTEGER,               INTENT(IN) :: n
REAL(8),DIMENSION(0:n),INTENT(OUT):: l8m1,l8m2,d8mi,k8m1,e8m,m8m1,m8m2
!-----------------------------------------------------------------------------
INTEGER                           :: i
REAL(8)                           :: beta,a0,a1,a2,b0,b1
REAL(8),DIMENSION(0:n,-2:2)       :: ad
REAL(8),DIMENSION(0:n, 0:2)       :: ld
REAL(8),DIMENSION(0:n,-1:1)       :: bd
REAL(8),DIMENSION(0:n,-1:0)       :: kd
REAL(8),DIMENSION(0:n)            :: dd,ed
!=============================================================================
beta=64
a0=70/beta
a1=28/beta
a2=1 /beta
ad=0
DO i=0,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
   ad(i,-2)=a2
   ad(i,2) =a2
ENDDO
ad(0,0)  =ad(0,0)/2 ;          ad(0,-1) =0; ad(0,-2)=0
ad(1,0)  =ad(1,0)+ad(1,-2);    ad(1,-2) =0
ad(n-1,0)=ad(n-1,0)+ad(n-1,2); ad(n-1,2)=0
ad(n,0)  =ad(n,0)/2  ;         ad(n,1)  =0; ad(n,2)=0
CALL udub_d(ad,ld,dd,n+1,2)
DO i=0,n
   d8mi(i)=dd(i)   
   IF(i < n  )l8m1(i+1)=ld(i,1)
   if(i < n-1)l8m2(i+2)=ld(i,2)
ENDDO
l8m1(0)=0; l8m2(0)=0; l8m2(1)=0

beta=8
b1=1/beta
b0=7/beta - b1
bd=0
DO i=0,n
   bd(i,0) =b0
   bd(i,-1)=b1
   bd(i,1) =b1
ENDDO
bd(0,0)=bd(0,0)/2 ; bd(0,-1)=0
bd(n,0)=bd(n,0)/2 ; bd(n,1) =0
CALL ldlb_d(bd,kd,ed,n+1,1)
DO i=0,n
   e8m(i) =1./ed(i)
   IF(i > 0)k8m1(i)=kd(i,-1)
ENDDO
e8m(n)=2*e8m(n)
k8m1(0)=0
m8m1=0; m8m1(1)=2+k8m1(1); m8m1(2:n-1)=1+k8m1(2:n-1); m8m1(n)=1+2*k8m1(n)
m8m2=0; m8m2(2:n)=k8m1(1:n-1)
END SUBROUTINE set_vr8m_d

!=============================================================================
SUBROUTINE set_vr10m_d(n,l10m1,l10m2,d10mi,k10m1,k10m2,e10m, &
                       m10m1,m10m2,m10m3)
!=============================================================================
INTEGER,               INTENT(IN) :: n
REAL(8),DIMENSION(0:n),INTENT(OUT):: l10m1,l10m2,d10mi,k10m1,k10m2,e10m, &
                                     m10m1,m10m2,m10m3
!-----------------------------------------------------------------------------
INTEGER                           :: i
REAL(8)                           :: beta,a0,a1,a2,b0,b1,b2
REAL(8),DIMENSION(0:n,-2:2)       :: ad
REAL(8),DIMENSION(0:n, 0:2)       :: ld
REAL(8),DIMENSION(0:n,-2:2)       :: bd
REAL(8),DIMENSION(0:n,-2:0)       :: kd
REAL(8),DIMENSION(0:n)            :: dd,ed
!=============================================================================
beta=128
a0=126/beta
a1=60/beta
a2=5 /beta
ad=0
DO i=0,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
   ad(i,-2)=a2
   ad(i,2) =a2
ENDDO
ad(0,0)  =ad(0,0)/2 ;          ad(0,-1) =0; ad(0,-2)=0
ad(1,0)  =ad(1,0)  +ad(1, -2); ad(1,-2) =0
ad(n-1,0)=ad(n-1,0)+ad(n-1,2); ad(n-1,2)=0
ad(n,0)  =ad(n,0)/2  ;         ad(n,1)  =0; ad(n,2)=0
CALL udub_d(ad,ld,dd,n+1,2)
DO i=0,n
   d10mi(i)=dd(i)   
   IF(i < n  )l10m1(i+1)=ld(i,1)
   if(i < n-1)l10m2(i+2)=ld(i,2)
ENDDO
l10m1(0)=0; l10m2(0)=0; l10m2(1)=0

beta=256
b2=1/beta
b1=45/beta  - b2
b0=210/beta - b1
bd=0
DO i=0,n
   bd(i,0) =b0
   bd(i,-1)=b1
   bd(i,1) =b1
   bd(i,-2)=b2
   bd(i,2) =b2
ENDDO
bd(0,0)  =bd(0,0)/2 ;          bd(0,-1) =0; bd(0,-2)=0
bd(1,0)  =bd(1,0)  +bd(1, -2); bd(1,-2) =0
bd(n-1,0)=bd(n-1,0)+bd(n-1,2); bd(n-1,2)=0
bd(n,0)  =bd(n,0)/2  ;         bd(n,1)  =0; bd(n,2)=0
CALL ldlb_d(bd,kd,ed,n+1,2)
DO i=0,n
   e10m(i) =1./ed(i)
   IF(i > 0)k10m1(i)=kd(i,-1)
   if(i > 1)k10m2(i)=kd(i,-2)
ENDDO
e10m(n)=2*e10m(n)
k10m1(0)=0; k10m2(0)=0; m10m2(1)=0
m10m1=0; m10m1(1)    =2+k10m1(1); 
         m10m1(2:n-1)=1+k10m1(2:n-1); 
         m10m1(n)    =1+2*k10m1(n)
m10m2=0; m10m2(2:n-1)=k10m1(1:n-2)+k10m2(2:n-1);
         m10m2(n)    =k10m1(n-1)  +2*k10m2(n)
m10m3=0; m10m3(3:n)  =k10m2(2:n-1)
END SUBROUTINE set_vr10m_d

!=============================================================================
SUBROUTINE set_vr4d_d(n,l4d1,d4d)
!=============================================================================
  INTEGER,              INTENT(IN ) :: n
  REAL(8),DIMENSION(n), INTENT(OUT) :: l4d1,d4d
!-----------------------------------------------------------------------------
  INTEGER                           :: i
  REAL(8)                           :: beta,a0,a1
  REAL(8),DIMENSION(n,-1:1)         :: ad
  REAL(8),DIMENSION(n,-1:0)         :: ld
  REAL(8),DIMENSION(n)              :: dd
!=============================================================================
beta=6
a0=4/beta
a1=1/beta
ad=0
DO i=1,n
   ad(i, 0)=a0
   ad(i,-1)=a1
   ad(i, 1)=a1
ENDDO
ad(1,0)=ad(1,0)-ad(1,-1); ad(1,-1)=0
ad(n,0)=ad(n,0)-ad(n,1);  ad(n,1) =0
CALL ldlb_d(ad,ld,dd,n,1)
DO i=1,n
   d4d(i)=dd(i)
   IF(i > 1)l4d1(i)=ld(i,-1)
ENDDO
l4d1(1)=0
END SUBROUTINE set_vr4d_d

!=============================================================================
SUBROUTINE set_vr8d_d(n,l8d1,l8d2,d8d)
!=============================================================================
INTEGER,              INTENT(IN ) :: n
REAL(8),DIMENSION(n), INTENT(OUT) :: l8d1,l8d2,d8d
!-----------------------------------------------------------------------------
INTEGER                           :: i
REAL(8)                           :: beta,a0,a1,a2
REAL(8),DIMENSION(n,-2:2)         :: ad
REAL(8),DIMENSION(n,-2:0)         :: ld
REAL(8),DIMENSION(n)              :: dd
!=============================================================================
beta=70
a0=36/beta
a1=16/beta
a2=1 /beta
ad=0
DO i=1,n
   ad(i, 0)=a0
   ad(i,-1)=a1
   ad(i, 1)=a1
   ad(i,-2)=a2
   ad(i, 2)=a2
ENDDO
ad(1,0)  =ad(1,0)  -ad(1,-1);  ad(1,-1) =0.
ad(1,1)  =ad(1,1)  -ad(1,-2);  ad(1,-2) =0.
ad(2,-1) =ad(2,-1) -ad(2,-2);  ad(2,-2) =0.
ad(n-1,1)=ad(n-1,1)-ad(n-1,2); ad(n-1,2)=0.
ad(n,0)  =ad(n,0)  -ad(n,1);   ad(n,1)  =0.
ad(n,-1) =ad(n,-1) -ad(n,2);   ad(n,2)  =0.
CALL ldlb_d(ad,ld,dd,n,2)
DO i=1,n
   d8d(i)=dd(i)   
   IF(i > 1)l8d1(i)=ld(i,-1)
   IF(i > 2)l8d2(i)=ld(i,-2)
ENDDO
l8d1(1)  =0
l8d2(1:2)=0
END SUBROUTINE set_vr8d_d

!=============================================================================
SUBROUTINE set_vr4q_d(n,l4q1,d4q,d4qi)
!=============================================================================
INTEGER,              INTENT(IN ) :: n
REAL(8),DIMENSION(n), INTENT(OUT) :: l4q1,d4q,d4qi
!-----------------------------------------------------------------------------
INTEGER                           :: i
REAL(8)                           :: beta,a0,a1
REAL(8),DIMENSION(n,-1:1)         :: ad
REAL(8),DIMENSION(n,0:1)          :: ld
REAL(8),DIMENSION(n)              :: dd
!=============================================================================
beta=24
a0=22/beta
a1=1 /beta
ad=0
DO i=1,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
ENDDO
ad(1,0)=ad(1,0)-ad(1,-1); ad(1,-1)=0
ad(n,0)=ad(n,0)-ad(n,1);  ad(n,1) =0
CALL udub_d(ad,ld,dd,n,1)
DO i=1,n
   d4qi(i)=dd(i)   
   d4q(i)=1/d4qi(i)
   IF(i < n)l4q1(i+1)=ld(i,1)
ENDDO
l4q1(1)=0
END SUBROUTINE set_vr4q_d

!=============================================================================
SUBROUTINE set_vr6q_d(n,l6q1,d6q,d6qi,k6q1,e6q,e6qi,m6q1,m6q2)
!=============================================================================
INTEGER,              INTENT(IN ):: n
REAL(8),DIMENSION(n), INTENT(OUT):: l6q1,d6q,d6qi,k6q1,e6q,e6qi,m6q1,m6q2
!-----------------------------------------------------------------------------
INTEGER                          :: i
REAL(8)                          :: beta,a0,a1,a2,b0,b1
REAL(8),DIMENSION(n,-1:1)        :: ad
REAL(8),DIMENSION(n,-1:0)        :: ld
REAL(8),DIMENSION(n,-1:1)        :: bd
REAL(8),DIMENSION(n, 0:1)        :: kd
REAL(8),DIMENSION(n)             :: dd,ed
!=============================================================================
beta=80
a0=62/beta
a1=9 /beta
ad=0
DO i=1,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
ENDDO
ad(1,0)  =ad(1,0)  -ad(1,-1);  ad(1,-1)=0.
ad(n,0)  =ad(n,0)  -ad(n,1);   ad(n,1)=0.
CALL ldlb_d(ad,ld,dd,n,1)
DO i=1,n
   d6qi(i)=dd(i)   
   d6q(i)=1/d6qi(i)
   IF(i > 1)l6q1(i)=ld(i,-1)
ENDDO
l6q1(1)=0

b1=17/(3*beta)
b0=63/beta + b1
bd=0
DO i=1,n
   bd(i,0) =b0
   bd(i,-1)=b1
   bd(i,1) =b1
ENDDO
bd(1,0)=bd(1,0)-bd(1,-1); bd(1,-1)=0.
bd(n,0)=bd(n,0)-bd(n,1);  bd(n,1)=0.
CALL udub_d(bd,kd,ed,n,1)
DO i=1,n
   e6q(i) =ed(i)
   e6qi(i)=1/e6q(i)
   IF(i < n)k6q1(i+1)=kd(i,1)
ENDDO
k6q1(1)=0
m6q1=0; m6q1(2:n)=-1.       +k6q1(2:n)
m6q2=0; m6q2(3:n)=-k6q1(3:n)
END SUBROUTINE set_vr6q_d

!=============================================================================
SUBROUTINE set_vr8q_d(n,l8q1,l8q2,d8q,d8qi,k8q1,e8q,e8qi,m8q1,m8q2)
!=============================================================================
INTEGER,              INTENT(IN ):: n
REAL(8),DIMENSION(n), INTENT(OUT):: l8q1,l8q2,d8q,d8qi,k8q1,e8q,e8qi,m8q1,m8q2
!-----------------------------------------------------------------------------
INTEGER                          :: i
REAL(8)                          :: beta,a0,a1,a2,b0,b1
REAL(8),DIMENSION(n,-2:2)        :: ad
REAL(8),DIMENSION(n,-2:0)        :: ld
REAL(8),DIMENSION(n,-1:1)        :: bd
REAL(8),DIMENSION(n, 0:1)        :: kd
REAL(8),DIMENSION(n)             :: dd,ed
!=============================================================================
beta=76160
a0=51338/beta
a1=12228/beta
a2=183  /beta
ad=0
DO i=1,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
   ad(i,-2)=a2
   ad(i, 2)=a2
ENDDO
ad(1,0)  =ad(1,0)  -ad(1,-1);  ad(1,-1) =0.
ad(1,1)  =ad(1,1)  -ad(1,-2);  ad(1,-2) =0.
ad(2,-1) =ad(2,-1) -ad(2,-2);  ad(2,-2) =0.
ad(n-1,1)=ad(n-1,1)-ad(n-1,2); ad(n-1,2)=0.
ad(n,0)  =ad(n,0)  -ad(n,1);   ad(n,1)  =0.
ad(n,-1) =ad(n,-1) -ad(n,2);   ad(n,2)  =0.
CALL ldlb_d(ad,ld,dd,n,2)
DO i=1,n
   d8qi(i)=dd(i)   
   d8q(i)=1/d8qi(i)
   IF(i > 1)l8q1(i)=ld(i,-1)
   IF(i > 2)l8q2(i)=ld(i,-2)
ENDDO
l8q1(1)  =0
l8q2(1:2)=0

b1=29360/(3*beta)
b0=46800/beta + b1
bd=0
DO i=1,n
   bd(i,0) =b0
   bd(i,-1)=b1
   bd(i,1) =b1
ENDDO
bd(1,0)=bd(1,0)-bd(1,-1); bd(1,-1)=0.
bd(n,0)=bd(n,0)-bd(n,1);  bd(n,1)=0.
CALL udub_d(bd,kd,ed,n,1)
DO i=1,n
   e8q(i)=ed(i)
   e8qi(i)=1/e8q(i)
   IF(i < n)k8q1(i+1)=kd(i,1)
ENDDO
k8q1(1)=0
m8q1=0; m8q1(2:n)=-1.       +k8q1(2:n)
m8q2=0; m8q2(3:n)=-k8q1(3:n)
END SUBROUTINE set_vr8q_d

!=============================================================================
SUBROUTINE set_vr10q_d(n,l10q1,l10q2,d10q,d10qi,k10q1,k10q2,e10q,e10qi, &
                         m10q1,m10q2,m10q3)
!=============================================================================
INTEGER,              INTENT(IN ):: n
REAL(8),DIMENSION(n), INTENT(OUT):: l10q1,l10q2,d10q,d10qi, &
                                    k10q1,k10q2,e10q,e10qi, &
                                    m10q1,m10q2,m10q3
!-----------------------------------------------------------------------------
INTEGER                          :: i
REAL(8)                          :: beta,a0,a1,a2,b0,b1,b2
REAL(8),DIMENSION(n,-2:2)        :: ad
REAL(8),DIMENSION(n,-2:0)        :: ld
REAL(8),DIMENSION(n,-2:2)        :: bd
REAL(8),DIMENSION(n, 0:2)        :: kd
REAL(8),DIMENSION(n)             :: dd,ed
!=============================================================================
beta=2951424
a0=1731174/beta
a1=581100 /beta
a2=29025  /beta
ad=0
DO i=1,n
   ad(i,0) =a0
   ad(i,-1)=a1
   ad(i,1) =a1
   ad(i,-2)=a2
   ad(i, 2)=a2
ENDDO
ad(1,0)  =ad(1,0)  -ad(1,-1);  ad(1,-1) =0.
ad(1,1)  =ad(1,1)  -ad(1,-2);  ad(1,-2) =0.
ad(2,-1) =ad(2,-1) -ad(2,-2);  ad(2,-2) =0.
ad(n-1,1)=ad(n-1,1)-ad(n-1,2); ad(n-1,2)=0.
ad(n,0)  =ad(n,0)  -ad(n,1);   ad(n,1)  =0.
ad(n,-1) =ad(n,-1) -ad(n,2);   ad(n,2)  =0.
CALL ldlb_d(ad,ld,dd,n,2)
DO i=1,n
   d10qi(i)=dd(i)   
   d10q(i)=1/d10qi(i)
   IF(i > 1)l10q1(i)=ld(i,-1)
   IF(i > 2)l10q2(i)=ld(i,-2)
ENDDO
l10q1(1)  =0
l10q2(1:2)=0

b2=69049  /(5*beta)
b1=1515525/(3*beta) + b2
b0=1366850/beta     + b1
bd=0
DO i=1,n
   bd(i,0) =b0
   bd(i,-1)=b1
   bd(i,1) =b1
   bd(i,-2)=b2
   bd(i,2) =b2
ENDDO
bd(1,0)  =bd(1,0)  -bd(1,-1);  bd(1,-1) =0.
bd(1,1)  =bd(1,1)  -bd(1,-2);  bd(1,-2) =0.
bd(2,-1) =bd(2,-1) -bd(2,-2);  bd(2,-2) =0.
bd(n-1,1)=bd(n-1,1)-bd(n-1,2); bd(n-1,2)=0.
bd(n,0)  =bd(n,0)  -bd(n,1);   bd(n,1)  =0.
bd(n,-1) =bd(n,-1) -bd(n,2);   bd(n,2)  =0.
CALL udub_d(bd,kd,ed,n,2)
DO i=1,n
   e10q(i)=ed(i)
   e10qi(i)=1/e10q(i)
   IF(i < n)  k10q1(i+1)=kd(i,1)
   IF(i < n-1)k10q2(i+2)=kd(i,2)
ENDDO
k10q1(1)  =0
k10q2(1:2)=0
m10q1=0; m10q1(2:n)=-1.        +k10q1(2:n)
m10q2=0; m10q2(3:n)=-k10q1(3:n)+k10q2(3:n)
m10q3=0; m10q3(4:n)=-k10q2(4:n)
END SUBROUTINE set_vr10q_d

!END MODULE MODULE_set_vops


