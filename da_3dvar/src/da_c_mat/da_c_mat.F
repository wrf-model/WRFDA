MODULE da_c_mat

IMPLICIT NONE

REAL(8) ::                                                 &
     b4m_d,l4m1_d,                                         &   
     b6m_d,k6m1_d,l6m1_d,                                  &
     b8m_d,k8m1_d,l8m1_d,l8m2_d,                           &
     b10m_d,k10m1_d,k10m2_d,l10m1_d,l10m2_d,               &
     b4d_d,b4d1_d,l4d1_d,                                  &
     b8d_d,b8d1_d,b8d2_d,k8d1_d,l8d1_d,l8d2_d,             &
     b4qi_d,b4q_d,l4q1_d,                                  &
     b6qi_d,b6q_d,k6q1_d,l6q1_d,                           &
     b8qi_d,b8q_d,k8q1_d,l8q1_d,l8q2_d,                    &
     b10qi_d,b10q_d,k10q1_d,k10q2_d,l10q1_d,l10q2_d,       &
     l4t1_d,k4t1_d,                                        &
     l8t1_d,l8t2_d,k8t1_d,k8t2_d

INTEGER ::                                                 &
     fl4md,             fl6md,       fl8md,        fl10md, &
     fl4dd,                          fl8dd,                &
     fl4qd,       fk6qd,fl6qd, fk8qd,fl8qd, fk10qd,fl10qd, &
     fk4td,fl4td,              fk8td,fl8td

INTEGER, PARAMETER :: nnp=6
REAL         ::                         & ! Coefficients for...
     b4m,l4m1,                          & ! 4th-order midpoint interpolation 
     b6m,k6m1,l6m1,                     & ! 6th-order midpoint interpolation
     b8m,k8m1,l8m1,l8m2,                & ! 8th-order midpoint interpolation
     b10m,k10m1,k10m2,l10m1,l10m2,      & ! 10th-order midpoint interpolation
     b4d,b4d1,l4d1,                     & ! 4th-order same-grid differencing
     b8d,b8d1,b8d2,k8d1,l8d1,l8d2,      & ! 8th-order same-grid differencing
     b4qi,b4q,l4q1,                     & ! 4th-order invertible quadrature
     b6qi,b6q,k6q1,l6q1,                & ! 6th-order invertible quadrature
     b8qi,b8q,k8q1,l8q1,l8q2,           & ! 8th-order invertible quadrature
     b10qi,b10q,k10q1,k10q2,l10q1,l10q2,& ! 10th-or invertible quadrature
     l4t1,k4t1,                         & ! 4th-order invertible translation
     l8t1,l8t2,k8t1,k8t2                  ! 8th-order invertible translation

! Significant widths of the exponential tails of recursive filters
! associated with each of the compact schemes in the context of
! Real(4) arithmetic:
INTEGER  ::                                         &
          fl4m,       fl6m,      fl8m,        fl10m,       &
          fl4d,                  fl8d,                     &
          fl4q,  fk6q,fl6q, fk8q,fl8q, fk10q,fl10q,        &
     fk4t,fl4t,             fk8t,fl8t

! Extrapolation orders (np..) and extrapolation weights (w***..) 
! required to initiate the recursive steps
! of the various "compact" numerical schemes on uniform grids.
! These are initialialized by calls to appropriate subroutines,
! set_cpt_wexts('..',npoly) where the ".." identifies the specific operation:
INTEGER :: &
     np4m, np6m, np8m, np10m,           &
     np4d,       np8d,                  &
     np4q, np6q, np8q, np10q,           &
     np4t,       np8t

REAL , ALLOCATABLE,DIMENSION(:,:) ::         & 
wj__4m, w___4m,  w_l_4m,  wll_4m,  wl__4m,          & ! 4m
wj__6m, wk__6m,  w_lk6m,  wllk6m,  wl__6m, w_l_6m,  & ! 6m
wj__8m, wk__8m,  w_lk8m,  wllk8m,  wl__8m, w_l_8m,  & ! 8m
wj__10m,wk__10m, w_lk10m, wllk10m, wl__10m,w_l_10m, & ! 10m
wi__4d,          w_l_4d,  wll_4d,  wl__4d,          & ! 4d
wi__8d, wk__8d,  w_lk8d,  wllk8d,  wl__8d, w_l_8d,  & ! 8d
wi__4q,          w_l_4q,  wll_4q,                   & ! 4qi
        wl__4q,  w___4q,                            & ! 4q
wi__6q, wk__6q,  w_lk6q,  wllk6q,  w_l_6q,          & ! 6qi
        wl__6q,  w_kl6q,  wkkl6q,  w_k_6q,          & ! 6q
wi__8q, wk__8q,  w_lk8q,  wllk8q,  w_l_8q,          & ! 8qi
        wl__8q,  w_kl8q,  wkkl8q,  w_k_8q,          & ! 8q
wi__10q,wk__10q, w_lk10q, wllk10q, w_l_10q,         & ! 10qi
        wl__10q, w_kl10q, wkkl10q, w_k_10q,         & ! 10q
        wk__4t,  w_ll4t,  wlkk4t,  w_l_4t,          & ! 4t
        wl__4t,  w_kk4t,  wkll4t,  w_k_4t,          & ! 4ti
        wk__8t,  w_ll8t,  wlkk8t,  w_l_8t,          & ! 8t
        wl__8t,  w_kk8t,  wkll8t,  w_k_8t             ! 8ti

REAL,   DIMENSION(:),  ALLOCATABLE  ::               & ! Coefficients for...
 vl4q1,vd4q,vd4qi,                                   & ! 4th order quad coeff.
 vl6q1,vd6q,vd6qi,vk6q1,ve6q,ve6qi,vm6q1,vm6q2,      & ! 6th order quad coeff.
 vl8q1,vl8q2,vd8q,vd8qi,vk8q1,ve8q,ve8qi,vm8q1,vm8q2,& ! 8th order quad coeff.
 vl10q1,vl10q2,vd10q,vd10qi,vk10q1,vk10q2,           & !10th order quad coeff.
 ve10q,ve10qi,vm10q1,vm10q2,vm10q3,                  & 
 vl4m1,vd4mi,                                        & ! 4th order inter coeff.
 vl6m1,vd6mi,vk6m1,ve6m,vm6m1,vm6m2,                 & ! 6th order inter coeff.
 vl8m1,vl8m2,vd8mi,vk8m1,ve8m,vm8m1,vm8m2,           & ! 8th order inter coeff.
 vl10m1,vl10m2,vd10mi,vk10m1,vk10m2,ve10m,           & !10th order inter coeff.
 vm10m1,vm10m2,vm10m3,                               &
 vl4d1,vd4d,                                         & ! 4th order diff coeff.
 vl8d1,vl8d2,vd8d                                      ! 8th order diff coeff. 

INTERFACE set_ext
   MODULE PROCEDURE set_ext1, set_ext2, set_ext3, set_ext4, &
                    set_ext5, set_ext6, set_ext7, set_ext8
END INTERFACE
INTERFACE set_ext_d
   MODULE PROCEDURE set_ext1_d, set_ext2_d, set_ext3_d, set_ext4_d, &
                    set_ext5_d, set_ext6_d, set_ext7_d, set_ext8_d
END INTERFACE
INTERFACE set_mext
   MODULE PROCEDURE set_mext4, set_mext5
END INTERFACE
INTERFACE set_mext_d
   MODULE PROCEDURE set_mext4_d, set_mext5_d
END INTERFACE

!!!DALEPRIVATE :: mcmax, mcmax_d
INTERFACE pro333  ; MODULE PROCEDURE pro333;                 END INTERFACE
INTERFACE pro333_d; MODULE PROCEDURE dpro333;                END INTERFACE
INTERFACE cro33   ; MODULE PROCEDURE cro33;                  END INTERFACE
INTERFACE cro33_d;  MODULE PROCEDURE dcro33;                 END INTERFACE
INTERFACE norv;     MODULE PROCEDURE norv;                   END INTERFACE
INTERFACE norv_d;   MODULE PROCEDURE dnorv;                  END INTERFACE
INTERFACE norq;     MODULE PROCEDURE norq;                   END INTERFACE
INTERFACE norq_d;   MODULE PROCEDURE dnorq;                  END INTERFACE
INTERFACE swpvv;    MODULE PROCEDURE swpvv;                  END INTERFACE
INTERFACE swpvv_d;  MODULE PROCEDURE dswpvv;                 END INTERFACE
INTERFACE mulmd;    MODULE PROCEDURE mulmd;                  END INTERFACE
INTERFACE mulmd_d;  MODULE PROCEDURE dmulmd;                 END INTERFACE
INTERFACE multd;    MODULE PROCEDURE multd;                  END INTERFACE
INTERFACE multd_d;  MODULE PROCEDURE dmultd;                 END INTERFACE
INTERFACE muldm;    MODULE PROCEDURE muldm;                  END INTERFACE
INTERFACE muldm_d;  MODULE PROCEDURE dmuldm;                 END INTERFACE
INTERFACE muldt;    MODULE PROCEDURE muldt;                  END INTERFACE
INTERFACE muldt_d;  MODULE PROCEDURE dmuldt;                 END INTERFACE
INTERFACE mulpp;    MODULE PROCEDURE mulpp;                  END INTERFACE
INTERFACE mulpp_d;  MODULE PROCEDURE dmulpp;                 END INTERFACE
INTERFACE msbpp;    MODULE PROCEDURE msbpp;                  END INTERFACE
INTERFACE msbpp_d;  MODULE PROCEDURE dmsbpp;                 END INTERFACE
INTERFACE madpp;    MODULE PROCEDURE madpp;                  END INTERFACE
INTERFACE madpp_d;  MODULE PROCEDURE dmadpp;                 END INTERFACE
INTERFACE difp;     MODULE PROCEDURE difp;                   END INTERFACE
INTERFACE difp_d;   MODULE PROCEDURE ddifp;                  END INTERFACE
INTERFACE intp;     MODULE PROCEDURE intp;                   END INTERFACE
INTERFACE intp_d;   MODULE PROCEDURE dintp;                  END INTERFACE
INTERFACE invp;     MODULE PROCEDURE invp;                   END INTERFACE
INTERFACE invp_d;   MODULE PROCEDURE dinvp;                  END INTERFACE
INTERFACE powp;     MODULE PROCEDURE powp;                   END INTERFACE
INTERFACE powp_d;   MODULE PROCEDURE dpowp;                  END INTERFACE
INTERFACE polps;    MODULE PROCEDURE polps;                  END INTERFACE
INTERFACE polps_d;  MODULE PROCEDURE dpolps;                 END INTERFACE
INTERFACE polpp;    MODULE PROCEDURE polpp;                  END INTERFACE
INTERFACE polpp_d;  MODULE PROCEDURE dpolpp;                 END INTERFACE
INTERFACE prgv;     MODULE PROCEDURE prgv;                   END INTERFACE
INTERFACE prgv_d;   MODULE PROCEDURE dprgv;                  END INTERFACE
INTERFACE mulcc;    MODULE PROCEDURE mulcc;                  END INTERFACE
INTERFACE mulcc_d;  MODULE PROCEDURE dmulcc;                 END INTERFACE
INTERFACE msbcc;    MODULE PROCEDURE msbcc;                  END INTERFACE
INTERFACE msbcc_d;  MODULE PROCEDURE dmsbcc;                 END INTERFACE
INTERFACE madcc;    MODULE PROCEDURE madcc;                  END INTERFACE
INTERFACE madcc_d;  MODULE PROCEDURE dmadcc;                 END INTERFACE
INTERFACE zerl;     MODULE PROCEDURE zerl;                   END INTERFACE
INTERFACE zerl_d;   MODULE PROCEDURE dzerl;                  END INTERFACE
INTERFACE zeru;     MODULE PROCEDURE zeru;                   END INTERFACE
INTERFACE zeru_d;   MODULE PROCEDURE dzeru;                  END INTERFACE
INTERFACE ldum;     MODULE PROCEDURE ldum;                   END INTERFACE
INTERFACE ldum_d;   MODULE PROCEDURE dldum;                  END INTERFACE
INTERFACE udlmm;    MODULE PROCEDURE udlmm, udlmv;           END INTERFACE
INTERFACE udlmm_d;  MODULE PROCEDURE dudlmm,dudlmv;          END INTERFACE
INTERFACE linvan;   MODULE PROCEDURE linvan;                 END INTERFACE
INTERFACE linvan_d; MODULE PROCEDURE dlinvan;                END INTERFACE
INTERFACE condm;    MODULE PROCEDURE condm;                  END INTERFACE
INTERFACE condm_d;  MODULE PROCEDURE dcondm;                 END INTERFACE
INTERFACE copdm;    MODULE PROCEDURE copdm;                  END INTERFACE
INTERFACE copdm_d;  MODULE PROCEDURE dcopdm;                 END INTERFACE
INTERFACE consm;    MODULE PROCEDURE consm;                  END INTERFACE
INTERFACE consm_d;  MODULE PROCEDURE dconsm;                 END INTERFACE
INTERFACE copsm;    MODULE PROCEDURE copsm;                  END INTERFACE
INTERFACE copsm_d;  MODULE PROCEDURE dcopsm;                 END INTERFACE
INTERFACE l1lm;     MODULE PROCEDURE l1lm;                   END INTERFACE
INTERFACE l1lm_d;   MODULE PROCEDURE dl1lm;                  END INTERFACE
INTERFACE ldlm;     MODULE PROCEDURE ldlm;                   END INTERFACE
INTERFACE ldlm_d;   MODULE PROCEDURE dldlm;                  END INTERFACE
INTERFACE invh;     MODULE PROCEDURE invh;                   END INTERFACE
INTERFACE invh_d;   MODULE PROCEDURE dinvh;                  END INTERFACE
INTERFACE invl;     MODULE PROCEDURE invl;                   END INTERFACE
INTERFACE invl_d;   MODULE PROCEDURE dinvl;                  END INTERFACE
INTERFACE linlv;    MODULE PROCEDURE linlv;                  END INTERFACE
INTERFACE linlv_d;  MODULE PROCEDURE dlinlv;                 END INTERFACE
INTERFACE linuv;    MODULE PROCEDURE linuv;                  END INTERFACE
INTERFACE linuv_d;  MODULE PROCEDURE dlinuv;                 END INTERFACE
INTERFACE trcm;     MODULE PROCEDURE trcm;                   END INTERFACE
INTERFACE trcm_d;   MODULE PROCEDURE dtrcm;                  END INTERFACE
INTERFACE inv;      MODULE PROCEDURE invmt, linmmt, linmvt;  END INTERFACE
INTERFACE inv_d;    MODULE PROCEDURE dinvmt,dlinmmt,dlinmvt; END INTERFACE
INTERFACE mcmax;    MODULE PROCEDURE mcmax;                  END INTERFACE
INTERFACE mcmax_d;  MODULE PROCEDURE dmcmax;                 END INTERFACE
INTERFACE clib_d; MODULE PROCEDURE dclib;          END INTERFACE
INTERFACE ldub_d; MODULE PROCEDURE dldub;          END INTERFACE
INTERFACE l1ubb_d;MODULE PROCEDURE dl1ubb;         END INTERFACE
INTERFACE l1ueb_d;MODULE PROCEDURE dl1ueb;         END INTERFACE
INTERFACE ldlb_d; MODULE PROCEDURE dldlb;          END INTERFACE
INTERFACE udub_d; MODULE PROCEDURE dudub;          END INTERFACE

CONTAINS

#include <cpt_consts.inc>
#include <pmat1.inc>
#include <pmat2.inc>
#include <powmat.inc>
#include <set_vops.inc>

END MODULE da_c_mat

