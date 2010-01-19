!WRF:DRIVER_LAYER:INTEGRATION
!

MODULE module_integrate_tst

CONTAINS

SUBROUTINE integrate_tst ( grid )



   USE module_domain
   USE module_driver_constants
   USE module_nesting
   USE module_configure
   USE module_timing
   USE WRF_ESMF_MOD
   USE module_integrate
   USE module_integrate_ad
   USE module_integrate_tl


   IMPLICIT NONE

#ifdef DM_PARALLEL
    include "mpif.h"
#endif

   !  Input data.

   TYPE(domain) , POINTER :: grid

real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_rainc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_raincv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_rainnc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_rainncv

real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_rainc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_raincv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_rainnc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_rainncv

   !  Local data.

   INTEGER                         :: k_start , k_end, its, ite, jts, jte, i, j, k, kts, kte
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe

!---------------------------------------------------------------------------------------
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_u_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_u_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_v_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_v_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_w_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_w_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_ph_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_ph_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_t_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_t_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_mu_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_mu_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_p
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_al
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: S_moist_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: S_moist_2
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: S_mu_b
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_rainnc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_rainncv


real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_u_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_u_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_v_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_v_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_w_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_w_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_ph_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_ph_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_t_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_t_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_mu_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_mu_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_mudf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_p
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_al
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: B_moist_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: B_moist_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_rainnc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_rainncv

real                                     :: dtbc_save
!---------------------------------------------------------------------------------------

   REAL :: SAVE_L, COEF, ALPHA_M, FACTOR, VAL_N, VAL_L, VAL_A, nsum
   INTEGER :: NT,h, ifs, ife, jfs, jfe, comm, ierror, rc

   character(len=19), dimension(1000) :: nl_date_string
   integer                            :: nl_date_index
   character(len=19), dimension(1000) :: ad_date_string
   integer                            :: ad_date_index



!  Obtain dimension information stored in the grid data structure.
  CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )

   its = grid%sp31-3 ; ite = grid%ep31+3
   kts = grid%sp32 ; kte = grid%ep32
   jts = grid%sp33-3 ; jte = grid%ep33+3

   if ( ips .eq. ids ) then
     ifs = its
   else
     ifs = ips
   endif
   if ( ipe .eq. ide ) then
     ife = ite
   else
     ife = ipe
   endif
   if ( jps .eq. jds ) then
     jfs = jts
   else
     jfs = jps
   endif
   if ( jpe .eq. jde ) then
     jfe = jte
   else
     jfe = jpe
   endif

   print*,'ifs =',ifs
   print*,'ife =',ife
   print*,'jfs =',jfs
   print*,'jfe =',jfe

   print*,'its =',its
   print*,'ite =',ite
   print*,'ims =',ims
   print*,'ime =',ime

   print*,'jts =',jts
   print*,'jte =',jte
   print*,'jms =',jms
   print*,'jme =',jme

   print*,'kts =',kts
   print*,'kte =',kte
   print*,'num_moist =',num_moist
   print*,'grid%sm31 grid%em31 =',grid%sm31,grid%em31
   print*,'grid%sm32 grid%em32 =',grid%sm32,grid%em32
   print*,'grid%sm33 grid%em33 =',grid%sm33,grid%em33
   print*,'grid%spec_bdy_width =',grid%spec_bdy_width

   dtbc_save= grid%dtbc

   S_u_1 = grid%em_u_1
   S_u_2 = grid%em_u_2
   S_v_1 = grid%em_v_1
   S_v_2 = grid%em_v_2
   S_w_1 = grid%em_w_1
   S_w_2 = grid%em_w_2
   S_ph_1 = grid%em_ph_1
   S_ph_2 = grid%em_ph_2
   S_t_1 = grid%em_t_1
   S_t_2 = grid%em_t_2
   S_mu_1 = grid%em_mu_1
   S_mu_2 = grid%em_mu_2
   S_p = grid%em_p
   S_al = grid%em_al
   S_z = grid%em_z
   S_moist_1 = grid%moist_1
   S_moist_2 = grid%moist_2
   S_rainnc(:,:) = grid%rainnc(:,:)
   S_rainncv(:,:)= grid%rainncv(:,:)

!  NLM
   CALL wrf_message( '-------------- Non-linear model ---------------' )

   model_config_rec%dyn_opt = DYN_EM
   CALL integrate ( grid )

   grid%em_z=0.; grid%em_mudf=0.

   do j=jts,jte
     do k=kts,kte
       do i=its,ite
         B_u_1(i,k,j) = grid%em_u_1(i,k,j)
         B_u_2(i,k,j) = grid%em_u_2(i,k,j)
         B_v_1(i,k,j) = grid%em_v_1(i,k,j)
         B_v_2(i,k,j) = grid%em_v_2(i,k,j)
         B_w_1(i,k,j) = grid%em_w_1(i,k,j)
         B_w_2(i,k,j) = grid%em_w_2(i,k,j)
         B_ph_1(i,k,j) = grid%em_ph_1(i,k,j)
         B_ph_2(i,k,j) = grid%em_ph_2(i,k,j)
         B_t_1(i,k,j) = grid%em_t_1(i,k,j)
         B_t_2(i,k,j) = grid%em_t_2(i,k,j)
         B_p(i,k,j) = grid%em_p(i,k,j)
         B_al(i,k,j) = grid%em_al(i,k,j)
         B_z(i,k,j) = grid%em_z(i,k,j)
       enddo
     enddo
   enddo

   do j=jts,jte
     do i=its,ite
       B_mu_1(i,j) =grid%em_mu_1(i,j)
       B_mu_2(i,j) =grid%em_mu_2(i,j)
       B_rainnc(i,j)=grid%rainnc(i,j)
       B_rainncv(i,j)=grid%rainncv(i,j)
     enddo
   enddo

   do h=1,num_moist
     do j=jts,jte
       do k=kts,kte
         do i=its,ite
           B_moist_1(i,k,j,h) =grid%moist_1(i,k,j,h)
           B_moist_2(i,k,j,h) =grid%moist_2(i,k,j,h)
         enddo
       enddo
     enddo
   enddo

!  TCL

   grid%em_u_1 = S_u_1
   grid%em_u_2 = S_u_2
   grid%em_v_1 = S_v_1
   grid%em_v_2 = S_v_2
   grid%em_w_1 = S_w_1
   grid%em_w_2 = S_w_2
   grid%em_ph_1 = S_ph_1
   grid%em_ph_2 = S_ph_2
   grid%em_t_1 = S_t_1
   grid%em_t_2 = S_t_2
   grid%em_mu_1 = S_mu_1
   grid%em_mu_2 = S_mu_2
   grid%em_p = S_p
   grid%em_al = S_al
   grid%em_z = S_z
   grid%moist_1 = S_moist_1
   grid%moist_2 = S_moist_2
   grid%rainnc(:,:)=S_rainnc(:,:)
   grid%rainncv(:,:)=S_rainncv(:,:)

   grid%em_g_u_1 = grid%em_u_1
   grid%em_g_u_2 = grid%em_u_2
   grid%em_g_v_1 = grid%em_v_1
   grid%em_g_v_2 = grid%em_v_2
   grid%em_g_w_1 = grid%em_w_1
   grid%em_g_w_2 = grid%em_w_2
   grid%em_g_ph_1 = grid%em_ph_1
   grid%em_g_ph_2 = grid%em_ph_2
   grid%em_g_t_1 = grid%em_t_1
   grid%em_g_t_2 = grid%em_t_2
   grid%em_g_mu_1 = grid%em_mu_1
   grid%em_g_mu_2 = grid%em_mu_2
   grid%em_g_p = grid%em_p
   grid%em_g_al = grid%em_al
   grid%em_g_z = grid%em_z
   grid%g_moist_1 = grid%moist_1
   grid%g_moist_2 = grid%moist_2
   g_rainnc(:,:)=grid%rainnc(:,:)
   g_rainncv(:,:)=grid%rainncv(:,:)
   grid%em_g_u_b = 0.0
   grid%em_g_u_bt = 0.0
   grid%em_g_v_b = 0.0
   grid%em_g_v_bt = 0.0
   grid%em_g_w_b = 0.0
   grid%em_g_w_bt = 0.0
   grid%em_g_ph_b = 0.0
   grid%em_g_ph_bt = 0.0
   grid%em_g_t_b = 0.0
   grid%em_g_t_bt = 0.0
   grid%em_g_mu_b = 0.0
   grid%em_g_mu_bt = 0.0
   grid%em_g_rqv_b = 0.0
   grid%em_g_rqv_bt = 0.0
   grid%dtbc = dtbc_save

   CALL Setup_date_string (grid, nl_date_string, 'tn' )
   nl_date_index = 1

   grid%current_time = grid%start_time
   CALL ESMF_ClockSetCurrTime( grid%domain_clock, currTime=grid%current_time, rc=rc )
   CALL ESMF_ClockGetCurrTime( grid%domain_clock, currTime=grid%current_time, rc=rc )

   CALL wrf_message( '-------------- Tangent-linear model ---------------' )
   model_config_rec%dyn_opt = DYN_EM_TL
!Print*, 'CALLING integrate_tl rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr'
   CALL integrate_tl ( grid, nl_date_string, nl_date_index )
!Print*, 'CALLING integrate_tl sssssssssssssssssssssssssssssssssssssssssssssssssssssssss'

   grid%em_g_z=0.; grid%em_g_mudf=0.

   SAVE_L =0.0

   do j=jfs,jfe
     do k=kts,kte
       do i=ifs,ife
         SAVE_L =SAVE_L + grid%em_g_u_1(i,k,j)  * grid%em_g_u_1(i,k,j)    &
                        + grid%em_g_u_2(i,k,j)  * grid%em_g_u_2(i,k,j)    &
                        + grid%em_g_v_1(i,k,j)  * grid%em_g_v_1(i,k,j)    &
                        + grid%em_g_v_2(i,k,j)  * grid%em_g_v_2(i,k,j)    &
                        + grid%em_g_w_1(i,k,j)  * grid%em_g_w_1(i,k,j)    &
                        + grid%em_g_w_2(i,k,j)  * grid%em_g_w_2(i,k,j)    &
                        + grid%em_g_ph_1(i,k,j)  * grid%em_g_ph_1(i,k,j)  &
                        + grid%em_g_ph_2(i,k,j)  * grid%em_g_ph_2(i,k,j)  &
                        + grid%em_g_t_1(i,k,j)  * grid%em_g_t_1(i,k,j)    &
                        + grid%em_g_t_2(i,k,j)  * grid%em_g_t_2(i,k,j)    &
                        + grid%em_g_p(i,k,j)  * grid%em_g_p(i,k,j)    &
                        + grid%em_g_al(i,k,j)  * grid%em_g_al(i,k,j)    &
                        + grid%em_g_z(i,k,j)  * grid%em_g_z(i,k,j)

       enddo
     enddo
   enddo

   do j=jfs,jfe
     do i=ifs,ife
       SAVE_L =SAVE_L + grid%em_g_mu_1(i,j)  * grid%em_g_mu_1(i,j)    &
                      + grid%em_g_mu_2(i,j)  * grid%em_g_mu_2(i,j)    &
                      + g_rainnc(i,j) * g_rainnc(i,j)   &
                      + g_rainncv(i,j) * g_rainncv(i,j) &
                      + grid%em_g_mudf(i,j)  * grid%em_g_mudf(i,j)
     enddo
   enddo

   do h=1,num_moist
     do j=jfs,jfe
       do k=kts,kte
         do i=ifs,ife
           SAVE_L =SAVE_L + grid%g_moist_1(i,k,j,h)  * grid%g_moist_1(i,k,j,h)    &
                          + grid%g_moist_2(i,k,j,h)  * grid%g_moist_2(i,k,j,h)
         enddo
       enddo
     enddo
   enddo

#ifdef DM_PARALLEL
   call wrf_get_dm_communicator(comm)
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum
#endif

   ALPHA_M=1.
   DO NT=1,11

   ALPHA_M=0.1*ALPHA_M
   FACTOR=1.+ALPHA_M
   grid%em_u_1 = FACTOR*S_u_1
   grid%em_u_2 = FACTOR*S_u_2
   grid%em_v_1 = FACTOR*S_v_1
   grid%em_v_2 = FACTOR*S_v_2
   grid%em_w_1 = FACTOR*S_w_1
   grid%em_w_2 = FACTOR*S_w_2
   grid%em_ph_1 = FACTOR*S_ph_1
   grid%em_ph_2 = FACTOR*S_ph_2
   grid%em_t_1 = FACTOR*S_t_1
   grid%em_t_2 = FACTOR*S_t_2
   grid%em_mu_1 = FACTOR*S_mu_1
   grid%em_mu_2 = FACTOR*S_mu_2
   grid%em_p = FACTOR*S_p
   grid%em_al = FACTOR*S_al
   grid%em_z = FACTOR*S_z
   grid%moist_1 = FACTOR*S_moist_1
   grid%moist_2 = FACTOR*S_moist_2
   grid%rainnc = FACTOR*S_rainnc
   grid%rainncv = FACTOR*S_rainncv
   grid%dtbc = dtbc_save

   CALL wrf_message( '-------------- Non-linear model ---------------' )

   grid%current_time = grid%start_time
   CALL ESMF_ClockSetCurrTime( grid%domain_clock, currTime=grid%current_time, rc=rc )
   CALL ESMF_ClockGetCurrTime( grid%domain_clock, currTime=grid%current_time, rc=rc )
   model_config_rec%dyn_opt = DYN_EM

   CALL integrate ( grid )

   grid%em_z=0.; grid%em_mudf=0.

   VAL_N = 0.0

   do j=jfs,jfe
     do k=kts,kte
       do i=ifs,ife
         VAL_N=VAL_N  + (grid%em_u_1(i,k,j)  - B_u_1(i,k,j) ) *(grid%em_u_1(i,k,j)  - B_u_1(i,k,j))     &
                      + (grid%em_u_2(i,k,j)  - B_u_2(i,k,j) ) *(grid%em_u_2(i,k,j)  - B_u_2(i,k,j))     &
                      + (grid%em_v_1(i,k,j)  - B_v_1(i,k,j) ) *(grid%em_v_1(i,k,j)  - B_v_1(i,k,j))     &
                      + (grid%em_v_2(i,k,j)  - B_v_2(i,k,j) ) *(grid%em_v_2(i,k,j)  - B_v_2(i,k,j))     &
                      + (grid%em_w_1(i,k,j)  - B_w_1(i,k,j) ) *(grid%em_w_1(i,k,j)  - B_w_1(i,k,j))     &
                      + (grid%em_w_2(i,k,j)  - B_w_2(i,k,j) ) *(grid%em_w_2(i,k,j)  - B_w_2(i,k,j))     &
                      + (grid%em_ph_1(i,k,j)  - B_ph_1(i,k,j) ) *(grid%em_ph_1(i,k,j)  - B_ph_1(i,k,j)) &
                      + (grid%em_ph_2(i,k,j)  - B_ph_2(i,k,j) ) *(grid%em_ph_2(i,k,j)  - B_ph_2(i,k,j)) &
                      + (grid%em_t_1(i,k,j)  - B_t_1(i,k,j) ) *(grid%em_t_1(i,k,j)  - B_t_1(i,k,j))     &
                      + (grid%em_t_2(i,k,j)  - B_t_2(i,k,j) ) *(grid%em_t_2(i,k,j)  - B_t_2(i,k,j))     &
                      + (grid%em_p(i,k,j)  - B_p(i,k,j) ) *(grid%em_p(i,k,j)  - B_p(i,k,j))     &
                      + (grid%em_al(i,k,j)  - B_al(i,k,j) ) *(grid%em_al(i,k,j)  - B_al(i,k,j))     &
                      + (grid%em_z(i,k,j)  - B_z(i,k,j) ) *(grid%em_z(i,k,j)  - B_z(i,k,j))
       enddo
     enddo
   enddo

   do j=jfs,jfe
     do i=ifs,ife
       VAL_N=VAL_N  + (grid%em_mu_1(i,j) -B_mu_1(i,j))*(grid%em_mu_1(i,j) -B_mu_1(i,j))    &
                    + (grid%em_mu_2(i,j) -B_mu_2(i,j))*(grid%em_mu_2(i,j) -B_mu_2(i,j))    &
                    + (grid%rainnc(i,j) -B_rainnc(i,j))*(grid%rainnc(i,j) -B_rainnc(i,j))  &
                    + (grid%rainncv(i,j) -B_rainncv(i,j))*(grid%rainncv(i,j)-B_rainncv(i,j))  &
                    + (grid%em_mudf(i,j) -B_mudf(i,j))*(grid%em_mudf(i,j) -B_mudf(i,j))
     enddo
   enddo

   do h=1,num_moist
     do j=jfs,jfe
       do k=kts,kte
         do i=ifs,ife
           VAL_N=VAL_N  + (grid%moist_1(i,k,j,h) -B_moist_1(i,k,j,h))*(grid%moist_1(i,k,j,h) -B_moist_1(i,k,j,h))  &
                        + (grid%moist_2(i,k,j,h) -B_moist_2(i,k,j,h))*(grid%moist_2(i,k,j,h) -B_moist_2(i,k,j,h))
         enddo
       enddo
     enddo
   enddo
#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum
#endif

   VAL_L=SAVE_L*ALPHA_M**2
   COEF=VAL_N/VAL_L
   WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
        'g_em_sn: ALPHA_M=',ALPHA_M,'  COEF=',COEF, &
        '  VAL_N=',VAL_N,'  VAL_L=',VAL_L

   ENDDO

!  ADJ test

   FACTOR=0.1

   grid%em_u_1 = S_u_1
   grid%em_u_2 = S_u_2
   grid%em_v_1 = S_v_1
   grid%em_v_2 = S_v_2
   grid%em_w_1 = S_w_1
   grid%em_w_2 = S_w_2
   grid%em_ph_1 = S_ph_1
   grid%em_ph_2 = S_ph_2
   grid%em_t_1 = S_t_1
   grid%em_t_2 = S_t_2
   grid%em_mu_1 = S_mu_1
   grid%em_mu_2 = S_mu_2
   grid%em_p = S_p
   grid%em_al = S_al
   grid%em_z = S_z
   grid%moist_1 = S_moist_1
   grid%moist_2 = S_moist_2
   grid%rainnc = S_rainnc
   grid%rainncv = S_rainncv

   grid%em_g_u_1 = FACTOR*S_u_1
   grid%em_g_u_2 = FACTOR*S_u_2
   grid%em_g_v_1 = FACTOR*S_v_1
   grid%em_g_v_2 = FACTOR*S_v_2
   grid%em_g_w_1 = FACTOR*S_w_1
   grid%em_g_w_2 = FACTOR*S_w_2
   grid%em_g_ph_1 = FACTOR*S_ph_1
   grid%em_g_ph_2 = FACTOR*S_ph_2
   grid%em_g_t_1 = FACTOR*S_t_1
   grid%em_g_t_2 = FACTOR*S_t_2
   grid%em_g_mu_1 = FACTOR*S_mu_1
   grid%em_g_mu_2 = FACTOR*S_mu_2
   grid%em_g_p = FACTOR*S_p
   grid%em_g_al = FACTOR*S_al
   grid%em_g_z = FACTOR*S_z
   grid%g_moist_1 = FACTOR*S_moist_1
   grid%g_moist_2 = FACTOR*S_moist_2
   g_rainnc = FACTOR*S_rainnc
   g_rainncv = FACTOR*S_rainncv

   grid%em_g_u_b = 0.
   grid%em_g_u_bt =0.
   grid%em_g_v_b = 0.
   grid%em_g_v_bt =0.
   grid%em_g_w_b = 0.
   grid%em_g_w_bt =0.
   grid%em_g_ph_b =0.
   grid%em_g_ph_bt =0.
   grid%em_g_t_b = 0.
   grid%em_g_t_bt =0.
   grid%em_g_mu_b =0.
   grid%em_g_mu_bt =0.
   grid%em_g_rqv_b =0.
   grid%em_g_rqv_bt =0.

   B_u_1 = grid%em_g_u_1
   B_u_2 = grid%em_g_u_2
   B_v_1 = grid%em_g_v_1
   B_v_2 = grid%em_g_v_2
   B_w_1 = grid%em_g_w_1
   B_w_2 = grid%em_g_w_2
   B_ph_1 = grid%em_g_ph_1
   B_ph_2 = grid%em_g_ph_2
   B_t_1 = grid%em_g_t_1
   B_t_2 = grid%em_g_t_2
   B_mu_1 = grid%em_g_mu_1
   B_mu_2 = grid%em_g_mu_2
   B_p = grid%em_g_p
   B_al = grid%em_g_al
   B_z = grid%em_g_z
   B_moist_1 = grid%g_moist_1
   B_moist_2 = grid%g_moist_2
   B_rainnc = g_rainnc
   B_rainncv = g_rainncv
   grid%dtbc = dtbc_save

   CALL Setup_date_string (grid, nl_date_string, 'tn' )
   nl_date_index = 1

   grid%current_time = grid%start_time
   CALL ESMF_ClockSetCurrTime( grid%domain_clock, currTime=grid%current_time, rc=rc )
   CALL ESMF_ClockGetCurrTime( grid%domain_clock, currTime=grid%current_time, rc=rc )

   CALL wrf_message( '-------------- Tangent-linear model ---------------' )
   model_config_rec%dyn_opt = DYN_EM_TL
   CALL integrate_tl ( grid, nl_date_string, nl_date_index )

   grid%em_g_z=0.; grid%em_g_mudf=0.

   VAL_L =0.0

   do j=jfs,jfe
     do k=kts,kte
       do i=ifs,ife
         VAL_L =VAL_L + grid%em_g_u_1(i,k,j)  * grid%em_g_u_1(i,k,j)    &
                      + grid%em_g_u_2(i,k,j)  * grid%em_g_u_2(i,k,j)    &
                      + grid%em_g_v_1(i,k,j)  * grid%em_g_v_1(i,k,j)    &
                      + grid%em_g_v_2(i,k,j)  * grid%em_g_v_2(i,k,j)    &
                      + grid%em_g_w_1(i,k,j)  * grid%em_g_w_1(i,k,j)    &
                      + grid%em_g_w_2(i,k,j)  * grid%em_g_w_2(i,k,j)    &
                      + grid%em_g_ph_1(i,k,j)  * grid%em_g_ph_1(i,k,j)  &
                      + grid%em_g_ph_2(i,k,j)  * grid%em_g_ph_2(i,k,j)  &
                      + grid%em_g_t_1(i,k,j)  * grid%em_g_t_1(i,k,j)    &
                      + grid%em_g_t_2(i,k,j)  * grid%em_g_t_2(i,k,j)    &
                      + grid%em_g_p(i,k,j)  * grid%em_g_p(i,k,j)    &
                      + grid%em_g_al(i,k,j)  * grid%em_g_al(i,k,j)    &
                      + grid%em_g_z(i,k,j)  * grid%em_g_z(i,k,j)
       enddo
     enddo
   enddo

   do j=jfs,jfe
     do i=ifs,ife
       VAL_L =VAL_L + grid%em_g_mu_1(i,j)  * grid%em_g_mu_1(i,j)    &
                    + grid%em_g_mu_2(i,j)  * grid%em_g_mu_2(i,j)    &
                    + g_rainnc(i,j) * g_rainnc(i,j)  &
                    + g_rainncv(i,j) * g_rainncv(i,j)  &
                    + grid%em_g_mudf(i,j)  * grid%em_g_mudf(i,j)
     enddo
   enddo

   do h=1,num_moist
     do j=jfs,jfe
       do k=kts,kte
         do i=ifs,ife
           VAL_L =VAL_L + grid%g_moist_1(i,k,j,h)  * grid%g_moist_1(i,k,j,h)    &
                        + grid%g_moist_2(i,k,j,h)  * grid%g_moist_2(i,k,j,h)
         enddo
       enddo
     enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum
#endif

!  ADJ

   grid%em_u_1 = S_u_1
   grid%em_u_2 = S_u_2
   grid%em_v_1 = S_v_1
   grid%em_v_2 = S_v_2
   grid%em_w_1 = S_w_1
   grid%em_w_2 = S_w_2
   grid%em_ph_1 = S_ph_1
   grid%em_ph_2 = S_ph_2
   grid%em_t_1 = S_t_1
   grid%em_t_2 = S_t_2
   grid%em_mu_1 = S_mu_1
   grid%em_mu_2 = S_mu_2
   grid%em_p = S_p
   grid%em_al = S_al
   grid%em_z = S_z
   grid%moist_1 = S_moist_1
   grid%moist_2 = S_moist_2
   grid%rainnc = S_rainnc
   grid%rainncv = S_rainncv

   grid%em_a_u_1 = 0.
   grid%em_a_u_2 = 0.
   grid%em_a_v_1 = 0.
   grid%em_a_v_2 =0.
   grid%em_a_w_1 =0.
   grid%em_a_w_2 =0.
   grid%em_a_ph_1 =0.
   grid%em_a_ph_2 = 0.
   grid%em_a_t_1 = 0.
   grid%em_a_t_2 =0.
   grid%em_a_mu_1 = 0.
   grid%em_a_mu_2 =0.
   grid%em_a_p = 0.
   grid%em_a_al =0.
   grid%em_a_z =0.
   grid%a_moist_1 = 0.
   grid%a_moist_2 =0.
   a_rainnc = 0.
   a_rainncv = 0.
   grid%em_a_u_b = 0.
   grid%em_a_u_bt = 0.
   grid%em_a_v_b = 0.
   grid%em_a_v_bt =0.
   grid%em_a_w_b = 0.
   grid%em_a_w_bt =0.
   grid%em_a_ph_b =0.
   grid%em_a_ph_bt =0.
   grid%em_a_t_b = 0.
   grid%em_a_t_bt =0.
   grid%em_a_mu_b =0.
   grid%em_a_mu_bt =0.
   grid%em_a_rqv_b =0.
   grid%em_a_rqv_bt =0.

   grid%em_a_u_1 = grid%em_g_u_1
   grid%em_a_u_2 = grid%em_g_u_2
   grid%em_a_v_1 = grid%em_g_v_1
   grid%em_a_v_2 = grid%em_g_v_2
   grid%em_a_w_1 = grid%em_g_w_1
   grid%em_a_w_2 = grid%em_g_w_2
   grid%em_a_ph_1 = grid%em_g_ph_1
   grid%em_a_ph_2 = grid%em_g_ph_2
   grid%em_a_t_1 = grid%em_g_t_1
   grid%em_a_t_2 = grid%em_g_t_2
   grid%em_a_mu_1 = grid%em_g_mu_1
   grid%em_a_mu_2 = grid%em_g_mu_2
   grid%em_a_mudf = grid%em_g_mudf
   grid%em_a_p = grid%em_g_p
   grid%em_a_al = grid%em_g_al
   grid%em_a_z = grid%em_g_z
   grid%a_moist_1 = grid%g_moist_1
   grid%a_moist_2 = grid%g_moist_2
   a_rainnc = g_rainnc
   a_rainncv = g_rainncv

   grid%em_g_z=0.; grid%em_g_mudf=0.
   grid%dtbc = dtbc_save

   CALL Setup_date_string (grid, ad_date_string, 'ad' )

   CALL Setup_date_string (grid, nl_date_string, 'an' )

   nl_date_index = 1
   ad_date_index = 1

   grid%current_time = grid%stop_time
   CALL ESMF_ClockSetCurrTime( grid%domain_clock, currTime=grid%current_time, rc=rc )
   CALL ESMF_ClockGetCurrTime( grid%domain_clock, currTime=grid%current_time, rc=rc )

   CALL wrf_message( '-------------- Adjoint-linear model ---------------' )
   model_config_rec%dyn_opt = DYN_EM_AD

!ADDED BY WALLS, Nov 24, 2009
Print*, 'CALLING integrate_ad yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy'
   CALL integrate_ad ( grid, nl_date_string, nl_date_index, ad_date_string, ad_date_index )
Print*, 'CALLING integrate_ad zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'

   VAL_A=0.0

   do j=jfs,jfe
     do k=kts,kte
       do i=ifs,ife
         VAL_A= VAL_A + grid%em_a_u_1(i,k,j) * B_u_1(i,k,j)               &
                      + grid%em_a_u_2(i,k,j) * B_u_2(i,k,j)               &
                      + grid%em_a_v_1(i,k,j) * B_v_1(i,k,j)               &
                      + grid%em_a_v_2(i,k,j) * B_v_2(i,k,j)               &
                      + grid%em_a_w_1(i,k,j) * B_w_1(i,k,j)               &
                      + grid%em_a_w_2(i,k,j) * B_w_2(i,k,j)               &
                      + grid%em_a_ph_1(i,k,j) * B_ph_1(i,k,j)             &
                      + grid%em_a_ph_2(i,k,j) * B_ph_2(i,k,j)             &
                      + grid%em_a_t_1(i,k,j) * B_t_1(i,k,j)               &
                      + grid%em_a_t_2(i,k,j) * B_t_2(i,k,j)               &
                      + grid%em_a_p(i,k,j) * B_p(i,k,j)                   &
                      + grid%em_a_al(i,k,j) * B_al(i,k,j)                 &
                      + grid%em_a_z(i,k,j) * B_z(i,k,j)
       enddo
     enddo
   enddo

   do j=jfs,jfe
     do i=ifs,ife
       VAL_A= VAL_A + grid%em_a_mu_1(i,j) * B_mu_1(i,j)                 &
                    + grid%em_a_mu_2(i,j) * B_mu_2(i,j)                 &
                    + a_rainnc(i,j) * B_rainnc(i,j)                     &
                    + a_rainncv(i,j) * B_rainncv(i,j)     
     enddo
   enddo

   do h=1,num_moist
     do j=jfs,jfe
       do k=kts,kte
         do i=ifs,ife
           VAL_A= VAL_A + grid%a_moist_1(i,k,j,h) * B_moist_1(i,k,j,h)   &
                        + grid%a_moist_2(i,k,j,h) * B_moist_2(i,k,j,h)
         enddo
       enddo
     enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum
#endif

   print*, '                '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

!  RECOVER

   grid%em_u_1 = S_u_1
   grid%em_u_2 = S_u_2
   grid%em_v_1 = S_v_1
   grid%em_v_2 = S_v_2
   grid%em_w_1 = S_w_1
   grid%em_w_2 = S_w_2
   grid%em_ph_1 = S_ph_1
   grid%em_ph_2 = S_ph_2
   grid%em_t_1 = S_t_1
   grid%em_t_2 = S_t_2
   grid%em_mu_1 = S_mu_1
   grid%em_mu_2 = S_mu_2
   grid%em_p = S_p
   grid%em_al = S_al
   grid%em_z = S_z
   grid%moist_1 = S_moist_1
   grid%moist_2 = S_moist_2
   grid%rainnc = S_rainnc
   grid%rainncv = S_rainncv
!  print*,"Attention:",maxval(S_mu_b),minval(S_mu_b)


END SUBROUTINE integrate_tst

END MODULE module_integrate_tst
