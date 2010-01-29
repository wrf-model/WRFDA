! ----------------------------------------------------------------------
SUBROUTINE test_solve_em ( grid , config_flags , &
! Actual arguments generated from Registry
#include "em_dummy_args.inc"
!
                    )


! Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm
! Mediation layer modules
! Model layer modules
   USE module_model_constants
! Registry generated module
   USE module_state_description
   
   IMPLICIT NONE

#ifdef DM_PARALLEL
  include "mpif.h"
#endif
   !  Input data.

   TYPE(domain) , TARGET          :: grid

   !  Definitions of dummy arguments to this routine (generated from Registry).
#include <em_dummy_decl.inc>

   !  Structure that contains run-time configuration (namelist) data for domain
   TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

   ! Local data
   INTEGER                         :: k_start , k_end, its, ite, jts, jte
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER                         :: ij , iteration
   INTEGER                         :: im , num_3d_m , ic , num_3d_c
   INTEGER                         :: loop
   INTEGER                         :: ijds, ijde
   INTEGER                         :: itmpstep
   INTEGER                         :: sz

! storage for tendencies and decoupled state (generated from Registry)
#include <em_i1_decl.inc>

   INTEGER :: rc 
   INTEGER :: number_of_small_timesteps, rk_step
   INTEGER :: klevel,ijm,ijp,i,j,k,size1,size2    ! for prints/plots only
   INTEGER :: idum1, idum2, dynamics_option

   INTEGER :: rk_order, iwmax, jwmax, kwmax
   REAL :: dt_rk, dts_rk, dtm, wmax
   LOGICAL :: leapfrog
   INTEGER :: l,kte,kk
   INTEGER :: kts

! These are used if -DDEREF_KLUDGE is compiled
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31  , em31  , sm32  , em32  , sm33  , em33
   INTEGER     :: sm31x , em31x , sm32x , em32x , sm33x , em33x
   INTEGER     :: sm31y , em31y , sm32y , em32y , sm33y , em33y

! Define benchmarking timers if -DBENCH is compiled

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

   ! TLM Diagnostics
   REAL :: ALPHA_M    ! Perturbation value that is added to the NLM input values in each step of the test loop 
   REAL :: VAL_N      ! L2 norm of the perturbations as computed from the perturbed NLM solution, i.e. the difference vector
   REAL :: VAL_L      ! SAVE_L * ALPHA_M**2,
   REAL :: SAVE_L     ! Sum of all values, for g_* ^2 (i..e. all TLM solution values squared)
                      ! L2 norm of the perturbations as computed from the TLM solution
                      ! (the TLM, since it's linear, only has to be computed once, and then the result multiplied by 
		      ! the square of the scaling factor, since the L2 norm is proportional to the square of the perturbations, 
		      ! and the perturbations are proportional to the scaling factor.)
   REAL :: COEF       ! VAL_L / VAL_N; Ratio of the NLM to TLM L2 norm; this should asymptote to unity.

   ! ADM Diagnostics   
   REAL :: VAL_A      ! I.e., VAL_AD in output; Sum of all values; (a_*) * (a_* - 1) 
   ! For ADM, VAL_L is VAL_TL, i.e. Sum of all values, for g_* ^2 (i..e. all TLM solution values squared) 
   
   REAL :: FACTOR, nsum
   INTEGER :: NT,h, ifs, ife, jfs, jfe, comm, ierror
   INTEGER, PARAMETER :: tladj_steps = 1    ! 090923 bp; to enable multiple steps in tladj test
   LOGICAL :: stopOnError                   ! 090924 bp; halt program execution on tladj test failure
   INTEGER :: isolationVariable             ! 091113 bp; isolate selected vars 
                                            ! 0 = none, 1 = u,v,w, 2 = ph, 3 = t, 4 = mu
                                            ! 5 = moist_1,moist_2; 6 = rainnc, rainncv; 7 = p (pressure); 8 = al (inverse density)

! Get isolationVariable from namelist
   isolationVariable = 0 
   call nl_get_isolation_variable(1, isolationVariable)
   if( isolationVariable < 0 .or. isolationVariable > 9 ) call wrf_error_fatal('Invalid isolationVariable value') 

!  Obtain dimension information stored in the grid data structure.
   CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )


   CALL nl_get_stop_on_error( 1, stopOnError )
   if( stopOnError ) STOP 'stopOnError not yet implemented for test_solve_em()'
 
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

   print*,'ips =', ips, ' ipe= ', ipe
   print*,'ids =', ids, ' ide= ', ide
   print*,'jps =', jps, ' jpe= ', jpe
   print*,'jds =', jds, ' jde= ', jde
   print*,'ims =',ims, ' ime =',ime
   print*,'jms =',jms, ' jme =',jme

   print*,'ifs =',ifs,' ife =',ife
   print*,'jfs =',jfs,' jfe =',jfe

   print*,'its =',its,' ite =',ite
   print*,'jts =',jts,' jte =',jte
   print*,'kts =',kts,' kte =',kte

   print*,'num_moist =',num_moist
   print*,'grid%sm31 grid%em31 =',grid%sm31,grid%em31
   print*,'grid%sm32 grid%em32 =',grid%sm32,grid%em32
   print*,'grid%sm33 grid%em33 =',grid%sm33,grid%em33
   print*,'grid%spec_bdy_width =',grid%spec_bdy_width

   dtbc_save= grid%dtbc
   S_u_1 = u_1 
   S_u_2 = u_2 
   S_v_1 = v_1 
   S_v_2 = v_2 
   S_w_1 = w_1 
   S_w_2 = w_2 

   S_ph_1 = ph_1 
   S_ph_2 = ph_2 
   S_t_1 = t_1 
   S_t_2 = t_2 
   S_mu_1 = mu_1 
   S_mu_2 = mu_2 
   S_p = p 
   S_al = al 
   S_moist_1 = moist_1 
   S_moist_2 = moist_2 
   S_rainnc(:,:) = rainnc(:,:)
   S_rainncv(:,:)= rainncv(:,:)

!NLM

   CALL solve_em ( grid , config_flags , &
! Actual arguments generated from Registry
#include "em_dummy_args.inc"
!
                    )

   z=0.; mudf=0.  

   do j=jts,jte
     do k=kts,kte
       do i=its,ite
         B_u_1(i,k,j) = u_1(i,k,j)
         B_u_2(i,k,j) = u_2(i,k,j) 
         B_v_1(i,k,j) = v_1(i,k,j)
         B_v_2(i,k,j) = v_2(i,k,j)
         B_w_1(i,k,j) = w_1(i,k,j)
         B_w_2(i,k,j) = w_2(i,k,j)
         B_ph_1(i,k,j) = ph_1(i,k,j)
         B_ph_2(i,k,j) = ph_2(i,k,j)
         B_t_1(i,k,j) = t_1(i,k,j)
         B_t_2(i,k,j) = t_2(i,k,j)
         B_p(i,k,j) = p(i,k,j)
         B_al(i,k,j) = al(i,k,j)
         B_z(i,k,j) = z(i,k,j)
       enddo
     enddo
   enddo

   do j=jts,jte
     do i=its,ite
       B_mu_1(i,j) =mu_1(i,j)
       B_mu_2(i,j) =mu_2(i,j)
       B_mudf(i,j) =mudf(i,j)
       B_rainnc(i,j)=rainnc(i,j)
       B_rainncv(i,j)=rainncv(i,j)
     enddo
   enddo

   do h=1,num_moist
     do j=jts,jte
       do k=kts,kte
         do i=its,ite
           B_moist_1(i,k,j,h) =moist_1(i,k,j,h)
           B_moist_2(i,k,j,h) =moist_2(i,k,j,h)
         enddo
       enddo
     enddo
   enddo

!  TCL

   u_1 = S_u_1 
   u_2 = S_u_2 
   v_1 = S_v_1 
   v_2 = S_v_2 
   w_1 = S_w_1 
   w_2 = S_w_2 
   ph_1 = S_ph_1 
   ph_2 = S_ph_2 
   t_1 = S_t_1 
   t_2 = S_t_2 
   mu_1 = S_mu_1 
   mu_2 = S_mu_2 
   p = S_p 
   al = S_al 
   moist_1 = S_moist_1 
   moist_2 = S_moist_2 
   rainnc(:,:)=S_rainnc(:,:)
   rainncv(:,:)=S_rainncv(:,:)

   g_u_1 = u_1 
   g_u_2 = u_2 
   g_v_1 = v_1 
   g_v_2 = v_2 
   g_w_1 = w_1 
   g_w_2 = w_2 
   g_ph_1 = ph_1 
   g_ph_2 = ph_2 
   g_t_1 = t_1 
   g_t_2 = t_2 
   g_mu_1 = mu_1 
   g_mu_2 = mu_2 
   g_p = p 
   g_al = al 
   g_moist_1 = moist_1 
   g_moist_2 = moist_2 
   g_rainnc(:,:)=rainnc(:,:)
   g_rainncv(:,:)=rainncv(:,:)
   g_u_b = 0.0 
   g_u_bt = 0.0 
   g_v_b = 0.0 
   g_v_bt = 0.0 
   g_w_b = 0.0 
   g_w_bt = 0.0 
   g_ph_b = 0.0 
   g_ph_bt = 0.0 
   g_t_b = 0.0 
   g_t_bt = 0.0 
   g_mu_b = 0.0 
   g_mu_bt = 0.0 
   g_rqv_b = 0.0 
   g_rqv_bt = 0.0 
   grid%dtbc = dtbc_save

! Isolate selected variable
   call isolateVars(isolationVariable, grid, &
      g_u_1, g_u_2, g_v_1, g_v_2, g_w_1, g_w_2, &
      g_ph_1, g_ph_2, &
      g_t_1, g_t_2, &
      g_mu_1, g_mu_2, &
      g_p, g_al, &
      g_moist_1, g_moist_2, &
      g_rainnc, g_rainncv)

   CALL solve_em_tl ( grid , config_flags , &
! Actual arguments generated from Registry
#include "em_dummy_args.inc"
!
                    )

   call isolateVars(isolationVariable, grid, &
      g_u_1, g_u_2, g_v_1, g_v_2, g_w_1, g_w_2, &
      g_ph_1, g_ph_2, &
      g_t_1, g_t_2, &
      g_mu_1, g_mu_2, &
      g_p, g_al, &
      g_moist_1, g_moist_2, &
      g_rainnc, g_rainncv)

   g_z=0.; g_mudf=0. 

   SAVE_L =0.0

   do j=jfs,jfe
     do k=kts,kte
       do i=ifs,ife
         SAVE_L =SAVE_L + g_u_1(i,k,j)  * g_u_1(i,k,j)    &
                        + g_u_2(i,k,j)  * g_u_2(i,k,j)    &
                        + g_v_1(i,k,j)  * g_v_1(i,k,j)    &
                        + g_v_2(i,k,j)  * g_v_2(i,k,j)    &
                        + g_w_1(i,k,j)  * g_w_1(i,k,j)    &
                        + g_w_2(i,k,j)  * g_w_2(i,k,j)    &
                        + g_ph_1(i,k,j)  * g_ph_1(i,k,j)  &
                        + g_ph_2(i,k,j)  * g_ph_2(i,k,j)  &
                        + g_t_1(i,k,j)  * g_t_1(i,k,j)    &
                        + g_t_2(i,k,j)  * g_t_2(i,k,j)    &
                        + g_p(i,k,j)  * g_p(i,k,j)    &
                        + g_al(i,k,j)  * g_al(i,k,j)    &
                        + g_z(i,k,j)  * g_z(i,k,j)    

       enddo
     enddo
   enddo

   do j=jfs,jfe
     do i=ifs,ife
       SAVE_L =SAVE_L + g_mu_1(i,j)  * g_mu_1(i,j)    &
                      + g_mu_2(i,j)  * g_mu_2(i,j)    &
                      + g_rainnc(i,j) * g_rainnc(i,j)   &
                      + g_rainncv(i,j) * g_rainncv(i,j) &
                      + g_mudf(i,j)  * g_mudf(i,j)    
     enddo
   enddo

   do h=1,num_moist
     do j=jfs,jfe
       do k=kts,kte
         do i=ifs,ife
           SAVE_L =SAVE_L + g_moist_1(i,k,j,h)  * g_moist_1(i,k,j,h)    &
                          + g_moist_2(i,k,j,h)  * g_moist_2(i,k,j,h) 
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
   test_loop: DO NT=1,11

      ALPHA_M=0.1*ALPHA_M
      FACTOR=1.+ALPHA_M
      u_1 = FACTOR*S_u_1 
      u_2 = FACTOR*S_u_2 
      v_1 = FACTOR*S_v_1 
      v_2 = FACTOR*S_v_2 
      w_1 = FACTOR*S_w_1 
      w_2 = FACTOR*S_w_2 
      ph_1 = FACTOR*S_ph_1 
      ph_2 = FACTOR*S_ph_2 
      t_1 = FACTOR*S_t_1 
      t_2 = FACTOR*S_t_2 
      mu_1 = FACTOR*S_mu_1 
      mu_2 = FACTOR*S_mu_2 
      p = FACTOR*S_p 
      al = FACTOR*S_al 
      moist_1 = FACTOR*S_moist_1 
      moist_2 = FACTOR*S_moist_2 
      rainnc = FACTOR*S_rainnc
      rainncv = FACTOR*S_rainncv
      grid%dtbc = dtbc_save

! isolation of selected variable test
      select case (isolationVariable)
      case (0)    ! no isolation
      case (1)    ! isolate u,v,w
         write(*,*) 'In loop: Isolate variables U,V,W'
         ph_1 = S_ph_1 
         ph_2 = S_ph_2 
         t_1 = S_t_1 
         t_2 = S_t_2 
         mu_1 = S_mu_1 
         mu_2 = S_mu_2 
         p = S_p 
         al = S_al 
         moist_1 = S_moist_1 
         moist_2 = S_moist_2 
         rainnc(:,:) = S_rainnc(:,:) 
         rainncv(:,:)= S_rainncv(:,:)
      case(2)
         write(*,*) 'In loop: Isolate variables ph'
         u_1 = S_u_1
         u_2 = S_u_2
         v_1 = S_v_1
         v_2 = S_v_2
         w_1 = S_w_1
         w_2 = S_w_2
         t_1 = S_t_1 
         t_2 = S_t_2 
         mu_1 = S_mu_1 
         mu_2 = S_mu_2 
         p = S_p 
         al = S_al 
         moist_1 = S_moist_1 
         moist_2 = S_moist_2 
         rainnc(:,:) = S_rainnc(:,:) 
         rainncv(:,:)= S_rainncv(:,:)
      case(3)
         write(*,*) 'In loop: Isolate variables T'
         u_1 = S_u_1
         u_2 = S_u_2
         v_1 = S_v_1
         v_2 = S_v_2
         w_1 = S_w_1
         w_2 = S_w_2
         ph_1 = S_ph_1 
         ph_2 = S_ph_2 
         mu_1 = S_mu_1 
         mu_2 = S_mu_2 
         p = S_p 
         al = S_al 
         moist_1 = S_moist_1 
         moist_2 = S_moist_2 
         rainnc(:,:) = S_rainnc(:,:) 
         rainncv(:,:)= S_rainncv(:,:)
      case(4)
         write(*,*) 'In loop: Isolate variables mu'
         u_1 = S_u_1
         u_2 = S_u_2
         v_1 = S_v_1
         v_2 = S_v_2
         w_1 = S_w_1
         w_2 = S_w_2
         ph_1 = S_ph_1 
         ph_2 = S_ph_2 
         t_1 = S_t_1 
         t_2 = S_t_2 
         p = S_p 
         al = S_al 
         moist_1 = S_moist_1 
         moist_2 = S_moist_2 
         rainnc(:,:) = S_rainnc(:,:) 
         rainncv(:,:)= S_rainncv(:,:)
      case(5)
         write(*,*) 'In loop: Isolate variables moist_1, moist_2'
         u_1 = S_u_1
         u_2 = S_u_2
         v_1 = S_v_1
         v_2 = S_v_2
         w_1 = S_w_1
         w_2 = S_w_2
         ph_1 = S_ph_1 
         ph_2 = S_ph_2 
         mu_1 = S_mu_1 
         mu_2 = S_mu_2 
         t_1 = S_t_1 
         t_2 = S_t_2 
         p = S_p 
         al = S_al 
         rainnc(:,:) = S_rainnc(:,:) 
         rainncv(:,:)= S_rainncv(:,:)
      case(6)
         write(*,*) 'In loop: Isolate variables rainnc, rainncv'
         u_1 = S_u_1
         u_2 = S_u_2
         v_1 = S_v_1
         v_2 = S_v_2
         w_1 = S_w_1
         w_2 = S_w_2
         ph_1 = S_ph_1 
         ph_2 = S_ph_2 
         mu_1 = S_mu_1 
         mu_2 = S_mu_2 
         t_1 = S_t_1 
         t_2 = S_t_2 
         p = S_p 
         al = S_al 
         moist_1 = S_moist_1 
         moist_2 = S_moist_2 
      case(7)
         write(*,*) 'In loop: Isolate variables p (pressure)'
         u_1 = S_u_1
         u_2 = S_u_2
         v_1 = S_v_1
         v_2 = S_v_2
         w_1 = S_w_1
         w_2 = S_w_2
         ph_1 = S_ph_1 
         ph_2 = S_ph_2 
         mu_1 = S_mu_1 
         mu_2 = S_mu_2 
         t_1 = S_t_1 
         t_2 = S_t_2 
         al = S_al 
         moist_1 = S_moist_1 
         moist_2 = S_moist_2 
         rainnc(:,:) = S_rainnc(:,:) 
         rainncv(:,:)= S_rainncv(:,:)
      case(8)
         write(*,*) 'In loop: Isolate variables al (inverse density?)'  
         u_1 = S_u_1
         u_2 = S_u_2
         v_1 = S_v_1
         v_2 = S_v_2
         w_1 = S_w_1
         w_2 = S_w_2
         ph_1 = S_ph_1 
         ph_2 = S_ph_2 
         mu_1 = S_mu_1 
         mu_2 = S_mu_2 
         t_1 = S_t_1 
         t_2 = S_t_2 
         p = S_p 
         moist_1 = S_moist_1 
         moist_2 = S_moist_2 
         rainnc(:,:) = S_rainnc(:,:) 
         rainncv(:,:)= S_rainncv(:,:)
      case(9)
         write(*,*) 'In loop: isolate all variables except al and P'
         al = S_al 
         p = S_p 
      end select
   
      CALL solve_em ( grid , config_flags , &
! Actual arguments generated from Registry
#include "em_dummy_args.inc"
!
                       )
! isolation of selected variable test
      select case (isolationVariable)
      case (0)    ! no isolation
      case (1)    ! isolate u,v,w
         write(*,*) 'In loop: Isolate variables U,V,W 2'
         ph_1 = B_ph_1 
         ph_2 = B_ph_2 
         t_1 = B_t_1 
         t_2 = B_t_2 
         mu_1 = B_mu_1 
         mu_2 = B_mu_2 
         p = B_p 
         al = B_al 
         moist_1 = B_moist_1 
         moist_2 = B_moist_2 
         rainnc(:,:) = B_rainnc(:,:) 
         rainncv(:,:)= B_rainncv(:,:)
      case(2)
         write(*,*) 'In loop: Isolate variables ph 2'
         u_1 = B_u_1
         u_2 = B_u_2
         v_1 = B_v_1
         v_2 = B_v_2
         w_1 = B_w_1
         w_2 = B_w_2
         t_1 = B_t_1 
         t_2 = B_t_2 
         mu_1 = B_mu_1 
         mu_2 = B_mu_2 
         p = B_p 
         al = B_al 
         moist_1 = B_moist_1 
         moist_2 = B_moist_2 
         rainnc(:,:) = B_rainnc(:,:) 
         rainncv(:,:)= B_rainncv(:,:)
      case(3)
         write(*,*) 'In loop: Isolate variables T 2'
         u_1 = B_u_1
         u_2 = B_u_2
         v_1 = B_v_1
         v_2 = B_v_2
         w_1 = B_w_1
         w_2 = B_w_2
         ph_1 = B_ph_1 
         ph_2 = B_ph_2 
         mu_1 = B_mu_1 
         mu_2 = B_mu_2 
         p = B_p 
         al = B_al 
         moist_1 = B_moist_1 
         moist_2 = B_moist_2 
         rainnc(:,:) = B_rainnc(:,:) 
         rainncv(:,:)= B_rainncv(:,:)
      case(4)
         write(*,*) 'In loop: Isolate variables mu 2'
         u_1 = B_u_1
         u_2 = B_u_2
         v_1 = B_v_1
         v_2 = B_v_2
         w_1 = B_w_1
         w_2 = B_w_2
         t_1 = B_t_1 
         t_2 = B_t_2 
         ph_1 = B_ph_1 
         ph_2 = B_ph_2 
         p = B_p 
         al = B_al 
         moist_1 = B_moist_1 
         moist_2 = B_moist_2 
         rainnc(:,:) = B_rainnc(:,:) 
         rainncv(:,:)= B_rainncv(:,:)
      case(5)
         write(*,*) 'In loop: Isolate variables moist_1, moist_2'
         u_1 = B_u_1
         u_2 = B_u_2
         v_1 = B_v_1
         v_2 = B_v_2
         w_1 = B_w_1
         w_2 = B_w_2
         t_1 = B_t_1 
         t_2 = B_t_2 
         ph_1 = B_ph_1 
         ph_2 = B_ph_2 
         mu_1 = B_mu_1 
         mu_2 = B_mu_2 
         p = B_p 
         al = B_al 
         rainnc(:,:) = B_rainnc(:,:) 
         rainncv(:,:)= B_rainncv(:,:)
      case(6)
         write(*,*) 'In loop: Isolate variables rainnc, rainncv'
         u_1 = B_u_1
         u_2 = B_u_2
         v_1 = B_v_1
         v_2 = B_v_2
         w_1 = B_w_1
         w_2 = B_w_2
         t_1 = B_t_1 
         t_2 = B_t_2 
         ph_1 = B_ph_1 
         ph_2 = B_ph_2 
         mu_1 = B_mu_1 
         mu_2 = B_mu_2 
         p = B_p 
         al = B_al 
         moist_1 = B_moist_1 
         moist_2 = B_moist_2 
      case(7)
         write(*,*) 'In loop: Isolate variables p (pressure)'
         u_1 = B_u_1
         u_2 = B_u_2
         v_1 = B_v_1
         v_2 = B_v_2
         w_1 = B_w_1
         w_2 = B_w_2
         t_1 = B_t_1 
         t_2 = B_t_2 
         ph_1 = B_ph_1 
         ph_2 = B_ph_2 
         mu_1 = B_mu_1 
         mu_2 = B_mu_2 
         al = B_al 
         moist_1 = B_moist_1 
         moist_2 = B_moist_2 
         rainnc(:,:) = B_rainnc(:,:) 
         rainncv(:,:)= B_rainncv(:,:)
      case(8)
         write(*,*) 'In loop: Isolate variables al (inverse density?) '
         u_1 = B_u_1
         u_2 = B_u_2
         v_1 = B_v_1
         v_2 = B_v_2
         w_1 = B_w_1
         w_2 = B_w_2
         t_1 = B_t_1 
         t_2 = B_t_2 
         ph_1 = B_ph_1 
         ph_2 = B_ph_2 
         mu_1 = B_mu_1 
         mu_2 = B_mu_2 
         p = B_p 
         moist_1 = B_moist_1 
         moist_2 = B_moist_2 
         rainnc(:,:) = B_rainnc(:,:) 
         rainncv(:,:)= B_rainncv(:,:)
      case(9)
         write(*,*) 'In loop: Isolate all variables except al and P'
         al = B_al 
         p = B_p 
      end select

      z=0.; mudf=0. 

      VAL_N = 0.0

      do i=ifs,ife
        do k=kts,kte
          do j=jfs,jfe
            VAL_N=VAL_N  + (u_1(i,k,j)  - B_u_1(i,k,j) ) *(u_1(i,k,j)  - B_u_1(i,k,j))     &
                         + (u_2(i,k,j)  - B_u_2(i,k,j) ) *(u_2(i,k,j)  - B_u_2(i,k,j))     &
                         + (v_1(i,k,j)  - B_v_1(i,k,j) ) *(v_1(i,k,j)  - B_v_1(i,k,j))     &
                         + (v_2(i,k,j)  - B_v_2(i,k,j) ) *(v_2(i,k,j)  - B_v_2(i,k,j))     &
                         + (w_1(i,k,j)  - B_w_1(i,k,j) ) *(w_1(i,k,j)  - B_w_1(i,k,j))     &
                         + (w_2(i,k,j)  - B_w_2(i,k,j) ) *(w_2(i,k,j)  - B_w_2(i,k,j))     &
                         + (ph_1(i,k,j)  - B_ph_1(i,k,j) ) *(ph_1(i,k,j)  - B_ph_1(i,k,j)) &
                         + (ph_2(i,k,j)  - B_ph_2(i,k,j) ) *(ph_2(i,k,j)  - B_ph_2(i,k,j)) &
                         + (t_1(i,k,j)  - B_t_1(i,k,j) ) *(t_1(i,k,j)  - B_t_1(i,k,j))     &
                         + (t_2(i,k,j)  - B_t_2(i,k,j) ) *(t_2(i,k,j)  - B_t_2(i,k,j))     &
                         + (p(i,k,j)  - B_p(i,k,j) ) *(p(i,k,j)  - B_p(i,k,j))     &
                         + (al(i,k,j)  - B_al(i,k,j) ) *(al(i,k,j)  - B_al(i,k,j))     &
                         + (z(i,k,j)  - B_z(i,k,j) ) *(z(i,k,j)  - B_z(i,k,j))     
          enddo
        enddo
      enddo
      do i=ifs,ife
        do j=jfs,jfe
          VAL_N=VAL_N  + (mu_1(i,j) -B_mu_1(i,j))*(mu_1(i,j) -B_mu_1(i,j))    &
                       + (mu_2(i,j) -B_mu_2(i,j))*(mu_2(i,j) -B_mu_2(i,j))    &
                       + (rainnc(i,j) -B_rainnc(i,j))*(rainnc(i,j) -B_rainnc(i,j))  &
                       + (rainncv(i,j) -B_rainncv(i,j))*(rainncv(i,j)-B_rainncv(i,j))  &
                       + (mudf(i,j) -B_mudf(i,j))*(mudf(i,j) -B_mudf(i,j))  
        enddo
      enddo

      do i=ifs,ife
        do k=kts,kte
          do j=jfs,jfe
            do h=1,num_moist
              VAL_N=VAL_N  + (moist_1(i,k,j,h) -B_moist_1(i,k,j,h))*(moist_1(i,k,j,h) -B_moist_1(i,k,j,h))  &
                           + (moist_2(i,k,j,h) -B_moist_2(i,k,j,h))*(moist_2(i,k,j,h) -B_moist_2(i,k,j,h))
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

   ENDDO test_loop   ! end perturbation loop

!  ADJ test

   FACTOR=0.1

   u_1 = S_u_1 
   u_2 = S_u_2 
   v_1 = S_v_1 
   v_2 = S_v_2 
   w_1 = S_w_1 
   w_2 = S_w_2 
   ph_1 = S_ph_1 
   ph_2 = S_ph_2 
   t_1 = S_t_1 
   t_2 = S_t_2 
   mu_1 = S_mu_1 
   mu_2 = S_mu_2 
   p = S_p 
   al = S_al 
   moist_1 = S_moist_1 
   moist_2 = S_moist_2 
   rainnc = S_rainnc
   rainncv = S_rainncv

   g_u_1 = FACTOR*S_u_1 
   g_u_2 = FACTOR*S_u_2 
   g_v_1 = FACTOR*S_v_1 
   g_v_2 = FACTOR*S_v_2 
   g_w_1 = FACTOR*S_w_1 
   g_w_2 = FACTOR*S_w_2 
   g_ph_1 = FACTOR*S_ph_1 
   g_ph_2 = FACTOR*S_ph_2 
   g_t_1 = FACTOR*S_t_1 
   g_t_2 = FACTOR*S_t_2 
   g_mu_1 = FACTOR*S_mu_1 
   g_mu_2 = FACTOR*S_mu_2 
   g_p = FACTOR*S_p 
   g_al = FACTOR*S_al 
   g_moist_1 = FACTOR*S_moist_1 
   g_moist_2 = FACTOR*S_moist_2 
   g_rainnc = FACTOR*S_rainnc
   g_rainncv = FACTOR*S_rainncv

   g_u_b = 0.
   g_u_bt =0.
   g_v_b = 0.
   g_v_bt =0.
   g_w_b = 0.
   g_w_bt =0.
   g_ph_b =0.
   g_ph_bt =0.
   g_t_b = 0.
   g_t_bt =0.
   g_mu_b =0.
   g_mu_bt =0.
   g_rqv_b =0.
   g_rqv_bt =0.

! Variable isolation
   call isolateVars(isolationVariable, grid, &
      g_u_1, g_u_2, g_v_1, g_v_2, g_w_1, g_w_2, &
      g_ph_1, g_ph_2, &
      g_t_1, g_t_2, &
      g_mu_1, g_mu_2, &
      g_p, g_al, &
      g_moist_1, g_moist_2, &
      g_rainnc, g_rainncv)
   g_z=0.; g_mudf=0. 

   B_u_1 = g_u_1 
   B_u_2 = g_u_2 
   B_v_1 = g_v_1 
   B_v_2 = g_v_2 
   B_w_1 = g_w_1 
   B_w_2 = g_w_2 
   B_ph_1 = g_ph_1 
   B_ph_2 = g_ph_2 
   B_t_1 = g_t_1 
   B_t_2 = g_t_2 
   B_mu_1 = g_mu_1 
   B_mu_2 = g_mu_2 
   B_p = g_p 
   B_al = g_al 
   B_moist_1 = g_moist_1 
   B_moist_2 = g_moist_2 
   B_rainnc = g_rainnc
   B_rainncv = g_rainncv
   grid%dtbc = dtbc_save

   CALL solve_em_tl ( grid , config_flags , &
! Actual arguments generated from Registry
#include "em_dummy_args.inc"
!
                    )

! Variable isolation
   call isolateVars(isolationVariable, grid, &
      g_u_1, g_u_2, g_v_1, g_v_2, g_w_1, g_w_2, &
      g_ph_1, g_ph_2, &
      g_t_1, g_t_2, &
      g_mu_1, g_mu_2, &
      g_p, g_al, &
      g_moist_1, g_moist_2, &
      g_rainnc, g_rainncv)
   g_z=0.; g_mudf=0. 

   VAL_L =0.0

   do j=jfs,jfe
     do k=kts,kte
       do i=ifs,ife
         VAL_L =VAL_L + g_u_1(i,k,j)  * g_u_1(i,k,j)    &
                      + g_u_2(i,k,j)  * g_u_2(i,k,j)    &
                      + g_v_1(i,k,j)  * g_v_1(i,k,j)    &
                      + g_v_2(i,k,j)  * g_v_2(i,k,j)    &
                      + g_w_1(i,k,j)  * g_w_1(i,k,j)    &
                      + g_w_2(i,k,j)  * g_w_2(i,k,j)    &
                      + g_ph_1(i,k,j)  * g_ph_1(i,k,j)  &
                      + g_ph_2(i,k,j)  * g_ph_2(i,k,j)  &
                      + g_t_1(i,k,j)  * g_t_1(i,k,j)    &
                      + g_t_2(i,k,j)  * g_t_2(i,k,j)    &
                      + g_p(i,k,j)  * g_p(i,k,j)    &
                      + g_al(i,k,j)  * g_al(i,k,j)    &
                      + g_z(i,k,j)  * g_z(i,k,j)
       enddo
     enddo
   enddo

   do j=jfs,jfe
     do i=ifs,ife
       VAL_L =VAL_L + g_mu_1(i,j)  * g_mu_1(i,j)    &
                    + g_mu_2(i,j)  * g_mu_2(i,j)    &
                    + g_rainnc(i,j) * g_rainnc(i,j)  &
                    + g_rainncv(i,j) * g_rainncv(i,j)  &
                    + g_mudf(i,j)  * g_mudf(i,j)
     enddo
   enddo

   do h=1,num_moist
     do j=jfs,jfe
       do k=kts,kte
         do i=ifs,ife
           VAL_L =VAL_L + g_moist_1(i,k,j,h)  * g_moist_1(i,k,j,h)    &
                        + g_moist_2(i,k,j,h)  * g_moist_2(i,k,j,h)
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

   u_1 = S_u_1 
   u_2 = S_u_2 
   v_1 = S_v_1 
   v_2 = S_v_2 
   w_1 = S_w_1 
   w_2 = S_w_2 
   ph_1 = S_ph_1 
   ph_2 = S_ph_2 
   t_1 = S_t_1 
   t_2 = S_t_2 
   mu_1 = S_mu_1 
   mu_2 = S_mu_2 
   p = S_p 
   al = S_al 
   moist_1 = S_moist_1 
   moist_2 = S_moist_2 
   rainnc = S_rainnc
   rainncv = S_rainncv

   a_u_1 = 0.
   a_u_2 = 0.
   a_v_1 = 0.
   a_v_2 =0.
   a_w_1 =0.
   a_w_2 =0.
   a_ph_1 =0.
   a_ph_2 = 0.
   a_t_1 = 0.
   a_t_2 =0.
   a_mu_1 = 0.
   a_mu_2 =0.
   a_p = 0.
   a_al =0.
   a_moist_1 = 0.
   a_moist_2 =0.
   a_rainnc = 0.
   a_rainncv = 0.
   a_u_b = 0.
   a_u_bt = 0.
   a_v_b = 0.
   a_v_bt =0.
   a_w_b = 0.
   a_w_bt =0.
   a_ph_b =0.
   a_ph_bt =0.
   a_t_b = 0.
   a_t_bt =0.
   a_mu_b =0.
   a_mu_bt =0.
   a_rqv_b =0.
   a_rqv_bt =0.

   a_u_1 = g_u_1 
   a_u_2 = g_u_2 
   a_v_1 = g_v_1 
   a_v_2 = g_v_2 
   a_w_1 = g_w_1 
   a_w_2 = g_w_2 
   a_ph_1 = g_ph_1 
   a_ph_2 = g_ph_2 
   a_t_1 = g_t_1 
   a_t_2 = g_t_2 
   a_mu_1 = g_mu_1 
   a_mu_2 = g_mu_2 
   a_mudf = g_mudf 
   a_p = g_p 
   a_al = g_al 
   a_z = g_z 
   a_moist_1 = g_moist_1 
   a_moist_2 = g_moist_2 
   a_rainnc = g_rainnc
   a_rainncv = g_rainncv

   g_z=0.; g_mudf=0.
   grid%dtbc = dtbc_save

   CALL solve_em_ad ( grid , config_flags , &
!
#include "em_dummy_args.inc"
!
                 )

   VAL_A = 0.0

   do j=jfs,jfe
     do k=kts,kte
       do i=ifs,ife
         VAL_A = VAL_A + a_u_1(i,k,j) * B_u_1(i,k,j)               &
                      + a_u_2(i,k,j) * B_u_2(i,k,j)               &
                      + a_v_1(i,k,j) * B_v_1(i,k,j)               &
                      + a_v_2(i,k,j) * B_v_2(i,k,j)               &
                      + a_w_1(i,k,j) * B_w_1(i,k,j)               &
                      + a_w_2(i,k,j) * B_w_2(i,k,j)               & 
                      + a_ph_1(i,k,j) * B_ph_1(i,k,j)             &
                      + a_ph_2(i,k,j) * B_ph_2(i,k,j)             &
                      + a_t_1(i,k,j) * B_t_1(i,k,j)               &
                      + a_t_2(i,k,j) * B_t_2(i,k,j)               &
                      + a_p(i,k,j) * B_p(i,k,j)                   &
                      + a_al(i,k,j) * B_al(i,k,j)  
       enddo
     enddo
   enddo

   do j=jfs,jfe
     do i=ifs,ife
       VAL_A= VAL_A + a_mu_1(i,j) * B_mu_1(i,j)                 &
                    + a_rainnc(i,j) * B_rainnc(i,j)      &
                    + a_rainncv(i,j) * B_rainncv(i,j)     &
                    + a_mu_2(i,j) * B_mu_2(i,j)        
     enddo
   enddo

   do h=1,num_moist
     do j=jfs,jfe
       do k=kts,kte
         do i=ifs,ife
           VAL_A= VAL_A + a_moist_1(i,k,j,h) * B_moist_1(i,k,j,h)   &
                        + a_moist_2(i,k,j,h) * B_moist_2(i,k,j,h)   
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
   write(6,fmt='(A,E22.13)') '      g_em_sn: VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      g_em_sn: VAL_AD: ', VAL_A
   !  Halt execution if specified in namelist

!  RECOVER

   u_1 = S_u_1
   u_2 = S_u_2
   v_1 = S_v_1
   v_2 = S_v_2
   w_1 = S_w_1
   w_2 = S_w_2
   ph_1 = S_ph_1
   ph_2 = S_ph_2
   t_1 = S_t_1
   t_2 = S_t_2
   mu_1 = S_mu_1
   mu_2 = S_mu_2
   p = S_p
   al = S_al
   z = S_z 
   moist_1 = S_moist_1
   moist_2 = S_moist_2
   rainnc = S_rainnc
   rainncv = S_rainncv
! bp; ??
 ! print*,"test_solve_em(): Attention:",maxval(S_mu_b),minval(S_mu_b)

END SUBROUTINE test_solve_em

!
! Isolate selected variable. 
!
subroutine isolateVars(isolationVar, &
   grid, &
   u_1, u_2, v_1, v_2, w_1, w_2, &
   ph_1, ph_2, &
   t_1, t_2, &
   mu_1, mu_2, &
   p, al, &
   moist_1, moist_2 , &
   rainnc, rainncv &
   )

   USE module_domain

   IMPLICIT NONE

   TYPE(domain) , TARGET          :: grid
   integer, intent(in) :: isolationVar
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: u_1
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: u_2
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: v_1
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: v_2
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: w_1
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: w_2
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: ph_1
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: ph_2
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: t_1
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: t_2
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: p
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: al
   real , dimension(grid%sm31:grid%em31,grid%sm33:grid%em33) :: mu_1 
   real , dimension(grid%sm31:grid%em31,grid%sm33:grid%em33) :: mu_2
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist) :: moist_1
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist) :: moist_2
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: rainnc
   real , dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: rainncv


   select case ( isolationVar )
   case (0)    ! no isolation
   case (1)    ! isolate u,v,w
      write(*,*) 'Isolate variables U,V,W'
      ph_1 = 0. 
      ph_2 = 0.
      t_1 = 0. 
      t_2 = 0. 
      mu_1 = 0.
      mu_2 = 0.
      p = 0. 
      al = 0. 
      moist_1 = 0.
      moist_2 = 0.
      rainnc = 0.
      rainncv= 0.
   case (2)   ! ph
      write(*,*) 'Isolate variables ph'
      u_1 = 0. 
      u_2 = 0. 
      v_1 = 0. 
      v_2 = 0. 
      w_1 = 0. 
      w_2 = 0. 
      t_1 = 0. 
      t_2 = 0. 
      mu_1 = 0.
      mu_2 = 0.
      p = 0. 
      al = 0. 
      moist_1 = 0.
      moist_2 = 0.
      rainnc = 0.
      rainncv= 0.
   case (3)   ! T
      write(*,*) 'Isolate variables T'
      u_1 = 0. 
      u_2 = 0. 
      v_1 = 0. 
      v_2 = 0. 
      w_1 = 0. 
      w_2 = 0. 
      ph_1 = 0. 
      ph_2 = 0.
      mu_1 = 0.
      mu_2 = 0.
      p = 0. 
      al = 0. 
      moist_1 = 0.
      moist_2 = 0.
      rainnc = 0.
      rainncv= 0.
   case (4)   ! mu 
      write(*,*) 'Isolate variables mu'
      u_1 = 0. 
      u_2 = 0. 
      v_1 = 0. 
      v_2 = 0. 
      w_1 = 0. 
      w_2 = 0. 
      ph_1 = 0. 
      ph_2 = 0.
      t_1 = 0. 
      t_2 = 0. 
      p = 0. 
      al = 0. 
      moist_1 = 0.
      moist_2 = 0.
      rainnc = 0.
      rainncv= 0.
   case (5)   ! moist_1, moist_2 
      write(*,*) 'Isolate moist_* '
      u_1 = 0. 
      u_2 = 0. 
      v_1 = 0. 
      v_2 = 0. 
      w_1 = 0. 
      w_2 = 0. 
      ph_1 = 0. 
      ph_2 = 0.
      mu_1 = 0.
      mu_2 = 0.
      t_1 = 0. 
      t_2 = 0. 
      p = 0. 
      al = 0. 
      rainnc = 0.
      rainncv= 0.
   case (6)   ! rainnc, rainncv
      write(*,*) 'Isolate rainnc, rainncv '
      u_1 = 0. 
      u_2 = 0. 
      v_1 = 0. 
      v_2 = 0. 
      w_1 = 0. 
      w_2 = 0. 
      ph_1 = 0. 
      ph_2 = 0.
      mu_1 = 0.
      mu_2 = 0.
      t_1 = 0. 
      t_2 = 0. 
      p = 0. 
      al = 0. 
      moist_1 = 0.
      moist_2 = 0.
   case (7)   ! pressure 
      write(*,*) 'Isolate p'
      u_1 = 0. 
      u_2 = 0. 
      v_1 = 0. 
      v_2 = 0. 
      w_1 = 0. 
      w_2 = 0. 
      ph_1 = 0. 
      ph_2 = 0.
      mu_1 = 0.
      mu_2 = 0.
      t_1 = 0. 
      t_2 = 0. 
      al = 0. 
      moist_1 = 0.
      moist_2 = 0.
      rainnc = 0.
      rainncv= 0.
   case (8)   ! al (inverse density) 
      write(*,*) 'Isolate al'
      u_1 = 0. 
      u_2 = 0. 
      v_1 = 0. 
      v_2 = 0. 
      w_1 = 0. 
      w_2 = 0. 
      ph_1 = 0. 
      ph_2 = 0.
      mu_1 = 0.
      mu_2 = 0.
      t_1 = 0. 
      t_2 = 0. 
      p = 0. 
      moist_1 = 0.
      moist_2 = 0.
      rainnc = 0.
      rainncv= 0.
   case (9) !  Isolate all except P and al
      al = 0. 
      p = 0. 
   case default
      call wrf_error_fatal('Invalid isolation variable in isolateVars')
   end select
   
end subroutine isolateVars
