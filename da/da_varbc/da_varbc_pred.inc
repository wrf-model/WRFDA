subroutine da_varbc_pred(inst, iv)

   !---------------------------------------------------------------------------
   !  PURPOSE: Calculate and normalize bias predictors
   !
   ! pred(1) - 1 (Constant)
   ! pred(2) - 1000-300 thickness 43 1005.43-521.46 thickness
   ! pred(3) - 200-50 thickness   43  194.36-56.73  thickness
   ! pred(4) - T_skin
   ! pred(5) - total column precipitable water
   ! pred(6) - satellite scan position
   ! pred(7) - satellite scan position **2
   ! pred(8) - satellite scan position **3
   !
   ! pred(9) - 10 -2 thickness    43  10.37-2.61 thickness
   ! pred(10)- 50 -10thickness    43  45.29-10.37 thickness
   !
   !  Called from da_varbc_coldstart
   !
   !  HISTORY: 10/26/2007 - Creation                  Tom Auligne
   !---------------------------------------------------------------------------
   
  implicit none

  integer,parameter   :: npred_hk = 4       ! Number of H&K bias predictors

  integer, intent(in)           :: inst     ! Instrument number
  type (iv_type), intent(inout) :: iv       ! O-B structure.

  integer   :: nlevels, i, npredmax, n
  real      :: pred_hk(npred_hk)


  if (trace_use) call da_trace_entry("da_varbc_pred")

!  write(unit=stdout,fmt='(A,A)') 'VARBC: Computing bias preditors for', &
!  	         trim(adjustl(iv%instid(inst)%rttovid_string)) 

  do n= iv%instid(inst)%info%n1,iv%instid(inst)%info%n2   ! loop for pixel      	
       
  ! get H&K predictors
  !--------------------
  if (rtm_option==rtm_option_rttov) then
#ifdef RTTOV
      call da_predictor_rttov( &
	   pred_hk(1:npred_hk), npred_hk, &
	   iv%instid(inst)%t(1:43,n), &
           iv%instid(inst)%mr(1:43,n)/q2ppmv, &
	   iv%instid(inst)%ts(n))
#endif
   else if (rtm_option==rtm_option_crtm) then
#ifdef CRTM
      nlevels=iv%instid(inst)%nlevels-1
      call da_predictor_crtm( &
	   pred_hk(1:npred_hk), npred_hk, nlevels,&
           iv%instid(inst)%tm(1:nlevels,n), &
           iv%instid(inst)%qm(1:nlevels,n), &
	   iv%instid(inst)%ts(n), &
           iv%instid(inst)%pm(1:nlevels,n),&
	   iv%instid(inst)%pf(0:nlevels,n))
#endif
   end if     

  ! Populate predictors
  !---------------------  
   npredmax = iv%instid(inst)%varbc_info%npredmax
   iv%instid(inst)%varbc_info%pred(1:npredmax,n) = 0.0
  
  ! Constant predictor
   iv%instid(inst)%varbc_info%pred(1,n) = 1.0 
  
  ! H&K predictors
   if (npredmax >= 2) iv%instid(inst)%varbc_info%pred(2,n) = pred_hk(1)
   if (npredmax >= 3) iv%instid(inst)%varbc_info%pred(3,n) = pred_hk(2)
   if (npredmax >= 4) iv%instid(inst)%varbc_info%pred(4,n) = pred_hk(3)
   if (npredmax >= 5) iv%instid(inst)%varbc_info%pred(5,n) = pred_hk(4)
		
  ! Scan predictors	
   if (npredmax >= 6) iv%instid(inst)%varbc_info%pred(6,n) = iv%instid(inst)%scanpos(n)
   if (npredmax >= 7) iv%instid(inst)%varbc_info%pred(7,n) = iv%instid(inst)%scanpos(n)**2
   if (npredmax >= 8) iv%instid(inst)%varbc_info%pred(8,n) = iv%instid(inst)%scanpos(n)**3

   end do                        ! pixel loop
   
   if (trace_use) call da_trace_exit("da_varbc_pred")

end subroutine da_varbc_pred
