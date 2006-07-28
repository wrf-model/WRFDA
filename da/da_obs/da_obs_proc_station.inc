  subroutine da_obs_proc_station(obs)

! Processing obs data read in at a station:
!
! (1) Surface correction
! (2) Vertical checks
! (3) Missing data check
!
                              
   TYPE (multi_level_type), intent(inout) :: obs

   REAL :: po, to, rho, es, qs, qvo
   INTEGER :: i

!-----------------------------------------------------------------------------

loop_levels: DO i = 1, obs % info % levels
!-----------------------------------------------------------------------------
!   Gross check for t, p & q
!-----------------------------------------------------------------------------

   if (obs%each(i)%t %inv <  75.0 .or. obs%each(i)%t %inv > 350.0   ) then
                                       obs%each(i)%t %inv = missing_r
                                       obs%each(i)%t %qc  = missing
   endif
   if (obs%each(i)%p %inv <=  0.0 .or. obs%each(i)%p %inv > 110000.0) then
                                       obs%each(i)%p %inv = missing_r
                                       obs%each(i)%p %qc  = missing
   endif
   if (obs%each(i)%rh%inv < 0.0)  then
                                       obs%each(i)%rh%inv = missing_r
                                       obs%each(i)%rh%qc  = missing
   endif
!-----------------------------------------------------------------------------

      po   = obs % each(i) % p  % inv
      to   = obs % each(i) % t  % inv
      rho  = obs % each(i) % rh  % inv

      if ( ob_format == 2 ) then ! Calculate q if possible:

         if (abs(po -  missing_r) > 1. .and. abs(to -  missing_r) > 1. .and. &
             abs(rho-  missing_r) > 1.) then

             call da_tp_to_qs(to, po, es, qs )

             if (rho > 100.) then
               qvo = qs
             else
               qvo  = rho * qs / 100.
             endif

         else
             qvo       = missing_r
         endif

         obs % each(i) % q  % inv = qvo
         obs % each(i) % q  % qc = obs % each(i) % rh % qc
         obs % each(i) % q  % error = obs % each(i) % rh % error

      endif

   enddo loop_levels

end subroutine da_obs_proc_station

