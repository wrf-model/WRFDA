subroutine da_add_flow_dependence_xa( ne, ep, vp, xa, &
                                      its,ite, jts,jte, kts,kte )

   !-----------------------------------------------------------------------
   ! Purpose : Add flow-dependent increments in model space (xa).
   ! Owner: Dale Barker
   !-----------------------------------------------------------------------
                                      
   implicit none
   
   integer, intent(in)                  :: ne  ! Ensemble size.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbations.
   type (vp_type), intent(in)           :: vp  ! CV on grid structure.
   type (x_type), intent(inout)         :: xa  ! Analysis increments.
   integer, intent(in)                  :: its, ite, jts, jte, kts, kte
   
   integer                              :: n, k! Loop counters.

   do n = 1, ne
      do k = kts, kte

         ! u:
         xa % u(its:ite,jts:jte,k) = xa % u(its:ite,jts:jte,k) + &
                                     vp % alpha(its:ite,jts:jte,n) * &
                                     ep % v1(its:ite,jts:jte,k,n) ! v1 = u
         ! v:
         xa % v(its:ite,jts:jte,k) = xa % v(its:ite,jts:jte,k) + &
                                     vp % alpha(its:ite,jts:jte,n) * &
                                     ep % v2(its:ite,jts:jte,k,n) ! v2 = v
         ! t:
         xa % t(its:ite,jts:jte,k) = xa % t(its:ite,jts:jte,k) + &
                                     vp % alpha(its:ite,jts:jte,n) * &
                                     ep % v3(its:ite,jts:jte,k,n) ! v3 = t
         ! q:
         xa % q(its:ite,jts:jte,k) = xa % q(its:ite,jts:jte,k) + &
                                     vp % alpha(its:ite,jts:jte,n) * &
                                     ep % v4(its:ite,jts:jte,k,n) ! v4 = q
      end do

     ! ps:
     xa % psfc(its:ite,jts:jte) = xa % psfc(its:ite,jts:jte) + &
                                  vp % alpha(its:ite,jts:jte,n) * &
                                  ep % v5(its:ite,jts:jte,1,n) ! v5 = ps
   end do

   IF (trace_use) call da_trace_exit("da_add_flow_dependence_xa")

end subroutine da_add_flow_dependence_xa

