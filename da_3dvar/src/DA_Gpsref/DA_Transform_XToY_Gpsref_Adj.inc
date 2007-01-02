SUBROUTINE DA_TRANSFORM_XTOY_GPSRef_ADJ( iv, xp, jo_grad_y, jo_grad_x, xb)

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(IN)    :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)
   TYPE (xb_type),  INTENT(IN)   :: xb          ! first guess state.

   INTEGER                       :: n        ! Loop counter.
   INTEGER                       :: i, j     ! Index dimension.
   REAL                          :: dx, dxm  !
   REAL                          :: dy, dym  !
   INTEGER                       :: num_levs ! obs vertical levels

   IF ( iv%num_gpsref < 1 ) return

   do n=iv%ob_numb(iv%current_ob_time-1)%gpsref + 1, iv%ob_numb(iv%current_ob_time)%gpsref

      if(iv%gpsref(n)%loc%proc_domain_with_halo) then

         num_levs = iv % gpsref(n) % info % levels

         i = iv%gpsref(n)%loc%i
         j = iv%gpsref(n)%loc%j
         dx = iv%gpsref(n)%loc%dx
         dy = iv%gpsref(n)%loc%dy
         dxm = iv%gpsref(n)%loc%dxm
         dym = iv%gpsref(n)%loc%dym

#ifndef DEREF_KLUDGE
         call Interp_lin_3D_adj(jo_grad_x %ref, xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%gpsref(n)%ref, num_levs, &
                                iv%gpsref(n)%zk, num_levs)
#else
         call Interp_lin_3D_adj(jo_grad_x %ref(xp%ims,xp%jms,xp%kms), xp, &
                                i, j, dx, dy, dxm, dym, &
                                jo_grad_y%gpsref(n)%ref(1), num_levs, &
                                iv%gpsref(n)%zk(1), num_levs)
#endif

      endif

   ENDDO

END SUBROUTINE DA_TRANSFORM_XTOY_GPSRef_ADJ

