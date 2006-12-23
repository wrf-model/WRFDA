subroutine da_get_innov_vector_ssmi_rv( it, xb, ob, iv)

   implicit none

   integer, intent(in)           :: it         ! External iteration.
   type(xb_type), intent(in)    :: xb         ! first guess state.
   type(y_type),  intent(in)    :: ob         ! Observation structure.
   type(ob_type), intent(inout) :: iv         ! O-B structure.

   integer                      :: n           ! Loop counter.
   integer                      :: i, j        ! Index dimension.
   real                         :: dx, dxm     ! Interpolation weights.
   real                         :: dy, dym     ! Interpolation weights.
   real                         :: model_tpw   ! Model value tpw   at oblocation
   real                         :: model_speed ! Model value speed at oblocation
   integer                      :: itpw, itpwf, ispeed, ispeedf  
                  
   if (iv % num_ssmi_retrieval > 0) then
      itpw = 0 ; itpwf = 0 ; ispeed = 0 ; ispeedf = 0 
      do n=1, iv % num_ssmi_retrieval

         ! COMPUTE inNOVATION VECTOR
         ! =========================

         ! Obs coordinates on model grid

         ! TPW

         i = iv%ssmi_retrieval(n)%loc%i
         j = iv%ssmi_retrieval(n)%loc%j
         dx = iv%ssmi_retrieval(n)%loc%dx
         dy = iv%ssmi_retrieval(n)%loc%dy
         dxm = iv%ssmi_retrieval(n)%loc%dxm
         dym = iv%ssmi_retrieval(n)%loc%dym


         iv % ssmi_retrieval(n) % tpw % inv  = 0.0
         if (abs(ob % ssmi_retrieval(n) % tpw - missing_r) > 1.  .and.  &  
               iv % ssmi_retrieval(n) % tpw % qc >= obs_qc_pointer) then
            model_tpw = dym*(dxm*xb%tpw(i,j)   + dx*xb%tpw(i+1,j)) + &
                           dy *(dxm*xb%tpw(i,j+1) + dx*xb%tpw(i+1,j+1))

            iv % ssmi_retrieval(n) % tpw % inv = &
               ob % ssmi_retrieval(n) % tpw - model_tpw
         end if

         ! SURFACE WinD SPEED

         iv % ssmi_retrieval(n) % speed % inv  = 0.0
         if (abs(ob % ssmi_retrieval(n) % speed - missing_r) > 1. .and.     &
                iv % ssmi_retrieval(n) % speed % qc >= obs_qc_pointer) then

            model_speed = dym*(dxm*xb%speed(i,j ) + dx*xb%speed(i+1,j )) &
                        + dy *(dxm*xb%speed(i,j+1) + dx*xb%speed(i+1,j+1))
            iv % ssmi_retrieval(n) % speed % inv =  &
               ob % ssmi_retrieval(n) % speed - model_speed
         end if

         !------------------------------------------------------------------
         ! Perform optional maximum error check:
         !------------------------------------------------------------------

         if (check_max_iv) then
            call da_check_max_iv_ssmi_rv(it, iv % ssmi_retrieval(n),&
               itpw,itpwf, ispeed,ispeedf)
         end if
      end do

      if (check_max_iv_print) then
         write(unit = check_max_iv_unit, fmt ='(A,i5,A)') &
            'For outer iteration ',it,&
            ' Total Rejections for SSMI(TPW and SPEED) follows:'
         write(unit = check_max_iv_unit, fmt = '(/,4(2(A,I6),/))') &
            'Number of failed ssmi tpw   observations:  ',itpwf,  ' on ',itpw,   &
            'Number of failed ssmi speed observations:  ',ispeedf,' on ',ispeed
      end if
   end if

end subroutine da_get_innov_vector_ssmi_rv


