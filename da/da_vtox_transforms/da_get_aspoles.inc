subroutine da_get_aspoles(x,              &
          ids, ide, jds, jde, kds, kde,  &
          ims, ime, jms, jme, kms, kme,  &
          its, ite, jts, jte, kts, kte   )

   !---------------------------------------------------------------------------
   ! PURPOSE: Treatment for Adjoint of Scalar field at Poles
   !---------------------------------------------------------------------------

   implicit none
   
   integer, intent(in)    :: ids, ide, jds, jde, kds, kde
   integer, intent(in)    :: ims, ime, jms, jme, kms, kme
   integer, intent(in)    :: its, ite, jts, jte, kts, kte
   real,    intent(inout) :: x(ims:ime,jms:jme,kms:kme)   

   integer                :: k
   real                   :: tmpvar,tmps,tmp_s

   tmpvar      = 1.0/real(ide-ids+1)

   ! Stop compiler whinge

   if (kde ==0) then
   end if

   if (trace_use) call da_trace_entry("da_get_aspoles")

   do k = kts, kte
      tmps =0.0  ; tmp_s =0.0
      if (jts == jds) tmp_s = tmpvar*sum(x(its:ite,jts,k))

      tmps = wrf_dm_sum_real( tmp_s )
      if (jts == jds) then
         x(its:ite,jts+1,k) = x(its:ite,jts+1,k) + tmps
         x(its:ite,jts,k) = 0.0
      end if

      tmps =0.0  ; tmp_s =0.0
      if (jte == jde) tmp_s = tmpvar*sum(x(its:ite,jte,k))

      tmps = wrf_dm_sum_real( tmp_s )
      if (jte == jde) then
         x(its:ite,jte-1,k) = x(its:ite,jte-1,k) + tmps
         x(its:ite,jte,k) = 0.0
      end if
   end do

   if (trace_use) call da_trace_exit("da_get_aspoles")

end subroutine da_get_aspoles

