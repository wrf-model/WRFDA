subroutine da_get_vpoles(u,v,lon,        &
          ids, ide, jds, jde, kds, kde,  &
          ims, ime, jms, jme, kms, kme,  &
          its, ite, jts, jte, kts, kte   )

   !---------------------------------------------------------------------------
   !  PURPOSE: Treatment for Polar winds  
   !---------------------------------------------------------------------------
   
   implicit none
   
   integer, intent(in)    :: ids, ide, jds, jde, kds, kde
   integer, intent(in)    :: ims, ime, jms, jme, kms, kme
   integer, intent(in)    :: its, ite, jts, jte, kts, kte
   real, intent(inout)    :: u(ims:ime,jms:jme,kms:kme)   ! u wind comp.
   real, intent(inout)    :: v(ims:ime,jms:jme,kms:kme)   ! v wind comp.
   real, intent(in)       :: lon(ims:ime,jms:jme)         ! Longitudes   
 
   real                   :: conv,tmpvar                                         
   real                   :: tmpu,tmp_u,tmpv,tmp_v
   integer                :: k
   real, dimension(ims:ime) :: xls, xln

   ! Stop compiler whinge

   if (kde==0.OR.kds==0) then
   end if

   if (trace_use) call da_trace_entry("da_get_vpoles")

   conv = pi/180.0

   tmpvar       = 1.0/real(ide-ids+1)
   xls(its:ite) = conv*lon(its:ite,jts)
   xln(its:ite) = conv*lon(its:ite,jte)

   do k = kts,kte
      tmp_u =0.0
      tmp_v =0.0
      tmpu = 0.
      tmpv = 0.

      if (jts == jds) then 
         tmp_u = tmpvar*sum(-u(its:ite,jts+1,k)*cos(xls(its:ite))& 
                            +v(its:ite,jts+1,k)*sin(xls(its:ite)))
         tmp_v = tmpvar*sum(-u(its:ite,jts+1,k)*sin(xls(its:ite))& 
                            -v(its:ite,jts+1,k)*cos(xls(its:ite)))
      end if

      tmpu = wrf_dm_sum_real( tmp_u )
      tmpv = wrf_dm_sum_real( tmp_v )

      if (jts == jds) then 
        u(its:ite,jts,k) = -tmpu*cos(xls(its:ite)) -tmpv*sin(xls(its:ite))
        v(its:ite,jts,k) =  tmpu*sin(xls(its:ite)) -tmpv*cos(xls(its:ite))
      end if
 
      tmp_u =0.0
      tmp_v =0.0
      tmpu = 0.
      tmpv = 0.

      if (jte == jde) then 
         tmp_u = tmpvar*sum(-u(its:ite,jte-1,k)*cos(xln(its:ite))& 
                            -v(its:ite,jte-1,k)*sin(xln(its:ite)))
         tmp_v = tmpvar*sum( u(its:ite,jte-1,k)*sin(xln(its:ite))& 
                            -v(its:ite,jte-1,k)*cos(xln(its:ite)))
      end if
      tmpu = wrf_dm_sum_real( tmp_u )
      tmpv = wrf_dm_sum_real( tmp_v )
      if (jte == jde) then 
         u(its:ite,jte,k) = -tmpu*cos(xln(its:ite)) +tmpv*sin(xln(its:ite))
         v(its:ite,jte,k) = -tmpu*sin(xln(its:ite)) -tmpv*cos(xln(its:ite))
      end if
   end do

   if (trace_use) call da_trace_exit("da_get_vpoles")

end subroutine da_get_vpoles


