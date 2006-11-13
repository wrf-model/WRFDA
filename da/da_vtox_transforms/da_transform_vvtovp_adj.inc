subroutine da_transform_vvtovp_adj( evec, eval, vertical_wgt, &
                                    vp, vv, mz, &
                                    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte )

   !---------------------------------------------------------------------------
   ! PURPOSE: Adjoint of da_transform_vvtovp.
   !---------------------------------------------------------------------------

   implicit none
   
   integer, intent(in)   :: mz                         ! # vertical modes.
   integer, intent(in)   :: ids,ide, jds,jde, kds,kde  ! domain dims.
   integer, intent(in)   :: ims,ime, jms,jme, kms,kme  ! memory dims.
   integer, intent(in)   :: its,ite, jts,jte, kts,kte  ! tile   dims

   real, intent(in)      :: evec(jds:jde,kds:kde,1:mz) ! Eigenvectors.
   real, intent(in)      :: eval(jds:jde,1:mz)         ! Eigenvalues.
   real, intent(in)      :: vertical_wgt(ims:ime,jms:jme,kms:kme) ! Weighting.
   real, intent(inout)   :: vp(ims:ime,jms:jme,kms:kme)! CV in level space.
   real, intent(out)     :: vv(ims:ime,jms:jme,kms:kme)! CV in EOF space.
 
   integer               :: i, j, m, k                 ! Loop counters.
   real                  :: temp

   ! Stop compiler whinge
   if (ide==0 .OR. ids==0) then
   end if

   if (trace_use) call da_trace_entry("da_transform_vvtovp_adj")

   !-------------------------------------------------------------------
   ! [1.0] Apply inner-product weighting if vertical_ip > 0:
   !------------------------------------------------------------------- 

   if ( vertical_ip /= 0 ) then
      vp(its:ite,jts:jte,kts:kte) = vp(its:ite,jts:jte,kts:kte) / &
                                    vertical_wgt(its:ite,jts:jte,kts:kte)
   end if

   !-------------------------------------------------------------------
   ! [2.0] Perform vp(i,j,k) = E L^{1/2} vv(i,j,m) transform:
   !------------------------------------------------------------------- 

   vv = 0.0
   do m = 1, mz
      do k = kts, kte
         do j = jts, jte
            temp = evec(j,k,m) * eval(j,m)

            do i = its, ite
               vv(i,j,m) = vv(i,j,m) + temp*vp(i,j,k)
            end do
         end do
      end do
   end do

  if (trace_use) call da_trace_exit("da_transform_vvtovp_adj")

end subroutine da_transform_vvtovp_adj

