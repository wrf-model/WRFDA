subroutine da_vertical_transform(string, be, vertical_wgt, vv, vp)

   !---------------------------------------------------------------------
   ! Purpose: TBD
   !---------------------------------------------------------------------

   implicit none   

   character (len=*), intent(in) :: string      ! Character operation
   type (be_type), intent(in)    :: be          ! Background error structure.
   real, intent(in)      :: vertical_wgt(ims:ime,jms:jme,kms:kme) ! Weighting.
   type (vp_type), intent(inout) :: vv          ! CV in gridpt/EOF space.
   type (vp_type), intent(inout) :: vp          ! CV in gridpt/level space.

   integer                       :: j, m        ! Loop counters.
   real                          :: alpha_stddev_inv ! 1/ sigma_alpha

   if (trace_use) call da_trace_entry("da_vertical_transform")

   select case(string)
      
   case ('u');
      
      !-------------------------------------------------------------------
      ! [1.0] Perform vp(i,j,k) = E L^{1/2} vv(i,j,m) transform:
      !------------------------------------------------------------------- 

      if (be % v1 % mz > 0) then
         call da_transform_vvtovp(&
#ifndef DEREF_KLUDGE
                                   be % v1 % evec, &
                                   be % v1 % val, &
                                   vertical_wgt, &
                                   vv % v1, vp % v1, &
#else
                                   be % v1 % evec(jds,kds,1), &
                                   be % v1 % val(jds,1), &
                                   vertical_wgt(ims,jms,kms), &
                                   vv % v1(ims,jms,1), vp % v1(ims,jms,kms), &
#endif
                                   be % v1 % mz)
      else
         vp % v1(its:ite,jts:jte,kts:kte) = 0.0
      end if

      if (be % v2 % mz > 0) then
         call da_transform_vvtovp(&
#ifndef DEREF_KLUDGE
                                   be % v2 % evec, &
                                   be % v2 % val, &
                                   vertical_wgt, &
                                   vv % v2, vp % v2, &
#else
                                   be % v2 % evec(jds,kds,1), &
                                   be % v2 % val(jds,1), &
                                   vertical_wgt(ims,jms,kms), &
                                   vv % v2(ims,jms,1), vp % v2(ims,jms,kms), &
#endif
                                   be % v2 % mz)
      else
         vp % v2(its:ite,jts:jte,kts:kte) = 0.0
      end if

      if (be % v3 % mz > 0) then
         call da_transform_vvtovp(& 
#ifndef DEREF_KLUDGE
                                   be % v3 % evec, &
                                   be % v3 % val, &
                                   vertical_wgt, &
                                   vv % v3, vp % v3, &
#else
                                   be % v3 % evec(jds,kds,1), &
                                   be % v3 % val(jds,1), &
                                   vertical_wgt(ims,jms,kms), &
                                   vv % v3(ims,jms,1), vp % v3(ims,jms,kms), &
#endif
                                   be % v3 % mz)
      else
         vp % v3(its:ite,jts:jte,kts:kte) = 0.0
      end if

      if (be % v4 % mz > 0) then
         call da_transform_vvtovp(&
#ifndef DEREF_KLUDGE
                                   be % v4 % evec, &
                                   be % v4 % val, &
                                   vertical_wgt, &
                                   vv % v4, vp % v4, &
#else
                                   be % v4 % evec(jds,kds,1), &
                                   be % v4 % val(jds,1), &
                                   vertical_wgt(ims,jms,kms), &
                                   vv % v4(ims,jms,1), vp % v4(ims,jms,kms), &
#endif
                                   be % v4 % mz)
      else
         vp % v4(its:ite,jts:jte,kts:kte) = 0.0
      end if

      if (be % v5 % mz > 0) then
         if (global) then
            vp % v5(its:ite,jts:jte,1) = vv % v5(its:ite,jts:jte,1)
         else 
            call da_transform_vvtovp(&
#ifndef DEREF_KLUDGE
                                   be % v5 % evec, &
                                   be % v5 % val, &
                                   vertical_wgt, & 
                                   vv % v5, vp % v5, &
#else
                                   be % v5 % evec(jds,kds,1), &
                                   be % v5 % val(jds,1), &
                                   vertical_wgt(ims,jms,kms), &
                                   vv % v5(ims,jms,1), vp % v5(ims,jms,kms), &
#endif
                                   be % v5 % mz)    
         end if
      else
         vp % v5(its:ite,jts:jte,kts:kte) = 0.0
      end if

      if (be % ne > 0) then
         do m = 1, be % ne
            do j = jts, jte
               vp % alpha(its:ite,j,m) = vv % alpha(its:ite,j,m) * be % alpha % val(j,m)
            end do
         end do
      end if

   case ('u_inv');
     
      !------------------------------------------------------------------- 
      ! [2.0] Perform vv(i,j,m) = L^{-1/2} E^T vp(i,j,k) transform:
      !------------------------------------------------------------------- 

      if (be % v1 % mz > 0) then
         call da_transform_vptovv(&
#ifndef DEREF_KLUDGE
                                   be % v1 % evec, &
                                   be % v1 % val, &
                                   vertical_wgt, &
                                   vp % v1, vv % v1, &
#else
                                   be % v1 % evec(jds,kds,1), &
                                   be % v1 % val(jds,1), &
                                   vertical_wgt(ims,jms,1), &
                                   vp % v1(ims,jms,1), vv % v1(ims,jms,1), &
#endif
                                   be % v1 % mz)
      end if

      if (be % v2 % mz > 0) then
         call da_transform_vptovv(&
#ifndef DEREF_KLUDGE
                                   be % v2 % evec, &
                                   be % v2 % val, &
                                   vertical_wgt, &
                                   vp % v2, vv % v2, &
#else
                                   be % v2 % evec(jds,kds,1), &
                                   be % v2 % val(jds,1), &
                                   vertical_wgt(ims,jms,1), &
                                   vp % v2(ims,jms,1), vv % v2(ims,jms,1), &
#endif
                                   be % v2 % mz)
      end if

      if (be % v3 % mz > 0) then
         call da_transform_vptovv(&
#ifndef DEREF_KLUDGE
                                   be % v3 % evec, &
                                   be % v3 % val, &
                                   vertical_wgt, &
                                   vp % v3, vv % v3, &
#else
                                   be % v3 % evec(jds,kds,1), &
                                   be % v3 % val(jds,1), &
                                   vertical_wgt(ims,jms,1), &
                                   vp % v3(ims,jms,1), vv % v3(ims,jms,1), &
#endif
                                   be % v3 % mz)
      end if

      if (be % v4 % mz > 0) then
         call da_transform_vptovv(&
#ifndef DEREF_KLUDGE
                                   be % v4 % evec, &
                                   be % v4 % val, &
                                   vertical_wgt, &
                                   vp % v4, vv % v4, &
#else
                                   be % v4 % evec(jds,kds,1), &
                                   be % v4 % val(jds,1), &
                                   vertical_wgt(ims,jms,1), &
                                   vp % v4(ims,jms,1), vv % v4(ims,jms,1), &
#endif
                                   be % v4 % mz)
      end if

      if (be % v5 % mz > 0) then
        if(global) then
         vv % v5(its:ite,jts:jte,1) = vp % v5(its:ite,jts:jte,1)
        else
         call da_transform_vptovv(&
#ifndef DEREF_KLUDGE
                                   be % v5 % evec, &
                                   be % v5 % val, &
                                   vertical_wgt, &
                                   vp % v5, vv % v5, &
#else
                                   be % v5 % evec(jts,kts,1), &
                                   be % v5 % val(jts,1), &
                                   vertical_wgt(ims,jms,1), &
                                   vp % v5(ims,jms,1), vv % v5(ims,jms,1), &
#endif
                                   be % v5 % mz)
        end if
      end if

      if (be % ne > 0) then
         do m = 1, be % ne
            do j = jts, jte
               alpha_stddev_inv = 1.0 / be % alpha % val(j,m)
               vv % alpha(its:ite,j,m) = vp % alpha(its:ite,j,m) * alpha_stddev_inv
            end do
         end do
      end if

   case ('u_adj');
    
      !------------------------------------------------------------------- 
      ! [3.0] Perform vv_adj = U_{v}^{T} vp_adj transform:
      !------------------------------------------------------------------- 

      if (be % v1 % mz > 0) then
         call da_transform_vvtovp_adj(&
#ifndef DEREF_KLUDGE
                                       be % v1 % evec, &
                                       be % v1 % val, &
                                       vertical_wgt, &
                                       vp % v1, &
                                       vv % v1, be % v1 % mz)
#else
                                       be % v1 % evec(jds,kds,1), &
                                       be % v1 % val(jds,1), &
                                       vertical_wgt(ims,jms,kms), &
                                       vp % v1(ims,jms,kms), &
                                       vv % v1(ims,jms,1), be % v1 % mz)
#endif
      end if

      if (be % v2 % mz > 0) then
         call da_transform_vvtovp_adj(&
#ifndef DEREF_KLUDGE
                                       be % v2 % evec, &
                                       be % v2 % val, &
                                       vertical_wgt, &
                                       vp % v2, &
                                       vv % v2, be % v2 % mz)
#else
                                       be % v2 % evec(jds,kds,1), &
                                       be % v2 % val(jds,1), &
                                       vertical_wgt(ims,jms,kms), &
                                       vp % v2(ims,jms,kms), &
                                       vv % v2(ims,jms,1), be % v2 % mz)
#endif
      end if

      if (be % v3 % mz > 0) then
         call da_transform_vvtovp_adj(&
#ifndef DEREF_KLUDGE
                                       be % v3 % evec, &
                                       be % v3 % val, &
                                       vertical_wgt, &
                                       vp % v3, &
                                       vv % v3, be % v3 % mz)
#else
                                       be % v3 % evec(jds,kds,1), &
                                       be % v3 % val(jds,1), &
                                       vertical_wgt(ims,jms,kms), &
                                       vp % v3(ims,jms,kms), &
                                       vv % v3(ims,jms,1), be % v3 % mz)
#endif
      end if

      if (be % v4 % mz > 0) then
         call da_transform_vvtovp_adj(&
#ifndef DEREF_KLUDGE
                                       be % v4 % evec, &
                                       be % v4 % val, &
                                       vertical_wgt, &
                                       vp % v4, &
                                       vv % v4, be % v4 % mz)
#else
                                       be % v4 % evec(jds,kds,1), &
                                       be % v4 % val(jds,1), &
                                       vertical_wgt(ims,jms,kms), &
                                       vp % v4(ims,jms,kms), &
                                       vv % v4(ims,jms,1), be % v4 % mz)
#endif
      end if

      if (be % v5 % mz > 0) then
         if (global) then
            vv % v5(its:ite,jts:jte,1) = vp % v5(its:ite,jts:jte,1)
         else
            call da_transform_vvtovp_adj(&
#ifndef DEREF_KLUDGE
                                       be % v5 % evec, &
                                       be % v5 % val, &
                                       vertical_wgt, &
                                       vp % v5, &
                                       vv % v5, be % v5 % mz)
#else
                                       be % v5 % evec(jds,kds,1), &
                                       be % v5 % val(jds,1), &
                                       vertical_wgt(ims,jms,kms), &
                                       vp % v5(ims,jms,kms), &
                                       vv % v5(ims,jms,1), be % v5 % mz)
#endif
         end if
      end if

      if (be % ne > 0) then
         do m = 1, be % ne
            do j = jts, jte
               vv % alpha(its:ite,j,m) = vp % alpha(its:ite,j,m) * be % alpha % val(j,m)
            end do
         end do
      end if

   case default;
   
      call da_error(__FILE__,__LINE__, &
         (/"Invalid da_vertical_transform option "//trim(string)/))

   end select

   if (trace_use) call da_trace_exit("da_vertical_transform")

end subroutine da_vertical_transform


