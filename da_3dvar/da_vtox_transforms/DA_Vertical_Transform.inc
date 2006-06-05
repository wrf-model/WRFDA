subroutine DA_Vertical_Transform( string, be, vertical_wgt, vv, vp, &
                                  ids,ide, jds,jde, kds,kde,  &
                                  ims,ime, jms,jme, kms,kme,  &
                                  its,ite, jts,jte, kts,kte )

   IMPLICIT NONE   

   integer, intent(in)           :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)           :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)           :: its,ite, jts,jte, kts,kte ! tile   dims.

   CHARACTER (LEN=*), intent(in) :: string      ! Character operation
   type (be_type), intent(in)    :: be          ! Background error structure.
   real, intent(in)      :: vertical_wgt(ims:ime,jms:jme,kms:kme) ! Weighting.
   type (vp_type), intent(inout) :: vv          ! CV in gridpt/EOF space.
   type (vp_type), intent(inout) :: vp          ! CV in gridpt/level space.

   integer :: k_end

   IF (trace_use) CALL da_trace_entry("da_vertical_transform")


   select case( string )
      
      case ( 'u' );
      
!-------------------------------------------------------------------
!     [1.0] Perform vp(i,j,k) = E L^{1/2} vv(i,j,m) transform:
!------------------------------------------------------------------- 

      IF ( be % v1 % mz > 0 ) THEN
         CALL DA_Transform_VvToVp( &
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
                                   be % v1 % mz, &
                                   ids,ide, jds,jde, kds,kde,  &
                                   ims,ime, jms,jme, kms,kme,  &
                                   its,ite, jts,jte, kts,kte )
      ELSE
         vp % v1(its:ite,jts:jte,kts:kte) = 0.0
      END IF

      IF ( be % v2 % mz > 0 ) THEN
         CALL DA_Transform_VvToVp( &
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
                                   be % v2 % mz, &
                                   ids,ide, jds,jde, kds,kde,  &
                                   ims,ime, jms,jme, kms,kme,  &
                                   its,ite, jts,jte, kts,kte )
      ELSE
         vp % v2(its:ite,jts:jte,kts:kte) = 0.0
      END IF

      IF ( be % v3 % mz > 0 ) THEN
         CALL DA_Transform_VvToVp( & 
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
                                   be % v3 % mz, &
                                   ids,ide, jds,jde, kds,kde,  &
                                   ims,ime, jms,jme, kms,kme,  &
                                   its,ite, jts,jte, kts,kte )
      ELSE
         vp % v3(its:ite,jts:jte,kts:kte) = 0.0
      END IF

      IF ( be % v4 % mz > 0 ) THEN
         CALL DA_Transform_VvToVp( &
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
                                   be % v4 % mz, &
                                   ids,ide, jds,jde, kds,kde,  &
                                   ims,ime, jms,jme, kms,kme,  &
                                   its,ite, jts,jte, kts,kte )
      ELSE
         vp % v4(its:ite,jts:jte,kts:kte) = 0.0
      END IF

      vp % v5(its:ite,jts:jte,kts:kte) = 0.0
      IF ( be % v5 % mz == 1 ) vp % v5(its:ite,jts:jte,kts:kte) = vv % v5(its:ite,jts:jte,kts:kte)

      if ( be % ne > 0 ) then
!        Uv transform is null:
         vp % alpha(its:ite,jts:jte,1:be%ne) = vv % alpha(its:ite,jts:jte,1:be%ne)
      end if

   case ( 'u_inv');
     
!------------------------------------------------------------------- 
!     [2.0] Perform vv(i,j,m) = L^{-1/2} E^T vp(i,j,k) transform:
!------------------------------------------------------------------- 

      IF ( be % v1 % mz > 0 ) THEN
         CALL DA_Transform_VpToVv( &
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
                                   be % v1 % mz, &
                                   ids,ide, jds,jde, kds,kde,  &
                                   ims,ime, jms,jme, kms,kme,  &
                                   its,ite, jts,jte, kts,kte )
      END IF

      IF ( be % v2 % mz > 0 ) THEN
         CALL DA_Transform_VpToVv( &
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
                                   be % v2 % mz, &
                                   ids,ide, jds,jde, kds,kde,  &
                                   ims,ime, jms,jme, kms,kme,  &
                                   its,ite, jts,jte, kts,kte )
      END IF

      IF ( be % v3 % mz > 0 ) THEN
         CALL DA_Transform_VpToVv( &
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
                                   be % v3 % mz, &
                                   ids,ide, jds,jde, kds,kde,  &
                                   ims,ime, jms,jme, kms,kme,  &
                                   its,ite, jts,jte, kts,kte )
      END IF

      IF ( be % v4 % mz > 0 ) THEN
         CALL DA_Transform_VpToVv( &
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
                                   be % v4 % mz, &
                                   ids,ide, jds,jde, kds,kde,  &
                                   ims,ime, jms,jme, kms,kme,  &
                                   its,ite, jts,jte, kts,kte )
      END IF

      vv % v5(its:ite,jts:jte,kts:kte) = 0.0
      IF ( be % v5 % mz == 1 ) vv % v5(its:ite,jts:jte,kts:kte) = vp % v5(its:ite,jts:jte,kts:kte)

      if ( be % ne > 0 ) then
!        Uv transform is null:
         vv % alpha(its:ite,jts:jte,1:be%ne) = vp % alpha(its:ite,jts:jte,1:be%ne)
      end if

   case ('u_adj');
    
!------------------------------------------------------------------- 
!     [3.0] Perform vv_adj = U_{v}^{T} vp_adj transform:
!------------------------------------------------------------------- 

      IF ( be % v1 % mz > 0 ) THEN
         CALL DA_Transform_VvToVp_Adj( &
#ifndef DEREF_KLUDGE
                                       be % v1 % evec, &
                                       be % v1 % val, &
                                       vertical_wgt, &
                                       vp % v1, &
                                       vv % v1, be % v1 % mz, &
#else
                                       be % v1 % evec(jds,kds,1), &
                                       be % v1 % val(jds,1), &
                                       vertical_wgt(ims,jms,kms), &
                                       vp % v1(ims,jms,kms), &
                                       vv % v1(ims,jms,1), be % v1 % mz, &
#endif
                                       ids,ide, jds,jde, kds,kde,  &
                                       ims,ime, jms,jme, kms,kme,  &
                                       its,ite, jts,jte, kts,kte )
      END IF

      IF ( be % v2 % mz > 0 ) THEN
         CALL DA_Transform_VvToVp_Adj( &
#ifndef DEREF_KLUDGE
                                       be % v2 % evec, &
                                       be % v2 % val, &
                                       vertical_wgt, &
                                       vp % v2, &
                                       vv % v2, be % v2 % mz, &
#else
                                       be % v2 % evec(jds,kds,1), &
                                       be % v2 % val(jds,1), &
                                       vertical_wgt(ims,jms,kms), &
                                       vp % v2(ims,jms,kms), &
                                       vv % v2(ims,jms,1), be % v2 % mz, &
#endif
                                       ids,ide, jds,jde, kds,kde,  &
                                       ims,ime, jms,jme, kms,kme,  &
                                       its,ite, jts,jte, kts,kte )
      END IF

      IF ( be % v3 % mz > 0 ) THEN
         CALL DA_Transform_VvToVp_Adj( &
#ifndef DEREF_KLUDGE
                                       be % v3 % evec, &
                                       be % v3 % val, &
                                       vertical_wgt, &
                                       vp % v3, &
                                       vv % v3, be % v3 % mz, &
#else
                                       be % v3 % evec(jds,kds,1), &
                                       be % v3 % val(jds,1), &
                                       vertical_wgt(ims,jms,kms), &
                                       vp % v3(ims,jms,kms), &
                                       vv % v3(ims,jms,1), be % v3 % mz, &
#endif
                                       ids,ide, jds,jde, kds,kde,  &
                                       ims,ime, jms,jme, kms,kme,  &
                                       its,ite, jts,jte, kts,kte )
      END IF

      IF ( be % v4 % mz > 0 ) THEN
         CALL DA_Transform_VvToVp_Adj( &
#ifndef DEREF_KLUDGE
                                       be % v4 % evec, &
                                       be % v4 % val, &
                                       vertical_wgt, &
                                       vp % v4, &
                                       vv % v4, be % v4 % mz, &
#else
                                       be % v4 % evec(jds,kds,1), &
                                       be % v4 % val(jds,1), &
                                       vertical_wgt(ims,jms,kms), &
                                       vp % v4(ims,jms,kms), &
                                       vv % v4(ims,jms,1), be % v4 % mz, &
#endif
                                       ids,ide, jds,jde, kds,kde,  &
                                       ims,ime, jms,jme, kms,kme,  &
                                       its,ite, jts,jte, kts,kte )
      END IF

      vv % v5(its:ite,jts:jte,kts:kte) = 0.0
      if ( be % v5 % mz == 1 ) vv % v5(its:ite,jts:jte,kts:kte) = vp % v5(its:ite,jts:jte,kts:kte)

      if ( be % ne > 0 ) then
!        Uv transform is null:
         vv % alpha(its:ite,jts:jte,1:be%ne) = vp % alpha(its:ite,jts:jte,1:be%ne)
      end if

   case default;
   
      CALL wrf_error_fatal3(__FILE__,__LINE__, &
                            "Invalid DA_Vertical_Transform option "//TRIM(string))

   end select

   IF (trace_use) CALL da_trace_exit("da_vertical_transform")

END subroutine DA_Vertical_Transform

