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

      IF ( be % v5 % mz > 0 ) THEN
        if( cv_options == 4 ) then
         vp % v5(its:ite,jts:jte,1) = vv % v5(its:ite,jts:jte,1)
        else
           if( cv_options == 5 ) then
              k_end = kts
           else
              k_end = kte
           endif

         CALL DA_Transform_VvToVp( &
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
                                   be % v5 % mz, &
                                   ids,ide, jds,jde, kds,kde,  &
                                   ims,ime, jms,jme, kms,kme,  &
                                   its,ite, jts,jte, kts,k_end )
        endif

      ELSE
         vp % v5(its:ite,jts:jte,kts:kte) = 0.0
      END IF

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

      IF ( be % v5 % mz > 0 ) THEN
        if( cv_options == 4 ) then
         vv % v5(its:ite,jts:jte,1) = vp % v5(its:ite,jts:jte,1)
        else
           if( cv_options == 5 ) then
              k_end = kts
           else
              k_end = kte
           endif
         CALL DA_Transform_VpToVv( &
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
                                   be % v5 % mz, &
                                   ids,ide, jds,jde, kds,kde,  &
                                   ims,ime, jms,jme, kms,kme,  &
                                   its,ite, jts,jte, kts,k_end )
        endif
      END IF

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

      IF ( be % v5 % mz > 0 ) THEN
        if( cv_options == 4 ) then
         vv % v5(its:ite,jts:jte,1) = vp % v5(its:ite,jts:jte,1)
        else
           if( cv_options == 5 ) then
              k_end = kts
           else
              k_end = kte
           endif
         CALL DA_Transform_VvToVp_Adj( &
#ifndef DEREF_KLUDGE
                                       be % v5 % evec, &
                                       be % v5 % val, &
                                       vertical_wgt, &
                                       vp % v5, &
                                       vv % v5, be % v5 % mz, &
#else
                                       be % v5 % evec(jds,kds,1), &
                                       be % v5 % val(jds,1), &
                                       vertical_wgt(ims,jms,kms), &
                                       vp % v5(ims,jms,kms), &
                                       vv % v5(ims,jms,1), be % v5 % mz, &
#endif
                                       ids,ide, jds,jde, kds,kde,  &
                                       ims,ime, jms,jme, kms,kme,  &
                                       its,ite, jts,jte, kts,k_end )
        endif 
      END IF

   case default;
   
      write(0,'(A,A)') "Invalid DA_Vertical_Transform option ", TRIM(string)

      CALL wrf_shutdown

      STOP 'Stopped, Invalid DA_Vertical_Transform option.'

   end select

END subroutine DA_Vertical_Transform

