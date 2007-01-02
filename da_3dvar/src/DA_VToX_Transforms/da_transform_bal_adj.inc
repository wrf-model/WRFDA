SUBROUTINE da_transform_bal_adj( xb, vp, xa, be ,xp,&
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)           :: xb ! First guess structure.
   TYPE (vp_type), INTENT(INOUT)        :: vp ! CV on grid structure.out
   TYPE (x_type), INTENT(INOUT)         :: xa ! Analysis increments.in
   TYPE (be_type), INTENT(IN)           :: be ! Background errors.
   type (xpose_type), intent(inout) :: xp   ! Dimensions and xpose buffers.
   integer, intent(in)  :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)  :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)  :: its,ite, jts,jte, kts,kte ! tile   dims.

   INTEGER                              :: i, j, k, kk, iunit  ! Loop counters.
!-------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------- 
  
!  linear balance btw psi and t-b, Psfc_b and chi_b 


!  [3.4]  adj of Transform psi and chi to u and v:        

   CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)

   vp%v1(:,:,:)=0.
   vp%v2(:,:,:)=0.

   CALL DA_PsiChi_To_UV_Adj( xa % u, xa % v, xb % coefx,   &
                            xb % coefy,  vp % v1, vp % v2, &
                            ids,ide, jds,jde, kds,kde,     &
                            ims,ime, jms,jme, kms,kme,     &
                            its,ite, jts,jte, kts,kte )

!  [3.3]  adj Calculate Psfc_b from psi

!--convert from delt.ps to delt.ln(ps)
   xa%psfc(its:ite,jts:jte) = xa%psfc(its:ite,jts:jte) &
                            * xb%psfc(its:ite,jts:jte)

    DO kk = kts,kte
    DO j = jts,jte
    DO i= its,ite
       vp % v1(i,j,kk)= vp % v1(i,j,kk)+ &
                        be % wgvz(i,j,kk) * xa % psfc(i,j)
    END DO
    END DO
    END DO

   vp % v5(its:ite,jts:jte,1)=xa % psfc(its:ite,jts:jte)

!  [3.2] adj Calculate chi_b from psi

    DO k = kts,kte
     DO j = jts,jte
      DO i= its,ite
         vp % v1(i,j,k)= vp % v1(i,j,k)+ &
                         be % bvz(i,j,k) * vp % v2(i,j,k) 
      END DO
     END DO
    END DO

!  [3.1] Calculate t_b from psi

   DO kk = kts,kte
    DO k = kts,kte
     DO j = jts,jte
      DO i= its,ite
         vp % v1(i,j,kk)= vp % v1(i,j,kk)+ &
                          be % agvz(i,j,k,kk) * xa % t(i,j,k)     
      END DO
     END DO
    END DO
   END DO

   vp % v3(its:ite,jts:jte,kts:kte)= xa % t(its:ite,jts:jte,kts:kte)

!  [3.5] treat humidity                         

   IF ( cv_options == 3 ) THEN
      vp % v4(its:ite,jts:jte,kts:kte) = xa % q(its:ite,jts:jte,kts:kte) &
                                       * xb % qs(its:ite,jts:jte,kts:kte)

   ELSE IF ( cv_options_hum == 1 ) THEN

      vp % v4(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte)+&
                                         xa % q(its:ite,jts:jte,kts:kte)

   ELSE IF ( cv_options_hum == 2 ) THEN

      CALL DA_TPRH_To_Q_Adj( xb, xp, xa )

      vp % v4(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte)+&
                                         xa % rh(its:ite,jts:jte,kts:kte)

   END IF

END SUBROUTINE da_transform_bal_adj   

