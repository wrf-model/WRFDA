SUBROUTINE da_get_innov_vector_ssmi_rv ( it, xb, xp, ob, iv )

   IMPLICIT NONE

   INTEGER, INTENT(IN)           :: it         ! External iteration.
   TYPE (xb_type), INTENT(IN)    :: xb         ! first guess state.
   TYPE (xpose_type), INTENT(IN) :: xp         ! Domain decomposition vars.
   TYPE (y_type),  INTENT(IN)    :: ob         ! Observation structure.
   TYPE (ob_type), INTENT(INOUT) :: iv         ! O-B structure.

   INTEGER                      :: n           ! Loop counter.
   INTEGER                      :: i, j        ! Index dimension.
   REAL                         :: dx, dxm     ! Interpolation weights.
   REAL                         :: dy, dym     ! Interpolation weights.
   REAL                         :: model_tpw   ! Model value tpw   at oblocation.
   REAL                         :: model_speed ! Model value speed at oblocation.
   INTEGER                      :: itpw, itpwf, ispeed, ispeedf                    
   IF ( iv % num_ssmi_retrieval > 0 ) THEN
   itpw = 0 ; itpwf = 0 ; ispeed = 0 ; ispeedf = 0 
   DO n=1, iv % num_ssmi_retrieval

    if(iv%ssmi_retrieval(n)%loc%proc_domain_with_halo) then

!     COMPUTE INNOVATION VECTOR
!     =========================

      !  Obs coordinates on model grid

      ! TPW

      i = iv%ssmi_retrieval(n)%loc%i
      j = iv%ssmi_retrieval(n)%loc%j
      dx = iv%ssmi_retrieval(n)%loc%dx
      dy = iv%ssmi_retrieval(n)%loc%dy
      dxm = iv%ssmi_retrieval(n)%loc%dxm
      dym = iv%ssmi_retrieval(n)%loc%dym


      iv % ssmi_retrieval (n) % tpw % inv  = 0.0
      IF (abs(ob % ssmi_retrieval(n) % tpw - missing_r) > 1.  .and.  &  
          iv % ssmi_retrieval(n) % tpw % qc >= obs_qc_pointer ) THEN
          model_tpw = dym*(dxm*xb%tpw(i,j)   + dx*xb%tpw(i+1,j)) + &
                      dy *(dxm*xb%tpw(i,j+1) + dx*xb%tpw(i+1,j+1))

          iv % ssmi_retrieval (n) % tpw % inv = ob % ssmi_retrieval (n) % tpw &
                                              - model_tpw
      ENDIF
      
      ! SURFACE WIND SPEED

      iv % ssmi_retrieval (n) % speed % inv  = 0.0
      IF (abs(ob % ssmi_retrieval(n) % speed - missing_r) > 1. .and.     &
           iv % ssmi_retrieval(n) % speed % qc >= obs_qc_pointer ) THEN

          model_speed = dym*(dxm*xb%speed (i,j  ) + dx*xb%speed (i+1,j  )) &
                      + dy *(dxm*xb%speed (i,j+1) + dx*xb%speed (i+1,j+1))
          iv % ssmi_retrieval (n) % speed % inv = ob % ssmi_retrieval (n) % speed &
                                                - model_speed
      ENDIF
      
!------------------------------------------------------------------------
!     Perform optional maximum error check:
!------------------------------------------------------------------------

     IF ( check_max_iv )   &
        CALL da_check_max_iv_ssmi_rv(it, iv % ssmi_retrieval (n),&
          itpw,itpwf, ispeed,ispeedf )
    endif

   ENDDO
   write(unit = check_max_iv_unit, FMT ='(A,i5,A)')'For outer iteration ',it,&
      ' Total Rejections for SSMI (TPW and SPEED) follows:'
   WRITE (UNIT = check_max_iv_unit, FMT = '(/,4(2(A,I6),/))') &
   'Number of failed ssmi tpw   observations:  ',itpwf,  ' on ',itpw,   &
   'Number of failed ssmi speed observations:  ',ispeedf,' on ',ispeed

   ENDIF

END SUBROUTINE da_get_innov_vector_ssmi_rv

