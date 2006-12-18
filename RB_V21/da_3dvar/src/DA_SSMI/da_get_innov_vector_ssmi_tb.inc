SUBROUTINE da_get_innov_vector_ssmi_tb ( it, xb, xp, ob, iv )

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
   REAL                         :: model_tb19h ! Model value tb19h at oblocation.
   REAL                         :: model_tb19v ! Model value tb19v at oblocation.
   REAL                         :: model_tb22v ! Model value tb22v at oblocation.
   REAL                         :: model_tb37h ! Model value tb37h at oblocation.
   REAL                         :: model_tb37v ! Model value tb37v at oblocation.
   REAL                         :: model_tb85h ! Model value tb85h at oblocation.
   REAL                         :: model_tb85v ! Model value tb85v at oblocation.

   INTEGER                      :: itb19v, itb19vf, &
                                   itb19h, itb19hf, &
                                   itb22v, itb22vf, &
                                   itb37v, itb37vf, &
                                   itb37h, itb37hf, &
                                   itb85v, itb85vf, &
                                   itb85h, itb85hf

!   print*,'num_ssmi_tb=',iv % num_ssmi_tb
   IF ( iv % num_ssmi_tb > 0 ) THEN

   itb19v = 0
   itb19vf = 0
   itb19h = 0
   itb19hf = 0
   itb22v = 0
   itb22vf = 0
   itb37v = 0
   itb37vf = 0
   itb37h = 0
   itb37hf = 0
   itb85v = 0
   itb85vf = 0
   itb85h = 0
   itb85hf = 0

   DO n=1, iv % num_ssmi_tb

    if(iv%ssmi_tb(n)%loc%proc_domain_with_halo) then

!     COMPUTE INNOVATION VECTOR
!     =========================

      !  Obs coordinates on model grid

      ! TB

      i = iv%ssmi_tb(n)%loc%i
      j = iv%ssmi_tb(n)%loc%j
      dx = iv%ssmi_tb(n)%loc%dx
      dy = iv%ssmi_tb(n)%loc%dy
      dxm = iv%ssmi_tb(n)%loc%dxm
      dym = iv%ssmi_tb(n)%loc%dym
    
! Tb19h

      IF (abs(ob % ssmi_tb(n) % tb19h - missing_r) > 1.) THEN
          model_tb19h = dym*(dxm*xb%tb19h(i,j)   + dx*xb%tb19h(i+1,j)) &
                      + dy *(dxm*xb%tb19h(i,j+1) + dx*xb%tb19h(i+1,j+1))
          iv % ssmi_tb (n) % tb19h % inv = ob % ssmi_tb (n) % tb19h - model_tb19h
      ELSE
          iv % ssmi_tb (n) % tb19h % inv = 0.
      ENDIF

! Tb19v

      IF (abs(ob % ssmi_tb(n) % tb19v - missing_r) > 1.) THEN
          model_tb19v = dym*(dxm*xb%tb19v (i,j)   + dx *xb%tb19v (i+1,j)) &
                      + dy *(dxm*xb%tb19v (i,j+1) + dx *xb%tb19v (i+1,j+1))
          iv % ssmi_tb (n) % tb19v % inv = ob % ssmi_tb (n) % tb19v - model_tb19v
      ELSE
          iv % ssmi_tb (n) % tb19v % inv = 0.
      ENDIF

! Tb19v


      IF (abs(ob % ssmi_tb(n) % tb22v - missing_r) > 1.) THEN
          model_tb22v = dym*(dxm*xb%tb22v (i,j)   + dx *xb%tb22v (i+1,j)) &
                      + dy *(dxm*xb%tb22v (i,j+1) + dx *xb%tb22v (i+1,j+1))
          iv % ssmi_tb (n) % tb22v % inv = ob % ssmi_tb (n) % tb22v - model_tb22v
      ELSE
          iv % ssmi_tb (n) % tb22v % inv = 0.0
      ENDIF

! Tb37h

      IF (abs(ob % ssmi_tb(n) % tb37h - missing_r) > 1.) THEN
          model_tb37h = dym*(dxm*xb%tb37h (i,j)   + dx *xb%tb37h (i+1,j)) &
                      + dy *(dxm*xb%tb37h (i,j+1) + dx *xb%tb37h (i+1,j+1))
          iv % ssmi_tb (n) % tb37h % inv = ob % ssmi_tb (n) % tb37h - model_tb37h
      ELSE
          iv % ssmi_tb (n) % tb37h % inv = 0.
      ENDIF

! Tb37v

      IF (abs(ob % ssmi_tb(n) % tb37v - missing_r) > 1.) THEN
          model_tb37v = dym*(dxm*xb%tb37v (i,j)   + dx *xb%tb37v (i+1,j)) &
                      + dy *(dxm*xb%tb37v (i,j+1) + dx *xb%tb37v (i+1,j+1))
          iv % ssmi_tb (n) % tb37v % inv = ob % ssmi_tb (n) % tb37v - model_tb37v
      ELSE
          iv % ssmi_tb (n) % tb37v % inv = 0.
      ENDIF

! Tb85h

      IF (abs(ob % ssmi_tb(n) % tb85h - missing_r) > 1.) THEN
          model_tb85h = dym*(dxm*xb%tb85h (i,j)   + dx *xb%tb85h (i+1,j)) &
                      + dy *(dxm*xb%tb85h (i,j+1) + dx *xb%tb85h (i+1,j+1))
          iv % ssmi_tb (n) % tb85h % inv = ob % ssmi_tb (n) % tb85h - model_tb85h
      ELSE
          iv % ssmi_tb (n) % tb85h % inv = 0.
      ENDIF

! Tb85v

      IF (abs(ob % ssmi_tb(n) % tb85v - missing_r) > 1.) THEN
          model_tb85v = dym*(dxm*xb%tb85v (i,j)   + dx *xb%tb85v (i+1,j)) &
                      + dy *(dxm*xb%tb85v (i,j+1) + dx *xb%tb85v (i+1,j+1))
          iv % ssmi_tb (n) % tb85v % inv = ob % ssmi_tb (n) % tb85v - model_tb85v
      ELSE
          iv % ssmi_tb (n) % tb85v % inv = 0.
      ENDIF


!------------------------------------------------------------------------
!     Perform optional maximum error check:
!------------------------------------------------------------------------

      IF ( check_max_iv ) CALL da_check_max_iv_ssmi_tb(it, iv % ssmi_tb (n), &
                                   itb19v, itb19vf, itb19h, itb19hf, &
                                   itb22v, itb22vf,                  &
                                   itb37v, itb37vf, itb37h, itb37hf, &
                                   itb85v, itb85vf, itb85h, itb85hf  )

    endif

   ENDDO

   write(unit = check_max_iv_unit, FMT ='(A,i5,A)')'For outer iteration ',it,&
      ' Total Rejections for SSMI (Tbs) follows:'
   WRITE (UNIT = check_max_iv_unit, FMT = '(2(A,I6))') &
   'Number of failed ssmi tb19v   observations:  ',itb19v,  ' on ',itb19v,   &
   'Number of failed ssmi tb19h   observations:  ',itb19h,  ' on ',itb19h,   &
   'Number of failed ssmi tb22v   observations:  ',itb22v,  ' on ',itb22v,   &
   'Number of failed ssmi tb37v   observations:  ',itb37v,  ' on ',itb37v,   &
   'Number of failed ssmi tb37h   observations:  ',itb37h,  ' on ',itb37h,   &
   'Number of failed ssmi tb85v   observations:  ',itb85v,  ' on ',itb85v,   &
   'Number of failed ssmi tb85h   observations:  ',itb85h,  ' on ',itb85h

   ENDIF

END SUBROUTINE da_get_innov_vector_ssmi_tb

