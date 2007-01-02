SUBROUTINE DA_Allocate_MM5_Model( x_mm5 )

!------------------------------------------------------------------------------
! PURPOSE: Allocate arrays of MM5 mode structure.
!
! METHOD:  Allocate component in turn.
!
! HISTORY: 01/06/2000 - Creation of F90 version.           Dale Barker
!
! PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (mm5_model_type), intent(inout) :: x_mm5

   INTEGER                              :: iy     ! 1st dimension of arrays.
   INTEGER                              :: jx     ! 2nd dimension of arrays.
   INTEGER                              :: kz     ! 3rd dimension of arrays.
   INTEGER                              :: kp     ! number of pressure level.

!-End Of Header----------------------------------------------------------------

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   iy = x_mm5 % big_header % bhi(16, 1)
   jx = x_mm5 % big_header % bhi(17, 1)
   kz = x_mm5 % big_header % bhi(12, 5)

   kp = x_mm5 % big_header % bhi(12, 3)

   if(kp < 1) kp = x_mm5 % big_header % bhi(12, 2)
   if(kp < 1) kp = 1

!------------------------------------------------------------------------------
!  [2.0] Allocate components of MM5 fields in structure:
!------------------------------------------------------------------------------

   ALLOCATE ( x_mm5 % u (1:iy,1:jx,1:kz) )
   ALLOCATE ( x_mm5 % v (1:iy,1:jx,1:kz) )
   ALLOCATE ( x_mm5 % t (1:iy,1:jx,1:kz) )
   ALLOCATE ( x_mm5 % q (1:iy,1:jx,1:kz) )
   ALLOCATE ( x_mm5 % qcw (1:iy,1:jx,1:kz) )
   ALLOCATE ( x_mm5 % qrn (1:iy,1:jx,1:kz) )
   ALLOCATE ( x_mm5 % qci (1:iy,1:jx,1:kz) )
   ALLOCATE ( x_mm5 % qsn (1:iy,1:jx,1:kz) )
   ALLOCATE ( x_mm5 % qgr (1:iy,1:jx,1:kz) )
   ALLOCATE ( x_mm5 % pp_c (1:iy,1:jx,1:kz) )
   ALLOCATE ( x_mm5 % w (1:iy,1:jx,1:kz+1) )

   ALLOCATE ( x_mm5 % chi (1:iy,1:jx,1:kz) )
   ALLOCATE ( x_mm5 % psi (1:iy,1:jx,1:kz) )
   ALLOCATE ( x_mm5 % blnc_p (1:iy,1:jx,1:kz) )
   ALLOCATE ( x_mm5 % rh (1:iy,1:jx,1:kz) )

   ALLOCATE ( x_mm5 % psac (1:iy,1:jx) )
   ALLOCATE ( x_mm5 % psad (1:iy,1:jx) )
   ALLOCATE ( x_mm5 % cori (1:iy,1:jx) )
   ALLOCATE ( x_mm5 % msfc (1:iy,1:jx) )
   ALLOCATE ( x_mm5 % msfd (1:iy,1:jx) )
   ALLOCATE ( x_mm5 % latc (1:iy,1:jx) )
   ALLOCATE ( x_mm5 % lonc (1:iy,1:jx) )
   ALLOCATE ( x_mm5 % latd (1:iy,1:jx) )
   ALLOCATE ( x_mm5 % lond (1:iy,1:jx) )
   ALLOCATE ( x_mm5 % tgrn (1:iy,1:jx) )
   ALLOCATE ( x_mm5 % terr (1:iy,1:jx) )
   ALLOCATE ( x_mm5 % lanu (1:iy,1:jx) )
   ALLOCATE ( x_mm5 % snow (1:iy,1:jx) )
   ALLOCATE ( x_mm5 % sigmah(1:kz) )
   ALLOCATE ( x_mm5 % landmask(1:iy,1:jx) )

   if(x_mm5 % big_header % bhi(26, 10) == 2) then
      ALLOCATE ( x_mm5 %      t_eval_glo (1:kz) )
      ALLOCATE ( x_mm5 %    chi_eval_glo (1:kz) )
      ALLOCATE ( x_mm5 %    psi_eval_glo (1:kz) )
      ALLOCATE ( x_mm5 % blnc_p_eval_glo (1:kz) )

      ALLOCATE ( x_mm5 %      t_evec_glo (1:kz,1:kz) )
      ALLOCATE ( x_mm5 %    chi_evec_glo (1:kz,1:kz) )
      ALLOCATE ( x_mm5 %    psi_evec_glo (1:kz,1:kz) )
      ALLOCATE ( x_mm5 % blnc_p_evec_glo (1:kz,1:kz) )

      ALLOCATE ( x_mm5 %      t_eval_loc (1:iy,1:kz) )
      ALLOCATE ( x_mm5 %    chi_eval_loc (1:iy,1:kz) )
      ALLOCATE ( x_mm5 %    psi_eval_loc (1:iy,1:kz) )
      ALLOCATE ( x_mm5 % blnc_p_eval_loc (1:iy,1:kz) )

      ALLOCATE ( x_mm5 %      t_evec_loc (1:iy,1:kz,1:kz) )
      ALLOCATE ( x_mm5 %    chi_evec_loc (1:iy,1:kz,1:kz) )
      ALLOCATE ( x_mm5 %    psi_evec_loc (1:iy,1:kz,1:kz) )
      ALLOCATE ( x_mm5 % blnc_p_evec_loc (1:iy,1:kz,1:kz) )
      
      IF ( cv_options_hum == 1 .or. cv_options_hum == 3 ) THEN
         ALLOCATE ( x_mm5 % q_eval_glo (1:kz) )
         ALLOCATE ( x_mm5 % q_evec_glo (1:kz,1:kz) )
         ALLOCATE ( x_mm5 % q_eval_loc (1:iy,1:kz) )
         ALLOCATE ( x_mm5 % q_evec_loc (1:iy,1:kz,1:kz) )
      ELSE IF ( cv_options_hum == 2 ) THEN
         ALLOCATE ( x_mm5 % rh_eval_glo (1:kz) )
         ALLOCATE ( x_mm5 % rh_evec_glo (1:kz,1:kz) )
         ALLOCATE ( x_mm5 % rh_eval_loc (1:iy,1:kz) )
         ALLOCATE ( x_mm5 % rh_evec_loc (1:iy,1:kz,1:kz) )
      END IF

      ALLOCATE ( x_mm5 % pb_vert_reg    (1:iy,1:kz,1:kz) )

!-----Assign pb_vert_reg to 1.0 in case pb_vert_reg has no value.

      x_mm5 % pb_vert_reg = 1.0

   END IF

   IF ( x_mm5 % big_header % bhi(30, 10) == 1) THEN
      ALLOCATE ( x_mm5 %    chi_sl (1:kz) )
      ALLOCATE ( x_mm5 %    psi_sl (1:kz) )
      ALLOCATE ( x_mm5 % blnc_p_sl (1:kz) )
      ALLOCATE ( x_mm5 %      t_sl (1:kz) )
      ALLOCATE ( x_mm5 %      q_sl (1:kz) )
      ALLOCATE ( x_mm5 %     rh_sl (1:kz) )
      
      x_mm5 % psi_sl (1:kz) = 0.0
      x_mm5 % chi_sl (1:kz) = 0.0
      x_mm5 % blnc_p_sl (1:kz) = 0.0
      x_mm5 % t_sl (1:kz) = 0.0
      x_mm5 % q_sl (1:kz) = 0.0
      x_mm5 % rh_sl (1:kz) = 0.0
   END IF

!------------------------------------------------------------------------------
!  [3.0] Initialise to zero:
!------------------------------------------------------------------------------

   x_mm5 % u (:,:,:)  = 0.0
   x_mm5 % v (:,:,:)  = 0.0
   x_mm5 % t (:,:,:)  = 0.0
   x_mm5 % q (:,:,:)  = 0.0
   x_mm5 % pp_c (:,:,:)  = 0.0
   x_mm5 % w (:,:,:)  = 0.0
   x_mm5 % tgrn(:,:) = 0.0
   
END SUBROUTINE DA_Allocate_MM5_Model
