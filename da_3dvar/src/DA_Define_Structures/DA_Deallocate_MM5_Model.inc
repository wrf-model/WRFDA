SUBROUTINE DA_Deallocate_MM5_Model( x_mm5 )

!------------------------------------------------------------------------------
! PURPOSE: Deallocate arrays of MM5 mode structure.
!
! METHOD:  Deallocate arrays in turn.
!
! HISTORY: 01/06/2000 - Creation of F90 version.           Dale Barker
!
! PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (mm5_model_type), intent(inout) :: x_mm5

!-End Of Header----------------------------------------------------------------

!------------------------------------------------------------------------------
!  [1.0] Deallocate components of MM5 fields in structure:
!------------------------------------------------------------------------------

   DEALLOCATE ( x_mm5 % u )
   DEALLOCATE ( x_mm5 % v )
   DEALLOCATE ( x_mm5 % t )
   DEALLOCATE ( x_mm5 % q )
   DEALLOCATE ( x_mm5 % qcw )
   DEALLOCATE ( x_mm5 % qrn )
   DEALLOCATE ( x_mm5 % qci )
   DEALLOCATE ( x_mm5 % qsn )
   DEALLOCATE ( x_mm5 % qgr )
   DEALLOCATE ( x_mm5 % pp_c )
   DEALLOCATE ( x_mm5 % w )

   DEALLOCATE ( x_mm5 % chi )
   DEALLOCATE ( x_mm5 % psi )
   DEALLOCATE ( x_mm5 % blnc_p )
   DEALLOCATE ( x_mm5 % rh )

   DEALLOCATE ( x_mm5 % psac )
   DEALLOCATE ( x_mm5 % psad )
   DEALLOCATE ( x_mm5 % cori )
   DEALLOCATE ( x_mm5 % msfc )
   DEALLOCATE ( x_mm5 % msfd )
   DEALLOCATE ( x_mm5 % latc )
   DEALLOCATE ( x_mm5 % lonc )
   DEALLOCATE ( x_mm5 % latd )
   DEALLOCATE ( x_mm5 % lond )
   
   DEALLOCATE ( x_mm5 % tgrn )
   DEALLOCATE ( x_mm5 % terr )
   DEALLOCATE ( x_mm5 % lanu )
   DEALLOCATE ( x_mm5 % snow )
   DEALLOCATE ( x_mm5 % sigmah )
   DEALLOCATE ( x_mm5 % landmask )

   if(x_mm5 % big_header % bhi(26, 10) == 2) then
      DEALLOCATE ( x_mm5 %      t_eval_glo )
      DEALLOCATE ( x_mm5 %    chi_eval_glo )
      DEALLOCATE ( x_mm5 %    psi_eval_glo )
      DEALLOCATE ( x_mm5 % blnc_p_eval_glo )

      DEALLOCATE ( x_mm5 %      t_evec_glo )
      DEALLOCATE ( x_mm5 %    chi_evec_glo )
      DEALLOCATE ( x_mm5 %    psi_evec_glo )
      DEALLOCATE ( x_mm5 % blnc_p_evec_glo )

      DEALLOCATE ( x_mm5 %      t_eval_loc )
      DEALLOCATE ( x_mm5 %    chi_eval_loc )
      DEALLOCATE ( x_mm5 %    psi_eval_loc )
      DEALLOCATE ( x_mm5 % blnc_p_eval_loc )
 
      DEALLOCATE ( x_mm5 %      t_evec_loc )
      DEALLOCATE ( x_mm5 %    chi_evec_loc )
      DEALLOCATE ( x_mm5 %    psi_evec_loc )
      DEALLOCATE ( x_mm5 % blnc_p_evec_loc )

      IF ( cv_options_hum == 1 .or. cv_options_hum == 3 ) THEN
         DEALLOCATE ( x_mm5 % q_eval_glo )
         DEALLOCATE ( x_mm5 % q_evec_glo )
         DEALLOCATE ( x_mm5 % q_eval_loc )
         DEALLOCATE ( x_mm5 % q_evec_loc )
      ELSE IF ( cv_options_hum == 2 ) THEN
         DEALLOCATE ( x_mm5 % rh_eval_glo )
         DEALLOCATE ( x_mm5 % rh_evec_glo )
         DEALLOCATE ( x_mm5 % rh_eval_loc )
         DEALLOCATE ( x_mm5 % rh_evec_loc )
      END IF
      
      DEALLOCATE ( x_mm5 % pb_vert_reg )

      IF (x_mm5 % big_header % bhi(30, 10) == 1) THEN
         DEALLOCATE ( x_mm5 %    chi_sl )
         DEALLOCATE ( x_mm5 %    psi_sl )
         DEALLOCATE ( x_mm5 % blnc_p_sl )
         DEALLOCATE ( x_mm5 %      t_sl )
         DEALLOCATE ( x_mm5 %      q_sl )
         DEALLOCATE ( x_mm5 %     rh_sl )
      END IF
      
   end if

END SUBROUTINE DA_Deallocate_MM5_Model
