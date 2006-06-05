SUBROUTINE DA_Calculate_GradY_SSMI( oi, re, jo_grad_y )
!-------------------------------------------------------------------------
!  HISTORY: 
!
!      Created: 02/17/2003                   S. R. H. Rizvi
!
!      Purpose: Applies obs inverse on re-vector
!               
!-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: oi          ! Ob Inc. structure.
   TYPE (y_type), INTENT(INOUT) :: re          ! Residual structure.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)

   INTEGER                      :: n
   IF ( oi%num_ssmi_tb .gt. 0 ) THEN

    DO n=1, oi%num_ssmi_tb

     if( oi%ssmi_tb(n)%loc%proc_domain_with_halo ) then
        if(oi%ssmi_tb(n)%tb19v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb19v = 0.0
       jo_grad_y%ssmi_tb(n)%tb19v = - re%ssmi_tb(n)%tb19v / &
                                    ( oi%ssmi_tb(n)%tb19v%error * &
                                      oi%ssmi_tb(n)%tb19v%error )
 
       if(oi%ssmi_tb(n)%tb19h%qc < obs_qc_pointer) re%ssmi_tb(n)%tb19h = 0.0
       jo_grad_y%ssmi_tb(n)%tb19h = - re%ssmi_tb(n)%tb19h / &
                                    ( oi%ssmi_tb(n)%tb19h%error * &
                                      oi%ssmi_tb(n)%tb19h%error )
 
       if(oi%ssmi_tb(n)%tb22v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb22v = 0.0
       jo_grad_y%ssmi_tb(n)%tb22v = - re%ssmi_tb(n)%tb22v / &
                                    ( oi%ssmi_tb(n)%tb22v%error * &
                                      oi%ssmi_tb(n)%tb22v%error )
 
       if(oi%ssmi_tb(n)%tb37v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb37v = 0.0
       jo_grad_y%ssmi_tb(n)%tb37v = - re%ssmi_tb(n)%tb37v / &
                                    ( oi%ssmi_tb(n)%tb37v%error * &
                                      oi%ssmi_tb(n)%tb37v%error )
 
       if(oi%ssmi_tb(n)%tb37h%qc < obs_qc_pointer) re%ssmi_tb(n)%tb37h = 0.0
       jo_grad_y%ssmi_tb(n)%tb37h = - re%ssmi_tb(n)%tb37h / &
                                    ( oi%ssmi_tb(n)%tb37h%error * &
                                      oi%ssmi_tb(n)%tb37h%error )
 
       if(oi%ssmi_tb(n)%tb85v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb85v = 0.0
       jo_grad_y%ssmi_tb(n)%tb85v = - re%ssmi_tb(n)%tb85v / &
                                    ( oi%ssmi_tb(n)%tb85v%error * &
                                      oi%ssmi_tb(n)%tb85v%error )
 
       if(oi%ssmi_tb(n)%tb85h%qc < obs_qc_pointer) re%ssmi_tb(n)%tb85h = 0.0
       jo_grad_y%ssmi_tb(n)%tb85h = - re%ssmi_tb(n)%tb85h / &
                                    ( oi%ssmi_tb(n)%tb85h%error * &
                                      oi%ssmi_tb(n)%tb85h%error )
 
     endif
 
    END DO
   
   ELSE IF ( oi%num_ssmi_retrieval .gt. 0) THEN

    DO n=1, oi%num_ssmi_retrieval

     if( oi%ssmi_retrieval(n)%loc%proc_domain_with_halo ) then

       if(oi%ssmi_retrieval(n)%speed%qc < obs_qc_pointer) re%ssmi_retrieval(n)%speed = 0.0
       jo_grad_y%ssmi_retrieval(n)%Speed = - re%ssmi_retrieval(n)%Speed / &
                                          ( oi%ssmi_retrieval(n)%Speed%error * &
                                            oi%ssmi_retrieval(n)%Speed%error )
                                            
       if(oi%ssmi_retrieval(n)%tpw%qc < obs_qc_pointer) re%ssmi_retrieval(n)%tpw = 0.0
       jo_grad_y%ssmi_retrieval(n)%tpw = -re%ssmi_retrieval(n)%tpw / &
                                        ( oi%ssmi_retrieval(n)%tpw%error * &
                                          oi%ssmi_retrieval(n)%tpw%error )
     endif

    END DO
   
   ENDIF

END SUBROUTINE DA_Calculate_GradY_SSMI
