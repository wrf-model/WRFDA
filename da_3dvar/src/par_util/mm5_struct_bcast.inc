
SUBROUTINE mm5_struct_bcast( x_mm5 )

!------------------------------------------------------------------------------
!  PURPOSE: Broadcast (the necessary parts of) the MM5 model structure 
!           from monitor processor to all other processors. 
!------------------------------------------------------------------------------

    TYPE (mm5_model_type), INTENT(INOUT)   :: x_mm5  ! MM5 model structure.

    integer                      :: dims(3)  ! Domain dimensions.
    integer                      :: size1d   ! Number of elements in 1D arrays. 
    integer                      :: size2d   ! Number of elements in 2D arrays. 
    integer                      :: size3d   ! Number of elements in 3D arrays.
    integer                      :: size     ! Number of elements, misc.

    LOGICAL, EXTERNAL :: wrf_dm_on_monitor

!  write(6,'(/A)')'Performing mm5_struct_bcast operation ...'

   call wrf_dm_bcast_integer ( x_mm5 % big_header % bhi, 50*20 )
   call wrf_dm_bcast_real ( x_mm5 % big_header % bhr, 20*20 )
   call wrf_dm_bcast_bytes ( x_mm5 % big_header % bhic(23,1)(1:4), 4 )
   call wrf_dm_bcast_string ( x_mm5 % sub_header(1) % current_date, 24 )

   IF ( .not. wrf_dm_on_monitor() ) THEN
     call DA_Allocate_MM5_Model( x_mm5 )
   END IF

   dims(1) = x_mm5 % big_header % bhi(16, 1) 
   dims(2) = x_mm5 % big_header % bhi(17, 1) 
   dims(3) = x_mm5 % big_header % bhi(12, 5) 

   size1d = dims(3)
   size2d = dims(1) * dims(2) 
   size3d = size2d * dims(3) 

!3D arrays
   call wrf_dm_bcast_real( x_mm5%u, size3d )
   call wrf_dm_bcast_real( x_mm5%v, size3d )
   call wrf_dm_bcast_real( x_mm5%t, size3d )
   call wrf_dm_bcast_real( x_mm5%q, size3d )
   call wrf_dm_bcast_real( x_mm5%pp_c, size3d )
   call wrf_dm_bcast_real( x_mm5%w, size2d * ( dims(3) +1 ) )
   call wrf_dm_bcast_real( x_mm5%psi, size3d )
   call wrf_dm_bcast_real( x_mm5%chi, size3d )
   call wrf_dm_bcast_real( x_mm5%blnc_p, size3d )
   call wrf_dm_bcast_real( x_mm5%rh, size3d )

   call wrf_dm_bcast_real( x_mm5%qcw, size3d )
   call wrf_dm_bcast_real( x_mm5%qrn, size3d )
   call wrf_dm_bcast_real( x_mm5%qci, size3d )
   call wrf_dm_bcast_real( x_mm5%qsn, size3d )
   call wrf_dm_bcast_real( x_mm5%qgr, size3d )

!2D arrays
   call wrf_dm_bcast_real( x_mm5%psac, size2d )
   call wrf_dm_bcast_real( x_mm5%psad, size2d )
   call wrf_dm_bcast_real( x_mm5%cori, size2d )
   call wrf_dm_bcast_real( x_mm5%msfc, size2d )
   call wrf_dm_bcast_real( x_mm5%msfd, size2d )
   call wrf_dm_bcast_real( x_mm5%latc, size2d )
   call wrf_dm_bcast_real( x_mm5%lonc, size2d )
   call wrf_dm_bcast_real( x_mm5%latd, size2d )
   call wrf_dm_bcast_real( x_mm5%lond, size2d )
   call wrf_dm_bcast_real( x_mm5%tgrn, size2d )
   call wrf_dm_bcast_real( x_mm5%terr, size2d )
   call wrf_dm_bcast_real( x_mm5%lanu, size2d )
   call wrf_dm_bcast_real( x_mm5%snow, size2d )
   call wrf_dm_bcast_real( x_mm5%landmask, size2d )

!1D arrays
   call wrf_dm_bcast_real( x_mm5%sigmah, size1d )

   if(x_mm5 % big_header % bhi(26, 10) == 2) then
     size = dims(3)
     call wrf_dm_bcast_real( x_mm5%t_eval_glo, size )
     call wrf_dm_bcast_real( x_mm5%chi_eval_glo, size )
     call wrf_dm_bcast_real( x_mm5%psi_eval_glo, size )
     call wrf_dm_bcast_real( x_mm5%blnc_p_eval_glo, size )
     size = dims(3) * dims(3)
     call wrf_dm_bcast_real( x_mm5%t_evec_glo, size )
     call wrf_dm_bcast_real( x_mm5%chi_evec_glo, size )
     call wrf_dm_bcast_real( x_mm5%psi_evec_glo, size )
     call wrf_dm_bcast_real( x_mm5%blnc_p_evec_glo, size )
     size = dims(1) * dims(3)
     call wrf_dm_bcast_real( x_mm5%t_eval_loc, size )
     call wrf_dm_bcast_real( x_mm5%chi_eval_loc, size )
     call wrf_dm_bcast_real( x_mm5%psi_eval_loc, size )
     call wrf_dm_bcast_real( x_mm5%blnc_p_eval_loc, size )
     size = dims(1) * dims(3) * dims(3)
     call wrf_dm_bcast_real( x_mm5%t_evec_loc, size )
     call wrf_dm_bcast_real( x_mm5%chi_evec_loc, size )
     call wrf_dm_bcast_real( x_mm5%psi_evec_loc, size )
     call wrf_dm_bcast_real( x_mm5%blnc_p_evec_loc, size )

     IF ( cv_options_hum == 1 .or. cv_options_hum == 3 ) THEN
       size = dims(3)
       call wrf_dm_bcast_real( x_mm5%q_eval_glo, size )
       size = dims(3)*dims(3)
       call wrf_dm_bcast_real( x_mm5%q_evec_glo, size )
       size = dims(1)*dims(3)
       call wrf_dm_bcast_real( x_mm5%q_eval_loc, size )
       size = dims(1)*dims(3)*dims(3)
       call wrf_dm_bcast_real( x_mm5%q_evec_loc, size )
     ELSE IF ( cv_options_hum == 2 ) THEN
       size = dims(3)
       call wrf_dm_bcast_real( x_mm5%rh_eval_glo, size )
       size = dims(3)*dims(3)
       call wrf_dm_bcast_real( x_mm5%rh_evec_glo, size )
       size = dims(1)*dims(3)
       call wrf_dm_bcast_real( x_mm5%rh_eval_loc, size )
       size = dims(1)*dims(3)*dims(3)
       call wrf_dm_bcast_real( x_mm5%rh_evec_loc, size )
     END IF

     size = dims(1) * dims(3) * dims(3)
     call wrf_dm_bcast_real( x_mm5%pb_vert_reg, size )

   end if

   IF ( x_mm5 % big_header % bhi(30, 10) == 1) THEN
     call wrf_dm_bcast_real( x_mm5%chi_sl, dims(3) )
     call wrf_dm_bcast_real( x_mm5%psi_sl, dims(3) )
     call wrf_dm_bcast_real( x_mm5%blnc_p_sl, dims(3) )
     call wrf_dm_bcast_real( x_mm5%t_sl, dims(3) )
     call wrf_dm_bcast_real( x_mm5%q_sl, dims(3) )
     call wrf_dm_bcast_real( x_mm5%rh_sl, dims(3) )
   END IF

   RETURN
END SUBROUTINE mm5_struct_bcast

