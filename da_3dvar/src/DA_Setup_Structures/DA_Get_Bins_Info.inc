subroutine DA_Get_Bins_Info(ni, nj, nk, num_bins2d, bin2d, evec_g, eval_g,&
                  evec_loc, eval_loc, max_vert_var, var_scaling, be_sub)
!------------------------------------------------------------------------------
!  PURPOSE: Extracts Eigen vectors/values info from bins
!           and builds up background error structure 
!
!  HISTORY: 30 November 2004                       Syed RH Rizvi
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------
   IMPLICIT NONE

 integer, intent(in)    :: ni, nj, nk       ! W-E, S-N and Vert. Dims 
 integer, intent(in)    :: num_bins2d       ! Number of bins For 2D fields
 integer, intent(in)    :: bin2d(:,:)       ! Bin assigned to each 2D
 real,    intent(in)    :: evec_g(:,:)      ! Global Eig. vectors 
 real,    intent(in)    :: eval_g(:)        ! Global Eig. values  
 real,    intent(in)    :: evec_loc(:,:,:)  ! Local Eig. vectors   
 real,    intent(in)    :: eval_loc(:,:)    ! Local Eig. values  
 real,    intent(in)    :: max_vert_var     ! Vertical variance
 real,    intent(in)    :: var_scaling      ! Variance re-scaling factor
 type (be_subtype), intent(inout) :: be_sub ! Background error structure
!
   real, allocatable    :: e_vec_loc(:,:,:)
   real, allocatable    :: e_val_loc(:,:)
!
  integer               :: i, j, k, b
!------------------------------------------------------------------------------
   allocate ( e_vec_loc(1:nj,1:nk,1:nk) )
   allocate ( e_val_loc(1:nj,1:nk) )
 if( vert_evalue == 1) then              ! Use global eigen vectors
  do k = 1, nk
   e_val_loc(1:nj,k)     = eval_g(k)
    do i =1,nk
     e_vec_loc(1:nj,k,i) = evec_g(k,i)  
    end do  
  end do  
!
 else if(vert_evalue == 2) then          ! Use local  eigen vectors
    do j = 1, nj
      b = bin2d(1,j)         
       do k=1,nk
        e_val_loc(j,k)   = eval_loc(k,b)
        do i = 1,nk
         e_vec_loc(j,k,i) = evec_loc(k,i,b)
        end do
       end do
    end do
 else
  stop 'Wrong choice of vert_evalue'
 end if

    CALL DA_Get_Vertical_Truncation(max_vert_var, eval_g, be_sub)
    CALL DA_Allocate_Background_Errors(nj, nk, eval_g, evec_g, &
    e_val_loc, e_vec_loc, be_sub)
    if( be_sub%mz > 0 ) be_sub%val(:,:) = var_scaling * be_sub%val(:,:)
   deallocate ( e_vec_loc )
   deallocate ( e_val_loc )
 return
end subroutine DA_Get_Bins_Info
