MODULE module_kma2netcdf_interface

   use module_kma_wave2grid

   implicit none

CONTAINS

SUBROUTINE kma2netcdf_interface ( grid, config_flags ) 

   USE module_domain
   USE module_timing
   USE module_driver_constants
   USE module_configure

   IMPLICIT NONE

!--Input data.

   TYPE(domain) , INTENT(INOUT)  :: grid
   TYPE (grid_config_rec_type)   :: config_flags

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

!   call W2GCONV(grid%ht,grid%psfc,grid%em_u_2,grid%em_v_2,grid%em_t_2,grid%moist_2)
  print*,'in grid sd31, ed31 ',grid%sd31,grid%ed31
  print*,'in grid sd32, ed32 ',grid%sd32,grid%ed32
  print*,'in grid sd33, ed33 ',grid%sd33,grid%ed33
   call W2GCONV(grid%ht(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1),&
                grid%psfc(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1),&
        grid%em_t_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1),&
        grid%em_u_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1),&
        grid%em_v_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1),&
    grid%moist_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1,P_qv) )

!   rizvi
   
!   write(61,fmt='(5e16.8)')grid%ht(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1)
!   write(61,fmt='(5e16.8)')grid%psfc(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1)
!   write(61,fmt='(5e16.8)')&
!        grid%em_t_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1)
!   write(61,fmt='(5e16.8)')&
!        grid%em_u_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1)
!   write(61,fmt='(5e16.8)')&
!        grid%em_v_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1)
!   write(61,fmt='(5e16.8)')&
!        grid%moist_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1,P_qv)
   write(61)grid%ht(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1)
   write(61)grid%psfc(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1)
   write(61)grid%em_t_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1)
   write(61)grid%em_u_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1)
   write(61)grid%em_v_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1)
   write(61)&
       grid%moist_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1,P_qv)

!   rizvi
   
END SUBROUTINE kma2netcdf_interface

END MODULE module_kma2netcdf_interface

