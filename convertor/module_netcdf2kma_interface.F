MODULE module_netcdf2kma_interface

   use module_wave2grid_kma
   implicit none

CONTAINS

SUBROUTINE netcdf2kma_interface ( grid, config_flags ) 

   USE module_domain
   USE module_timing
   USE module_driver_constants
   USE module_configure

   IMPLICIT NONE

!--Input data.

   TYPE(domain) , INTENT(INOUT)  :: grid
   TYPE (grid_config_rec_type)   :: config_flags
! we have to convert in equal lat/lon data 
!           to Gaussian latitude
!
  CALL PREGSM(grid%psfc(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1),&
        grid%em_t_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1),&
        grid%em_u_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1),&
        grid%em_v_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1),&
    grid%moist_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1,P_qv))

END SUBROUTINE netcdf2kma_interface

END MODULE module_netcdf2kma_interface

