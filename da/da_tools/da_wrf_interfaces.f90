module da_wrf_interfaces

   interface
      subroutine set_scalar_indices_from_config (idomain, dummy2, dummy1)
        integer, intent(in) :: idomain
        integer, intent(in) :: dummy1
        integer, intent(in) :: dummy2
      end subroutine set_scalar_indices_from_config
   end interface

   interface
      subroutine init_modules(phase)
         integer, intent(in) :: phase
      end subroutine init_modules
   end interface

   interface
      subroutine init_wrfio
      end subroutine init_wrfio
   end interface

   interface
      subroutine wrf_get_dm_communicator (communicator)
         integer , intent(out) :: communicator
      end subroutine wrf_get_dm_communicator
   end interface

   interface
      subroutine wrf_debug(level , str) 
         character*(*), intent(in) :: str
         integer,          intent(in) :: level 
      end subroutine wrf_debug
   end interface

   interface
      subroutine wrf_dm_bcast_integer(buf, n1)
         implicit none
         integer, intent(in)    ::  n1
         integer, intent(inout) :: buf(:)
      end subroutine wrf_dm_bcast_integer
   end interface

   interface 
      subroutine setup_timekeeping(grid)
        use module_domain
        type(domain), pointer :: grid
      end subroutine setup_timekeeping
   end interface

   interface
      subroutine wrf_message(str)
         character(len=*), intent(in) :: str
      end subroutine wrf_message
   end interface

   interface
      subroutine wrf_error_fatal (str)
         character*(*), intent(in) :: str
      end subroutine wrf_error_fatal
   end interface

   interface
      subroutine wrf_error_fatal3 (file_str, line, str)
        character(len=*), intent(in) :: file_str
        integer,          intent(in) :: line
        character(len=*), intent(in) :: str(:)
      end subroutine wrf_error_fatal3
   end interface

   interface
      subroutine wrf_check_error(expected, actual, str, file_str, line)
        integer,          intent(in) :: expected
        integer,          intent(in) :: actual
        character(len=*), intent(in) :: str(:)
        character(len=*), intent(in) :: file_str(:)
        integer,          intent(in) :: line
      end subroutine wrf_check_error
   end interface 

   interface
      subroutine wrf_abort
      end subroutine wrf_abort
   end interface 

   interface 
      subroutine wrf_shutdown
      end subroutine wrf_shutdown
   end interface

   interface 
      subroutine med_shutdown_io (grid , config_flags)
         use module_domain
         use module_io_domain
         use module_configure
         type (domain),               intent(in) :: grid
         type (grid_config_rec_type), intent(in) :: config_flags
      end subroutine med_shutdown_io
   end interface

end module da_wrf_interfaces
