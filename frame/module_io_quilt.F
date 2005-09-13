!WRF:DRIVER_LAYER:IO
!

!#define mpi_x_comm_size(i,j,k)  Mpi_Comm_Size ( i,j,k ) ; write(0,*) 4


MODULE module_wrf_quilt
!<DESCRIPTION>
!<PRE>
! This module contains WRF-specific I/O quilt routines called by both 
! client (compute) and server (I/O quilt) tasks.  I/O quilt servers are 
! a run-time optimization that allow I/O operations, executed on the I/O 
! quilt server tasks, to be overlapped with useful computation, executed on 
! the compute tasks.  Since I/O operations are often quite slow compared to 
! computation, this performance optimization can increase parallel 
! efficiency.  
!
! Currently, one group of I/O servers can be specified at run-time.  Namelist 
! variable "nio_tasks_per_group" is used to specify the number of I/O server 
! tasks in this group.  In most cases, parallel efficiency is optimized when 
! the minimum number of I/O server tasks are used.  If memory needed to cache 
! I/O operations fits on a single processor, then set nio_tasks_per_group=1.  
! If not, increase the number of I/O server tasks until I/O operations fit in 
! memory.  In the future, multiple groups of I/O server tasks will be 
! supported.  The number of groups will be specified by namelist variable 
! "nio_groups".  For now, nio_groups must be set to 1.  Currently, I/O servers 
! only support overlap of output operations with computation.  Also, only I/O 
! packages that do no support native parallel I/O may be used with I/O server 
! tasks.  This excludes PHDF5 and MCEL.  
!
! In this module, the I/O quilt server tasks call package-dependent 
! WRF-specific I/O interfaces to perform I/O operations requested by the 
! client (compute) tasks.  All of these calls occur inside subroutine 
! quilt().  
! 
! The client (compute) tasks call package-independent WRF-specific "quilt I/O" 
! interfaces that send requests to the I/O quilt servers.  All of these calls 
! are made from module_io.F.  
!
! These routines have the same names and (roughly) the same arguments as those 
! specified in the WRF I/O API except that:
! - "Quilt I/O" routines defined in this file and called by routines in 
!   module_io.F have the "wrf_quilt_" prefix.
! - Package-dependent routines called from routines in this file are defined 
!   in the external I/O packages and have the "ext_" prefix.
!
! Both client (compute) and server tasks call routine init_module_wrf_quilt() 
! which then calls setup_quilt_servers() determine which tasks are compute 
! tasks and which are server tasks.  Before the end of init_module_wrf_quilt() 
! server tasks call routine quilt() and remain there for the rest of the model 
! run.  Compute tasks return from init_module_wrf_quilt() to perform model 
! computations.  
!
! See http://www.mmm.ucar.edu/wrf/WG2/software_2.0/IOAPI.doc for the latest
! version of the WRF I/O API.  This document includes detailed descriptions
! of subroutines and their arguments that are not duplicated here.
!</PRE>
!</DESCRIPTION>
  USE module_internal_header_util

  INTEGER, PARAMETER :: int_num_handles = 99
  LOGICAL, DIMENSION(int_num_handles) :: okay_to_write, int_handle_in_use, okay_to_commit
  INTEGER, DIMENSION(int_num_handles) :: int_num_bytes_to_write, io_form
  REAL, POINTER    :: int_local_output_buffer(:)
  INTEGER          :: int_local_output_cursor
  LOGICAL          :: quilting_enabled
  LOGICAL          :: disable_quilt = .FALSE.


  CONTAINS


    SUBROUTINE init_module_wrf_quilt
!<DESCRIPTION>
!<PRE>
! Both client (compute) and server tasks call this routine to initialize the 
! module.  Routine setup_quilt_servers() is called from this routine to 
! determine which tasks are compute tasks and which are server tasks.  Server 
! tasks then call routine quilt() and remain there for the rest of the model 
! run.  Compute tasks return from init_module_wrf_quilt() to perform model 
! computations.  
!</PRE>
!</DESCRIPTION>
      RETURN
    END SUBROUTINE init_module_wrf_quilt
END MODULE module_wrf_quilt

!<DESCRIPTION>
!<PRE>
! Remaining routines in this file are defined outside of the module
! either to defeat arg/param type checking or to avoid an explicit use
! dependence.
!</PRE>
!</DESCRIPTION>

SUBROUTINE disable_quilting
!<DESCRIPTION>
!<PRE>
! Call this in programs that you never want to be quilting (e.g. real)
! Must call before call to init_module_wrf_quilt().  
!</PRE>
!</DESCRIPTION>
  USE module_wrf_quilt
  disable_quilt = .TRUE.
  RETURN
END SUBROUTINE disable_quilting

LOGICAL FUNCTION  use_output_servers()
!<DESCRIPTION>
!<PRE>
! Returns .TRUE. if I/O quilt servers are in-use for write operations.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
  USE module_wrf_quilt
  use_output_servers = quilting_enabled
  RETURN
END FUNCTION use_output_servers

LOGICAL FUNCTION  use_input_servers()
!<DESCRIPTION>
!<PRE>
! Returns .TRUE. if I/O quilt servers are in-use for read operations.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
  USE module_wrf_quilt
  use_input_servers = .FALSE.
  RETURN
END FUNCTION use_input_servers

SUBROUTINE wrf_quilt_open_for_write_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                     DataHandle , io_form_arg, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to begin data definition ("training") phase
! for writing to WRF dataset FileName.  io_form_arg indicates file format.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
  RETURN  
END SUBROUTINE wrf_quilt_open_for_write_begin

SUBROUTINE wrf_quilt_open_for_write_commit( DataHandle , Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to switch an internal flag to enable output
! for the dataset referenced by DataHandle.  The call to
! wrf_quilt_open_for_write_commit() must be paired with a call to
! wrf_quilt_open_for_write_begin().
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
  RETURN  
END SUBROUTINE wrf_quilt_open_for_write_commit

SUBROUTINE wrf_quilt_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               DataHandle , Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to open WRF dataset FileName for reading.
! This routine is called only by client (compute) tasks.  
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
  RETURN  
END SUBROUTINE wrf_quilt_open_for_read

SUBROUTINE wrf_quilt_inquire_opened ( DataHandle, FileName , FileStatus, Status )
!<DESCRIPTION>
!<PRE>
! Inquire if the dataset referenced by DataHandle is open.
! Does not require communication with I/O servers.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
  RETURN
END SUBROUTINE wrf_quilt_inquire_opened

SUBROUTINE wrf_quilt_inquire_filename ( DataHandle, FileName , FileStatus, Status )
!<DESCRIPTION>
!<PRE>
! Return the Filename and FileStatus associated with DataHandle.
! Does not require communication with I/O servers.
!
! Note that the current implementation does not actually return FileName.
! Currenlty, WRF does not use this returned value.  Fixing this would simply
! require saving the file names on the client tasks in an array similar to
! okay_to_write().
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
  RETURN
END SUBROUTINE wrf_quilt_inquire_filename

SUBROUTINE wrf_quilt_iosync ( DataHandle, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to synchronize the disk copy of a dataset
! with memory buffers.
!
! After the "iosync" header (request) is sent to the I/O quilt server,
! the compute tasks will then send the entire contents (headers and data) of
! int_local_output_buffer to their I/O quilt server.  This communication is
! done in subroutine send_to_io_quilt_servers().  After the I/O quilt servers
! receive this data, they will write all accumulated fields to disk.
!
! Significant time may be required for the I/O quilt servers to organize
! fields and write them to disk.  Therefore, the "iosync" request should be
! sent only when the compute tasks are ready to run for a while without
! needing to communicate with the servers.  Otherwise, the compute tasks
! will end up waiting for the servers to finish writing to disk, thus wasting
! any performance benefits of having servers at all.
!
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
  RETURN
END SUBROUTINE wrf_quilt_iosync

SUBROUTINE wrf_quilt_ioclose ( DataHandle, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to close the dataset referenced by
! DataHandle.
! This routine also clears the client file handle and, if needed, deallocates
! int_local_output_buffer.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
  RETURN
END SUBROUTINE wrf_quilt_ioclose

SUBROUTINE wrf_quilt_ioexit( Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to shut down the WRF I/O system.
! Do not call any wrf_quilt_*() routines after this routine has been called.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
  RETURN  
END SUBROUTINE

SUBROUTINE wrf_quilt_get_next_time ( DataHandle, DateStr, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to return the next time stamp.
! This is not yet supported.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>






  RETURN
END SUBROUTINE wrf_quilt_get_next_time

SUBROUTINE wrf_quilt_get_previous_time ( DataHandle, DateStr, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to return the previous time stamp.
! This is not yet supported.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>






  RETURN
END SUBROUTINE wrf_quilt_get_previous_time

SUBROUTINE wrf_quilt_set_time ( DataHandle, Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to set the time stamp in the dataset
! referenced by DataHandle.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_set_time

SUBROUTINE wrf_quilt_get_next_var ( DataHandle, VarName, Status )
!<DESCRIPTION>
!<PRE>
! When reading, instruct the I/O quilt servers to return the name of the next
! variable in the current time frame.
! This is not yet supported.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>






  RETURN
END SUBROUTINE wrf_quilt_get_next_var

SUBROUTINE wrf_quilt_get_dom_ti_real ( DataHandle,Element,   Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent domain metadata named "Element"
! from the open dataset described by DataHandle.
! Metadata of type real are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  

! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_real 

SUBROUTINE wrf_quilt_put_dom_ti_real ( DataHandle,Element,   Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! domain metadata named "Element"
! to the open dataset described by DataHandle.
! Metadata of type real are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_real 

SUBROUTINE wrf_quilt_get_dom_ti_double ( DataHandle,Element,   Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent domain metadata named "Element"
! from the open dataset described by DataHandle.
! Metadata of type double are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_double 

SUBROUTINE wrf_quilt_put_dom_ti_double ( DataHandle,Element,   Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! domain metadata named "Element"
! to the open dataset described by DataHandle.
! Metadata of type double are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_double 

SUBROUTINE wrf_quilt_get_dom_ti_integer ( DataHandle,Element,   Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent domain metadata named "Element"
! from the open dataset described by DataHandle.
! Metadata of type integer are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_integer 

SUBROUTINE wrf_quilt_put_dom_ti_integer ( DataHandle,Element,   Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! domain metadata named "Element"
! to the open dataset described by DataHandle.
! Metadata of type integer are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_integer 

SUBROUTINE wrf_quilt_get_dom_ti_logical ( DataHandle,Element,   Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent domain metadata named "Element"
! from the open dataset described by DataHandle.
! Metadata of type logical are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_logical 

SUBROUTINE wrf_quilt_put_dom_ti_logical ( DataHandle,Element,   Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! domain metadata named "Element"
! to the open dataset described by DataHandle.
! Metadata of type logical are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_logical 

SUBROUTINE wrf_quilt_get_dom_ti_char ( DataHandle,Element,   Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read time independent
! domain metadata named "Element"
! from the open dataset described by DataHandle.
! Metadata of type char are
! stored in string Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_char 

SUBROUTINE wrf_quilt_put_dom_ti_char ( DataHandle, Element,  Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write time independent
! domain metadata named "Element"
! to the open dataset described by DataHandle.
! Metadata of type char are
! copied from string Data.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_char 

SUBROUTINE wrf_quilt_get_dom_td_real ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent domain metadata named "Element" valid at time DateStr
! from the open dataset described by DataHandle.
! Metadata of type real are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_dom_td_real 

SUBROUTINE wrf_quilt_put_dom_td_real ( DataHandle,Element, DateStr,  Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! domain metadata named "Element" valid at time DateStr
! to the open dataset described by DataHandle.
! Metadata of type real are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_dom_td_real 

SUBROUTINE wrf_quilt_get_dom_td_double ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent domain metadata named "Element" valid at time DateStr
! from the open dataset described by DataHandle.
! Metadata of type double are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_dom_td_double 

SUBROUTINE wrf_quilt_put_dom_td_double ( DataHandle,Element, DateStr,  Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! domain metadata named "Element" valid at time DateStr
! to the open dataset described by DataHandle.
! Metadata of type double are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_dom_td_double 

SUBROUTINE wrf_quilt_get_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent domain metadata named "Element" valid at time DateStr
! from the open dataset described by DataHandle.
! Metadata of type integer are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_dom_td_integer 

SUBROUTINE wrf_quilt_put_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! domain metadata named "Element" valid at time DateStr
! to the open dataset described by DataHandle.
! Metadata of type integer are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_dom_td_integer 

SUBROUTINE wrf_quilt_get_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent domain metadata named "Element" valid at time DateStr
! from the open dataset described by DataHandle.
! Metadata of type logical are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_dom_td_logical 

SUBROUTINE wrf_quilt_put_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! domain metadata named "Element" valid at time DateStr
! to the open dataset described by DataHandle.
! Metadata of type logical are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_dom_td_logical 

SUBROUTINE wrf_quilt_get_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read time dependent
! domain metadata named "Element" valid at time DateStr
! from the open dataset described by DataHandle.
! Metadata of type char are
! stored in string Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_dom_td_char 

SUBROUTINE wrf_quilt_put_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct $he I/O quilt servers to write time dependent
! domain metadata named "Element" valid at time DateStr
! to the open dataset described by DataHandle.
! Metadata of type char are
! copied from string Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_dom_td_char 

SUBROUTINE wrf_quilt_get_var_ti_real ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent attribute "Element" of variable "Varname"
! from the open dataset described by DataHandle.
! Attribute of type real is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_var_ti_real 

SUBROUTINE wrf_quilt_put_var_ti_real ( DataHandle,Element,  Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! attribute "Element" of variable "Varname"
! to the open dataset described by DataHandle.
! Attribute of type real is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_var_ti_real 

SUBROUTINE wrf_quilt_get_var_ti_double ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent attribute "Element" of variable "Varname"
! from the open dataset described by DataHandle.
! Attribute of type double is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_var_ti_double 

SUBROUTINE wrf_quilt_put_var_ti_double ( DataHandle,Element,  Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! attribute "Element" of variable "Varname"
! to the open dataset described by DataHandle.
! Attribute of type double is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_var_ti_double 

SUBROUTINE wrf_quilt_get_var_ti_integer ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent attribute "Element" of variable "Varname"
! from the open dataset described by DataHandle.
! Attribute of type integer is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_var_ti_integer 

SUBROUTINE wrf_quilt_put_var_ti_integer ( DataHandle,Element,  Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! attribute "Element" of variable "Varname"
! to the open dataset described by DataHandle.
! Attribute of type integer is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_var_ti_integer 

SUBROUTINE wrf_quilt_get_var_ti_logical ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent attribute "Element" of variable "Varname"
! from the open dataset described by DataHandle.
! Attribute of type logical is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_var_ti_logical 

SUBROUTINE wrf_quilt_put_var_ti_logical ( DataHandle,Element,  Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! attribute "Element" of variable "Varname"
! to the open dataset described by DataHandle.
! Attribute of type logical is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_var_ti_logical 

SUBROUTINE wrf_quilt_get_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read time independent
! attribute "Element" of variable "Varname"
! from the open dataset described by DataHandle.
! Attribute of type char is
! stored in string Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_var_ti_char 

SUBROUTINE wrf_quilt_put_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write time independent
! attribute "Element" of variable "Varname"
! to the open dataset described by DataHandle.
! Attribute of type char is
! copied from string Data.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>

RETURN
END SUBROUTINE wrf_quilt_put_var_ti_char 

SUBROUTINE wrf_quilt_get_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent attribute "Element" of variable "Varname" valid at time DateStr
! from the open dataset described by DataHandle.
! Attribute of type real is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_var_td_real 

SUBROUTINE wrf_quilt_put_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! attribute "Element" of variable "Varname" valid at time DateStr
! to the open dataset described by DataHandle.
! Attribute of type real is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_var_td_real 

SUBROUTINE wrf_quilt_get_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent attribute "Element" of variable "Varname" valid at time DateStr
! from the open dataset described by DataHandle.
! Attribute of type double is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_var_td_double 

SUBROUTINE wrf_quilt_put_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! attribute "Element" of variable "Varname" valid at time DateStr
! to the open dataset described by DataHandle.
! Attribute of type double is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_var_td_double 

SUBROUTINE wrf_quilt_get_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount,Status)
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent attribute "Element" of variable "Varname" valid at time DateStr
! from the open dataset described by DataHandle.
! Attribute of type integer is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_var_td_integer 

SUBROUTINE wrf_quilt_put_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! attribute "Element" of variable "Varname" valid at time DateStr
! to the open dataset described by DataHandle.
! Attribute of type integer is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_var_td_integer 

SUBROUTINE wrf_quilt_get_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent attribute "Element" of variable "Varname" valid at time DateStr
! from the open dataset described by DataHandle.
! Attribute of type logical is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_var_td_logical 

SUBROUTINE wrf_quilt_put_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! attribute "Element" of variable "Varname" valid at time DateStr
! to the open dataset described by DataHandle.
! Attribute of type logical is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_var_td_logical 

SUBROUTINE wrf_quilt_get_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read time dependent
! attribute "Element" of variable "Varname" valid at time DateStr
! from the open dataset described by DataHandle.
! Attribute of type char is
! stored in string Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_var_td_char 

SUBROUTINE wrf_quilt_put_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write time dependent
! attribute "Element" of variable "Varname" valid at time DateStr
! to the open dataset described by DataHandle.
! Attribute of type char is
! copied from string Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_put_var_td_char 

SUBROUTINE wrf_quilt_read_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
                            DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to read the variable named VarName from the
! dataset pointed to by DataHandle.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_read_field

SUBROUTINE wrf_quilt_write_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm,  &
                             DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )
!<DESCRIPTION>
!<PRE>
! Prepare instructions for the I/O quilt servers to write the variable named
! VarName to the dataset pointed to by DataHandle.
!
! During a "training" write this routine accumulates number and sizes of
! messages that will be sent to the I/O server associated with this compute
! (client) task.
!
! During a "real" write, this routine begins by allocating
! int_local_output_buffer if it has not already been allocated.  Sizes
! accumulated during "training" are used to determine how big
! int_local_output_buffer must be.  This routine then stores "int_field"
! headers and associated field data in int_local_output_buffer.  The contents
! of int_local_output_buffer are actually sent to the I/O quilt server in
! routine wrf_quilt_iosync().  This scheme allows output of multiple variables
! to be aggregated into a single "iosync" operation.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
  RETURN
END SUBROUTINE wrf_quilt_write_field

SUBROUTINE wrf_quilt_get_var_info ( DataHandle , VarName , NDim , MemoryOrder , Stagger , &
                              DomainStart , DomainEnd , Status )
!<DESCRIPTION>
!<PRE>
! This routine applies only to a dataset that is open for read.  It instructs
! the I/O quilt servers to return information about variable VarName.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
RETURN
END SUBROUTINE wrf_quilt_get_var_info

SUBROUTINE get_mpi_comm_io_groups( retval, isrvr )
!<DESCRIPTION>
!<PRE>
! This routine returns the compute+io communicator to which this
! compute task belongs for I/O server group "isrvr".
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>







      RETURN
END SUBROUTINE get_mpi_comm_io_groups

SUBROUTINE get_nio_tasks_in_group( retval )
!<DESCRIPTION>
!<PRE>
! This routine returns the number of I/O server tasks in each 
! I/O server group.  It can be called by both clients and 
! servers.  
!</PRE>
!</DESCRIPTION>






      RETURN
END SUBROUTINE get_nio_tasks_in_group


