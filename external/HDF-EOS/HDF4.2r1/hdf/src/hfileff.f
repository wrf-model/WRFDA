C****************************************************************************
C* NCSA HDF                                                                 *
C* Software Development Group                                               *
C* National Center for Supercomputing Applications                          *
C* University of Illinois at Urbana-Champaign                               *
C* 605 E. Springfield, Champaign IL 61820                                   *
C*                                                                          *
C* For conditions of distribution and use, see the accompanying             *
C* hdf/COPYING file.                                                        *
C*                                                                          *
C****************************************************************************
C
C $Id: hfileff.f,v 1.13 2001/10/01 21:53:47 epourmal Exp $
C
C------------------------------------------------------------------------------
C File:     hfileFf.f
C Purpose:  Fortran stubs for Palette Fortran routines
C Invokes:  hfileF.c 
C Contents: 
C   hopen:          Call hiopen to open file
C hnumber:          Call hnumber
C Remarks: none
C----------------------------------------------------------------------------*/


C------------------------------------------------------------------------------
C Name: hopen
C Purpose:  call hiopen, open file
C Inputs:   path: Name of file to be opened
C           access: DFACC_READ, DFACC_WRITE, DFACC_CREATE,
C                      or any bitwise-or of the above.
C           ndds: Number of dds in header block if file needs to be created.
C Returns: 0 on success, FAIL on failure with error set
C Users:    Fortran stub routine
C Invokes: hiopen
C----------------------------------------------------------------------------*/

      integer function hopen(filename, access, defdds)

      character*(*) filename
      integer       access, defdds, hiopen

      hopen = hiopen(filename, access, defdds, len(filename))
      return
      end

C------------------------------------------------------------------------------
C Name: hxsdir
C Purpose:  call hxisdir to set directory variable for locating an external file
C Inputs:   dir: names of directory separated by colons.
C Returns:  SUCCEED if no error, else FAIL
C Users:    Fortran stub routine
C Invokes: hxisdir
C----------------------------------------------------------------------------*/

      integer function hxsdir(dir)

      character*(*) dir
      integer       hxisdir

      hxsdir = hxisdir(dir, len(dir))
      return
      end

C------------------------------------------------------------------------------
C Name: hxscdir
C Purpose:  call hxiscdir to set directory variable for creating an external file
C Inputs:   dir: name of the directory
C Returns:  SUCCEED if no error, else FAIL
C Users:    Fortran stub routine
C Invokes: hxiscdir
C----------------------------------------------------------------------------*/

      integer function hxscdir(dir)

      character*(*) dir
      integer       hxiscdir

      hxscdir = hxiscdir(dir, len(dir))
      return
      end
C-----------------------------------------------------------------------------
C Name: hglibver
C Purpose: retrieves the version information for the current HDF library
C Outputs: major_v - major version number
C          minor_v - minor version number
C          release - release number
C          string  - version number test string
C Retruns: SUCCEED (0) if successful and FAIL(-1) otherwise
C-----------------------------------------------------------------------------*/

      integer function hglibver(major_v, minor_v, release, string)

      integer major_v, minor_v, release
      character*(*) string
      integer hglibverc 

      hglibver = hglibverc(major_v, minor_v, release, string,
     .                     len(string))
      return
      end
C-----------------------------------------------------------------------------
C Name: hgfilver
C Purpose: retrieves the version information for the current HDF library
C Inputs:  file_id - file identifier
C Outputs: major_v - major version number
C          minor_v - minor version number
C          release - release number
C          string  - version number test string
C Retruns: SUCCEED (0) if successful and FAIL(-1) otherwise
C-----------------------------------------------------------------------------*/

      integer function hgfilver(file_id, major_v, minor_v, release,
     .                          string)

      integer file_id, major_v, minor_v, release
      character*(*) string
      integer hgfilverc 

      hgfilver = hgfilverc(file_id, major_v, minor_v, release, string,
     .                     len(string))
      return
      end
C------------------------------------------------------------------------------
C Name: hishdff
C Purpose:  Identifies if the file "file_name" is an HDF file. 
C Inputs:   file_name:  File name
C Returns: TRUE (1) if successful, FALSE (0) otherwise.
C Invokes: hiopen
C----------------------------------------------------------------------------*/

      integer function hishdff(filename)

      character*(*) filename
      integer       hiishdf

      hishdff = hiishdf(filename, len(filename))
      return
      end
C-----------------------------------------------------------------------------
C Name: hestringf
C Purpose: retrieves the error message associated with the specified error code 
C Inputs:  error_code 
C Outputs: error_message - string associated with the error code 
C Retruns: SUCCEED (0) if successful and FAIL(-1) otherwise
C-----------------------------------------------------------------------------*/

      integer function hestringf(error_code, error_message)

      integer error_code 
      character*(*) error_message 
      integer hestringc 

      hestringf = hestringc(error_code, error_message,
     .                      len(error_message))
      return
      end
C-----------------------------------------------------------------------------
C Name: heprntf
C Purpose: prints values from the error stack 
C Inputs:  filename - name of the output file; if length of the
C          filename is 0, then output goes to stdout.  
C          print_levels - number of levels to print 
C Retruns: SUCCEED (0) if successful and FAIL(-1) otherwise
C
C Remarks: This routine always prints to the standard output.
C-----------------------------------------------------------------------------*/

      integer function heprntf(filename, print_levels)
      integer print_levels 
      character*(*) filename
      
      integer heprntc
      heprntf = heprntc(filename, print_levels,len(filename))
      return
      end
