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
C $Id: dfff.f,v 1.4 1993/11/03 19:58:15 koziol Exp $ 
C
C------------------------------------------------------------------------------
C File:     dfFf.f
C Purpose:  Fortran stubs for Fortran low level i/o routines
C Invokes:  dfF.c dfkit.c
C Contents: 
C   dfopen:     call dfiopen to open HDF file
C   dfishdf:    call dfiishdf to find is file is HDF
C -----------------------------------------------------------------------------

C------------------------------------------------------------------------------
C Name:     dfopen
C Purpose:  call dfiopen to open HDF file
C Inputs:   name: name of HDF file to open
C           access: integer for access mode: DFACC_READ etc.
C           defdds: default number of DDs per header block
C Returns:  0 on success, -1 on failure with DFerror set
C Users:    HDF Fortran programmers
C Invokes:  dfiopen
C------------------------------------------------------------------------------


      integer function dfopen(name, access, defdds)
      character*(*) name
      integer access, defdds, dfiopen

      dfopen = dfiopen(name, access, defdds, len(name))
      return
      end

C------------------------------------------------------------------------------
C Name:    dfaccess
C Purpose: call dfiaccess to set up access to a data element
C Inputs:  dfile: pointer to open HDF file
C          tag: tag of element to access
C          ref: ref of element to access
C          access: access mode requested
C Returns: 0 on success, -1 on failure with DFerror set
C Users:   HDF FORTRAN programmers
C Invokes: dfiaccess
C------------------------------------------------------------------------------

      integer function dfaccess(dfile, tag, ref, access)
      character*(*) access
      integer dfile, tag, ref, dfiaccess

      dfaccess = dfiaccess(dfile, tag, ref, access, len(access))
      return
      end

C------------------------------------------------------------------------------
C Name:     dfishdf
C Purpose:  call dfiishdf to test file
C Inputs:   name: name of file to test
C Returns:  0 on success, -1 on failure with DFerror set
C Users:    HDF Fortran programmers
C Invokes:  dfiishdf
C------------------------------------------------------------------------------


      integer function dfishdf(name)
      character*(*) name
      integer dfiishdf

      dfishdf = dfiishdf(name, len(name))
      return
      end
