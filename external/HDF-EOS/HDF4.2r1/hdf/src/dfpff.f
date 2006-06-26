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
C $Id: dfpff.f,v 1.5 1993/11/03 19:59:16 koziol Exp $
C
C------------------------------------------------------------------------------
C File:     dfpFf.f
C Purpose:  Fortran stubs for Palette Fortran routines
C Invokes:  dfpF.c dfkit.c
C Contents: 
C   dpgpal:         Call dpigpal to get palette
C   dpapal:         Call dpippal to add palette to file
C   dpppal:         Call dpippal to write/overwrite palette in file
C   dpnpal:         Call dpinpal to get number of palettes in file
C   dpwref:         Call dpiwref to set ref of pal to write next
C   dprref:         Call dpirref to set ref of pal to read next
C   dfpgetpal:      Call dpigpal to get palette
C   dfpaddpal:      Call dpippal to add palette to file
C   dfpputpal:      Call dpippal to write/overwrite palette in file
C   dfpnpals:       Call dpinpal to get number of palettes in file
C   dfpwriteref:    Call dpiwref to set ref of pal to write next
C   dfpreadref:     Call dpirref to set ref of pal to read next
C Remarks: none
C----------------------------------------------------------------------------*/


C------------------------------------------------------------------------------
C Name: dpgpal
C Purpose:  call dpigpal, get palette
C Inputs:   filename: filename to get pal from
C           pal: space to put palette
C Returns: 0 on success, -1 on failure with DFerror set
C Users:    Fortran stub routine
C Invokes: dpigpal
C----------------------------------------------------------------------------*/

      integer function dpgpal(filename, pal)

      character*(*) filename
      character*(*) pal
      integer dpigpal

      dpgpal = dpigpal(filename, pal, len(filename))
      return
      end


C------------------------------------------------------------------------------
C Name: dpapal
C Purpose:  call dpippal, add palette
C Inputs:   filename: filename to put pal into
C           pal: palette
C Returns: 0 on success, -1 on failure with DFerror set
C Users:    Fortran stub routine
C Invokes: dpippal
C----------------------------------------------------------------------------*/

      integer function dpapal(filename, pal)

      character*(*) filename
      character*(*) pal
      integer dpippal

      dpapal = dpippal(filename, pal, 0, 'a', len(filename))
      return
      end
      
      
C------------------------------------------------------------------------------
C     Name: dpppal
C     Purpose:  call dpippal, write palette
C     Inputs:   filename: filename to put pal to
C     		pal: palette
C           ow, filemode: see dfpputpal
C     Returns: 0 on success, -1 on failure with DFerror set
C     Users:    Fortran stub routine
C     Invokes: dpippal
C----------------------------------------------------------------------------*/
      
      integer function dpppal(filename, pal, ow, filemode)
      
      character*(*) filename
      character*(*) pal
      integer dpippal, ow
      character*(*) filemode
      
      dpppal = dpippal(filename, pal, ow, filemode, len(filename))
      return
      end
      
      
C------------------------------------------------------------------------------
C     Name: dpnpals
C     Purpose:  How many palettes are present in this file?
C     Inputs:   filename: name of HDF file
C     Returns: number of palettes on success, -1 on failure with DFerror set
C     Users:    HDF programmers, other routines and utilities
C     Invokes: dpinpal
C----------------------------------------------------------------------------*/
      
      integer function dpnpals(filename)
      
      character*(*) filename
      integer dpinpal
      
      dpnpals = dpinpal(filename, len(filename))
      return
      end
      
      
C------------------------------------------------------------------------------
C     Name: dpwref
C     Purpose:  Ref to write next
C     Inputs:   filename: name of HDF file
C     ref: ref to write next
C     Returns: number of palettes on success, -1 on failure with DFerror set
C     Users:    HDF programmers, other routines and utilities
C     Invokes: dpiwref
C----------------------------------------------------------------------------*/
      
      integer function dpwref(filename, ref)
      
      character*(*) filename
      integer ref, dpiwref
      
      dpwref = dpiwref(filename, ref, len(filename))
      return
      end
      
      
C------------------------------------------------------------------------------
C     Name: dprref
C     Purpose:  Ref to read next
C     Inputs:   filename: name of HDF file
C     ref: ref to read next
C     Returns: number of palettes on success, -1 on failure with DFerror set
C     Users:    HDF programmers, other routines and utilities
C     Invokes: dpirref
C----------------------------------------------------------------------------*/
      
      integer function dprref(filename, ref)
      
      character*(*) filename
      integer ref, dpirref
      
      dprref = dpirref(filename, ref, len(filename))
      return
      end
      
      
CEND7MAX
      
      
C------------------------------------------------------------------------------
C     Name: dfpgetpal
C     Purpose:  call dpigpal, get palette
C     Inputs:   filename: filename to get pal from
C     pal: space to put palette
C     Returns: 0 on success, -1 on failure with DFerror set
C     Users:    Fortran stub routine
C     Invokes: dpigpal
C----------------------------------------------------------------------------*/
      
      integer function dfpgetpal(filename, pal)
      
      character*(*) filename
      character*(*) pal
      integer dpigpal
      
      dfpgetpal = dpigpal(filename, pal, len(filename))
      return
      end
      
      
C------------------------------------------------------------------------------
C     Name: dfpaddpal
C     Purpose:  call dpippal, add palette
C     Inputs:   filename: filename to put pal into
C     pal: palette
C     Returns: 0 on success, -1 on failure with DFerror set
C     Users:    Fortran stub routine
C     Invokes: dpippal
C----------------------------------------------------------------------------*/
      
      integer function dfpaddpal(filename, pal)
      
      character*(*) filename
      character*(*) pal
      integer dpippal
      
      dfpaddpal = dpippal(filename, pal, 0, 'a', len(filename))
      return
      end
      
      
C------------------------------------------------------------------------------
C     Name: dfpputpal
C     Purpose:  call dpippal, write palette
C     Inputs:   filename: filename to put pal to
C     pal: palette
C     ow, filemode: see dfpputpal
C     Returns: 0 on success, -1 on failure with DFerror set
C     Users:    Fortran stub routine
C     Invokes: dpippal
C----------------------------------------------------------------------------*/
      
      integer function dfpputpal(filename, pal, ow, filemode)
      
      character*(*) filename
      character*(*) pal
      integer dpippal, ow
      character*(*) filemode
      
      dfpputpal = dpippal(filename, pal, ow, filemode, len(filename))
      return
      end
      
      
C------------------------------------------------------------------------------
C     Name: dpnpals
C     Purpose:  How many palettes are present in this file?
C     Inputs:   filename: name of HDF file
C     Returns: number of palettes on success, -1 on failure with DFerror set
C     Users:    HDF programmers, other routines and utilities
C     Invokes: dpinpal
C----------------------------------------------------------------------------*/
      
      integer function dfpnpals(filename)
      
      character*(*) filename
      integer dpinpal
      
      dfpnpals = dpinpal(filename, len(filename))
      return
      end

      
C------------------------------------------------------------------------------
C     Name: dfpwriteref
C     Purpose:  Ref to write next
C     Inputs:   filename: name of HDF file
C     ref: ref to write next
C     Returns: number of palettes on success, -1 on failure with DFerror set
C     Users:    HDF programmers, other routines and utilities
C     Invokes: dpiwref
C----------------------------------------------------------------------------*/
      
      integer function dfpwriteref(filename, ref)
      
      character*(*) filename
      integer ref, dpiwref
      
      dfpwriteref = dpiwref(filename, ref, len(filename))
      return
      end
      
      
C------------------------------------------------------------------------------
C     Name: dfpreadref
C     Purpose:  Ref to read next
C     Inputs:   filename: name of HDF file
C     ref: ref to read next
C     Returns: number of palettes on success, -1 on failure with DFerror set
C     Users:    HDF programmers, other routines and utilities
C     Invokes: dpirref
C----------------------------------------------------------------------------*/
      
      integer function dfpreadref(filename, ref)
      
      character*(*) filename
      integer ref, dpirref
      
      dfpreadref = dpirref(filename, ref, len(filename))
      return
      end
