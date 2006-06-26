# Microsoft Visual C++ generated build script - Do not modify

PROJ = MFHDF
DEBUG = 1
PROGTYPE = 3
CALLER = 
ARGS = 
DLLS = 
ORIGIN = MSVCNT
ORIGIN_VER = 1.00
PROJPATH = E:\KOZIOL\MFHDF\ 
USEMFC = 0
CC = cl
CPP = cl
CXX = cl
CCREATEPCHFLAG = 
CPPCREATEPCHFLAG = 
CUSEPCHFLAG = 
CPPUSEPCHFLAG = 
FIRSTC = ARRAY.C
FIRSTCPP = 
RC = rc
CFLAGS_D_LIB32 = /nologo /W3 /Z7 /YX /D "_DEBUG" /D "STDC_INCLUDES" /D "_X86_" /D "_WINDOWS" /D "HDF" /D "WINNT" /D "NO_SYS_XDR_INC" /D "i386" /I "e:\koziol\mfhdf\xdr" /FR /ML /Fp"MFHDF.PCH"
CFLAGS_R_LIB32 = /nologo /W3 /YX /O2 /D "NDEBUG" /D "_X86_" /D "_WINDOWS" /D "HDF" /D "WINNT" /D "NO_SYS_XDR_INC" /D "i386" /I "e:\koziol\mfhdf\xdr" /FR /ML /Fp"MFHDF.PCH"
LFLAGS_D_LIB32 = /NOLOGO
LFLAGS_R_LIB32 = /NOLOGO
RCFLAGS32 = 
D_RCDEFINES32 = -d_DEBUG
R_RCDEFINES32 = -dNDEBUG
OBJS_EXT = 
LIBS_EXT = 
!if "$(DEBUG)" == "1"
CFLAGS = $(CFLAGS_D_LIB32)
LFLAGS = 
LIBS = 
LFLAGS_LIB=$(LFLAGS_D_LIB32)
MAPFILE_OPTION = 
RCDEFINES = $(D_RCDEFINES32)
!else
CFLAGS = $(CFLAGS_R_LIB32)
LFLAGS = 
LIBS = 
MAPFILE_OPTION = 
LFLAGS_LIB=$(LFLAGS_R_LIB32)
RCDEFINES = $(R_RCDEFINES32)
!endif
SBRS = ARRAY.SBR \
		ATTR.SBR \
		CDF.SBR \
		DIM.SBR \
		ERROR.SBR \
		FILE.SBR \
		GLOBDEF.SBR \
		HDFSDS.SBR \
		IARRAY.SBR \
		MFSD.SBR \
		PUTGET.SBR \
		PUTGETG.SBR \
		SHARRAY.SBR \
		STRING.SBR \
		VAR.SBR \
		XDRPOSIX.SBR \
		BYTEORDR.SBR \
		XDR.SBR \
		XDRARRAY.SBR \
		XDRFLOAT.SBR \
		XDRSTDIO.SBR \
		GETOPT.SBR


ARRAY_DEP =  \
	e:\koziol\mfhdf\libsrc\local_nc.h \
	d:\msvcnt\include\rpc\xdr.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h \
	e:\koziol\mfhdf\libsrc\netcdf.h \
	d:\users\koziol\hdf\src\vg.h \
	d:\users\koziol\hdf\src\tbbt.h \
	e:\koziol\mfhdf\libsrc\alloc.h


ATTR_DEP =  \
	e:\koziol\mfhdf\libsrc\local_nc.h \
	d:\msvcnt\include\rpc\xdr.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h \
	e:\koziol\mfhdf\libsrc\netcdf.h \
	d:\users\koziol\hdf\src\vg.h \
	d:\users\koziol\hdf\src\tbbt.h \
	e:\koziol\mfhdf\libsrc\alloc.h


CDF_DEP =  \
	e:\koziol\mfhdf\libsrc\local_nc.h \
	d:\msvcnt\include\rpc\xdr.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h \
	e:\koziol\mfhdf\libsrc\netcdf.h \
	d:\users\koziol\hdf\src\vg.h \
	d:\users\koziol\hdf\src\tbbt.h \
	e:\koziol\mfhdf\libsrc\alloc.h


DIM_DEP =  \
	e:\koziol\mfhdf\libsrc\local_nc.h \
	d:\msvcnt\include\rpc\xdr.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h \
	e:\koziol\mfhdf\libsrc\netcdf.h \
	d:\users\koziol\hdf\src\vg.h \
	d:\users\koziol\hdf\src\tbbt.h \
	e:\koziol\mfhdf\libsrc\alloc.h


ERROR_DEP =  \
	e:\koziol\mfhdf\libsrc\local_nc.h \
	d:\msvcnt\include\rpc\xdr.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h \
	e:\koziol\mfhdf\libsrc\netcdf.h \
	d:\users\koziol\hdf\src\vg.h \
	d:\users\koziol\hdf\src\tbbt.h


FILE_DEP =  \
	e:\koziol\mfhdf\libsrc\local_nc.h \
	d:\msvcnt\include\rpc\xdr.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h \
	e:\koziol\mfhdf\libsrc\netcdf.h \
	d:\users\koziol\hdf\src\vg.h \
	d:\users\koziol\hdf\src\tbbt.h \
	e:\koziol\mfhdf\libsrc\alloc.h


GLOBDEF_DEP =  \
	e:\koziol\mfhdf\libsrc\netcdf.h


HDFSDS_DEP =  \
	e:\koziol\mfhdf\libsrc\local_nc.h \
	d:\msvcnt\include\rpc\xdr.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h \
	e:\koziol\mfhdf\libsrc\netcdf.h \
	d:\users\koziol\hdf\src\vg.h \
	d:\users\koziol\hdf\src\tbbt.h


IARRAY_DEP =  \
	e:\koziol\mfhdf\libsrc\local_nc.h \
	d:\msvcnt\include\rpc\xdr.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h \
	e:\koziol\mfhdf\libsrc\netcdf.h \
	d:\users\koziol\hdf\src\vg.h \
	d:\users\koziol\hdf\src\tbbt.h \
	e:\koziol\mfhdf\libsrc\alloc.h


MFSD_DEP =  \
	e:\koziol\mfhdf\libsrc\mfhdf.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h \
	e:\koziol\mfhdf\libsrc\netcdf.h \
	e:\koziol\mfhdf\libsrc\local_nc.h \
	d:\msvcnt\include\rpc\xdr.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\vg.h \
	d:\users\koziol\hdf\src\tbbt.h \
	d:\users\koziol\hdf\src\hfile.h


PUTGET_DEP =  \
	e:\koziol\mfhdf\libsrc\local_nc.h \
	d:\msvcnt\include\rpc\xdr.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h \
	e:\koziol\mfhdf\libsrc\netcdf.h \
	d:\users\koziol\hdf\src\vg.h \
	d:\users\koziol\hdf\src\tbbt.h \
	e:\koziol\mfhdf\libsrc\alloc.h


PUTGETG_DEP =  \
	e:\koziol\mfhdf\libsrc\local_nc.h \
	d:\msvcnt\include\rpc\xdr.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h \
	e:\koziol\mfhdf\libsrc\netcdf.h \
	d:\users\koziol\hdf\src\vg.h \
	d:\users\koziol\hdf\src\tbbt.h


SHARRAY_DEP =  \
	e:\koziol\mfhdf\libsrc\local_nc.h \
	d:\msvcnt\include\rpc\xdr.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h \
	e:\koziol\mfhdf\libsrc\netcdf.h \
	d:\users\koziol\hdf\src\vg.h \
	d:\users\koziol\hdf\src\tbbt.h \
	e:\koziol\mfhdf\libsrc\alloc.h


STRING_DEP =  \
	e:\koziol\mfhdf\libsrc\local_nc.h \
	d:\msvcnt\include\rpc\xdr.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h \
	e:\koziol\mfhdf\libsrc\netcdf.h \
	d:\users\koziol\hdf\src\vg.h \
	d:\users\koziol\hdf\src\tbbt.h \
	e:\koziol\mfhdf\libsrc\alloc.h


VAR_DEP =  \
	e:\koziol\mfhdf\libsrc\local_nc.h \
	d:\msvcnt\include\rpc\xdr.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h \
	e:\koziol\mfhdf\libsrc\netcdf.h \
	d:\users\koziol\hdf\src\vg.h \
	d:\users\koziol\hdf\src\tbbt.h \
	e:\koziol\mfhdf\libsrc\alloc.h


XDRPOSIX_DEP =  \
	e:\koziol\mfhdf\libsrc\netcdf.h \
	e:\koziol\mfhdf\libsrc\local_nc.h \
	d:\msvcnt\include\rpc\xdr.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h \
	d:\users\koziol\hdf\src\vg.h \
	d:\users\koziol\hdf\src\tbbt.h \
	e:\koziol\mfhdf\libsrc\mfhdf.h


BYTEORDR_DEP = 

XDR_DEP =  \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h


XDRARRAY_DEP =  \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h


XDRFLOAT_DEP =  \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h


XDRSTDIO_DEP =  \
	d:\msvcnt\include\netinet\in.h \
	e:\koziol\mfhdf\xdr\xdr.h \
	d:\users\koziol\hdf\src\hdf.h \
	d:\users\koziol\hdf\src\hdfi.h \
	d:\msvcnt\include\sys\file.h \
	d:\users\koziol\hdf\src\dfivms.h \
	d:\users\koziol\hdf\src\maldebug.h \
	d:\users\koziol\hdf\src\hcomp.h \
	d:\users\koziol\hdf\src\herr.h \
	d:\users\koziol\hdf\src\hproto.h \
	d:\users\koziol\hdf\src\vproto.h


GETOPT_DEP = 

all:	$(PROJ).LIB $(PROJ).BSC

ARRAY.OBJ:	LIBSRC\ARRAY.C $(ARRAY_DEP)
	$(CC) $(CFLAGS) $(CCREATEPCHFLAG) /c LIBSRC\ARRAY.C

ATTR.OBJ:	LIBSRC\ATTR.C $(ATTR_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LIBSRC\ATTR.C

CDF.OBJ:	LIBSRC\CDF.C $(CDF_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LIBSRC\CDF.C

DIM.OBJ:	LIBSRC\DIM.C $(DIM_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LIBSRC\DIM.C

ERROR.OBJ:	LIBSRC\ERROR.C $(ERROR_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LIBSRC\ERROR.C

FILE.OBJ:	LIBSRC\FILE.C $(FILE_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LIBSRC\FILE.C

GLOBDEF.OBJ:	LIBSRC\GLOBDEF.C $(GLOBDEF_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LIBSRC\GLOBDEF.C

HDFSDS.OBJ:	LIBSRC\HDFSDS.C $(HDFSDS_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LIBSRC\HDFSDS.C

IARRAY.OBJ:	LIBSRC\IARRAY.C $(IARRAY_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LIBSRC\IARRAY.C

MFSD.OBJ:	LIBSRC\MFSD.C $(MFSD_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LIBSRC\MFSD.C

PUTGET.OBJ:	LIBSRC\PUTGET.C $(PUTGET_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LIBSRC\PUTGET.C

PUTGETG.OBJ:	LIBSRC\PUTGETG.C $(PUTGETG_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LIBSRC\PUTGETG.C

SHARRAY.OBJ:	LIBSRC\SHARRAY.C $(SHARRAY_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LIBSRC\SHARRAY.C

STRING.OBJ:	LIBSRC\STRING.C $(STRING_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LIBSRC\STRING.C

VAR.OBJ:	LIBSRC\VAR.C $(VAR_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LIBSRC\VAR.C

XDRPOSIX.OBJ:	LIBSRC\XDRPOSIX.C $(XDRPOSIX_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LIBSRC\XDRPOSIX.C

BYTEORDR.OBJ:	XDR\BYTEORDR.C $(BYTEORDR_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c XDR\BYTEORDR.C

XDR.OBJ:	XDR\XDR.C $(XDR_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c XDR\XDR.C

XDRARRAY.OBJ:	XDR\XDRARRAY.C $(XDRARRAY_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c XDR\XDRARRAY.C

XDRFLOAT.OBJ:	XDR\XDRFLOAT.C $(XDRFLOAT_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c XDR\XDRFLOAT.C

XDRSTDIO.OBJ:	XDR\XDRSTDIO.C $(XDRSTDIO_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c XDR\XDRSTDIO.C

GETOPT.OBJ:	UTIL\GETOPT.C $(GETOPT_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c UTIL\GETOPT.C

$(PROJ).LIB:	ARRAY.OBJ ATTR.OBJ CDF.OBJ DIM.OBJ ERROR.OBJ FILE.OBJ GLOBDEF.OBJ HDFSDS.OBJ \
	IARRAY.OBJ MFSD.OBJ PUTGET.OBJ PUTGETG.OBJ SHARRAY.OBJ STRING.OBJ VAR.OBJ XDRPOSIX.OBJ \
	BYTEORDR.OBJ XDR.OBJ XDRARRAY.OBJ XDRFLOAT.OBJ XDRSTDIO.OBJ GETOPT.OBJ $(OBJS_EXT) $(LIBS_EXT)
	echo >NUL @<<$(PROJ).CRF
ARRAY.OBJ 
ATTR.OBJ 
CDF.OBJ 
DIM.OBJ 
ERROR.OBJ 
FILE.OBJ 
GLOBDEF.OBJ 
HDFSDS.OBJ 
IARRAY.OBJ 
MFSD.OBJ 
PUTGET.OBJ 
PUTGETG.OBJ 
SHARRAY.OBJ 
STRING.OBJ 
VAR.OBJ 
XDRPOSIX.OBJ 
BYTEORDR.OBJ 
XDR.OBJ 
XDRARRAY.OBJ 
XDRFLOAT.OBJ 
XDRSTDIO.OBJ 
GETOPT.OBJ 


<<
	if exist $@ del $@
	link -LIB @$(PROJ).CRF

$(PROJ).BSC: $(SBRS)
	bscmake @<<
/o$@ $(SBRS)
<<
