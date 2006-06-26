# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "win32cdf.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

################################################################################
# Begin Project
CPP=cl.exe

!IF  "$(CFG)" == "Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
OUTDIR=.
INTDIR=.

ALL : $(OUTDIR)/win32cdf.lib $(OUTDIR)/win32cdf.bsc

# ADD BASE CPP /nologo /ML /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /ML /W3 /GX /O2 /I "..\..\hdf\src" /I "..\..\hdf\jpeg" /I "..\xdr" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "NO_SYS_XDR_INC" /D "HDF" /c
# SUBTRACT CPP /YX /Fr
CPP_PROJ=/nologo /ML /W3 /GX /O2 /I "..\..\hdf\src" /I "..\..\hdf\jpeg" /I\
 "..\xdr" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "NO_SYS_XDR_INC" /D "HDF"\
 /Fo$(INTDIR)/ /c 
CPP_OBJS=
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"win32cdf.bsc" 

$(OUTDIR)/win32cdf.bsc : $(OUTDIR)  $(BSC32_SBRS)
LIB32=lib.exe
DEF_FLAGS=
DEF_FILE=
LIB32_OBJS= \
	$(INTDIR)/globdef.obj \
	$(INTDIR)/error.obj \
	$(INTDIR)/iarray.obj \
	$(INTDIR)/array.obj \
	$(INTDIR)/attr.obj \
	$(INTDIR)/putgetg.obj \
	$(INTDIR)/hdfsds.obj \
	$(INTDIR)/string.obj \
	$(INTDIR)/dim.obj \
	$(INTDIR)/cdf.obj \
	$(INTDIR)/var.obj \
	$(INTDIR)/mfsd.obj \
	$(INTDIR)/xdrposix.obj \
	$(INTDIR)/file.obj \
	$(INTDIR)/nssdc.obj \
	$(INTDIR)/sharray.obj \
	$(INTDIR)/putget.obj
# ADD BASE LIB32 /NOLOGO
# ADD LIB32 /NOLOGO
LIB32_FLAGS=/NOLOGO /OUT:$(OUTDIR)\"win32cdf.lib" 

$(OUTDIR)/win32cdf.lib : $(OUTDIR)  $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "WinDebug"
# PROP BASE Intermediate_Dir "WinDebug"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
OUTDIR=.
INTDIR=.

ALL : $(OUTDIR)/win32cdf.lib $(OUTDIR)/win32cdf.bsc

# ADD BASE CPP /nologo /ML /W3 /GX /Z7 /YX /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /ML /W3 /GX /Z7 /Od /I "..\..\hdf\src" /I "..\..\hdf\jpeg" /I "..\xdr" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "NO_SYS_XDR_INC" /D "HDF" /c
# SUBTRACT CPP /YX /Fr
CPP_PROJ=/nologo /ML /W3 /GX /Z7 /Od /I "..\..\hdf\src" /I "..\..\hdf\jpeg" /I\
 "..\xdr" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "NO_SYS_XDR_INC" /D "HDF"\
 /Fo$(INTDIR)/ /c 
CPP_OBJS=
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"win32cdf.bsc" 

$(OUTDIR)/win32cdf.bsc : $(OUTDIR)  $(BSC32_SBRS)
LIB32=lib.exe
DEF_FLAGS=
DEF_FILE=
LIB32_OBJS= \
	$(INTDIR)/globdef.obj \
	$(INTDIR)/error.obj \
	$(INTDIR)/iarray.obj \
	$(INTDIR)/array.obj \
	$(INTDIR)/attr.obj \
	$(INTDIR)/putgetg.obj \
	$(INTDIR)/hdfsds.obj \
	$(INTDIR)/string.obj \
	$(INTDIR)/dim.obj \
	$(INTDIR)/cdf.obj \
	$(INTDIR)/var.obj \
	$(INTDIR)/mfsd.obj \
	$(INTDIR)/xdrposix.obj \
	$(INTDIR)/file.obj \
	$(INTDIR)/nssdc.obj \
	$(INTDIR)/sharray.obj \
	$(INTDIR)/putget.obj
# ADD BASE LIB32 /NOLOGO
# ADD LIB32 /NOLOGO
LIB32_FLAGS=/NOLOGO /OUT:$(OUTDIR)\"win32cdf.lib" 

$(OUTDIR)/win32cdf.lib : $(OUTDIR)  $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Group "Source Files"

################################################################################
# Begin Source File

SOURCE=.\globdef.c

$(INTDIR)/globdef.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\error.c

$(INTDIR)/error.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\iarray.c

$(INTDIR)/iarray.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\alloc.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\error.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\array.c

$(INTDIR)/array.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\attr.c

$(INTDIR)/attr.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\putgetg.c

$(INTDIR)/putgetg.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hdfsds.c

$(INTDIR)/hdfsds.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\string.c

$(INTDIR)/string.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mfhdf.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\dim.c

$(INTDIR)/dim.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cdf.c

$(INTDIR)/cdf.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\var.c

$(INTDIR)/var.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mfsd.c

$(INTDIR)/mfsd.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\xdrposix.c

$(INTDIR)/xdrposix.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\local_nc.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\file.c

$(INTDIR)/file.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\nssdc.c

$(INTDIR)/nssdc.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\sharray.c

$(INTDIR)/sharray.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\putget.c

$(INTDIR)/putget.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\netcdf.h
# End Source File
# End Group
# End Project
################################################################################
