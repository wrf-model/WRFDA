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
!MESSAGE NMAKE /f "win32hdf.mak" CFG="Win32 Debug"
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
# PROP Target_Last_Scanned "Win32 Debug"
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

ALL : .\win32hdf.lib .\win32hdf.bsc

# ADD BASE CPP /nologo /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\jpeg" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /c
# SUBTRACT CPP /YX /Fr
CPP_PROJ=/nologo /W3 /GX /O2 /I "..\jpeg" /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fo$(INTDIR)/ /c 
CPP_OBJS=
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"win32hdf.bsc" 

.\win32hdf.bsc : $(OUTDIR)  $(BSC32_SBRS)
LIB32=lib.exe
DEF_FLAGS=
DEF_FILE=
LIB32_OBJS= \
	.\dfkswap.obj \
	.\vconv.obj \
	.\dfrle.obj \
	.\hbitio.obj \
	.\dfjpeg.obj \
	.\tbbt.obj \
	.\dff.obj \
	.\hextelt.obj \
	.\dfsd.obj \
	.\dfimcomp.obj \
	.\dfkconv.obj \
	.\crle.obj \
	.\dfkfuji.obj \
	.\herrf.obj \
	.\vsfld.obj \
	.\dfconv.obj \
	.\dfufp2i.obj \
	.\dfstubs.obj \
	.\dfkvms.obj \
	.\mstdio.obj \
	.\dfgroup.obj \
	.\dfp.obj \
	.\dfutil.obj \
	.\dfcomp.obj \
	.\vio.obj \
	.\mfgr.obj \
	.\herr.obj \
	.\dfgr.obj \
	.\hkit.obj \
	.\df24.obj \
	.\dfkcray.obj \
	.\cnone.obj \
	.\vhi.obj \
	.\hblocks.obj \
	.\hdfalloc.obj \
	.\vrw.obj \
	.\mfan.obj \
	.\hvblocks.obj \
	.\vg.obj \
	.\cnbit.obj \
	.\dfan.obj \
	.\cskphuff.obj \
	.\vgp.obj \
	.\hcomp.obj \
	.\dfknat.obj \
	.\vparse.obj \
	.\dfunjpeg.obj \
	.\hfile.obj \
	.\cgzip.obj \
	.\dfr8.obj
# ADD BASE LIB32 /NOLOGO
# ADD LIB32 /NOLOGO
LIB32_FLAGS=/NOLOGO /OUT:$(OUTDIR)\"win32hdf.lib" 

.\win32hdf.lib : $(OUTDIR)  $(DEF_FILE) $(LIB32_OBJS)
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

ALL : .\win32hdf.lib .\win32hdf.bsc

# ADD BASE CPP /nologo /W3 /GX /Z7 /YX /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /W3 /GX /Z7 /Od /I "..\jpeg" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /c
# SUBTRACT CPP /YX /Fr
CPP_PROJ=/nologo /W3 /GX /Z7 /Od /I "..\jpeg" /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /Fo$(INTDIR)/ /c 
CPP_OBJS=
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"win32hdf.bsc" 

.\win32hdf.bsc : $(OUTDIR)  $(BSC32_SBRS)
LIB32=lib.exe
DEF_FLAGS=
DEF_FILE=
LIB32_OBJS= \
	.\dfkswap.obj \
	.\vconv.obj \
	.\dfrle.obj \
	.\hbitio.obj \
	.\dfjpeg.obj \
	.\tbbt.obj \
	.\dff.obj \
	.\hextelt.obj \
	.\dfsd.obj \
	.\dfimcomp.obj \
	.\dfkconv.obj \
	.\crle.obj \
	.\dfkfuji.obj \
	.\herrf.obj \
	.\vsfld.obj \
	.\dfconv.obj \
	.\dfufp2i.obj \
	.\dfstubs.obj \
	.\dfkvms.obj \
	.\mstdio.obj \
	.\dfgroup.obj \
	.\dfp.obj \
	.\dfutil.obj \
	.\dfcomp.obj \
	.\vio.obj \
	.\mfgr.obj \
	.\herr.obj \
	.\dfgr.obj \
	.\hkit.obj \
	.\df24.obj \
	.\dfkcray.obj \
	.\cnone.obj \
	.\vhi.obj \
	.\hblocks.obj \
	.\hdfalloc.obj \
	.\vrw.obj \
	.\mfan.obj \
	.\hvblocks.obj \
	.\vg.obj \
	.\cnbit.obj \
	.\dfan.obj \
	.\cskphuff.obj \
	.\vgp.obj \
	.\hcomp.obj \
	.\dfknat.obj \
	.\vparse.obj \
	.\dfunjpeg.obj \
	.\hfile.obj \
	.\cgzip.obj \
	.\dfr8.obj
# ADD BASE LIB32 /NOLOGO
# ADD LIB32 /NOLOGO
LIB32_FLAGS=/NOLOGO /OUT:$(OUTDIR)\"win32hdf.lib" 

.\win32hdf.lib : $(OUTDIR)  $(DEF_FILE) $(LIB32_OBJS)
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

SOURCE=.\dfkswap.c

.\dfkswap.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vconv.c

.\vconv.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfivms.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfrle.c

.\dfrle.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hbitio.c

.\hbitio.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfrig.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\hbitio.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfjpeg.c

.\dfjpeg.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\tbbt.c

.\tbbt.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dff.c

.\dff.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hextelt.c

.\hextelt.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfsd.c

.\dfsd.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfimcomp.c

.\dfimcomp.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hdf.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\tbbt.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfkconv.c

.\dfkconv.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfsd.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\crle.c

.\crle.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\patchlevel.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfkfuji.c

.\dfkfuji.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\crle.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\herrf.c

.\herrf.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vsfld.c

.\vsfld.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfconv.c

.\dfconv.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfufp2i.c

.\dfufp2i.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfstubs.c

.\dfstubs.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfkvms.c

.\dfkvms.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfufp2i.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfstubs.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\mstdio.c

.\mstdio.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfgroup.c

.\dfgroup.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfp.c

.\dfp.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfutil.c

.\dfutil.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mstdio.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfcomp.c

.\dfcomp.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vio.c

.\vio.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mfgr.c

.\mfgr.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfconvrt.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\herr.c

.\herr.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mfgr.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfgr.c

.\dfgr.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hkit.c

.\hkit.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\df24.c

.\df24.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\herr.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfkcray.c

.\dfkcray.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfgr.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\cnone.c

.\cnone.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hkit.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\vhi.c

.\vhi.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hblocks.c

.\hblocks.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cnone.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\hdfalloc.c

.\hdfalloc.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vrw.c

.\vrw.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mfan.c

.\mfan.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hvblocks.c

.\hvblocks.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vg.c

.\vg.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cnbit.c

.\cnbit.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mfan.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\hdfi.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfan.c

.\dfan.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cskphuff.c

.\cskphuff.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vgp.c

.\vgp.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hconv.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\vg.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\cnbit.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfan.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\cskphuff.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\df.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\hcomp.c

.\hcomp.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfknat.c

.\dfknat.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hproto.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\vparse.c

.\vparse.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfunjpeg.c

.\dfunjpeg.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hcomp.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfi.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\hfile.c

.\hfile.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vproto.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\cgzip.c

.\cgzip.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hfile.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\dfr8.c

.\dfr8.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cgzip.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\hcompi.h
# End Source File
# End Group
# End Project
################################################################################
