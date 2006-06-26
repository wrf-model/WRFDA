# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "hdp.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

################################################################################
# Begin Project
# PROP Target_Last_Scanned "Win32 Debug"
CPP=cl.exe
RSC=rc.exe

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

ALL : .\hdp.exe .\hdp.bsc

# ADD BASE CPP /nologo /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\xdr" /I "..\libsrc" /I "..\..\hdf\src" /I "..\..\hdf\jpeg" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "HDF" /D "NO_SYS_XDR_INC" /c
# SUBTRACT CPP /YX /Fr
CPP_PROJ=/nologo /W3 /GX /O2 /I "..\xdr" /I "..\libsrc" /I "..\..\hdf\src" /I\
 "..\..\hdf\jpeg" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "HDF" /D\
 "NO_SYS_XDR_INC" /Fo$(INTDIR)/ /c 
CPP_OBJS=
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"hdp.bsc" 

.\hdp.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
DEF_FILE=
LINK32_OBJS= \
	.\hdp_sds.obj \
	.\hdp_list.obj \
	.\hdp.obj \
	.\hdp_util.obj \
	.\hdp_vg.obj \
	.\show.obj \
	.\hdp_vd.obj \
	.\hdp_rig.obj \
	.\hdp_dump.obj
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ..\xdr\win32xdr.lib ..\libsrc\win32cdf.lib ..\..\hdf\src\win32hdf.lib ..\..\hdf\jpeg\win32jpg.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib ..\xdr\win32xdr.lib ..\libsrc\win32cdf.lib\
 ..\..\hdf\src\win32hdf.lib ..\..\hdf\jpeg\win32jpg.lib /NOLOGO\
 /SUBSYSTEM:console /INCREMENTAL:no /PDB:$(OUTDIR)/"hdp.pdb" /MACHINE:I386\
 /OUT:$(OUTDIR)/"hdp.exe" 

.\hdp.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
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

ALL : .\hdp.exe .\hdp.bsc

# ADD BASE CPP /nologo /W3 /GX /Zi /YX /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /W3 /GX /Zi /Od /I "..\xdr" /I "..\libsrc" /I "..\..\hdf\src" /I "..\..\hdf\jpeg" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "HDF" /D "NO_SYS_XDR_INC" /c
# SUBTRACT CPP /YX /Fr
CPP_PROJ=/nologo /W3 /GX /Zi /Od /I "..\xdr" /I "..\libsrc" /I "..\..\hdf\src"\
 /I "..\..\hdf\jpeg" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "HDF" /D\
 "NO_SYS_XDR_INC" /Fo$(INTDIR)/ /Fd$(OUTDIR)/"hdp.pdb" /c 
CPP_OBJS=
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"hdp.bsc" 

.\hdp.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
DEF_FILE=
LINK32_OBJS= \
	.\hdp_sds.obj \
	.\hdp_list.obj \
	.\hdp.obj \
	.\hdp_util.obj \
	.\hdp_vg.obj \
	.\show.obj \
	.\hdp_vd.obj \
	.\hdp_rig.obj \
	.\hdp_dump.obj
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ..\xdr\win32xdr.lib ..\libsrc\win32cdf.lib ..\..\hdf\src\win32hdf.lib ..\..\hdf\jpeg\win32jpg.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib ..\xdr\win32xdr.lib ..\libsrc\win32cdf.lib\
 ..\..\hdf\src\win32hdf.lib ..\..\hdf\jpeg\win32jpg.lib /NOLOGO\
 /SUBSYSTEM:console /INCREMENTAL:yes /PDB:$(OUTDIR)/"hdp.pdb" /DEBUG\
 /MACHINE:I386 /OUT:$(OUTDIR)/"hdp.exe" 

.\hdp.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
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

SOURCE=.\hdp_sds.c

.\hdp_sds.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hdp_list.c

.\hdp_list.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hdp.c

.\hdp.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hdp.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\hdp_util.c

.\hdp_util.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hdp_vg.c

.\hdp_vg.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\show.c

.\show.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hdp_vd.c

.\hdp_vd.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hdp_rig.c

.\hdp_rig.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hdp_dump.c

.\hdp_dump.obj :  $(SOURCE)  $(INTDIR)

# End Source File
# End Group
# End Project
################################################################################
