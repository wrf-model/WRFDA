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
!MESSAGE NMAKE /f "win32tst.mak" CFG="Win32 Debug"
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

ALL : $(OUTDIR)/testhdf.exe $(OUTDIR)/testhdf.bsc

# ADD BASE CPP /nologo /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\src" /I "..\jpeg" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /c
# SUBTRACT CPP /YX /Fr
CPP_PROJ=/nologo /W3 /GX /O2 /I "..\src" /I "..\jpeg" /D "WIN32" /D "NDEBUG" /D\
 "_CONSOLE" /Fo$(INTDIR)/ /c 
CPP_OBJS=
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"testhdf.bsc" 

$(OUTDIR)/testhdf.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/tvset.obj \
	$(INTDIR)/man.obj \
	$(INTDIR)/vers.obj \
	$(INTDIR)/blocks.obj \
	$(INTDIR)/tree.obj \
	$(INTDIR)/testhdf.obj \
	$(INTDIR)/rig.obj \
	$(INTDIR)/sdmms.obj \
	$(INTDIR)/an.obj \
	$(INTDIR)/bitio.obj \
	$(INTDIR)/vblocks.obj \
	$(INTDIR)/sdstr.obj \
	$(INTDIR)/conv.obj \
	$(INTDIR)/litend.obj \
	$(INTDIR)/file1.obj \
	$(INTDIR)/sdnmms.obj \
	$(INTDIR)/extelt.obj \
	$(INTDIR)/nbit.obj \
	$(INTDIR)/comp.obj \
	$(INTDIR)/anfile.obj \
	$(INTDIR)/file.obj \
	$(INTDIR)/mgr.obj \
	$(INTDIR)/slab.obj
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ..\src\win32hdf.lib ..\jpeg\win32jpg.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib ..\src\win32hdf.lib ..\jpeg\win32jpg.lib /NOLOGO\
 /SUBSYSTEM:console /INCREMENTAL:no /PDB:$(OUTDIR)/"testhdf.pdb" /MACHINE:I386\
 /OUT:$(OUTDIR)/"testhdf.exe" 

$(OUTDIR)/testhdf.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

ALL : $(OUTDIR)/testhdf.exe $(OUTDIR)/testhdf.bsc

# ADD BASE CPP /nologo /W3 /GX /Zi /YX /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /W3 /GX /Z7 /Od /I "..\src" /I "..\jpeg" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /c
# SUBTRACT CPP /YX /Fr
CPP_PROJ=/nologo /W3 /GX /Z7 /Od /I "..\src" /I "..\jpeg" /D "WIN32" /D\
 "_DEBUG" /D "_CONSOLE" /Fo$(INTDIR)/ /c 
CPP_OBJS=
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"testhdf.bsc" 

$(OUTDIR)/testhdf.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/tvset.obj \
	$(INTDIR)/man.obj \
	$(INTDIR)/vers.obj \
	$(INTDIR)/blocks.obj \
	$(INTDIR)/tree.obj \
	$(INTDIR)/testhdf.obj \
	$(INTDIR)/rig.obj \
	$(INTDIR)/sdmms.obj \
	$(INTDIR)/an.obj \
	$(INTDIR)/bitio.obj \
	$(INTDIR)/vblocks.obj \
	$(INTDIR)/sdstr.obj \
	$(INTDIR)/conv.obj \
	$(INTDIR)/litend.obj \
	$(INTDIR)/file1.obj \
	$(INTDIR)/sdnmms.obj \
	$(INTDIR)/extelt.obj \
	$(INTDIR)/nbit.obj \
	$(INTDIR)/comp.obj \
	$(INTDIR)/anfile.obj \
	$(INTDIR)/file.obj \
	$(INTDIR)/mgr.obj \
	$(INTDIR)/slab.obj
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ..\src\win32hdf.lib ..\jpeg\win32jpg.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib ..\src\win32hdf.lib ..\jpeg\win32jpg.lib /NOLOGO\
 /SUBSYSTEM:console /INCREMENTAL:yes /PDB:$(OUTDIR)/"testhdf.pdb" /DEBUG\
 /MACHINE:I386 /OUT:$(OUTDIR)/"testhdf.exe" 

$(OUTDIR)/testhdf.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\tvset.c

$(INTDIR)/tvset.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\man.c

$(INTDIR)/man.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vers.c

$(INTDIR)/vers.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\blocks.c

$(INTDIR)/blocks.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\tree.c

$(INTDIR)/tree.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\testhdf.c

$(INTDIR)/testhdf.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\rig.c

$(INTDIR)/rig.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\sdmms.c

$(INTDIR)/sdmms.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\tutils.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\an.c

$(INTDIR)/an.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\bitio.c

$(INTDIR)/bitio.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vblocks.c

$(INTDIR)/vblocks.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\sdstr.c

$(INTDIR)/sdstr.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\conv.c

$(INTDIR)/conv.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\litend.c

$(INTDIR)/litend.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\file1.c

$(INTDIR)/file1.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\sdnmms.c

$(INTDIR)/sdnmms.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\extelt.c

$(INTDIR)/extelt.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\nbit.c

$(INTDIR)/nbit.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp.c

$(INTDIR)/comp.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\anfile.c

$(INTDIR)/anfile.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\file.c

$(INTDIR)/file.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mgr.c

$(INTDIR)/mgr.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\tproto.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\slab.c

$(INTDIR)/slab.obj :  $(SOURCE)  $(INTDIR)

# End Source File
# End Group
# End Project
################################################################################
