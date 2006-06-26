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
!MESSAGE NMAKE /f "nctest.mak" CFG="Win32 Debug"
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

ALL : $(OUTDIR)/nctest.exe $(OUTDIR)/nctest.bsc

# ADD BASE CPP /nologo /ML /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /ML /W3 /GX /O2 /I "..\xdr" /I "..\libsrc" /I "..\..\hdf\src" /I "..\..\hdf\jpeg" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "NO_SYS_XDR_INC" /D "HDF" /c
# SUBTRACT CPP /YX /Fr
CPP_PROJ=/nologo /ML /W3 /GX /O2 /I "..\xdr" /I "..\libsrc" /I "..\..\hdf\src"\
 /I "..\..\hdf\jpeg" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "NO_SYS_XDR_INC" /D\
 "HDF" /Fo$(INTDIR)/ /c 
CPP_OBJS=
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"nctest.bsc" 

$(OUTDIR)/nctest.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
DEF_FILE=$(INTDIR)/nctest.def
LINK32_OBJS= \
	$(INTDIR)/varput.obj \
	$(INTDIR)/emalloc.obj \
	$(INTDIR)/vargetg.obj \
	$(INTDIR)/vartests.obj \
	$(INTDIR)/vardef.obj \
	$(INTDIR)/vputget.obj \
	$(INTDIR)/misctest.obj \
	$(INTDIR)/slabs.obj \
	$(INTDIR)/varget.obj \
	$(INTDIR)/vputgetg.obj \
	$(INTDIR)/driver.obj \
	$(INTDIR)/add.obj \
	$(INTDIR)/rec.obj \
	$(INTDIR)/dimtests.obj \
	$(INTDIR)/cdftests.obj \
	$(INTDIR)/val.obj \
	$(INTDIR)/error.obj \
	$(INTDIR)/varputg.obj \
	$(INTDIR)/atttests.obj
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ..\xdr\win32xdr.lib ..\libsrc\win32cdf.lib ..\..\hdf\src\win32hdf.lib ..\..\hdf\jpeg\win32jpg.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib ..\xdr\win32xdr.lib ..\libsrc\win32cdf.lib\
 ..\..\hdf\src\win32hdf.lib ..\..\hdf\jpeg\win32jpg.lib /NOLOGO\
 /SUBSYSTEM:console /INCREMENTAL:no /PDB:$(OUTDIR)/"nctest.pdb" /MACHINE:I386\
 /DEF:".\nctest.def" /OUT:$(OUTDIR)/"nctest.exe" 

$(OUTDIR)/nctest.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

ALL : $(OUTDIR)/nctest.exe $(OUTDIR)/nctest.bsc

# ADD BASE CPP /nologo /ML /W3 /GX /Zi /YX /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /ML /W3 /GX /Zi /Od /I "..\xdr" /I "..\libsrc" /I "..\..\hdf\src" /I "..\..\hdf\jpeg" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "NO_SYS_XDR_INC" /D "HDF" /c
# SUBTRACT CPP /YX /Fr
CPP_PROJ=/nologo /ML /W3 /GX /Zi /Od /I "..\xdr" /I "..\libsrc" /I\
 "..\..\hdf\src" /I "..\..\hdf\jpeg" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_SYS_XDR_INC" /D "HDF" /Fo$(INTDIR)/ /Fd$(OUTDIR)/"nctest.pdb" /c 
CPP_OBJS=
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"nctest.bsc" 

$(OUTDIR)/nctest.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
DEF_FILE=$(INTDIR)/nctest.def
LINK32_OBJS= \
	$(INTDIR)/varput.obj \
	$(INTDIR)/emalloc.obj \
	$(INTDIR)/vargetg.obj \
	$(INTDIR)/vartests.obj \
	$(INTDIR)/vardef.obj \
	$(INTDIR)/vputget.obj \
	$(INTDIR)/misctest.obj \
	$(INTDIR)/slabs.obj \
	$(INTDIR)/varget.obj \
	$(INTDIR)/vputgetg.obj \
	$(INTDIR)/driver.obj \
	$(INTDIR)/add.obj \
	$(INTDIR)/rec.obj \
	$(INTDIR)/dimtests.obj \
	$(INTDIR)/cdftests.obj \
	$(INTDIR)/val.obj \
	$(INTDIR)/error.obj \
	$(INTDIR)/varputg.obj \
	$(INTDIR)/atttests.obj
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ..\xdr\win32xdr.lib ..\libsrc\win32cdf.lib ..\..\hdf\src\win32hdf.lib ..\..\hdf\jpeg\win32jpg.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib ..\xdr\win32xdr.lib ..\libsrc\win32cdf.lib\
 ..\..\hdf\src\win32hdf.lib ..\..\hdf\jpeg\win32jpg.lib /NOLOGO\
 /SUBSYSTEM:console /INCREMENTAL:yes /PDB:$(OUTDIR)/"nctest.pdb" /DEBUG\
 /MACHINE:I386 /DEF:".\nctest.def" /OUT:$(OUTDIR)/"nctest.exe" 

$(OUTDIR)/nctest.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\varput.c

$(INTDIR)/varput.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\nctest.def
# End Source File
################################################################################
# Begin Source File

SOURCE=.\tests.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\emalloc.c

$(INTDIR)/emalloc.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vargetg.c

$(INTDIR)/vargetg.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\emalloc.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\vartests.c

$(INTDIR)/vartests.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vardef.c

$(INTDIR)/vardef.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vputget.c

$(INTDIR)/vputget.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\misctest.c

$(INTDIR)/misctest.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\slabs.c

$(INTDIR)/slabs.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\varget.c

$(INTDIR)/varget.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\testcdf.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\vputgetg.c

$(INTDIR)/vputgetg.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\driver.c

$(INTDIR)/driver.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\add.c

$(INTDIR)/add.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\add.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\rec.c

$(INTDIR)/rec.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dimtests.c

$(INTDIR)/dimtests.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cdftests.c

$(INTDIR)/cdftests.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\val.c

$(INTDIR)/val.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\val.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\error.c

$(INTDIR)/error.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\error.h
# End Source File
################################################################################
# Begin Source File

SOURCE=.\varputg.c

$(INTDIR)/varputg.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\atttests.c

$(INTDIR)/atttests.obj :  $(SOURCE)  $(INTDIR)

# End Source File
# End Group
# End Project
################################################################################
