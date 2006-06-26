OS2     = 0
CC        = cl
#CFLAGS    = /c /AL /Za
CFLAGS    = /DPC /DHDF /c /Gt64 /AH /W4 $(IFLAGS) /Zp1 /Zl
#CFLAGS = /c /Gt64 /AH /W4 /DPC /DHDF /Ox /Mq /DTEST_WIN
#CFLAGS=  /D$(MACHINE) /Gt64 /AH /W4 /Ozaxb2 $(IFLAGS) /Zp1 /Zl

F77   = fl
#FFLAGS    = /c /AL
FFLAGS= /c /Gt64 /AH /W2 /Od /Tf

AR        = lib
ARFLAGS   =

ASM       = masm

FORT = 1
