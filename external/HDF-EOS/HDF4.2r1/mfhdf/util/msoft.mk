#  Copyright 1989, University Corporation for Atmospheric Research
#
#	DOS and OS/2 makefile for utility routine used by the netCDF
#

CC     = cl
CFLAGS = /c /AL /Za

OBJS = getopt.obj

all:	getopt.obj

getopt.obj: getopt.c
	$(CC) $(CFLAGS) getopt.c

test:
install:

clean:
	rm -f *.bak getopt.obj

