/****************************************************************************\
**  Title:       GIF.H                                                      **
**  Purpose:     GIF Header file                                            **
**  Version:     1.0                                                        **
**  Date:        March 1992                                                 **
**  Author:      James D. Murray, Anaheim, CA, USA                          **
**  C Compilers: Borland C++ v2.0, Microsoft C v6.00a                       **
**                                                                          **
**  This file contains the header structures for the GIF image              **
**  file format.                                                            **
**                                                                          **
**                                                                          **
**  Copyright (C) 1991 by Graphics Software Labs.  All rights reserved.     **
\****************************************************************************/

#include "hdf.h"
#ifndef GIF_H
#define GIF_H   1

#define MAX_PAL 768

/*#include "datatype.h"         Data type definitions */


typedef uint8  BYTE;
typedef uint16 WORD;
typedef char   CHAR;
typedef uint8  boolean;

#define false 0;
#define true 1;

/* Set the EndianOrder.
** The GIF Reader file should do this.
** Set EndianOrder = 0 if machine is little endian
**     EndianOrder = 1 if machine is big endian.
*/
extern int  EndianOrder;	
                                                            
/*
**  The GIF header format.
**
**  This structure actually contains the header, logical screen
**  descriptor, and the global color table for the GIF image.
*/
typedef struct _GifHeader       /* Offset   Description            */
{
    BYTE        PackedField;    /*  0Ah     Color Information      */
	WORD		TableSize;
	BYTE        ImageCount;     /*  Keep a count of the number of images	*/
	BYTE		CommentCount;
	BYTE		ApplicationCount;
	BYTE		PlainTextCount;
	BYTE		HDFPalette[256][3];
	BYTE		HeaderDump[6];	/*	BYTE array to dump header contents		*/	
	BYTE		LSDDump[7];		/*	Logical Screen Descriptor dump			*/
} GIFHEAD;                      


/*
**  The GIF Image Descriptor.
*/
typedef struct _GifImageDescriptor
{
    WORD        ImageWidth;         /* Width of the image in pixels           */
    WORD        ImageHeight;        /* Height of the image in pixels          */
    BYTE        PackedField;        /* Image and Color Table Data Information */
	WORD		TableSize;
	WORD		CodeSize;			/* Minimum LZW CodeSize for image data    */
	BYTE		HDFPalette[256][3];
	BYTE		GIDDump[9];			/* GifImageDescriptor dump				  */

	BYTE		*Image;				/* Decompressed Raster Image			  */
	BYTE 	    *GIFImage;
} GIFIMAGEDESC;


/*
**  GIF 89a Graphic Control Extension Block
*/
typedef struct _GifGraphicControlExtension
{
	BYTE	GCEDump[5];			/* Graphic Control Extension Dump		*/
} GIFGRAPHICCONTROL;


/*
**  GIF 89a Plain Text Extension Block
*/
typedef struct _GifPlainTextExtension
{
	BYTE	PTEDump[15];		/* Plain Text Extension Dump			*/
    BYTE   *PlainTextData;      /* Plain Text data sub-blocks           */
	WORD	DataSize;
} GIFPLAINTEXT;


/*
**  GIF 89a Application Extension Block
*/
typedef struct _GifApplicationExtension
{
	BYTE	AEDump[14];			/* Application Extension Dump			*/
    BYTE   *ApplicationData;    /* Application data sub-blocks          */
	WORD	DataSize;
} GIFAPPLICATION;

/*
**  GIF 89a Comment Extension Block
*/
typedef struct _GifCommentExtension
{
	BYTE	CEDump[2];			/* Comment Extension Dump				*/
    BYTE   *CommentData;        /* Comment data sub-blocks              */
	WORD	DataSize;
    BYTE    Terminator;         /* Block Terminator (always 0)          */
} GIFCOMMENT;

/*
** GIF to HDF Memory Struct
** Purpose : The gif to hdf structure is used to pass all the 
**           gif data to the memory, which gets caught by the hdf driver
**           Its the drivers job to put the data in the appropriate places 
**           in the HDF file.
**           I have assumed that the ImageDescriptors and GraphicControls follow
**           one another, ie. I have not associated them with each other. The driver
**           must assume a 1-1 correspondance. The same discussion with plain text
**           extension.
*/
typedef struct _GifToMem
{
	GIFHEAD            *GifHeader;
	GIFIMAGEDESC      **GifImageDesc;
	GIFGRAPHICCONTROL **GifGraphicControlExtension;
	GIFPLAINTEXT      **GifPlainTextExtension;
	GIFAPPLICATION    **GifApplicationExtension;
	GIFCOMMENT        **GifCommentExtension;
} GIFTOMEM;


/*
**  Function Prototypes
*/
/* GIF2MEM.C */
GIFTOMEM Gif2Mem(BYTE *);

/* GIFREAD.C */
int ReadGifHeader(GIFHEAD *, BYTE **);
int ReadGifImageDesc(GIFIMAGEDESC *, BYTE **);
int ReadGifGraphicControl(GIFGRAPHICCONTROL *, BYTE **);
int ReadGifPlainText(GIFPLAINTEXT *, BYTE **);
int ReadGifApplication(GIFAPPLICATION *, BYTE **);
int ReadGifComment(GIFCOMMENT *, BYTE **);

/* WRITEHDF.C */
int WriteHDF(GIFTOMEM , CHAR * , CHAR *);

BYTE *ReadDataSubBlocks(BYTE ** , WORD *);
BYTE *Decompress (GIFIMAGEDESC * , GIFHEAD *);
BYTE GetByte(BYTE *);
WORD GetWord(BYTE *);
#endif  /* GIF_H */

