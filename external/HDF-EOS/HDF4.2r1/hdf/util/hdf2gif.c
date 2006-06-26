#include "gif.h"
#include <stdio.h>

#define HDFNAME "laser.hdf"
#define VGROUPCLASS "RIG0.0"
#define GIFNAME "temp.gif"

extern int hdfWriteGIF(FILE *fp, BYTE *pic, int ptype, int w, int h, BYTE *rmap,
    BYTE *gmap, BYTE *bmap, BYTE *pc2ncmap, int numcols, int colorstyle, int BitsPerPixel);

int EndianOrder;

VOID
PutByte(b , fpGif)
BYTE b;
FILE *fpGif;
{
	if (fputc(b , fpGif) == EOF) {
		printf("File Writing Error, cannot continue");
		exit(-1);
	}
}

VOID
WordToByte(w , b)
WORD w;
BYTE *b;
{
	if (EndianOrder == 0) /* Big Endian */
	{
		b[0] = w & 0xFF00;
		b[1] = w & 0xFF;
	}
	else /* Little Endian */
	{
		b[0] = w & 0xFF;
		b[1] = w & 0xFF00;
	}
}

VOID
putword(w, fp)
int w;
FILE *fp;
{
	/* writes a 16-bit integer in GIF order (LSB first) */
	
	fputc(w &0xff, fp);
    
	fputc((w>>8)&0xff,fp);
}

int main(int argc , char **argv) {
	
	intn  status;       /* status for functions returning an intn */
	int32 file_id,      /* HDF file identifier */
		gr_id,          /* GR interface identifier */
		ri_id,
		pal_id,
		start[2],       /* start position to write for each dimension */			
		stride[2],
		dim_sizes[2],   /* dimension sizes of the image array */
		interlace_mode, /* interlace mode of the image */
		data_type,      /* data type of the image data */
		i,
		index;
	
	char  gr_name[256];
	
	FILE *fpGif;
	
	int32 ncomp;
	int32 num_attrs;
	int32 num_entries;
	int32 n_images;
	int32 n_fileattributes;
	
	BYTE *Image;
	/* compression structs */
	
	char *HDFName;
	char *GIFName;
	/* reference variables */
	
	int has_local_palette; /* treated as a flag */
	int loop_times; /* number of times to loop, i'm going to treat it as a yes or no */
	
	BYTE* b;
	BYTE  x;
	
	BYTE  GlobalPalette[256][3];
	BYTE  Red[256];
	BYTE  Green[256];
	BYTE  Blue[256];
	
	int   RWidth, RHeight;
	int   LeftOfs, TopOfs;
	int   ColorMapSize, InitCodeSize, Background, BitsPerPixel;
	int   j,nc;
	int	  w,h;
	int   numcols = 256;
	int   CountDown;
	int   curx , cury;
	int   time_out;
	
	BYTE pc2nc[256] , r1[256] , g1[256] , b1[256];
	if (argc < 3) {
		printf("Wrong number of arguments.\nUsage:\nhdf2gif <hdf file> <gif file>\n");
		exit (-1);
	}
	
	HDFName = argv[1];
	GIFName = argv[2];
	/* Set a default 10 ms time between two consequetive images in case of multiple image file */
	if (argc > 3)
		time_out = atoi(argv[3]);
	else
		time_out = 10; 
	
	/* Do Endian Order testing and set Endian Order */
	x = 0x0001;
	b = (BYTE *) &w;
	EndianOrder = (b[0] ? 1:0);
	
	start[0] = start[1] = 0;
	stride[0] = stride[1] = 1;
	
	/*Start HDF file*/
	file_id = Hopen(HDFName, DFACC_READ, 0);
	if(file_id == -1) {
		printf("Unable to open HDF file");
		status = HEvalue(1);
		printf(HEstring(status));
		exit(0);
	}
	
	if (!(fpGif = fopen(GIFName , "wb"))) {
		printf("Error opening gif file for output. Aborting.\n");
		exit (-1);
	}
	
	/* Open the hdf file using the GR interface and retrieve the images from it
	** Note that for now the images have to be 8 bit. If they are 24 bit this 
	** program cannot handle it. I shall try to include it, if not please
	** refer to the file hdf2gif.c in the source code for the remormat utility.
	** There you should find the code to convert 24 bit images to 8 bit (GIF)
	*/
	gr_id = GRstart(file_id);
	
	if ((status = GRfileinfo(gr_id , &n_images , &n_fileattributes)) == -1) {
		status = HEvalue(1);
		printf(HEstring(status));
		exit(0);
	}
	
	if (n_images < 1) {
		printf("Error: No GRimages found in hdf file. Aborting.\n");
		exit (-1);
	}
	
	
	Background = 0;
	for (index = 0 ; index < n_images ; index++) {
		
		has_local_palette = true;
		ri_id = GRselect(gr_id , index);
		if ((pal_id = GRgetlutid(ri_id , 0)) == -1)
			has_local_palette = false;
		
		if (has_local_palette) {
			status = GRgetlutinfo(pal_id , &ncomp , &data_type , &interlace_mode , &num_entries);
			status = GRreadlut(pal_id , (VOIDP)&GlobalPalette);
		}
		
		status = GRgetiminfo(ri_id , gr_name , &ncomp , &data_type , &interlace_mode , dim_sizes , &num_attrs);
		if (!(data_type == DFNT_CHAR || data_type == DFNT_UCHAR || data_type == DFNT_INT8 || data_type == DFNT_UINT8 || data_type == DFNT_NINT8 || data_type == DFNT_NUINT8)) {
			printf("The GR data type of image %s in the hdf file appears not to be 8-bit. Trying next image...\n", gr_name);
			continue;
		}
		
		/* BUG FIX 601 - pkamat */
		if (1 != ncomp) {  /* not an 8-bit image */ 
		  if (3 == ncomp) { /* 24-bit image */
		    printf("The GR data type of image %s in the hdf file appears to be a 24-bit image. ", gr_name);
		    printf("Use hdf2jpeg to convert this image. Trying next image... \n");
		    continue;
                  }
		  printf("The GR data type of image %s in the hdf file does not appear to be a 8-bit image. ", gr_name);
		  printf("Trying next image... \n");	
		  continue;
		}	
		/* End BUG FIX 601 */ 
		
		Image = (BYTE *)malloc(dim_sizes[0] * dim_sizes[1]);
		status = GRreadimage(ri_id , start , stride , dim_sizes , Image);
		w = dim_sizes[0];
		h = dim_sizes[1];
		
		/* If the first image does not have a palette, I make my own global color table
		** Obviously this is not the best thing to do, better steps would be:
		** 1. Check for either a global palette or a global attribute called palette
		** 2. Check for palettes in any of the other images.
		*/
		if (!has_local_palette) {
			for (i = 0 ; i < 256 ; i++) {
				Red[i] = 255 - i;
				Green[i] = 255 - i;
				Blue[i] = 255 - i;
			}
		}
		else {
			for (i = 0 ; i < 256 ; i++){
				Red[i] = GlobalPalette[i][0];
				Green[i] = GlobalPalette[i][1];
				Blue[i] = GlobalPalette[i][2];
			}
		}
		
		for (i=0; i<256; i++) { pc2nc[i] = r1[i] = g1[i] = b1[i] = 0; }
		/* compute number of unique colors */
		nc = 0;
		for (i=0; i<numcols; i++) {
			/* see if color #i is already used */
			for (j=0; j<i; j++) {
				if (Red[i] == Red[j] && Green[i] == Green[j] && 
					Blue[i] == Blue[j]) break;
			}
			if (j==i) {  /* wasn't found */
				pc2nc[i] = nc;
				r1[nc] = Red[i];
				g1[nc] = Green[i];
				b1[nc] = Blue[i];
				nc++;
			}
			else pc2nc[i] = pc2nc[j];
		}
		/* figure out 'BitsPerPixel' */
		for (i=1; i<8; i++) {
			if ( (1<<i) >= nc) break;
		}
		BitsPerPixel = i;
		ColorMapSize = 1 << BitsPerPixel;
		
		RWidth  = dim_sizes[0];
		RHeight = dim_sizes[1];
		LeftOfs = TopOfs = 0;
		
		CountDown = w * h;    /* # of pixels we'll be doing */
		
		if (BitsPerPixel <= 1) InitCodeSize = 2;
		else InitCodeSize = BitsPerPixel;
		
		curx = cury = 0;
		
		if (!fpGif) {
			fprintf(stderr,  "WriteGIF: file not open for writing\n" );
			return (1);
		}
		
		/* If it is the first image we do all the header stuff that isn't required for the
		** rest of the images. 
        */
		if (index == 0) {
			/* Write out the GIF header and logical screen descriptor */
			if (n_images > 0) {
				fwrite("GIF89a", 1, 6, fpGif);    /* the GIF magic number */
				loop_times = 0;
			}
			else {
				fwrite("GIF87a", 1, 6, fpGif);    /* the GIF magic number */
				loop_times = 1;
			}
			
			putword(RWidth, fpGif);           /* screen descriptor */
			putword(RHeight, fpGif);
			
			i = 0x00;	                 /* No, there is no color map */
			i |= (8-1)<<4;                 /* OR in the color resolution (hardwired 8) */
			i |= (BitsPerPixel - 1);       /* OR in the # of bits per pixel */
			fputc(i,fpGif);          
			
			fputc(Background,fpGif);         /* background color */
			
			fputc(0, fpGif);                  /* future expansion byte */
			
			
			/* If loop_times is 0 , put in the application extension to make the gif anime loop
			** indefinitely
			*/
			if (!loop_times) {
				fputc(0x21 , fpGif);
				fputc(0xFF , fpGif);
				fputc(11 , fpGif);
				fwrite("NETSCAPE2.0" , 1 , 11 , fpGif);
				fputc(3 , fpGif);
				fputc(1 , fpGif);
				fputc(0 , fpGif);
				fputc(0 , fpGif);
				fputc(0 , fpGif);
				
			}
			
			
		}
		
		if (n_images > 1) {
			/* write a graphic control block */
			fputc(0x21 , fpGif);
			fputc(0xF9 , fpGif);
			fputc(4 , fpGif);
			fputc(4 , fpGif);
			putword(time_out , fpGif);
			fputc(255, fpGif);
			fputc(0 , fpGif);
		}
		
		/* Put Image Descriptor
		** Hardwiring Left Offset and Top Offset to 0x00
		*/
		
		fputc   (0x2c , fpGif);
		putword (0x00 , fpGif);
		putword (0x00  , fpGif);
		putword (RWidth   , fpGif);
		putword (RHeight  , fpGif);
		if (has_local_palette) {
			fputc   ((0x80 | (BitsPerPixel - 1)) , fpGif);
			for (i=0; i<ColorMapSize; i++) {       /* write out Global colormap */
				fputc(r1[i], fpGif);
				fputc(g1[i], fpGif);
				fputc(b1[i], fpGif);
			}
		}
		else 
			fputc (0x00 , fpGif);
		
		fputc (InitCodeSize , fpGif);
		
		i = hdfWriteGIF(fpGif , Image , 0 , dim_sizes[0] , dim_sizes[1] , r1, g1 , b1 , pc2nc , 256 , 8 , BitsPerPixel);
		fputc(0x00 , fpGif);		
		free (Image);
		status = GRendaccess (ri_id);
	} 
	
	fputc(';',fpGif);                    /* Write GIF file terminator */
	status = GRend (gr_id);
	
	/* Close the HDF file */
	status = Hclose (file_id);
	fclose (fpGif);
	return(0);
}
