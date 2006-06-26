#include <hdf.h>
#include "gif.h"
#include <string.h>
#include <stdlib.h>

int
WriteHDF(GifMemoryStruct, HDFName , GIFFileName)
GIFTOMEM GifMemoryStruct;
char     *HDFName;
char     *GIFFileName;
{
	GIFHEAD            gifHead;           /* GIF Header structure            */
    GIFIMAGEDESC	   gifImageDesc;      /* Logical Image Descriptor struct */

	
	intn  status;       /* status for functions returning an intn */
	int32 file_id,      /* HDF file identifier */
		gr_id,          /* GR interface identifier */
		ri_id,
		vgroup_id,		/* VGroup interface identifier */
		pal_id,
		start[2],       /* start position to write for each dimension */			
		edges[2],       /* number of elements to be written along each dimension */
		dim_sizes[2],   /* dimension sizes of the image array */
		interlace_mode, /* interlace mode of the image */
		data_type,      /* data type of the image data */
		ImageCount,
		CommentCount,
		ApplicationCount,
		PlainTextCount,
		i;


	/* compression structs */
	uint32 comp_type;	/* compression type */
	comp_info c_info;	/* the compression information for setcompress */

	char ImageName[256];	/* Image name for the GR Image */
	char CommentName[256];
	char ApplicationName[256];
	char PlainTextName[256];
	
	/* reference variables */
	uint32 gr_ref;		/* GR reference for the VGroup */

	/* Setting compression to gzip. If you want to change compression
	** type or params change it here
	*/
	c_info.deflate.level = 6;
	comp_type = COMP_CODE_NONE;
	data_type = DFNT_UINT8;
	interlace_mode = MFGR_INTERLACE_PIXEL;

	/* Get the stuff from the GIFMem data struct */
	gifHead = *(GifMemoryStruct.GifHeader);

	ImageCount = (int32)gifHead.ImageCount;
	CommentCount = (int32)gifHead.CommentCount;
	ApplicationCount = (int32)gifHead.ApplicationCount;
	PlainTextCount = (int32)gifHead.PlainTextCount;

	/*Start HDF file*/
	file_id = Hopen(HDFName, DFACC_CREATE, 0);
	if(file_id == -1) {
		printf("Unable to create HDF file");
		status = HEvalue(1);
		printf(HEstring(status));
		exit(0);
	}

	if ((status = Vstart(file_id)) == -1) {
		printf("Could not start VGroup interface\n");
		printf(HEstring(HEvalue(1)));
		exit(-1);
	}

	vgroup_id = Vattach(file_id , -1 , "w");
	
	status = Vsetname(vgroup_id , GIFFileName);
	
	status = Vsetclass(vgroup_id , "GIF");
	
	
	/* Put the global palette in as an attribute to the vgroup */
	if (gifHead.PackedField & 0x80) {
		status = Vsetattr (vgroup_id, "Global Palette" , DFNT_UINT8, 3 * gifHead.TableSize , (VOIDP)gifHead.HDFPalette);
		if (status) {
			printf("Could not add global palette.\n");
			printf(HEstring(HEvalue(1)));
		}
	}
	for (i = 0 ; i < CommentCount ; i++) {
		sprintf(CommentName , "Comment Extension Data %d" , (int)i);
		status = Vsetattr (vgroup_id , CommentName , DFNT_CHAR8 , (int32)(GifMemoryStruct.GifCommentExtension[i])->DataSize , (VOIDP)(GifMemoryStruct.GifCommentExtension[i])->CommentData);
		free(GifMemoryStruct.GifCommentExtension[i]);
	}
	free(GifMemoryStruct.GifCommentExtension);
	
	for (i = 0 ; i < ApplicationCount ; i++) {
		sprintf(ApplicationName , "Application Extension Data %d", (int)i);
		status = Vsetattr (vgroup_id , ApplicationName , DFNT_CHAR8 , (int32)(GifMemoryStruct.GifApplicationExtension[i])->DataSize , (VOIDP)(GifMemoryStruct.GifApplicationExtension[i])->ApplicationData);
		sprintf(ApplicationName , "Application Extension Dump %d", (int)i);
		status = Vsetattr (vgroup_id , ApplicationName , DFNT_CHAR8 , (int32)11, (VOIDP)(GifMemoryStruct.GifApplicationExtension[i])->AEDump);
	}

	for (i = 0 ; i < PlainTextCount ; i++) {
		sprintf(PlainTextName , "PlainText Extension Data %d", (int)i);
		status = Vsetattr (vgroup_id , PlainTextName , DFNT_CHAR8 , (int32)(GifMemoryStruct.GifPlainTextExtension[i])->DataSize , (VOIDP)(GifMemoryStruct.GifPlainTextExtension[i])->PlainTextData);
		sprintf(PlainTextName , "PlainText Extension Dump %d", (int)i);
		status = Vsetattr (vgroup_id , PlainTextName , DFNT_CHAR8 , (int32)15, (VOIDP)(GifMemoryStruct.GifPlainTextExtension[i])->PTEDump);
	}

	gr_id = GRstart(file_id);
	/* Add GR images into VGroup */
	for(i = 0 ; i < ImageCount ; i++)
	{
		
		gifImageDesc = *(GifMemoryStruct.GifImageDesc[i]);
		
		dim_sizes[0] = gifImageDesc.ImageWidth;
		dim_sizes[1] = gifImageDesc.ImageHeight;
		
		start[0] = start[1] = 0;
		edges[0] = gifImageDesc.ImageWidth;
		edges[1] = gifImageDesc.ImageHeight;
		
		/* Create GR Image */
		sprintf(ImageName,"Image%d",(int)i);
		ri_id = GRcreate (gr_id, ImageName, 1, data_type, interlace_mode, dim_sizes);

		/* GRSetCompress */
		if ((status = GRsetcompress(ri_id, comp_type, &c_info)) == -1) {
			printf("Error occured while setting compression\n");
			printf(HEstring(HEvalue(1)));
			exit(-1);
		}

		/* Write the GR Image */
		if ((status = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)gifImageDesc.Image)) == -1) {
			printf("Error occured while trying to write GR image\n");
			printf(HEstring(HEvalue(1)));
			exit(-1);
		}

		/* Create palette */
		pal_id = GRgetlutid (ri_id , 0);

		if ((status = GRwritelut (pal_id , 3, DFNT_UINT8, interlace_mode, 256, (VOIDP)gifImageDesc.HDFPalette)) == -1) {
			printf("Could not write palette\n");
			printf(HEstring(HEvalue(1)));
			exit(-1);
		}
		
		/* Put both image and palette in VGroup */
		gr_ref = GRidtoref(ri_id);
		
		if ((status = GRendaccess(ri_id)) == -1) {
			printf("Could not terminate GR access\n");
			printf(HEstring(HEvalue(1)));
			exit(-1);
		}

		
		
		/* Adding GR to vgroup */
		if((status = Vaddtagref(vgroup_id,(int32)1965,gr_ref))==-1) {
			printf("Could not add tag to Vgroup");
			printf(HEstring(HEvalue(1)));
		}

		
	}

	/* Terminate GR access */
	if ((status = GRend (gr_id))==-1) {
		printf("Could not end GR access\n");
		printf(HEstring(HEvalue(1)));
		printf("Trying to continue (file may be corrupt)...\n");
	}
	
	/* Terminate access to the VGroup */
	if ((status = Vdetach(vgroup_id))==-1) {
		printf("Could not detach Vgroup\n");
		printf(HEstring(HEvalue(1)));
		printf("Trying to continure (file may be corrupt)...\n");
	}

	/* Terminate access to the V interface */
	if ((status = Vend(file_id))==-1) {
		printf("Could not end VGroup access\n");
		printf(HEstring(HEvalue(1)));
		printf("Trying to continure (file may be corrupt)... \n");
	}

	/* Close the HDF file */
	if ((status = Hclose (file_id))==-1) {
		printf("Could not close HDF file. Fatal Error");
		printf(HEstring(HEvalue(1)));
		return(-1);
	}
	return(0);
}
