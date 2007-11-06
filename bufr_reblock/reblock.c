/*------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:        REBLOCK  - byteswap f77 control words in block file
!
! !INTERFACE:
!
!     reblock blockedfile(big endian) reblockedfile(little endian)
!
! !DESCRIPTION:
!       Read a blocked BUFR file, byteswap the f77 control words
!       and write out the file so that it can be processed on little
!       endian machines
!
! !REVISION HISTORY:
!
! 2002.09.24 Baoyu YIN        Inital release
!
!EOP
!---------------------------------------------------------------------- */

#include <stdio.h>

#define BUFSIZE 12000
int WORDSIZE;

int wrdlen();
int getLen(unsigned char *bufr);
void error(char* str);

int main(int argc, char **argv)
{
   FILE *infid, *outfid;
   int nread;
   int recLen;
   unsigned char databuf[BUFSIZE];
   unsigned char *byteptr, byteval;

   if (argc != 3) {
      error("Usage: blockedfile(big endian) reblockedfile(little endian)");
   }

   WORDSIZE = wrdlen();

   infid=fopen(argv[1],"rb");
   if (infid == NULL) error("Can't read input file");

   outfid=fopen(argv[2],"wb");
   if (outfid == NULL) error("Can't write output file");

   recLen = 0;

   while ((nread=fread(databuf,1,WORDSIZE,infid)) > 0) {

      byteptr=databuf;
      /* byteswap f77 control word before each record */
      byteval=byteptr[0]; byteptr[0]=byteptr[3]; byteptr[3]=byteval;
      byteval=byteptr[1]; byteptr[1]=byteptr[2]; byteptr[2]=byteval;
      if (fwrite(databuf,1,WORDSIZE,outfid)!=nread) error("error in writing bufr");

      /* read the first two bytes to compute record length */
      nread=fread(databuf,1,WORDSIZE*2,infid);
      if (fwrite(databuf,1,WORDSIZE*2,outfid)!=nread) error("error in writing bufr");

      recLen = getLen(databuf);

      /* read the rest of the record */
      nread=fread(databuf,1,(recLen-2)*WORDSIZE,infid);
      if (fwrite(databuf,1,(recLen-2)*WORDSIZE,outfid)!=nread) error("error in writing bufr");

      nread=fread(databuf,1,WORDSIZE,infid);
      byteptr=databuf;
      /* byteswap f77 control word after each record */
      byteval=byteptr[0]; byteptr[0]=byteptr[3]; byteptr[3]=byteval;
      byteval=byteptr[1]; byteptr[1]=byteptr[2]; byteptr[2]=byteval;
      if (fwrite(databuf,1,WORDSIZE,outfid)!=nread) error("error in writing bufr");

   }

   fclose(infid);
   fclose(outfid);
}

/* the record length is stored at the 5th, 6th, and 7th byte in each record */

int getLen(unsigned char *bufr)
{
   int length;
   int byte1=0, byte2=0, byte3=0;

   /* computing the length from 4-6 bytes */
    length = 0;
    byte1 =  bufr[4];
    byte2 =  bufr[5];
    byte3 =  bufr[6];
    length = ( byte1 << 16) | ( byte2 << 8) | ( byte3 ) ;
    length = (1 + length/8 ) * 8 / WORDSIZE;
    return length;
}

/* get the number of bytes per word */

int wrdlen()
{
    int i, one = 1;
    int nbytw;

    for (i=0; i<64; i++) {
       one = one << 1;
       if ( one == 0 ) break;
    }

    nbytw = (i+1)/8;
    return nbytw;
}

void error(char* str)
{
    printf("%s\n", str);
    exit(1);
}
