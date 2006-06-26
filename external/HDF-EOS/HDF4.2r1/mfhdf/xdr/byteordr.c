/*
 * Example: Use the standard library version instead, if you have one.
 */

/* switch the order of the bytes in a long integer */
long ntohl(i_in)
long i_in;
{
	long i_out;
	register unsigned char *inptr, *outptr;

	inptr = (unsigned char *) &i_in;
	outptr = (unsigned char *) &i_out;

	outptr[3] = inptr[0];
	outptr[2] = inptr[1];
	outptr[1] = inptr[2];
	outptr[0] = inptr[3];

	return(i_out);
}

/* switch the order of the bytes in a long integer */
long htonl(i_in)
long i_in;
{
	long i_out;
	register unsigned char *inptr, *outptr;

	inptr = (unsigned char *) &i_in;
	outptr = (unsigned char *) &i_out;

	outptr[3] = inptr[0];
	outptr[2] = inptr[1];
	outptr[1] = inptr[2];
	outptr[0] = inptr[3];

	return(i_out);
}


/* switch the order of the bytes in a short integer */
short ntohs(i_in)
short i_in;
{
	short i_out;
	register unsigned char *inptr, *outptr;

	inptr = (unsigned char *) &i_in;
	outptr = (unsigned char *) &i_out;

	outptr[1] = inptr[0];
	outptr[0] = inptr[1];

	return(i_out);
}

/* switch the order of the bytes in a short integer */
short htons(i_in)
short i_in;
{
	short i_out;
	register unsigned char *inptr, *outptr;

	inptr = (unsigned char *) &i_in;
	outptr = (unsigned char *) &i_out;

	outptr[1] = inptr[0];
	outptr[0] = inptr[1];

	return(i_out);
}
