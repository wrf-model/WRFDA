#include "tproto.h"

#define INT16MAX 32767 /* 0x7fff */

#define UINT16MAX 65535 /* 0xffff */

#define INT32MAX 2147483647 /* 0x7fffffff */

#define UINT32MAX 4294967295UL /* 0xffffffff */

extern int Verbocity;

void
test_macros(void)
{

	signed char *p;
	uint16 new16u, old16u, str16u;
	int16 new16, old16, str16;
	uint32 new32u, old32u, str32u;
	int32 new32, old32, str32;
	int i,j;
	int errors1, errors2, errors3, errors4;
	int errors = 0;

	uint16 data1[10] = {0,1,2,3,4,UINT16MAX-4,UINT16MAX-3,UINT16MAX-2,UINT16MAX-1,UINT16MAX};

	int16 data2[20] = {-INT16MAX-1,-INT16MAX,-INT16MAX+1,-INT16MAX+2,-INT16MAX+3,-INT16MAX+4,
                          -4,-3,-2,-1,0,1,2,3,4,
                          INT16MAX-4,INT16MAX-3,INT16MAX-2,INT16MAX-1,INT16MAX};

	uint32 data3[10] = {0,1,2,3,4,UINT32MAX-4,UINT32MAX-3,UINT32MAX-2,UINT32MAX-1,UINT32MAX};

	int32 data4[20] = {-INT32MAX-1,-INT32MAX,-INT32MAX+1,-INT32MAX+2,-INT32MAX+3,-INT32MAX+4,
                          -4,-3,-2,-1,0,1,2,3,4,
                          INT32MAX-4,INT32MAX-3,INT32MAX-2,INT32MAX-1,INT32MAX};

	if (Verbosity > 5) printf("\n");
	errors1 = 0;
	for (j = 0; j < 10; j++ ){
		old16u = data1[j];
		p = (signed char *) &str16u;
		UINT16ENCODE(p, old16u);
		p = (signed char *) &str16u;
		UINT16DECODE(p, new16u);

		if ( old16u != new16u ) {
			if (Verbosity > 8) {
				printf("old16u = %d, %x\n", old16u, old16u);
				printf("str: ");
				p = (signed char *) &str16u;
				for (i=0; i<sizeof(uint16); i++){
					printf("%x ", 0xff & *p++);
				}
				printf("\n");
				printf("new16u = %d, %x\n", new16u, new16u);
				printf("\n");
			}
			errors1++;
		}
	}
	if (Verbosity > 5) {
		if (errors1 == 0) {
			printf("UNSIGNED INTEGER16: SUCCESSFUL\n");
		} else {
			printf("UNSIGNED INTEGER16: %d ERRORS\n",errors1);
		}
	}

	if (Verbosity > 5) printf("\n");
	errors2 = 0;
	for (j = 0; j < 20; j++ ){
		old16 = data2[j];
		p = (signed char *) &str16;
		INT16ENCODE(p, old16);
		p = (signed char *) &str16;
		INT16DECODE(p, new16);

		if ( old16 != new16 ) {
			if (Verbosity > 8) {
				printf("old16 = %d, %x\n", old16, old16);
				printf("str: ");
				p = (signed char *) &str16;
				for (i=0; i<sizeof(int16); i++){
					printf("%x ", 0xff & *p++);
				}
				printf("\n");
				printf("new16 = %d, %x\n", new16, new16);
				printf("\n");
			}
			errors2++;
		}
	}
	if (Verbosity > 5) {
		if (errors2 == 0) {
			printf("SIGNED INTEGER16: SUCCESSFUL\n");
		} else {
			printf("SIGNED INTEGER16: %d ERRORS\n",errors2);
		}
	}

	if (Verbosity > 5) printf("\n");
	errors3 = 0;
	for (j = 0; j < 10; j++ ){
		old32u = data3[j];
		p = (signed char *) &str32u;
		UINT32ENCODE(p, old32u);
		p = (signed char *) &str32u;
		UINT32DECODE(p, new32u);

		if ( old32u != new32u ) {
			if (Verbosity > 8) {
				printf("old32u = %u, %x\n", (unsigned)old32u, (unsigned)old32u);
				printf("str: ");
				p = (signed char *) &str32u;
				for (i=0; i<sizeof(uint32); i++){
					printf("%x ", 0xff & *p++);
				}
				printf("\n");
				printf("new32u = %u, %x\n", (unsigned)new32u, (unsigned)new32u);
				printf("\n");
			}
			errors3++;
		}
	}
	if (Verbosity > 5) {
		if (errors3 == 0) {
			printf("UNSIGNED INTEGER32: SUCCESSFUL\n");
		} else {
			printf("UNSIGNED INTEGER32: %d ERRORS\n",errors3);
		}
	}

	if (Verbosity > 5) printf("\n");
	errors4 = 0;
	for (j = 0; j < 20; j++ ){
		old32 = data4[j];
		p = (signed char *) &str32;
		INT32ENCODE(p, old32);
		p = (signed char *) &str32;
		INT32DECODE(p, new32);

		if ( old32 != new32 ) {
			if (Verbosity > 8) {
				printf("old32 = %d, %x\n", (int)old32, (unsigned)old32);
				printf("str: ");
				p = (signed char *) &str32;
				for (i=0; i<sizeof(int32); i++){
					printf("%x ", 0xff & *p++);
				}
				printf("\n");
				printf("new32 = %d, %x\n", (int)new32, (unsigned)new32);
				printf("\n");
			}
			errors4++;
		}
	}
	if (Verbosity > 5) {
		if (errors4 == 0) {
			printf("SIGNED INTEGER32: SUCCESSFUL\n");
		} else {
			printf("SIGNED INTEGER32: %d ERRORS\n",errors4);
		}
	}

	errors = errors1+errors2+errors3+errors4;
	if (errors > 0) {
		printf("            %d ERRORS were detected during (macros) Testing\n",errors);
		num_errs += errors;
	}


}   /* end test_macros() */


