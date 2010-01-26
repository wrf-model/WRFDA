static char adSid[]="$Id: adStack.c 2158 2007-10-24 11:51:52Z llh $";

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifndef CRAY
# ifdef NOUNDERSCORE
#      define PUSHCHARACTERARRAY  pushcharacterarray
#      define POPCHARACTERARRAY  popcharacterarray
#      define LOOKCHARACTERARRAY  lookcharacterarray
#      define PUSHBOOLEANARRAY  pushbooleanarray
#      define POPBOOLEANARRAY  popbooleanarray
#      define LOOKBOOLEANARRAY  lookbooleanarray

#      define PUSHINTEGER4ARRAY  pushinteger4array
#      define POPINTEGER4ARRAY   popinteger4array
#      define LOOKINTEGER4ARRAY  lookinteger4array
#      define PUSHINTEGER8ARRAY  pushinteger8array
#      define POPINTEGER8ARRAY   popinteger8array
#      define LOOKINTEGER8ARRAY  lookinteger8array
#      define PUSHINTEGER16ARRAY  pushinteger16array
#      define POPINTEGER16ARRAY   popinteger16array
#      define LOOKINTEGER16ARRAY  lookinteger16array

#      define PUSHREAL4ARRAY  pushreal4array
#      define POPREAL4ARRAY   popreal4array
#      define LOOKREAL4ARRAY  lookreal4array
#      define PUSHREAL8ARRAY  pushreal8array
#      define POPREAL8ARRAY   popreal8array
#      define LOOKREAL8ARRAY  lookreal8array
#      define PUSHREAL16ARRAY  pushreal16array
#      define POPREAL16ARRAY   popreal16array
#      define LOOKREAL16ARRAY  lookreal16array
#      define PUSHREAL32ARRAY  pushreal32array
#      define POPREAL32ARRAY   popreal32array
#      define LOOKREAL32ARRAY  lookreal32array

#      define PUSHCOMPLEX4ARRAY  pushcomplex4array
#      define POPCOMPLEX4ARRAY   popcomplex4array
#      define LOOKCOMPLEX4ARRAY  lookcomplex4array
#      define PUSHCOMPLEX8ARRAY  pushcomplex8array
#      define POPCOMPLEX8ARRAY   popcomplex8array
#      define LOOKCOMPLEX8ARRAY  lookcomplex8array
#      define PUSHCOMPLEX16ARRAY  pushcomplex16array
#      define POPCOMPLEX16ARRAY   popcomplex16array
#      define LOOKCOMPLEX16ARRAY  lookcomplex16array
#      define PUSHCOMPLEX32ARRAY  pushcomplex32array
#      define POPCOMPLEX32ARRAY   popcomplex32array
#      define LOOKCOMPLEX32ARRAY  lookcomplex32array
# else
#   ifdef F2CSTYLE
#      define PUSHCHARACTERARRAY  pushcharacterarray__
#      define POPCHARACTERARRAY  popcharacterarray__
#      define LOOKCHARACTERARRAY  lookcharacterarray__
#      define PUSHBOOLEANARRAY  pushbooleanarray__
#      define POPBOOLEANARRAY  popbooleanarray__
#      define LOOKBOOLEANARRAY  lookbooleanarray__

#      define PUSHINTEGER4ARRAY  pushinteger4array__
#      define POPINTEGER4ARRAY   popinteger4array__
#      define LOOKINTEGER4ARRAY  lookinteger4array__
#      define PUSHINTEGER8ARRAY  pushinteger8array__
#      define POPINTEGER8ARRAY   popinteger8array__
#      define LOOKINTEGER8ARRAY  lookinteger8array__
#      define PUSHINTEGER16ARRAY  pushinteger16array__
#      define POPINTEGER16ARRAY   popinteger16array__
#      define LOOKINTEGER16ARRAY  lookinteger16array__

#      define PUSHREAL4ARRAY  pushreal4array__
#      define POPREAL4ARRAY   popreal4array__
#      define LOOKREAL4ARRAY  lookreal4array__
#      define PUSHREAL8ARRAY  pushreal8array__
#      define POPREAL8ARRAY   popreal8array__
#      define LOOKREAL8ARRAY  lookreal8array__
#      define PUSHREAL16ARRAY  pushreal16array__
#      define POPREAL16ARRAY   popreal16array__
#      define LOOKREAL16ARRAY  lookreal16array__
#      define PUSHREAL32ARRAY  pushreal32array__
#      define POPREAL32ARRAY   popreal32array__
#      define LOOKREAL32ARRAY  lookreal32array__

#      define PUSHCOMPLEX4ARRAY  pushcomplex4array__
#      define POPCOMPLEX4ARRAY   popcomplex4array__
#      define LOOKCOMPLEX4ARRAY  lookcomplex4array__
#      define PUSHCOMPLEX8ARRAY  pushcomplex8array__
#      define POPCOMPLEX8ARRAY   popcomplex8array__
#      define LOOKCOMPLEX8ARRAY  lookcomplex8array__
#      define PUSHCOMPLEX16ARRAY  pushcomplex16array__
#      define POPCOMPLEX16ARRAY   popcomplex16array__
#      define LOOKCOMPLEX16ARRAY  lookcomplex16array__
#      define PUSHCOMPLEX32ARRAY  pushcomplex32array__
#      define POPCOMPLEX32ARRAY   popcomplex32array__
#      define LOOKCOMPLEX32ARRAY  lookcomplex32array__
#   else
#      define PUSHCHARACTERARRAY  pushcharacterarray_
#      define POPCHARACTERARRAY  popcharacterarray_
#      define LOOKCHARACTERARRAY  lookcharacterarray_
#      define PUSHBOOLEANARRAY  pushbooleanarray_
#      define POPBOOLEANARRAY  popbooleanarray_
#      define LOOKBOOLEANARRAY  lookbooleanarray_

#      define PUSHINTEGER4ARRAY  pushinteger4array_
#      define POPINTEGER4ARRAY   popinteger4array_
#      define LOOKINTEGER4ARRAY  lookinteger4array_
#      define PUSHINTEGER8ARRAY  pushinteger8array_
#      define POPINTEGER8ARRAY   popinteger8array_
#      define LOOKINTEGER8ARRAY  lookinteger8array_
#      define PUSHINTEGER16ARRAY  pushinteger16array_
#      define POPINTEGER16ARRAY   popinteger16array_
#      define LOOKINTEGER16ARRAY  lookinteger16array_

#      define PUSHREAL4ARRAY  pushreal4array_
#      define POPREAL4ARRAY   popreal4array_
#      define LOOKREAL4ARRAY  lookreal4array_
#      define PUSHREAL8ARRAY  pushreal8array_
#      define POPREAL8ARRAY   popreal8array_
#      define LOOKREAL8ARRAY  lookreal8array_
#      define PUSHREAL16ARRAY  pushreal16array_
#      define POPREAL16ARRAY   popreal16array_
#      define LOOKREAL16ARRAY  lookreal16array_
#      define PUSHREAL32ARRAY  pushreal32array_
#      define POPREAL32ARRAY   popreal32array_
#      define LOOKREAL32ARRAY  lookreal32array_

#      define PUSHCOMPLEX4ARRAY  pushcomplex4array_
#      define POPCOMPLEX4ARRAY   popcomplex4array_
#      define LOOKCOMPLEX4ARRAY  lookcomplex4array_
#      define PUSHCOMPLEX8ARRAY  pushcomplex8array_
#      define POPCOMPLEX8ARRAY   popcomplex8array_
#      define LOOKCOMPLEX8ARRAY  lookcomplex8array_
#      define PUSHCOMPLEX16ARRAY  pushcomplex16array_
#      define POPCOMPLEX16ARRAY   popcomplex16array_
#      define LOOKCOMPLEX16ARRAY  lookcomplex16array_
#      define PUSHCOMPLEX32ARRAY  pushcomplex32array_
#      define POPCOMPLEX32ARRAY   popcomplex32array_
#      define LOOKCOMPLEX32ARRAY  lookcomplex32array_
#   endif
# endif
#endif

#define ONE_BLOCK_SIZE 16384
#ifndef STACK_SIZE_TRACING
#define STACK_SIZE_TRACING 1
#endif
/* The main stack is a double-chain of DoubleChainedBlock objects.
 * Each DoubleChainedBlock holds an array[ONE_BLOCK_SIZE] of char. */
typedef struct _doubleChainedBlock{
  struct _doubleChainedBlock *prev ;
  char                       *contents ;
  struct _doubleChainedBlock *next ;
} DoubleChainedBlock ;

/* Globals that define the current position in the stack: */
static DoubleChainedBlock *curStack = NULL ;
static char               *curStackTop    = NULL ;
/* Globals that define the current LOOKing position in the stack: */
static DoubleChainedBlock *lookStack = NULL ;
static char               *lookStackTop    = NULL ;

static long int mmctraffic = 0 ;
static long int mmctrafficM = 0 ;
#ifdef STACK_SIZE_TRACING
long int bigStackSize = 0;
#endif

/* PUSHes "nbChars" consecutive chars from a location starting at address "x".
 * Resets the LOOKing position if it was active.
 * Checks that there is enough space left to hold "nbChars" chars.
 * Otherwise, allocates the necessary space. */
void pushNarray(char *x, unsigned int nbChars) {
  unsigned int nbmax = (curStack)?ONE_BLOCK_SIZE-(curStackTop-(curStack->contents)):0 ;
#ifdef STACK_SIZE_TRACING
  bigStackSize += nbChars;
#endif

  mmctraffic += nbChars ;
  while (mmctraffic >= 1048576) {
     mmctraffic -= 1048576 ;
     mmctrafficM++ ;
  }

  lookStack = NULL ;
  if (nbChars <= nbmax) {
    memcpy(curStackTop,x,nbChars);
    curStackTop+=nbChars ;
  } else {
    char *inx = x+(nbChars-nbmax) ;
    if (nbmax>0) memcpy(curStackTop,inx,nbmax) ;
    while (inx>x) {
      /* Create new block: */
      if ((curStack == NULL) || (curStack->next == NULL)) {
	DoubleChainedBlock *newStack ;
	char *contents = (char*)malloc(ONE_BLOCK_SIZE*sizeof(char)) ;
	newStack = (DoubleChainedBlock*)malloc(sizeof(DoubleChainedBlock)) ;
	if ((contents == NULL) || (newStack == NULL)) {
	  DoubleChainedBlock *stack = curStack ;
	  int nbBlocks = (stack?-1:0) ;
	  while(stack) {
	      stack = stack->prev ;
	      nbBlocks++ ;
	  }
	  printf("Out of memory (allocated %i blocks of %i bytes)\n",
		 nbBlocks, ONE_BLOCK_SIZE) ;
          exit(0);
	}
	if (curStack != NULL) curStack->next = newStack ;
	newStack->prev = curStack ;
	newStack->next = NULL ;
	newStack->contents = contents ;
	curStack = newStack ;
      } else
	curStack = curStack->next ;
      /* new block created! */
      inx -= ONE_BLOCK_SIZE ;
      if(inx>x)
	memcpy(curStack->contents,inx,ONE_BLOCK_SIZE) ;
      else {
	unsigned int nbhead = (inx-x)+ONE_BLOCK_SIZE ;
	curStackTop = curStack->contents ;
	memcpy(curStackTop,x,nbhead) ;
	curStackTop += nbhead ;
      }
    }
  }
}

/* POPs "nbChars" consecutive chars to a location starting at address "x".
 * Resets the LOOKing position if it was active.
 * Checks that there is enough data to fill "nbChars" chars.
 * Otherwise, pops as many blocks as necessary. */
void popNarray(char *x, unsigned int nbChars) {
  unsigned int nbmax = curStackTop-(curStack->contents) ;
#ifdef STACK_SIZE_TRACING
  bigStackSize -= nbChars;
#endif
  lookStack = NULL ;
  if (nbChars <= nbmax) {
    curStackTop-=nbChars ;
    memcpy(x,curStackTop,nbChars);
  } else {
    char *tlx = x+nbChars ;
    if (nbmax>0) memcpy(x,curStack->contents,nbmax) ;
    x+=nbmax ;
    while (x<tlx) {
      curStack = curStack->prev ;
      if (x+ONE_BLOCK_SIZE<tlx) {
	memcpy(x,curStack->contents,ONE_BLOCK_SIZE) ;
	x += ONE_BLOCK_SIZE ;
      } else {
	unsigned int nbtail = tlx-x ;
	curStackTop=(curStack->contents)+ONE_BLOCK_SIZE-nbtail ;
	memcpy(x,curStackTop,nbtail) ;
	x = tlx ;
      }
    }
  }
}

/* LOOKs "nbChars" consecutive chars to a location starting at address "x".
 * Activates the LOOKing position if it was reset.
 * LOOKing is just like POPping, except that the main pointer
 * remains in place, so that the value is not POPped.
 * Further PUSHs or POPs will start from the same place as if
 * no LOOK had been made. */
void lookNarray(char *x, unsigned int nbChars) {
  unsigned int nbmax ;
  if (lookStack == NULL) {
    lookStack = curStack ;
    lookStackTop = curStackTop ;
  }
  nbmax = lookStackTop-(lookStack->contents) ;
  if (nbChars <= nbmax) {
    lookStackTop-=nbChars ;
    memcpy(x,lookStackTop,nbChars);
  } else {
    char *tlx = x+nbChars ;
    if (nbmax>0) memcpy(x,lookStack->contents,nbmax) ;
    x+=nbmax ;
    while (x<tlx) {
      lookStack = lookStack->prev ;
      if (x+ONE_BLOCK_SIZE<tlx) {
	memcpy(x,lookStack->contents,ONE_BLOCK_SIZE) ;
	x += ONE_BLOCK_SIZE ;
      } else {
	unsigned int nbtail = tlx-x ;
	lookStackTop=(lookStack->contents)+ONE_BLOCK_SIZE-nbtail ;
	memcpy(x,lookStackTop,nbtail) ;
	x = tlx ;
      }
    }
  }
}

/****** Exported PUSH/POP/LOOK functions for ARRAYS: ******/

void PUSHCHARACTERARRAY(char *x, unsigned int *n) {
  pushNarray(x,*n) ;
}
void POPCHARACTERARRAY(char *x, unsigned int *n) {
  popNarray(x,*n) ;
}
void LOOKCHARACTERARRAY(char *x, unsigned int *n) {
  lookNarray(x,*n) ;
}

void PUSHBOOLEANARRAY(char *x, unsigned int *n) {
  pushNarray(x,(*n*4)) ;
}
void POPBOOLEANARRAY(char *x, unsigned int *n) {
  popNarray(x,(*n*4)) ;
}
void LOOKBOOLEANARRAY(char *x, unsigned int *n) {
  lookNarray(x,(*n*4)) ;
}

void PUSHINTEGER4ARRAY(char *x, unsigned int *n) {
  pushNarray(x,(*n*4)) ;
}
void POPINTEGER4ARRAY(char *x, unsigned int *n) {
  popNarray(x,(*n*4)) ;
}
void LOOKINTEGER4ARRAY(char *x, unsigned int *n) {
  lookNarray(x,(*n*4)) ;
}

void PUSHINTEGER8ARRAY(char *x, unsigned int *n) {
  pushNarray(x,(*n*8)) ;
}
void POPINTEGER8ARRAY(char *x, unsigned int *n) {
  popNarray(x,(*n*8)) ;
}
void LOOKINTEGER8ARRAY(char *x, unsigned int *n) {
  lookNarray(x,(*n*8)) ;
}

void PUSHINTEGER16ARRAY(char *x, unsigned int *n) {
  pushNarray(x,(*n*16)) ;
}
void POPINTEGER16ARRAY(char *x, unsigned int *n) {
  popNarray(x,(*n*16)) ;
}
void LOOKINTEGER16ARRAY(char *x, unsigned int *n) {
  lookNarray(x,(*n*16)) ;
}

void PUSHREAL4ARRAY(char *x, unsigned int *n) {
  pushNarray(x,(*n*4)) ;
}
void POPREAL4ARRAY(char *x, unsigned int *n) {
  popNarray(x,(*n*4)) ;
}
void LOOKREAL4ARRAY(char *x, unsigned int *n) {
  lookNarray(x,(*n*4)) ;
}

void PUSHREAL8ARRAY(char *x, unsigned int *n) {
  pushNarray(x,(*n*8)) ;
}
void POPREAL8ARRAY(char *x, unsigned int *n) {
  popNarray(x,(*n*8)) ;
}
void LOOKREAL8ARRAY(char *x, unsigned int *n) {
  lookNarray(x,(*n*8)) ;
}

void PUSHREAL16ARRAY(char *x, unsigned int *n) {
  pushNarray(x,(*n*16)) ;
}
void POPREAL16ARRAY(char *x, unsigned int *n) {
  popNarray(x,(*n*16)) ;
}
void LOOKREAL16ARRAY(char *x, unsigned int *n) {
  lookNarray(x,(*n*16)) ;
}

void PUSHREAL32ARRAY(char *x, unsigned int *n) {
  pushNarray(x,(*n*32)) ;
}
void POPREAL32ARRAY(char *x, unsigned int *n) {
  popNarray(x,(*n*32)) ;
}
void LOOKREAL32ARRAY(char *x, unsigned int *n) {
  lookNarray(x,(*n*32)) ;
}

void PUSHCOMPLEX4ARRAY(char *x, unsigned int *n) {
  pushNarray(x,(*n*4)) ;
}
void POPCOMPLEX4ARRAY(char *x, unsigned int *n) {
  popNarray(x,(*n*4)) ;
}
void LOOKCOMPLEX4ARRAY(char *x, unsigned int *n) {
  lookNarray(x,(*n*4)) ;
}

void PUSHCOMPLEX8ARRAY(char *x, unsigned int *n) {
  pushNarray(x,(*n*8)) ;
}
void POPCOMPLEX8ARRAY(char *x, unsigned int *n) {
  popNarray(x,(*n*8)) ;
}
void LOOKCOMPLEX8ARRAY(char *x, unsigned int *n) {
  lookNarray(x,(*n*8)) ;
}

void PUSHCOMPLEX16ARRAY(char *x, unsigned int *n) {
  pushNarray(x,(*n*16)) ;
}
void POPCOMPLEX16ARRAY(char *x, unsigned int *n) {
  popNarray(x,(*n*16)) ;
}
void LOOKCOMPLEX16ARRAY(char *x, unsigned int *n) {
  lookNarray(x,(*n*16)) ;
}

void PUSHCOMPLEX32ARRAY(char *x, unsigned int *n) {
  pushNarray(x,(*n*32)) ;
}
void POPCOMPLEX32ARRAY(char *x, unsigned int *n) {
  popNarray(x,(*n*32)) ;
}
void LOOKCOMPLEX32ARRAY(char *x, unsigned int *n) {
  lookNarray(x,(*n*32)) ;
}

/************* Debug displays of the state of the stack: ***********/

void printctraffic_() {
    printf(" C Traffic: %6li Mb and %6li millionths\n", mmctrafficM, (((mmctraffic*1000)/1024)*1000)/1024) ;
}

void printtopplace_() {
    DoubleChainedBlock *stack = curStack ;
    int nbBlocks = (stack?-1:0) ;
    int remainder = 0;
    while(stack) {
	stack = stack->prev ;
	nbBlocks++ ;
    }
    if (curStack && curStackTop) remainder = curStackTop-(curStack->contents) ;
    printf(" Stack size: %f Kbytes\n",
           nbBlocks*(ONE_BLOCK_SIZE/1024)+((float)remainder)/1024) ;
}

void printtopplacenum_(int *n) {
    DoubleChainedBlock *stack = curStack ;
    int nbBlocks = (stack?-1:0) ;
    int remainder = 0;
    while(stack) {
	stack = stack->prev ;
	nbBlocks++ ;
    }
    if (curStack && curStackTop) remainder = curStackTop-(curStack->contents) ;
    printf(" Stack size at location %i : %f Kbytes\n",
           *n, nbBlocks*(ONE_BLOCK_SIZE/1024)+((float)remainder)/1024) ;
}

void printstackmax_() {
    DoubleChainedBlock *stack = curStack ;
    int nbBlocks = (stack?-2:0) ;
    int remainder = 0;
    while(stack) {
	stack = stack->prev ;
	nbBlocks++ ;
    }
    stack = curStack ;
    while(stack) {
	stack = stack->next ;
	nbBlocks++ ;
    }
    printf(" Max Stack size: %i blocks of %i bytes => total:%f Mbytes\n",
           nbBlocks,ONE_BLOCK_SIZE,(nbBlocks*(((float)ONE_BLOCK_SIZE)/1024))/1024) ;
}

void printlookingplace_() {
    if (lookStack == NULL)
	printtopplace_() ;
    else {
	DoubleChainedBlock *stack = lookStack ;
	int nbBlocks = (stack?-1:0) ;
	while(stack) {
	    stack = stack->prev ;
	    nbBlocks++ ;
	}
	printf(" Stack look: %i*%i+%li\n",nbBlocks,ONE_BLOCK_SIZE,lookStackTop-(lookStack->contents)) ;
    }
}
