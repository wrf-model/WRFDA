# include <stdio.h>
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX 200
# define output(c) (void)putc(c,yyout)
#if defined(__cplusplus) || defined(__STDC__)
#if defined(__cplusplus) && defined(__EXTERN_C__)
extern "C" {
#endif
	int yyback(int *, int);
	int yyinput(void);
	int yylook(void);
	void yyoutput(int);
	int yyracc(int);
	int yyreject(void);
	void yyunput(int);
	int yylex(void);
#ifndef yyless
	void yyless(int);
#endif
#ifndef yywrap
	int yywrap(void);
#endif
#ifdef LEXDEBUG
	void allprint(char);
	void sprint(char *);
#endif
#if defined(__cplusplus) && defined(__EXTERN_C__)
}
#endif
#endif
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO (void)fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;

# line 3 "ncgen.l"
/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: msoftyy.c,v 1.4 1996/03/26 22:39:38 georgev Exp $
 *********************************************************************/

#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)


# line 11 "ncgen.l"
/* lex specification for tokens for ncgen */

char errstr[100];		/* for short error messages */
extern long strtol();
void expand_escapes();

#include <string.h>
#include <ctype.h>
#include <limits.h>
#include "msofttab.h"
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:

# line 26 "ncgen.l"
	/* comment */ ;
break;
case 2:

# line 28 "ncgen.l"
	{
			 if(yyleng > MAXTRST) {
				yyerror("string too long, truncated\n");
			        yytext[MAXTRST-1] = '\0';
			 }
			 expand_escapes(termstring,yytext,yyleng);
		 	 return (TERMSTRING);
		        }
break;
case 3:

# line 37 "ncgen.l"
{return (FLOAT_K);}
break;
case 4:

# line 38 "ncgen.l"
	{return (CHAR_K);}
break;
case 5:

# line 39 "ncgen.l"
	{return (BYTE_K);}
break;
case 6:

# line 40 "ncgen.l"
	{return (SHORT_K);}
break;
case 7:

# line 41 "ncgen.l"
{return (LONG_K);}
break;
case 8:

# line 42 "ncgen.l"
	{return (DOUBLE_K);}
break;
case 9:

# line 43 "ncgen.l"
{long_val = -1;
			 return (NC_UNLIMITED_K);}
break;
case 10:

# line 46 "ncgen.l"
{return (DIMENSIONS);}
break;
case 11:

# line 47 "ncgen.l"
{return (VARIABLES);}
break;
case 12:

# line 48 "ncgen.l"
	{return (DATA);}
break;
case 13:

# line 49 "ncgen.l"
{
		char *s = (char*)yytext+strlen("netcdf");
		char *t = (char*)yytext+yyleng-1;
		while (isspace(*s))
			s++;
		while (isspace(*t))
			t--;
		t++;
		netcdfname = (char *) emalloc(t-s+1);
		(void) strncpy(netcdfname, s, t-s);
		netcdfname[t-s] = '\0';
		return (NETCDF);
		}
break;
case 14:

# line 63 "ncgen.l"
{    /* double missing values */
		double_val = FILL_DOUBLE;  /* IEEE double infinity */
		return (DOUBLE_CONST);
		}
break;
case 15:

# line 68 "ncgen.l"
{    /* float missing values */
		float_val = FILL_FLOAT;  /* IEEE float infinity */
		return (FLOAT_CONST);
		}
break;
case 16:

# line 72 "ncgen.l"
{
		if ((yylval = lookup(yytext)) == NULL) {
			yylval = install(yytext);
			}
		return (IDENT);
		}
break;
case 17:

# line 79 "ncgen.l"
	{
		lineno++ ;
		}
break;
case 18:

# line 82 "ncgen.l"
{
		if (sscanf((char*)yytext, "%le", &double_val) != 1) {
		    sprintf(errstr,"bad long or double constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
                return (DOUBLE_CONST);
                }
break;
case 19:

# line 89 "ncgen.l"
{
		if (sscanf((char*)yytext, "%e", &float_val) != 1) {
		    sprintf(errstr,"bad float constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
                return (FLOAT_CONST);
                }
break;
case 20:

# line 96 "ncgen.l"
{
		if (sscanf((char*)yytext, "%hd", &short_val) != 1) {
		    sprintf(errstr,"bad short constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
		return (SHORT_CONST);
	        }
break;
case 21:

# line 103 "ncgen.l"
{
#ifdef cray	/* machines where longs have more precision than doubles. */
    		char *ptr;
		long_val = strtol((char*)yytext, &ptr, 0);
		if (ptr == (char*)yytext) {
		    sprintf(errstr,"bad long constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
		return (LONG_CONST);
#else		/* machines where doubles have more precision than longs. */
		/*
		 * Because strtol and sscanf with "%ld" may silently give
		 * bad results from undetected overflow for strings like
		 * "30000000000", we scan as double first.
		 */
		double dd;
#ifdef VMS  /* work around bug in VMS strtol() */
		if (STREQ((char*)yytext, "-2147483648")) {
		    long_val = -2147483648;
		    return (LONG_CONST);
		}
#endif /* VMS */
		if (sscanf((char*)yytext, "%le", &dd) != 1) {
		    sprintf(errstr,"bad long constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
		if (dd < LONG_MIN  ||  dd > LONG_MAX) {
		    double_val = dd;
		    return DOUBLE_CONST;
		} else {
		    long_val = dd;
		    return LONG_CONST;
		}
#endif /* cray */
	        }
break;
case 22:

# line 138 "ncgen.l"
{
		long dd;
#ifdef VMS  /* work around bug in VMS strtol() */
		if (STREQ((char*)yytext, "-2147483648")) {
		    long_val = -2147483648;
		    return (LONG_CONST);
		}
#endif /* VMS */
		if (sscanf((char*)yytext, "%li", &dd) != 1) {
		    sprintf(errstr,"bad long constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
		long_val = dd;
		return LONG_CONST;
	        }
break;
case 23:

# line 153 "ncgen.l"
         {
	        (void) sscanf((char*)&yytext[1],"%c",&byte_val);
		return (BYTE_CONST);
                }
break;
case 24:

# line 157 "ncgen.l"
 {
		byte_val = strtol((char*)&yytext[2], (char **) 0, 8);
		return (BYTE_CONST);
                }
break;
case 25:

# line 161 "ncgen.l"
 {
		byte_val = strtol((char*)&yytext[2], (char **) 0, 16);
		return (BYTE_CONST);
                }
break;
case 26:

# line 165 "ncgen.l"
       {
	       switch ((char)yytext[2]) {
	          case 'a': byte_val = '\007'; break; /* not everyone under-
						       * stands '\a' yet */
     	          case 'b': byte_val = '\b'; break;
		  case 'f': byte_val = '\f'; break;
		  case 'n': byte_val = '\n'; break;
		  case 'r': byte_val = '\r'; break;
		  case 't': byte_val = '\t'; break;
		  case 'v': byte_val = '\v'; break;
		  case '\\': byte_val = '\\'; break;
		  case '?': byte_val = '\?'; break;
		  case '\'': byte_val = '\''; break;
		  default: byte_val = (char)yytext[2];
	           }
		return (BYTE_CONST);
                }
break;
case 27:

# line 183 "ncgen.l"
{/* whitespace */ ;
		}
break;
case 28:

# line 185 "ncgen.l"
	return (yytext[0]) ;
break;
case -1:
break;
default:
(void)fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int yyvstop[] = {
0,

28,
0,

27,
28,
0,

17,
0,

28,
0,

28,
0,

28,
0,

28,
0,

18,
28,
0,

28,
0,

21,
28,
0,

21,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

27,
0,

2,
0,

18,
0,

21,
0,

18,
0,

19,
0,

1,
0,

22,
0,

22,
0,

22,
0,

21,
0,

20,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
18,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

2,
0,

23,
0,

18,
0,

18,
0,

22,
0,

18,
22,
0,

22,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
18,
0,

16,
19,
0,

16,
0,

16,
0,

7,
16,
0,

15,
16,
0,

16,
0,

16,
0,

14,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

7,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

26,
0,

24,
26,
0,

14,
0,

18,
22,
0,

19,
22,
0,

18,
22,
0,

5,
16,
0,

4,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

7,
16,
0,

16,
0,

3,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

24,
0,

25,
0,

12,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

6,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

8,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

13,
0,

13,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

15,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

9,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

11,
0,

16,
0,

10,
0,
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,4,	1,5,	
0,0,	4,38,	0,0,	0,0,	
4,38,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,6,	
4,38,	41,93,	0,0,	0,0,	
1,7,	42,94,	95,137,	0,0,	
1,8,	0,0,	1,9,	1,10,	
1,11,	1,12,	1,13,	11,52,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,13,	0,0,	0,0,	
13,46,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,14,	1,15,	
1,16,	1,17,	1,18,	1,19,	
1,14,	23,76,	1,20,	16,61,	
22,74,	1,21,	19,69,	1,22,	
9,48,	21,73,	17,62,	1,23,	
1,24,	20,71,	1,25,	1,26,	
13,56,	1,14,	17,63,	15,60,	
24,77,	1,3,	25,78,	13,57,	
17,64,	26,79,	60,105,	1,27,	
1,28,	1,29,	37,92,	1,30,	
22,75,	2,7,	1,31,	28,81,	
33,88,	1,32,	19,70,	1,33,	
2,10,	2,11,	30,85,	1,34,	
1,35,	20,72,	1,36,	1,37,	
13,56,	29,82,	31,86,	27,80,	
1,3,	32,87,	34,89,	13,57,	
17,65,	29,83,	35,90,	36,91,	
2,15,	2,16,	48,99,	29,84,	
61,106,	62,107,	63,108,	2,20,	
64,109,	65,110,	69,113,	70,114,	
2,22,	6,39,	71,115,	72,116,	
2,23,	73,117,	74,118,	2,25,	
2,26,	6,39,	6,39,	75,119,	
76,120,	77,121,	78,122,	79,123,	
80,124,	81,125,	46,46,	82,126,	
2,27,	2,28,	2,29,	83,127,	
2,30,	84,128,	85,129,	2,31,	
86,130,	87,131,	2,32,	88,132,	
2,33,	89,133,	6,40,	90,134,	
2,34,	2,35,	91,135,	2,36,	
2,37,	92,136,	7,42,	6,39,	
99,141,	6,39,	46,56,	105,145,	
6,39,	6,39,	7,42,	7,42,	
106,146,	46,57,	107,147,	104,102,	
6,39,	108,148,	109,149,	98,49,	
110,150,	98,51,	104,57,	113,151,	
101,49,	6,39,	101,51,	98,49,	
6,39,	6,39,	6,39,	6,39,	
101,49,	114,152,	115,153,	7,42,	
6,39,	116,154,	46,56,	117,155,	
118,156,	120,157,	121,158,	6,39,	
7,42,	46,57,	7,42,	104,102,	
6,39,	7,42,	7,42,	98,49,	
6,41,	98,51,	104,57,	122,159,	
101,49,	7,42,	101,51,	98,49,	
123,160,	124,145,	125,146,	126,161,	
101,49,	127,162,	7,42,	128,163,	
129,164,	7,42,	7,42,	7,42,	
7,42,	130,165,	131,155,	133,157,	
134,168,	7,42,	135,169,	136,170,	
147,175,	148,176,	149,177,	6,39,	
7,42,	132,166,	150,178,	151,157,	
152,179,	7,42,	153,180,	154,181,	
8,44,	7,43,	8,45,	8,46,	
8,46,	8,46,	8,46,	8,46,	
8,46,	8,46,	8,46,	8,46,	
156,182,	10,44,	10,44,	10,44,	
10,44,	10,44,	10,44,	10,44,	
10,44,	10,44,	10,44,	8,47,	
158,183,	132,167,	159,184,	160,185,	
161,175,	162,186,	163,187,	164,157,	
7,42,	10,49,	10,50,	10,51,	
165,188,	166,189,	167,190,	168,183,	
12,44,	10,49,	12,53,	12,53,	
12,53,	12,53,	12,53,	12,53,	
12,53,	12,53,	12,53,	12,53,	
169,191,	170,192,	172,171,	8,47,	
174,173,	176,193,	177,194,	12,54,	
12,54,	12,54,	12,54,	12,55,	
12,54,	10,49,	10,50,	10,51,	
178,195,	179,196,	12,56,	180,197,	
181,198,	10,49,	96,138,	182,199,	
184,200,	12,57,	185,201,	186,202,	
187,194,	188,203,	12,58,	96,139,	
96,139,	96,139,	96,139,	96,139,	
96,139,	96,139,	96,139,	12,54,	
12,54,	12,54,	12,54,	12,55,	
12,54,	189,199,	190,199,	191,204,	
192,205,	193,206,	12,56,	195,207,	
196,208,	197,155,	198,209,	200,212,	
201,213,	12,57,	202,214,	203,155,	
14,59,	204,215,	12,58,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	205,216,	206,217,	207,218,	
208,219,	209,219,	211,0,	212,220,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	213,221,	214,222,	
215,223,	216,224,	14,59,	217,225,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	18,66,	218,119,	
18,67,	220,226,	221,227,	18,68,	
18,68,	18,68,	18,68,	18,68,	
18,68,	18,68,	18,68,	18,68,	
18,68,	43,95,	222,228,	223,226,	
224,229,	225,230,	227,231,	228,232,	
229,231,	43,95,	43,0,	230,233,	
45,45,	45,45,	45,45,	45,45,	
45,45,	45,45,	45,45,	45,45,	
45,45,	66,98,	66,98,	66,98,	
66,98,	66,98,	66,98,	66,98,	
66,98,	66,98,	66,98,	232,233,	
0,0,	0,0,	43,95,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	43,95,	
0,0,	43,95,	45,57,	0,0,	
43,96,	43,96,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
43,95,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	43,95,	0,0,	0,0,	
43,95,	43,95,	43,95,	43,95,	
0,0,	0,0,	0,0,	0,0,	
43,95,	0,0,	45,57,	0,0,	
0,0,	0,0,	0,0,	43,95,	
0,0,	0,0,	0,0,	0,0,	
43,95,	47,66,	0,0,	47,66,	
43,95,	0,0,	47,98,	47,98,	
47,98,	47,98,	47,98,	47,98,	
47,98,	47,98,	47,98,	47,98,	
50,100,	0,0,	50,100,	0,0,	
0,0,	50,101,	50,101,	50,101,	
50,101,	50,101,	50,101,	50,101,	
50,101,	50,101,	50,101,	52,52,	
43,97,	0,0,	0,0,	43,95,	
55,66,	0,0,	55,66,	52,52,	
52,0,	55,103,	55,103,	55,103,	
55,103,	55,103,	55,103,	55,103,	
55,103,	55,103,	55,103,	67,68,	
67,68,	67,68,	67,68,	67,68,	
67,68,	67,68,	67,68,	67,68,	
67,68,	0,0,	0,0,	0,0,	
52,52,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	52,52,	0,0,	52,52,	
0,0,	0,0,	52,52,	52,52,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	52,52,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	52,52,	
0,0,	0,0,	52,52,	52,52,	
52,52,	52,52,	0,0,	0,0,	
0,0,	0,0,	52,52,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	52,52,	0,0,	0,0,	
0,0,	0,0,	52,52,	0,0,	
0,0,	53,44,	52,52,	53,53,	
53,53,	53,53,	53,53,	53,53,	
53,53,	53,53,	53,53,	53,53,	
53,53,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
53,54,	53,54,	53,54,	53,54,	
53,55,	53,54,	0,0,	0,0,	
0,0,	0,0,	0,0,	53,102,	
0,0,	52,52,	0,0,	0,0,	
0,0,	0,0,	53,57,	100,101,	
100,101,	100,101,	100,101,	100,101,	
100,101,	100,101,	100,101,	100,101,	
100,101,	0,0,	0,0,	0,0,	
53,54,	53,54,	53,54,	53,54,	
53,55,	53,54,	0,0,	0,0,	
0,0,	0,0,	0,0,	53,102,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	53,57,	54,54,	
54,54,	54,54,	54,54,	54,54,	
54,54,	54,54,	54,54,	54,54,	
54,54,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
54,54,	54,54,	54,54,	54,54,	
54,54,	54,54,	139,171,	0,0,	
0,0,	0,0,	0,0,	54,102,	
0,0,	0,0,	0,0,	139,172,	
139,172,	139,172,	139,172,	139,172,	
139,172,	139,172,	139,172,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
54,54,	54,54,	54,54,	54,54,	
54,54,	54,54,	0,0,	0,0,	
0,0,	0,0,	0,0,	54,102,	
58,104,	58,104,	58,104,	58,104,	
58,104,	58,104,	58,104,	58,104,	
58,104,	58,104,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	58,104,	58,104,	58,104,	
58,104,	58,104,	58,104,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	68,68,	
68,68,	68,68,	68,68,	68,68,	
68,68,	68,68,	68,68,	68,68,	
68,68,	0,0,	0,0,	0,0,	
0,0,	58,104,	58,104,	58,104,	
58,104,	58,104,	58,104,	68,111,	
0,0,	68,112,	0,0,	0,0,	
0,0,	0,0,	0,0,	68,111,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	68,111,	
97,137,	68,112,	0,0,	0,0,	
0,0,	0,0,	0,0,	68,111,	
0,0,	97,140,	97,140,	97,140,	
97,140,	97,140,	97,140,	97,140,	
97,140,	97,140,	97,140,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	97,140,	97,140,	
97,140,	97,140,	97,140,	97,140,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
103,103,	103,103,	103,103,	103,103,	
103,103,	103,103,	103,103,	103,103,	
103,103,	103,103,	0,0,	0,0,	
0,0,	0,0,	97,140,	97,140,	
97,140,	97,140,	97,140,	97,140,	
103,142,	0,0,	103,143,	0,0,	
0,0,	0,0,	0,0,	0,0,	
103,144,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
103,142,	140,173,	103,143,	0,0,	
0,0,	0,0,	0,0,	0,0,	
103,144,	0,0,	140,174,	140,174,	
140,174,	140,174,	140,174,	140,174,	
140,174,	140,174,	140,174,	140,174,	
0,0,	0,0,	0,0,	0,0,	
0,0,	199,210,	0,0,	140,174,	
140,174,	140,174,	140,174,	140,174,	
140,174,	199,210,	199,210,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	199,210,	140,174,	
140,174,	140,174,	140,174,	140,174,	
140,174,	0,0,	0,0,	199,210,	
0,0,	199,211,	0,0,	0,0,	
199,211,	199,211,	0,0,	0,0,	
0,0,	0,0,	0,0,	210,210,	
199,211,	0,0,	210,210,	210,210,	
0,0,	0,0,	0,0,	0,0,	
0,0,	199,211,	210,210,	0,0,	
199,211,	199,211,	199,211,	199,211,	
0,0,	0,0,	0,0,	210,210,	
199,211,	0,0,	210,210,	210,210,	
210,210,	210,210,	0,0,	199,211,	
0,0,	0,0,	210,210,	0,0,	
199,211,	0,0,	0,0,	0,0,	
199,210,	210,210,	0,0,	0,0,	
0,0,	0,0,	210,210,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	199,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	210,0,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-66,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+4,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+6,
yycrank+-144,	0,		yyvstop+8,
yycrank+-185,	0,		yyvstop+10,
yycrank+230,	0,		yyvstop+12,
yycrank+2,	yysvec+8,	yyvstop+14,
yycrank+241,	0,		yyvstop+16,
yycrank+4,	0,		yyvstop+19,
yycrank+270,	0,		yyvstop+21,
yycrank+12,	yysvec+8,	yyvstop+24,
yycrank+343,	0,		yyvstop+27,
yycrank+2,	yysvec+14,	yyvstop+30,
yycrank+3,	yysvec+14,	yyvstop+33,
yycrank+17,	yysvec+14,	yyvstop+36,
yycrank+423,	yysvec+14,	yyvstop+39,
yycrank+2,	yysvec+14,	yyvstop+42,
yycrank+7,	yysvec+14,	yyvstop+45,
yycrank+2,	yysvec+14,	yyvstop+48,
yycrank+7,	yysvec+14,	yyvstop+51,
yycrank+4,	yysvec+14,	yyvstop+54,
yycrank+20,	yysvec+14,	yyvstop+57,
yycrank+16,	yysvec+14,	yyvstop+60,
yycrank+32,	yysvec+14,	yyvstop+63,
yycrank+2,	yysvec+14,	yyvstop+66,
yycrank+3,	yysvec+14,	yyvstop+69,
yycrank+24,	yysvec+14,	yyvstop+72,
yycrank+6,	yysvec+14,	yyvstop+75,
yycrank+12,	yysvec+14,	yyvstop+78,
yycrank+14,	yysvec+14,	yyvstop+81,
yycrank+7,	yysvec+14,	yyvstop+84,
yycrank+25,	yysvec+14,	yyvstop+87,
yycrank+26,	yysvec+14,	yyvstop+90,
yycrank+21,	yysvec+14,	yyvstop+93,
yycrank+5,	yysvec+14,	yyvstop+96,
yycrank+0,	yysvec+4,	yyvstop+99,
yycrank+0,	yysvec+6,	0,	
yycrank+0,	0,		yyvstop+101,
yycrank+-3,	yysvec+6,	0,	
yycrank+2,	0,		0,	
yycrank+-480,	0,		0,	
yycrank+0,	yysvec+10,	yyvstop+103,
yycrank+443,	yysvec+8,	0,	
yycrank+114,	yysvec+8,	yyvstop+105,
yycrank+526,	0,		0,	
yycrank+37,	0,		0,	
yycrank+0,	0,		yyvstop+107,
yycrank+541,	0,		0,	
yycrank+0,	0,		yyvstop+109,
yycrank+-598,	0,		yyvstop+111,
yycrank+643,	0,		yyvstop+113,
yycrank+711,	0,		yyvstop+115,
yycrank+561,	yysvec+54,	yyvstop+117,
yycrank+0,	0,		yyvstop+119,
yycrank+0,	0,		yyvstop+121,
yycrank+772,	0,		0,	
yycrank+0,	yysvec+14,	yyvstop+123,
yycrank+14,	yysvec+14,	yyvstop+125,
yycrank+71,	yysvec+14,	yyvstop+127,
yycrank+53,	yysvec+14,	yyvstop+129,
yycrank+61,	yysvec+14,	yyvstop+131,
yycrank+55,	yysvec+14,	yyvstop+133,
yycrank+24,	yysvec+14,	yyvstop+135,
yycrank+453,	0,		0,	
yycrank+571,	yysvec+14,	yyvstop+137,
yycrank+807,	yysvec+14,	yyvstop+139,
yycrank+63,	yysvec+14,	yyvstop+142,
yycrank+32,	yysvec+14,	yyvstop+144,
yycrank+62,	yysvec+14,	yyvstop+146,
yycrank+45,	yysvec+14,	yyvstop+148,
yycrank+71,	yysvec+14,	yyvstop+150,
yycrank+66,	yysvec+14,	yyvstop+152,
yycrank+77,	yysvec+14,	yyvstop+154,
yycrank+91,	yysvec+14,	yyvstop+156,
yycrank+78,	yysvec+14,	yyvstop+158,
yycrank+82,	yysvec+14,	yyvstop+160,
yycrank+77,	yysvec+14,	yyvstop+162,
yycrank+44,	yysvec+14,	yyvstop+164,
yycrank+64,	yysvec+14,	yyvstop+166,
yycrank+47,	yysvec+14,	yyvstop+168,
yycrank+58,	yysvec+14,	yyvstop+170,
yycrank+52,	yysvec+14,	yyvstop+172,
yycrank+59,	yysvec+14,	yyvstop+174,
yycrank+56,	yysvec+14,	yyvstop+176,
yycrank+63,	yysvec+14,	yyvstop+178,
yycrank+59,	yysvec+14,	yyvstop+180,
yycrank+80,	yysvec+14,	yyvstop+182,
yycrank+68,	yysvec+14,	yyvstop+184,
yycrank+74,	yysvec+14,	yyvstop+186,
yycrank+71,	yysvec+14,	yyvstop+188,
yycrank+0,	yysvec+6,	yyvstop+190,
yycrank+0,	0,		yyvstop+192,
yycrank+3,	0,		0,	
yycrank+311,	0,		0,	
yycrank+869,	0,		0,	
yycrank+135,	yysvec+66,	yyvstop+194,
yycrank+110,	0,		0,	
yycrank+679,	0,		0,	
yycrank+140,	yysvec+100,	yyvstop+196,
yycrank+0,	0,		yyvstop+198,
yycrank+904,	yysvec+54,	yyvstop+200,
yycrank+123,	yysvec+58,	yyvstop+203,
yycrank+122,	yysvec+14,	yyvstop+205,
yycrank+114,	yysvec+14,	yyvstop+207,
yycrank+133,	yysvec+14,	yyvstop+209,
yycrank+132,	yysvec+14,	yyvstop+211,
yycrank+136,	yysvec+14,	yyvstop+213,
yycrank+106,	yysvec+14,	yyvstop+215,
yycrank+0,	yysvec+14,	yyvstop+217,
yycrank+0,	yysvec+14,	yyvstop+220,
yycrank+142,	yysvec+14,	yyvstop+223,
yycrank+120,	yysvec+14,	yyvstop+225,
yycrank+149,	yysvec+14,	yyvstop+227,
yycrank+116,	yysvec+14,	yyvstop+230,
yycrank+152,	yysvec+14,	yyvstop+233,
yycrank+157,	yysvec+14,	yyvstop+235,
yycrank+0,	yysvec+14,	yyvstop+237,
yycrank+149,	yysvec+14,	yyvstop+240,
yycrank+144,	yysvec+14,	yyvstop+242,
yycrank+166,	yysvec+14,	yyvstop+244,
yycrank+171,	yysvec+14,	yyvstop+246,
yycrank+144,	yysvec+14,	yyvstop+248,
yycrank+132,	yysvec+14,	yyvstop+250,
yycrank+150,	yysvec+14,	yyvstop+252,
yycrank+148,	yysvec+14,	yyvstop+254,
yycrank+153,	yysvec+14,	yyvstop+256,
yycrank+155,	yysvec+14,	yyvstop+258,
yycrank+156,	yysvec+14,	yyvstop+260,
yycrank+155,	yysvec+14,	yyvstop+263,
yycrank+202,	yysvec+14,	yyvstop+265,
yycrank+151,	yysvec+14,	yyvstop+267,
yycrank+146,	yysvec+14,	yyvstop+269,
yycrank+157,	yysvec+14,	yyvstop+271,
yycrank+158,	yysvec+14,	yyvstop+273,
yycrank+0,	0,		yyvstop+275,
yycrank+0,	0,		yyvstop+277,
yycrank+743,	0,		0,	
yycrank+966,	0,		0,	
yycrank+0,	0,		yyvstop+280,
yycrank+0,	yysvec+54,	yyvstop+282,
yycrank+0,	yysvec+54,	yyvstop+285,
yycrank+0,	0,		yyvstop+288,
yycrank+0,	yysvec+14,	yyvstop+291,
yycrank+0,	yysvec+14,	yyvstop+294,
yycrank+206,	yysvec+14,	yyvstop+297,
yycrank+187,	yysvec+14,	yyvstop+299,
yycrank+190,	yysvec+14,	yyvstop+301,
yycrank+162,	yysvec+14,	yyvstop+303,
yycrank+187,	yysvec+14,	yyvstop+305,
yycrank+156,	yysvec+14,	yyvstop+307,
yycrank+203,	yysvec+14,	yyvstop+309,
yycrank+165,	yysvec+14,	yyvstop+311,
yycrank+0,	yysvec+14,	yyvstop+313,
yycrank+220,	yysvec+14,	yyvstop+316,
yycrank+0,	yysvec+14,	yyvstop+318,
yycrank+216,	yysvec+14,	yyvstop+321,
yycrank+225,	yysvec+14,	yyvstop+323,
yycrank+238,	yysvec+14,	yyvstop+325,
yycrank+246,	yysvec+14,	yyvstop+327,
yycrank+195,	yysvec+14,	yyvstop+329,
yycrank+198,	yysvec+14,	yyvstop+331,
yycrank+191,	yysvec+14,	yyvstop+333,
yycrank+209,	yysvec+14,	yyvstop+335,
yycrank+245,	yysvec+14,	yyvstop+337,
yycrank+214,	yysvec+14,	yyvstop+339,
yycrank+199,	yysvec+14,	yyvstop+341,
yycrank+219,	yysvec+14,	yyvstop+343,
yycrank+232,	yysvec+14,	yyvstop+345,
yycrank+0,	0,		yyvstop+347,
yycrank+291,	0,		0,	
yycrank+0,	0,		yyvstop+349,
yycrank+293,	0,		0,	
yycrank+0,	0,		yyvstop+351,
yycrank+250,	yysvec+14,	yyvstop+353,
yycrank+265,	yysvec+14,	yyvstop+355,
yycrank+243,	yysvec+14,	yyvstop+357,
yycrank+272,	yysvec+14,	yyvstop+359,
yycrank+278,	yysvec+14,	yyvstop+361,
yycrank+243,	yysvec+14,	yyvstop+363,
yycrank+281,	yysvec+14,	yyvstop+365,
yycrank+0,	yysvec+14,	yyvstop+367,
yycrank+279,	yysvec+14,	yyvstop+370,
yycrank+288,	yysvec+14,	yyvstop+372,
yycrank+240,	yysvec+14,	yyvstop+374,
yycrank+255,	yysvec+14,	yyvstop+376,
yycrank+256,	yysvec+14,	yyvstop+378,
yycrank+303,	yysvec+14,	yyvstop+380,
yycrank+272,	yysvec+14,	yyvstop+382,
yycrank+270,	yysvec+14,	yyvstop+384,
yycrank+278,	yysvec+14,	yyvstop+386,
yycrank+304,	yysvec+14,	yyvstop+388,
yycrank+0,	yysvec+14,	yyvstop+390,
yycrank+306,	yysvec+14,	yyvstop+393,
yycrank+270,	yysvec+14,	yyvstop+395,
yycrank+299,	yysvec+14,	yyvstop+397,
yycrank+266,	yysvec+14,	yyvstop+399,
yycrank+-1028,	0,		yyvstop+401,
yycrank+299,	yysvec+14,	yyvstop+403,
yycrank+308,	yysvec+14,	yyvstop+405,
yycrank+281,	yysvec+14,	yyvstop+407,
yycrank+273,	yysvec+14,	yyvstop+409,
yycrank+273,	yysvec+14,	yyvstop+411,
yycrank+293,	yysvec+14,	yyvstop+413,
yycrank+323,	yysvec+14,	yyvstop+415,
yycrank+293,	yysvec+14,	yyvstop+417,
yycrank+302,	yysvec+14,	yyvstop+419,
yycrank+284,	yysvec+14,	yyvstop+421,
yycrank+-1038,	yysvec+199,	yyvstop+423,
yycrank+-283,	yysvec+199,	yyvstop+425,
yycrank+338,	yysvec+14,	yyvstop+428,
yycrank+365,	yysvec+14,	yyvstop+430,
yycrank+324,	yysvec+14,	yyvstop+432,
yycrank+335,	yysvec+14,	yyvstop+434,
yycrank+336,	yysvec+14,	yyvstop+436,
yycrank+361,	yysvec+14,	yyvstop+438,
yycrank+365,	yysvec+14,	yyvstop+440,
yycrank+0,	yysvec+14,	yyvstop+442,
yycrank+401,	yysvec+14,	yyvstop+445,
yycrank+387,	yysvec+14,	yyvstop+447,
yycrank+372,	yysvec+14,	yyvstop+449,
yycrank+383,	yysvec+14,	yyvstop+451,
yycrank+369,	yysvec+14,	yyvstop+453,
yycrank+402,	yysvec+14,	yyvstop+455,
yycrank+0,	yysvec+14,	yyvstop+457,
yycrank+428,	yysvec+14,	yyvstop+460,
yycrank+372,	yysvec+14,	yyvstop+462,
yycrank+430,	yysvec+14,	yyvstop+464,
yycrank+433,	yysvec+14,	yyvstop+466,
yycrank+0,	0,		yyvstop+468,
yycrank+453,	yysvec+14,	yyvstop+470,
yycrank+0,	0,		yyvstop+472,
0,	0,	0};
struct yywork *yytop = yycrank+1161;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,011 ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,'"' ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,'+' ,01  ,'-' ,01  ,01  ,
'0' ,'1' ,'1' ,'1' ,'1' ,'1' ,'1' ,'1' ,
'8' ,'8' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'A' ,'A' ,'D' ,'E' ,'F' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'L' ,'G' ,'G' ,'G' ,
'G' ,'G' ,'G' ,'S' ,'G' ,'G' ,'G' ,'G' ,
'X' ,'G' ,'G' ,01  ,0134,01  ,01  ,'G' ,
01  ,'A' ,'A' ,'A' ,'D' ,'E' ,'F' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'L' ,'G' ,'G' ,'G' ,
'G' ,'G' ,'G' ,'S' ,'G' ,'G' ,'G' ,'G' ,
'X' ,'G' ,'G' ,'{' ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
/*	Copyright (c) 1989 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ident "@(#)ncform 6.4 92/06/19 SMI"

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
#if defined(__cplusplus) || defined(__STDC__)
int yylook(void)
#else
yylook()
#endif
{
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
			if(yylastch > &yytext[YYLMAX]) {
				fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
				exit(1);
			}
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
#if defined(__cplusplus) || defined(__STDC__)
int yyback(int *p, int m)
#else
yyback(p, m)
	int *p;
#endif
{
	if (p==0) return(0);
	while (*p) {
		if (*p++ == m)
			return(1);
	}
	return(0);
}
	/* the following are only used in the lex library */
#if defined(__cplusplus) || defined(__STDC__)
int yyinput(void)
#else
yyinput()
#endif
{
	return(input());
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyoutput(int c)
#else
yyoutput(c)
  int c; 
#endif
{
	output(c);
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyunput(int c)
#else
yyunput(c)
   int c; 
#endif
{
	unput(c);
	}
