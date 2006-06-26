/* A Bison parser, made by GNU Bison 2.0.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     NC_UNLIMITED_K = 258,
     BYTE_K = 259,
     CHAR_K = 260,
     SHORT_K = 261,
     LONG_K = 262,
     FLOAT_K = 263,
     DOUBLE_K = 264,
     IDENT = 265,
     TERMSTRING = 266,
     BYTE_CONST = 267,
     CHAR_CONST = 268,
     SHORT_CONST = 269,
     LONG_CONST = 270,
     FLOAT_CONST = 271,
     DOUBLE_CONST = 272,
     DIMENSIONS = 273,
     VARIABLES = 274,
     NETCDF = 275,
     DATA = 276
   };
#endif
#define NC_UNLIMITED_K 258
#define BYTE_K 259
#define CHAR_K 260
#define SHORT_K 261
#define LONG_K 262
#define FLOAT_K 263
#define DOUBLE_K 264
#define IDENT 265
#define TERMSTRING 266
#define BYTE_CONST 267
#define CHAR_CONST 268
#define SHORT_CONST 269
#define LONG_CONST 270
#define FLOAT_CONST 271
#define DOUBLE_CONST 272
#define DIMENSIONS 273
#define VARIABLES 274
#define NETCDF 275
#define DATA 276




/* Copy the first part of user declarations.  */
#line 9 "./ncgen.y"

#ifndef lint
static char SccsId[] = "$Id: ncgen.y,v 1.7 1997/11/05 19:40:51 koziol Exp $";
#endif
#include        <string.h>
#include	<stdlib.h>
#include        "ncgen.h"

typedef struct Symbol {		/* symbol table entry */
	char    	*name;
	struct Symbol   *next;
	unsigned	is_dim : 1;	/* appears as netCDF dimension */
	unsigned	is_var : 1;	/* appears as netCDF variable */
	unsigned	is_att : 1;	/* appears as netCDF attribute */
	int             dnum;	        /* handle as a dimension */
	int             vnum;	        /* handle as a variable */
	} *YYSTYPE1;

#define YYSTYPE YYSTYPE1
YYSTYPE install(), lookup();
YYSTYPE symlist;		/* symbol table: linked list */

void init_netcdf();		/* initializes netcdf counts (e.g. nvars) */
void define_netcdf();		/* generates all define mode stuff */
void load_netcdf();		/* generates variable puts */
void close_netcdf();		/* generates close */

void derror();			/* varargs message emitter */
void *emalloc(), *erealloc();	/* malloc that checks for memory exhausted */
void clearout();		/* initializes symbol table */
void nc_getfill();		/* to get fill value for various types */
void nc_putfill();		/* to get fill value for various types */
void nc_fill();		/* fills a generic array with a value */
int  put_variable();            /* invoke nc calls or generate code to put */
                                /* variable values            */
extern int derror_count;	/* counts errors in netcdf definition */
extern int lineno;		/* line number for error messages */

static int not_a_string;	/* whether last constant read was a string */
static char termstring[MAXTRST]; /* last terminal string read */
static double double_val;	/* last double value read */
static float float_val;		/* last float value read */
static nclong long_val;		/* last long value read */
static short short_val;		/* last short value read */
static char char_val;		/* last char value read */
static char byte_val;		/* last byte value read */

static nc_type type_code;	/* holds declared type for variables */
static nc_type atype_code;	/* holds derived type for attributes */
static char *netcdfname;	/* to construct netcdf file name */
static void *att_space;		/* pointer to block for attribute values */
static nc_type valtype;		/* type code for list of attribute values  */

static char *char_valp;		/* pointers used to accumulate data values */
static char *byte_valp;
static short *short_valp;
static nclong *long_valp;
static float *float_valp;
static double *double_valp;
static void *rec_cur;		/* pointer to where next data value goes */
static void *rec_start;		/* start of space for a record of data */


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 213 of yacc.c.  */
#line 192 "y.tab.c"

#if ! defined (yyoverflow) || YYERROR_VERBOSE

# ifndef YYFREE
#  define YYFREE free
# endif
# ifndef YYMALLOC
#  define YYMALLOC malloc
# endif

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   else
#    define YYSTACK_ALLOC alloca
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (defined (YYSTYPE_IS_TRIVIAL) && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short int yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short int) + sizeof (YYSTYPE))			\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined (__GNUC__) && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short int yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   70

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  30
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  39
/* YYNRULES -- Number of rules. */
#define YYNRULES  71
/* YYNRULES -- Number of states. */
#define YYNSTATES  101

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   276

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      27,    28,     2,     2,    25,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    29,    24,
       2,    26,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    22,     2,    23,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned char yyprhs[] =
{
       0,     0,     3,     4,     5,     6,    16,    17,    20,    23,
      27,    29,    33,    37,    41,    43,    45,    46,    49,    52,
      56,    58,    60,    63,    65,    67,    69,    71,    73,    75,
      77,    81,    82,    86,    88,    89,    93,    95,    99,   101,
     102,   107,   111,   114,   116,   118,   120,   124,   126,   128,
     130,   132,   134,   136,   138,   140,   141,   144,   147,   151,
     152,   157,   159,   163,   164,   167,   169,   171,   173,   175,
     177,   179
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const yysigned_char yyrhs[] =
{
      31,     0,    -1,    -1,    -1,    -1,    20,    22,    32,    35,
      33,    41,    34,    61,    23,    -1,    -1,    18,    36,    -1,
      37,    24,    -1,    36,    37,    24,    -1,    38,    -1,    37,
      25,    38,    -1,    39,    26,    15,    -1,    39,    26,     3,
      -1,    40,    -1,    10,    -1,    -1,    19,    42,    -1,    43,
      24,    -1,    42,    43,    24,    -1,    44,    -1,    53,    -1,
      45,    46,    -1,     4,    -1,     5,    -1,     6,    -1,     7,
      -1,     8,    -1,     9,    -1,    47,    -1,    46,    25,    47,
      -1,    -1,    49,    48,    50,    -1,    10,    -1,    -1,    27,
      51,    28,    -1,    52,    -1,    51,    25,    52,    -1,    40,
      -1,    -1,    55,    54,    26,    58,    -1,    56,    29,    57,
      -1,    29,    57,    -1,    49,    -1,    10,    -1,    59,    -1,
      58,    25,    59,    -1,    60,    -1,    13,    -1,    11,    -1,
      12,    -1,    14,    -1,    15,    -1,    16,    -1,    17,    -1,
      -1,    21,    62,    -1,    63,    24,    -1,    62,    63,    24,
      -1,    -1,    56,    64,    26,    65,    -1,    66,    -1,    65,
      25,    66,    -1,    -1,    67,    68,    -1,    13,    -1,    11,
      -1,    12,    -1,    14,    -1,    15,    -1,    16,    -1,    17,
      -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short int yyrline[] =
{
       0,   103,   103,   105,   110,   101,   121,   122,   124,   125,
     127,   128,   130,   136,   144,   155,   157,   158,   160,   161,
     163,   163,   165,   167,   168,   169,   170,   171,   172,   174,
     175,   178,   177,   203,   205,   206,   208,   209,   211,   231,
     230,   262,   263,   269,   279,   285,   286,   288,   297,   303,
     314,   320,   326,   332,   338,   346,   347,   350,   351,   354,
     353,   416,   417,   420,   420,   486,   511,   544,   569,   594,
     619,   644
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "NC_UNLIMITED_K", "BYTE_K", "CHAR_K",
  "SHORT_K", "LONG_K", "FLOAT_K", "DOUBLE_K", "IDENT", "TERMSTRING",
  "BYTE_CONST", "CHAR_CONST", "SHORT_CONST", "LONG_CONST", "FLOAT_CONST",
  "DOUBLE_CONST", "DIMENSIONS", "VARIABLES", "NETCDF", "DATA", "'{'",
  "'}'", "';'", "','", "'='", "'('", "')'", "':'", "$accept", "ncdesc",
  "@1", "@2", "@3", "dimsection", "dimdecls", "dimdecline", "dimdecl",
  "dimd", "dim", "vasection", "vadecls", "vadecl", "vardecl", "type",
  "varlist", "varspec", "@4", "var", "dimspec", "dimlist", "vdim",
  "attdecl", "@5", "att", "avar", "attr", "attvallist", "aconst",
  "attconst", "datasection", "datadecls", "datadecl", "@6", "constlist",
  "dconst", "@7", "const", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short int yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   123,   125,    59,    44,    61,    40,    41,    58
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    30,    32,    33,    34,    31,    35,    35,    36,    36,
      37,    37,    38,    38,    39,    40,    41,    41,    42,    42,
      43,    43,    44,    45,    45,    45,    45,    45,    45,    46,
      46,    48,    47,    49,    50,    50,    51,    51,    52,    54,
      53,    55,    55,    56,    57,    58,    58,    59,    60,    60,
      60,    60,    60,    60,    60,    61,    61,    62,    62,    64,
      63,    65,    65,    67,    66,    68,    68,    68,    68,    68,
      68,    68
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     0,     0,     0,     9,     0,     2,     2,     3,
       1,     3,     3,     3,     1,     1,     0,     2,     2,     3,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       3,     0,     3,     1,     0,     3,     1,     3,     1,     0,
       4,     3,     2,     1,     1,     1,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     2,     2,     3,     0,
       4,     1,     3,     0,     2,     1,     1,     1,     1,     1,
       1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
       0,     0,     0,     2,     1,     6,     0,     3,    15,     7,
       0,    10,     0,    14,    16,     0,     8,     0,     0,     0,
       4,     9,    11,    13,    12,    23,    24,    25,    26,    27,
      28,    33,     0,    17,     0,    20,     0,    43,    21,    39,
       0,    55,    44,    42,     0,    18,    22,    29,    31,     0,
       0,     0,     0,    19,     0,    34,     0,    41,    59,    56,
       0,     5,    30,     0,    32,    49,    50,    48,    51,    52,
      53,    54,    40,    45,    47,     0,     0,    57,    38,     0,
      36,     0,    63,    58,     0,    35,    46,    60,    61,     0,
      37,    63,    66,    67,    65,    68,    69,    70,    71,    64,
      62
};

/* YYDEFGOTO[NTERM-NUM]. */
static const yysigned_char yydefgoto[] =
{
      -1,     2,     5,    14,    41,     7,     9,    10,    11,    12,
      13,    20,    33,    34,    35,    36,    46,    47,    55,    37,
      64,    79,    80,    38,    49,    39,    40,    43,    72,    73,
      74,    52,    59,    60,    75,    87,    88,    89,    99
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -57
static const yysigned_char yypact[] =
{
       7,    -2,    38,   -57,   -57,    21,    30,   -57,   -57,    30,
      -1,   -57,    15,   -57,    23,    12,   -57,    30,     6,    -4,
     -57,   -57,   -57,   -57,   -57,   -57,   -57,   -57,   -57,   -57,
     -57,   -57,    33,    -4,    20,   -57,    35,   -57,   -57,   -57,
      17,    26,   -57,   -57,    24,   -57,    25,   -57,   -57,    27,
      33,    35,    28,   -57,    35,    22,     0,   -57,   -57,    35,
      31,   -57,   -57,    30,   -57,   -57,   -57,   -57,   -57,   -57,
     -57,   -57,    29,   -57,   -57,    32,    36,   -57,   -57,    -6,
     -57,     0,   -57,   -57,    30,   -57,   -57,    34,   -57,    18,
     -57,   -57,   -57,   -57,   -57,   -57,   -57,   -57,   -57,   -57,
     -57
};

/* YYPGOTO[NTERM-NUM].  */
static const yysigned_char yypgoto[] =
{
     -57,   -57,   -57,   -57,   -57,   -57,   -57,    43,    39,   -57,
     -56,   -57,   -57,    37,   -57,   -57,   -57,     3,   -57,   -28,
     -57,   -57,   -23,   -57,   -57,   -57,   -41,    13,   -57,   -19,
     -57,   -57,   -57,     5,   -57,   -57,   -26,   -57,   -57
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const unsigned char yytable[] =
{
      25,    26,    27,    28,    29,    30,    31,    78,    48,    23,
      58,    65,    66,    67,    68,    69,    70,    71,    58,    84,
       3,    24,    85,    16,    17,    32,    48,     1,    78,    92,
      93,    94,    95,    96,    97,    98,    21,    17,     4,     6,
       8,    18,    19,    42,    45,    31,    50,    51,    53,    63,
      54,    61,    15,    56,    81,    77,    22,    62,    82,    91,
      83,    90,    86,    57,    76,   100,     0,     0,     0,     0,
      44
};

static const yysigned_char yycheck[] =
{
       4,     5,     6,     7,     8,     9,    10,    63,    36,     3,
      51,    11,    12,    13,    14,    15,    16,    17,    59,    25,
      22,    15,    28,    24,    25,    29,    54,    20,    84,    11,
      12,    13,    14,    15,    16,    17,    24,    25,     0,    18,
      10,    26,    19,    10,    24,    10,    29,    21,    24,    27,
      25,    23,     9,    26,    25,    24,    17,    54,    26,    25,
      24,    84,    81,    50,    59,    91,    -1,    -1,    -1,    -1,
      33
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,    20,    31,    22,     0,    32,    18,    35,    10,    36,
      37,    38,    39,    40,    33,    37,    24,    25,    26,    19,
      41,    24,    38,     3,    15,     4,     5,     6,     7,     8,
       9,    10,    29,    42,    43,    44,    45,    49,    53,    55,
      56,    34,    10,    57,    43,    24,    46,    47,    49,    54,
      29,    21,    61,    24,    25,    48,    26,    57,    56,    62,
      63,    23,    47,    27,    50,    11,    12,    13,    14,    15,
      16,    17,    58,    59,    60,    64,    63,    24,    40,    51,
      52,    25,    26,    24,    25,    28,    59,    65,    66,    67,
      52,    25,    11,    12,    13,    14,    15,    16,    17,    68,
      66
};

#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");\
      YYERROR;							\
    }								\
while (0)


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (N)								\
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (0)
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
              (Loc).first_line, (Loc).first_column,	\
              (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Type, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short int *bottom, short int *top)
#else
static void
yy_stack_print (bottom, top)
    short int *bottom;
    short int *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %u), ",
             yyrule - 1, yylno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname [yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname [yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

#endif /* !YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);


# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short int yyssa[YYINITDEPTH];
  short int *yyss = yyssa;
  register short int *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;


  yyvsp[0] = yylval;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short int *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short int *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a look-ahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to look-ahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 103 "./ncgen.y"
    { init_netcdf(); }
    break;

  case 3:
#line 105 "./ncgen.y"
    {
                       if (ndims > MAX_NC_DIMS)
                         derror("Too many dimensions");
                   }
    break;

  case 4:
#line 110 "./ncgen.y"
    {
		       if (derror_count == 0)
			 define_netcdf(netcdfname);
		   }
    break;

  case 5:
#line 116 "./ncgen.y"
    {
		       if (derror_count == 0)
			 close_netcdf();
		   }
    break;

  case 12:
#line 131 "./ncgen.y"
    { if (long_val <= 0)
			 derror("negative dimension size");
		     dims[ndims].size = long_val;
		     ndims++;
		   }
    break;

  case 13:
#line 137 "./ncgen.y"
    {  if (rec_dim != -1)
			 derror("only one NC_UNLIMITED dimension allowed");
		     rec_dim = ndims; /* the unlimited (record) dimension */
		     dims[ndims].size = NC_UNLIMITED;
		     ndims++;
		   }
    break;

  case 14:
#line 145 "./ncgen.y"
    { if ((yyvsp[0])->is_dim == 1) {
		        derror( "duplicate dimension declaration for %s",
		                (yyvsp[0])->name);
		     }
	             (yyvsp[0])->is_dim = 1;
		     (yyvsp[0])->dnum = ndims;
		     dims[ndims].name = (char *) emalloc(strlen((yyvsp[0])->name)+1);
		     (void) strcpy(dims[ndims].name, (yyvsp[0])->name);
		   }
    break;

  case 23:
#line 167 "./ncgen.y"
    { type_code = NC_BYTE; }
    break;

  case 24:
#line 168 "./ncgen.y"
    { type_code = NC_CHAR; }
    break;

  case 25:
#line 169 "./ncgen.y"
    { type_code = NC_SHORT; }
    break;

  case 26:
#line 170 "./ncgen.y"
    { type_code = NC_LONG; }
    break;

  case 27:
#line 171 "./ncgen.y"
    { type_code = NC_FLOAT; }
    break;

  case 28:
#line 172 "./ncgen.y"
    { type_code = NC_DOUBLE; }
    break;

  case 31:
#line 178 "./ncgen.y"
    {
		    if (nvars >= MAX_NC_VARS)
		       derror("too many variables");
		    nvdims = 0;
		    /* make sure variable not re-declared */
		    if ((yyvsp[0])->is_var == 1) {
		       derror( "duplicate variable declaration for %s",
		               (yyvsp[0])->name);
		    }
	            (yyvsp[0])->is_var = 1;
		    (yyvsp[0])->vnum = nvars;
		    vars[nvars].name = (char *) emalloc(strlen((yyvsp[0])->name)+1);
		    (void) strcpy(vars[nvars].name, (yyvsp[0])->name);
		    vars[nvars].type = type_code;
		    /* set default fill value.  You can override this with
		     * the variable attribute "_FillValue". */
		    nc_getfill(type_code, &vars[nvars].fill_value);
		    vars[nvars].has_data = 0; /* has no data (yet) */
		   }
    break;

  case 32:
#line 198 "./ncgen.y"
    {
		    vars[nvars].ndims = nvdims;
		    nvars++;
		   }
    break;

  case 38:
#line 212 "./ncgen.y"
    {
		    if (nvdims >= MAX_VAR_DIMS) {
		       derror("%s has too many dimensions",vars[nvars].name);
		    }
		    if ((yyvsp[0])->is_dim == 1)
		       dimnum = (yyvsp[0])->dnum;
		    else {
		       derror( "%s is not declared as a dimension",
			       (yyvsp[0])->name);
	               dimnum = ndims;
		    }
		    if (rec_dim != -1 && dimnum == rec_dim && nvdims != 0) {
		       derror("unlimited dimension must be first");
		    }
		    vars[nvars].dims[nvdims] = dimnum;
                    nvdims++;
		   }
    break;

  case 39:
#line 231 "./ncgen.y"
    {
		       valnum = 0;
		       valtype = NC_UNSPECIFIED;
		       /* get a large block for attributes, realloc later */
		       att_space = emalloc(MAX_NC_ATTSIZE);
		       /* make all kinds of pointers point to it */
		       char_valp = (char *) att_space;
		       byte_valp = (char *) att_space;
		       short_valp = (short *) att_space;
		       long_valp = (nclong *) att_space;
		       float_valp = (float *) att_space;
		       double_valp = (double *) att_space;
		   }
    break;

  case 40:
#line 245 "./ncgen.y"
    {
		       if (natts >= MAX_NC_ATTS)
			 derror("too many attributes");
		       atts[natts].var = varnum ;
		       atts[natts].type = valtype;
		       atts[natts].len = valnum;
		       /* shrink space down to what was really needed */
		       att_space = erealloc(att_space, valnum*nctypelen(valtype));
		       atts[natts].val = att_space;
		       if (STREQ(atts[natts].name, _FillValue)) {
			   nc_putfill(atts[natts].type,
				       atts[natts].val,
				       &vars[atts[natts].var].fill_value);
		       }
		       natts++;
		   }
    break;

  case 42:
#line 264 "./ncgen.y"
    {
		    varnum = -1;  /* handle of "global" attribute */
		   }
    break;

  case 43:
#line 270 "./ncgen.y"
    { if ((yyvsp[0])->is_var == 1)
		       varnum = (yyvsp[0])->vnum;
		    else {
		      derror("%s not declared as a variable, fatal error",
			     (yyvsp[0])->name);
		      YYABORT;
		      }
		   }
    break;

  case 44:
#line 280 "./ncgen.y"
    {
		       atts[natts].name = (char *) emalloc(strlen((yyvsp[0])->name)+1);
		       (void) strcpy(atts[natts].name,(yyvsp[0])->name);
		   }
    break;

  case 47:
#line 289 "./ncgen.y"
    {
		    if (valtype == NC_UNSPECIFIED)
		      valtype = atype_code;
		    if (valtype != atype_code)
		      derror("values for attribute must be all of same type");
		   }
    break;

  case 48:
#line 298 "./ncgen.y"
    {
		       atype_code = NC_CHAR;
		       *char_valp++ = char_val;
		       valnum++;
		   }
    break;

  case 49:
#line 304 "./ncgen.y"
    {
		       atype_code = NC_CHAR;
		       {
			   /* don't null-terminate attribute strings */
			   int len = strlen(termstring);
			   valnum += len;
			   (void)strncpy(char_valp,termstring,len);
			   char_valp += len;
		       }
		   }
    break;

  case 50:
#line 315 "./ncgen.y"
    {
		       atype_code = NC_BYTE;
		       *byte_valp++ = byte_val;
		       valnum++;
		   }
    break;

  case 51:
#line 321 "./ncgen.y"
    {
		       atype_code = NC_SHORT;
		       *short_valp++ = short_val;
		       valnum++;
		   }
    break;

  case 52:
#line 327 "./ncgen.y"
    {
		       atype_code = NC_LONG;
		       *long_valp++ = long_val;
		       valnum++;
		   }
    break;

  case 53:
#line 333 "./ncgen.y"
    {
		       atype_code = NC_FLOAT;
		       *float_valp++ = float_val;
		       valnum++;
		   }
    break;

  case 54:
#line 339 "./ncgen.y"
    {
		       atype_code = NC_DOUBLE;
		       *double_valp++ = double_val;
		       valnum++;
		   }
    break;

  case 59:
#line 354 "./ncgen.y"
    {
		       valtype = vars[varnum].type; /* variable type */
		       valnum = 0;	/* values accumulated for variable */
		       vars[varnum].has_data = 1;
		       /* compute dimensions product (size of a "record") */
		       var_size = nctypelen(valtype);
		       if (vars[varnum].ndims == 0)
			   var_len = 1;
		       else if (vars[varnum].dims[0] == rec_dim) {
			   var_len = 1; /* one record for unlimited vars */
			   netcdf_record_number = 0;
		       }
		       else
			 var_len = dims[vars[varnum].dims[0]].size;
		       for(dimnum = 1; dimnum < vars[varnum].ndims; dimnum++)
			 var_len = var_len*dims[vars[varnum].dims[dimnum]].size;
		       /* allocate memory for a record of variable data */
		       if (var_len*var_size != (unsigned)(var_len*var_size)) {
			   derror("too much data for this machine");
			   exit(9);
		       }
		       rec_start = malloc ((unsigned)(var_len*var_size));
		       if (rec_start == 0) {
			   derror ("out of memory\n");
			   exit(3);
		       }
		       rec_cur = rec_start;
		       switch (valtype) {
			 case NC_CHAR:
			   char_valp = (char *) rec_start;
			   break;
			 case NC_BYTE:
			   byte_valp = (char *) rec_start;
			   break;
			 case NC_SHORT:
			   short_valp = (short *) rec_start;
			   break;
			 case NC_LONG:
			   long_valp = (nclong *) rec_start;
			   break;
			 case NC_FLOAT:
			   float_valp = (float *) rec_start;
			   break;
			 case NC_DOUBLE:
			   double_valp = (double *) rec_start;
			   break;
		       }
		 }
    break;

  case 60:
#line 403 "./ncgen.y"
    {
		       if (valnum > 0 && valnum < var_len) { /* leftovers */
			   nc_fill(valtype,
				    var_len - valnum,
				    rec_cur,
				    vars[varnum].fill_value);
			   /* put out record of var_len values */
			   if (derror_count == 0)
			     put_variable(rec_start);
		       }
		       free ((char *) rec_start);
		 }
    break;

  case 63:
#line 420 "./ncgen.y"
    {
		       if(valnum >= var_len) {
			   derror("too many values for this variable");
			   exit (4);
		       }
		       not_a_string = 1;
                   }
    break;

  case 64:
#line 428 "./ncgen.y"
    {
		       if (not_a_string) {
			   switch (valtype) {
			     case NC_CHAR:
			       rec_cur = (void *) char_valp;
			       break;
			     case NC_BYTE:
			       rec_cur = (void *) byte_valp;
			       break;
			     case NC_SHORT:
			       rec_cur = (void *) short_valp;
			       break;
			     case NC_LONG:
			       rec_cur = (void *) long_valp;
			       break;
			     case NC_FLOAT:
			       rec_cur = (void *) float_valp;
			       break;
			     case NC_DOUBLE:
			       rec_cur = (void *) double_valp;
			       break;
			   }
		       }
		       if (valnum >= var_len) {
			   /* put out record of var_len elements */
			   if (derror_count == 0)
			     put_variable(rec_start);
			   /* if this variable is unbounded, reset for */
			   /* next record */
			   if (vars[varnum].dims[0] == rec_dim) {
			       valnum = 0;
			       netcdf_record_number++;
			       rec_cur = rec_start;
			       switch (valtype) {
				 case NC_CHAR:
				   char_valp = (char *) rec_start;
				   break;
				 case NC_BYTE:
				   byte_valp = (char *) rec_start;
				   break;
				 case NC_SHORT:
				   short_valp = (short *) rec_start;
				   break;
				 case NC_LONG:
				   long_valp = (nclong *) rec_start;
				   break;
				 case NC_FLOAT:
				   float_valp = (float *) rec_start;
				   break;
				 case NC_DOUBLE:
				   double_valp = (double *) rec_start;
				   break;
			       }
			   }
		       }
		 }
    break;

  case 65:
#line 487 "./ncgen.y"
    {
		       atype_code = NC_CHAR;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = char_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = char_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = char_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = char_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = char_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = char_val;
			   break;
		       }
		       valnum++;
		   }
    break;

  case 66:
#line 512 "./ncgen.y"
    {
		       not_a_string = 0;
		       atype_code = NC_CHAR;
		       {
			   int len = strlen(termstring);

			   valnum += len;
			   if(valnum > var_len) {
			       derror("string won't fit in this variable");
			       exit (5);
			   }
			   switch (valtype) {
			     case NC_CHAR:
			       (void)strncpy(char_valp,termstring,len);
			       char_valp += len;
			       rec_cur = (void *) char_valp;
			       break;
			     case NC_BYTE:
			       (void)strncpy(byte_valp,termstring,len);
			       byte_valp += len;
			       rec_cur = (void *) byte_valp;
			       break;
			     case NC_SHORT:
			     case NC_LONG:
			     case NC_FLOAT:
			     case NC_DOUBLE:
			       derror("string value invalid for %s variable",
				      nctype(valtype));
			       break;
			   }
		       }
		   }
    break;

  case 67:
#line 545 "./ncgen.y"
    {
		       atype_code = NC_BYTE;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = byte_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = byte_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = byte_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = byte_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = byte_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = byte_val;
			   break;
		       }
		       valnum++;
		   }
    break;

  case 68:
#line 570 "./ncgen.y"
    {
		       atype_code = NC_SHORT;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = short_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = short_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = short_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = short_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = short_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = short_val;
			   break;
		       }
		       valnum++;
		   }
    break;

  case 69:
#line 595 "./ncgen.y"
    {
		       atype_code = NC_LONG;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = long_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = long_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = long_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = long_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = long_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = long_val;
			   break;
		       }
		       valnum++;
		   }
    break;

  case 70:
#line 620 "./ncgen.y"
    {
		       atype_code = NC_FLOAT;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = float_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = float_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = float_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = float_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = float_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = float_val;
			   break;
		       }
		       valnum++;
		   }
    break;

  case 71:
#line 645 "./ncgen.y"
    {
		       atype_code = NC_DOUBLE;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = double_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = double_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = double_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = double_val;
			   break;
			 case NC_FLOAT:
			   if (double_val == FILL_DOUBLE)
			     *float_valp++ = FILL_FLOAT;
			   else
			     *float_valp++ = double_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = double_val;
			   break;
		       }
		       valnum++;
		   }
    break;


    }

/* Line 1037 of yacc.c.  */
#line 1826 "y.tab.c"

  yyvsp -= yylen;
  yyssp -= yylen;


  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  int yytype = YYTRANSLATE (yychar);
	  const char* yyprefix;
	  char *yymsg;
	  int yyx;

	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  int yyxbegin = yyn < 0 ? -yyn : 0;

	  /* Stay within bounds of both yycheck and yytname.  */
	  int yychecklim = YYLAST - yyn;
	  int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
	  int yycount = 0;

	  yyprefix = ", expecting ";
	  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      {
		yysize += yystrlen (yyprefix) + yystrlen (yytname [yyx]);
		yycount += 1;
		if (yycount == 5)
		  {
		    yysize = 0;
		    break;
		  }
	      }
	  yysize += (sizeof ("syntax error, unexpected ")
		     + yystrlen (yytname[yytype]));
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "syntax error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[yytype]);

	      if (yycount < 5)
		{
		  yyprefix = ", expecting ";
		  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
		    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
		      {
			yyp = yystpcpy (yyp, yyprefix);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yyprefix = " or ";
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("syntax error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("syntax error");
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* If at end of input, pop the error token,
	     then the rest of the stack, then return failure.  */
	  if (yychar == YYEOF)
	     for (;;)
	       {

		 YYPOPSTACK;
		 if (yyssp == yyss)
		   YYABORT;
		 yydestruct ("Error: popping",
                             yystos[*yyssp], yyvsp);
	       }
        }
      else
	{
	  yydestruct ("Error: discarding", yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

#ifdef __GNUC__
  /* Pacify GCC when the user code never invokes YYERROR and the label
     yyerrorlab therefore never appears in user code.  */
  if (0)
     goto yyerrorlab;
#endif

yyvsp -= yylen;
  yyssp -= yylen;
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping", yystos[yystate], yyvsp);
      YYPOPSTACK;
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token. */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yydestruct ("Error: discarding lookahead",
              yytoken, &yylval);
  yychar = YYEMPTY;
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*----------------------------------------------.
| yyoverflowlab -- parser overflow comes here.  |
`----------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}


#line 676 "./ncgen.y"


/* PROGRAMS */

/* get lexical input routine generated by lex  */
#include "ncgenyy.c"

void derror();

yyerror(s)	/* called for yacc syntax error */
     char *s;
{
	derror(s);
}

#ifndef yywrap
int
yywrap()			/* returns 1 on EOF if no more input */
{
    return  1;
}
#endif /* yywrap() */


/* Symbol table operations for ncgen tool */

YYSTYPE lookup(sname)       /* find sname in symbol table (linear search) */
char *sname;
{
    YYSTYPE sp;
    for (sp = symlist; sp != (YYSTYPE) 0; sp = sp -> next)
	if (STREQ(sp -> name, sname)) {
	    return sp;
	}
    return 0;			/* 0 ==> not found */
}

YYSTYPE install(sname)  /* install sname in symbol table */
char *sname;
{
    YYSTYPE sp;

    sp = (YYSTYPE) emalloc (sizeof (struct Symbol));
    sp -> name = (char *) emalloc (strlen (sname) + 1);/* +1 for '\0' */
    (void) strcpy (sp -> name, sname);
    sp -> next = symlist;	/* put at front of list */
    sp -> is_dim = 0;
    sp -> is_var = 0;
    sp -> is_att = 0;
    symlist = sp;
    return sp;
}

void
clearout()	/* reset symbol table to empty */
{
    YYSTYPE sp, tp;
    for (sp = symlist; sp != (YYSTYPE) 0;) {
	tp = sp -> next;
	free (sp -> name);
	free ((char *) sp);
	sp = tp;
    }
    symlist = 0;
}

