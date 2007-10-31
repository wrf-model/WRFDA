/* A Bison parser, made by GNU Bison 1.875c.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

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
     INT_K = 262,
     FLOAT_K = 263,
     DOUBLE_K = 264,
     IDENT = 265,
     TERMSTRING = 266,
     BYTE_CONST = 267,
     CHAR_CONST = 268,
     SHORT_CONST = 269,
     INT_CONST = 270,
     FLOAT_CONST = 271,
     DOUBLE_CONST = 272,
     DIMENSIONS = 273,
     VARIABLES = 274,
     NETCDF = 275,
     DATA = 276,
     FILLVALUE = 277
   };
#endif
#define NC_UNLIMITED_K 258
#define BYTE_K 259
#define CHAR_K 260
#define SHORT_K 261
#define INT_K 262
#define FLOAT_K 263
#define DOUBLE_K 264
#define IDENT 265
#define TERMSTRING 266
#define BYTE_CONST 267
#define CHAR_CONST 268
#define SHORT_CONST 269
#define INT_CONST 270
#define FLOAT_CONST 271
#define DOUBLE_CONST 272
#define DIMENSIONS 273
#define VARIABLES 274
#define NETCDF 275
#define DATA 276
#define FILLVALUE 277




/* Copy the first part of user declarations.  */
#line 9 "./ncgen.y"

#ifndef lint
static char SccsId[] = "$Id: ncgen.y,v 1.4 2004/12/23 19:31:04 robl Exp $";
#endif
#include        <string.h>
#include	<stdlib.h>
#include	<pnetcdf.h>
#include 	"generic.h"
#include        "ncgen.h"
#include	"genlib.h"	/* for grow_darray() et al */

typedef struct Symbol {		/* symbol table entry */
	char    	*name;
	struct Symbol   *next;
	unsigned	is_dim : 1;	/* appears as netCDF dimension */
	unsigned	is_var : 1;	/* appears as netCDF variable */
	unsigned	is_att : 1;	/* appears as netCDF attribute */
	int             dnum;	        /* handle as a dimension */
	int             vnum;	        /* handle as a variable */
	} *YYSTYPE1;

/* True if string a equals string b*/
#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)
#define NC_UNSPECIFIED ((nc_type)0)	/* unspecified (as yet) type */

#define YYSTYPE YYSTYPE1
YYSTYPE symlist;		/* symbol table: linked list */

extern int derror_count;	/* counts errors in netcdf definition */
extern int lineno;		/* line number for error messages */

static int not_a_string;	/* whether last constant read was a string */
static char termstring[MAXTRST]; /* last terminal string read */
static double double_val;	/* last double value read */
static float float_val;		/* last float value read */
static int int_val;		/* last int value read */
static short short_val;		/* last short value read */
static char char_val;		/* last char value read */
static signed char byte_val;	/* last byte value read */

static nc_type type_code;	/* holds declared type for variables */
static nc_type atype_code;	/* holds derived type for attributes */
static char *netcdfname;	/* to construct netcdf file name */
static void *att_space;		/* pointer to block for attribute values */
static nc_type valtype;		/* type code for list of attribute values  */

static char *char_valp;		/* pointers used to accumulate data values */
static signed char *byte_valp;
static short *short_valp;
static int *int_valp;
static float *float_valp;
static double *double_valp;
static void *rec_cur;		/* pointer to where next data value goes */
static void *rec_start;		/* start of space for data */


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


/* Line 214 of yacc.c.  */
#line 187 "y.tab.c"

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
#   define YYSTACK_ALLOC alloca
#  endif
# else
#  if defined (alloca) || defined (_ALLOCA_H)
#   define YYSTACK_ALLOC alloca
#  else
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
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
  short yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
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
   typedef short yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   92

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  31
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  40
/* YYNRULES -- Number of rules. */
#define YYNRULES  77
/* YYNRULES -- Number of states. */
#define YYNSTATES  108

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   277

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      28,    29,     2,     2,    26,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    30,    25,
       2,    27,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    23,     2,    24,     2,     2,     2,     2,
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
      15,    16,    17,    18,    19,    20,    21,    22
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned char yyprhs[] =
{
       0,     0,     3,     4,     5,    14,    15,    18,    21,    25,
      27,    31,    35,    39,    43,    45,    47,    48,    51,    53,
      56,    60,    62,    64,    67,    71,    73,    76,    78,    80,
      82,    84,    86,    88,    90,    94,    95,    99,   101,   102,
     106,   108,   112,   114,   115,   120,   124,   127,   129,   131,
     133,   137,   139,   141,   143,   145,   147,   149,   151,   153,
     154,   157,   159,   162,   166,   167,   172,   174,   178,   179,
     182,   184,   186,   188,   190,   192,   194,   196
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const yysigned_char yyrhs[] =
{
      32,     0,    -1,    -1,    -1,    20,    23,    33,    35,    41,
      34,    63,    24,    -1,    -1,    18,    36,    -1,    37,    25,
      -1,    36,    37,    25,    -1,    38,    -1,    37,    26,    38,
      -1,    39,    27,    15,    -1,    39,    27,    17,    -1,    39,
      27,     3,    -1,    40,    -1,    10,    -1,    -1,    19,    42,
      -1,    44,    -1,    43,    25,    -1,    42,    43,    25,    -1,
      46,    -1,    55,    -1,    45,    25,    -1,    44,    45,    25,
      -1,    55,    -1,    47,    48,    -1,     4,    -1,     5,    -1,
       6,    -1,     7,    -1,     8,    -1,     9,    -1,    49,    -1,
      48,    26,    49,    -1,    -1,    51,    50,    52,    -1,    10,
      -1,    -1,    28,    53,    29,    -1,    54,    -1,    53,    26,
      54,    -1,    40,    -1,    -1,    57,    56,    27,    60,    -1,
      58,    30,    59,    -1,    30,    59,    -1,    51,    -1,    10,
      -1,    61,    -1,    60,    26,    61,    -1,    62,    -1,    13,
      -1,    11,    -1,    12,    -1,    14,    -1,    15,    -1,    16,
      -1,    17,    -1,    -1,    21,    64,    -1,    21,    -1,    65,
      25,    -1,    64,    65,    25,    -1,    -1,    58,    66,    27,
      67,    -1,    68,    -1,    67,    26,    68,    -1,    -1,    69,
      70,    -1,    13,    -1,    11,    -1,    12,    -1,    14,    -1,
      15,    -1,    16,    -1,    17,    -1,    22,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,    97,    97,   100,    95,   113,   114,   116,   117,   119,
     120,   122,   128,   139,   147,   163,   165,   166,   167,   169,
     170,   172,   172,   174,   175,   177,   179,   181,   182,   183,
     184,   185,   186,   188,   189,   192,   191,   230,   232,   233,
     235,   236,   238,   260,   259,   304,   305,   311,   321,   332,
     333,   335,   344,   350,   363,   369,   375,   381,   387,   395,
     396,   397,   400,   401,   404,   403,   473,   474,   477,   477,
     531,   559,   617,   645,   673,   701,   729,   760
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "NC_UNLIMITED_K", "BYTE_K", "CHAR_K",
  "SHORT_K", "INT_K", "FLOAT_K", "DOUBLE_K", "IDENT", "TERMSTRING",
  "BYTE_CONST", "CHAR_CONST", "SHORT_CONST", "INT_CONST", "FLOAT_CONST",
  "DOUBLE_CONST", "DIMENSIONS", "VARIABLES", "NETCDF", "DATA", "FILLVALUE",
  "'{'", "'}'", "';'", "','", "'='", "'('", "')'", "':'", "$accept",
  "ncdesc", "@1", "@2", "dimsection", "dimdecls", "dimdecline", "dimdecl",
  "dimd", "dim", "vasection", "vadecls", "vadecl", "gattdecls", "gattdecl",
  "vardecl", "type", "varlist", "varspec", "@3", "var", "dimspec",
  "dimlist", "vdim", "attdecl", "@4", "att", "avar", "attr", "attvallist",
  "aconst", "attconst", "datasection", "datadecls", "datadecl", "@5",
  "constlist", "dconst", "@6", "const", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   123,   125,    59,    44,    61,    40,    41,
      58
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    31,    33,    34,    32,    35,    35,    36,    36,    37,
      37,    38,    38,    38,    39,    40,    41,    41,    41,    42,
      42,    43,    43,    44,    44,    45,    46,    47,    47,    47,
      47,    47,    47,    48,    48,    50,    49,    51,    52,    52,
      53,    53,    54,    56,    55,    57,    57,    58,    59,    60,
      60,    61,    62,    62,    62,    62,    62,    62,    62,    63,
      63,    63,    64,    64,    66,    65,    67,    67,    69,    68,
      70,    70,    70,    70,    70,    70,    70,    70
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     0,     0,     8,     0,     2,     2,     3,     1,
       3,     3,     3,     3,     1,     1,     0,     2,     1,     2,
       3,     1,     1,     2,     3,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     3,     0,     3,     1,     0,     3,
       1,     3,     1,     0,     4,     3,     2,     1,     1,     1,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       2,     1,     2,     3,     0,     4,     1,     3,     0,     2,
       1,     1,     1,     1,     1,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
       0,     0,     0,     2,     1,     5,     0,    16,    15,     6,
       0,     9,     0,    14,    37,     0,     0,     3,    18,     0,
      47,    25,    43,     0,     0,     7,     0,     0,    27,    28,
      29,    30,    31,    32,    17,     0,    21,     0,    22,    48,
      46,    59,     0,    23,     0,     0,     8,    10,    13,    11,
      12,     0,    19,    26,    33,    35,    61,     0,    24,     0,
      45,    20,     0,    38,    64,    60,     0,     4,    53,    54,
      52,    55,    56,    57,    58,    44,    49,    51,    34,     0,
      36,     0,     0,    62,     0,    42,     0,    40,    68,    63,
      50,     0,    39,    65,    66,     0,    41,    68,    71,    72,
      70,    73,    74,    75,    76,    77,    69,    67
};

/* YYDEFGOTO[NTERM-NUM]. */
static const yysigned_char yydefgoto[] =
{
      -1,     2,     5,    41,     7,     9,    10,    11,    12,    13,
      17,    34,    35,    18,    19,    36,    37,    53,    54,    63,
      20,    80,    86,    87,    21,    44,    22,    23,    40,    75,
      76,    77,    57,    65,    66,    81,    93,    94,    95,   106
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -69
static const yysigned_char yypact[] =
{
       1,     4,    30,   -69,   -69,    23,    32,    -2,   -69,    32,
     -12,   -69,    13,   -69,   -69,    -4,    33,   -69,    -1,    27,
     -69,   -69,   -69,    24,    -6,   -69,    32,     7,   -69,   -69,
     -69,   -69,   -69,   -69,    -4,    28,   -69,    45,   -69,   -69,
     -69,    35,    36,   -69,    31,    33,   -69,   -69,   -69,   -69,
     -69,    37,   -69,    38,   -69,   -69,    45,    39,   -69,    34,
     -69,   -69,    45,    29,   -69,    45,    40,   -69,   -69,   -69,
     -69,   -69,   -69,   -69,   -69,    41,   -69,   -69,   -69,    32,
     -69,    42,    43,   -69,    34,   -69,   -11,   -69,   -69,   -69,
     -69,    32,   -69,    44,   -69,    22,   -69,   -69,   -69,   -69,
     -69,   -69,   -69,   -69,   -69,   -69,   -69,   -69
};

/* YYPGOTO[NTERM-NUM].  */
static const yysigned_char yypgoto[] =
{
     -69,   -69,   -69,   -69,   -69,   -69,    50,    46,   -69,   -68,
     -69,   -69,    26,   -69,    48,   -69,   -69,   -69,     9,   -69,
     -30,   -69,   -69,   -18,    -3,   -69,   -69,   -40,    47,   -69,
     -10,   -69,   -69,   -69,    10,   -69,   -69,   -21,   -69,   -69
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const unsigned char yytable[] =
{
      28,    29,    30,    31,    32,    33,    14,    55,    14,    14,
      48,    85,    38,    25,    26,    91,    64,    15,    92,    46,
      26,     1,    49,    85,    50,    64,    16,     3,    16,    16,
       4,    38,    55,    98,    99,   100,   101,   102,   103,   104,
      27,     6,     8,    39,   105,    68,    69,    70,    71,    72,
      73,    74,    43,    52,    45,    14,    56,    79,    59,    24,
      51,    58,    61,    67,    62,    83,    42,    84,    89,    88,
      97,    78,    47,    96,    90,    82,   107,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    60
};

static const yysigned_char yycheck[] =
{
       4,     5,     6,     7,     8,     9,    10,    37,    10,    10,
       3,    79,    15,    25,    26,    26,    56,    19,    29,    25,
      26,    20,    15,    91,    17,    65,    30,    23,    30,    30,
       0,    34,    62,    11,    12,    13,    14,    15,    16,    17,
      27,    18,    10,    10,    22,    11,    12,    13,    14,    15,
      16,    17,    25,    25,    30,    10,    21,    28,    27,     9,
      34,    25,    25,    24,    26,    25,    18,    26,    25,    27,
      26,    62,    26,    91,    84,    65,    97,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    45
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,    20,    32,    23,     0,    33,    18,    35,    10,    36,
      37,    38,    39,    40,    10,    19,    30,    41,    44,    45,
      51,    55,    57,    58,    37,    25,    26,    27,     4,     5,
       6,     7,     8,     9,    42,    43,    46,    47,    55,    10,
      59,    34,    45,    25,    56,    30,    25,    38,     3,    15,
      17,    43,    25,    48,    49,    51,    21,    63,    25,    27,
      59,    25,    26,    50,    58,    64,    65,    24,    11,    12,
      13,    14,    15,    16,    17,    60,    61,    62,    49,    28,
      52,    66,    65,    25,    26,    40,    53,    54,    27,    25,
      61,    26,    29,    67,    68,    69,    54,    26,    11,    12,
      13,    14,    15,    16,    17,    22,    70,    68
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

/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)		\
   ((Current).first_line   = (Rhs)[1].first_line,	\
    (Current).first_column = (Rhs)[1].first_column,	\
    (Current).last_line    = (Rhs)[N].last_line,	\
    (Current).last_column  = (Rhs)[N].last_column)
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

# define YYDSYMPRINT(Args)			\
do {						\
  if (yydebug)					\
    yysymprint Args;				\
} while (0)

# define YYDSYMPRINTF(Title, Token, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Token, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short *bottom, short *top)
#else
static void
yy_stack_print (bottom, top)
    short *bottom;
    short *top;
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
# define YYDSYMPRINT(Args)
# define YYDSYMPRINTF(Title, Token, Value, Location)
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

#if defined (YYMAXDEPTH) && YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

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
    {
      YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
# ifdef YYPRINT
      YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
    }
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

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
yydestruct (int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yytype, yyvaluep)
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

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



/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
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
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

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
	short *yyss1 = yyss;


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
	short *yyss1 = yyss;
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
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
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
      YYDSYMPRINTF ("Next token is", yytoken, &yylval, &yylloc);
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

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %s, ", yytname[yytoken]));

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
#line 97 "./ncgen.y"
    { init_netcdf(); }
    break;

  case 3:
#line 100 "./ncgen.y"
    {
		       if (derror_count == 0)
			 define_netcdf(netcdfname);
		       if (derror_count > 0)
		       	exit(6);
		   }
    break;

  case 4:
#line 108 "./ncgen.y"
    {
		       if (derror_count == 0)
			 close_netcdf();
		   }
    break;

  case 11:
#line 123 "./ncgen.y"
    { if (int_val <= 0)
			 derror("dimension length must be positive");
		     dims[ndims].size = int_val;
		     ndims++;
		   }
    break;

  case 12:
#line 129 "./ncgen.y"
    { /* for rare case where 2^31 < dimsize < 2^32 */
		   	if (double_val <= 0)
				derror("dimension length must be positive");
			if (double_val > 4294967295.0)
				derror("dimension too large");
			if (double_val - (size_t) double_val > 0)
				derror("dimension length must be an integer");
			dims[ndims].size = (size_t) double_val;
			ndims++;
		   }
    break;

  case 13:
#line 140 "./ncgen.y"
    {  if (rec_dim != -1)
			 derror("only one NC_UNLIMITED dimension allowed");
		     rec_dim = ndims; /* the unlimited (record) dimension */
		     dims[ndims].size = NC_UNLIMITED;
		     ndims++;
		   }
    break;

  case 14:
#line 148 "./ncgen.y"
    { if (yyvsp[0]->is_dim == 1) {
		        derror( "duplicate dimension declaration for %s",
		                yyvsp[0]->name);
		     }
	             yyvsp[0]->is_dim = 1;
		     yyvsp[0]->dnum = ndims;
		     /* make sure dims array will hold dimensions */
		     grow_darray(ndims,  /* must hold ndims+1 dims */
				 &dims); /* grow as needed */
		     dims[ndims].name = (char *) emalloc(strlen(yyvsp[0]->name)+1);
		     (void) strcpy(dims[ndims].name, yyvsp[0]->name);
		     /* name for use in generated Fortran and C variables */
		     dims[ndims].lname = decodify(yyvsp[0]->name);
		   }
    break;

  case 27:
#line 181 "./ncgen.y"
    { type_code = NC_BYTE; }
    break;

  case 28:
#line 182 "./ncgen.y"
    { type_code = NC_CHAR; }
    break;

  case 29:
#line 183 "./ncgen.y"
    { type_code = NC_SHORT; }
    break;

  case 30:
#line 184 "./ncgen.y"
    { type_code = NC_INT; }
    break;

  case 31:
#line 185 "./ncgen.y"
    { type_code = NC_FLOAT; }
    break;

  case 32:
#line 186 "./ncgen.y"
    { type_code = NC_DOUBLE; }
    break;

  case 35:
#line 192 "./ncgen.y"
    {
		    static struct vars dummyvar;

		    dummyvar.name = "dummy";
		    dummyvar.type = NC_DOUBLE;
		    dummyvar.ndims = 0;
		    dummyvar.dims = 0;
		    dummyvar.fill_value.doublev = NC_FILL_DOUBLE;
		    dummyvar.has_data = 0;

		    nvdims = 0;
		    /* make sure variable not re-declared */
		    if (yyvsp[0]->is_var == 1) {
		       derror( "duplicate variable declaration for %s",
		               yyvsp[0]->name);
		    }
	            yyvsp[0]->is_var = 1;
		    yyvsp[0]->vnum = nvars;
		    /* make sure vars array will hold variables */
		    grow_varray(nvars,  /* must hold nvars+1 vars */
				&vars); /* grow as needed */
		    vars[nvars] = dummyvar; /* to make Purify happy */
		    vars[nvars].name = (char *) emalloc(strlen(yyvsp[0]->name)+1);
		    (void) strcpy(vars[nvars].name, yyvsp[0]->name);
		    /* name for use in generated Fortran and C variables */
		    vars[nvars].lname = decodify(yyvsp[0]->name);
		    vars[nvars].type = type_code;
		    /* set default fill value.  You can override this with
		     * the variable attribute "_FillValue". */
		    nc_getfill(type_code, &vars[nvars].fill_value);
		    vars[nvars].has_data = 0; /* has no data (yet) */
		   }
    break;

  case 36:
#line 225 "./ncgen.y"
    {
		    vars[nvars].ndims = nvdims;
		    nvars++;
		   }
    break;

  case 42:
#line 239 "./ncgen.y"
    {
		    if (nvdims >= NC_MAX_VAR_DIMS) {
		       derror("%s has too many dimensions",vars[nvars].name);
		    }
		    if (yyvsp[0]->is_dim == 1)
		       dimnum = yyvsp[0]->dnum;
		    else {
		       derror( "%s is not declared as a dimension",
			       yyvsp[0]->name);
	               dimnum = ndims;
		    }
		    if (rec_dim != -1 && dimnum == rec_dim && nvdims != 0) {
		       derror("unlimited dimension must be first");
		    }
		    grow_iarray(nvdims, /* must hold nvdims+1 ints */
				&vars[nvars].dims); /* grow as needed */
		    vars[nvars].dims[nvdims] = dimnum;
                    nvdims++;
		   }
    break;

  case 43:
#line 260 "./ncgen.y"
    {
		       valnum = 0;
		       valtype = NC_UNSPECIFIED;
		       /* get a large block for attributes, realloc later */
		       att_space = emalloc(MAX_NC_ATTSIZE);
		       /* make all kinds of pointers point to it */
		       char_valp = (char *) att_space;
		       byte_valp = (signed char *) att_space;
		       short_valp = (short *) att_space;
		       int_valp = (int *) att_space;
		       float_valp = (float *) att_space;
		       double_valp = (double *) att_space;
		   }
    break;

  case 44:
#line 274 "./ncgen.y"
    {
		       {	/* check if duplicate attribute for this var */
			   int i;
			   for(i=0; i<natts; i++) { /* expensive */
			       if(atts[i].var == varnum &&
				  STREQ(atts[i].name,atts[natts].name)) {
				   derror("duplicate attribute %s:%s",
					  vars[varnum].name,atts[natts].name);
			       }
			   }
		       }
		       atts[natts].var = varnum ;
		       atts[natts].type = valtype;
		       atts[natts].len = valnum;
		       /* shrink space down to what was really needed */
		       att_space = erealloc(att_space, valnum*nctypesize(valtype));
		       atts[natts].val = att_space;
		       if (STREQ(atts[natts].name, _FillValue) &&
			   atts[natts].var != NC_GLOBAL) {
			   nc_putfill(atts[natts].type,
				       atts[natts].val,
				       &vars[atts[natts].var].fill_value);
			   if(atts[natts].type != vars[atts[natts].var].type) {
			       derror("variable %s: %s type mismatch",
				      vars[atts[natts].var].name, _FillValue);
			   }
		       }
		       natts++;
		   }
    break;

  case 46:
#line 306 "./ncgen.y"
    {
		    varnum = NC_GLOBAL;  /* handle of "global" attribute */
		   }
    break;

  case 47:
#line 312 "./ncgen.y"
    { if (yyvsp[0]->is_var == 1)
		       varnum = yyvsp[0]->vnum;
		    else {
		      derror("%s not declared as a variable, fatal error",
			     yyvsp[0]->name);
		      YYABORT;
		      }
		   }
    break;

  case 48:
#line 322 "./ncgen.y"
    {
		       /* make sure atts array will hold attributes */
		       grow_aarray(natts,  /* must hold natts+1 atts */
				   &atts); /* grow as needed */
		       atts[natts].name = (char *) emalloc(strlen(yyvsp[0]->name)+1);
		       (void) strcpy(atts[natts].name,yyvsp[0]->name);
		       /* name for use in generated Fortran and C variables */
		       atts[natts].lname = decodify(yyvsp[0]->name);
		   }
    break;

  case 51:
#line 336 "./ncgen.y"
    {
		    if (valtype == NC_UNSPECIFIED)
		      valtype = atype_code;
		    if (valtype != atype_code)
		      derror("values for attribute must be all of same type");
		   }
    break;

  case 52:
#line 345 "./ncgen.y"
    {
		       atype_code = NC_CHAR;
		       *char_valp++ = char_val;
		       valnum++;
		   }
    break;

  case 53:
#line 351 "./ncgen.y"
    {
		       atype_code = NC_CHAR;
		       {
			   /* don't null-terminate attribute strings */
			   size_t len = strlen(termstring);
			   if (len == 0) /* need null if that's only value */
			       len = 1;
			   (void)strncpy(char_valp,termstring,len);
			   valnum += len;
			   char_valp += len;
		       }
		   }
    break;

  case 54:
#line 364 "./ncgen.y"
    {
		       atype_code = NC_BYTE;
		       *byte_valp++ = byte_val;
		       valnum++;
		   }
    break;

  case 55:
#line 370 "./ncgen.y"
    {
		       atype_code = NC_SHORT;
		       *short_valp++ = short_val;
		       valnum++;
		   }
    break;

  case 56:
#line 376 "./ncgen.y"
    {
		       atype_code = NC_INT;
		       *int_valp++ = int_val;
		       valnum++;
		   }
    break;

  case 57:
#line 382 "./ncgen.y"
    {
		       atype_code = NC_FLOAT;
		       *float_valp++ = float_val;
		       valnum++;
		   }
    break;

  case 58:
#line 388 "./ncgen.y"
    {
		       atype_code = NC_DOUBLE;
		       *double_valp++ = double_val;
		       valnum++;
		   }
    break;

  case 64:
#line 404 "./ncgen.y"
    {
		       valtype = vars[varnum].type; /* variable type */
		       valnum = 0;	/* values accumulated for variable */
		       vars[varnum].has_data = 1;
		       /* compute dimensions product */
		       var_size = nctypesize(valtype);
		       if (vars[varnum].ndims == 0) { /* scalar */
			   var_len = 1;
		       } else if (vars[varnum].dims[0] == rec_dim) {
			   var_len = 1; /* one record for unlimited vars */
		       } else {
			   var_len = dims[vars[varnum].dims[0]].size;
		       }
		       for(dimnum = 1; dimnum < vars[varnum].ndims; dimnum++)
			 var_len = var_len*dims[vars[varnum].dims[dimnum]].size;
		       /* allocate memory for variable data */
		       if (var_len*var_size != (size_t)(var_len*var_size)) {
			   derror("variable %s too large for memory",
				  vars[varnum].name);
			   exit(9);
		       }
		       rec_len = var_len;
		       rec_start = malloc ((size_t)(rec_len*var_size));
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
			   byte_valp = (signed char *) rec_start;
			   break;
			 case NC_SHORT:
			   short_valp = (short *) rec_start;
			   break;
			 case NC_INT:
			   int_valp = (int *) rec_start;
			   break;
			 case NC_FLOAT:
			   float_valp = (float *) rec_start;
			   break;
			 case NC_DOUBLE:
			   double_valp = (double *) rec_start;
			   break;
			 default:
			   derror("Unhandled type %d\n", valtype);
			   break;

		       }
		 }
    break;

  case 65:
#line 458 "./ncgen.y"
    {
		       if (valnum < var_len) { /* leftovers */
			   nc_fill(valtype,
				    var_len - valnum,
				    rec_cur,
				    vars[varnum].fill_value);
		       }
		       /* put out var_len values */
		       /* vars[varnum].nrecs = valnum / rec_len; */
		       vars[varnum].nrecs = var_len / rec_len;
		       if (derror_count == 0)
			   put_variable(rec_start);
		       free ((char *) rec_start);
		 }
    break;

  case 68:
#line 477 "./ncgen.y"
    {
		       if(valnum >= var_len) {
			   if (vars[varnum].dims[0] != rec_dim) { /* not recvar */
			       derror("too many values for this variable, %d >= %d",
				      valnum, var_len);
			       exit (4);
			   } else { /* a record variable, so grow data
				      container and increment var_len by
				      multiple of record size */
			       ptrdiff_t rec_inc = (char *)rec_cur
				   - (char *)rec_start;
			       var_len += rec_len * (1 + valnum )/rec_len;
			       rec_start = erealloc(rec_start, var_len*var_size);
			       rec_cur = (char *)rec_start + rec_inc;
			       char_valp = (char *) rec_cur;
			       byte_valp = (signed char *) rec_cur;
			       short_valp = (short *) rec_cur;
			       int_valp = (int *) rec_cur;
			       float_valp = (float *) rec_cur;
			       double_valp = (double *) rec_cur;
			   }
		       }
		       not_a_string = 1;
                   }
    break;

  case 69:
#line 502 "./ncgen.y"
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
			     case NC_INT:
			       rec_cur = (void *) int_valp;
			       break;
			     case NC_FLOAT:
			       rec_cur = (void *) float_valp;
			       break;
			     case NC_DOUBLE:
			       rec_cur = (void *) double_valp;
			       break;
			     default: 
			       derror("Unhandled type %d\n", valtype);
			       break;
			   }
		       }
		   }
    break;

  case 70:
#line 532 "./ncgen.y"
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
			 case NC_INT:
			   *int_valp++ = char_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = char_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = char_val;
			   break; 
			 default: 
			   derror("Unhandled type %d\n", valtype);
			   break;
		       }
		       valnum++;
		   }
    break;

  case 71:
#line 560 "./ncgen.y"
    {
		       not_a_string = 0;
		       atype_code = NC_CHAR;
		       {
			   size_t len = strlen(termstring);

			   if(valnum + len > var_len) {
			       if (vars[varnum].dims[0] != rec_dim) {
				   derror("too many values for this variable, %d>%d", 
					  valnum+len, var_len);
				   exit (5);
			       } else {/* a record variable so grow it */
				   ptrdiff_t rec_inc = (char *)rec_cur
				       - (char *)rec_start;
				   var_len += rec_len * (len + valnum - var_len)/rec_len;
				   rec_start = erealloc(rec_start, var_len*var_size);
				   rec_cur = (char *)rec_start + rec_inc;
				   char_valp = (char *) rec_cur;
			       }
			   }
			   switch (valtype) {
			     case NC_CHAR:
			       {
				   int ld;
				   size_t i, sl;
				   (void)strncpy(char_valp,termstring,len);
				   
				   ld = vars[varnum].ndims-1;
				   if (ld > 0) {/* null-fill to size of last dim */
				       sl = dims[vars[varnum].dims[ld]].size;
				       for (i =len;i<sl;i++)
					   char_valp[i] = '\0';
				       if (sl < len)
					   sl = len;
				       valnum += sl;
				       char_valp += sl;
				   } else { /* scalar or 1D strings */
				       valnum += len;
				       char_valp += len;
				   }
				   rec_cur = (void *) char_valp;
			       }
			       break;
			     case NC_BYTE:
			     case NC_SHORT:
			     case NC_INT:
			     case NC_FLOAT:
			     case NC_DOUBLE:
			       derror("string value invalid for %s variable",
				      nctype(valtype));
			       break;
			     default: 
			       derror("Unhandled type %d\n", valtype);
			       break;
			   }
		       }
		   }
    break;

  case 72:
#line 618 "./ncgen.y"
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
			 case NC_INT:
			   *int_valp++ = byte_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = byte_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = byte_val;
			   break;
			 default: 
			   derror("Unhandled type %d\n", valtype);
			   break;
		       }
		       valnum++;
		   }
    break;

  case 73:
#line 646 "./ncgen.y"
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
			 case NC_INT:
			   *int_valp++ = short_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = short_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = short_val;
			   break;
			 default: 
			   derror("Unhandled type %d\n", valtype);
			   break;
		       }
		       valnum++;
		   }
    break;

  case 74:
#line 674 "./ncgen.y"
    {
		       atype_code = NC_INT;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = int_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = int_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = int_val;
			   break;
			 case NC_INT:
			   *int_valp++ = int_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = int_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = int_val;
			   break;
			 default: 
			   derror("Unhandled type %d\n", valtype);
			   break;
		       }
		       valnum++;
		   }
    break;

  case 75:
#line 702 "./ncgen.y"
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
			 case NC_INT:
			   *int_valp++ = float_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = float_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = float_val;
			   break;
			 default: 
			   derror("Unhandled type %d\n", valtype);
			   break;
		       }
		       valnum++;
		   }
    break;

  case 76:
#line 730 "./ncgen.y"
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
			 case NC_INT:
			   *int_valp++ = double_val;
			   break;
			 case NC_FLOAT:
			   if (double_val == NC_FILL_DOUBLE)
			     *float_valp++ = NC_FILL_FLOAT;
			   else
			     *float_valp++ = double_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = double_val;
			   break;
			 default: 
			   derror("Unhandled type %d\n", valtype);
			   break;
		       }
		       valnum++;
		   }
    break;

  case 77:
#line 761 "./ncgen.y"
    {
		       /* store fill_value */
		       switch (valtype) {
		       case NC_CHAR:
			   nc_fill(valtype, 1, (void *)char_valp++,
				   vars[varnum].fill_value);
			   break;
		       case NC_BYTE:
			   nc_fill(valtype, 1, (void *)byte_valp++,
				   vars[varnum].fill_value);
			   break;
		       case NC_SHORT:
			   nc_fill(valtype, 1, (void *)short_valp++,
				   vars[varnum].fill_value);
			   break;
		       case NC_INT:
			   nc_fill(valtype, 1, (void *)int_valp++,
				   vars[varnum].fill_value);
			   break;
		       case NC_FLOAT:
			   nc_fill(valtype, 1, (void *)float_valp++,
				   vars[varnum].fill_value);
			   break;
		       case NC_DOUBLE:
			   nc_fill(valtype, 1, (void *)double_valp++,
				   vars[varnum].fill_value);
			   break;
		       default: 
			   derror("Unhandled type %d\n", valtype);
			   break;
		       }
		       valnum++;
		   }
    break;


    }

/* Line 1000 of yacc.c.  */
#line 1924 "y.tab.c"

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
      /* If just tried and failed to reuse lookahead token after an
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
		 YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
		 yydestruct (yystos[*yyssp], yyvsp);
	       }
        }
      else
	{
	  YYDSYMPRINTF ("Error: discarding", yytoken, &yylval, &yylloc);
	  yydestruct (yytoken, &yylval);
	  yychar = YYEMPTY;

	}
    }

  /* Else will try to reuse lookahead token after shifting the error
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

      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
      yydestruct (yystos[yystate], yyvsp);
      YYPOPSTACK;
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;


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


#line 798 "./ncgen.y"


/* PROGRAMS */

#ifdef vms
void
#else
int
#endif
yyerror(	/* called for yacc syntax error */
     char *s)
{
	derror(s);
#ifndef vms
	return -1;
#endif
}

/* undefine yywrap macro, in case we are using bison instead of yacc */
#ifdef yywrap
#undef yywrap
#endif

int
yywrap(void)			/* returns 1 on EOF if no more input */
{
    return  1;
}


/* Symbol table operations for ncgen tool */

YYSTYPE lookup(       /* find sname in symbol table (linear search) */
	const char *sname)
{
    YYSTYPE sp;
    for (sp = symlist; sp != (YYSTYPE) 0; sp = sp -> next)
	if (STREQ(sp -> name, sname)) {
	    return sp;
	}
    return 0;			/* 0 ==> not found */
}

YYSTYPE install(  /* install sname in symbol table */
	const char *sname)
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
clearout(void)	/* reset symbol table to empty */
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

/* get lexical input routine generated by lex  */
#include "ncgenyy.c"

