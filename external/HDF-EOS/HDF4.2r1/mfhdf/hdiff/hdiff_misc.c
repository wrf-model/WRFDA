#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>



#include "hdf.h"
#include "mfhdf.h"

#include "hdiff.h"

/* 
* convert pathname of netcdf file into name for cdl unit, by taking 
* last component of path and stripping off any extension.
*/
char *
name_path(char *path)
{
 char *cp, *newc;
 
#ifdef vms
#define FILE_DELIMITER ']'
#endif    
#ifdef MSDOS
#define FILE_DELIMITER '\\'
#endif    
#ifndef FILE_DELIMITER /* default to unix */
#define FILE_DELIMITER '/'
#endif
 cp = strrchr(path, FILE_DELIMITER);
 if (cp == 0)  /* no delimiter */
  cp = path;
 else   /* skip delimeter */
  cp++;
 newc = (char *) malloc((unsigned) (strlen(cp)+1));
 if (newc == 0) {
  fprintf(stderr,"Out of memory!\n");
  exit(EXIT_FAILURE);
 }
 (void) strcpy(newc, cp); /* copy last component of path */
 if ((cp = strrchr(newc, '.')) != NULL)
  *cp = '\0';  /* strip off any extension */
 return newc;
}


char *
type_name(nc_type type)
{
 switch (type) {
 case DFNT_INT8:
  return "byte";
 case DFNT_CHAR:
  return "char";
 case DFNT_INT16:
  return "short";
 case DFNT_INT32:
  return "long";
 case DFNT_FLOAT:
  return "float";
 case DFNT_DOUBLE:
  return "double";
 default:
  fprintf(stderr,"type_name: bad type %d", type);
  return "bogus";
 }
}

/*
* Remove trailing zeros (after decimal point) but not trailing decimal
* point from ss, a string representation of a floating-point number that
* might include an exponent part.
*/
void
tztrim(char *ss)
  /* returned string representing dd */
{
 char *cp, *ep;
 
 cp = ss;
 if (*cp == '-')
  cp++;
 while(isdigit((int)*cp) || *cp == '.')
  cp++;
 if (*--cp == '.')
  return;
 ep = cp+1;
 while (*cp == '0')
  cp--;
 cp++;
 if (cp == ep)
  return;
 while (*ep)
  *cp++ = *ep++;
 *cp = '\0';
 return;
}


/*
* Print list of attribute values.  Attribute values must be printed with
* explicit type tags, because their types are not declared.
*/
void
pr_att_vals(nc_type type, int len, void *vals)
{
 int iel;
 union {
  char *cp;
  int16 *sp;
  int32 *lp;
  float32 *fp;
  float64 *dp;
 } gp;
 char *sp;
 unsigned char uc;
 char gps[30];  /* for ascii of a float or double precision */
 char *f_fmt = "%#.8g";
 char *d_fmt = "%#.16g";
 
 switch (type) {
 case DFNT_INT8:
  gp.cp = (char *) vals;
  for (iel = 0; iel < len; iel++)
   if (isprint(uc = *gp.cp++ & 0377))
    Printf ("'%c'%s", uc, iel<len-1 ? ", " : "");
   else
    Printf ("'\\%o'%s", uc, iel<len-1 ? ", " : "");
   break;
 case DFNT_CHAR:
  gp.cp = (char *) vals;
  Printf ("\"");
  /* adjust len so trailing nulls don't get printed */
  sp = gp.cp + len - 1;
  while (*sp-- == '\0' && len > 0)
   len--;
  for (iel = 0; iel < len; iel++)
   switch (uc = *gp.cp++ & 0377) {
 case '\b':
  Printf ("\\b");
  break;
 case '\f':
  Printf ("\\f");
  break;
 case '\n':  /* generate linebreaks after new-lines */
  Printf ("\\n\",\n    \"");
  break;
 case '\r':
  Printf ("\\r");
  break;
 case '\t':
  Printf ("\\t");
  break;
 case '\v':
  Printf ("\\v");
  break;
 case '\\':
  Printf ("\\\\");
  break;
 case '\'':
  Printf ("\\'");
  break;
 case '\"':
  Printf ("\\\"");
  break;
 default:
  Printf ("%c",uc);
  break;
  }
  Printf ("\"");
  break;
 case DFNT_INT16:
  gp.sp = (int16 *) vals;
  for (iel = 0; iel < len; iel++)
   Printf ("%ds%s",*gp.sp++,iel<len-1 ? ", " : "");
  break;
 case DFNT_INT32:
  gp.lp = (int32 *) vals;
  for (iel = 0; iel < len; iel++)
   Printf ("%d%s",*gp.lp++,iel<len-1 ? ", " : "");
  break;
 case DFNT_FLOAT:
  gp.fp = (float32 *) vals;
  for (iel = 0; iel < len; iel++) {
   int ll;
   (void) sprintf(gps, f_fmt, * gp.fp++);
   /* append a trailing "f" for floating-point attributes */
   ll = strlen(gps);
   gps[ll + 1] = '\0';
   gps[ll] = 'f';
   tztrim(gps); /* trim trailing 0's after '.' */
   Printf ("%s%s", gps, iel<len-1 ? ", " : "");
  }
  break;
 case DFNT_DOUBLE:
  gp.dp = (float64 *) vals;
  for (iel = 0; iel < len; iel++) {
   (void) sprintf(gps, d_fmt, *gp.dp++);
   tztrim(gps); /* trim trailing 0's after '.' */
   Printf ("%s%s", gps, iel<len-1 ? ", " : "");
  }
  break;
 default:
  fprintf(stderr,"pr_att_vals: bad type - %d", type);
 }
}



void
make_vars(char *optarg, diff_opt_t *opt, int option)
{
 char *cp = optarg;
 int nvars = 1;
 char ** cpp;
 
 /* compute number of variable names in comma-delimited list */
 if (option == 1)
  opt->nlvars = 1;
 else
  opt->nuvars = 1;
 
 while (*cp++)
  if (*cp == ',')
   nvars++;
  
  if (option == 1)
  {
   opt->lvars = (char **) malloc(nvars * sizeof(char*));
   if (!opt->lvars) {
    fprintf(stderr,"Out of memory!\n");
    exit(EXIT_FAILURE);
   }
   cpp = opt->lvars;
  }
  else
  {
   opt->uvars = (char **) malloc(nvars * sizeof(char*));
   if (!opt->uvars) {
    fprintf(stderr,"Out of memory!\n");
    exit(EXIT_FAILURE);
   }
   cpp = opt->uvars;
  }
  
  /* copy variable names into list */
  for (cp = strtok(optarg, ",");
  cp != NULL;
  cp = strtok((char *) NULL, ",")) {
   
   *cpp = (char *) malloc(strlen(cp) + 1);
   if (!*cpp) {
    fprintf(stderr,"Out of memory!\n");
    exit(EXIT_FAILURE);
   }
   strcpy(*cpp, cp);
   cpp++;
  }
  if (option == 1)
   opt->nlvars = nvars;
  else
   opt->nuvars = nvars;
}

