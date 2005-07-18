#include <stdio.h>
#include <fcntl.h>

#ifndef CRAY
# ifdef NOUNDERSCORE
#      define OPEN_FILE open_file
#      define CLOSE_FILE close_file
#      define WRITE_FILE write_file
#      define FLUSH_FILE flush_file
# else
#   ifdef F2CSTYLE
#      define OPEN_FILE open_file__
#      define CLOSE_FILE close_file__
#      define WRITE_FILE write_file__
#      define FLUSH_FILE flush_file__
#   else
#      define OPEN_FILE open_file_
#      define CLOSE_FILE close_file_
#      define WRITE_FILE write_file_
#      define FLUSH_FILE flush_file_
#   endif
# endif
#endif

/* 
 * Fortran-callable function to open/close files
 */
int OPEN_FILE (char *filename, char *permissions, int *outfd, int *ierr, 
	       int strlen1, int strlen2)
{
  char filename2[1000];
  char permstring[1000];
  int permvals;

  strncpy(filename2,filename,strlen1);
  filename2[strlen1]='\0';

  strncpy(permstring,permissions,strlen2);
  permstring[strlen2]='\0';

  if (strcmp(permstring,"w") == 0) {
    permvals = O_CREAT|O_WRONLY|O_TRUNC;
  } else {
    permvals = O_RDONLY;
  }

  *outfd = open(filename2,permvals,0644);
  if (*outfd == -1) 
    {
      fprintf(stderr,"setting ierr to -1, filename: %s\n",filename);
      perror("");
      *ierr = -1;
      return -1;
    }
  else
    {
      *ierr = 0;
      return 0;
    }
}

int WRITE_FILE(int *fd, char *buf, int *ierr, int strlen)
{
  int nbytes;
  nbytes = write(*fd,buf,strlen);
  if (nbytes != strlen)
    {
      *ierr = -1;
    }
  else
    {
      *ierr = 0;
    }
  return *ierr;
}

int CLOSE_FILE (int *fd)
{
  close(*fd);
  return 0;
}

int FLUSH_FILE (int *fd)
{
  fsync(*fd);
  return 0;
}

