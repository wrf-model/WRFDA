$! --------------------------------------------------------------------------
$! For making GETOPT.OBJ on VMS if you don't have MMS.
$! --------------------------------------------------------------------------
$!
$! $Id: make.com,v 1.1 1993/05/03 21:27:01 chouck Exp $
$
$ ccc := cc /opt/nodebug/nolist
$
$ ccc GETOPT.C
$
