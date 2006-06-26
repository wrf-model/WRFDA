$! If you have lex and yacc for VMS (e.g. with DEC Shell) this rebuilds
$! vms_yy.c, vmstab.c, and vmstab.h from ncgen.l and
$! ncgen.y.  If you don't have lex and yacc, just use the *-vms files provided
$! with the distribution.
$!
$ lex ncgen.l
$ rename lex_yy.c vms_yy.c
$ yacc -d ncgen.y
$ rename y_tab.c vmstab.c
$ rename y_tab.h vmstab.h
