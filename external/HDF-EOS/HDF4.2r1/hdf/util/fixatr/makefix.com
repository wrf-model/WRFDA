$ v = 'f$verify(0)'
$ if p1 .eqs. "DEBUG" then goto debug
$ x = f$verify(1)
$ fort rformat
$ fort command
$ macro parse
$ link rformat,command,parse
$ x = 'f$verify(0)'
$ goto 10
$ debug:
$ x = f$verify(1)
$ fort/lis/deb/noopt rformat
$ fort/lis/deb/noopt command
$ macro/lis/deb parse
$ link/deb rformat,command,parse
$ x = 'f$verify(0)'
$ 10:
$ v = f$verify(v)
$ ! put the help in the library
$ library/create/help fixatr.hlb fixatr.hlp
