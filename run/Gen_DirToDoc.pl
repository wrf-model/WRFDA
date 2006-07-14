#!/usr/bin/env perl

$USAGE="Gen_DirToDoc.pl source_directory sink_directory";

open (ERR,">&2");

if ($#ARGV != 1){
  print ERR "Gen_DirToDoc.pl: Wrong number of arguments\n";
  print ERR "USAGE: $USAGE\n";
  die ;
}

#if ($ENV{"GEN_ROOTDIR"} eq "") {
#  print ERR "Gen_DirToDoc.pl GEN_ROOTDIR not defined\n";
#  die ;
#}

$USER=$ENV{"USER"};
#$GEN_HOSTNAME=$ENV{"GEN_HOSTNAME"};
#$GEN_ROOTDIR=$ENV{"GEN_ROOTDIR"};
#$GEN_STYLESHEET=$ENV{"GEN_STYLESHEET"};
#$GEN_JAVASCRIPT=$ENV{"GEN_JAVASCRIPT"};
$GEN_STYLESHEET="StyleSheet.css";
$GEN_JAVASCRIPT="Documentation.js";

$SOURCEDIR=$ARGV[0];
$SINKDIR=$ARGV[1];
print "\n Converting $SOURCEDIR to $SINKDIR\n";

opendir(SOURCE,$SOURCEDIR) || die "Can't open $SOURCEDIR: $!\n";
@files=grep(/(\.f90$|\.F90$|\.inc$|\.f$|\.F$|\.dk$|\.cdk$|\.c$|\.h$|\.tcl$|\.pro$|\.proc$|\.pan$|\.pl$|\.elements$|Scr_|Sql_|IF_|Task_|Suite_|List_)/, readdir(SOURCE));
closedir(SOURCE);

$DATE=`date`;

if ($#files >= 0) {

  foreach $file (@files){

    $Infile="$SOURCEDIR/$file";

#    $SHORTDIR=$SOURCEDIR;
#    $SHORTDIR=~s/$ENV{"GEN_ROOTDIR"}\/// ;
    $SHORT_FILE=$file;
    $NAME=$file;
    $NAME=~s/(\.f90$|\.F90$|\.inc$|\.f$|\.F$|\.dk$|\.cdk$|\.c$|\.h$|\.tcl$|\.pro$|\.proc$|\.pan$|\.pl$|\.elements$)//;

    $OUT_HTML="$SINKDIR/$NAME.html";

    if (! -e $OUT_HTML || -M $Infile < -M $OUT_HTML) {
    
#      $Outfile=`GenScr_TempFile`;
#      chop ($Outfile);

      print "Updating $NAME.html\n";

      `rm -rf $OUT_HTML`;
      open (IN, "$Infile") || die "unable to open $Infile: $!\n";
      open (OUT,">$OUT_HTML") || die "unable to open $OUT_HTML: $!\n";

#      `rm -f $SINKDIR/$NAME.html`;
#      $BaseRoot=`basename $ENV{"GEN_ROOTDIR"}`;
#      chomp $BaseRoot;

      print OUT "<!DOCTYPE HTML PUBLIC \"-//W3C/DTD HTML 4.01//EN\">\n" ;
      print OUT "<HTML>\n"  ;
      print OUT "<HEAD>\n"  ;
      print OUT "<TITLE>$SHORT_FILE</TITLE>\n"  ;
      if ( ! $GEN_STYLESHEET == "" ) {
        print OUT "<LINK REL=\"stylesheet\" TYPE=\"text/css\"\n";
        print OUT "HREF=\"/$GEN_STYLESHEET\">\n";
      }
      print OUT "</HEAD>\n" ;
      print OUT "<BODY>\n" ;
      print OUT "<H1>$SHORT_FILE</H1> \n" ;
      if ( ! $GEN_JAVASCRIPT == "" ) {
        print OUT "<SCRIPT TYPE=\"script/javascript\" LANGUAGE=\"JavaScript\" SRC=\"/$GEN_JAVASCRIPT\"></SCRIPT>\n" ;
      }
#      print OUT "Produced from <A HREF=\"http://$GEN_HOSTNAME/~$USER/$BaseRoot/$SHORTDIR/$SHORT_FILE\">$GEN_ROOTDIR/$SHORTDIR/$SHORT_FILE</A><BR>\n" ;
      print OUT "<A HREF=\"Refs.$NAME.html\">References</A> to this file elsewhere.\n" ;
      print OUT "<PRE>\n" ;

      $i=0;
      while (<IN>) {
        chomp ($_);
        $i++;
        s/\&/\&amp\;/g ;
        s/</\&lt\;/g ;
        s/>/\&gt\;/g ;
        s/( )URL ([^ ,]*)/\1<A HREF="\2">\2<\/a>/;
        s/(.*)(UMDP|VSDP|OSDP|VTDP|NVDP|SVDP|OTDP|STDP|GTDP|VWP|NVWP|SVWP|OWP|SWP|GWP|SUDP|DADP|TTDP) ([A-Za-z0-9]*)/\1<A HREF="\2\3.html">\2 \3<\/a>/g ;
        s/^([\. ]*(\#include|include|INCLUDE|Include) [^a-zA-Z\&\;]*)([0-9a-zA-Z_\/]*)([0-9a-zA-Z_\/\.]*)/\1<A HREF="\3.html">\3\4<\/a>/g ;
        s/^( *(%I) [^a-zA-Z]*)([0-9a-zA-Z_\/]*)([0-9a-zA-Z_\/\.]*)/\1<A HREF="\3.html">\3\4<\/a>/g ;
        s/([a-zA-Z]*Prog_[^ ,;\":.\)\&(}]*.exe)/<A HREF="\1.html">\1<\/a>/g ;
        s/(\#.*)/<FONT COLOR="red">\1\2<\/FONT>/g ;
        s/(\#[ ]*IF:) ([0-9a-zA-Z_\/\.]*)/\1 <A HREF="\2.html">\2<\/a>/g ;
        s/(\#[ ]*List:) ([0-9a-zA-Z_\/\.]*)/\1 <A HREF="\2.html">\2<\/a>/g ;
        s/^( *(call|CALL|Call)) [ \(]*([^ (\&\n]*)/\1 <A HREF="\3.html">\3<\/a>/g ;
        s/([)] *(call|CALL|Call)) [ \(]*([^ (\&\n]*)/\1 <A HREF="\3.html">\3<\/a>/ ;
        s/^(\*(call|CALL|Call)) [ \(]*([^ (\&\n]*)/\1 <A HREF="\3.html">\3<\/a>/g ;
        s/( *[Mm]odule:) [ \(]*([^ (\&]*)/\1 <A HREF="\2.html">\2<\/a>/g ;
        s/( *[Pp]rogram:) [ \(]*([^ (\&]*)/\1 <A HREF="\2.html">\2<\/a>/g ;
        s/^( *[Uu][Ss][Ee]) [ \(]*([^ (,\&]*)/\1 <A HREF="\2.html">\2<\/a>/g ;
        s/(\`)([a-zA-Z]*(Scr|IF|Comp|List|Suite)_[^ ,;\"`:\)\&(}]*)/\1<A HREF="\2.html">\2<\/a>/g ;
        s/^([ .]*)([a-zA-Z]*(Scr|IF|Comp|List|Suite)_[^ ,;\"`:\)\&(}]*)/\1<A HREF="\2.html">\2<\/a>/g ;
        s/^([a-zA-Z]*(Scr|IF|Comp|List|Suite)_[^ ,;\"`:\)\&(}]*)/<A HREF="\1.html">\1<\/a>/g ;
        s/[(]c[)]/\&copy;/g ;
        s/exe.html/html/g ;
        s/pl.html/html/g ;
        s/(needs[ ]*|optional[ ]*|provides[ ]*)(DA_[0-9a-zA-Z_]*)/\1<A HREF="http:\/\/manse.johnbray.org.uk\/mmmwiki\/index.php\/Env_Vars#\2">\2<\/a>/g ;
#        printf OUT "%s%5d %s%s", "<A NAME=\"l$i\">", $i,"</A>",$_;
         print OUT "$i $_\n";
      }

      print OUT "</PRE>\n" ;
      print OUT "</BODY>\n" ;
      print OUT "</HTML>\n" ;

      close (OUT);
      close (IN);

#      `mv $Outfile $OUT_HTML`;

# ensure html file has same timestamp as source file
      `touch -r $Infile $SINKDIR/$NAME.html`;

# make sure HTML file has correct ownership
#       `chown $USER $SINKDIR/$NAME.html`;

# make files group readable

      `chmod g+r $SINKDIR/$NAME.html`;
    }
  }
}

opendir(SOURCE,$SOURCEDIR) || die "Can't open $SOURCEDIR: $!\n";
@files=grep(/(\.wpe$|\.wpd$|\.aw$|\.ag$|\.eps$|\.ps$|\.gz$|\.txt$|\.doc$|\.vf$|\.gif$|\.png$|\.html$|JCL_|Jcl_|_nav\.|\.register$|\.database$|\.basis$|\.jobsheet$|\.ver$|\.css$|\.pdf$|\.ppt$)/,readdir(SOURCE));
closedir (SOURCE);

if ($#files >= 0) {
  foreach $file (@files){
    if (! -e "$SINKDIR/$file"){
      `ln -s $SOURCEDIR/$file $SINKDIR/$file`;
    }
  }
}
# The end.
