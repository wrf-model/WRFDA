      MODULE module_wave2grid_kma
     
      PARAMETER ( KMAX=30 )
      PARAMETER ( IMAX=640, JMAX=320 )                                 
      PARAMETER ( IMAXE=640, JMAXE=321 )                                 
      PARAMETER ( ISST=360, JSST=181 )                                          
      PARAMETER ( ISNW=360, JSNW=180 )                                          
      PARAMETER ( IDIM=640, JDIM=320 ) ! MAX(MAX,SST,SNW)                       
      PARAMETER ( MAXJZ=16 )                                                    
      PARAMETER (MEND1 =214,NEND1=214,JEND1=214)
      PARAMETER (JMAXHF= JMAX/2)
      PARAMETER (MNWAV =MEND1*(MEND1+1)/2)
      PARAMETER (IVAR=6,IMX=IMAX+2)

      CONTAINS

#include  "BSSLZ1.inc"
#include  "CR8I2V.inc"
#include  "CVDATE.inc"
#include  "GAUSS.inc"
#include  "GH2TV.inc"
#include  "LT2GAU.inc"
#include  "GPLHGT.inc"
#include  "MINMAX.inc"
#include  "MONTWO.inc"
#include  "MOVEC.inc"
#include  "OUTZ.inc"
#include  "PACK.inc"
#include  "PRESUB.inc"
#include  "REDANL.inc"
#include  "REDDAT.inc"
#include  "REDDAT_ASCII.inc"
#include  "REDDAT_BIN.inc"
#include  "REDGES.inc"
#include  "REDHED.inc"
#include  "RESET.inc"
#include  "SPLDIF3_H.inc"
#include  "TETEN.inc"
#include  "VPRM.inc"
#include  "WRTDAT.inc"
#include  "WRTEOF.inc"
#include  "WRTHED.inc"
#include  "ZE2TVE.inc"
#include  "ZMNLAT.inc"
#include  "ZMNT.inc"
#include  "PREGSM.inc"

      END MODULE  module_wave2grid_kma
