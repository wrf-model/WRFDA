      MODULE module_wave2grid_kma
     
      PARAMETER ( KMAX=30 )
      PARAMETER ( IMAXE=640, JMAXE=321 )                                 
      PARAMETER ( IMAX=640, JMAX=320 )                                 
      PARAMETER (MEND1 =214,NEND1=214,JEND1=214)
C for T63      PARAMETER ( IMAXE=192, JMAXE=97 )                                 
C for T63      PARAMETER ( IMAX=192, JMAX=96  )                                 
C for T63      PARAMETER (MEND1 =64,NEND1=64,JEND1=64)
      PARAMETER ( ISST=360, JSST=181 )                                          
      PARAMETER ( ISNW=360, JSNW=180 )                                          
      PARAMETER ( IDIM=428, JDIM=214 ) ! MAX(MAX,SST,SNW)                       
      PARAMETER ( MAXJZ=16 )                                                    
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
#include  "OUTZ.inc"
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
#include  "PACK.inc"
#include  "VPRM.inc"
#include  "WRTDAT.inc"
#include  "WRTEOF.inc"
#include  "WRTHED.inc"
#include  "ZE2TVE.inc"
#include  "ZMNLAT.inc"
#include  "ZMNT.inc"
#include  "RELHUM.inc"            !shc
#include  "PREGSM.inc"
#include  "PREGSM1.inc"           !shc
#include  "Einc_to_Ganl.inc"      !shc

      END MODULE  module_wave2grid_kma
