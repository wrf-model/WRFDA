      MODULE module_kma_wave2grid

      PARAMETER(DPHI=0.5625,INTVL=1,KGRD=18,IGRD=640,JGRD=321)
          PARAMETER(KMAX=30, LMAX=KGRD-1, KLMAX=KMAX)
      PARAMETER(MEND1 =214,NEND1=214,JEND1=214,
     S          IMAXG =640,JMAXG=320,
     1          IMAX  =360./DPHI+0.5,IOUT =IMAX/INTVL  ,
     2          JMAX  =180./DPHI+1.5,JOUT =(JMAX-1)/INTVL+1,
     3          IMX   =IMAX+2,
     4          JOUTHF= (JOUT+1)/2,
     5          JMXGHF=(1+JMAXG)/2,KMX2  =KMAX*2  ,LMX2 =LMAX*2,
     6          MNWAV =MEND1*(MEND1+1)/2)

      CONTAINS

#include "BSSLZ1.inc"
#include "CUT.inc"
#include "FFT991.inc"
#include "G2W.inc"
#include "G2WDZ.inc"
#include "G2WPP.inc"
#include "GAUSS.inc"
#include "GOUT.inc"
#include "IDCMP.inc"
#include "LGNDR1.inc"
#include "LGNUV.inc"
#include "LGNW2G.inc"
#include "LT2GAU.inc"
#include "MNMX.inc"
#include "MOVEC.inc"
#include "PACK.inc"
#include "RADB2M.inc"
#include "RADB3M.inc"
#include "RADB4M.inc"
#include "RADB5M.inc"
#include "RADBGM.inc"
#include "RADF2M.inc"
#include "RADF3M.inc"
#include "RADF4M.inc"
#include "RADF5M.inc"
#include "RADFGM.inc"
#include "REDDAT.inc"
#include "REDHED.inc"
#include "REDIEG.inc"
#include "REOWAV.inc"
#include "REOWV.inc"
#include "RESET.inc"
#include "RFFTBM.inc"
#include "RFFTFM.inc"
#include "RFFTIM.inc"
#include "RFTB1M.inc"
#include "RFTB2M.inc"
#include "RFTB9M.inc"
#include "RFTF1M.inc"
#include "RFTF2M.inc"
#include "RFTF3M.inc"
#include "RFTF9M.inc"
#include "RFTI1M.inc"
#include "SETARY.inc"
#include "W2G.inc"
#include "W2GCONV.inc"
#include "W2GPXY.inc"
#include "W2GUV.inc"
#include "WAVMAG.inc"
#include "WEIHT2.inc"
#include "ZNME2PXX.inc"
      END MODULE module_kma_wave2grid

