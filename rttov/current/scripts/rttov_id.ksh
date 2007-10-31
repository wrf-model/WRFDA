#!/bin/ksh
#
# script function
# 
# defines the RTTOV identification numbers 
# for input platform and instrument names
# input names:
# platform is $1
# instrument is $2
#
# output are the variables
#   ${PLATFORM_N}   pltaform number
#   ${INST_N}       instrument number 
#   ${SENSOR_TYPE}  sensor type (ir, mw, hi)
#
# P. Brunel Feb 2003
#
# for the use of this script function the user shall 
# enter the following line (with the dot) before any reference to th function:
#. rttov_id.ksh
#
# example of call to the function is:
# rttov_id eos airs

function rttov_id {
  PLATFORM=$1
  INST=$2

  case ${PLATFORM} in
    'noaa')     PLATFORM_N=1;;
    'dmsp')     PLATFORM_N=2;;
    'meteosat') PLATFORM_N=3;;
    'goes')     PLATFORM_N=4;;
    'gms')      PLATFORM_N=5;;
    'fy2')      PLATFORM_N=6;;
    'trmm')     PLATFORM_N=7;;
    'ers')      PLATFORM_N=8;;
    'eos')      PLATFORM_N=9;;
    'metop')    PLATFORM_N=10;;
    'envisat')  PLATFORM_N=11;;
    'msg')      PLATFORM_N=12;;
    'fy1')      PLATFORM_N=13;;
    'adeos')    PLATFORM_N=14;;
    'mtsat')    PLATFORM_N=15;;
    'coriolis') PLATFORM_N=16;;
    'npoess')   PLATFORM_N=17;;
    'gifts')    PLATFORM_N=18;;
    *)          PLATFORM_N=-1;;
  esac

  case ${INST} in
    'hirs')   INST_N=0;;
    'msu')    INST_N=1;;
    'ssu')    INST_N=2;;
    'amsu-a'|'amsua') INST_N=3;;
    'amsu-b'|'amsub') INST_N=4;;
    'avhrr')  INST_N=5;;
    'ssmi')   INST_N=6;;
    'vtpr1')  INST_N=7;;
    'vtpr2')  INST_N=8;;
    'tmi')    INST_N=9;;
    'ssmis')  INST_N=10;;
    'airs')   INST_N=11;;
    'hsb')    INST_N=12;;
    'modis')  INST_N=13;;
    'atsr' | 'aatsr')   INST_N=14;;
    'mhs')    INST_N=15;;
    'iasi')   INST_N=16;;
    'amsr')   INST_N=17;;
    'atms')   INST_N=19;;
    'mviri')  INST_N=20;;
    'seviri') INST_N=21;;
    'imager') case ${PLATFORM} in
      'goes')  INST_N=22;;
      'gms')   INST_N=24;;
      'mtsat') INST_N=24;;
      *)       INST_N=-1;;
      esac;;
    'sounder')INST_N=23;;
    'vissr')  INST_N=25;;
    'mvisr')  INST_N=26;;
    'cris')   INST_N=27;;
    'cmis')   INST_N=28;;
    'viirs')  INST_N=29;;
    'windsat')INST_N=30;;
    'gifts')  INST_N=31;;
    *)        INST_N=-1;;
  esac

  case ${INST} in
    'msu' | 'amsua' | 'amsub' | 'tmi'  | 'ssmi' | 'ssmis' \
          | 'hsb'   | 'mhs'   | 'cmis' | 'amsr' | 'atms'  \
          | 'windsat' ) SENSOR_TYPE='mw';;
    'airs' | 'iasi' | 'cris' | 'gifts' ) SENSOR_TYPE='hi';;
    * ) SENSOR_TYPE='ir';;
  esac

export PLATFORM_N
export INST_N
export SENSOR_TYPE

}
