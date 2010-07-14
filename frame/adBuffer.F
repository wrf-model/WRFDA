!$Id: adBuffer.f 1875 2007-06-06 17:00:06Z llh $

!===================== CHARACTERS =======================:
      BLOCK DATA CHARACTERS
      CHARACTER ads1buf(512), ads1lbuf(512)
      INTEGER ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf, &
             ads1ibuf,ads1ilbuf,ads1inlbuf
      DATA ads1ibuf/1/
      DATA ads1ilbuf/-1/
      DATA ads1inlbuf/.FALSE./
      END

      SUBROUTINE PUSHCHARACTER(x)
      CHARACTER x, ads1buf(512), ads1lbuf(512)
      INTEGER ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf, &
             ads1ibuf,ads1ilbuf,ads1inlbuf
!
      call addftraffic(1)
      IF (ads1ilbuf.ne.-1) THEN
         ads1ilbuf = -1
         ads1inlbuf = .FALSE.
      ENDIF
      IF (ads1ibuf.ge.512) THEN
         ads1buf(512) = x
         CALL PUSHCHARACTERARRAY(ads1buf, 512)
      call addftraffic(-512)
         ads1ibuf = 1
      ELSE
         ads1buf(ads1ibuf) = x
         ads1ibuf = ads1ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKCHARACTER(x)
      CHARACTER x, ads1buf(512), ads1lbuf(512)
      INTEGER ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf, &
             ads1ibuf,ads1ilbuf,ads1inlbuf
!
      IF (ads1ilbuf.eq.-1) ads1ilbuf=ads1ibuf
      IF (ads1ilbuf.le.1) THEN
         CALL LOOKCHARACTERARRAY(ads1lbuf, 512)
         ads1inlbuf = .TRUE.
         ads1ilbuf = 512
         x = ads1lbuf(512)
      ELSE
         ads1ilbuf = ads1ilbuf-1
         if (ads1inlbuf) THEN
            x = ads1lbuf(ads1ilbuf)
         ELSE
            x = ads1buf(ads1ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPCHARACTER(x)
      CHARACTER x, ads1buf(512), ads1lbuf(512)
      INTEGER ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf, &
             ads1ibuf,ads1ilbuf,ads1inlbuf
!
      IF (ads1ilbuf.ne.-1) THEN
         ads1ilbuf = -1
         ads1inlbuf = .FALSE.
      ENDIF
      IF (ads1ibuf.le.1) THEN
         CALL POPCHARACTERARRAY(ads1buf, 512)
         ads1ibuf = 512
         x = ads1buf(512)
      ELSE
         ads1ibuf = ads1ibuf-1
         x = ads1buf(ads1ibuf)
      ENDIF
      END

!======================= BOOLEANS =========================
      BLOCK DATA BOOLEANS
      LOGICAL adl4buf(512), adl4lbuf(512)
      INTEGER adl4ibuf,adl4ilbuf
      LOGICAL adl4inlbuf
      COMMON /adl4fbuf/adl4buf,adl4lbuf, &
             adl4ibuf,adl4ilbuf,adl4inlbuf
      DATA adl4ibuf/1/
      DATA adl4ilbuf/-1/
      DATA adl4inlbuf/.FALSE./
      END

      SUBROUTINE PUSHBOOLEAN(x)
      LOGICAL x, adl4buf(512), adl4lbuf(512)
      INTEGER adl4ibuf,adl4ilbuf
      LOGICAL adl4inlbuf
      COMMON /adl4fbuf/adl4buf,adl4lbuf, &
             adl4ibuf,adl4ilbuf,adl4inlbuf
!
      call addftraffic(4)
      IF (adl4ilbuf.ne.-1) THEN
         adl4ilbuf = -1
         adl4inlbuf = .FALSE.
      ENDIF
      IF (adl4ibuf.ge.512) THEN
         adl4buf(512) = x
         CALL PUSHBOOLEANARRAY(adl4buf, 512)
      call addftraffic(-2048)
         adl4ibuf = 1
      ELSE
         adl4buf(adl4ibuf) = x
         adl4ibuf = adl4ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKBOOLEAN(x)
      LOGICAL x, adl4buf(512), adl4lbuf(512)
      INTEGER adl4ibuf,adl4ilbuf
      LOGICAL adl4inlbuf
      COMMON /adl4fbuf/adl4buf,adl4lbuf, &
             adl4ibuf,adl4ilbuf,adl4inlbuf
!
      IF (adl4ilbuf.eq.-1) adl4ilbuf=adl4ibuf
      IF (adl4ilbuf.le.1) THEN
         CALL LOOKBOOLEANARRAY(adl4lbuf, 512)
         adl4inlbuf = .TRUE.
         adl4ilbuf = 512
         x = adl4lbuf(512)
      ELSE
         adl4ilbuf = adl4ilbuf-1
         if (adl4inlbuf) THEN
            x = adl4lbuf(adl4ilbuf)
         ELSE
            x = adl4buf(adl4ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPBOOLEAN(x)
      LOGICAL x, adl4buf(512), adl4lbuf(512)
      INTEGER adl4ibuf,adl4ilbuf
      LOGICAL adl4inlbuf
      COMMON /adl4fbuf/adl4buf,adl4lbuf, &
             adl4ibuf,adl4ilbuf,adl4inlbuf
!
      IF (adl4ilbuf.ne.-1) THEN
         adl4ilbuf = -1
         adl4inlbuf = .FALSE.
      ENDIF
      IF (adl4ibuf.le.1) THEN
         CALL POPBOOLEANARRAY(adl4buf, 512)
         adl4ibuf = 512
         x = adl4buf(512)
      ELSE
         adl4ibuf = adl4ibuf-1
         x = adl4buf(adl4ibuf)
      ENDIF
      END

!======================= INTEGER*4 =========================:
      BLOCK DATA INTEGERS4
      INTEGER*4 adi4buf(512), adi4lbuf(512)
      INTEGER adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf, &
             adi4ibuf,adi4ilbuf,adi4inlbuf
      DATA adi4ibuf/1/
      DATA adi4ilbuf/-1/
      DATA adi4inlbuf/.FALSE./
      END

      SUBROUTINE PUSHINTEGER4(x)
      INTEGER*4 x, adi4buf(512), adi4lbuf(512)
      INTEGER adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf, &
             adi4ibuf,adi4ilbuf,adi4inlbuf
!
      call addftraffic(4)
      IF (adi4ilbuf.ne.-1) THEN
         adi4ilbuf = -1
         adi4inlbuf = .FALSE.
      ENDIF
      IF (adi4ibuf.ge.512) THEN
         adi4buf(512) = x
         CALL PUSHINTEGER4ARRAY(adi4buf, 512)
      call addftraffic(-2048)
         adi4ibuf = 1
      ELSE
         adi4buf(adi4ibuf) = x
         adi4ibuf = adi4ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKINTEGER4(x)
      INTEGER*4 x, adi4buf(512), adi4lbuf(512)
      INTEGER adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf, &
             adi4ibuf,adi4ilbuf,adi4inlbuf
!
      IF (adi4ilbuf.eq.-1) adi4ilbuf=adi4ibuf
      IF (adi4ilbuf.le.1) THEN
         CALL LOOKINTEGER4ARRAY(adi4lbuf, 512)
         adi4inlbuf = .TRUE.
         adi4ilbuf = 512
         x = adi4lbuf(512)
      ELSE
         adi4ilbuf = adi4ilbuf-1
         if (adi4inlbuf) THEN
            x = adi4lbuf(adi4ilbuf)
         ELSE
            x = adi4buf(adi4ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPINTEGER4(x)
      INTEGER*4 x, adi4buf(512), adi4lbuf(512)
      INTEGER adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf, &
             adi4ibuf,adi4ilbuf,adi4inlbuf
!
      IF (adi4ilbuf.ne.-1) THEN
         adi4ilbuf = -1
         adi4inlbuf = .FALSE.
      ENDIF
      IF (adi4ibuf.le.1) THEN
         CALL POPINTEGER4ARRAY(adi4buf, 512)
         adi4ibuf = 512
         x = adi4buf(512)
      ELSE
         adi4ibuf = adi4ibuf-1
         x = adi4buf(adi4ibuf)
      ENDIF
      END

!======================= INTEGER*8 =========================
      BLOCK DATA INTEGERS8
      INTEGER*8 adi8buf(512), adi8lbuf(512)
      INTEGER adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf, &
             adi8ibuf,adi8ilbuf,adi8inlbuf
      DATA adi8ibuf/1/
      DATA adi8ilbuf/-1/
      DATA adi8inlbuf/.FALSE./
      END

      SUBROUTINE PUSHINTEGER8(x)
      INTEGER*8 x, adi8buf(512), adi8lbuf(512)
      INTEGER adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf, &
             adi8ibuf,adi8ilbuf,adi8inlbuf
!
      call addftraffic(8)
      IF (adi8ilbuf.ne.-1) THEN
         adi8ilbuf = -1
         adi8inlbuf = .FALSE.
      ENDIF
      IF (adi8ibuf.ge.512) THEN
         adi8buf(512) = x
         CALL PUSHINTEGER8ARRAY(adi8buf, 512)
      call addftraffic(-4096)
         adi8ibuf = 1
      ELSE
         adi8buf(adi8ibuf) = x
         adi8ibuf = adi8ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKINTEGER8(x)
      INTEGER*8 x, adi8buf(512), adi8lbuf(512)
      INTEGER adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf, &
             adi8ibuf,adi8ilbuf,adi8inlbuf
!
      IF (adi8ilbuf.eq.-1) adi8ilbuf=adi8ibuf
      IF (adi8ilbuf.le.1) THEN
         CALL LOOKINTEGER8ARRAY(adi8lbuf, 512)
         adi8inlbuf = .TRUE.
         adi8ilbuf = 512
         x = adi8lbuf(512)
      ELSE
         adi8ilbuf = adi8ilbuf-1
         if (adi8inlbuf) THEN
            x = adi8lbuf(adi8ilbuf)
         ELSE
            x = adi8buf(adi8ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPINTEGER8(x)
      INTEGER*8 x, adi8buf(512), adi8lbuf(512)
      INTEGER adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf, &
             adi8ibuf,adi8ilbuf,adi8inlbuf
!
      IF (adi8ilbuf.ne.-1) THEN
         adi8ilbuf = -1
         adi8inlbuf = .FALSE.
      ENDIF
      IF (adi8ibuf.le.1) THEN
         CALL POPINTEGER8ARRAY(adi8buf, 512)
         adi8ibuf = 512
         x = adi8buf(512)
      ELSE
         adi8ibuf = adi8ibuf-1
         x = adi8buf(adi8ibuf)
      ENDIF
      END

!======================= REAL*4 =========================
      BLOCK DATA REALS4
      REAL*4 adr4buf(512), adr4lbuf(512)
      INTEGER adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf, &
             adr4ibuf,adr4ilbuf,adr4inlbuf
      DATA adr4ibuf/1/
      DATA adr4ilbuf/-1/
      DATA adr4inlbuf/.FALSE./
      END

      SUBROUTINE PUSHREAL4(x)
      REAL*4 x, adr4buf(512), adr4lbuf(512)
      INTEGER adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf, &
             adr4ibuf,adr4ilbuf,adr4inlbuf
!
      call addftraffic(4)
      IF (adr4ilbuf.ne.-1) THEN
         adr4ilbuf = -1
         adr4inlbuf = .FALSE.
      ENDIF
      IF (adr4ibuf.ge.512) THEN
         adr4buf(512) = x
         CALL PUSHREAL4ARRAY(adr4buf, 512)
      call addftraffic(-2048)
         adr4ibuf = 1
      ELSE
         adr4buf(adr4ibuf) = x
         adr4ibuf = adr4ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKREAL4(x)
      REAL*4 x, adr4buf(512), adr4lbuf(512)
      INTEGER adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf, &
             adr4ibuf,adr4ilbuf,adr4inlbuf
!
      IF (adr4ilbuf.eq.-1) adr4ilbuf=adr4ibuf
      IF (adr4ilbuf.le.1) THEN
         CALL LOOKREAL4ARRAY(adr4lbuf, 512)
         adr4inlbuf = .TRUE.
         adr4ilbuf = 512
         x = adr4lbuf(512)
      ELSE
         adr4ilbuf = adr4ilbuf-1
         if (adr4inlbuf) THEN
            x = adr4lbuf(adr4ilbuf)
         ELSE
            x = adr4buf(adr4ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPREAL4(x)
      REAL*4 x, adr4buf(512), adr4lbuf(512)
      INTEGER adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf, &
             adr4ibuf,adr4ilbuf,adr4inlbuf
!
      IF (adr4ilbuf.ne.-1) THEN
         adr4ilbuf = -1
         adr4inlbuf = .FALSE.
      ENDIF
      IF (adr4ibuf.le.1) THEN
         CALL POPREAL4ARRAY(adr4buf, 512)
         adr4ibuf = 512
         x = adr4buf(512)
      ELSE
         adr4ibuf = adr4ibuf-1
         x = adr4buf(adr4ibuf)
      ENDIF
      END

!======================= REAL*8 =========================
      BLOCK DATA REALS8
      REAL*8 adr8buf(512), adr8lbuf(512)
      INTEGER adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf, &
             adr8ibuf,adr8ilbuf,adr8inlbuf
      DATA adr8ibuf/1/
      DATA adr8ilbuf/-1/
      DATA adr8inlbuf/.FALSE./
      END

      SUBROUTINE PUSHREAL8(x)
      REAL*8 x, adr8buf(512), adr8lbuf(512)
      INTEGER adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf, &
             adr8ibuf,adr8ilbuf,adr8inlbuf
!
      call addftraffic(8)
      IF (adr8ilbuf.ne.-1) THEN
         adr8ilbuf = -1
         adr8inlbuf = .FALSE.
      ENDIF
      IF (adr8ibuf.ge.512) THEN
         adr8buf(512) = x
         CALL PUSHREAL8ARRAY(adr8buf, 512)
      call addftraffic(-4096)
         adr8ibuf = 1
      ELSE
         adr8buf(adr8ibuf) = x
         adr8ibuf = adr8ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKREAL8(x)
      REAL*8 x, adr8buf(512), adr8lbuf(512)
      INTEGER adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf, &
             adr8ibuf,adr8ilbuf,adr8inlbuf
!
      IF (adr8ilbuf.eq.-1) adr8ilbuf=adr8ibuf
      IF (adr8ilbuf.le.1) THEN
         CALL LOOKREAL8ARRAY(adr8lbuf, 512)
         adr8inlbuf = .TRUE.
         adr8ilbuf = 512
         x = adr8lbuf(512)
      ELSE
         adr8ilbuf = adr8ilbuf-1
         if (adr8inlbuf) THEN
            x = adr8lbuf(adr8ilbuf)
         ELSE
            x = adr8buf(adr8ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPREAL8(x)
      REAL*8 x, adr8buf(512), adr8lbuf(512)
      INTEGER adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf, &
             adr8ibuf,adr8ilbuf,adr8inlbuf
!
      IF (adr8ilbuf.ne.-1) THEN
         adr8ilbuf = -1
         adr8inlbuf = .FALSE.
      ENDIF
      IF (adr8ibuf.le.1) THEN
         CALL POPREAL8ARRAY(adr8buf, 512)
         adr8ibuf = 512
         x = adr8buf(512)
      ELSE
         adr8ibuf = adr8ibuf-1
         x = adr8buf(adr8ibuf)
      ENDIF
      END

!======================= REAL*16 =========================
      BLOCK DATA REALS16
      DOUBLE PRECISION adr16buf(512), adr16lbuf(512)
      INTEGER adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf, &
             adr16ibuf,adr16ilbuf,adr16inlbuf
      DATA adr16ibuf/1/
      DATA adr16ilbuf/-1/
      DATA adr16inlbuf/.FALSE./
      END

      SUBROUTINE PUSHREAL16(x)
      DOUBLE PRECISION x, adr16buf(512), adr16lbuf(512)
      INTEGER adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf, &
             adr16ibuf,adr16ilbuf,adr16inlbuf
!
      call addftraffic(16)
      IF (adr16ilbuf.ne.-1) THEN
         adr16ilbuf = -1
         adr16inlbuf = .FALSE.
      ENDIF
      IF (adr16ibuf.ge.512) THEN
         adr16buf(512) = x
         CALL PUSHREAL16ARRAY(adr16buf, 512)
      call addftraffic(-8192)
         adr16ibuf = 1
      ELSE
         adr16buf(adr16ibuf) = x
         adr16ibuf = adr16ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKREAL16(x)
      DOUBLE PRECISION x, adr16buf(512), adr16lbuf(512)
      INTEGER adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf, &
             adr16ibuf,adr16ilbuf,adr16inlbuf
!
      IF (adr16ilbuf.eq.-1) adr16ilbuf=adr16ibuf
      IF (adr16ilbuf.le.1) THEN
         CALL LOOKREAL16ARRAY(adr16lbuf, 512)
         adr16inlbuf = .TRUE.
         adr16ilbuf = 512
         x = adr16lbuf(512)
      ELSE
         adr16ilbuf = adr16ilbuf-1
         if (adr16inlbuf) THEN
            x = adr16lbuf(adr16ilbuf)
         ELSE
            x = adr16buf(adr16ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPREAL16(x)
      DOUBLE PRECISION x, adr16buf(512), adr16lbuf(512)
      INTEGER adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf, &
             adr16ibuf,adr16ilbuf,adr16inlbuf
!
      IF (adr16ilbuf.ne.-1) THEN
         adr16ilbuf = -1
         adr16inlbuf = .FALSE.
      ENDIF
      IF (adr16ibuf.le.1) THEN
         CALL POPREAL16ARRAY(adr16buf, 512)
         adr16ibuf = 512
         x = adr16buf(512)
      ELSE
         adr16ibuf = adr16ibuf-1
         x = adr16buf(adr16ibuf)
      ENDIF
      END

!======================= COMPLEX*8 =========================
      BLOCK DATA COMPLEXS8
      COMPLEX*8 adc8buf(512), adc8lbuf(512)
      INTEGER adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf, &
             adc8ibuf,adc8ilbuf,adc8inlbuf
      DATA adc8ibuf/1/
      DATA adc8ilbuf/-1/
      DATA adc8inlbuf/.FALSE./
      END

      SUBROUTINE PUSHCOMPLEX8(x)
      COMPLEX*8 x, adc8buf(512), adc8lbuf(512)
      INTEGER adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf, &
             adc8ibuf,adc8ilbuf,adc8inlbuf
!
      call addftraffic(8)
      IF (adc8ilbuf.ne.-1) THEN
         adc8ilbuf = -1
         adc8inlbuf = .FALSE.
      ENDIF
      IF (adc8ibuf.ge.512) THEN
         adc8buf(512) = x
         CALL PUSHCOMPLEX8ARRAY(adc8buf, 512)
      call addftraffic(-4096)
         adc8ibuf = 1
      ELSE
         adc8buf(adc8ibuf) = x
         adc8ibuf = adc8ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKCOMPLEX8(x)
      COMPLEX*8 x, adc8buf(512), adc8lbuf(512)
      INTEGER adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf, &
             adc8ibuf,adc8ilbuf,adc8inlbuf
!
      IF (adc8ilbuf.eq.-1) adc8ilbuf=adc8ibuf
      IF (adc8ilbuf.le.1) THEN
         CALL LOOKCOMPLEX8ARRAY(adc8lbuf, 512)
         adc8inlbuf = .TRUE.
         adc8ilbuf = 512
         x = adc8lbuf(512)
      ELSE
         adc8ilbuf = adc8ilbuf-1
         if (adc8inlbuf) THEN
            x = adc8lbuf(adc8ilbuf)
         ELSE
            x = adc8buf(adc8ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPCOMPLEX8(x)
      COMPLEX*8 x, adc8buf(512), adc8lbuf(512)
      INTEGER adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf, &
             adc8ibuf,adc8ilbuf,adc8inlbuf
!
      IF (adc8ilbuf.ne.-1) THEN
         adc8ilbuf = -1
         adc8inlbuf = .FALSE.
      ENDIF
      IF (adc8ibuf.le.1) THEN
         CALL POPCOMPLEX8ARRAY(adc8buf, 512)
         adc8ibuf = 512
         x = adc8buf(512)
      ELSE
         adc8ibuf = adc8ibuf-1
         x = adc8buf(adc8ibuf)
      ENDIF
      END

!======================= COMPLEX*16 =========================
      BLOCK DATA COMPLEXS16
      COMPLEX*16 adc16buf(512), adc16lbuf(512)
      INTEGER adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf, &
             adc16ibuf,adc16ilbuf,adc16inlbuf
      DATA adc16ibuf/1/
      DATA adc16ilbuf/-1/
      DATA adc16inlbuf/.FALSE./
      END

      SUBROUTINE PUSHCOMPLEX16(x)
      COMPLEX*16 x, adc16buf(512), adc16lbuf(512)
      INTEGER adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf, &
             adc16ibuf,adc16ilbuf,adc16inlbuf
!
      call addftraffic(16)
      IF (adc16ilbuf.ne.-1) THEN
         adc16ilbuf = -1
         adc16inlbuf = .FALSE.
      ENDIF
      IF (adc16ibuf.ge.512) THEN
         adc16buf(512) = x
         CALL PUSHCOMPLEX16ARRAY(adc16buf, 512)
      call addftraffic(-8192)
         adc16ibuf = 1
      ELSE
         adc16buf(adc16ibuf) = x
         adc16ibuf = adc16ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKCOMPLEX16(x)
      COMPLEX*16 x, adc16buf(512), adc16lbuf(512)
      INTEGER adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf, &
             adc16ibuf,adc16ilbuf,adc16inlbuf
!
      IF (adc16ilbuf.eq.-1) adc16ilbuf=adc16ibuf
      IF (adc16ilbuf.le.1) THEN
         CALL LOOKCOMPLEX16ARRAY(adc16lbuf, 512)
         adc16inlbuf = .TRUE.
         adc16ilbuf = 512
         x = adc16lbuf(512)
      ELSE
         adc16ilbuf = adc16ilbuf-1
         if (adc16inlbuf) THEN
            x = adc16lbuf(adc16ilbuf)
         ELSE
            x = adc16buf(adc16ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPCOMPLEX16(x)
      COMPLEX*16 x, adc16buf(512), adc16lbuf(512)
      INTEGER adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf, &
             adc16ibuf,adc16ilbuf,adc16inlbuf
!
      IF (adc16ilbuf.ne.-1) THEN
         adc16ilbuf = -1
         adc16inlbuf = .FALSE.
      ENDIF
      IF (adc16ibuf.le.1) THEN
         CALL POPCOMPLEX16ARRAY(adc16buf, 512)
         adc16ibuf = 512
         x = adc16buf(512)
      ELSE
         adc16ibuf = adc16ibuf-1
         x = adc16buf(adc16ibuf)
      ENDIF
      END

!=========== MEASUREMENT OF PUSH/POP TRAFFIC ==========

      BLOCK DATA MEMTRAFFIC
      INTEGER*8 mmftraffic,mmftrafficM
      COMMON /mmcomtraffic/mmftraffic,mmftrafficM
      DATA mmftraffic/0/
      DATA mmftrafficM/0/
      END

      subroutine addftraffic(n)
      INTEGER n
      INTEGER*8 mmftraffic,mmftrafficM
      COMMON /mmcomtraffic/mmftraffic,mmftrafficM
!
      mmftraffic = mmftraffic+n
      if (mmftraffic.ge.1048576) then
 100     mmftraffic = mmftraffic-1048576
         mmftrafficM = mmftrafficM+1
         if (mmftraffic.ge.1048576) then
            goto 100
         else
            goto 300
         endif
      else if (mmftraffic.lt.0) then
 200     mmftraffic = mmftraffic+1048576
         mmftrafficM = mmftrafficM-1
         if (mmftraffic.lt.0) then
            goto 200
         else
            goto 300
         endif
      endif
 300  continue
      END

      SUBROUTINE PRINTTRAFFIC()
      INTEGER*8 mmftraffic,mmftrafficM
      COMMON /mmcomtraffic/mmftraffic,mmftrafficM
      call printctraffic()
      write (6,1001) ' F Traffic: ',mmftrafficM,' Mb and ', &
           (((mmftraffic*1000)/1024)*1000)/1024, ' millionths'
1001  format(a,i6,a,i6,a)
      END

! ============ PRINTING THE SIZE OF STACKS AND BUFFERS ==========

      SUBROUTINE PRINTBUFFERTOP()
      CHARACTER ads1buf(512), ads1lbuf(512)
      INTEGER ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf, &
             ads1ibuf,ads1ilbuf,ads1inlbuf
      LOGICAL adl4buf(512), adl4lbuf(512)
      INTEGER adl4ibuf,adl4ilbuf
      LOGICAL adl4inlbuf
      COMMON /adl4fbuf/adl4buf,adl4lbuf, &
             adl4ibuf,adl4ilbuf,adl4inlbuf
      INTEGER*4 adi4buf(512), adi4lbuf(512)
      INTEGER adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf, &
             adi4ibuf,adi4ilbuf,adi4inlbuf
      INTEGER*8 adi8buf(512), adi8lbuf(512)
      INTEGER adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf, &
             adi8ibuf,adi8ilbuf,adi8inlbuf
!           INTEGER*16 adi16buf(512), adi16lbuf(512)
!           INTEGER adi16ibuf,adi16ilbuf
!           LOGICAL adi16inlbuf
!           COMMON /adi16fbuf/adi16buf,adi16lbuf,
!          +       adi16ibuf,adi16ilbuf,adi16inlbuf
      REAL*4 adr4buf(512), adr4lbuf(512)
      INTEGER adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf, &
             adr4ibuf,adr4ilbuf,adr4inlbuf
      REAL*8 adr8buf(512), adr8lbuf(512)
      INTEGER adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf, &
             adr8ibuf,adr8ilbuf,adr8inlbuf
      DOUBLE PRECISION adr16buf(512), adr16lbuf(512)
      INTEGER adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf, &
             adr16ibuf,adr16ilbuf,adr16inlbuf
!           REAL*32 x, adr32buf(512), adr32lbuf(512)
!           INTEGER adr32ibuf,adr32ilbuf
!           LOGICAL adr32inlbuf
!           COMMON /adr32fbuf/adr32buf,adr32lbuf,
!          +       adr32ibuf,adr32ilbuf,adr32inlbuf
!           COMPLEX*4 adc4buf(512), adc4lbuf(512)
!           INTEGER adc4ibuf,adc4ilbuf
!           LOGICAL adc4inlbuf
!           COMMON /adc4fbuf/adc4buf,adc4lbuf,
!          +       adc4ibuf,adc4ilbuf,adc4inlbuf
      COMPLEX*8 adc8buf(512), adc8lbuf(512)
      INTEGER adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf, &
             adc8ibuf,adc8ilbuf,adc8inlbuf
      COMPLEX*16 adc16buf(512), adc16lbuf(512)
      INTEGER adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf, &
             adc16ibuf,adc16ilbuf,adc16inlbuf
!           COMPLEX*32 adc32buf(512), adc32lbuf(512)
!           INTEGER adc32ibuf,adc32ilbuf
!           LOGICAL adc32inlbuf
!           COMMON /adc32fbuf/adc32buf,adc32lbuf,
!          +       adc32ibuf,adc32ilbuf,adc32inlbuf
      integer size
!     
      size = 0
      size = size + (ads1ibuf-1)*1
      size = size + (adl4ibuf-1)*4
      size = size + (adi4ibuf-1)*4
      size = size + (adi8ibuf-1)*8
!           size = size + (adi16ibuf-1)*16
      size = size + (adr4ibuf-1)*4
      size = size + (adr8ibuf-1)*8
      size = size + (adr16ibuf-1)*16
!           size = size + (adr32ibuf-1)*32
!           size = size + (adc4ibuf-1)*4
      size = size + (adc8ibuf-1)*8
      size = size + (adc16ibuf-1)*16
!           size = size + (adc32ibuf-1)*32
      print *,'Buffer size:',size,' bytes i.e. ',size/1024.0,' Kbytes'
!
      end
      FUNCTION SMALLSTACKSIZE()
      CHARACTER ads1buf(512), ads1lbuf(512)
      INTEGER ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf, &
             ads1ibuf,ads1ilbuf,ads1inlbuf
      LOGICAL adl4buf(512), adl4lbuf(512)
      INTEGER adl4ibuf,adl4ilbuf
      LOGICAL adl4inlbuf
      COMMON /adl4fbuf/adl4buf,adl4lbuf, &
             adl4ibuf,adl4ilbuf,adl4inlbuf
      INTEGER*4 adi4buf(512), adi4lbuf(512)
      INTEGER adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf, &
             adi4ibuf,adi4ilbuf,adi4inlbuf
      INTEGER*8 adi8buf(512), adi8lbuf(512)
      INTEGER adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf, &
             adi8ibuf,adi8ilbuf,adi8inlbuf
!           INTEGER*16 adi16buf(512), adi16lbuf(512)
!           INTEGER adi16ibuf,adi16ilbuf
!           LOGICAL adi16inlbuf
!           COMMON /adi16fbuf/adi16buf,adi16lbuf,
!          +       adi16ibuf,adi16ilbuf,adi16inlbuf
      REAL*4 adr4buf(512), adr4lbuf(512)
      INTEGER adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf, &
             adr4ibuf,adr4ilbuf,adr4inlbuf
      REAL*8 adr8buf(512), adr8lbuf(512)
      INTEGER adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf, &
             adr8ibuf,adr8ilbuf,adr8inlbuf
      DOUBLE PRECISION adr16buf(512), adr16lbuf(512)
      INTEGER adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf, &
             adr16ibuf,adr16ilbuf,adr16inlbuf
!           REAL*32 x, adr32buf(512), adr32lbuf(512)
!           INTEGER adr32ibuf,adr32ilbuf
!           LOGICAL adr32inlbuf
!           COMMON /adr32fbuf/adr32buf,adr32lbuf,
!          +       adr32ibuf,adr32ilbuf,adr32inlbuf
!           COMPLEX*4 adc4buf(512), adc4lbuf(512)
!           INTEGER adc4ibuf,adc4ilbuf
!           LOGICAL adc4inlbuf
!           COMMON /adc4fbuf/adc4buf,adc4lbuf,
!          +       adc4ibuf,adc4ilbuf,adc4inlbuf
      COMPLEX*8 adc8buf(512), adc8lbuf(512)
      INTEGER adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf, &
             adc8ibuf,adc8ilbuf,adc8inlbuf
      COMPLEX*16 adc16buf(512), adc16lbuf(512)
      INTEGER adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf, &
             adc16ibuf,adc16ilbuf,adc16inlbuf
!           COMPLEX*32 adc32buf(512), adc32lbuf(512)
!           INTEGER adc32ibuf,adc32ilbuf
!           LOGICAL adc32inlbuf
!           COMMON /adc32fbuf/adc32buf,adc32lbuf,
!          +       adc32ibuf,adc32ilbuf,adc32inlbuf
      integer*4 smallstacksize
!     
      smallstacksize = 0
      smallstacksize = smallstacksize + (ads1ibuf-1)*1
      smallstacksize = smallstacksize + (adl4ibuf-1)*4
      smallstacksize = smallstacksize + (adi4ibuf-1)*4
      smallstacksize = smallstacksize + (adi8ibuf-1)*8
!           smallstacksize = smallstacksize + (adi16ibuf-1)*16
      smallstacksize = smallstacksize + (adr4ibuf-1)*4
      smallstacksize = smallstacksize + (adr8ibuf-1)*8
      smallstacksize = smallstacksize + (adr16ibuf-1)*16
!           smallstacksize = smallstacksize + (adr32ibuf-1)*32
!           smallstacksize = smallstacksize + (adc4ibuf-1)*4
      smallstacksize = smallstacksize + (adc8ibuf-1)*8
      smallstacksize = smallstacksize + (adc16ibuf-1)*16
!           smallstacksize = smallstacksize + (adc32ibuf-1)*32
!
      end

!========================================================
! PUSH* POP* SUBROUTINES FOR OTHER DATA TYPES
! Uncomment if these types are available on your compiler
!  and they are needed by the reverse differentiated code
! Don't forget to uncomment the corresponding lines in
!  subroutine PRINTBUFFERTOP, otherwise these types'
!  contribution to buffer occupation will not be seen.
!  (not very important anyway...)

!======================= INTEGER*16 =========================
!           BLOCK DATA INTEGERS16
!           INTEGER*16 adi16buf(512), adi16lbuf(512)
!           INTEGER adi16ibuf,adi16ilbuf
!           LOGICAL adi16inlbuf
!           COMMON /adi16fbuf/adi16buf,adi16lbuf,
!          +       adi16ibuf,adi16ilbuf,adi16inlbuf
!           DATA adi16ibuf/1/
!           DATA adi16ilbuf/-1/
!           DATA adi16inlbuf/.FALSE./
!           END
!     c
!           SUBROUTINE PUSHINTEGER16(x)
!           INTEGER*16 x, adi16buf(512), adi16lbuf(512)
!           INTEGER adi16ibuf,adi16ilbuf
!           LOGICAL adi16inlbuf
!           COMMON /adi16fbuf/adi16buf,adi16lbuf,
!          +       adi16ibuf,adi16ilbuf,adi16inlbuf
!     c
!           call addftraffic(16)
!           IF (adi16ilbuf.ne.-1) THEN
!              adi16ilbuf = -1
!              adi16inlbuf = .FALSE.
!           ENDIF
!           IF (adi16ibuf.ge.512) THEN
!              adi16buf(512) = x
!              CALL PUSHINTEGER16ARRAY(adi16buf, 512)
!           call addftraffic(-8192)
!              adi16ibuf = 1
!           ELSE
!              adi16buf(adi16ibuf) = x
!              adi16ibuf = adi16ibuf+1
!           ENDIF
!           END
!     
!           SUBROUTINE LOOKINTEGER16(x)
!           INTEGER*16 x, adi16buf(512), adi16lbuf(512)
!           INTEGER adi16ibuf,adi16ilbuf
!           LOGICAL adi16inlbuf
!           COMMON /adi16fbuf/adi16buf,adi16lbuf,
!          +       adi16ibuf,adi16ilbuf,adi16inlbuf
!     c
!           IF (adi16ilbuf.eq.-1) adi16ilbuf=adi16ibuf
!           IF (adi16ilbuf.le.1) THEN
!              CALL LOOKINTEGER16ARRAY(adi16lbuf, 512)
!              adi16inlbuf = .TRUE.
!              adi16ilbuf = 512
!              x = adi16lbuf(512)
!           ELSE
!              adi16ilbuf = adi16ilbuf-1
!              if (adi16inlbuf) THEN
!                 x = adi16lbuf(adi16ilbuf)
!              ELSE
!                 x = adi16buf(adi16ilbuf)
!              ENDIF
!           ENDIF
!           END
!     
!           SUBROUTINE POPINTEGER16(x)
!           INTEGER*16 x, adi16buf(512), adi16lbuf(512)
!           INTEGER adi16ibuf,adi16ilbuf
!           LOGICAL adi16inlbuf
!           COMMON /adi16fbuf/adi16buf,adi16lbuf,
!          +       adi16ibuf,adi16ilbuf,adi16inlbuf
!     c
!           IF (adi16ilbuf.ne.-1) THEN
!              adi16ilbuf = -1
!              adi16inlbuf = .FALSE.
!           ENDIF
!           IF (adi16ibuf.le.1) THEN
!              CALL POPINTEGER16ARRAY(adi16buf, 512)
!              adi16ibuf = 512
!              x = adi16buf(512)
!           ELSE
!              adi16ibuf = adi16ibuf-1
!              x = adi16buf(adi16ibuf)
!           ENDIF
!           END

!======================= REAL*32 =========================
!           BLOCK DATA REALS32
!           REAL*32 adr32buf(512), adr32lbuf(512)
!           INTEGER adr32ibuf,adr32ilbuf
!           LOGICAL adr32inlbuf
!           COMMON /adr32fbuf/adr32buf,adr32lbuf,
!          +       adr32ibuf,adr32ilbuf,adr32inlbuf
!           DATA adr32ibuf/1/
!           DATA adr32ilbuf/-1/
!           DATA adr32inlbuf/.FALSE./
!           END
!     c
!           SUBROUTINE PUSHREAL32(x)
!           REAL*32 x, adr32buf(512), adr32lbuf(512)
!           INTEGER adr32ibuf,adr32ilbuf
!           LOGICAL adr32inlbuf
!           COMMON /adr32fbuf/adr32buf,adr32lbuf,
!          +       adr32ibuf,adr32ilbuf,adr32inlbuf
!     c
!           call addftraffic(32)
!           IF (adr32ilbuf.ne.-1) THEN
!              adr32ilbuf = -1
!              adr32inlbuf = .FALSE.
!           ENDIF
!           IF (adr32ibuf.ge.512) THEN
!              adr32buf(512) = x
!              CALL PUSHREAL32ARRAY(adr32buf, 512)
!           call addftraffic(-16384)
!              adr32ibuf = 1
!           ELSE
!              adr32buf(adr32ibuf) = x
!              adr32ibuf = adr32ibuf+1
!           ENDIF
!           END
!     
!           SUBROUTINE LOOKREAL32(x)
!           REAL*32 x, adr32buf(512), adr32lbuf(512)
!           INTEGER adr32ibuf,adr32ilbuf
!           LOGICAL adr32inlbuf
!           COMMON /adr32fbuf/adr32buf,adr32lbuf,
!          +       adr32ibuf,adr32ilbuf,adr32inlbuf
!     c
!           IF (adr32ilbuf.eq.-1) adr32ilbuf=adr32ibuf
!           IF (adr32ilbuf.le.1) THEN
!              CALL LOOKREAL32ARRAY(adr32lbuf, 512)
!              adr32inlbuf = .TRUE.
!              adr32ilbuf = 512
!              x = adr32lbuf(512)
!           ELSE
!              adr32ilbuf = adr32ilbuf-1
!              if (adr32inlbuf) THEN
!                 x = adr32lbuf(adr32ilbuf)
!              ELSE
!                 x = adr32buf(adr32ilbuf)
!              ENDIF
!           ENDIF
!           END
!     
!           SUBROUTINE POPREAL32(x)
!           REAL*32 x, adr32buf(512), adr32lbuf(512)
!           INTEGER adr32ibuf,adr32ilbuf
!           LOGICAL adr32inlbuf
!           COMMON /adr32fbuf/adr32buf,adr32lbuf,
!          +       adr32ibuf,adr32ilbuf,adr32inlbuf
!     c
!           IF (adr32ilbuf.ne.-1) THEN
!              adr32ilbuf = -1
!              adr32inlbuf = .FALSE.
!           ENDIF
!           IF (adr32ibuf.le.1) THEN
!              CALL POPREAL32ARRAY(adr32buf, 512)
!              adr32ibuf = 512
!              x = adr32buf(512)
!           ELSE
!              adr32ibuf = adr32ibuf-1
!              x = adr32buf(adr32ibuf)
!           ENDIF
!           END

!======================= COMPLEX*4 =========================
!           BLOCK DATA COMPLEXS4
!           COMPLEX*4 adc4buf(512), adc4lbuf(512)
!           INTEGER adc4ibuf,adc4ilbuf
!           LOGICAL adc4inlbuf
!           COMMON /adc4fbuf/adc4buf,adc4lbuf,
!          +       adc4ibuf,adc4ilbuf,adc4inlbuf
!           DATA adc4ibuf/1/
!           DATA adc4ilbuf/-1/
!           DATA adc4inlbuf/.FALSE./
!           END
!     c
!           SUBROUTINE PUSHCOMPLEX4(x)
!           COMPLEX*4 x, adc4buf(512), adc4lbuf(512)
!           INTEGER adc4ibuf,adc4ilbuf
!           LOGICAL adc4inlbuf
!           COMMON /adc4fbuf/adc4buf,adc4lbuf,
!          +       adc4ibuf,adc4ilbuf,adc4inlbuf
!     c
!           call addftraffic(4)
!           IF (adc4ilbuf.ne.-1) THEN
!              adc4ilbuf = -1
!              adc4inlbuf = .FALSE.
!           ENDIF
!           IF (adc4ibuf.ge.512) THEN
!              adc4buf(512) = x
!              CALL PUSHCOMPLEX4ARRAY(adc4buf, 512)
!           call addftraffic(-2048)
!              adc4ibuf = 1
!           ELSE
!              adc4buf(adc4ibuf) = x
!              adc4ibuf = adc4ibuf+1
!           ENDIF
!           END
!     
!           SUBROUTINE LOOKCOMPLEX4(x)
!           COMPLEX*4 x, adc4buf(512), adc4lbuf(512)
!           INTEGER adc4ibuf,adc4ilbuf
!           LOGICAL adc4inlbuf
!           COMMON /adc4fbuf/adc4buf,adc4lbuf,
!          +       adc4ibuf,adc4ilbuf,adc4inlbuf
!     c
!           IF (adc4ilbuf.eq.-1) adc4ilbuf=adc4ibuf
!           IF (adc4ilbuf.le.1) THEN
!              CALL LOOKCOMPLEX4ARRAY(adc4lbuf, 512)
!              adc4inlbuf = .TRUE.
!              adc4ilbuf = 512
!              x = adc4lbuf(512)
!           ELSE
!              adc4ilbuf = adc4ilbuf-1
!              if (adc4inlbuf) THEN
!                 x = adc4lbuf(adc4ilbuf)
!              ELSE
!                 x = adc4buf(adc4ilbuf)
!              ENDIF
!           ENDIF
!           END
!     
!           SUBROUTINE POPCOMPLEX4(x)
!           COMPLEX*4 x, adc4buf(512), adc4lbuf(512)
!           INTEGER adc4ibuf,adc4ilbuf
!           LOGICAL adc4inlbuf
!           COMMON /adc4fbuf/adc4buf,adc4lbuf,
!          +       adc4ibuf,adc4ilbuf,adc4inlbuf
!     c
!           IF (adc4ilbuf.ne.-1) THEN
!              adc4ilbuf = -1
!              adc4inlbuf = .FALSE.
!           ENDIF
!           IF (adc4ibuf.le.1) THEN
!              CALL POPCOMPLEX4ARRAY(adc4buf, 512)
!              adc4ibuf = 512
!              x = adc4buf(512)
!           ELSE
!              adc4ibuf = adc4ibuf-1
!              x = adc4buf(adc4ibuf)
!           ENDIF
!           END

!======================= COMPLEX*32 =========================
!           BLOCK DATA COMPLEXS32
!           COMPLEX*32 adc32buf(512), adc32lbuf(512)
!           INTEGER adc32ibuf,adc32ilbuf
!           LOGICAL adc32inlbuf
!           COMMON /adc32fbuf/adc32buf,adc32lbuf,
!          +       adc32ibuf,adc32ilbuf,adc32inlbuf
!           DATA adc32ibuf/1/
!           DATA adc32ilbuf/-1/
!           DATA adc32inlbuf/.FALSE./
!           END
!     c
!           SUBROUTINE PUSHCOMPLEX32(x)
!           COMPLEX*32 x, adc32buf(512), adc32lbuf(512)
!           INTEGER adc32ibuf,adc32ilbuf
!           LOGICAL adc32inlbuf
!           COMMON /adc32fbuf/adc32buf,adc32lbuf,
!          +       adc32ibuf,adc32ilbuf,adc32inlbuf
!     c
!           call addftraffic(32)
!           IF (adc32ilbuf.ne.-1) THEN
!              adc32ilbuf = -1
!              adc32inlbuf = .FALSE.
!           ENDIF
!           IF (adc32ibuf.ge.512) THEN
!              adc32buf(512) = x
!              CALL PUSHCOMPLEX32ARRAY(adc32buf, 512)
!           call addftraffic(-16384)
!              adc32ibuf = 1
!           ELSE
!              adc32buf(adc32ibuf) = x
!              adc32ibuf = adc32ibuf+1
!           ENDIF
!           END
!     
!           SUBROUTINE LOOKCOMPLEX32(x)
!           COMPLEX*32 x, adc32buf(512), adc32lbuf(512)
!           INTEGER adc32ibuf,adc32ilbuf
!           LOGICAL adc32inlbuf
!           COMMON /adc32fbuf/adc32buf,adc32lbuf,
!          +       adc32ibuf,adc32ilbuf,adc32inlbuf
!     c
!           IF (adc32ilbuf.eq.-1) adc32ilbuf=adc32ibuf
!           IF (adc32ilbuf.le.1) THEN
!              CALL LOOKCOMPLEX32ARRAY(adc32lbuf, 512)
!              adc32inlbuf = .TRUE.
!              adc32ilbuf = 512
!              x = adc32lbuf(512)
!           ELSE
!              adc32ilbuf = adc32ilbuf-1
!              if (adc32inlbuf) THEN
!                 x = adc32lbuf(adc32ilbuf)
!              ELSE
!                 x = adc32buf(adc32ilbuf)
!              ENDIF
!           ENDIF
!           END
!     
!           SUBROUTINE POPCOMPLEX32(x)
!           COMPLEX*32 x, adc32buf(512), adc32lbuf(512)
!           INTEGER adc32ibuf,adc32ilbuf
!           LOGICAL adc32inlbuf
!           COMMON /adc32fbuf/adc32buf,adc32lbuf,
!          +       adc32ibuf,adc32ilbuf,adc32inlbuf
!     c
!           IF (adc32ilbuf.ne.-1) THEN
!              adc32ilbuf = -1
!              adc32inlbuf = .FALSE.
!           ENDIF
!           IF (adc32ibuf.le.1) THEN
!              CALL POPCOMPLEX32ARRAY(adc32buf, 512)
!              adc32ibuf = 512
!              x = adc32buf(512)
!           ELSE
!              adc32ibuf = adc32ibuf-1
!              x = adc32buf(adc32ibuf)
!           ENDIF
!           END

!========================================================
!        HOW TO CREATE PUSH* POP* SUBROUTINES
!              YET FOR OTHER DATA TYPES
! ** Duplicate the commented program lines below
! ** In the duplicated subroutines, replace:
!            TTTT by the basic name of the type
!            z9   by the initial and size of the type
!                (integer:i real:r complex:c boolean:b character:s)
!            9    by the size of the type
! ** Uncomment the duplicated subroutines
! ** Don't forget to insert the corresponding lines in
!  subroutine PRINTBUFFERTOP, otherwise these types'
!  contribution to buffer occupation will not be seen.
!  (not very important anyway...)

!======================= TTTT*9 =========================
!           BLOCK DATA TTTTS9
!           TTTT*9 adz9buf(512), adz9lbuf(512)
!           INTEGER adz9ibuf,adz9ilbuf
!           LOGICAL adz9inlbuf
!           COMMON /adz9fbuf/adz9buf,adz9lbuf,
!          +       adz9ibuf,adz9ilbuf,adz9inlbuf
!           DATA adz9ibuf/1/
!           DATA adz9ilbuf/-1/
!           DATA adz9inlbuf/.FALSE./
!           END
!     c
!           SUBROUTINE PUSHTTTT9(x)
!           TTTT*9 x, adz9buf(512), adz9lbuf(512)
!           INTEGER adz9ibuf,adz9ilbuf
!           LOGICAL adz9inlbuf
!           COMMON /adz9fbuf/adz9buf,adz9lbuf,
!          +       adz9ibuf,adz9ilbuf,adz9inlbuf
!     c
!           call addftraffic(9)
!           IF (adz9ilbuf.ne.-1) THEN
!              adz9ilbuf = -1
!              adz9inlbuf = .FALSE.
!           ENDIF
!           IF (adz9ibuf.ge.512) THEN
!              adz9buf(512) = x
!              CALL PUSHTTTT9ARRAY(adz9buf, 512)
!           call addftraffic(-9*512)
!              adz9ibuf = 1
!           ELSE
!              adz9buf(adz9ibuf) = x
!              adz9ibuf = adz9ibuf+1
!           ENDIF
!           END
!     
!           SUBROUTINE LOOKTTTT9(x)
!           TTTT*9 x, adz9buf(512), adz9lbuf(512)
!           INTEGER adz9ibuf,adz9ilbuf
!           LOGICAL adz9inlbuf
!           COMMON /adz9fbuf/adz9buf,adz9lbuf,
!          +       adz9ibuf,adz9ilbuf,adz9inlbuf
!     c
!           IF (adz9ilbuf.eq.-1) adz9ilbuf=adz9ibuf
!           IF (adz9ilbuf.le.1) THEN
!              CALL LOOKTTTT9ARRAY(adz9lbuf, 512)
!              adz9inlbuf = .TRUE.
!              adz9ilbuf = 512
!              x = adz9lbuf(512)
!           ELSE
!              adz9ilbuf = adz9ilbuf-1
!              if (adz9inlbuf) THEN
!                 x = adz9lbuf(adz9ilbuf)
!              ELSE
!                 x = adz9buf(adz9ilbuf)
!              ENDIF
!           ENDIF
!           END
!     
!           SUBROUTINE POPTTTT9(x)
!           TTTT*9 x, adz9buf(512), adz9lbuf(512)
!           INTEGER adz9ibuf,adz9ilbuf
!           LOGICAL adz9inlbuf
!           COMMON /adz9fbuf/adz9buf,adz9lbuf,
!          +       adz9ibuf,adz9ilbuf,adz9inlbuf
!     c
!           IF (adz9ilbuf.ne.-1) THEN
!              adz9ilbuf = -1
!              adz9inlbuf = .FALSE.
!           ENDIF
!           IF (adz9ibuf.le.1) THEN
!              CALL POPTTTT9ARRAY(adz9buf, 512)
!              adz9ibuf = 512
!              x = adz9buf(512)
!           ELSE
!              adz9ibuf = adz9ibuf-1
!              x = adz9buf(adz9ibuf)
!           ENDIF
!           END
