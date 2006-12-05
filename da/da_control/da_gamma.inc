! REV PROCESSED 213 LINES OF CODE. program doNE.
real function da_gamma(X)
!D    doUBLE PRECISION function DGAMMA(X)
!----------------------------------------------------------------------
! 
! THIS RoutinE CALCULATES THE GAMMA function FOR A real ARGUMENT X.
!   COMPUTATION IS BASED ON AN ALGORITHM outLINED in REFERENCE 1.
!   THE program useS RATIONAL functionS THAT APPROXIMATE THE GAMMA
!   function TO AT LEAST 20 SIGNifICANT DECIMAL DIGITS.  COEFFICIENTS
!   FOR THE APPROXIMATION OVER THE inTERVAL (1,2) ARE UNPUBLISHED.
!   THOSE FOR THE APPROXIMATION FOR X .GE. 12 ARE FROM REFERENCE 2.
!   THE ACCURACY ACHIEVED DEPendS ON THE ARITHMETIC SYSTEM, THE
!   COMPILER, THE inTRinSIC functionS, AND PROPER selectION OF THE
!   MACHinE-DEPendENT CONSTANTS.
! 
! 
!*******************************************************************
!*******************************************************************
! 
! EXPLANATION OF MACHinE-DEPendENT CONSTANTS
! 
! BETA   - RADIX FOR THE FLOATinG-POinT REpresentATION
! MAXEXP - THE SMALLEST POSITIVE POWER OF BETA THAT OVERFLOWS
! XBIG   - THE LARGEST ARGUMENT FOR WHICH GAMMA(X) IS REpresentABLE
!          in THE MACHinE, I.E., THE SOLUTION TO THE EQUATION
!                  GAMMA(XBIG) = BETA**MAXEXP
! XinF   - THE LARGEST MACHinE REpresentABLE FLOATinG-POinT NUMBER;
!          APPROXIMATELY BETA**MAXEXP
! EPS    - THE SMALLEST POSITIVE FLOATinG-POinT NUMBER SUCH THAT
!          1.0+EPS .GT. 1.0
! XMinin - THE SMALLEST POSITIVE FLOATinG-POinT NUMBER SUCH THAT
!          1/XMinin IS MACHinE REpresentABLE
! 
!     APPROXIMATE VALUES FOR SOME IMPORTANT MACHinES ARE:
! 
!                            BETA       MAXEXP        XBIG
! 
! CRAY-1         (S.P.)        2         8191        966.961
! CYBER 180/855
!   UNDER NOS    (S.P.)        2         1070        177.803
! IEEE (IBM/XT,
!   SUN, ETC.)   (S.P.)        2          128        35.040
! IEEE (IBM/XT,
!   SUN, ETC.)   (D.P.)        2         1024        171.624
! IBM 3033       (D.P.)       16           63        57.574
! VAX D-FORMAT   (D.P.)        2          127        34.844
! VAX G-FORMAT   (D.P.)        2         1023        171.489
! 
!                            XinF         EPS        XMinin
! 
! CRAY-1         (S.P.)   5.45E+2465   7.11E-15    1.84E-2466
! CYBER 180/855
!   UNDER NOS    (S.P.)   1.26E+322    3.55E-15    3.14E-294
! IEEE (IBM/XT,
!   SUN, ETC.)   (S.P.)   3.40E+38     1.19E-7     1.18E-38
! IEEE (IBM/XT,
!   SUN, ETC.)   (D.P.)   1.79D+308    2.22D-16    2.23D-308
! IBM 3033       (D.P.)   7.23D+75     2.22D-16    1.39D-76
! VAX D-FORMAT   (D.P.)   1.70D+38     1.39D-17    5.88D-39
! VAX G-FORMAT   (D.P.)   8.98D+307    1.11D-16    1.12D-308
! 
!*******************************************************************
!*******************************************************************
! 
! ERROR returnS
! 
!  THE program returnS THE VALUE XinF FOR SinGULARITIES OR
!     WHEN OVERFLOW WOULD OCCUR.  THE COMPUTATION IS BELIEVED
!     TO BE FREE OF UNDERFLOW AND OVERFLOW.
! 
! 
!  inTRinSIC functionS REQUIRED ARE:
! 
!     inT, DBLE, EXP, LOG, real, Sin
! 
! 
! REFERENCES:  AN OVERVIEW OF SOFTWARE DEVELOPMENT FOR SPECIAL
!              functionS   W. J. CODY, LECTURE NOTES in MATHEMATICS,
!              506, NUMERICAL ANALYSIS DUNDEE, 1975, G. A. WATSON
!              (ED.), SPRinGER VERLAG, BERLin, 1976.
! 
!              COMPUTER APPROXIMATIONS, HART, ET. AL., WILEY AND
!              SONS, NEW YORK, 1968.
! 
!  LATEST MODifICATION: OCTOBER 12, 1989
! 
!  AUTHORS: W. J. CODY AND L. STOLTZ
!           APPLIED MATHEMATICS DIVISION
!           ARGONNE NATIONAL LABORATORY
!           ARGONNE, IL 60439
! 
!----------------------------------------------------------------------
      integer I,N
      logical PARITY
!D    doUBLE PRECISION
      real :: &
         C,CONV,EPS,FACT,HALF,ONE,P,PI,Q,RES,sqrtPI,sum,TWELVE,  &
         TWO,X,XBIG,XDEN,XinF,XMinin,XNUM,Y,Y1,YSQ,Z,ZERO
      dimension C(7),P(8),Q(8)
!----------------------------------------------------------------------
!  MATHEMATICAL CONSTANTS
!----------------------------------------------------------------------
      DATA ONE,HALF,TWELVE,TWO,ZERO/1.0E0,0.5E0,12.0E0,2.0E0,0.0E0/,  &
           sqrtPI/0.9189385332046727417803297E0/,  &
           PI/3.1415926535897932384626434E0/
!D    DATA ONE,HALF,TWELVE,TWO,ZERO/1.0D0,0.5D0,12.0D0,2.0D0,0.0D0/,
!D   1     sqrtPI/0.9189385332046727417803297D0/,
!D   2     PI/3.1415926535897932384626434D0/
!----------------------------------------------------------------------
!  MACHinE DEPendENT parameterS
!----------------------------------------------------------------------
      DATA XBIG,XMinin,EPS/35.040E0,1.18E-38,1.19E-7/,  &
          XinF/3.4E38/
!D    DATA XBIG,XMinin,EPS/171.624D0,2.23D-308,2.22D-16/,
!D   1     XinF/1.79D308/
!----------------------------------------------------------------------
!  NUMERATOR AND DENOMinATOR COEFFICIENTS FOR RATIONAL MinIMAX
!     APPROXIMATION OVER (1,2).
!----------------------------------------------------------------------
      DATA P/-1.71618513886549492533811E+0,2.47656508055759199108314E+1, &
             -3.79804256470945635097577E+2,6.29331155312818442661052E+2, &
             8.66966202790413211295064E+2,-3.14512729688483675254357E+4, &
             -3.61444134186911729807069E+4,6.64561438202405440627855E+4/
      DATA Q/-3.08402300119738975254353E+1,3.15350626979604161529144E+2, &
            -1.01515636749021914166146E+3,-3.10777167157231109440444E+3, &
              2.25381184209801510330112E+4,4.75584627752788110767815E+3, &
            -1.34659959864969306392456E+5,-1.15132259675553483497211E+5/
!D    DATA P/-1.71618513886549492533811D+0,2.47656508055759199108314D+1,
!D   1       -3.79804256470945635097577D+2,6.29331155312818442661052D+2,
!D   2       8.66966202790413211295064D+2,-3.14512729688483675254357D+4,
!D   3       -3.61444134186911729807069D+4,6.64561438202405440627855D+4/
!D    DATA Q/-3.08402300119738975254353D+1,3.15350626979604161529144D+2,
!D   1      -1.01515636749021914166146D+3,-3.10777167157231109440444D+3,
!D   2        2.25381184209801510330112D+4,4.75584627752788110767815D+3,
!D   3      -1.34659959864969306392456D+5,-1.15132259675553483497211D+5/
!----------------------------------------------------------------------
!  COEFFICIENTS FOR MinIMAX APPROXIMATION OVER (12, inF).
!----------------------------------------------------------------------
      DATA C/-1.910444077728E-03,8.4171387781295E-04, &
           -5.952379913043012E-04,7.93650793500350248E-04, &
           -2.777777777777681622553E-03,8.333333333333333331554247E-02, &
            5.7083835261E-03/
!D    DATA C/-1.910444077728D-03,8.4171387781295D-04,
!D   1     -5.952379913043012D-04,7.93650793500350248D-04,
!D   2     -2.777777777777681622553D-03,8.333333333333333331554247D-02,
!D   3      5.7083835261D-03/
!----------------------------------------------------------------------
!  STATEMENT functionS FOR CONVERSION BETWEEN integer AND FLOAT
!----------------------------------------------------------------------
      CONV(I) = real(I)
!D    CONV(I) = DBLE(I)
      PARITY=.false.
      FACT=ONE
      N=0
      Y=X
      if(Y.LE.ZERO)then
!----------------------------------------------------------------------
!  ARGUMENT IS NEGATIVE
!----------------------------------------------------------------------
        Y=-X
        Y1=AinT(Y)
        RES=Y-Y1
        if(RES.NE.ZERO)then
          if(Y1.NE.AinT(Y1*HALF)*TWO)PARITY=.true.
          FACT=-PI/Sin(PI*RES)
          Y=Y+ONE
        else
          RES=XinF
          goto 900
        end if
      end if
!----------------------------------------------------------------------
!  ARGUMENT IS POSITIVE
!----------------------------------------------------------------------
      if(Y.LT.EPS)then
!----------------------------------------------------------------------
!  ARGUMENT .LT. EPS
!----------------------------------------------------------------------
        if(Y.GE.XMinin)then
          RES=ONE/Y
        else
          RES=XinF
          goto 900
        end if
      elseif(Y.LT.TWELVE)then
        Y1=Y
        if(Y.LT.ONE)then
!----------------------------------------------------------------------
!  0.0 .LT. ARGUMENT .LT. 1.0
!----------------------------------------------------------------------
          Z=Y
          Y=Y+ONE
        else
!----------------------------------------------------------------------
!  1.0 .LT. ARGUMENT .LT. 12.0, REDUCE ARGUMENT if NECESSARY
!----------------------------------------------------------------------
          N=inT(Y)-1
          Y=Y-CONV(N)
          Z=Y-ONE
        end if
!----------------------------------------------------------------------
!  EVALUATE APPROXIMATION FOR 1.0 .LT. ARGUMENT .LT. 2.0
!----------------------------------------------------------------------
        XNUM=ZERO
        XDEN=ONE
        do 260 I=1,8
          XNUM=(XNUM+P(I))*Z
          XDEN=XDEN*Z+Q(I)
  260   continue
        RES=XNUM/XDEN+ONE
        if(Y1.LT.Y)then
!----------------------------------------------------------------------
!  ADJUST RESULT FOR case  0.0 .LT. ARGUMENT .LT. 1.0
!----------------------------------------------------------------------
          RES=RES/Y1
        elseif(Y1.GT.Y)then
!----------------------------------------------------------------------
!  ADJUST RESULT FOR case  2.0 .LT. ARGUMENT .LT. 12.0
!----------------------------------------------------------------------
          do 290 I=1,N
            RES=RES*Y
            Y=Y+ONE
  290     continue
        end if
      else
!----------------------------------------------------------------------
!  EVALUATE FOR ARGUMENT .GE. 12.0,
!----------------------------------------------------------------------
        if(Y.LE.XBIG)then
          YSQ=Y*Y
          sum=C(7)
          do 350 I=1,6
            sum=sum/YSQ+C(I)
  350     continue
          sum=sum/Y-Y+sqrtPI
          sum=sum+(Y-HALF)*LOG(Y)
          RES=EXP(sum)
        else
          RES=XinF
          goto 900
        end if
      end if
!----------------------------------------------------------------------
!  FinAL ADJUSTMENTS AND return
!----------------------------------------------------------------------
      if(PARITY)RES=-RES
      if(FACT.NE.ONE)RES=FACT/RES
  900 da_gamma=RES
!D900 DGAMMA = RES
! ---------- LAST LINE OF GAMMA ----------
end function da_gamma
  
