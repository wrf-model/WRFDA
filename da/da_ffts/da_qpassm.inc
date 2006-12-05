!C     subroutine 'Var_QPASSM' - PERFORMS ONE PASS THROUGH DATA AS PART!C     OF MULTIPLE real FFT (FOURIER ANALYSIS) RoutinE
!C
!C     A IS FIRST real inPUT VECTOR
!C         EQUIVALENCE B(1) WITH A(ifAC*LA*inC1+1)
!C     C IS FIRST real outPUT VECTOR
!C         EQUIVALENCE D(1) WITH C(LA*inC2+1)
!C     TRIGS IS A PRECALCULATED LIST OF SinES & COSinES
!C     inC1 IS THE ADDRESSinG inCREMENT FOR A
!C     inC2 IS THE ADDRESSinG inCREMENT FOR C
!C     inC3 IS THE inCREMENT BETWEEN inPUT VECTORS A
!C     inC4 IS THE inCREMENT BETWEEN outPUT VECTORS C
!C     LOT IS THE NUMBER OF VECTORS
!C     N IS THE LENGTH OF THE VECTORS
!C     ifAC IS THE CURRENT FACTOR OF N
!C     LA = N/(PRODUCT OF FACTORS useD SO FAR)
!C     ERR IS AN ERROR inDICATOR:
!C              0 - PASS COMPLETED WITHout ERROR
!C              1 - LOT GREATER THAN VectorLength
!C              2 - ifAC NOT CATERED FOR
!C              3 - ifAC ONLY CATERED FOR if LA=N/ifAC
!C
!C-----------------------------------------------------------------------
!C
     subroutine da_qpassm(A,B,C,D,TRIGS,inC1,inC2,inC3,inC4,LOT,N,ifAC,LA,ERR)

      integer :: inc1, inc2, inc3, inc4, lot, n, ifac, la, err

      real  :: a, b, c, d, trigs
      dimension A(*),B(*),C(*),D(*),TRIGS(N)

      real  :: Sin36, Sin72, QRT5, Sin60
      DATA Sin36/0.587785252292473/,Sin72/0.951056516295154/, &
          QRT5/0.559016994374947/,Sin60/0.866025403784437/

      real    :: s1, s2, s3, s4, s5
      real    :: sin45, zsin36, zsin72, zqrt5, zsin60, zsin45, z
      real    :: a0, a1, a2, a3, a4, a5, a6, a10, a11, a20, a21
      real    :: b0, b1, b2, b3, b4, b5, b6, b10, b11, b20, b21
      real    :: c1, c2, c3, c4, c5
      integer :: i, ijk, l, k, kb, m, iink, jink, ijump, kstop
      integer :: ibad, igo, ia, ie, je, ibase, jbase, ja, jb, j, ic
      integer :: if, jf, kf, ib, jc, kc, id, jd, kd, ke, ig, ih
      integer :: vectorlength
!
!- End of header ---------------------------------------------------------------


      M=N/ifAC
      IinK=LA*inC1
      JinK=LA*inC2
      IJUMP=(ifAC-1)*IinK
      KSTOP=(N-ifAC)/(2*ifAC)
!
      IBAD=1
      VectorLength = lot
      if (LOT.GT.VectorLength) goto 910
      IBASE=0
      JBASE=0
      IGO=ifAC-1
      if (IGO.EQ.7) IGO=6
      IBAD=2
      if (IGO.LT.1.OR.IGO.GT.6) goto 910
      goto (200,300,400,500,600,800),IGO
!
!     CODinG FOR FACTOR 2
!     -------------------
  200 continue
      IA=1
      IB=IA+IinK
      JA=1
      JB=JA+(2*M-LA)*inC2
!
      if (LA.EQ.M) goto 290
!
      do 220 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 210 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      C(JB+J)=A(IA+I)-A(IB+I)
      I=I+inC3
      J=J+inC4
  210 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  220 continue
      JA=JA+JinK
      JinK=2*JinK
      JB=JB-JinK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IinK
      if (JA.EQ.JB) goto 260
      do 250 K=LA,KSTOP,LA
      KB=K+K
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      JBASE=0
      do 240 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 230 IJK=1,LOT
      C(JA+J)=A(IA+I)+(C1*A(IB+I)+S1*B(IB+I))
      C(JB+J)=A(IA+I)-(C1*A(IB+I)+S1*B(IB+I))
      D(JA+J)=(C1*B(IB+I)-S1*A(IB+I))+B(IA+I)
      D(JB+J)=(C1*B(IB+I)-S1*A(IB+I))-B(IA+I)
      I=I+inC3
      J=J+inC4
  230 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  240 continue
      IBASE=IBASE+IJUMP
      JA=JA+JinK
      JB=JB-JinK
  250 continue
      if (JA.GT.JB) goto 900
  260 continue
      JBASE=0
      do 280 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 270 IJK=1,LOT
      C(JA+J)=A(IA+I)
      D(JA+J)=-A(IB+I)
      I=I+inC3
      J=J+inC4
  270 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  280 continue
      goto 900
!
  290 continue
      Z=1.0/FLOAT(N)
      do 294 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 292 IJK=1,LOT
      C(JA+J)=Z*(A(IA+I)+A(IB+I))
      C(JB+J)=Z*(A(IA+I)-A(IB+I))
      I=I+inC3
      J=J+inC4
  292 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  294 continue
      goto 900
!
!     CODinG FOR FACTOR 3
!     -------------------
  300 continue
      IA=1
      IB=IA+IinK
      IC=IB+IinK
      JA=1
      JB=JA+(2*M-LA)*inC2
      JC=JB
!
      if (LA.EQ.M) goto 390
!
      do 320 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 310 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      C(JB+J)=A(IA+I)-0.5*(A(IB+I)+A(IC+I))
      D(JB+J)=Sin60*(A(IC+I)-A(IB+I))
      I=I+inC3
      J=J+inC4
  310 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  320 continue
      JA=JA+JinK
      JinK=2*JinK
      JB=JB+JinK
      JC=JC-JinK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IinK
      if (JA.EQ.JC) goto 360
      do 350 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      JBASE=0
      do 340 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 330 IJK=1,LOT
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C2*A(IC+I)+S2*B(IC+I))
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C2*B(IC+I)-S2*A(IC+I))
      A2=A(IA+I)-0.5*A1
      B2=B(IA+I)-0.5*B1
      A3=Sin60*((C1*A(IB+I)+S1*B(IB+I))-(C2*A(IC+I)+S2*B(IC+I)))
      B3=Sin60*((C1*B(IB+I)-S1*A(IB+I))-(C2*B(IC+I)-S2*A(IC+I)))
      C(JA+J)=A(IA+I)+A1
      D(JA+J)=B(IA+I)+B1
      C(JB+J)=A2+B3
      D(JB+J)=B2-A3
      C(JC+J)=A2-B3
      D(JC+J)=-(B2+A3)
      I=I+inC3
      J=J+inC4
  330 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  340 continue
      IBASE=IBASE+IJUMP
      JA=JA+JinK
      JB=JB+JinK
      JC=JC-JinK
  350 continue
      if (JA.GT.JC) goto 900
  360 continue
      JBASE=0
      do 380 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 370 IJK=1,LOT
      C(JA+J)=A(IA+I)+0.5*(A(IB+I)-A(IC+I))
      D(JA+J)=-Sin60*(A(IB+I)+A(IC+I))
      C(JB+J)=A(IA+I)-(A(IB+I)-A(IC+I))
      I=I+inC3
      J=J+inC4
  370 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  380 continue
      goto 900
!
  390 continue
      Z=1.0/FLOAT(N)
      ZSin60=Z*Sin60
      do 394 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 392 IJK=1,LOT
      C(JA+J)=Z*(A(IA+I)+(A(IB+I)+A(IC+I)))
      C(JB+J)=Z*(A(IA+I)-0.5*(A(IB+I)+A(IC+I)))
      D(JB+J)=ZSin60*(A(IC+I)-A(IB+I))
      I=I+inC3
      J=J+inC4
  392 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  394 continue
      goto 900
!
!     CODinG FOR FACTOR 4
!     -------------------
  400 continue
      IA=1
      IB=IA+IinK
      IC=IB+IinK
      ID=IC+IinK
      JA=1
      JB=JA+(2*M-LA)*inC2
      JC=JB+2*M*inC2
      JD=JB
!
      if (LA.EQ.M) goto 490
!
      do 420 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 410 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      C(JC+J)=(A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))
      C(JB+J)=A(IA+I)-A(IC+I)
      D(JB+J)=A(ID+I)-A(IB+I)
      I=I+inC3
      J=J+inC4
  410 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  420 continue
      JA=JA+JinK
      JinK=2*JinK
      JB=JB+JinK
      JC=JC-JinK
      JD=JD-JinK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IinK
      if (JB.EQ.JC) goto 460
      do 450 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      JBASE=0
      do 440 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 430 IJK=1,LOT
      A0=A(IA+I)+(C2*A(IC+I)+S2*B(IC+I))
      A2=A(IA+I)-(C2*A(IC+I)+S2*B(IC+I))
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C3*A(ID+I)+S3*B(ID+I))
      A3=(C1*A(IB+I)+S1*B(IB+I))-(C3*A(ID+I)+S3*B(ID+I))
      B0=B(IA+I)+(C2*B(IC+I)-S2*A(IC+I))
      B2=B(IA+I)-(C2*B(IC+I)-S2*A(IC+I))
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C3*B(ID+I)-S3*A(ID+I))
      B3=(C1*B(IB+I)-S1*A(IB+I))-(C3*B(ID+I)-S3*A(ID+I))
      C(JA+J)=A0+A1
      C(JC+J)=A0-A1
      D(JA+J)=B0+B1
      D(JC+J)=B1-B0
      C(JB+J)=A2+B3
      C(JD+J)=A2-B3
      D(JB+J)=B2-A3
      D(JD+J)=-(B2+A3)
      I=I+inC3
      J=J+inC4
  430 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  440 continue
      IBASE=IBASE+IJUMP
      JA=JA+JinK
      JB=JB+JinK
      JC=JC-JinK
      JD=JD-JinK
  450 continue
      if (JB.GT.JC) goto 900
  460 continue
      Sin45=sqrt(0.5)
      JBASE=0
      do 480 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 470 IJK=1,LOT
      C(JA+J)=A(IA+I)+Sin45*(A(IB+I)-A(ID+I))
      C(JB+J)=A(IA+I)-Sin45*(A(IB+I)-A(ID+I))
      D(JA+J)=-A(IC+I)-Sin45*(A(IB+I)+A(ID+I))
      D(JB+J)=A(IC+I)-Sin45*(A(IB+I)+A(ID+I))
      I=I+inC3
      J=J+inC4
  470 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  480 continue
      goto 900
!
  490 continue
      Z=1.0/FLOAT(N)
      do 494 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 492 IJK=1,LOT
      C(JA+J)=Z*((A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I)))
      C(JC+J)=Z*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I)))
      C(JB+J)=Z*(A(IA+I)-A(IC+I))
      D(JB+J)=Z*(A(ID+I)-A(IB+I))
      I=I+inC3
      J=J+inC4
  492 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  494 continue
      goto 900
!
!     CODinG FOR FACTOR 5
!     -------------------
  500 continue
      IA=1
      IB=IA+IinK
      IC=IB+IinK
      ID=IC+IinK
      IE=ID+IinK
      JA=1
      JB=JA+(2*M-LA)*inC2
      JC=JB+2*M*inC2
      JD=JC
      JE=JB
!
      if (LA.EQ.M) goto 590
!
      do 520 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 510 IJK=1,LOT
      A1=A(IB+I)+A(IE+I)
      A3=A(IB+I)-A(IE+I)
      A2=A(IC+I)+A(ID+I)
      A4=A(IC+I)-A(ID+I)
      A5=A(IA+I)-0.25*(A1+A2)
      A6=QRT5*(A1-A2)
      C(JA+J)=A(IA+I)+(A1+A2)
      C(JB+J)=A5+A6
      C(JC+J)=A5-A6
      D(JB+J)=-Sin72*A3-Sin36*A4
      D(JC+J)=-Sin36*A3+Sin72*A4
      I=I+inC3
      J=J+inC4
  510 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  520 continue
      JA=JA+JinK
      JinK=2*JinK
      JB=JB+JinK
      JC=JC+JinK
      JD=JD-JinK
      JE=JE-JinK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IinK
      if (JB.EQ.JD) goto 560
      do 550 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      JBASE=0
      do 540 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 530 IJK=1,LOT
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C4*A(IE+I)+S4*B(IE+I))
      A3=(C1*A(IB+I)+S1*B(IB+I))-(C4*A(IE+I)+S4*B(IE+I))
      A2=(C2*A(IC+I)+S2*B(IC+I))+(C3*A(ID+I)+S3*B(ID+I))
      A4=(C2*A(IC+I)+S2*B(IC+I))-(C3*A(ID+I)+S3*B(ID+I))
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C4*B(IE+I)-S4*A(IE+I))
      B3=(C1*B(IB+I)-S1*A(IB+I))-(C4*B(IE+I)-S4*A(IE+I))
      B2=(C2*B(IC+I)-S2*A(IC+I))+(C3*B(ID+I)-S3*A(ID+I))
      B4=(C2*B(IC+I)-S2*A(IC+I))-(C3*B(ID+I)-S3*A(ID+I))
      A5=A(IA+I)-0.25*(A1+A2)
      A6=QRT5*(A1-A2)
      B5=B(IA+I)-0.25*(B1+B2)
      B6=QRT5*(B1-B2)
      A10=A5+A6
      A20=A5-A6
      B10=B5+B6
      B20=B5-B6
      A11=Sin72*B3+Sin36*B4
      A21=Sin36*B3-Sin72*B4
      B11=Sin72*A3+Sin36*A4
      B21=Sin36*A3-Sin72*A4
      C(JA+J)=A(IA+I)+(A1+A2)
      C(JB+J)=A10+A11
      C(JE+J)=A10-A11
      C(JC+J)=A20+A21
      C(JD+J)=A20-A21
      D(JA+J)=B(IA+I)+(B1+B2)
      D(JB+J)=B10-B11
      D(JE+J)=-(B10+B11)
      D(JC+J)=B20-B21
      D(JD+J)=-(B20+B21)
      I=I+inC3
      J=J+inC4
  530 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  540 continue
      IBASE=IBASE+IJUMP
      JA=JA+JinK
      JB=JB+JinK
      JC=JC+JinK
      JD=JD-JinK
      JE=JE-JinK
  550 continue
      if (JB.GT.JD) goto 900
  560 continue
      JBASE=0
      do 580 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 570 IJK=1,LOT
      A1=A(IB+I)+A(IE+I)
      A3=A(IB+I)-A(IE+I)
      A2=A(IC+I)+A(ID+I)
      A4=A(IC+I)-A(ID+I)
      A5=A(IA+I)+0.25*(A3-A4)
      A6=QRT5*(A3+A4)
      C(JA+J)=A5+A6
      C(JB+J)=A5-A6
      C(JC+J)=A(IA+I)-(A3-A4)
      D(JA+J)=-Sin36*A1-Sin72*A2
      D(JB+J)=-Sin72*A1+Sin36*A2
      I=I+inC3
      J=J+inC4
  570 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  580 continue
      goto 900
!
  590 continue
      Z=1.0/FLOAT(N)
      ZQRT5=Z*QRT5
      ZSin36=Z*Sin36
      ZSin72=Z*Sin72
      do 594 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 592 IJK=1,LOT
      A1=A(IB+I)+A(IE+I)
      A3=A(IB+I)-A(IE+I)
      A2=A(IC+I)+A(ID+I)
      A4=A(IC+I)-A(ID+I)
      A5=Z*(A(IA+I)-0.25*(A1+A2))
      A6=ZQRT5*(A1-A2)
      C(JA+J)=Z*(A(IA+I)+(A1+A2))
      C(JB+J)=A5+A6
      C(JC+J)=A5-A6
      D(JB+J)=-ZSin72*A3-ZSin36*A4
      D(JC+J)=-ZSin36*A3+ZSin72*A4
      I=I+inC3
      J=J+inC4
  592 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  594 continue
      goto 900
!
!     CODinG FOR FACTOR 6
!     -------------------
  600 continue
      IA=1
      IB=IA+IinK
      IC=IB+IinK
      ID=IC+IinK
      IE=ID+IinK
      if=IE+IinK
      JA=1
      JB=JA+(2*M-LA)*inC2
      JC=JB+2*M*inC2
      JD=JC+2*M*inC2
      JE=JC
      JF=JB
!
      if (LA.EQ.M) goto 690
!
      do 620 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 610 IJK=1,LOT
      A11=(A(IC+I)+A(if+I))+(A(IB+I)+A(IE+I))
      C(JA+J)=(A(IA+I)+A(ID+I))+A11
      C(JC+J)=(A(IA+I)+A(ID+I)-0.5*A11)
      D(JC+J)=Sin60*((A(IC+I)+A(if+I))-(A(IB+I)+A(IE+I)))
      A11=(A(IC+I)-A(if+I))+(A(IE+I)-A(IB+I))
      C(JB+J)=(A(IA+I)-A(ID+I))-0.5*A11
      D(JB+J)=Sin60*((A(IE+I)-A(IB+I))-(A(IC+I)-A(if+I)))
      C(JD+J)=(A(IA+I)-A(ID+I))+A11
      I=I+inC3
      J=J+inC4
  610 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  620 continue
      JA=JA+JinK
      JinK=2*JinK
      JB=JB+JinK
      JC=JC+JinK
      JD=JD-JinK
      JE=JE-JinK
      JF=JF-JinK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IinK
      if (JC.EQ.JD) goto 660
      do 650 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      KF=KE+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      C5=TRIGS(KF+1)
      S5=TRIGS(KF+2)
      JBASE=0
      do 640 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 630 IJK=1,LOT
      A1=C1*A(IB+I)+S1*B(IB+I)
      B1=C1*B(IB+I)-S1*A(IB+I)
      A2=C2*A(IC+I)+S2*B(IC+I)
      B2=C2*B(IC+I)-S2*A(IC+I)
      A3=C3*A(ID+I)+S3*B(ID+I)
      B3=C3*B(ID+I)-S3*A(ID+I)
      A4=C4*A(IE+I)+S4*B(IE+I)
      B4=C4*B(IE+I)-S4*A(IE+I)
      A5=C5*A(if+I)+S5*B(if+I)
      B5=C5*B(if+I)-S5*A(if+I)
      A11=(A2+A5)+(A1+A4)
      A20=(A(IA+I)+A3)-0.5*A11
      A21=Sin60*((A2+A5)-(A1+A4))
      B11=(B2+B5)+(B1+B4)
      B20=(B(IA+I)+B3)-0.5*B11
      B21=Sin60*((B2+B5)-(B1+B4))
      C(JA+J)=(A(IA+I)+A3)+A11
      D(JA+J)=(B(IA+I)+B3)+B11
      C(JC+J)=A20-B21
      D(JC+J)=A21+B20
      C(JE+J)=A20+B21
      D(JE+J)=A21-B20
      A11=(A2-A5)+(A4-A1)
      A20=(A(IA+I)-A3)-0.5*A11
      A21=Sin60*((A4-A1)-(A2-A5))
      B11=(B5-B2)-(B4-B1)
      B20=(B3-B(IA+I))-0.5*B11
      B21=Sin60*((B5-B2)+(B4-B1))
      C(JB+J)=A20-B21
      D(JB+J)=A21-B20
      C(JD+J)=A11+(A(IA+I)-A3)
      D(JD+J)=B11+(B3-B(IA+I))
      C(JF+J)=A20+B21
      D(JF+J)=A21+B20
      I=I+inC3
      J=J+inC4
  630 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  640 continue
      IBASE=IBASE+IJUMP
      JA=JA+JinK
      JB=JB+JinK
      JC=JC+JinK
      JD=JD-JinK
      JE=JE-JinK
      JF=JF-JinK
  650 continue
      if (JC.GT.JD) goto 900
  660 continue
      JBASE=0
      do 680 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 670 IJK=1,LOT
      C(JA+J)=(A(IA+I)+0.5*(A(IC+I)-A(IE+I)))+ Sin60*(A(IB+I)-A(if+I))
      D(JA+J)=-(A(ID+I)+0.5*(A(IB+I)+A(if+I)))-Sin60*(A(IC+I)+A(IE+I))
      C(JB+J)=A(IA+I)-(A(IC+I)-A(IE+I))
      D(JB+J)=A(ID+I)-(A(IB+I)+A(if+I))
      C(JC+J)=(A(IA+I)+0.5*(A(IC+I)-A(IE+I)))-Sin60*(A(IB+I)-A(if+I))
      D(JC+J)=-(A(ID+I)+0.5*(A(IB+I)+A(if+I)))+Sin60*(A(IC+I)+A(IE+I))
      I=I+inC3
      J=J+inC4
  670 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  680 continue
      goto 900
!
  690 continue
      Z=1.0/FLOAT(N)
      ZSin60=Z*Sin60
      do 694 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 692 IJK=1,LOT
      A11=(A(IC+I)+A(if+I))+(A(IB+I)+A(IE+I))
      C(JA+J)=Z*((A(IA+I)+A(ID+I))+A11)
      C(JC+J)=Z*((A(IA+I)+A(ID+I))-0.5*A11)
      D(JC+J)=ZSin60*((A(IC+I)+A(if+I))-(A(IB+I)+A(IE+I)))
      A11=(A(IC+I)-A(if+I))+(A(IE+I)-A(IB+I))
      C(JB+J)=Z*((A(IA+I)-A(ID+I))-0.5*A11)
      D(JB+J)=ZSin60*((A(IE+I)-A(IB+I))-(A(IC+I)-A(if+I)))
      C(JD+J)=Z*((A(IA+I)-A(ID+I))+A11)
      I=I+inC3
      J=J+inC4
  692 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  694 continue
      goto 900
!
!     CODinG FOR FACTOR 8
!     -------------------
  800 continue
      IBAD=3
      if (LA.NE.M) goto 910
      IA=1
      IB=IA+IinK
      IC=IB+IinK
      ID=IC+IinK
      IE=ID+IinK
      if=IE+IinK
      IG=if+IinK
      IH=IG+IinK
      JA=1
      JB=JA+LA*inC2
      JC=JB+2*M*inC2
      JD=JC+2*M*inC2
      JE=JD+2*M*inC2
      Z=1.0/FLOAT(N)
      ZSin45=Z*sqrt(0.5)
!
      do 820 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      do 810 IJK=1,LOT
      C(JA+J)=Z*(((A(IA+I)+A(IE+I))+(A(IC+I)+A(IG+I)))+ &
         ((A(ID+I)+A(IH+I))+(A(IB+I)+A(if+I))))
      C(JE+J)=Z*(((A(IA+I)+A(IE+I))+(A(IC+I)+A(IG+I)))- &
         ((A(ID+I)+A(IH+I))+(A(IB+I)+A(if+I))))
      C(JC+J)=Z*((A(IA+I)+A(IE+I))-(A(IC+I)+A(IG+I)))
      D(JC+J)=Z*((A(ID+I)+A(IH+I))-(A(IB+I)+A(if+I)))
      C(JB+J)=Z*(A(IA+I)-A(IE+I))+ZSin45*((A(IH+I)-A(ID+I))-(A(if+I)-A(IB+I)))
      C(JD+J)=Z*(A(IA+I)-A(IE+I))-ZSin45*((A(IH+I)-A(ID+I))-(A(if+I)-A(IB+I)))
      D(JB+J)=ZSin45*((A(IH+I)-A(ID+I))+(A(if+I)-A(IB+I)))+Z*(A(IG+I)-A(IC+I))
      D(JD+J)=ZSin45*((A(IH+I)-A(ID+I))+(A(if+I)-A(IB+I)))-Z*(A(IG+I)-A(IC+I))
      I=I+inC3
      J=J+inC4
  810 continue
      IBASE=IBASE+inC1
      JBASE=JBASE+inC2
  820 continue
!

  900 continue
      IBAD=0
  910 continue
      ERR=IBAD

      return
      end subroutine da_qpassm
