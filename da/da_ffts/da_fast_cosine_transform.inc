!DECK FFT551
!     subroutine 'FFT551' - MULTIPLE FAST COSinE TRANSFORM
!
!     AUTHOR: CLIVE TEMPERTON, MAY 1988
!     [ALL-FORTRAN VERSION: C.T., OCTOBER 1995]
!
!     COSinE TRANSFORM OF LENGTH N IS CONVERTED TO
!     real PERIODIC TRANSFORM OF LENGTH N BY PRE- AND POST-
!     PROCESSinG. real PERIODIC TRANSFORM IS PERFORMED BY
!     PRUNinG REDUNDANT OPERATIONS FROM COMPLEX TRANSFORM.
!
!     SEE FOR EXAMPLE PAUL SWARZTRAUBER, "SYMMETRIC FFT'S",
!     MATH. COMP. 47 (1986), 323-346.
!
!     A IS THE ARRAY CONTAininG inPUT & outPUT DATA
!     WORK IS AN AREA OF size (N+1)*Min(LOT,64)
!     TRIGS IS A PREVIOUSLY PREPARED LIST OF TRIG function VALUES
!     ifAX IS A PREVIOUSLY PREPARED LIST OF FACTORS OF N
!     inC IS THE inCREMENT WITHin EACH DATA 'VECTOR'
!         (E.G. inC=1 FOR CONSECUTIVELY STORED DATA)
!     JUMP IS THE inCREMENT BETWEEN THE START OF EACH DATA VECTOR
!     N+1 IS THE LENGTH OF THE DATA VECTORS
!        (WHICH inCLUDE NONZERO VALUES AT BOTH endPOinTS)
!     LOT IS THE NUMBER OF DATA VECTORS
!     ISIGN = +1 FOR TRANSFORM FROM SPECTRAL TO GRIDPOinT
!           = -1 FOR TRANSFORM FROM GRIDPOinT TO SPECTRAL
!
!     ORDERinG OF COEFFICIENTS:   Z(0) , Z(1) , Z(2) , ... , Z(N)
!
!     ORDERinG OF DATA:           X(0) , X(1) , X(2) , ... , X(N)
!
!     VECTORIZATION IS ACHIEVED ON CRAY BY doinG THE TRANSFORMS
!     in PARALLEL
!
!     N MUST BE COMPOSED OF FACTORS 2,3 & 5 AND MUST BE EVEN
!
!     DEFinITION OF TRANSFORMS:
!     -------------------------
!
!     ISIGN=+1: X(I)=sum(J=0,...,N)(E(J)*Z(J)*COS(I*J*PI/N))
!                    WHERE E(J)=0.5 FOR J=0,N --- else E(J)=1
!
!     ISIGN=-1: Z(J)=(2/N)*sum(I=0,...,N)(E(I)*X(I)*COS(I*J*PI/N))
!
! N.B.  FFT551 has an unusual definition of the FFTs,
!       such that the the coeff of wave0 is NOT the mean.
!
!---------------------------------------------------------------------
Subroutine da_fast_cosine_transform & ! in
 ( ISIGN,                & ! in
   inC,                  & ! in
   JUMP,                 & ! in
   LOT,                  & ! in
   N,                    & ! in
   ifAX,                 & ! in
   TRIGS,                & ! in
   A,                    & ! inout
   IDIM )                   ! in

! Code Description:  ORIGinAL CODE F77 IS HARDLY TOUCHED !!!

 integer , intent (in)    :: ISIGN         ! Switch forward (-1) or inverse (+1)
 integer , intent (in)    :: inC           ! increment within each data
                                           ! vector  (e.g. inC=1 for 
                                           ! consecutively stored data)
 integer , intent (in)    :: Jump          ! increment between start of
                                           ! data vectors
 integer , intent (in)    :: LOT           ! Number of data vectors
 integer , intent (in)    :: N             ! N+1 is the length of the data 

 integer , intent (in)    :: ifAX(10)      ! previously prepared list of 
                                           ! factors of N
 
 real    , intent (in)    :: TRIGS(3*N)    ! previously prepared list of 
                                           ! trigonometric function values
 real    , intent (inout) :: A( inC*(N+1) + JUMP*(LOT-1) ) ! data array                                       !  vectors  (which include zeros 
                                           ! at the endpoints)
 integer , intent (in)    :: IDIM           ! dimension workspace 

 real :: WORK(IDIM)                      ! size (n+1)*min(lot,VectorLength)
 integer                  :: NFAX,NX,NH
 integer                  :: NBLOX,NVEX,NB
 integer                  :: K, IC, J, LA, IGO, JA,JB,IA,IB
 integer                  :: ifAC,IERR,ISTART

 real                     :: CO,S, t1,t2,si,scale
 integer                  :: vectorlength

      VectorLength = LOT
      NFAX=ifAX(1)
      NX=N+1
      NH=N/2
      NBLOX=1+(LOT-1)/VectorLength
      NVEX=LOT-(NBLOX-1)*VectorLength
      ISTART=1
!
      do 200 NB=1,NBLOX
!
!     PREPROCESSinG
!     -------------
      IA=ISTART
      IB=IA+NH*inC
      IC=IA+N*inC
      JA=1
      JB=NH+1
      if (MOD(NFAX,2).EQ.1) then
!DIR$ IVDEP
         do 105 J=1,NVEX
         T1=0.5*(A(IA)+A(IC))
         T2=0.5*(A(IA)-A(IC))
         A(IA)=T1
         A(IC)=T2
         IA=IA+JUMP
         IC=IC+JUMP
  105    CONTinUE
      else  
!DIR$ IVDEP
         do 110 J=1,NVEX
         WORK(JA)=0.5*(A(IA)+A(IC))
         WORK(JB)=A(IB)
         A(IC)=0.5*(A(IA)-A(IC))
         IA=IA+JUMP
         IB=IB+JUMP
         IC=IC+JUMP
         JA=JA+NX
         JB=JB+NX
  110    CONTinUE
      end if
!
      do 130 K=1,NH-1
      JA=K+1
      JB=N+1-K
      IA=ISTART+K*inC
      IB=ISTART+(JB-1)*inC
      IC=ISTART+N*inC
      SI=TRIGS(2*N+K)
      CO=TRIGS(2*N+NH-K)
      if (MOD(NFAX,2).EQ.1) then
!DIR$ IVDEP
         do 115 J=1,NVEX
         T1 = 0.5*(A(IA)+A(IB)) - SI*(A(IA)-A(IB))
         T2 = 0.5*(A(IA)+A(IB)) + SI*(A(IA)-A(IB))
         A(IC) = A(IC) + CO*(A(IA)-A(IB))
         A(IA) = T1
         A(IB) = T2
         IA=IA+JUMP
         IB=IB+JUMP
         IC=IC+JUMP
  115    CONTinUE
      else
!DIR$ IVDEP
         do 120 J=1,NVEX
         WORK(JA) = 0.5*(A(IA)+A(IB)) - SI*(A(IA)-A(IB))
         WORK(JB) = 0.5*(A(IA)+A(IB)) + SI*(A(IA)-A(IB))
         A(IC) = A(IC) + CO*(A(IA)-A(IB))
         IA=IA+JUMP
         IB=IB+JUMP
         IC=IC+JUMP
         JA=JA+NX
         JB=JB+NX
  120    CONTinUE
      end if
  130 CONTinUE
!
!     PERIODIC FOURIER ANALYSIS
!     -------------------------
      IA=ISTART
      LA=N
      IGO=1-2*MOD(NFAX,2)
!
      do 140 K=1,NFAX
      ifAC=ifAX(NFAX+2-K)
      LA=LA/ifAC
      IERR=-1
      if (IGO.EQ.+1) then
        call da_qpassm(WORK,WORK(ifAC*LA+1),A(IA),A(IA+LA*inC), &
                    TRIGS,1,inC,NX,JUMP,NVEX,N,ifAC,LA,IERR)
      else if (IGO.EQ.-1) then
        call da_qpassm(A(IA),A(IA+ifAC*LA*inC),WORK,WORK(LA+1), &
                    TRIGS,inC,1,JUMP,NX,NVEX,N,ifAC,LA,IERR)
      end if
      if (IERR.NE.0) goto 500
      IGO=-IGO
  140 CONTinUE
!
!     POSTPROCESSinG
!     --------------
      SCALE=2.0
      if (ISIGN.EQ.+1) SCALE = FLOAT(N)
      S=1.0
      if (ISIGN.EQ.-1) S = 2.0/FLOAT(N)
      JA=ISTART
      JB=JA+N*inC
      IA=1
      IB=N
!DIR$ IVDEP
      do 150 J=1,NVEX
      A(JA)=SCALE*WORK(IA)
      A(JA+inC)=S*A(JB)
      A(JB)=SCALE*WORK(IB)
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
  150 CONTinUE
!
      do 170 K=2,N-2,2
      JA=ISTART+K*inC
      IA=K
!DIR$ IVDEP
      do 160 J=1,NVEX
      A(JA)=SCALE*WORK(IA)
      A(JA+inC)=-SCALE*WORK(IA+1)+A(JA-inC)
      IA=IA+NX
      JA=JA+JUMP
  160 CONTinUE
  170 CONTinUE
!
      ISTART=ISTART+NVEX*JUMP
      NVEX=VectorLength
  200 CONTinUE
      goto 570
!
!     ERROR MESSAGES
!     --------------
  500 CONTinUE
      goto (510,530,550) IERR
  510 CONTinUE
      write(unit=stderr,fmt='(A,I4,A)') &
        'VECTOR LENGTH =',NVEX,', GREATER THAN VectorLength'
      goto 570
  530 CONTinUE
      write(unit=stderr,fmt='(A,I3,A)') &
        'FACTOR =',ifAC,', NOT CATERED FOR'
      goto 570
  550 CONTinUE
      write(unit=stderr,fmt='(A,I3,A)') &
        'FACTOR =',ifAC,', ONLY CATERED FOR if LA*ifAC=N'
  570 CONTinUE

      return
      end subroutine da_fast_cosine_transform


