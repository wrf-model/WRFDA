Subroutine da_fast_sine_transform  & ! in
 ( ISIGN,              & ! in
   inC,                & ! in
   JUMP,               & ! in
   LOT,                & ! in
   N,                  & ! in
   ifAX,               & ! in
   TRIGS,              & ! in
   A,                  & ! inout
   DIM )                 ! in
!
!
! Description:
!     MULTIPLE FAST SinE TRANSFORM
!     (Originally called FFT661, then Var_SinTrans)
!      author: clive temperton, may 1988 
!       (slightly modified for all-fortran version)
!
!     Sine transform of length n is converted to
!     real periodic transform of length n by pre- and post-
!     processing. real periodic transform is performed by
!     pruning redundant operations from complex transform.
!
!     see for example paul swarztrauber, "symmetric fft's",
!     math. comp. 47 (1986), 323-346.
!
! Method:
!
!     ordering of coefficients:   z(0) , z(1) , z(2) , ... , z(n)
!     ordering of data:           x(0) , x(1) , x(2) , ... , x(n)
!
!    vectorization is achieved on cray by doing the transforms
!    in parallel
!
!    N must be composed of factors 2,3 & 5 and must be even
!
!    definition of transforms:
!     -------------------------
!
!     isign=+1: x(i)=sum(j=1,...,n-1)(z(j)*sin(i*j*pi/n))
!
!     isign=-1: z(j)=(2/n)*sum(i=1,...,n-1)(x(i)*sin(i*j*pi/n))
!
! Current Code Owner: Andrew Lorenc
!
!   History:
! Version   Date     Comment
! -------   ----     -------
! 0.1       14/12/93 Original code. Phil Andrews
! 0.2       16/09/94 Small Modifications for the
!                    incorporation in the VAR project. HB
! 1.1       21/04/95 placed under control. JB
! 1.2       01/06/95 Tracing added. JB
!
! Code Description:
!    NB   BECAuse OF THE TRICKY NESTED LOOPS 
!         ORIGinAL CODE F77 IS HARDLY TOUCHED !!!

Implicit none

! Subroutine arguments
 integer , intent (in)    :: ISIGN         ! Switch forward (-1) or inverse (+1)
 integer , intent (in)    :: inC           ! increment within each data
                                           ! vector  (e.g. inC=1 for 
                                           ! consecutively stored data)
 integer , intent (in)    :: Jump          ! increment between start of
                                           ! data vectors
 integer , intent (in)    :: LOT           ! Number of data vectors
 integer , intent (in)    :: N             ! N+1 is the length of the data 
                                           !  vectors  (which include zeros 
                                           ! at the endpoints)
 integer , intent (in)    :: DIM           ! dimension workspace 
 integer , intent (in)    :: ifAX(10)      ! previously prepared list of 
                                           ! factors of N
 
 real    , intent (in)    :: TRIGS(3*N)    ! previously prepared list of 
                                           ! trigonometric function values
 real    , intent (inout) :: A( inC*(N+1) + JUMP*(LOT-1) ) ! data array

                                                    ! No descriptions given
 integer                  :: NFAX,NX,NH
 integer                  :: NBLOX,NVEX,NB
 integer                  :: K,JA,JB,IA,IB,IGO,LA,J
 integer                  :: ifAC,IERR,ISTART

 real                     :: SI,T1,T2,SCALE
 integer                  :: vectorlength
 real                     :: WORK(DIM)     ! size (n+1)*min(lot,VectorLength)
     
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
      do 120 K=1,NH-1
      JA=K+1
      JB=N+1-K
      IA=ISTART+K*inC
      IB=ISTART+(JB-1)*inC
      SI=TRIGS(2*N+K)
      if (MOD(NFAX,2).EQ.0) then
!DIR$ IVDEP
         do 110 J=1,NVEX
         WORK(JA) = SI*(A(IA)+A(IB)) + 0.5*(A(IA)-A(IB))
         WORK(JB) = SI*(A(IA)+A(IB)) - 0.5*(A(IA)-A(IB))
         IA=IA+JUMP
         IB=IB+JUMP
         JA=JA+NX
         JB=JB+NX
  110    CONTinUE
      else
!DIR$ IVDEP
         do 115 J=1,NVEX
         T1 = SI*(A(IA)+A(IB)) + 0.5*(A(IA)-A(IB))
         T2 = SI*(A(IA)+A(IB)) - 0.5*(A(IA)-A(IB))
         A(IA) = T1
         A(IB) = T2
         IA=IA+JUMP
         IB=IB+JUMP
  115    CONTinUE
      end if
  120 CONTinUE

      JA=1
      JB=NH+1
      IA=ISTART
      IB=ISTART+NH*inC
      if (MOD(NFAX,2).EQ.0) then
!DIR$ IVDEP
         do 130 J=1,NVEX
         WORK(JA)=0.0
         WORK(JB)=2.0*A(IB)
         IB=IB+JUMP
         JA=JA+NX
         JB=JB+NX
  130    CONTinUE
         IGO = +1
      else
!DIR$ IVDEP
         do 135 J=1,NVEX
         A(IA)=0.0
         A(IB)=2.0*A(IB)
         IA=IA+JUMP
         IB=IB+JUMP
  135    CONTinUE
         IGO = -1
      end if
!
!     PERIODIC FOURIER ANALYSIS
!     -------------------------
      IA=ISTART
      LA=N
!
      do 140 K=1,NFAX
      ifAC=ifAX(NFAX+2-K)
      LA=LA/ifAC
      IERR=-1
      if (IGO.EQ.+1) then
        call da_qpassm(WORK,WORK(ifAC*LA+1),A(IA),A(LA*inC+IA), &
                    TRIGS,1,inC,NX,JUMP,NVEX,N,ifAC,LA,IERR)
      else if (IGO.EQ.-1) then
        call da_qpassm(A(IA),A(ifAC*LA*inC+IA),WORK,WORK(LA+1), &
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
      JA=ISTART
      JB=JA+N*inC
      IA=1
!DIR$ IVDEP
      do 150 J=1,NVEX
      A(JA)=0.0
      A(JA+inC)=0.5*SCALE*WORK(IA)
      A(JB)=0.0
      IA=IA+NX
      JA=JA+JUMP
      JB=JB+JUMP
  150 CONTinUE
!
      do 170 K=2,N-2,2
      JA=ISTART+K*inC
      IA=K
!DIR$ IVDEP
      do 160 J=1,NVEX
      A(JA)=-SCALE*WORK(IA+1)
      A(JA+inC)=SCALE*WORK(IA)+A(JA-inC)
      IA=IA+NX
      JA=JA+JUMP
  160 CONTinUE
  170 CONTinUE
!
      ISTART=ISTART+NVEX*JUMP
      NVEX=VectorLength
  200 CONTinUE

      Go To 570
!
!     ERROR MESSAGES
!     --------------
  500 CONTinUE
      goto (510,530,550) IERR
  510 CONTinUE
      write(unit=stderr,fmt='(A,I5,A)') 'NVEX=', NVEX ,' GREATER THAN VectorLength'
      goto 570
  530 CONTinUE
      write(unit=stderr,fmt='(A,I5,A)') 'ifAC=', ifAC, 'NOT CATERED FOR'
      goto 570
  550 CONTinUE
      write(unit=stderr,fmt='(A,I5,A)') 'ifAC=', ifAC, ' ONLY CATERED FOR if LA*ifAC=N'
  570 CONTinUE


      return


End subroutine da_fast_sine_transform



