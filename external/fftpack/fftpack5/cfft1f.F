!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!   Copyright (C) 1995-2004, Scientific Computing Division,             
!   University Corporation for Atmospheric Research                     
!   Licensed under the GNU General Public License (GPL)                 
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: cfft1f.f,v 1.2 2004/06/15 21:08:32 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE CFFT1F (N, INC, C, LENC, WSAVE, LENSAV,                &
     &                  WORK, LENWRK, IER)                              
      INTEGER  N, INC, LENC, LENSAV, LENWRK, IER 
      COMPLEX  C(LENC) 
      REAL     WSAVE(LENSAV)     ,WORK(LENWRK) 
!                                                                       
      IER = 0 
!                                                                       
      IF (LENC .LT. INC*(N-1) + 1) THEN 
        IER = 1 
        CALL XERFFT ('CFFT1F ', 6) 
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))) + 4) THEN 
        IER = 2 
        CALL XERFFT ('CFFT1F ', 8) 
      ELSEIF (LENWRK .LT. 2*N) THEN 
        IER = 3 
        CALL XERFFT ('CFFTMF ', 10) 
      ENDIF 
!                                                                       
      IF (N .EQ. 1) RETURN 
!                                                                       
      IW1 = N+N+1 
      CALL C1FM1F (N,INC,C,WORK,WSAVE,WSAVE(IW1),                       &
     &                           WSAVE(IW1+1))                          
      RETURN 
      END                                           
