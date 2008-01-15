
MODULE module_nup_feedback

   implicit none

   integer, parameter       :: smooth_option = 1   

   interface copy_fcn
     module procedure copy_fcn_real, copy_fcn_double
   end interface

   interface copy_fcnm
     module procedure copy_fcnm_real, copy_fcnm_double
   end interface

   interface smoother
     module procedure smoother_real, smoother_double
   end interface

   contains

   SUBROUTINE copy_fcn_real ( cfld,                                 &  ! CD field
                           cids, cide, cjds, cjde, ckds, ckde,   &
                           nfld,                                 &  ! ND field
                           nids, nide, njds, njde, nkds, nkde,   &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde, 1 ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nids:nide, njds:njde, nkds:nkde, 1 ),INTENT(IN)  :: nfld

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints

     !  Loop over the coarse grid in the area of the fine mesh.  Do not
     !  process the coarse grid values that are along the lateral BC
     !  provided to the fine grid.  Since that is in the specified zone
     !  for the fine grid, it should not be used in any feedback to the
     !  coarse grid as it should not have changed.

     !  Due to peculiarities of staggering, it is simpler to handle the feedback
     !  for the staggerings based upon whether it is a even ratio (2::1, 4::1, etc.) or
     !  an odd staggering ratio (3::1, 5::1, etc.). 

     !  Though there are separate grid ratios for the i and j directions, this code
     !  is not general enough to handle aspect ratios .NE. 1 for the fine grid cell.
 
     !  These are local integer increments in the looping.  Basically, istag=1 means
     !  that we will assume one less point in the i direction.  Note that ci and cj
     !  have a maximum value that is decreased by istag and jstag, respectively.  

     !  Horizontal momentum feedback is along the face, not within the cell.  For a
     !  3::1 ratio, temperature would use 9 pts for feedback, while u and v use
     !  only 3 points for feedback from the nest to the parent.

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        IF      ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN
           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + jstag + 1
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, cj, ck, 1 ) = 0.
                    DO ijpoints = 1 , nri * nrj
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, cj, ck, 1 ) =  cfld( ci, cj, ck, 1 ) + &
                                             1./REAL(nri*nrj) * nfld( ni+ipoints , nj+jpoints , nk, 1 )
                    END DO
                 ENDDO
              ENDDO
           ENDDO

        ELSE IF ( (       xstag ) .AND. ( .NOT. ystag ) ) THEN
           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + jstag + 1
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, cj, ck, 1 ) = 0.
                    DO ijpoints = (nri+1)/2 , (nri+1)/2 + nri*(nri-1) , nri
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, cj, ck, 1 ) =  cfld( ci, cj, ck, 1 ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nj+jpoints , nk, 1 )
                    END DO
                 ENDDO
              ENDDO
           ENDDO

        ELSE IF ( ( .NOT. xstag ) .AND. (       ystag ) ) THEN
           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + jstag + 1
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, cj, ck, 1 ) = 0.
                    DO ijpoints = ( nrj*nrj +1 )/2 - nrj/2 , ( nrj*nrj +1 )/2 - nrj/2 + nrj-1
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, cj, ck, 1 ) =  cfld( ci, cj, ck, 1 ) + &
                                             1./REAL(    nrj) * nfld( ni+ipoints , nj+jpoints , nk, 1 )
                    END DO
                 ENDDO
              ENDDO
           ENDDO

        END IF

     !  Even refinement ratio

     ELSE IF ( MOD(nrj,2) .EQ. 0) THEN
        IF ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN

        !  This is a simple schematic of the feedback indexing used in the even
        !  ratio nests.  For simplicity, a 2::1 ratio is depicted.  Only the 
        !  mass variable staggering is shown. 
        !                                                                  Each of
        !  the boxes with a "T" and four small "t" represents a coarse grid (CG)
        !  cell, that is composed of four (2::1 ratio) fine grid (FG) cells.
   
        !  Shown below is the area of the CG that is in the area of the FG.   The
        !  first grid point of the depicted CG is the starting location of the nest
        !  in the parent domain (ipos,jpos - i_parent_start and j_parent_start from
        !  the namelist).  
   
        !  For each of the CG points, the feedback loop is over each of the FG points
        !  within the CG cell.  For a 2::1 ratio, there are four total points (this is 
        !  the ijpoints loop).  The feedback value to the CG is the arithmetic mean of 
        !  all of the FG values within each CG cell.

!              |-------------||-------------|                          |-------------||-------------|
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
! jpos+        |             ||             |                          |             ||             |
! (njde-njds)- |      T      ||      T      |                          |      T      ||      T      |
! jstag        |             ||             |                          |             ||             |
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |-------------||-------------|                          |-------------||-------------|
!              |-------------||-------------|                          |-------------||-------------|
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |             ||             |                          |             ||             |
!              |      T      ||      T      |                          |      T      ||      T      |
!              |             ||             |                          |             ||             |
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |-------------||-------------|                          |-------------||-------------|
!
!                   ...
!                   ...
!                   ...
!                   ...
!                   ...

!              |-------------||-------------|                          |-------------||-------------|
! jpoints = 1  |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |             ||             |                          |             ||             |
!              |      T      ||      T      |                          |      T      ||      T      |
!              |             ||             |                          |             ||             |
! jpoints = 0, |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!  nj=3        |-------------||-------------|                          |-------------||-------------|
!              |-------------||-------------|                          |-------------||-------------|
! jpoints = 1  |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |             ||             |                          |             ||             |
!    jpos      |      T      ||      T      |          ...             |      T      ||      T      |
!              |             ||             |          ...             |             ||             |
! jpoints = 0, |  t      t   ||  t      t   |          ...             |  t      t   ||  t      t   |
!  nj=1        |-------------||-------------|                          |-------------||-------------|
!                     ^                                                                      ^
!                     |                                                                      |
!                     |                                                                      |
!                   ipos                                                                   ipos+
!     ni =        1              3                                                         (nide-nids)/nri
! ipoints=        0      1       0      1                                                  -istag
!

           !  For performance benefits, users can comment out the inner most loop (and cfld=0) and
           !  hardcode the loop feedback.  For example, it is set up to run a 2::1 ratio
           !  if uncommented.  This lacks generality, but is likely to gain timing benefits
           !  with compilers unable to unroll inner loops that do not have parameterized sizes.
   
           !  The extra +1 ---------/ and the extra -1 ----\  (both for ci and cj) 
           !                       /                        \   keeps the feedback out of the 
           !                      /                          \  outer row/col, since that CG data
           !                     /                            \ specified the nest boundary originally
           !                    /                              \   This
           !                   /                                \    is just
           !                  /                                  \   a sentence to not end a line
           !                 /                                    \   with a stupid backslash
           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + jstag
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + istag
                    cfld( ci, cj, ck, 1 ) = 0.
                    DO ijpoints = 1 , nri * nrj
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, cj, ck, 1 ) =  cfld( ci, cj, ck, 1 ) + &
                                             1./REAL(nri*nrj) * nfld( ni+ipoints , nj+jpoints , nk, 1 )
                    END DO
                 END DO
              END DO
           END DO

        !  U

        ELSE IF ( (       xstag ) .AND. ( .NOT. ystag ) ) THEN
!              |---------------|
!              |               |
! jpoints = 1  u       u       |
!              |               |
!              U               |
!              |               |
! jpoints = 0, u       u       |
!  nj=3        |               |
!              |---------------|
!              |---------------|
!              |               |
! jpoints = 1  u       u       |
!              |               |
!    jpos      U               |
!              |               |
! jpoints = 0, u       u       |
! nj=1         |               |
!              |---------------|
! 
!              ^               
!              |              
!              |             
!            ipos           
!     ni =     1               3
! ipoints=     0       1       0 
!

           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + 1
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + 1
                    cfld( ci, cj, ck, 1 ) = 0.
                    DO ijpoints = 1 , nri*nrj , nri
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, cj, ck, 1 ) =  cfld( ci, cj, ck, 1 ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nj+jpoints , nk, 1 )
                    END DO
                 ENDDO
              ENDDO
           ENDDO

        !  V 

        ELSE IF ( ( .NOT. xstag ) .AND. (       ystag ) ) THEN
           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + 1
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + 1
                    cfld( ci, cj, ck, 1 ) = 0.
                    DO ijpoints = 1 , nri
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, cj, ck, 1 ) =  cfld( ci, cj, ck, 1 ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nj+jpoints , nk, 1 )
                    END DO
                 ENDDO
              ENDDO
           ENDDO
        END IF
     END IF

     RETURN

   END SUBROUTINE copy_fcn_real

   SUBROUTINE copy_fcn_double ( cfld,                                 &  ! CD field
                           cids, cide, cjds, cjde, ckds, ckde,   &
                           nfld,                                 &  ! ND field
                           nids, nide, njds, njde, nkds, nkde,   &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     DOUBLE PRECISION, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde, 1 ), INTENT(OUT) :: cfld
     DOUBLE PRECISION, DIMENSION ( nids:nide, njds:njde, nkds:nkde, 1 ),INTENT(IN)  :: nfld

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints

     !  Loop over the coarse grid in the area of the fine mesh.  Do not
     !  process the coarse grid values that are along the lateral BC
     !  provided to the fine grid.  Since that is in the specified zone
     !  for the fine grid, it should not be used in any feedback to the
     !  coarse grid as it should not have changed.

     !  Due to peculiarities of staggering, it is simpler to handle the feedback
     !  for the staggerings based upon whether it is a even ratio (2::1, 4::1, etc.) or
     !  an odd staggering ratio (3::1, 5::1, etc.). 

     !  Though there are separate grid ratios for the i and j directions, this code
     !  is not general enough to handle aspect ratios .NE. 1 for the fine grid cell.
 
     !  These are local integer increments in the looping.  Basically, istag=1 means
     !  that we will assume one less point in the i direction.  Note that ci and cj
     !  have a maximum value that is decreased by istag and jstag, respectively.  

     !  Horizontal momentum feedback is along the face, not within the cell.  For a
     !  3::1 ratio, temperature would use 9 pts for feedback, while u and v use
     !  only 3 points for feedback from the nest to the parent.

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        IF      ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN
           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + jstag + 1
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, cj, ck, 1 ) = 0.
                    DO ijpoints = 1 , nri * nrj
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, cj, ck, 1 ) =  cfld( ci, cj, ck, 1 ) + &
                                             1./REAL(nri*nrj) * nfld( ni+ipoints , nj+jpoints , nk, 1 )
                    END DO
                 ENDDO
              ENDDO
           ENDDO

        ELSE IF ( (       xstag ) .AND. ( .NOT. ystag ) ) THEN
           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + jstag + 1
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, cj, ck, 1 ) = 0.
                    DO ijpoints = (nri+1)/2 , (nri+1)/2 + nri*(nri-1) , nri
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, cj, ck, 1 ) =  cfld( ci, cj, ck, 1 ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nj+jpoints , nk, 1 )
                    END DO
                 ENDDO
              ENDDO
           ENDDO

        ELSE IF ( ( .NOT. xstag ) .AND. (       ystag ) ) THEN
           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + jstag + 1
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, cj, ck, 1 ) = 0.
                    DO ijpoints = ( nrj*nrj +1 )/2 - nrj/2 , ( nrj*nrj +1 )/2 - nrj/2 + nrj-1
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, cj, ck, 1 ) =  cfld( ci, cj, ck, 1 ) + &
                                             1./REAL(    nrj) * nfld( ni+ipoints , nj+jpoints , nk, 1 )
                    END DO
                 ENDDO
              ENDDO
           ENDDO

        END IF

     !  Even refinement ratio

     ELSE IF ( MOD(nrj,2) .EQ. 0) THEN
        IF ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN

        !  This is a simple schematic of the feedback indexing used in the even
        !  ratio nests.  For simplicity, a 2::1 ratio is depicted.  Only the 
        !  mass variable staggering is shown. 
        !                                                                  Each of
        !  the boxes with a "T" and four small "t" represents a coarse grid (CG)
        !  cell, that is composed of four (2::1 ratio) fine grid (FG) cells.
   
        !  Shown below is the area of the CG that is in the area of the FG.   The
        !  first grid point of the depicted CG is the starting location of the nest
        !  in the parent domain (ipos,jpos - i_parent_start and j_parent_start from
        !  the namelist).  
   
        !  For each of the CG points, the feedback loop is over each of the FG points
        !  within the CG cell.  For a 2::1 ratio, there are four total points (this is 
        !  the ijpoints loop).  The feedback value to the CG is the arithmetic mean of 
        !  all of the FG values within each CG cell.

!              |-------------||-------------|                          |-------------||-------------|
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
! jpos+        |             ||             |                          |             ||             |
! (njde-njds)- |      T      ||      T      |                          |      T      ||      T      |
! jstag        |             ||             |                          |             ||             |
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |-------------||-------------|                          |-------------||-------------|
!              |-------------||-------------|                          |-------------||-------------|
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |             ||             |                          |             ||             |
!              |      T      ||      T      |                          |      T      ||      T      |
!              |             ||             |                          |             ||             |
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |-------------||-------------|                          |-------------||-------------|
!
!                   ...
!                   ...
!                   ...
!                   ...
!                   ...

!              |-------------||-------------|                          |-------------||-------------|
! jpoints = 1  |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |             ||             |                          |             ||             |
!              |      T      ||      T      |                          |      T      ||      T      |
!              |             ||             |                          |             ||             |
! jpoints = 0, |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!  nj=3        |-------------||-------------|                          |-------------||-------------|
!              |-------------||-------------|                          |-------------||-------------|
! jpoints = 1  |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |             ||             |                          |             ||             |
!    jpos      |      T      ||      T      |          ...             |      T      ||      T      |
!              |             ||             |          ...             |             ||             |
! jpoints = 0, |  t      t   ||  t      t   |          ...             |  t      t   ||  t      t   |
!  nj=1        |-------------||-------------|                          |-------------||-------------|
!                     ^                                                                      ^
!                     |                                                                      |
!                     |                                                                      |
!                   ipos                                                                   ipos+
!     ni =        1              3                                                         (nide-nids)/nri
! ipoints=        0      1       0      1                                                  -istag
!

           !  For performance benefits, users can comment out the inner most loop (and cfld=0) and
           !  hardcode the loop feedback.  For example, it is set up to run a 2::1 ratio
           !  if uncommented.  This lacks generality, but is likely to gain timing benefits
           !  with compilers unable to unroll inner loops that do not have parameterized sizes.
   
           !  The extra +1 ---------/ and the extra -1 ----\  (both for ci and cj) 
           !                       /                        \   keeps the feedback out of the 
           !                      /                          \  outer row/col, since that CG data
           !                     /                            \ specified the nest boundary originally
           !                    /                              \   This
           !                   /                                \    is just
           !                  /                                  \   a sentence to not end a line
           !                 /                                    \   with a stupid backslash
           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + jstag
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + istag
                    cfld( ci, cj, ck, 1 ) = 0.
                    DO ijpoints = 1 , nri * nrj
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, cj, ck, 1 ) =  cfld( ci, cj, ck, 1 ) + &
                                             1./REAL(nri*nrj) * nfld( ni+ipoints , nj+jpoints , nk, 1 )
                    END DO
                 END DO
              END DO
           END DO

        !  U

        ELSE IF ( (       xstag ) .AND. ( .NOT. ystag ) ) THEN
!              |---------------|
!              |               |
! jpoints = 1  u       u       |
!              |               |
!              U               |
!              |               |
! jpoints = 0, u       u       |
!  nj=3        |               |
!              |---------------|
!              |---------------|
!              |               |
! jpoints = 1  u       u       |
!              |               |
!    jpos      U               |
!              |               |
! jpoints = 0, u       u       |
! nj=1         |               |
!              |---------------|
! 
!              ^               
!              |              
!              |             
!            ipos           
!     ni =     1               3
! ipoints=     0       1       0 
!

           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + 1
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + 1
                    cfld( ci, cj, ck, 1 ) = 0.
                    DO ijpoints = 1 , nri*nrj , nri
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, cj, ck, 1 ) =  cfld( ci, cj, ck, 1 ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nj+jpoints , nk, 1 )
                    END DO
                 ENDDO
              ENDDO
           ENDDO

        !  V 

        ELSE IF ( ( .NOT. xstag ) .AND. (       ystag ) ) THEN
           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + 1
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + 1
                    cfld( ci, cj, ck, 1 ) = 0.
                    DO ijpoints = 1 , nri
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, cj, ck, 1 ) =  cfld( ci, cj, ck, 1 ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nj+jpoints , nk, 1 )
                    END DO
                 ENDDO
              ENDDO
           ENDDO
        END IF
     END IF

     RETURN

   END SUBROUTINE copy_fcn_double
!==================================

   SUBROUTINE copy_fcnm_real (  cfld,                                 &  ! CD field
                           cids, cide, cjds, cjde, ckds, ckde,   &
                           nfld,                                 &  ! ND field
                           nids, nide, njds, njde, nkds, nkde,   &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde, 1 ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nids:nide, njds:njde, nkds:nkde, 1 ), INTENT(IN) :: nfld

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        DO ck = ckds, ckde
           nk = ck
           DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                 ni = (ci-ipos)*nri + istag + 1
                 cfld( ci, cj, ck, 1 ) =  nfld( ni  , nj , nk, 1  )
              ENDDO
           ENDDO
        ENDDO

     ELSE  ! even refinement ratio, pick nearest neighbor on SW corner
        DO ck = ckds, ckde
           nk = ck
           DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
              nj = (cj-jpos)*nrj + 1
              DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                 ni = (ci-ipos)*nri + 1
                 ipoints = nri/2 -1
                 jpoints = nrj/2 -1
                 cfld( ci, cj, ck, 1 ) =  nfld( ni+ipoints , nj+jpoints , nk, 1 )
              END DO
           END DO
        END DO

     END IF

     RETURN

   END SUBROUTINE copy_fcnm_real

   SUBROUTINE copy_fcnm_double (  cfld,                                 &  ! CD field
                           cids, cide, cjds, cjde, ckds, ckde,   &
                           nfld,                                 &  ! ND field
                           nids, nide, njds, njde, nkds, nkde,   &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     DOUBLE PRECISION, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde, 1 ), INTENT(OUT) :: cfld
     DOUBLE PRECISION, DIMENSION ( nids:nide, njds:njde, nkds:nkde, 1 ), INTENT(IN) :: nfld

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        DO ck = ckds, ckde
           nk = ck
           DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                 ni = (ci-ipos)*nri + istag + 1
                 cfld( ci, cj, ck , 1) =  nfld( ni  , nj , nk , 1  )
              ENDDO
           ENDDO
        ENDDO

     ELSE  ! even refinement ratio, pick nearest neighbor on SW corner
        DO ck = ckds, ckde
           nk = ck
           DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
              nj = (cj-jpos)*nrj + 1
              DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                 ni = (ci-ipos)*nri + 1
                 ipoints = nri/2 -1
                 jpoints = nrj/2 -1
                 cfld( ci, cj, ck, 1 ) =  nfld( ni+ipoints , nj+jpoints , nk, 1 )
              END DO
           END DO
        END DO

     END IF

     RETURN

   END SUBROUTINE copy_fcnm_double
!==================================
! this is the 1pt function used in feedback for integers

   SUBROUTINE copy_fcni ( cfld,                                 &  ! CD field
                           cids, cide, cjds, cjde, ckds, ckde,   &
                           nfld,                                 &  ! ND field
                           nids, nide, njds, njde, nkds, nkde,   &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     INTEGER, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde ), INTENT(OUT) :: cfld
     INTEGER, DIMENSION ( nids:nide, njds:njde, nkds:nkde ), INTENT(IN) :: nfld

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        DO ck = ckds, ckde
           nk = ck
           DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                 ni = (ci-ipos)*nri + istag + 1
                 cfld( ci, cj, ck ) =  nfld( ni  , nj , nk  )
              ENDDO
           ENDDO
        ENDDO

     ELSE  ! even refinement ratio
        DO ck = ckds, ckde
           nk = ck
           DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
              nj = (cj-jpos)*nrj + 1
              DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                 ni = (ci-ipos)*nri + 1
                 ipoints = nri/2 -1
                 jpoints = nrj/2 -1
                 cfld( ci, cj, ck ) =  nfld( ni+ipoints , nj+jpoints , nk )
              END DO
           END DO
        END DO

     END IF

     RETURN

   END SUBROUTINE copy_fcni

!==================================
   SUBROUTINE smoother_real ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      nids, nide, nkds, nkde, njds, njde,   &
                      xstag, ystag,                         &  ! staggering of field
                      ipos, jpos,                           &  ! Position of lower left of nest in
                      nri, nrj                              &
                      )
 
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cids:cide, ckds:ckde, cjds:cjde, 1 ) :: cfld

      !  These are the 2d smoothers used on the fedback data.  These filters
      !  are run on the coarse grid data (after the nested info has been
      !  fedback).  Only the area of the nest in the coarse grid is filtered.

      IF      ( smooth_option == 0 ) THEN
! no op
      ELSE IF ( smooth_option == 1 ) THEN
         CALL sm121 ( cfld , &
                      cids, cide, cjds, cjde, ckds, ckde,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, njds, njde, nkds, nkde,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
      ELSE IF ( smooth_option == 2 ) THEN
         CALL smdsm ( cfld , &
                      cids, cide, cjds, cjde, ckds, ckde,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, njds, njde, nkds, nkde,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
      END IF

   END SUBROUTINE smoother_real 

   SUBROUTINE sm121 ( cfld , &
                      cids, cide, cjds, cjde, ckds, ckde,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, njds, njde, nkds, nkde,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
   
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde, 1 ) :: cfld
      REAL, DIMENSION ( cids:cide,            cjds:cjde ) :: cfldnew
   
      INTEGER                        :: i , j , k , loop
      INTEGER :: istag,jstag

      INTEGER, PARAMETER  :: smooth_passes = 1 ! More passes requires a larger stencil (currently 48 pt)

      istag = 1 ; jstag = 1
      IF ( xstag ) istag = 0
      IF ( ystag ) jstag = 0
   
      !  Simple 1-2-1 smoother.
   
      smoothing_passes : DO loop = 1 , smooth_passes
   
         DO k = ckds , ckde
   
            !  Initialize dummy cfldnew

            DO i = MAX(ipos,cids-3) , MIN(ipos+(nide-nids)/nri,cide+3)
               DO j = MAX(jpos,cjds-3) , MIN(jpos+(njde-njds)/nrj,cjde+3)
                  cfldnew(i,j) = cfld(i,j,k,1) 
               END DO
            END DO

            !  1-2-1 smoothing in the j direction first, 
   
            DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
            DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
                  cfldnew(i,j) = 0.25 * ( cfld(i,j+1,k,1) + 2.*cfld(i,j,k,1) + cfld(i,j-1,k,1) )
               END DO
            END DO

            !  then 1-2-1 smoothing in the i direction last
       
            DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
            DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
                  cfld(i,j,k,1) =  0.25 * ( cfldnew(i+1,j) + 2.*cfldnew(i,j) + cfldnew(i-1,j) )
               END DO
            END DO
       
         END DO
    
      END DO smoothing_passes
   
   END SUBROUTINE sm121

   SUBROUTINE smdsm ( cfld , &
                      cids, cide, cjds, cjde, ckds, ckde,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, njds, njde, nkds, nkde,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
   
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde, 1 ) :: cfld
      REAL, DIMENSION ( cids:cide,            cjds:cjde ) :: cfldnew
   
      REAL , DIMENSION ( 2 )         :: xnu
      INTEGER                        :: i , j , k , loop , n 
      INTEGER :: istag,jstag

      INTEGER, PARAMETER  :: smooth_passes = 1 ! More passes requires a larger stencil (currently 48 pt)

      xnu  =  (/ 0.50 , -0.52 /)
    
      istag = 1 ; jstag = 1
      IF ( xstag ) istag = 0
      IF ( ystag ) jstag = 0
   
      !  The odd number passes of this are the "smoother", the even
      !  number passes are the "de-smoother" (note the different signs on xnu).
   
      smoothing_passes : DO loop = 1 , smooth_passes * 2
   
         n  =  2 - MOD ( loop , 2 )
    
         DO k = ckds , ckde
   
            DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
               DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
                  cfldnew(i,j) = cfld(i,j,k,1) + xnu(n) * ((cfld(i,j+1,k,1) + cfld(i,j-1,k,1)) * 0.5-cfld(i,j,k,1))
               END DO
            END DO
       
            DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
               DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
                  cfld(i,j,k,1) = cfldnew(i,j)
               END DO
            END DO
       
            DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
               DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
                  cfldnew(i,j) = cfld(i,j,k,1) + xnu(n) * ((cfld(i+1,j,k,1) + cfld(i-1,j,k,1)) * 0.5-cfld(i,j,k,1))
               END DO
            END DO
       
            DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
               DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
                  cfld(i,j,k,1) = cfldnew(i,j)
               END DO
            END DO
   
         END DO
    
      END DO smoothing_passes
   
   END SUBROUTINE smdsm

   SUBROUTINE smoother_double ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      nids, nide, nkds, nkde, njds, njde,   &
                      xstag, ystag,                         &  ! staggering of field
                      ipos, jpos,                           &  ! Position of lower left of nest in
                      nri, nrj                              &
                      )
 
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      DOUBLE PRECISION, DIMENSION ( cids:cide, ckds:ckde, cjds:cjde, 1 ) :: cfld

      !  These are the 2d smoothers used on the fedback data.  These filters
      !  are run on the coarse grid data (after the nested info has been
      !  fedback).  Only the area of the nest in the coarse grid is filtered.

      IF      ( smooth_option == 0 ) THEN
! no op
      ELSE IF ( smooth_option == 1 ) THEN
         CALL sm121_double ( cfld , &
                      cids, cide, cjds, cjde, ckds, ckde,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, njds, njde, nkds, nkde,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
      ELSE IF ( smooth_option == 2 ) THEN
         CALL smdsm_double ( cfld , &
                      cids, cide, cjds, cjde, ckds, ckde,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, njds, njde, nkds, nkde,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
      END IF

   END SUBROUTINE smoother_double 

   SUBROUTINE sm121_double ( cfld , &
                      cids, cide, cjds, cjde, ckds, ckde,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, njds, njde, nkds, nkde,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
   
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      DOUBLE PRECISION, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde, 1 ) :: cfld
      DOUBLE PRECISION, DIMENSION ( cids:cide,            cjds:cjde ) :: cfldnew
   
      INTEGER                        :: i , j , k , loop
      INTEGER :: istag,jstag

      INTEGER, PARAMETER  :: smooth_passes = 1 ! More passes requires a larger stencil (currently 48 pt)

      istag = 1 ; jstag = 1
      IF ( xstag ) istag = 0
      IF ( ystag ) jstag = 0
   
      !  Simple 1-2-1 smoother.
   
      smoothing_passes : DO loop = 1 , smooth_passes
   
         DO k = ckds , ckde
   
            !  Initialize dummy cfldnew

            DO i = MAX(ipos,cids-3) , MIN(ipos+(nide-nids)/nri,cide+3)
               DO j = MAX(jpos,cjds-3) , MIN(jpos+(njde-njds)/nrj,cjde+3)
                  cfldnew(i,j) = cfld(i,j,k,1) 
               END DO
            END DO

            !  1-2-1 smoothing in the j direction first, 
   
            DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
            DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
                  cfldnew(i,j) = 0.25 * ( cfld(i,j+1,k,1) + 2.*cfld(i,j,k,1) + cfld(i,j-1,k,1) )
               END DO
            END DO

            !  then 1-2-1 smoothing in the i direction last
       
            DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
            DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
                  cfld(i,j,k,1) =  0.25 * ( cfldnew(i+1,j) + 2.*cfldnew(i,j) + cfldnew(i-1,j) )
               END DO
            END DO
       
         END DO
    
      END DO smoothing_passes
   
   END SUBROUTINE sm121_double

   SUBROUTINE smdsm_double ( cfld , &
                      cids, cide, cjds, cjde, ckds, ckde,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, njds, njde, nkds, nkde,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
   
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      DOUBLE PRECISION, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde, 1 ) :: cfld
      DOUBLE PRECISION, DIMENSION ( cids:cide,            cjds:cjde ) :: cfldnew
   
      DOUBLE PRECISION , DIMENSION ( 2 )         :: xnu
      INTEGER                        :: i , j , k , loop , n 
      INTEGER :: istag,jstag

      INTEGER, PARAMETER  :: smooth_passes = 1 ! More passes requires a larger stencil (currently 48 pt)

      xnu  =  (/ 0.50 , -0.52 /)
    
      istag = 1 ; jstag = 1
      IF ( xstag ) istag = 0
      IF ( ystag ) jstag = 0
   
      !  The odd number passes of this are the "smoother", the even
      !  number passes are the "de-smoother" (note the different signs on xnu).
   
      smoothing_passes : DO loop = 1 , smooth_passes * 2
   
         n  =  2 - MOD ( loop , 2 )
    
         DO k = ckds , ckde
   
            DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
               DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
                  cfldnew(i,j) = cfld(i,j,k,1) + xnu(n) * ((cfld(i,j+1,k,1) + cfld(i,j-1,k,1)) * 0.5-cfld(i,j,k,1))
               END DO
            END DO
       
            DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
               DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
                  cfld(i,j,k,1) = cfldnew(i,j)
               END DO
            END DO
       
            DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
               DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
                  cfldnew(i,j) = cfld(i,j,k,1) + xnu(n) * ((cfld(i+1,j,k,1) + cfld(i-1,j,k,1)) * 0.5-cfld(i,j,k,1))
               END DO
            END DO
       
            DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
               DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
                  cfld(i,j,k,1) = cfldnew(i,j)
               END DO
            END DO
   
         END DO
    
      END DO smoothing_passes
   
   END SUBROUTINE smdsm_double

END MODULE module_nup_feedback

!==================================

