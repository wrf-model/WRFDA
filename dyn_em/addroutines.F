SUBROUTINE calc_dpsdt ( itimestep, dt, psfc, psfcm, cn, &
                        avg_abs_dpsdt, dpsdt,           &
                        ids, ide, jds, jde,             &
                        ims, ime, jms, jme,             &
                        its, ite, jts, jte              )

  IMPLICIT NONE

#ifdef DM_PARALLEL
  include "mpif.h"
  logical, external :: wrf_dm_on_monitor
#endif
   
   ! Input data

  INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, &
                                      ims, ime, jms, jme, &
                                      its, ite, jts, jte
  
  INTEGER,    INTENT(IN   ) :: itimestep
  REAL,       INTENT(IN   ) :: dt
  REAL,       INTENT(  OUT) :: avg_abs_dpsdt

  REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN   ) :: psfc
  REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: psfcm
  REAL, DIMENSION( ims:ime , jms:jme ), INTENT(  OUT) :: dpsdt

  CHARACTER(LEN=2) ,     INTENT(IN   ) :: cn

  integer :: i, j, k, itf, jtf, ierror
  REAL    :: nsum

  itf=MIN(ite,ide-1)
  jtf=MIN(jte,jde-1)

  DO j=jts,jtf
  DO i=its,itf
     dpsdt(i,j) = (psfc(i,j)-psfcm(i,j))*3.*3600./(dt*100.)
  ENDDO
  ENDDO
  
  avg_abs_dpsdt = 0.
  DO j=jts,jtf
  DO i=its,itf
     avg_abs_dpsdt = avg_abs_dpsdt + abs(dpsdt(i,j))
  ENDDO
  ENDDO
  avg_abs_dpsdt = avg_abs_dpsdt/( (ide-ids)*(jde-jds) )

#ifdef DM_PARALLEL
  call MPI_ALLREDUCE( avg_abs_dpsdt, nsum, 1, MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )

  avg_abs_dpsdt = nsum

  IF ( wrf_dm_on_monitor() ) THEN
     write(unit=*, fmt='(A,A,A,i6,2x,f12.5)') &
          ' Step, Domain averaged abs(d',cn,'dt) = ', itimestep, avg_abs_dpsdt
  ENDIF
#else
     write(unit=*, fmt='(A,A,A,i6,2x,f12.5)') &
          ' Step, Domain averaged abs(d',cn,'dt) = ', itimestep, avg_abs_dpsdt
#endif

  IF ( cn .eq. 'ps' ) then
    DO j=jts,jtf
    DO i=its,itf
       psfcm(i,j) = psfc(i,j)
    ENDDO
    ENDDO
  ENDIF

END SUBROUTINE calc_dpsdt

SUBROUTINE calc_q_conv_3d ( moist_1, moist_2, dt,          &
                                q_convergence,                 &
                                n_moist,                       &
                                ids, ide, jds, jde, kds, kde,  &
                                ims, ime, jms, jme, kms, kme,  &
                                its, ite, jts, jte, kts, kte   )

  IMPLICIT NONE

  INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte  
  INTEGER ,          INTENT(IN   ) :: n_moist
  REAL    ,          INTENT(IN   ) :: dt
  REAL    , DIMENSION( ims:ime, kms:kme, jms:jme, n_moist ), &
                     INTENT(   IN) :: moist_1,moist_2
  REAL    , DIMENSION( ims:ime, kms:kme, jms:jme          ), &
                     INTENT(  OUT) :: q_convergence

  integer :: i, j, k, itf, jtf, ktf

  itf=MIN(ite,ide-1)
  jtf=MIN(jte,jde-1)
  ktf=MIN(kte,kde-1)

  DO j=jts,jtf
  DO k=kts,ktf
  DO i=its,itf
! assign convergence = qv to test the allocation
!     q_convergence(i,k,j) = moist_1(i,k,j,2)
! assume convergence = tendency (unit: (g/kg)/3h)
     q_convergence(i,k,j) = (moist_2(i,k,j,2)-moist_1(i,k,j,2))/dt &
                            *1000. * 3.*3600.
  ENDDO
  ENDDO
  ENDDO
  
END SUBROUTINE calc_q_conv_3d

SUBROUTINE calc_q_conv_2d ( moist_2, u_2, v_2,             &
                            dt, dx, q_conv_2d,             &
                            n_moist,                       &
                            ids, ide, jds, jde, kds, kde,  &
                            ims, ime, jms, jme, kms, kme,  &
                            its, ite, jts, jte, kts, kte   )

! Calculates vertically integrated water vapour convergence

  IMPLICIT NONE

  INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte  
  INTEGER ,          INTENT(IN   ) :: n_moist
  REAL    ,          INTENT(IN   ) :: dt
  REAL    ,          INTENT(IN   ) :: dx
  REAL    , DIMENSION( ims:ime, kms:kme, jms:jme, n_moist ), &
                     INTENT(   IN) :: moist_2
  REAL    , DIMENSION( ims:ime, kms:kme, jms:jme          ), &
                     INTENT(   IN) :: u_2,                   &
                                      v_2 
  REAL    , DIMENSION( ims:ime, jms:jme                   ), &
                     INTENT(  OUT) :: q_conv_2d


! Local varables
  integer :: i, j, k, itf, jtf, ktf
  REAL    , DIMENSION( ims:ime, kms:kme, jms:jme) :: q_conv_3d

  itf=MIN(ite,ide-1)
  jtf=MIN(jte,jde-1)
  ktf=MIN(kte,kde-1)

  DO j=jts,jtf
  DO k=kts,ktf
  DO i=its,itf
! water vapour convergence (unit: (g kg-1 s-1)
     q_conv_3d(i,k,j) = ((moist_2(i+1,k,j,2)*u_2(i+1,k,j) - &
                          moist_2(i-1,k,j,2)*u_2(i-1,k,j) + &
                          moist_2(i,k,j+1,2)*v_2(i,k,j+1) - &
                          moist_2(i,k,j-1,2)*v_2(i,k,j-1))*1000.)/(2.*dx)

  ENDDO
  ENDDO
  ENDDO


  DO i=its,itf
  DO j=jts,jtf
! Initialising q_conv_2d
     q_conv_2d(i,j) = 0.
  DO k=kts,ktf
! Calculate vertically integrated water convergence
     q_conv_2d(i,j) = q_conv_2d(i,j) +  q_conv_3d(i,k,j)
  ENDDO
  ENDDO
  ENDDO

END SUBROUTINE calc_q_conv_2d
