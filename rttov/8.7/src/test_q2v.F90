!
Program test_q2v
  ! Description:
  ! Main code to test functionalities of RTTOV gaz unit conversions
  ! The RTTOV6/7 reference profile is used to validate the tests
  !
  ! Copyright:
  !    This software was developed within the context of
  !    the EUMETSAT Satellite Application Facility on
  !    Numerical Weather Prediction (NWP SAF), under the
  !    Cooperation Agreement dated 25 November 1998, between
  !    EUMETSAT and the Met Office, UK, by one or more partners
  !    within the NWP SAF. The partners in the NWP SAF are
  !    the Met Office, ECMWF, KNMI and MeteoFrance.
  !
  !    Copyright 2002, EUMETSAT, All Rights Reserved.
  !
  ! Method:
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0    27/01/03  Original (P Brunel)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  Use rttov_const, Only :   &
       gas_id_watervapour  ,&
       gas_id_ozone        ,&
       gas_unit_specconc   ,&
       gas_unit_ppmv

  Use parkind1, Only : jpim     ,jprb
  Implicit None
#include "rttov_q2v.interface"
#include "rttov_v2q.interface"

  !- Local variables
  Integer(Kind=jpim) :: luin
  Integer(Kind=jpim) :: ios
  Integer(Kind=jpim) :: l,il
  Integer(Kind=jpim) :: nerr
  Integer(Kind=jpim) :: n
  Integer(Kind=jpim) :: List_of_gases(2)
  Real(Kind=jprb) :: ref_h2o(43), ref_o3(43)
  Real(Kind=jprb) :: v_h2o_old(43)
  Real(Kind=jprb) :: v_o3_old(43)
  Real(Kind=jprb) :: q_gas(2)
  Real(Kind=jprb) :: v_gas(2,43)
  Real(Kind=jprb) :: p,t,t1
  !- End of header --------------------------------------------------------

  luin = 10
  open( unit=luin,             &
       & file='refprof_43.dat',&
       & status='old',         &
       & action='read',        &
       & iostat=ios)

  if(ios /= 0 )then
     write(*,*) 'error opening file refprof_43.dat   ios=', ios
     stop
  End if

  read(luin,*)
  read(luin,*)

  Do l=1,43
!     read(luin,"(19x,e12.6,10x,e12.6)")  ref_h2o(l), ref_o3(l)
     read(luin,*) il,p,t, ref_h2o(l),t1, ref_o3(l)
  End Do

  ! Converion specific concentration to ppmv with "old" formula
  !   specific concentration == mass mixing ratio
  ! constants extracted from RTTOV6/7 code
  Do l=1,43
     v_h2o_old(l) = ref_h2o(l) * 1.60771704e+6_JPRB
     v_o3_old(l)  = ref_o3(l)  * 6.03504e+5_JPRB
  End Do


  ! Converion specific concentration to ppmv with exact formula
  List_of_gases( 1 ) = gas_id_watervapour
  List_of_gases( 2 ) = gas_id_ozone

  ! calculate volume mixing ratio (ppmv) for the 2 gases
  Do l=1,43
     q_gas( 1 ) = ref_h2o(l)
     q_gas( 2 ) = ref_o3(l)
     Do n=1,2
        call rttov_q2v( gas_unit_specconc, ref_h2o(l),&
             & List_of_gases(n), q_gas(n), v_gas(n,l))
     End Do
  End Do

  ! Print differences between old and new formula
  write(*,*) ' Water Vapour '
  write(*,"(a3,3a12,a8)") 'lev','kg/kg','old ppmv','new ppmv',' %'
  Do l=1,43
     write(*,"(i3,3E12.5,F8.4)") l,&
          & ref_h2o(l), v_h2o_old(l), v_gas(1,l),&
          & 100._JPRB*(v_h2o_old(l) - v_gas(1,l)) / v_h2o_old(l)
  End Do

  write(*,*)
  write(*,*) ' Ozone '
  write(*,"(a3,3a12,a8)") 'lev','kg/kg','old ppmv','new ppmv',' %'
  Do l=1,43
     write(*,"(i3,3E12.5,F8.4)") l,&
          & ref_o3(l), v_o3_old(l), v_gas(2,l),&
          & 100._JPRB*(v_o3_old(l) - v_gas(2,l)) / v_o3_old(l)
  End Do


  ! Verify reverse calculation with rttov_v2q subroutine
  ! and compare to input value
  nerr = 0
  Do l=1,43
     Do n=1,2
        call rttov_v2q( gas_unit_specconc, ref_h2o(l),&
          & List_of_gases(n), v_gas(n,l), q_gas(n))
     End Do
     If( abs( q_gas( 1 ) - ref_h2o(l) ) >  ref_h2o(l)*1.e-09_JPRB ) then
        nerr = nerr + 1
        write(*,*) 'H2O conversion error for level',l
        write(*,"(3E20.9)")&
             & ref_h2o(l) ,&
             & v_gas(1,l),&
             & q_gas( 1 )
     End If
     If( abs( q_gas( 2 ) - ref_o3(l) ) >  ref_o3(l)*1.e-09_JPRB ) then
        nerr = nerr + 1
        write(*,*) 'O3 conversion error for level',l
        write(*,"(3E20.9)")&
             & ref_o3(l) ,&
             & v_gas(2,l),&
             & q_gas( 2 )
     End If
  End Do

  If( nerr == 0 ) then
     write(*,*)
     write(*,*) 'Reverse calculation test successfull (precision 1.e-09)'
     write(*,*)
  End If

  Stop
End Program test_q2v
