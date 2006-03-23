subroutine da_setup_cv( its, ite, jts, jte, be )

!------------------------------------------------------------------------------
!  PURPOSE: Calculate the size of the 1-dimensional control variable array.
!           
!------------------------------------------------------------------------------

    integer, intent(in)           :: its, ite ! Time dims.
    integer, intent(in)           :: jts, jte ! Time dims.
    type (be_type), intent(inout) :: be       ! background error.

    integer                       :: iy, jx   ! Local horizontal domain dimensions.
    integer                       :: ij       ! Product of horizontal dims.

!--------------------------------------------------------------
!   [1] Define standard control variable size:
!--------------------------------------------------------------

    if ( global ) then
       be % cv % size1c = ( be % v1 % max_wave+1 ) * ( be % v1 % max_wave+2 )/2
       be % cv % size2c = ( be % v2 % max_wave+1 ) * ( be % v2 % max_wave+2 )/2
       be % cv % size3c = ( be % v3 % max_wave+1 ) * ( be % v3 % max_wave+2 )/2
       be % cv % size4c = ( be % v4 % max_wave+1 ) * ( be % v4 % max_wave+2 )/2
       be % cv % size5c = ( be % v5 % max_wave+1 ) * ( be % v5 % max_wave+2 )/2

       be % cv % size1  = 2 * be % cv % size1c * be % v1 % mz
       be % cv % size2  = 2 * be % cv % size2c * be % v2 % mz
       be % cv % size3  = 2 * be % cv % size3c * be % v3 % mz
       be % cv % size4  = 2 * be % cv % size4c * be % v4 % mz
       be % cv % size5  = 2 * be % cv % size5c * be % v5 % mz
    else
       iy = ite - its + 1
       jx = jte - jts + 1
       ij = iy * jx
       be % cv % size1 = ij * be % v1 % mz
       be % cv % size2 = ij * be % v2 % mz
       be % cv % size3 = ij * be % v3 % mz
       be % cv % size4 = ij * be % v4 % mz
       be % cv % size5 = ij * be % v5 % mz
    end if

    be % cv % size_jb = be % cv % size1 + be % cv % size2 + be % cv % size3 + be % cv % size4 + be % cv % size5

!--------------------------------------------------------------
!   [2] Define flow-dependent control variable size:
!--------------------------------------------------------------

    be % cv % size_je = 0
    if ( global ) then
       be % cv % size_alphac = ( be % alpha % max_wave + 1 ) * &
                               ( be % alpha % max_wave + 2 )  / 2
       be % cv % size_je  = 2 * be % cv % size_alphac * be % alpha % mz
    else
       be % cv % size_je = ij * be % alpha % mz
       write(6,'(a)')' flow-dependence not yet coded for regional!'
       stop
    end if

    be % cv % size = be % cv % size_jb +  be % cv % size_je

end subroutine da_setup_cv

