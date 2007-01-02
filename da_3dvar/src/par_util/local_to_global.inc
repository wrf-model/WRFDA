
SUBROUTINE local_to_global(xp, x, xg, dim)

!------------------------------------------------------------------------------
!  PURPOSE: Make xg global-grid array by gathering the subdomain arrays x from 
!           each processor. The global array xg is stored only on the monitor
!           processor (typically, processor 0). 
!------------------------------------------------------------------------------

    TYPE (XPOSE_TYPE), INTENT(IN):: xp                                    ! Domain decomposition vars.
    REAL,  INTENT(IN)   :: x (xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme)  ! Local-grid input array.
    REAL,  INTENT(OUT)  :: xg(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)  ! Global-grid output array.
    INTEGER, INTENT(IN) :: dim                         ! Number of dimensions (=2 for 2D, =3 for 3D). 

    integer                      :: glen(3) ! Global grid dimensions
    integer                      :: llen(3) ! Local grid dimensions

! Set global and local index extent arrays.
   glen(1) = xp%ide-xp%ids+1
   llen(1) = xp%ime-xp%ims+1
   glen(2) = xp%jde-xp%jds+1
   llen(2) = xp%jme-xp%jms+1
   if(dim .eq. 2) then
     glen(3) = 1
     llen(3) = 1
     call rsl_write(xg,IO2D_IJ_INTERNAL,x,0,TRUE_RSL_REAL,glen,llen)
   elseif(dim.eq.3) then
     glen(3) = xp%kde-xp%kds+1
     llen(3) = xp%kme-xp%kms+1
     call rsl_write(xg,IO3D_IJK_INTERNAL,x,0,TRUE_RSL_REAL,glen,llen)
   else
     write(6,*) 'SUBROUTINE local_to_global: dim = ',dim,&
                'Only legal value is 2 for 2D or 3 for 3D.'
     call wrf_shutdown
     stop
   end if

END SUBROUTINE local_to_global

