subroutine da_local_to_global(xp, x, xg, dim)

!------------------------------------------------------------------------------
!  PURPOSE: Make xg global-grid array by gathering the subdomain arrays x from
!           each processor. The global array xg is stored only on the monitor
!           processor (typically, processor 0).
!------------------------------------------------------------------------------

   TYPE (XPOSE_TYPE), INTENT(IN):: xp                                    ! Domain decomposition vars.
   REAL,  INTENT(IN)   :: x (xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme)  ! Local-grid input array.
   REAL,  INTENT(OUT)  :: xg(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)  ! Global-grid output array.
   INTEGER, INTENT(IN) :: dim                         ! Number of dimensions (=2 for 2D, =3 for 3D).

   integer             :: i, j, k

   if (dim == 2) then
     do j=xp%jds, xp%jde
     do i=xp%ids, xp%ide
        xg(i,j,1) = x(i,j,1)
     enddo
     enddo
   else if (dim == 3) then
     do j=xp%jds, xp%jde
     do k=xp%kds, xp%kde
     do i=xp%ids, xp%ide
        xg(i,j,k) = x(i,j,k)
     enddo
     enddo
     enddo
   else
     WRITE(UNIT=errmsg(1),FMT='(A,I5,A)') &
       "dim=",dim,"must be 2 or 3"
     call da_error(__FILE__,__LINE__,errmsg(1))
   end if

end subroutine da_local_to_global

subroutine da_proc_sum_count_obs ( count_obs )

!------------------------------------------------------------------------------
!  PURPOSE: Do MPI sum operation across processors to get the global sum of
!           count_obs. The sum is returned only on the root processor,
!           i.e., processor 0. (In this way, we do not have to do all-to-all 
!           communication.)
!
!  IMPORTANT!!!
!           The length of sumval and procval arrays are currently 56. This is
!           the number of obs types (currently 14) times 4. This length should
!           be changed accordingly if more observation types are added.
!
!------------------------------------------------------------------------------

   TYPE (count_obs_type), INTENT(INOUT)     :: count_obs

   RETURN
end subroutine da_proc_sum_count_obs

subroutine da_proc_stats_combine( proc_ave, proc_err, proc_min, proc_max, &
                               nobs_min, nobs_max, klev_min, klev_max )

!------------------------------------------------------------------------------
!  PURPOSE: Do MPI reduction operations across processors to get the average, 
!           rms error, minimum, and maximum values for an observation field.
!           These are stored only on the root processor, i.e., processor 0.
!           (In this way, we do not have to do all-to-all communication.)
!
!------------------------------------------------------------------------------

   REAL,      INTENT(INOUT)      :: proc_ave       ! Processor average.
   REAL,      INTENT(INOUT)      :: proc_err       ! Processor rms error.
   REAL,      INTENT(INOUT)      :: proc_min       ! Processor minumum.
   REAL,      INTENT(INOUT)      :: proc_max       ! Processor maximum.
   INTEGER,   INTENT(INOUT)      :: nobs_min       ! Obs number of minimum.
   INTEGER,   INTENT(INOUT)      :: nobs_max       ! Obs number of maximum.
   INTEGER,   INTENT(INOUT)      :: klev_min       ! Level of minimum.
   INTEGER,   INTENT(INOUT)      :: klev_max       ! Level of maximum.

   RETURN
end subroutine da_proc_stats_combine

subroutine da_proc_maxmin_combine( n, max, min )

!------------------------------------------------------------------------------
!  PURPOSE: Do MPI reduction operations across processors to get the minimum
!           and maximum values for an observation field of length n. The
!           i, j location of the minimum and maximum, for each n, is also
!           communicated.
!           The return values are stored only on the root processor, i.e., 
!           processor 0.  (In this way, we do not have to do all-to-all 
!           communication.)
!
!------------------------------------------------------------------------------

   INTEGER,   INTENT(IN)                    :: n       ! Length of input fields.
   TYPE (maxmin_field_type), INTENT(INOUT)  :: max(n)  ! Max values over proc.
   TYPE (maxmin_field_type), INTENT(INOUT)  :: min(n)  ! Min values over proc.

   RETURN
end subroutine da_proc_maxmin_combine

subroutine da_proc_diagnostics_collect(num_sound, iv, re )

!------------------------------------------------------------------------------
!  PURPOSE: Do MPI reduction operations across processors to get values needed
!           for diagnostics output. These are stored only on the root processor, 
!           i.e., processor 0. (In this way, we do not have to do all-to-all 
!           communication.)
!
!------------------------------------------------------------------------------

   integer, intent(in)                       :: num_sound
   type (sound_type), intent(inout)          :: iv(:)
   type (residual_sound_type), intent(inout) :: re(:)

   RETURN
end subroutine da_proc_diagnostics_collect

