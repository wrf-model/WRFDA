subroutine da_local_to_global(xp, x, xg, dim)

   !---------------------------------------------------------------------------
   ! PURPOSE: Make xg global-grid array by gathering the subdomain arrays x from
   !          each processor. The global array xg is stored only on the monitor
   !          processor (typically, processor 0).
   !---------------------------------------------------------------------------

   type (XPOSE_type), intent(in):: xp                                    ! Domain decomposition vars.
   real,  intent(in)   :: x (xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme)  ! Local-grid input array.
   real,  intent(out)  :: xg(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)  ! Global-grid output array.
   integer, intent(in) :: dim                         ! Number of dimensions (=2 for 2D, =3 for 3D).

   integer             :: i, j, k

   if (dim == 2) then
      do j=xp%jds, xp%jde
         do i=xp%ids, xp%ide
            xg(i,j,1) = x(i,j,1)
         end do
      end do
   else if (dim == 3) then
      do j=xp%jds, xp%jde
         do k=xp%kds, xp%kde
            do i=xp%ids, xp%ide
               xg(i,j,k) = x(i,j,k)
            end do
         end do
      end do
   else
      write(unit=message(1),fmt='(A,I5,A)') &
         "dim=",dim,"must be 2 or 3"
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

end subroutine da_local_to_global

subroutine da_proc_sum_count_obs ( count_obs )

   !---------------------------------------------------------------------------
   ! PURPOSE: Do MPI sum operation across processors to get the global sum of
   !          count_obs. The sum is returned only on the root processor,
   !          i.e., processor 0. (In this way, we do not have to do all-to-all 
   !          communication.)
   !
   ! IMPORTANT!!!
   !          The length of sumval and procval arrays are currently 56. This is
   !          the number of obs types (currently 14) times 4. This length should
   !          be changed accordingly if more observation types are added.
   !---------------------------------------------------------------------------

   type (count_obs_type), intent(inout)     :: count_obs

end subroutine da_proc_sum_count_obs

subroutine da_proc_stats_combine( proc_ave, proc_err, proc_min, proc_max, &
                               nobs_min, nobs_max, klev_min, klev_max )

   !---------------------------------------------------------------------------
   !  PURPOSE: Do MPI reduction operations across processors to get the average, 
   !           rms error, minimum, and maximum values for an observation field.
   !           These are stored only on the root processor, i.e., processor 0.
   !           (In this way, we do not have to do all-to-all communication.)
   !
   !---------------------------------------------------------------------------

   real,      intent(inout)      :: proc_ave       ! Processor average.
   real,      intent(inout)      :: proc_err       ! Processor rms error.
   real,      intent(inout)      :: proc_min       ! Processor minumum.
   real,      intent(inout)      :: proc_max       ! Processor maximum.
   integer,   intent(inout)      :: nobs_min       ! Obs number of minimum.
   integer,   intent(inout)      :: nobs_max       ! Obs number of maximum.
   integer,   intent(inout)      :: klev_min       ! Level of minimum.
   integer,   intent(inout)      :: klev_max       ! Level of maximum.

end subroutine da_proc_stats_combine

subroutine da_proc_maxmin_combine( n, max, min )

   !---------------------------------------------------------------------------
   !  PURPOSE: Do MPI reduction operations across processors to get the minimum
   !           and maximum values for an observation field of length n. The
   !           i, j location of the minimum and maximum, for each n, is also
   !           communicated.
   !           The return values are stored only on the root processor, i.e., 
   !           processor 0.  (In this way, we do not have to do all-to-all 
   !           communication.)
   !
   !---------------------------------------------------------------------------

   integer,   intent(in)                    :: n       ! Length of input fields.
   type (maxmin_field_type), intent(inout)  :: max(n)  ! Max values over proc.
   type (maxmin_field_type), intent(inout)  :: min(n)  ! Min values over proc.

end subroutine da_proc_maxmin_combine

subroutine da_proc_diagnostics_collect(num_sound, iv, re )

   !---------------------------------------------------------------------------
   !  PURPOSE: Do MPI reduction operations across processors to get values needed
   !           for diagnostics output. These are stored only on the root processor, 
   !           i.e., processor 0. (In this way, we do not have to do all-to-all 
   !           communication.)
   !
   !---------------------------------------------------------------------------

   integer, intent(in)                       :: num_sound
   type (sound_type), intent(inout)          :: iv(:)
   type (residual_sound_type), intent(inout) :: re(:)

end subroutine da_proc_diagnostics_collect

