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
     write(6,*) 'SUBROUTINE local_to_global: dim = ',dim,&
                'Only legal value is 2 for 2D or 3 for 3D.'
     call wrf_shutdown
     stop
   end if

END SUBROUTINE local_to_global

SUBROUTINE mm5_struct_bcast( x_mm5 )

   TYPE (mm5_model_type), INTENT(INOUT)   :: x_mm5  ! MM5 model structure.

   RETURN
END SUBROUTINE mm5_struct_bcast

SUBROUTINE proc_sum_int ( value )

!------------------------------------------------------------------------------
!  PURPOSE: Do MPI sum operation across processors to get the global sum of
!           an integer value. The sum is returned only on the root processor,
!           i.e., processor 0. (In this way, we do not have to do all-to-all 
!           communication.)
!
!------------------------------------------------------------------------------

   INTEGER,   INTENT(IN)      :: value     ! Value on processor.

   return

END SUBROUTINE proc_sum_int

SUBROUTINE proc_sum_real ( n, value )

!------------------------------------------------------------------------------
!  PURPOSE: Do MPI reduction operation across processors to sum a real vector.  
!           On return, each of the N components of the vector "value" represents
!           the sum of parts from each processor. The result is stored only on 
!           the root processor, i.e., processor 0. (In this way, we do not have 
!           to do all-to-all communication.)
!
!------------------------------------------------------------------------------

   INTEGER, INTENT(IN)         :: n        ! Length of value array. 
   REAL,    INTENT(INOUT)      :: value(n) ! Array to be summed componentwise.

   return

END SUBROUTINE proc_sum_real

SUBROUTINE proc_sum_count_obs ( count_obs )

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
END SUBROUTINE proc_sum_count_obs

SUBROUTINE proc_stats_combine( proc_ave, proc_err, proc_min, proc_max, &
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
END SUBROUTINE proc_stats_combine

SUBROUTINE proc_maxmin_combine( n, max, min )

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
END SUBROUTINE proc_maxmin_combine

SUBROUTINE proc_diagnostics_collect(num_sound, iv, re )

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
END SUBROUTINE proc_diagnostics_collect

