subroutine da_create_bins( ni, nj, nk, bin_type, num_bins, num_bins2d, &
                           bin, bin2d, &
                           binwidth_lat, binwidth_hgt, num_bins_hgt, latitude, height )

   implicit none

   integer, intent(in)      :: ni, nj, nk                 ! Dimensions read in.
   integer, intent(in)      :: bin_type                   ! Type of bin to average over.
   integer, intent(out)     :: num_bins                   ! Number of bins.
   integer, intent(out)     :: num_bins2d                 ! Number of bins for 2D fields.
   integer, intent(out)     :: bin(1:ni,1:nj,1:nk)        ! Bin assigned to each 3D point.
   integer, intent(out)     :: bin2d(1:ni,1:nj)           ! Bin assigned to each 2D point.

   real, intent(in),optional:: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real, intent(in),optional:: binwidth_hgt               ! Used if bin_type = 2 (m).
   integer, intent(in), optional :: num_bins_hgt          ! Used if bin_type = 2.
   real, intent(in),optional:: latitude(1:ni,1:nj)        ! Latitude (degrees).
   real, intent(in),optional:: height(1:ni,1:nj,1:nk)     ! Height (m).

   integer                  :: b, i, j, k                 ! Loop counters.
   integer                  :: count                      ! Counter
   integer                  :: num_bins_lat               ! Used if bin_type = 2.
   integer                  :: bin_lat                    ! Latitude bin.
   integer                  :: bin_hgt                    ! Height bin.

   real, allocatable        :: binstart_lat(:)            ! Used if bin_type = 2 (degrees).
   real, allocatable        :: binstart_hgt(:)            ! Used if bin_type = 2 (degrees).

   if ( bin_type == 0 ) then         ! No averaging in space

      num_bins = nk * nj * ni
      num_bins2d = nj * ni    ! Equals number of horizontal points.

      count = 1
      do k = 1, nk
         do j = 1, nj
            do i = 1, ni
               bin(i,j,k) = count
               count = count + 1
            end do
         end do
      end do
      bin2d(:,:) = bin(:,:,1)

   else if ( bin_type == 1 ) then    ! Average over x-direction.

      num_bins = nj * nk
      num_bins2d = nj

      count = 1
      do k = 1, nk
         do j = 1, nj
            bin(1:ni,j,k) = count
            count = count + 1
         end do
      end do
      bin2d(:,:) = bin(:,:,1)

   else if ( bin_type == 2 ) then    ! Global latitude/height bins:

!     Setup latitude bins:
      num_bins_lat = nint(180.0 / binwidth_lat)
      allocate( binstart_lat(1:num_bins_lat) )
      binstart_lat(:) = 0.0
      do b = 1, num_bins_lat ! Assume south to north (as in WRF).
         binstart_lat(b) = -90.0 + real(b-1) * binwidth_lat
         write(6,'(i4,f15.5)')b, binstart_lat(b)
      end do

!     Setup height bins:
      allocate( binstart_hgt(1:num_bins_hgt) )
      binstart_hgt(:) = 0.0
      do b = 1, num_bins_hgt
         binstart_hgt(b) = real(b-1) * binwidth_hgt
         write(6,'(i4,f15.5)')b, binstart_hgt(b)
      end do

      num_bins = num_bins_lat * num_bins_hgt
      num_bins2d = num_bins_lat

      write(6,'(i3)')num_bins_lat, num_bins_hgt, num_bins
stop

!     Select height bins:
      do k = 1, nk
         do j = 1, nj
            do i = 1, ni
               if ( height(i,j,k) < binstart_hgt(1) ) then 
                  bin_hgt = 1 ! In first bin.
               else if ( height(i,j,k) >= binstart_hgt(num_bins_hgt) ) then
                  bin_hgt = num_bins_hgt ! In final bin.
               else 
                  do b = 1, num_bins_hgt-1
                     if ( height(i,j,k) >= binstart_hgt(b) .and. &
                          height(i,j,k) <  binstart_hgt(b+1) ) then
                        bin_hgt = b
                        exit
                     end if
                  end do
               end if

!              Select latitude bin that point falls in:
               if ( k == 1 ) then
                  do b = 1, num_bins_lat-1
                     if ( latitude(i,j) >= binstart_lat(b) .and. &
                        latitude(i,j) < binstart_lat(b+1) ) then
                        bin_lat = b
                        exit
                     end if
                  end do
                  if ( latitude(i,j) >= binstart_lat(num_bins_lat) ) then ! In final bin.
                     bin_lat = num_bins_lat
                  end if
                  bin2d(i,j) = bin_lat
               end if
               bin(i,j,k) = bin_lat + num_bins_lat * ( bin_hgt - 1 )
            end do
         end do
      end do

      deallocate( binstart_lat )
      deallocate( binstart_hgt )

   end if

end subroutine da_create_bins
