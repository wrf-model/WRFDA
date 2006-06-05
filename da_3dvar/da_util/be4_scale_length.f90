program scale_length

   use da_constants

   implicit none

   integer, parameter  :: nt=1000                    ! maximum time series
   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name
   character*3         :: be_method                  ! Be method ('NMC', or 'ENS')
   character*80        :: filename                   ! Input filename.
   character*1         :: k_label                    ! = 'k' if model level, 'm' if mode output.
   character*2         :: ck                         ! Level index -> character.
   character*3         :: ce                         ! Member index -> character.
   integer             :: ni, nj, nk                 ! Dimensions read in.
   integer             :: ni0, nj0                   ! Dimensions read in.
   integer             :: vertical_level
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: i, j, k, k1, k2, b, member ! Loop counters.

   real, allocatable   :: field(:,:,:)          ! Field projected into EOF space.

   real                      :: cut_dist_km,resolution_km

   namelist / scale_length_nl / start_date, end_date, interval, variable, &
                                be_method, ne, &
                                ni, nj, nk ,vertical_level, &
                                cut_dist_km, resolution_km
   logical             :: use_global_eofs, data_on_levels
   integer             :: num
!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!---------------------------------------------------------------------------------------------

   vertical_ip = 0

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   variable = 'psi'
   be_method = 'NMC'
   ne = 1

   use_global_eofs = .false.
   data_on_levels = .false.

   open(unit=namelist_unit, file='scale_length_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, scale_length_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing vertical error statistics for dates ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

   date = start_date
   cdate = sdate

   allocate(field(1:ni, 1:nj, 1:nt))

!----------------------------------------------------------------------------------------
   num=0
   do while ( cdate <= edate )
      do member = 1, ne
      num=num+1
         write(6,'(5a,i4)')'    Date = ', date, ', variable ', trim(variable), &
                           ' and member ', member

         write(ce,'(i3)')member
         if ( member < 10 ) ce = '00'//ce(3:3)
         if ( member >= 10 .and. member < 100 ) ce = '0'//ce(2:3)

!        Output fields (split into 2D files to allow parallel horizontal treatment):
!            read(ck,'(i2)') nk
            write(ck,'(i2)') nk

            if ( nk < 10 ) ck = '0'//ck(2:2)
            k_label = 'm'

!           Assumes variable directory has been created by script:
            filename = trim(variable)//'/'//date(1:10)//'.'//trim(variable)
!ols.101204            if (trim(variable).eq.'ps_u') then
!            filename = trim(filename)//'.'//trim(be_method)//'.e'//ce 
!            else
!            filename = trim(filename)//'.'//trim(be_method)//'.e'//ce//'.'//k_label//ck
!            endif
            filename = trim(filename)//'.'//trim(be_method)//'.e'//ce//'.'//ck

            open (iunit, file = filename, form='unformatted')
            read(iunit) ni0, nj0, k
            if (ni.ne.ni0.or.nj.ne.nj0) then
            write(*,'(a)') 'dimension problem',ni,ni0,nj,nj0
            stop
            endif
            read(iunit) data_on_levels, use_global_eofs
            read(iunit)field(1:ni,1:nj,num)
      end do ! End loop over members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do


     num=num-1

   call process_single_variable(field,variable,cut_dist_km,&
                      resolution_km,num,ni,nj,ck,nt)

   if (nk.eq.vertical_level) then
   call merge_scale_length(variable, nk)
   endif

end program scale_length
