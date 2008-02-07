  subroutine da_varbc_precond (iv)

   !---------------------------------------------------------------------------
   !  PURPOSE:  Calculate covariance matrix b/w bias predictors 
   !            for VarBC preconditioning
   !
   !  Called from da_get_innov_vector_radiance
   !
   !  HISTORY: 11/07/2007 - Creation                     Tom Auligne
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent (in)      :: iv             ! Innovation

   type (stats_rad_type), pointer   :: rad
   integer                          :: inst, n, i, j, k, ii, jj, n1, n2
   integer                          :: npred, npredmax, num_rad
   real, allocatable 		    :: hessian(:,:), eignvec(:,:), eignval(:)
   real                             :: hessian_local, bgerr_local
   
   if (trace_use) call da_trace_entry("da_varbc_precond")
   
   if ( iv%num_inst < 1 ) RETURN
   
   write(unit=stdout,fmt='(A)') 'VARBC: Estimate Hessian for preconditioning'

   do inst = 1, iv%num_inst      ! loop for sensors
      npredmax = iv%instid(inst)%varbc_info%npredmax
      n1 = iv%instid(inst)%info%n1
      n2 = iv%instid(inst)%info%n2
 
      allocate ( hessian(npredmax, npredmax) )
      allocate ( eignvec(npredmax, npredmax) )
      allocate ( eignval(npredmax)           )
	 
      do k = 1, iv%instid(inst)%nchan         ! loop for channels
         npred = iv%instid(inst)%varbc(k)%npred
         if (npred   <= 0) cycle	      ! VarBC channel only     

         num_rad        = COUNT( (iv%instid(inst)%info%proc_domain(1,n1:n2)) &
	                    .AND.(iv%instid(inst)%tb_qc(k,n1:n2) > qc_varbc_bad) )
         iv%instid(inst)%varbc(k)%nobs  = wrf_dm_sum_integer(num_rad)
	    
         write(unit=stdout,fmt='(A,I6,3A,I5)') &
	    'VARBC:',iv%instid(inst)%varbc(k)%nobs,' active observations for ', &
            trim(adjustl(iv%instid(inst)%rttovid_string)),' channel',k
	    
         if (iv%instid(inst)%varbc(k)%nobs == 0) cycle
	 
        !---------------------------------------------------------	 
	! Calculate estimation of the Hessian for preconditioning
        !---------------------------------------------------------	 
         do i = 1, npred
	    ii = iv%instid(inst)%varbc(k)%ipred(i)

	   ! Observation term
           !------------------		 
            do j = i, npred
	       jj = iv%instid(inst)%varbc(k)%ipred(j)
	       hessian_local = 0.0	
               do n= n1,n2      ! loop for pixel      
                  if (iv%instid(inst)%tb_qc(k,n) <= qc_varbc_bad)   cycle  ! good obs only
	          if (.not. iv%instid(inst)%info%proc_domain(1,n))  cycle  ! do not sum up HALO data
		  
       	          hessian_local = hessian_local +  &
   	                          iv%instid(inst)%varbc_info%pred(ii,n) * &
	                          iv%instid(inst)%varbc_info%pred(jj,n) / &
	                          iv%instid(inst)%tb_error(k,n)**2				     
               end do                                               !  end loop for pixel
	       
	       ! Sum hessian preconditioning across processors
               hessian(i,j) = wrf_dm_sum_real(hessian_local)	
	       hessian(j,i) = hessian(i,j)
	    end do
         
	   ! Background term
           !-----------------
            if (iv%instid(inst)%varbc_info%nbgerr(ii) <= 0) cycle
	    bgerr_local = 0.0
	    do n= n1,n2	    
               if (iv%instid(inst)%tb_qc(k,n) <= qc_varbc_bad)   cycle  ! good obs only
               if (.not. iv%instid(inst)%info%proc_domain(1,n))  cycle  ! do not sum up HALO data

   	       bgerr_local = bgerr_local + iv%instid(inst)%tb_error(k,n)**2  / &
	                                   iv%instid(inst)%varbc_info%nbgerr(ii)
	    end do

            iv%instid(inst)%varbc(k)%bgerr(i) = wrf_dm_sum_real(bgerr_local) / &
	                                        iv%instid(inst)%varbc(k)%nobs

	    if (iv%instid(inst)%varbc(k)%bgerr(i) > 0) &          
   	       hessian(i,i) = hessian(i,i) + 1/iv%instid(inst)%varbc(k)%bgerr(i)
	 end do   

        !--------------------------------------------------	 
	! Preconditioning = inverse square root of Hessian
        !--------------------------------------------------	 	 
         hessian = hessian / varbc_factor**2
	 
	 if (npred == 1) then
	    iv%instid(inst)%varbc(k)%vtox(1,1)     = 1.0 / sqrt(hessian(1,1))
         else 
	    call da_eof_decomposition(npred, hessian(1:npred,1:npred), &
	    	            eignvec(1:npred,1:npred),eignval(1:npred))

	    if (ANY(eignval(1:npred) <= 0)) then		    
	       write(unit=stdout,fmt='(3A,I4,A,10F18.5)') &
	       'VARBC: non-positive Hessian for ', iv%instid(inst)%rttovid_string, &
	              ' ,channel ',k,'--> Eigenvalues =',eignval(1:npred) 
	       do i = 1, npred
	          if (hessian(i,i) > 0) &
	             iv%instid(inst)%varbc(k)%vtox(i,i) = 1.0 / sqrt(hessian(i,i))
	       end do
	    else
 	       do i = 1, npred
	          do j = i, npred
                     iv%instid(inst)%varbc(k)%vtox(i,j) = sum(          &
		                           eignvec(i,1:npred)         * &
			                   sqrt(1.0/eignval(1:npred)) * &
			                   eignvec(j,1:npred) )
		       
		     iv%instid(inst)%varbc(k)%vtox(j,i) = &
          	     iv%instid(inst)%varbc(k)%vtox(i,j)
                  end do
	       end do
	    end if
	 end if   
!         do i = 1, npred
!	    print*,inst,k,i,(iv%instid(inst)%varbc(k)%vtox(i,j),j=1,npred)
!	 end do
      end do                     !  end loop for channels	
      deallocate(hessian, eignvec, eignval)   
   end do                        !  end loop for sensor


   if (trace_use) call da_trace_exit("da_varbc_precond")

 end subroutine da_varbc_precond
