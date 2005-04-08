SUBROUTINE DA_GET_SPoles(x,              &
          ids, ide, jds, jde, kds, kde,  &
          ims, ime, jms, jme, kms, kme,  &
          its, ite, jts, jte, kts, kte   )



!------------------------------------------------------------------------------
!  PURPOSE: Treatment for Scalar field at Poles                  
!
!  HISTORY:   
!          Creation: 10/26/2004	                 Syed RH Rizvi
!
!  PARENT_MODULE: DA_Tools              
!------------------------------------------------------------------------------
   IMPLICIT NONE
   

   integer, intent(in)    :: ids, ide, jds, jde, kds, kde
   integer, intent(in)    :: ims, ime, jms, jme, kms, kme
   integer, intent(in)    :: its, ite, jts, jte, kts, kte
   real, intent(inout)    :: x(ims:ime,jms:jme,kms:kme)   
!---------------------------------------------------------------------------
   integer                :: ierror, k
   real                   :: tmpvar,tmps,tmp_s
!---------------------------------------------------------------------------
   tmpvar      = 1.0/real(ide-ids+1)
!
 do k = kts, kte
   tmps =0.0  ; tmp_s =0.0
   if( jts == jds)  tmp_s = tmpvar*sum(x(its:ite,jts+1,k))

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE(tmp_s,tmps,1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, IERROR )
#else
   tmps = tmp_s                    
#endif
   if( jts == jds) x(its:ite,jts,k) = tmps
!
   tmps =0.0  ; tmp_s =0.0
   if( jte == jde) tmp_s = tmpvar*sum(x(its:ite,jte-1,k))

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE(tmp_s,tmps,1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, IERROR )
#else
   tmps = tmp_s                    
#endif
   if( jte == jde) x(its:ite,jte,k) = tmps
 end do
END SUBROUTINE DA_GET_SPoles

