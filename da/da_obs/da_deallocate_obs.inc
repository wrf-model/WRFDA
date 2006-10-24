SUBROUTINE da_deallocate_obs (ob)

   USE da_control

   implicit none

   TYPE (ob_type),    INTENT(inout) :: ob

   INTEGER :: i

   IF (trace_use) CALL da_trace_entry("da_deallocate_obs")

   IF (use_PilotObs) THEN
      DO i=1,ob%num_pilot
         DEALLOCATE(ob%pilot(i)%p)
         DEALLOCATE(ob%pilot(i)%zk)
         DEALLOCATE(ob%pilot(i)%u)
         DEALLOCATE(ob%pilot(i)%v)
      END DO
   ENDIF

   IF (use_SoundObs) THEN
      DO i=1,ob%num_sound
         deallocate (ob%sound(i)%h)
         deallocate (ob%sound(i)%p)
         deallocate (ob%sound(i)%zk)
         deallocate (ob%sound(i)%u)
         deallocate (ob%sound(i)%v)
         deallocate (ob%sound(i)%t)
         deallocate (ob%sound(i)%q)
      END DO
   END IF

   IF (use_SatemObs) THEN
      DO i=1,ob%num_satem
         deallocate (ob%satem(i)%p)
         deallocate (ob%satem(i)%thickness)
         deallocate (ob%satem(i)%org_thickness)
      END DO
   END IF

   IF (use_polaramvObs) THEN
      DO i=1,ob%num_polaramv
         deallocate (ob%polaramv(i)%p)
         deallocate (ob%polaramv(i)%zk)
         deallocate (ob%polaramv(i)%u)
         deallocate (ob%polaramv(i)%v)
      END DO
   END IF

   IF (use_geoamvObs) THEN
      DO i=1,ob%num_geoamv
         deallocate (ob%geoamv(i)%p)
         deallocate (ob%geoamv(i)%zk)
         deallocate (ob%geoamv(i)%u)
         deallocate (ob%geoamv(i)%v)
      END DO
   END IF

   IF (use_AirepObs) THEN
      DO i=1,ob%num_airep 
         deallocate (ob%airep(i)%h)
         deallocate (ob%airep(i)%p)
         deallocate (ob%airep(i)%zk)
         deallocate (ob%airep(i)%u)
         deallocate (ob%airep(i)%v)
         deallocate (ob%airep(i)%t)
      END DO
   END IF

   IF (use_GpsrefObs) THEN
      DO i=1,ob%num_gpsref
         deallocate (ob%gpsref(i)%h)
         deallocate (ob%gpsref(i)%zk)
         deallocate (ob%gpsref(i)%ref)
         deallocate (ob%gpsref(i)%p)
         deallocate (ob%gpsref(i)%t)
         deallocate (ob%gpsref(i)%q)
      END DO
   END IF

   IF (use_ssmt1obs) THEN
      DO i=1,ob%num_ssmt1
         deallocate (ob%ssmt1(i)%h)
         deallocate (ob%ssmt1(i)%p)
         deallocate (ob%ssmt1(i)%t)
         deallocate (ob%ssmt1(i)%zk)
      END DO
   END IF

   IF (use_ssmt2obs) THEN
      DO i=1,ob%num_ssmt2
         deallocate (ob%ssmt2(i)%h)
         deallocate (ob%ssmt2(i)%p)
         deallocate (ob%ssmt2(i)%zk)
         deallocate (ob%ssmt2(i)%rh)
      END DO
   END IF

   IF (use_ProfilerObs) THEN
      DO i=1,ob%num_profiler
         deallocate (ob%profiler(i)%p)
         deallocate (ob%profiler(i)%zk)
         deallocate (ob%profiler(i)%u)
         deallocate (ob%profiler(i)%v)
      END DO
   END IF

   IF (use_BogusObs) THEN
      DO i=1,ob%num_bogus
         deallocate (ob%bogus(i)%h)
         deallocate (ob%bogus(i)%p)
         deallocate (ob%bogus(i)%zk)
         deallocate (ob%bogus(i)%u)
         deallocate (ob%bogus(i)%v)
         deallocate (ob%bogus(i)%t)
         deallocate (ob%bogus(i)%q)
      END DO
   END IF
   IF (use_AIRSRETObs) THEN
      DO i=1,ob%num_airsr
         deallocate (ob%airsr(i)%h)
         deallocate (ob%airsr(i)%p)
         deallocate (ob%airsr(i)%zk)
         deallocate (ob%airsr(i)%t)
         deallocate (ob%airsr(i)%q)
      END DO
   END IF

   IF (trace_use) CALL da_trace_exit("da_deallocate_obs")

END SUBROUTINE da_deallocate_obs
