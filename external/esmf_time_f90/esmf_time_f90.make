ESMF_TIME_F90_OBJS = ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        Meat.o ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o   \
        ESMF_TimeInterval.o ESMF_Stubs.o ESMF_Mod.o \
        module_symbols_util.o \
	module_utility.o ESMF_AlarmClock.o

libesmf_time.a : $(ESMF_TIME_F90_OBJS)
	$(AR) libesmf_time.a $(ESMF_TIME_F90_OBJS)
	$(RANLIB) libesmf_time.a

Test1 : Test1.F90 libesmf_time.a
	$(FC) -c -g Test1.F90
	$(FC) -o Test1 Test1.o libesmf_time.a

# DEPENDENCIES : only dependencies after this line 

ESMF_Alarm.o : ESMF_BaseTime.o ESMF_Time.o ESMF_TimeInterval.o
ESMF_BaseTime.o : ESMF_Base.o
ESMF_Clock.o : ESMF_BaseTime.o ESMF_Time.o ESMF_TimeInterval.o ESMF_Alarm.o
ESMF_AlarmClock.o : ESMF_Alarm.o ESMF_Clock.o
ESMF_Time.o : ESMF_BaseTime.o ESMF_TimeInterval.o
ESMF_Base.o :
ESMF_Calendar.o : ESMF_BaseTime.o
ESMF_Fraction.o : ESMF_BaseTime.o
ESMF_TimeInterval.o : ESMF_BaseTime.o ESMF_Calendar.o ESMF_Fraction.o
ESMF_Mod.o : ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o    \
        ESMF_TimeInterval.o Meat.o ESMF_Stubs.o ESMF_AlarmClock.o
Meat.o : ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o    \
        ESMF_TimeInterval.o
ESMF_Stubs.o : ESMF_Base.o ESMF_Calendar.o \
        ESMF_Alarm.o ESMF_Clock.o ESMF_TimeInterval.o ESMF_Time.o
module_utility.o :  ESMF_Mod.o module_symbols_util.o
module_symbols_util.o :  ESMF_Mod.o

