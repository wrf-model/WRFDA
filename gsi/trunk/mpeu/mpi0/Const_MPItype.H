#if defined(sysIRIX64) || defined(sysOSF1) || defined(sysLinux)
#define Const_MPItype_
#else
#ifdef Const_MPItype_
#undef Const_MPItype_
#endif
#endif
