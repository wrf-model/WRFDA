MODULE da_h_ops

USE da_c_mat

IMPLICIT NONE

CONTAINS

#include <a2b.inc>
#include <a2c.inc>
#include <b2a.inc>
#include <c2a.inc>
#include <graph.inc>
#include <hops2.inc>
!include <hops1i.inc>
!include <hops2i.inc>
!include <hops2j.inc>
!include <hops3i.inc>
!include <hops3j.inc>

END MODULE da_h_ops

