#!/bin/ksh
# Create community release of WRFVAR by removing elements we don't
# want to release, and remove subversion information that would allow
# them to be recreated.
#
# Remove CRTM
#
find . -name .svn -exec rm -rf {} \;
rm -rf da/da_radiance/*crtm*
cat > da/da_radiance/da_crtm.f90 <<EOF
! Stub, as CRTM model not included in community release
module da_crtm
end module da_crtm
EOF
rm -rf run/*hirs* run/*eos-2* run/*airs* run/*amsub*

