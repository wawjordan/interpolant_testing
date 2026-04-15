#!./bin/bash
. clean_local.sh
gfortran $GFORTRAN_DEBUG_FLAGS interpolant_testing.F90 ${CONDA_PREFIX}/lib/libblas.so ${CONDA_PREFIX}/lib/liblapack.so