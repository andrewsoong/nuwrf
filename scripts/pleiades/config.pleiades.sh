#!/bin/sh
#------------------------------------------------------------------------------
# NASA/GSFC, Software Systems Support Office, Code 610.3
#------------------------------------------------------------------------------
#                                                                              
# SCRIPT:  config.pleiades.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample config file for running NU-WRF batch scripts on Pleiades.
#
#------------------------------------------------------------------------------

# We completely purge the module environment variables and LD_LIBRARY_PATH 
# before loading only those specific variables that we need.
source /usr/share/modules/init/sh 

module purge
unset LD_LIBRARY_PATH

module load gcc5/5.3.0 # Needed for Intel C++ compiler
module load comp-intel/2015.3.187
module load mpi-sgi/mpt # Use generic SGI MPT module per NAS recommendation
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib64

# Define locations of LIS, NUWRF, and the experiment work directory
LISDIR=/nobackupp8/nuwrf
NUWRFDIR=/nobackup/emkemp/NUWRF/svn/trunk
WORKDIR=/nobackup/emkemp/NUWRF/cases/12

# Set environment variables needed by RIP
export RIP_ROOT=$NUWRFDIR/RIP4
export NCARG_ROOT=/nobackupp8/nuwrf/lib/SLES11/nuwrflib-8r2/intel-2015.3.187/ncarg

# Make sure stacksize is unlimited
ulimit -S unlimited

# Check if using SGI MPT (special MPI launch command will be needed if true).
modules=$(module list 2>&1 >/dev/null)
USING_SGI_MPT=0
for entry in "$modules" ; do
    if echo "$entry" | grep -q "mpi-sgi" ; then
        USING_SGI_MPT=1
        break
    fi
done
export USING_SGI_MPT
