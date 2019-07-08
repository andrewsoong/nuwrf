#!/bin/sh
#SBATCH --job-name=ldt_prelis
#SBATCH --time=0:05:00
#SBATCH --account s0942
#SBATCH --output ldt_prelis.slurm.out
#Adjust node, core, and hardware constraints here
#SBATCH --ntasks=1 --constraint=hasw
#Substitute your e-mail here
##SBATCH --mail-user=user@nasa.gov
##SBATCH --mail-type=ALL
#Set quality of service, if needed.
##SBATCH --qos=debug
#------------------------------------------------------------------------------
# NASA/GSFC, Software Systems Support Office, Code 610.3
#------------------------------------------------------------------------------
#                                                                              
# SCRIPT:  run_ldt.discover.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample script for running LDT on the NASA GSFC Discover supercomputer with
# SLURM.
#
#------------------------------------------------------------------------------

# When a batch script is started, it starts in the user's home directory.
# Change to the directory where job was submitted.
if [ ! -z $SLURM_SUBMIT_DIR ] ; then
    cd $SLURM_SUBMIT_DIR || exit 1
fi

# Load config file for modules and paths
source ./config.discover.sh || exit 1

# Move to work directory and make sure ldt.config is present
if [ -z "$WORKDIR" ] ; then
    echo "ERROR, WORKDIR is not defined!"
    exit 1
fi
cd $WORKDIR || exit 1
if [ ! -e ldt.config ] ; then
    if [ ! -e ldt.config.prelis ] ; then
	echo "ERROR, ldt.config not found!"
	exit 1
    fi
    ln -s ldt.config.prelis ldt.config || exit 1
fi

# Create LIS directory links
if [ -z "$NUWRFDIR" ] ; then
    echo "ERROR, NUWRFDIR is not defined!"
    exit 1
fi

if [ -e $WORKDIR/LS_PARAMETERS ] ; then
    rm -f $WORKDIR/LS_PARAMETERS || exit 1
fi
ln -fs $LISDIR/LS_PARAMETERS $WORKDIR || exit 1
if [ ! -e $WORKDIR/LS_PARAMETERS ] ; then
    echo "ERROR, $WORKDIR/LS_PARAMETERS does not exist!"
    exit 1
fi

if [ -e $WORKDIR/metforcing_parms ] ; then
    rm -f $WORKDIR/metforcing_parms || exit 1
fi
ln -fs $LISDIR/LS_PARAMETERS/metforcing_parms $WORKDIR || exit 1
if [ ! -e $WORKDIR/metforcing_parms ] ; then
    echo "ERROR, $WORKDIR/metforcing_parms does not exist!"
    exit 1
fi

# Get LDT executable.
ln -fs $NUWRFDIR/ldt/make/LDT $WORKDIR/LDT || exit 1
if [ ! -e $WORKDIR/LDT ] ; then
    echo "ERROR, $WORKDIR/LDT does not exist!"
    exit 1
fi
cd $WORKDIR || exit 1

if [ -e ldt_logs_prelis ] ; then
    rm -rf ldt_logs_prelis || exit 1
fi
mkdir ldt_logs_prelis || exit 1

# Currently LDT only runs with 1 MPI process.
if [ "$USING_SGI_MPT" -eq 1 ] ; then
    mpiexec_mpt -n 1 ./LDT ldt.config || exit 1
elif [[ "$USING_INTEL_MPI" -eq 1 || "$USING_OPENMPI" -eq 1 ]] ; then
    mpirun -np 1 ./LDT ldt.config || exit 1
else
    echo "ERROR, unknown MPI implementation, don't know how to launch!"
    exit 1
fi

mv ldtlog_prelis.0000 ldt_logs_prelis || exit 1

# Clean up symbolic link
if [ -L ldt.config ] ; then
    rm ldt.config || exit 1
fi

# The end
exit 0


