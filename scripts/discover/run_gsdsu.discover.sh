#!/bin/sh
#SBATCH --job-name=gsdsu
#SBATCH --time=00:15:00
#SBATCH --account s0942
#SBATCH --output gsdsu.slurm.out
#Adjust node, core, and hardware constraints here
#SBATCH --ntasks=64 --constraint=hasw
#Substitute your e-mail here
##SBATCH --mail-user=user@nasa.gov
##SBATCH --mail-type=ALL
#Set quality of service, if needed.
##SBATCH --qos=debug
#------------------------------------------------------------------------------
# NASA/GSFC, Computational and Information Science and Technology Office,
# Code 606
#------------------------------------------------------------------------------
#                                                                              
# SCRIPT:  run_gsdsu.discover.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA CISTO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample script for running GSDSU.x on the NASA GSFC Discover supercomputer
# with SLURM.
#
#------------------------------------------------------------------------------

# When a batch script is started, it starts in the user's home directory.
# Change to the directory where job was submitted.
if [ ! -z $SLURM_SUBMIT_DIR ] ; then
    cd $SLURM_SUBMIT_DIR || exit 1
fi

# Load config file for modules and paths
source ./config.discover.sh || exit 1

# Move to work directory and make sure Configure_SDSU.F is present.
if [ -z "$WORKDIR" ] ; then
    echo "ERROR, WORKDIR is not defined!"
    exit 1
fi
cd $WORKDIR || exit 1
if [ ! -e Configure_SDSU.F ] ; then
    echo "FATAL, Configure_SDSU.F not found!"
    exit 1
fi

# Link the GSDSU look up table directories.
rm $WORKDIR/SSLUT
ln -fs $NUWRFDIR/GSDSU/SSLUT $WORKDIR/SSLUT || exit 1
if [ ! -e $WORKDIR/SSLUT ] ; then
    echo "FATAL, $WORKDIR/SSLUT does not exist!"
    exit 1
fi
rm $WORKDIR/DATAFILES
ln -fs $NUWRFDIR/GSDSU/DATAFILES $WORKDIR/DATAFILES || exit 1
if [ ! -e $WORKDIR/DATAFILES ] ; then
    echo "FATAL, $WORKDIR/DATAFILES does not exist!"
    exit 1
fi

# Link the GSDSU.x executable
ln -fs $NUWRFDIR/GSDSU/SRC/GSDSU.x $WORKDIR/GSDSU.x || exit 1
if [ ! -e $WORKDIR/GSDSU.x ] ; then
    echo "FATAL, $WORKDIR/GSDSU.x does not exist!"
    exit 1
fi

# Run GSDSU.x
if [ "$USING_SGI_MPT" -eq 1 ] ; then
    mpiexec_mpt -n $SLURM_NTASKS ./GSDSU.x || exit 1
elif [[ "$USING_INTEL_MPI" -eq 1 || "$USING_OPENMPI" -eq 1 ]] ; then
    mpirun -np $SLURM_NTASKS ./GSDSU.x || exit 1
else
    echo "ERROR, unknown MPI implementation, don't know how to launch!"
    exit 1
fi

# TODO:  Clean up log

# The end
exit 0
