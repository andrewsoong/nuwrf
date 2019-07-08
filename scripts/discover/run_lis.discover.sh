#!/bin/sh
#SBATCH --job-name=lis
#SBATCH --time=24:00:00
#SBATCH --account s0942
#SBATCH --output lis.slurm.out
#Adjust node, core, and hardware constraints here
#SBATCH --ntasks=84 --constraint=hasw
#Substitute your e-mail here
##SBATCH --mail-user=user@nasa.gov
##SBATCH --mail-type=ALL
#Set quality of service, if needed.
##SBATCH --qos=debug
#------------------------------------------------------------------------------
# NASA/GSFC, Software Systems Support Office, Code 610.3
#------------------------------------------------------------------------------
#                                                                              
# SCRIPT:  run_lis.discover.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample script for running LIS on the NASA GSFC Discover supercomputer with
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

# Move to work directory and make sure lis.config is present.
if [ -z "$WORKDIR" ] ; then
    echo "ERROR, WORKDIR is not defined!"
    exit 1
fi
cd $WORKDIR || exit 1
if [ ! -e lis.config ] ; then
    echo "ERROR, lis.config not found!"
    exit 1
fi

# Create LIS directory links
if [ -z "$NUWRFDIR" ] ; then
    echo "ERROR, NUWRFDIR is not defined!"
    exit 1
fi

# FIXME:  These must currently be customized by hand.
rm $WORKDIR/LS_PARAMETERS
rm $WORKDIR/MET_FORCING
rm $WORKDIR/noah36_parms
ln -fs $LISDIR/LS_PARAMETERS $WORKDIR || exit 1
if [ ! -e $WORKDIR/LS_PARAMETERS ] ; then
    echo "ERROR, $WORKDIR/LS_PARAMETERS does not exist!"
    exit 1
fi
ln -fs $LISDIR/MET_FORCING $WORKDIR || exit 1
if [ ! -e $WORKDIR/MET_FORCING ] ; then
    echo "ERROR, $WORKDIR/MET_FORCING does not exist!"
    exit 1
fi
ln -fs $LISDIR/LS_PARAMETERS/noah36_parms noah36_parms || exit 1
if [ ! -e $WORKDIR/noah36_parms ] ; then
    echo "ERROR, $WORKDIR/noah36_parms does not exist!"
    exit 1
fi

# Get LIS executable.
ln -fs $NUWRFDIR/WRFV3/lis/make/LIS $WORKDIR/LIS || exit 1
if [ ! -e $WORKDIR/LIS ] ; then
    echo "ERROR, $WORKDIR/LIS does not exist!"
    exit 1
fi
cd $WORKDIR || exit 1

# Special MPI launch command for SGI MPT on Discover
if [ "$USING_SGI_MPT" -eq 1 ] ; then
    mpiexec_mpt -n $SLURM_NTASKS ./LIS || exit 1
elif [[ "$USING_INTEL_MPI" -eq 1 || "$USING_OPENMPI" -eq 1 ]] ; then
    mpirun -np $SLURM_NTASKS ./LIS || exit 1
else
    echo "ERROR, unknown MPI implementation, don't know how to launch!"
    exit 1
fi

# Move the LIS log files
if [ -e $WORKDIR/lis_logs ] ; then
    rm -rf $WORKDIR/lis_logs || exit 1
fi
mkdir $WORKDIR/lis_logs || exit 1
lis_files=`ls lislog.*`
for file in ${lis_files} ; do
    mv $file $WORKDIR/lis_logs/$file || exit 1
done

# The end
exit 0

