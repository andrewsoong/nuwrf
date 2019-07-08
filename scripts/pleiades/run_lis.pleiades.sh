#!/bin/sh
#PBS -S /bin/sh
#PBS -N lis
#PBS -l select=1:ncpus=28:mpiprocs=28:model=bro
#PBS -l walltime=1:00:00
#PBS -W group_list=s0942
#PBS -j eo
#Optional e-mail notifications
##PBS -m abe
##PBS -M user@nasa.gov
#Optional queue designation
##PBS -q devel
#------------------------------------------------------------------------------
# NASA/GSFC, Software Systems Support Office, Code 610.3
#------------------------------------------------------------------------------
#                                                                              
# SCRIPT:  run_lis.pleiades.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample script for running LIS on the NASA ARC Pleiades supercomputer with
# PBS.
#
#------------------------------------------------------------------------------

# When a batch script is started, it starts in the user's home directory.
# Change to the directory where job was submitted.
if [ ! -z $PBS_O_WORKDIR ] ; then
    cd $PBS_O_WORKDIR || exit 1
fi

# Load config file for modules and paths
source ./config.pleiades.sh || exit 1

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

# Different MPI launch commands for different implementations.              
if [ "$USING_SGI_MPT" -eq 1 ] ; then
    mpiexec -np 28 ./LIS || exit 1
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

