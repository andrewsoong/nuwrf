#!/bin/sh
#PBS -S /bin/sh
#PBS -N gsdsu
#PBS -l select=4:ncpus=16:mpiprocs=16:model=has
#PBS -l walltime=0:15:00
#PBS -W group_list=s0942
#PBS -j eo
#Optional e-mail notification
##PBS -m abe
##PBS -M user@nasa.gov
#Optional queue designation
##PBS -q devel
#------------------------------------------------------------------------------
# NASA/GSFC, Computational and Information Science and Technology Office,
# Code 606
#------------------------------------------------------------------------------
#                                                                              
# SCRIPT:  run_gsdsu.pleiades.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA CISTO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample script for running GSDSU.x on the NASA ARC Pleiades supercomputer
# with PBS.
#
#------------------------------------------------------------------------------

# When a batch script is started, it starts in the user's home directory.
# Change to the directory where job was submitted.
if [ ! -z $PBS_O_WORKDIR ] ; then
    cd $PBS_O_WORKDIR || exit 1
fi

# Load config file for modules and paths
source ./config.pleiades.sh || exit 1

# Move to work directory and make sure Configure_SDSU.F is present.
if [ -z "$WORKDIR" ] ; then
    echo "FATAL, WORKDIR is not defined!"
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
    mpiexec -n 64 ./GSDSU.x || exit 1
else
    echo "FATAL, unknown MPI implementation, don't know how to launch!"
    exit 1
fi

# TODO:  Clean up log

# The end
exit 0
