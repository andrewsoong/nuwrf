#!/bin/sh
#PBS -S /bin/sh
#PBS -N ldt_postlis
#PBS -l select=1:ncpus=1:mpiprocs=1:model=bro
#PBS -l walltime=0:05:00
#PBS -W group_list=s0942
#PBS -j eo
#Optional e-mail notifications
##PBS -m abe
##PBS -M user@nasa.gov
#Optional queue designation
##PBS -q devel
#------------------------------------------------------------------------------
# NASA/GSFC, Computational and Information Science and Technology Office,
# Code 606
#------------------------------------------------------------------------------
#                                                                              
# SCRIPT:  run_ldt.pleiades.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA CISTO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample script for running LDT on the NASA ARC Pleiades supercomputer
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

# Move to work directory and make sure ldt.config is present
if [ -z "$WORKDIR" ] ; then
    echo "ERROR, WORKDIR is not defined!"
    exit 1
fi
cd $WORKDIR || exit 1
if [ ! -e ldt.config ] ; then
    if [ ! -e ldt.config.postlis ] ; then
        echo "ERROR, ldt.config not found!"
        exit 1
    fi
    ln -s ldt.config.postlis ldt.config || exit 1
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

# Get LDT executable
ln -fs $NUWRFDIR/ldt/make/LDT $WORKDIR/LDT || exit 1
if [ ! -e $WORKDIR/LDT ] ; then
    echo "ERROR, $WORKDIR/LDT does not exist!"
    exit 1
fi
cd $WORKDIR || exit 1


LOGDIR=ldt_postlis_logs
if [ -e "$LOGDIR" ] ; then
    rm -rf "$LOGDIR" || exit 1
fi
mkdir "$LOGDIR" || exit 1

# LDT is compiled with MPI but can only use single process for now.
# Different MPI launch commands for different implementations.              
if [ "$USING_SGI_MPT" -eq 1 ] ; then
    mpiexec -np 1 ./LDT ldt.config || exit 1 
else
    echo "ERROR, unknown MPI implementation, don't know how to launch!"
    exit 1
fi

mv ldtlog_postlis.0000 $LOGDIR || exit 1

# Clean up symbolic link
if [ -L ldt.config ] ; then
    rm ldt.config || exit 1
fi

# The end
exit 0

