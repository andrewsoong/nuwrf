#!/bin/sh
#PBS -S /bin/sh
#PBS -N plotgrids
#PBS -l select=1:ncpus=1:model=bro
#PBS -l walltime=0:01:00
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
# SCRIPT:  run_plotgrids.pleiades.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample script for running plotgrids.exe on the NASA NAS Pleiades
# supercomputer with PBS.
#
#------------------------------------------------------------------------------

# Change to the directory where job was submitted.
if [ ! -z $PBS_O_WORKDIR ] ; then
    cd $PBS_O_WORKDIR || exit 1
fi

# Load config file for modules and paths
source ./config.pleiades.sh || exit 1

# Move to work directory and make sure namelist.wps is present.
if [ -z "$WORKDIR" ] ; then
    echo "ERROR, WORKDIR is not defined!"
    exit 1
fi
cd $WORKDIR || exit 1
if [ ! -e namelist.wps ] ; then
    echo "ERROR, namelist.wps not found!"
    exit 1
fi

# Run plotgrids.exe
if [ -z "$NUWRFDIR" ] ; then
    echo "ERROR, NUWRFDIR is not defined!"
    exit 1
fi
ln -fs $NUWRFDIR/WPS/util/src/plotgrids.exe plotgrids.exe || exit 1
if [ ! -e plotgrids.exe ] ; then 
    echo "ERROR, plotgrids.exe does not exist!"
    exit 1
fi
./plotgrids.exe || exit 1

# The end
exit 0

