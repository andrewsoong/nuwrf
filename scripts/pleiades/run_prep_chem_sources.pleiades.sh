#!/bin/sh
#PBS -S /bin/sh
#PBS -N pcs
#PBS -l select=1:ncpus=1:model=bro
#PBS -l walltime=0:30:00
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
# SCRIPT:  run_prep_chem_sources.pleiades.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample batch script for running prep_chem_sources_RADM_WRF_FIM.exe on NASA 
# ARC Pleiades supercomputer with PBS.
#
#------------------------------------------------------------------------------

# When a batch script is started, it starts in the user's home directory.
# Change to the directory where job was submitted.
if [ ! -z $PBS_O_WORKDIR ] ; then
    cd $PBS_O_WORKDIR || exit 1
fi

# Load config file for modules and paths.
source ./config.pleiades.sh || exit 1

# Go to work directory and make sure prep_chem_sources.in is present.
if [ -z "$WORKDIR" ] ; then
    echo "ERROR, WORKDIR is not defined!"
    exit 1
fi
cd $WORKDIR || exit 1
if [ ! -e prep_chem_sources.inp ] ; then
    echo "ERROR, prep_chem_sources.inp not found!"
    exit 1
fi

# Run prep_chem_sources_RADM_WRF_FIM_SIMPLE.exe.  No MPI is used since the 
# program is serial.
if [ -z "$NUWRFDIR" ] ; then
    echo "ERROR, NUWRFDIR is not defined!"
    exit 1
fi
ln -fs \
  $NUWRFDIR/utils/prep_chem_sources/bin/prep_chem_sources_RADM_WRF_FIM_SIMPLE.exe || exit 1
if [ ! -e prep_chem_sources_RADM_WRF_FIM_SIMPLE.exe ] ; then
    echo "ERROR, prep_chem_sources_RADM_WRF_FIM_SIMPLE.exe does not exist!"
    exit 1
fi
./prep_chem_sources_RADM_WRF_FIM_SIMPLE.exe || exit 1

# The end
exit 0
