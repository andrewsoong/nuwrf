#!/bin/sh
#SBATCH --job-name=pcs
#SBATCH --time=0:10:00
#SBATCH --account s0942
#SBATCH --output pcs.slurm.out
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
# SCRIPT:  run_prep_chem_sources.discover.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample batch script for running prep_chem_sources_RADM_WRF_FIM.exe on NASA 
# GSFC Discover supercomputer with SLURM.
#
#------------------------------------------------------------------------------

# When a batch script is started, it starts in the user's home directory.
# Change to the directory where job was submitted.
if [ ! -z $SLURM_SUBMIT_DIR ] ; then
    cd $SLURM_SUBMIT_DIR || exit 1
fi

# Load config file for modules and paths.
source ./config.discover.sh || exit 1

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
  $NUWRFDIR/utils/prep_chem_sources/bin/prep_chem_sources_RADM_WRF_FIM_SIMPLE.exe \
  || exit 1
if [ ! -e prep_chem_sources_RADM_WRF_FIM_SIMPLE.exe ] ; then
    echo "ERROR, prep_chem_sources_RADM_WRF_FIM_SIMPLE.exe does not exist!"
    exit 1
fi
./prep_chem_sources_RADM_WRF_FIM_SIMPLE.exe || exit 1

# The end
exit 0
