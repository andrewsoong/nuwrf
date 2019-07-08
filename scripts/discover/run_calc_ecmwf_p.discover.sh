#!/bin/sh
#SBATCH --job-name=calc_ecmwf_p
#SBATCH --time=0:10:00
#SBATCH --account s0942
#SBATCH --output calc_ecmwf_p.slurm.out
#Adjust node, core, and hardware constraints here.
#SBATCH --ntasks=1 --constraint=hasw
#Substitute your e-mail here
##SBATCH --mail-user=user@nasa.gov
##SBATCH --mail-type=ALL
#Set quality of service, if needed.
##SBATCH --qos=debug
#------------------------------------------------------------------------------
# NASA/GSFC, Computational and Information Sciences and Technology Office,
# Code 606
#------------------------------------------------------------------------------
#                                                                              
# SCRIPT:  run_calc_ecmwf_p.discover.sh
#
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample batch script for running calc_ecmwf_p.exe on NASA GSFC Discover 
# supercomputer with SLURM.
#
#------------------------------------------------------------------------------

# When a batch script is started, it starts in the user's home directory.
# Change to the directory where job was submitted.
if [ ! -z $SLURM_SUBMIT_DIR ] ; then
    cd $SLURM_SUBMIT_DIR || exit 1
fi

# Load config file for modules and paths.
source ./config.discover.sh || exit 1

# Go to work directory and make sure namelist.wps is present.
if [ -z "$WORKDIR" ] ; then
    echo "FATAL, WORKDIR is not defined!"
    exit 1
fi
cd $WORKDIR || exit 1
if [ ! -e namelist.wps ] ; then
    echo "FATAL, namelist.wps not found!"
    exit 1
fi

# Make sure ecmwf_coeffs is present.
# NOTE: User may need to change source ecmwf_coeffs name depending on their
# data source.
ln -s ecmwf_coeffs_L91 ecmwf_coeffs
if [ ! -e ecmwf_coeffs ] ; then
    echo "FATAL, ecmwf_coeffs does not exist!"
    exit 1
fi

# Run calc_ecmwf_p.exe.  No MPI is used since the program is serial.
ln -fs $NUWRFDIR/WPS/util/calc_ecmwf_p.exe calc_ecmwf_p.exe || exit 1
if [ ! -e calc_ecmwf_p.exe ] ; then
    echo "FATAL, calc_ecmwf_p.exe does not exist!"
    exit 1
fi

./calc_ecmwf_p.exe >& calc_ecmwf_p.log || exit 1

# Move log files
if [ -e $WORKDIR/calc_ecmwf_p_logs ] ; then
    rm -rf $WORKDIR/calc_ecmwf_p_logs || exit 1
fi
mkdir $WORKDIR/calc_ecmwf_p_logs || exit 1
mv calc_ecmwf_p.log $WORKDIR/calc_ecmwf_p_logs || exit 1

# The end
exit 0
