#!/bin/sh
#SBATCH --job-name=go2w
#SBATCH --time=1:00:00
#SBATCH --account s0942
#SBATCH --output go2w.slurm.out
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
# SCRIPT:  run_gocart2wrf.discover.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample batch script for running gocart2wrf on NASA GSFC Discover 
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

# Go to work directory and make sure namelist.gocart2wrf is present.
if [ -z "$WORKDIR" ] ; then
    echo "ERROR, WORKDIR is not defined!"
    exit 1
fi
cd $WORKDIR || exit 1
if [ ! -e namelist.gocart2wrf ] ; then
    echo "ERROR, namelist.gocart2wrf not found!"
    exit 1
fi

# Make backup copies of wrfbdy and wrfinput files, as gocart2wrf will add
# new variables to them.
if [ ! -e wrfbdy_d01 ] ; then
    echo "ERROR, wrfbdy_d01 not found!"
    exit 1
fi
cp wrfbdy_d01 wrfbdy_d01.pre_gocart2wrf || exit 1
files=`ls wrfinput_d0[1-9]`
for file in $files ; do
    cp $file $file.pre_gocart2wrf || exit 1
done

# Run gocart2wrf.  No MPI is used since the program is serial.
ln -fs $NUWRFDIR/utils/gocart2wrf_2/bin/gocart2wrf gocart2wrf || exit 1
if [ ! -e gocart2wrf ] ; then
    echo "ERROR, gocart2wrf not found!"
    exit 1
fi
./gocart2wrf || exit 1

# The end
exit 0
