#!/bin/sh
#SBATCH --job-name=avg_tsfc
#SBATCH --time=0:10:00
#SBATCH --account s0942
#SBATCH --output avg_tsfc.slurm.out
#Adjust node, core, and hardware constraints here.
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
# SCRIPT:  run_avg_tsfc.discover.sh
#
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample batch script for running avg_tsfc.exe on NASA GSFC Discover 
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
    echo "ERROR, WORKDIR is not defined!"
    exit 1
fi
cd $WORKDIR || exit 1
if [ ! -e namelist.wps ] ; then
    echo "ERROR, namelist.wps not found!"
    exit 1
fi


if [ -z "$NUWRFDIR" ] ; then
    echo "ERROR, NUWRFDIR is not defined!"
    exit 1
fi

# Run avg_tsfc.exe.  No MPI is used since the program is serial.
ln -fs $NUWRFDIR/WPS/util/src/avg_tsfc.exe avg_tsfc.exe || exit 1
if [ ! -e avg_tsfc.exe ] ; then
    echo "ERROR, avg_tsfc.exe does not exist!"
    exit 1
fi

./avg_tsfc.exe >& avg_tsfc_data.log || exit 1

# Move log files
if [ -e $WORKDIR/avg_tsfc_logs ] ; then
    rm -rf $WORKDIR/avg_tsfc_logs || exit 1
fi
mkdir $WORKDIR/avg_tsfc_logs || exit 1
rsl_files=`ls avg_tsfc*.log logfile.log`
for file in $rsl_files ; do
    mv $file $WORKDIR/avg_tsfc_logs/$file || exit 1
done

# The end
exit 0
