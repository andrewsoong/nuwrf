#!/bin/sh
#SBATCH --job-name=ungrib
#SBATCH --time=0:10:00
#SBATCH --account s0942
#SBATCH --output ungrib.slurm.out
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
# SCRIPT:  run_ungrib.discover.sh
#
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample batch script for running ungrib.exe on NASA GSFC Discover 
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

# Make sure Vtable is present.
# NOTE:  User may need to change source Vtable name depending on their data
# source.
if [ -z "$NUWRFDIR" ] ; then
    echo "ERROR, NUWRFDIR is not defined!"
    exit 1
fi
if [ -e Vtable ] ; then
    rm -f Vtable || exit 1
fi
ln -fs $NUWRFDIR/WPS/ungrib/Variable_Tables/Vtable.GFS Vtable || exit 1
#ln -fs $NUWRFDIR/WPS/ungrib/Variable_Tables/Vtable.AWIP Vtable || exit 1
#ln -fs $NUWRFDIR/WPS/ungrib/Variable_Tables/Vtable.NAM Vtable || exit 1
#ln -fs $NUWRFDIR/WPS/ungrib/Variable_Tables/Vtable.NARR Vtable || exit 1
if [ ! -e Vtable ] ; then
    echo "ERROR, Vtable does not exist!"
    exit 1
fi

# Create GRIBFILE symbolic links to grib files.
ln -fs $NUWRFDIR/WPS/link_grib.csh link_grib.csh || exit 1
if [ ! -e link_grib.csh ] ; then
    echo "ERROR, link_grib.csh does not exist!"
    exit 1
fi
./link_grib.csh ungrib_input/* || exit 1

# Run ungrib.exe.  No MPI is used since the program is serial.
ln -fs $NUWRFDIR/WPS/ungrib/src/ungrib.exe ungrib.exe || exit 1
if [ ! -e ungrib.exe ] ; then
    echo "ERROR, ungrib.exe does not exist!"
    exit 1
fi

./ungrib.exe >& ungrib_data.log || exit 1

# Move log files
if [ -e $WORKDIR/ungrib_logs ] ; then
    rm -rf $WORKDIR/ungrib_logs || exit 1
fi
mkdir $WORKDIR/ungrib_logs || exit 1
rsl_files=`ls ungrib*.log`
for file in $rsl_files ; do
    mv $file $WORKDIR/ungrib_logs/$file || exit 1
done

# The end
exit 0
