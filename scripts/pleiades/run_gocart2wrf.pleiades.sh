#!/bin/sh
#PBS -S /bin/sh
#PBS -N go2w
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
# SCRIPT:  run_gocart2wrf.pleiades.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample batch script for running gocart2wrf on NASA ARC Pleiades
# supercomputer with PBS.
#
#------------------------------------------------------------------------------

# When a batch script is started, it starts in the user's home directory.
# Change to the directory where job was submitted.
if [ ! -z $PBS_O_WORKDIR ] ; then
    cd $PBS_O_WORKDIR || exit 1
fi

# Load config file for modules and paths.
source ./config.pleiades.sh || exit 1

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
