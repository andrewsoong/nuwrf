#!/bin/sh
#PBS -S /bin/sh
#PBS -N metgrid
#PBS -l select=1:ncpus=28:mpiprocs=28:model=bro
#PBS -l walltime=0:15:00
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
# SCRIPT:  run_metgrid.pleiades.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample script for running metgrid.exe on the NASA ARC Pleiades
# supercomputer with PBS.
#
#------------------------------------------------------------------------------

# When a batch script is started, it starts in the user's home directory.
# Change to the directory where job was submitted.
if [ ! -z $PBS_O_WORKDIR ] ; then
    cd $PBS_O_WORKDIR || exit 1
fi

# Load config file for modules and paths
source ./config.pleiades.sh || exit 1

# Move to work directory and make sure namelist.input is present.
if [ -z "$WORKDIR" ] ; then
    echo "ERROR, WORKDIR is not defined!"
    exit 1
fi
cd $WORKDIR || exit 1
if [ ! -e namelist.wps ] ; then
    echo "ERROR, namelist.wps not found!"
    exit 1
fi

# Put metgrid TBL look-up file into metgrid subdirectory.
if [ -z "$NUWRFDIR" ] ; then
    echo "ERROR, NUWRFDIR is not defined!"
    exit 1
fi
if [ -e metgrid ] ; then
    rm -rf metgrid || exit 1
fi
mkdir metgrid || exit 1
ln -fs $NUWRFDIR/WPS/metgrid/METGRID.TBL.ARW metgrid/METGRID.TBL || exit 1
if [ ! -e "metgrid/METGRID.TBL" ] ; then
    echo "ERROR, metgrid/METGRID.TBL does not exist!"
    exit 1
fi

# Run metgrid.exe
ln -fs $NUWRFDIR/WPS/metgrid/src/metgrid.exe metgrid.exe || exit 1
if [ ! -e "metgrid.exe" ] ; then
    echo "ERROR, metgrid.exe does not exist!"
    exit 1
fi

# Different MPI launch commands for different implementations.              
if [ "$USING_SGI_MPT" -eq 1 ] ; then
    mpiexec -np 28 ./metgrid.exe || exit 1 # SGI MPT
else
    echo "ERROR, unknown MPI implementation, don't know how to launch!"
    exit 1
fi

# Move the RSL out files.
if [ -e $WORKDIR/metgrid_logs ] ; then
    rm -rf $WORKDIR/metgrid_logs || exit 1
fi
mkdir $WORKDIR/metgrid_logs || exit 1
rsl_files=`ls metgrid.log.*`
for file in $rsl_files ; do
    mv $file $WORKDIR/metgrid_logs/$file || exit 1
done

# The end
exit 0

