#!/bin/sh
#PBS -S /bin/sh
#PBS -N geogrid
#PBS -l select=1:ncpus=28:mpiprocs=28:model=bro
#PBS -l walltime=0:01:00
#PBS -W group_list=s0942
#PBS -j eo
#Optional e-mail notification
##PBS -m abe
##PBS -M user@nasa.gov
#Optional queue designation
##PBS -q devel
#------------------------------------------------------------------------------
# NASA/GSFC, Software Systems Support Office, Code 610.3
#------------------------------------------------------------------------------
#                                                                              
# SCRIPT:  run_geogrid.pleiades.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample script for running geogrid.exe on the NASA ARC Pleiades
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

# Put geogrid TBL look-up file into geogrid subdirectory
if [ -z "$NUWRFDIR" ] ; then
    echo "ERROR, NUWRFDIR is not defined!"
    exit 1
fi
if [ -e geogrid ] ; then
    rm -rf geogrid || exit 1
fi
mkdir geogrid || exit 1

# Typical case: No chemistry
ln -fs $NUWRFDIR/WPS/geogrid/GEOGRID.TBL.ARW geogrid/GEOGRID.TBL || exit 1

# Special case: Run with chemistry
#ln -fs $NUWRFDIR/WPS/geogrid/GEOGRID.TBL.ARW_CHEM geogrid/GEOGRID.TBL || exit 1

# Very special case: Run with chemistry and seasonal EROD (specific to NU-WRF)
#ln -fs $NUWRFDIR/WPS/geogrid/GEOGRID.TBL.ARW_CHEM_NUWRF \
#       geogrid/GEOGRID.TBL || exit 1 

if [ ! -e geogrid/GEOGRID.TBL ] ; then 
    echo "ERROR, geogrid/GEOGRID.TBL does not exist!"
    exit 1
fi

# Run geogrid.exe
ln -fs $NUWRFDIR/WPS/geogrid/src/geogrid.exe geogrid.exe || exit 1
if [ ! -e geogrid.exe ] ; then 
    echo "ERROR, geogrid.exe does not exist!"
    exit 1
fi

# Different MPI launch commands for different implementations.              
if [ "$USING_SGI_MPT" -eq 1 ] ; then
    mpiexec -np 28 ./geogrid.exe || exit 1
else
    echo "ERROR, unknown MPI implementation, don't know how to launch!"
    exit 1
fi

# Move RSL output files
if [ -e $WORKDIR/geogrid_logs ] ; then
    rm -rf $WORKDIR/geogrid_logs || exit 1
fi
mkdir $WORKDIR/geogrid_logs || exit 1
rsl_files=`ls geogrid.log.*`
for file in $rsl_files ; do
    mv $file $WORKDIR/geogrid_logs/$file || exit 1
done

# The end
exit 0

