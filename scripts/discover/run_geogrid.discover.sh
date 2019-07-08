#!/bin/sh
#SBATCH --job-name=geogrid
#SBATCH --time=0:10:00
#SBATCH --account s0942
#SBATCH --output geogrid.slurm.out
#Adjust node, core, and hardware constraints here.
#SBATCH --ntasks=16 --constraint=hasw
#Substitute your e-mail here
##SBATCH --mail-user=user@nasa.gov
##SBATCH --mail-type=ALL
#Set quality of service, if needed.
##SBATCH --qos=debug
#------------------------------------------------------------------------------
# NASA/GSFC, Software Systems Support Office, Code 610.3
#------------------------------------------------------------------------------
#                                                                              
# SCRIPT:  run_geogrid.discover.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample script for running geogrid.exe on the NASA GSFC Discover 
# supercomputer with SLURM.
#
#------------------------------------------------------------------------------

# Change to the directory where job was submitted.
if [ ! -z $SLURM_SUBMIT_DIR ] ; then
    cd $SLURM_SUBMIT_DIR || exit 1
fi

# Load config file for modules and paths
source ./config.discover.sh || exit 1

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

# Special MPI launch command for SGI MPT on Discover
if [ "$USING_SGI_MPT" -eq 1 ] ; then
    mpiexec_mpt -n $SLURM_NTASKS ./geogrid.exe || exit 1
elif [[ "$USING_INTEL_MPI" -eq 1 || "$USING_OPENMPI" -eq 1 ]] ; then
    mpirun -np $SLURM_NTASKS ./geogrid.exe || exit 1
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

