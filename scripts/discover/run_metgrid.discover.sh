#!/bin/sh
#SBATCH --job-name=metgrid
#SBATCH --time=0:15:00
#SBATCH --account s0942
#SBATCH --output metgrid.slurm.out
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
# SCRIPT:  run_metgrid.discover.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample script for running metgrid.exe on the NASA GSFC Discover 
# supercomputer with SLURM.
#
#------------------------------------------------------------------------------

# When a batch script is started, it starts in the user's home directory.
# Change to the directory where job was submitted.
if [ ! -z $SLURM_SUBMIT_DIR ] ; then
    cd $SLURM_SUBMIT_DIR || exit 1
fi

# Load config file for modules and paths
source ./config.discover.sh || exit 1

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

# Special MPI launch command for SGI MPT on Discover                        
if [ "$USING_SGI_MPT" -eq 1 ] ; then
    mpiexec_mpt -np $SLURM_NTASKS ./metgrid.exe || exit 1
elif [[ "$USING_INTEL_MPI" -eq 1 || "$USING_OPENMPI" -eq 1 ]] ; then
    mpirun -np $SLURM_NTASKS ./metgrid.exe || exit 1
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

