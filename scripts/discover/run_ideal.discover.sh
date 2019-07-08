#!/bin/sh
#SBATCH --job-name=ideal
#SBATCH --time=0:02:00
#SBATCH --account s0942
#SBATCH --output ideal.slurm.out
#Adjust node, core, and hardware constraints here
#SBATCH --ntasks=1 --constraint=hasw
##Substitute your e-mail here
##SBATCH --mail-user=user@nasa.gov
##SBATCH --mail-type=ALL
##Set quality of service, if needed
##SBATCH --qos=debug
#------------------------------------------------------------------------------
# NASA/GSFC, Software Systems Support Office, Code 610.3
#------------------------------------------------------------------------------
#                                                                              
# SCRIPT:  run_ideal.discover.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp and Carlos Cruz, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample script for running ideal.exe on the NASA GSFC Discover 
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
    echo "ERROR, WORKDIR is not defined!" && exit 1
fi
cd $WORKDIR || exit 1
if [ ! -e namelist.input ] ; then
    echo "ERROR, namelist.input not found!" && exit 1
fi

# Copy the various wrf lookup files into the work directory.
if [ -z "$NUWRFDIR" ] ; then
    echo "ERROR, NUWRFDIR is not defined!" && exit 1
fi
cd $NUWRFDIR/WRFV3/run || exit 1
files=`ls`
for file in $files ; do

    [[ $file == *.exe ]] && continue
    [[ $file == namelist* ]] && continue
    [[ $file == README* ]] && continue
    [[ $file == input* ]] && continue

    ln -fs $NUWRFDIR/WRFV3/run/$file $WORKDIR/$file || exit 1
    if [ ! -e $WORKDIR/$file ] ; then
	echo "ERROR, $file does not exist!" && exit 1
    fi
done

# Run ideal.exe
ln -fs $NUWRFDIR/WRFV3/main/ideal.exe $WORKDIR/ideal.exe || exit 1
if [ ! -e $WORKDIR/ideal.exe ] ; then
    echo "ERROR, $WORKDIR/ideal.exe does not exist!" && exit 1
fi
cd $WORKDIR || exit 1

# Special MPI launch command for SGI MPT on Discover                        
if [ "$USING_SGI_MPT" -eq 1 ] ; then
    mpiexec_mpt -n $SLURM_NTASKS ./ideal.exe || exit 1
elif [[ "$USING_INTEL_MPI" -eq 1 || "$USING_OPENMPI" -eq 1 ]] ; then
    mpirun -np $SLURM_NTASKS ./ideal.exe || exit 1
else
    echo "ERROR, unknown MPI implementation, don't know how to launch!" 
    exit 1
fi

# Move the RSL log files.
if [ -e $WORKDIR/ideal_logs ] ; then
    rm -rf $WORKDIR/ideal_logs || exit 1
fi
mkdir $WORKDIR/ideal_logs || exit 1
rsl_files=`ls rsl.*`
for file in $rsl_files ; do
    mv $file $WORKDIR/ideal_logs/$file || exit 1
done

# The end
exit 0
