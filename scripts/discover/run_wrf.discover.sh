#!/bin/sh
#SBATCH --job-name=wrf
#SBATCH --time=1:00:00
#SBATCH --account s0942
#SBATCH --output wrf.slurm.out
#Adjust node, core, and hardware constraints here
#SBATCH --ntasks=1 --constraint=hasw
##Substitute your e-mail here
##SBATCH --mail-user=user@nasa.gov
##SBATCH --mail-type=ALL
##Set quality of service, if needed.
##SBATCH --qos=debug
#------------------------------------------------------------------------------
# NASA/GSFC, Software Systems Support Office, Code 610.3
#------------------------------------------------------------------------------
#                                                                              
# SCRIPT:  run_wrf.discover.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample script for running wrf.exe on the NASA GSFC Discover supercomputer
# with SLURM.
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
    if [ ! -e namelist.input.wrf ] ; then
	echo "ERROR, namelist.input not found!"
	exit 1
    else
	ln -s namelist.input.wrf namelist.input || exit 1
    fi
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

    if [ ! -e $file ] ; then
        echo "ERROR, $file does not exist!" && exit 1
    fi

    ln -fs $NUWRFDIR/WRFV3/run/$file $WORKDIR/$file || exit 1
    if [ ! -e $WORKDIR/$file ] ; then
	echo "ERROR, $WORKDIR/$file does not exist!" && exit 1
    fi
done

# For running with 2014 Goddard radiation
rm $WORKDIR/GODDARDRAD_SSLUT
ln -fs $NUWRFDIR/WRFV3/GODDARDRAD_SSLUT $WORKDIR/GODDARDRAD_SSLUT || exit 1
if [ ! -e $WORKDIR/GODDARDRAD_SSLUT ] ; then
    echo "ERROR, $WORKDIR/GODDARDRAD_SSLUT does not exist!" && exit 1
fi

# For running WRF/LIS. 
# FIXME: Add logic to skip this if uncoupled WRF will be run.
rm $WORKDIR/LS_PARAMETERS
rm $WORKDIR/MET_FORCING
ln -fs $LISDIR/LS_PARAMETERS $WORKDIR/LS_PARAMETERS || exit 1
if [ ! -e $WORKDIR/LS_PARAMETERS ] ; then
    echo "ERROR, $WORKDIR/LS_PARAMETERS does not exist!" && exit 1
fi
ln -fs $LISDIR/MET_FORCING $WORKDIR/MET_FORCING || exit 1
if [ ! -e $WORKDIR/MET_FORCING ] ; then
    echo "ERROR, $WORKDIR/MET_FORCING does not exist!" && exit 1
fi

# Link the wrf.exe executable
ln -fs $NUWRFDIR/WRFV3/main/wrf.exe $WORKDIR/wrf.exe || exit 1
if [ ! -e $WORKDIR/wrf.exe ] ; then
    echo "ERROR, $WORKDIR/wrf.exe does not exist!" && exit 1
fi

# Run wrf.exe
cd $WORKDIR || exit 1

# Special MPI launch command for SGI MPT on Discover                        
if [ "$USING_SGI_MPT" -eq 1 ] ; then
    mpiexec_mpt -n $SLURM_NTASKS ./wrf.exe || exit 1
elif [[ "$USING_INTEL_MPI" -eq 1 || "$USING_OPENMPI" -eq 1 ]] ; then
    mpirun -np $SLURM_NTASKS ./wrf.exe || exit 1
else
    echo "ERROR, unknown MPI implementation, don't know how to launch!"
    exit 1
fi

# Move the RSL log files.
if [ -e $WORKDIR/wrf_logs ] ; then
    rm -rf $WORKDIR/wrf_logs || exit 1
fi
mkdir $WORKDIR/wrf_logs || exit 1
rsl_files=`ls rsl.*`
for file in $rsl_files ; do
    mv $file $WORKDIR/wrf_logs/$file || exit 1
done
lislog_files=`ls lislog.*`
for file in $lislog_files ; do
    mv $file $WORKDIR/wrf_logs/$file || exit 1
done

# Clean up symbolic link
if [ -L namelist.input ] ; then
    rm namelist.input || exit 1
fi

# The end
exit 0

