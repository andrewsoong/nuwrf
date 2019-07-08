#!/bin/sh
#SBATCH --job-name=real
#SBATCH --time=0:20:00
#SBATCH --account s0942
#SBATCH --output real.slurm.out
#Adjust node, core, and hardware constraints here
#SBATCH --ntasks=28 --constraint=hasw
#Substitute your e-mail here
##SBATCH --mail-user=user@nasa.gov
##SBATCH --mail-type=ALL
#Set quality of service, if needed
##SBATCH --qos=debug
#------------------------------------------------------------------------------
# NASA/GSFC, Software Systems Support Office, Code 610.3
#------------------------------------------------------------------------------
#                                                                              
# SCRIPT:  run_real.discover.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample script for running real.exe on the NASA GSFC Discover 
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
if [ ! -e namelist.input ] ; then
    if [ ! -e namelist.input.real ] ; then
	echo "ERROR, namelist.input not found!"
	exit 1
    else
	ln -s namelist.input.real namelist.input || exit 1
    fi
fi

# Copy the various wrf lookup files into the work directory.
if [ -z "$NUWRFDIR" ] ; then
    echo "ERROR, NUWRFDIR is not defined!"
    exit 1
fi
cd $NUWRFDIR/WRFV3/run || exit 1
for file in aerosol.formatted aerosol_lat.formatted aerosol_lon.formatted \
    aerosol_plev.formatted bulkdens.asc_s_0_03_0_9 bulkradii.asc_s_0_03_0_9 \
    CAM_ABS_DATA CAM_AEROPT_DATA CAMtr_volume_mixing_ratio.A1B \
    CAMtr_volume_mixing_ratio.A2 CAMtr_volume_mixing_ratio.RCP4.5 \
    CAMtr_volume_mixing_ratio.RCP6 CAMtr_volume_mixing_ratio.RCP8.5 \
    capacity.asc CCN_ACTIVATE.BIN CLM_ALB_ICE_DFS_DATA CLM_ALB_ICE_DRC_DATA \
    CLM_ASM_ICE_DFS_DATA CLM_ASM_ICE_DRC_DATA CLM_DRDSDT0_DATA \
    CLM_EXT_ICE_DFS_DATA CLM_EXT_ICE_DRC_DATA CLM_KAPPA_DATA CLM_TAU_DATA \
    co2_trans coeff_p.asc coeff_q.asc constants.asc ETAMPNEW_DATA \
    ETAMPNEW_DATA_DBL ETAMPNEW_DATA.expanded_rain \
    ETAMPNEW_DATA.expanded_rain_DBL GENPARM.TBL grib2map.tbl gribmap.txt \
    kernels.asc_s_0_03_0_9 kernels_z.asc LANDUSE.TBL masses.asc MPTABLE.TBL \
    ozone.formatted ozone_lat.formatted ozone_plev.formatted RRTM_DATA \
    RRTM_DATA_DBL RRTMG_LW_DATA RRTMG_LW_DATA_DBL RRTMG_SW_DATA \
    RRTMG_SW_DATA_DBL SOILPARM.TBL termvels.asc tr49t67 tr49t85 tr67t85 \
    URBPARM.TBL URBPARM_UZE.TBL VEGPARM.TBL wind-turbine-1.tbl ; do

    ln -fs $NUWRFDIR/WRFV3/run/$file $WORKDIR/$file || exit 1
    if [ ! -e $WORKDIR/$file ] ; then
	echo "ERROR, $file does not exist!"
	exit 1
    fi
done

# Run real.exe
ln -fs $NUWRFDIR/WRFV3/main/real.exe $WORKDIR/real.exe || exit 1
if [ ! -e $WORKDIR/real.exe ] ; then
    echo "ERROR, $WORKDIR/real.exe does not exist!"
    exit 1
fi
cd $WORKDIR || exit 1

# Special MPI launch command for SGI MPT on Discover                        
if [ "$USING_SGI_MPT" -eq 1 ] ; then
    mpiexec_mpt -n $SLURM_NTASKS ./real.exe || exit 1
elif [[ "$USING_INTEL_MPI" -eq 1 || "$USING_OPENMPI" -eq 1 ]] ; then
    mpirun -np $SLURM_NTASKS ./real.exe || exit 1
else
    echo "ERROR, unknown MPI implementation, don't know how to launch!"
    exit 1
fi

# Move the RSL log files.
if [ -e $WORKDIR/real_logs ] ; then
    rm -rf $WORKDIR/real_logs || exit 1
fi
mkdir $WORKDIR/real_logs || exit 1
rsl_files=`ls rsl.*`
for file in $rsl_files ; do
    mv $file $WORKDIR/real_logs/$file || exit 1
done

# Clean up symbolic link
if [ -L namelist.input ] ; then
    rm namelist.input || exit 1
fi

# The end
exit 0
