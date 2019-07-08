#!/bin/sh
#PBS -S /bin/sh
#PBS -N wrf
#PBS -l select=6:ncpus=28:mpiprocs=28:model=bro
#PBS -l walltime=12:00:00
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
# SCRIPT:  run_wrf.pleiades.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample script for running wrf.exe on the NASA ARC Pleiades supercomputer
# with PBS.
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

    if [ ! -e $file ] ; then
        echo "ERROR, $file does not exist!"
        exit 1
    fi

    ln -fs $NUWRFDIR/WRFV3/run/$file $WORKDIR/$file || exit 1
    if [ ! -e $WORKDIR/$file ] ; then
	echo "ERROR, $WORKDIR/$file does not exist!"
	exit 1
    fi
done

# For running with 2014 Goddard radiation
rm $WORKDIR/GODDARDRAD_SSLUT
ln -fs $NUWRFDIR/WRFV3/GODDARDRAD_SSLUT $WORKDIR/GODDARDRAD_SSLUT || exit 1
if [ ! -e $WORKDIR/GODDARDRAD_SSLUT ] ; then
    echo "ERROR, $WORKDIR/GODDARDRAD_SSLUT does not exist!"
    exit 1
fi

# For running WRF/LIS. 
# FIXME: Add logic to skip this if uncoupled WRF will be run.
rm $WORKDIR/LS_PARAMETERS
rm $WORKDIR/MET_FORCING
ln -fs $LISDIR/LS_PARAMETERS $WORKDIR/LS_PARAMETERS || exit 1
if [ ! -e $WORKDIR/LS_PARAMETERS ] ; then
    echo "ERROR, $WORKDIR/LS_PARAMETERS does not exist!"
    exit 1
fi
ln -fs $LISDIR/MET_FORCING $WORKDIR/MET_FORCING || exit 1
if [ ! -e $WORKDIR/MET_FORCING ] ; then
    echo "ERROR, $WORKDIR/MET_FORCING does not exist!"
    exit 1
fi

# Link the wrf.exe executable
ln -fs $NUWRFDIR/WRFV3/main/wrf.exe $WORKDIR/wrf.exe || exit 1
if [ ! -e $WORKDIR/wrf.exe ] ; then
    echo "ERROR, $WORKDIR/wrf.exe does not exist!"
    exit 1
fi

# Run wrf.exe
cd $WORKDIR || exit 1

# Different MPI launch commands for different implementations.              
if [ "$USING_SGI_MPT" -eq 1 ] ; then
    mpiexec -np 168 ./wrf.exe || exit 1 
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

