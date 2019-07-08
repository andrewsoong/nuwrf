#!/bin/sh
#------------------------------------------------------------------------------
# NASA/GSFC, Computational and Information Science and Technology Office,
# Code 606
#------------------------------------------------------------------------------
#                                                                              
# SCRIPT:  run_geos2wrf_merra2.discover.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA CISTO/SSAI
#                                                                              
# DESCRIPTION: 
# Sample script for running GEOS2WRF code on NASA GSFC Discover supercomputer 
# to process MERRA2 assimilation data on GEOS-5 model levels every 3 hours. 
# Program namelist files are autogenerated.
#
# NOTE 1: MERRA2 "assimilation" data refers to the output from the GEOS-5 model
# as it is being adjusted toward the 6-hourly MERRA2 analyses via IAU. 
#
# NOTE 2: This script assumes the user has access to the GMAO MERRA2 directory
# on Discover. This data is not available for general use yet (as of 24 April
# 2015).
#
# For each time, there are 8 steps:
# 1. Process const_2d_asm_Nx collection file with geos2wps. Gets surface 
#    geopotential, lake fraction, and ocean fraction.
# 2. Process inst3_3d_asm_Nv collection file with geos2wps. Gets mean sea-level
#    pressure, surface pressure, geopotential height, temperature, specific 
#    humidity, zonal and meridional winds, and air pressure on model levels.
# 3. Process tavg1_2d_ocn_Nx collection file with geos2wps. Gets sea ice 
#    fraction.
# 4. Process tavg1_2d_slv_Nx collection file with geos2wps. Gets 10-meter zonal
#    and meridional winds, 2-meter temperature and specific humidity, and skin
#    temperature.
# 5. Run createSOILHGT to derive GEOS-5 terrain height.
# 6. Run createLANDSEA to derive GEOS-5 land-sea mask.
# 7. Run createRH to derive 2-meter relative humidity and relative humidity
#    at GEOS-5 model levels.
# 8. Consolidate data into single WPS intermediate binary file, and delete 
#    fields not directly usable by METGRID.
#
#------------------------------------------------------------------------------

# Strings must be YYYYMMDDHH
startTimeString=2007011918
endTimeString=2007012018

# Path to top level MERRA2 directory on Discover. User should contact GMAO
# for access. User can override path with environment variable (useful for
# regression testing).
merra2Root=/discover/nobackup/projects/gmao/merra2/merra2/scratch/
if [ ! -z "$MERRA2ROOT" ] ; then
    if [ -e "$MERRA2ROOT" ] ; then
	echo "Resetting merra2Root path to $MERRA2ROOT"
	merra2Root=$MERRA2ROOT
    else
	echo "WARNING, cannot access MERRA2ROOT path $MERRA2ROOT"
	echo "Reset MERRA2ROOT environment variable to valid value"
	echo "or unset it altogether!"
	exit 1
    fi
fi
if [ ! -e "$merra2Root" ] ; then
    echo "ERROR, $merra2Root does not exist!"
    exit 1
fi

# Load config file for modules and paths
source ./config.discover.sh || exit 1

# Move to work directory and make sure namelist.input is present.
if [ -z "$WORKDIR" ] ; then
    echo "ERROR, WORKDIR is not defined!"
    exit 1
fi
cd $WORKDIR || exit 1

deltaHour=3 # Process every three hours of MERRA2 assimilation data.

# Create links to GEOS2WRF binaries.
if [ -z "$NUWRFDIR" ] ; then
    echo "ERROR, NUWRFDIR is not defined!"
    exit 1
fi
for exec in geos2wps createSOILHGT createLANDSEA createRH ; do
    file="$NUWRFDIR/utils/geos2wrf_2/$exec"
    if [ ! -e "$file" ] ; then
	echo "ERROR, $file does not exist!"
	exit 1
    fi
    ln -fs $file $WORKDIR/$exec || exit 1
done

# Create link to updateDateTime.py
file="$NUWRFDIR/utils/geos2wrf_2/scripts/util/updateDateTime.py"
if [ ! -e "$file" ] ; then
    echo "ERROR, $file does not exist!"
    exit 1
fi
ln -fs $file $WORKDIR/updateDateTime.py || exit 1

# Loop through each time in requested time period.
currentTimeString=${startTimeString}
while [ "$currentTimeString" -le "$endTimeString" ] ; do

    cyear=`echo $currentTimeString | cut -c1-4`
    cmonth=`echo $currentTimeString | cut -c5-6`
    cday=`echo $currentTimeString | cut -c7-8`
    chour=`echo $currentTimeString | cut -c9-10`
    timeStamp="${cyear}-${cmonth}-${cday}_${chour}"
    date="${cyear}${cmonth}${cday}"

    # Sanity check chour. 
    valid=0
    for hour in "00" "03" "06" "09" "12" "15" "18" "21" ; do
        if [ "$chour" -eq "$hour" ] ; then
            valid=1
        fi
    done
    if [ ! "$valid" -eq 1 ] ; then
        echo "ERROR, invalid hour for MERRA2 data!"
        echo "chour = $chour"
        echo "Must be 00, 03, 06, 09, 12, 15, 18, or 21!"
        exit 1
    fi

    # Chop off preceeding zero in chour (avoids math errors later).
    firstDigit=`echo $chour | cut -c1`
    if [ "$firstDigit" -eq "0" ] ; then
        chour=`echo $chour | cut -c2`
    fi

    # Get prefix for MERRA2 stream, based on current year.
    if [ "$cyear" -lt 1992 ] ; then
        stream="MERRA2_100"
    elif [ "$cyear" -lt 2001 ] ; then
        stream="MERRA2_200"
    elif [ "$cyear" -lt 2011 ] ; then
        stream="MERRA2_300"
    else
        stream="MERRA2_400"
    fi

    # Get collection root directory
    collectionRoot=$merra2Root/$stream/stage

    #--------------------------------------------------------------------------
    # *** const_2d_asm_Nx collection ***
    #--------------------------------------------------------------------------

    echo "Processing const_2d_asm_Nx for $timeStamp"

    # Link to file
    file=$stream.const_2d_asm_Nx.00000000.nc4
    ln -fs $collectionRoot/$file $file || exit 1
    
    # Customize namelist
    cat > namelist.geos2wps<<EOF 
&files
  geosFileFormat=2,
  geosFileName='$file',
  outputDirectory='./',
/

&coordinates
 longitudeName='lon',
 latitudeName='lat',
/

&forecast
 numberOfTimes=1,
 validTimes(1)='${timeStamp}',
 timeIndices(1) = 1,
 forecastHours(1)=0,
/

&variables
 numberOfVariables=3,

 variableRanks(1) = 3,
 variableLevelTypes(1) = 1,
 variableNamesIn(1)='PHIS',
 variableNamesOut(1)='PHIS',
 variableUnits(1)='m**2 s**-2',
 variableDescriptions(1)='Surface geopotential',

 variableRanks(2) = 3,
 variableLevelTypes(2) = 1,
 variableNamesIn(2)='FRLAKE',
 variableNamesOut(2)='FRLAKE',
 variableUnits(2)='proprtn',
 variableDescriptions(2)='Lake fraction',

 variableRanks(3) = 3,
 variableLevelTypes(3) = 1,
 variableNamesIn(3)='FROCEAN',
 variableNamesOut(3)='FROCEAN',
 variableUnits(3)='proprtn',
 variableDescriptions(3)='Ocean fraction',

/

 &subsetData
  subset=.false.
 /

EOF

    # Run geos2wps. No MPI used since it is serial
    ./geos2wps || exit 1
    
    # Clean up 
    rm $file || exit 1

    #--------------------------------------------------------------------------
    # *** inst3_3d_asm_Nv collection ***
    #--------------------------------------------------------------------------

    echo "Processing inst3_3d_asm_Nv for $timeStamp"

    timeLevel=$((chour/3+1)) # Three-hourly data in this collection

    # Link to file
    file=$stream.inst3_3d_asm_Nv.$date.nc4
    ln -fs $collectionRoot/Y$cyear/M$cmonth/$file $file || exit 1

    # Customize namelist
    cat > namelist.geos2wps<<EOF
&files
  geosFileFormat=2,
  geosFileName='$file',
  outputDirectory='./',
/

&coordinates
 longitudeName='lon',
 latitudeName='lat',
 hasVerticalDimension=.true.,
 verticalName='lev',
/

&forecast
 numberOfTimes=1,
 validTimes(1)='${timeStamp}',
 timeIndices(1) = ${timeLevel},
 forecastHours(1)=0,
/

&variables
 numberOfVariables=8,

 variableRanks(1) = 3,
 variableLevelTypes(1) = 4,
 variableNamesIn(1)='SLP',
 variableNamesOut(1)='PMSL',
 variableUnits(1)='Pa',
 variableDescriptions(1)='Sea-level Pressure',

 variableRanks(2) = 3,
 variableLevelTypes(2) = 1,
 variableNamesIn(2)='PS',
 variableNamesOut(2)='PSFC',
 variableUnits(2)='Pa',
 variableDescriptions(2)='Surface Pressure',

 variableRanks(3) = 4,
 variableLevelTypes(3) = 11,
 variableNamesIn(3)='H',
 variableNamesOut(3)='HGT',
 variableUnits(3)='m',
 variableDescriptions(3)='Height',

 variableRanks(4) = 4,
 variableLevelTypes(4) = 11,
 variableNamesIn(4)='T',
 variableNamesOut(4)='TT',
 variableUnits(4)='K',
 variableDescriptions(4)='Temperature',

 variableRanks(5) = 4,
 variableLevelTypes(5) = 11,
 variableNamesIn(5)='QV',
 variableNamesOut(5)='SPECHUMD',
 variableUnits(5)='kg kg-1',
 variableDescriptions(5)='Specific Humidity',

 variableRanks(6) = 4,
 variableLevelTypes(6) = 11,
 variableNamesIn(6)='U',
 variableNamesOut(6)='UU',
 variableUnits(6)='m s-1',
 variableDescriptions(6)='U',

 variableRanks(7) = 4,
 variableLevelTypes(7) = 11,
 variableNamesIn(7)='V',
 variableNamesOut(7)='VV',
 variableUnits(7)='m s-1',
 variableDescriptions(7)='V',

 variableRanks(8) = 4,
 variableLevelTypes(8) = 11,
 variableNamesIn(8)='PL',
 variableNamesOut(8)='PRESSURE',
 variableUnits(8)='Pa',
 variableDescriptions(8)='Pressure',

/

 &subsetData
  subset=.false.
 /

EOF

    # Run geos2wps. No MPI used since it is serial
    ./geos2wps || exit 1
    
    # Clean up 
    rm $file || exit 1

    #--------------------------------------------------------------------------
    # *** tavg1_2d_ocn_Nx collection ***
    #--------------------------------------------------------------------------

    echo "Processing tavg1_2d_ocn_Nx for $timeStamp"

    timeLevel=$((chour+1)) # Hourly data in this collection

    # Link to file
    file=$stream.tavg1_2d_ocn_Nx.$date.nc4
    ln -fs $collectionRoot/Y$cyear/M$cmonth/$file $file || exit 1

    # Customize namelist file.
    cat > namelist.geos2wps<<EOF
&files
  geosFileFormat=2,
  geosFileName='$file'
  outputDirectory='./',
/

&coordinates
 longitudeName='lon',
 latitudeName='lat',
/

&forecast
 numberOfTimes=1,
 validTimes(1)='${timeStamp}',
 timeIndices(1) = ${timeLevel},
 forecastHours(1)=0,
/

&variables
 numberOfVariables=1,

 variableRanks(1) = 3,
 variableLevelTypes(1) = 1,
 variableNamesIn(1)='FRSEAICE',
 variableNamesOut(1)='SEAICE',
 variableUnits(1)='proprtn',
 variableDescriptions(1)='Ice flag',
/

 &subsetData
  subset=.false.
 /

EOF

    # Run geos2wps. No MPI used since it is serial
    ./geos2wps || exit 1
    
    # Clean up 
    rm $file || exit 1

    #--------------------------------------------------------------------------
    #*** tavg1_2d_slv_Nx collection ***
    #--------------------------------------------------------------------------

    echo "Processing tavg1_2d_slv_Nx for $timeStamp"

    timeLevel=$((chour+1)) # Hourly data in this collection

    # Link to file
    file=$stream.tavg1_2d_slv_Nx.$date.nc4
    ln -fs $collectionRoot/Y$cyear/M$cmonth/$file $file || exit 1

    # Customize namelist file.
    cat > namelist.geos2wps<<EOF
&files
  geosFileFormat=2,
  geosFileName='$file',
  outputDirectory='./',
/

&coordinates
 longitudeName='lon',
 latitudeName='lat',
/

&forecast
 numberOfTimes=1,
 validTimes(1)='${timeStamp}',
 timeIndices(1) = ${timeLevel},
 forecastHours(1)=0,
/

&variables
 numberOfVariables=5,

 variableRanks(1) = 3,
 variableLevelTypes(1) = 3,
 variableNamesIn(1)='U10M',
 variableNamesOut(1)='UU',
 variableUnits(1)='m s-1',
 variableDescriptions(1)='U                 at 10 m',

 variableRanks(2) = 3,
 variableLevelTypes(2) = 3,
 variableNamesIn(2)='V10M',
 variableNamesOut(2)='VV',
 variableUnits(2)='m s-1',
 variableDescriptions(2)='V                 at 10 m',

 variableRanks(3) = 3,
 variableLevelTypes(3) = 2,
 variableNamesIn(3)='T2M',
 variableNamesOut(3)='TT',
 variableUnits(3)='K',
 variableDescriptions(3)='Temperature       at 2 m',

 variableRanks(4) = 3,
 variableLevelTypes(4) = 2,
 variableNamesIn(4)='QV2M',
 variableNamesOut(4)='SPECHUMD',
 variableUnits(4)='kg kg-1',
 variableDescriptions(4)='Specific Humidity at 2 m',

 variableRanks(5) = 3,
 variableLevelTypes(5) = 1,
 variableNamesIn(5)='TS',
 variableNamesOut(5)='SKINTEMP',
 variableUnits(5)='K',
 variableDescriptions(5)='Skin temperature (can use for SST also)',
/

 &subsetData
  subset=.false.
 /

EOF

    # Run geos2wps. No MPI used since it is serial
    ./geos2wps || exit 1
    
    # Clean up 
    rm $file || exit 1

    #--------------------------------------------------------------------------
    # Run createSOILHGT.
    #--------------------------------------------------------------------------

    echo "Running createSOILHGT for $timeStamp"

    # Create temporary binary input file.
    cp PHIS_GROUND_LEVEL:${timeStamp} GEOS_TMP:${timeStamp} || exit 1

    # Customize namelist file
    cat > namelist.createSOILHGT<<EOF
&input
  directory='./',
  prefix='GEOS_TMP',
  year=${cyear}, 
  month=${cmonth},
  day=${cday},
  hour=${chour},
  surfaceGeopotentialName='PHIS',
/
EOF
 
    ./createSOILHGT || exit 1

    # Clean up
    rm GEOS_TMP:${timeStamp} || exit 1

    #--------------------------------------------------------------------------
    # Run createLANDSEA.
    #--------------------------------------------------------------------------

    echo "Running createLANDSEA for $timeStamp"

    # Create temporary binary input file.
    cat FRLAKE_GROUND_LEVEL:${timeStamp} \
        FROCEAN_GROUND_LEVEL:${timeStamp} \
        > GEOS_TMP:${timeStamp} || exit 1

    # Customize namelist
    cat > namelist.createLANDSEA<<EOF
&input
  directory='./',
  prefix='GEOS_TMP',
  year=${cyear}, 
  month=${cmonth},
  day=${cday},
  hour=${chour},
  lakeFractionName='FRLAKE',
  oceanFractionName='FROCEAN',
/

EOF

    ./createLANDSEA || exit 1

    # Clean up
    rm GEOS_TMP:${timeStamp} || exit 1

    #--------------------------------------------------------------------------
    # Run createRH
    #--------------------------------------------------------------------------

    echo "Running createRH for $timeStamp"

    # Create temporary binary input file
    cat PSFC_GROUND_LEVEL:${timeStamp} \
        PRESSURE_MODEL_LEVEL:${timeStamp} \
        TT_MODEL_LEVEL:${timeStamp} \
        TT_2M_ABOVE_GROUND_LEVEL:${timeStamp} \
        SPECHUMD_MODEL_LEVEL:${timeStamp} \
        SPECHUMD_2M_ABOVE_GROUND_LEVEL:${timeStamp} \
        > GEOS_TMP:${timeStamp} || exit 1

    # Customize namelist
    cat > namelist.createRH<<EOF

&input
  directory='./',
  prefix='GEOS_TMP',
  year=${cyear}, 
  month=${cmonth},
  day=${cday},
  hour=${chour},
  processSurfacePressure=.true.,
  surfacePressureName='PSFC',
  pressureName='PRESSURE',
  temperatureName='TT',
  specificHumidityName='SPECHUMD',
/

EOF

    ./createRH || exit 1

    # Clean up
    rm GEOS_TMP:${timeStamp} || exit 1

    #--------------------------------------------------------------------------
    # Consolidate the data
    #--------------------------------------------------------------------------

    echo "Consolidating data for $timeStamp"

    # LIS and external SST files may be in this directory. If they are, we
    # should preserve the files.
    if [ -e "LIS:${timeStamp}" ] ; then
	mv LIS:${timeStamp} LIS:${timeStamp}.save || exit 1
    fi
    if [ -e "SST:${timeStamp}" ] ; then
	mv SST:${timeStamp} SST:${timeStamp}.save || exit 1
    fi

    # Consolidate the MERRA2 data usable by METGRID, and delete the remainder.
    cat HGT_MODEL_LEVEL:${timeStamp} \
        LANDSEA_GROUND_LEVEL:${timeStamp} \
        PMSL_MEAN_SEA_LEVEL:${timeStamp} \
        PRESSURE_MODEL_LEVEL:${timeStamp} \
        PSFC_GROUND_LEVEL:${timeStamp} \
        RH_MODEL_LEVEL:${timeStamp} \
        RH_2M_ABOVE_GROUND_LEVEL:${timeStamp} \
        SEAICE_GROUND_LEVEL:${timeStamp} \
        SKINTEMP_GROUND_LEVEL:${timeStamp} \
        SOILHGT_GROUND_LEVEL:${timeStamp} \
        TT_2M_ABOVE_GROUND_LEVEL:${timeStamp} \
        TT_MODEL_LEVEL:${timeStamp} \
        UU_10M_ABOVE_GROUND_LEVEL:${timeStamp} \
        UU_MODEL_LEVEL:${timeStamp} \
        VV_10M_ABOVE_GROUND_LEVEL:${timeStamp} \
        VV_MODEL_LEVEL:${timeStamp} \
        > MERRA2:${timeStamp}.save || exit 1

    rm *:${timeStamp} || exit 1

    # Finish up
    mv MERRA2:${timeStamp}.save MERRA2:${timeStamp} || exit 1
    if [ -e LIS:${timeStamp}.save ] ; then
	mv LIS:${timeStamp}.save LIS:${timeStamp} || exit 1
    fi
    if [ -e SST:${timeStamp}.save ] ; then
	mv SST:${timeStamp}.save SST:${timeStamp} || exit 1
    fi

    #--------------------------------------------------------------------------
    # Go to next time
    #--------------------------------------------------------------------------
    currentTimeString=`./updateDateTime.py $currentTimeString $deltaHour`
done


# The end
echo "Processing completed."
exit 0

    