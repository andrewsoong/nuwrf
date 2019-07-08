#!/bin/sh
#------------------------------------------------------------------------------
# NASA/GSFC, Computational and Information Science and Technology Office, 
# Code 606
#------------------------------------------------------------------------------
#
# SCRIPT: fetch_rss_sst.sh
#
# AUTHOR:
# Eric Kemp, NASA CISTO/SSAI.  Based heavily on Run_SST.csh script by 
# Jossy Jacob.
#
# DESCRIPTION:
# Sample script for downloading RSS SST data using wget.
#
#------------------------------------------------------------------------------

# Load config file for modules and paths.
source ./config.discover.sh || exit 1

# Go to work directory and make sure lower-level Python script is available.
if [ -z "$WORKDIR" ] ; then
    echo "ERROR, WORKDIR is not defined!"
    exit 1
fi
cd $WORKDIR || exit 1
if [ -z "$NUWRFDIR" ] ; then
    echo "ERROR, NUWRFDIR is not defined!"
    exit 1
fi
ln -sf ${NUWRFDIR}/utils/sst2wrf/scripts/Get_dayofyear_daily.py . || exit 1

# Check args
if [ $# -ne 3 ] ; then
    echo "Usage: $0 startdate enddate instr"
    echo "  Where startdate and enddate are integers (yyyymmdd)"
    echo "  and instr indicates instruments used to generate SST field."
    exit 1
fi
startDate="$1" || exit 1
endDate="$2" || exit 1
instr="$3" || exit 1

# Parse start date
syear=`echo $startDate | cut -c1-4` || exit 1
smonth=`echo $startDate | cut -c5-6` || exit 1
sday=`echo $startDate | cut -c7-8` || exit 1

# Parse end date
eyear=`echo $endDate | cut -c1-4` || exit 1
emonth=`echo $endDate | cut -c5-6` || exit 1
eday=`echo $endDate | cut -c7-8` || exit 1

# Create list of dates to download data for.
./Get_dayofyear_daily.py $startDate $endDate 1 || exit 1
dates=`cat Datetime.dat` || exit 1

# Set up download paths
if [ "$instr" == "mw" ] || [ "$instr" == "mw_ir" ] ; then
    prefix="${instr}.fusion."
    ver="v04.0"
    # Two possibilities: higher quality or "real time".
    suffix1=".v04.0.gz"
    suffix2=".rt.gz"
else
    echo "ERROR, unrecognised instr value $instr"
    echo "Valid choices are mw and mw_ir"
    exit 1
fi

mkdir -p sstdownload/${instr} || exit 1
cd sstdownload/${instr} || exit 1

# Download data for each day
for date in $dates ; do
    year=`echo $date | cut -c1-4` || exit 1
    ftpname="ftp://data.remss.com/SST/daily_${ver}/${instr}/${year}/"
    jday=`echo $date | cut -c5-7` || exit 1
    fname1="${prefix}${year}.${jday}${suffix1}"
    fpname1="${ftpname}${fname1}"
    fname2="${prefix}${year}.${jday}${suffix2}"
    fpname2="${ftpname}${fname2}"
    # Try to get the higher quality file; if not available, settle for realtime
    wget "$fpname1" || wget "$fpname2" || exit 1
done

exit 0



