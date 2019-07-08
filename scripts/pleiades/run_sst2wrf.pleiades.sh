#!/bin/sh
#------------------------------------------------------------------------------
# NASA/GSFC, Computational and Information Science and Technology Office, 
# Code 606
#------------------------------------------------------------------------------
#
# SCRIPT: run_sst2wrf.sh
#
# AUTHOR:
# Eric Kemp, NASA CISTO/SSAI.  Based heavily on Run_SST.csh script by 
# Jossy Jacob.
#
# DESCRIPTION:
# Sample script for running SST2WRF using downloaded RSS SST data files.
#
#------------------------------------------------------------------------------

# Load config file for modules and paths.
source ./config.pleiades.sh || exit 1

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
ln -sf ${NUWRFDIR}/utils/sst2wrf/src/sst2wrf . || exit 1

usage="Usage: $0 startdate enddate instr hour1 [hour2 ... hourN]\n"
usage="${usage} Where startdate and enddate are integers (yyyymmdd),\n"
usage="${usage}   instr indicates instruments used to generate SST field,\n"
usage="${usage}   and hour1 through hourN indicates hour-of-day (in UTC)"

# Check args
if [ $# -lt 4 ] ; then
    echo ${usage}
    exit 1
fi
startDate="$1" || exit 1
endDate="$2" || exit 1
instr="$3" || exit 1

numHours=$(( $# - 3 ))
shift 3
hours=""
until [ -z "$1" ] ; do
    # Reject if not an integer
    case "$1" in
	*[!0-9]* )
	    echo $usage
	    exit 1
	    ;;
	*)
	    ;;
    esac
    # Reject if outside 0-23 UTC
    if [ "$1" -lt 0 ] || [ "$1" -gt 23 ] ; then
	echo $usage
	exit 1
    fi
    hours="${hours} $1,"
    shift
done

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
    # Two possibilities: higher quality or "real time".
    ver1="v04.0"
    ver2="rt"
else
    echo "ERROR, unrecognised instr value $instr"
    echo "Valid choices are mw and mw_ir"
    exit 1
fi

indir="./sstdownload/${instr}"
if [ ! -e ${indir} ] ; then
    echo "ERROR, cannot find RSS SST directory ${indir}!"
    exit 1
fi

outdir="./sstdata/${instr}"
mkdir -p ${outdir} || exit 1

# Process data for each day
for date in $dates ; do
    year=`echo $date | cut -c1-4` || exit 1
    jday=`echo $date | cut -c5-7` || exit 1
    fname1="${prefix}${year}.${jday}.${ver1}.gz"
    fname2="${prefix}${year}.${jday}.${ver2}.gz"
    if [ -e ${indir}/${fname1} ] ; then
	fname=${fname1}
	ver=${ver1}
    elif [ -e ${indir}/${fname2} ] ; then
	fname=${fname2}
	ver=${ver2}
    else
	echo "ERROR, cannot find ${fname1} or ${fname2}!"
	echo "Run fetch_rss_sst.sh to download SST data!"
	exit 1
    fi

    # Decompress SST file
    cd ${indir} || exit 1
    cp ${fname} ${fname}.backup || exit 1
    gunzip ${fname} || exit 1
    cd $WORKDIR || exit 1

    # Set up SST2WRF
    cat > namelist.sst2wrf<<EOF

  &input
    instrument="${instr}",
    year=${year},
    dayOfYear=${jday},
    version="${ver}",
    inputDirectory = "${indir}",
  /

  &output
    outputDirectory = "${outdir}",
    prefixWPS="SSTRSS",
  /

  &fakeoutput
    numFakeHours=${numHours},
    fakeHours = ${hours}
  /
EOF

    # Run SST2WRF
    ./sst2wrf > "sst2wrf.${prefix}${year}.${jday}.${ver}.log" || exit 1
    
    # Cleanup
    cd ${indir} || exit 1
    rm ${prefix}${year}.${jday}.${ver} || exit 1
    mv ${prefix}${year}.${jday}.${ver}.gz.backup \
	${prefix}${year}.${jday}.${ver}.gz || exit 1
    cd $WORKDIR || exit 1

done

# The end
exit 0



