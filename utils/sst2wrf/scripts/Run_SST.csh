#!/bin/csh
#======================================================
# Scripts to ftp SST data and process for SST2WRF. 
# This script ftp SST data from remote location
# Then create a namelist file, and process sst2wrf. 
# 30 Oct 2014 - Jossy P. Jacob (ASTG/ CISTO/ SSAI)
#======================================================
# How to run: 
# ./Run_SST.csh StartDate EndDate instrument RUNDIR NUWRFDIR
# ./Run_SST.csh 20100101 20100104 mw_ir <SCRATCH_DIR> <NUWRF_INSTALL_DIR>
#------------------------------------------------------
# SST data from the following websites will be downloaded and processed.
#
#   http://data.remss.com/SST/daily_v04.0/mw/
#   http://data.remss.com/SST/daily_v04.0/mw_ir/
#
# Note: mw_ir (Microwave IR) is only available up to 2017
#-------------------------------------------------------
#This script creates a dir sstdata/ in your RUNDIR and ftp SSTdata to this directory.
# sst2wrf process sstdata and output is in RUNDIR/data/${instr}/  (instr:mw or mw_ir)
# Start and end dates should be in the same year.
#===========================================================================================================================================
# Command line arguments:
set StartDate = $1 
set EndDate = $2 
set instr = $3  # mw_ir (Microwave IR) or mw (Microwave OISST v4.0)
set odir = $4 
set NUWRFDIR = $5

set WORKDIR = ${odir}
if ( "${odir}" == "." ) then
  set WORKDIR = `pwd`
endif

set srcdir = ${NUWRFDIR}/utils/sst2wrf/src 
set outdir = ${WORKDIR}/sstdata
set outdir1 = ${outdir}/${instr} 
mkdir -p $outdir 
mkdir -p $outdir1 

set dt = 1 #day (for daily files)
# Parse the start date
set syear = `echo ${StartDate} | cut -c1-4`
set smonth = `echo ${StartDate} | cut -c5-6`
set sday = `echo ${StartDate} | cut -c7-8`
# Parse the End date
set eyear = `echo ${EndDate} | cut -c1-4`
set emonth = `echo ${EndDate} | cut -c5-6`
set eday = `echo ${EndDate} | cut -c7-8`
cd $WORKDIR

if (${syear} > 2017 && ${instr} == 'mw_ir') then
   echo 'SST2WRF cannot process mw_ir data for years >= '${syear}
   exit
endif

if ( ! -e ${NUWRFDIR}/utils/bin/sst2wrf.x ) then
    echo 'Error, executable sst2wrf.x not found'
    exit 1
endif

# link the executable from NUWRFDIR
ln -sf ${NUWRFDIR}/utils/bin/sst2wrf.x .

# Creates Datetime.dat with all file dates between start and end dates
ln -sf ${NUWRFDIR}/utils/sst2wrf/scripts/Get_dayofyear_daily.py .
./Get_dayofyear_daily.py $StartDate $EndDate $dt
if ( ! -f 'Datetime.dat') then
   echo 'Failed to create Datetime.dat file!'
   exit
endif

set Dates = `cat Datetime.dat`
cd ${outdir}

# Download SST data and process with SST2WRF
set url_root = 'http://data.remss.com/sst/'
set year = $syear
set prefix = ${instr}'.fusion.'
set ftpname = ${url_root}/daily_v04.0/${instr}/${year}/

if ( $instr == 'mw' ) then
   set ver = 'rt'
else # mw_ir
   set ver = 'v04.0'
endif
set suffix = '.'${ver}'.gz'

echo Processing $Dates
foreach date ($Dates) 
 set sday = `echo ${date} | cut -c5-7`
  cd ${outdir}
  set Fname = ${prefix}${year}'.'${sday}${suffix}
  set fpname = ${ftpname}${Fname} 
  echo 'Downloading ' $Fname
  echo wget $fpname
  wget "${fpname}"
  gunzip $Fname
  cd ${WORKDIR}
cat > namelist.sst2wrf<<EOF 
! FILE:  namelist.sst2wrf
! DESCRIPTION:
! Lists input and output directories, number of days, dates, and
! names of SST files to process.
!-----------------------------------------------------------------------------------------

&input
  instrument = "${instr}",
  year = ${year},
  dayOfYear = ${sday}, 
  version = "${ver}",
  inputDirectory = "sstdata/",
/
&output 
  outputDirectory = "sstdata/${instr}",
  prefixWPS = "SSTRSS",
/
&fakeoutput 
  numFakeHours = 4, 
  fakeHours = 0, 6, 12, 18,
/  
EOF

# Run the sst2wrf 
./sst2wrf.x > sst2wrf${Fname}.log  
end  # for all Dates

echo 'Output files are in sstdata/'$instr
echo 'Completed SST FTP and SST2WRF processing. '
