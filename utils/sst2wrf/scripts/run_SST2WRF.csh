#!/bin/csh
#======================================================
# Scripts to process SST data for SST2WRF. 
# Then create a namelist file, and process sst2wrf. 
# 30 Oct 2014 - Jossy P. Jacob (ASTG/ CISTO/ SSAI)
#======================================================
# How to run: 
# ./run_SST2WRF.csh StartDate  EndDate Instrument
# ./run_SST2WRF.csh 20090410 20090411 mw_ir
#------------------------------------------------------
# SST data from the following websites were ftp'd and processed. 
#  ftp://data.remss.com/SST/daily_v04.0/mw/
#  ftp://data.remss.com/SST/daily_v04.0/mw_ir/
#-------------------------------------------------------
# Start and end dates should be in the same year.
#Make sure to have data directory SST_data/ with files for the StartDate to EndDate are in your workdir. 
#----------------------------------------------------------------------------------
#===================================================================================

set StartDate = $1 
set EndDate = $2 
set instr = $3  # mw_ir (Microwave IR) or mw (Microwave OISST v4.0)

#set StartDate = 20090410
#set EndDate = 20090411 
#set instr = mw_ir  # mw_ir (Microwave IR) or mw (Microwave OISST v4.0)
#set WORKDIR = /discover/nobackup/jjacob/NUWRF/SST2WRF/RUN_SST2WRF/
set WORKDIR = `echo $PWD`
set NUWRFDIR = /discover/nobackup/jjacob/NUWRF/trunk/
set srcdir = ${NUWRFDIR}/utils/sst2wrf/src 
set outdir = ${WORKDIR}/SST_data
set dt = 1 #day (for daily files)
# Parse the start date
set syear = `echo ${StartDate} | cut -c1-4`
set smonth = `echo ${StartDate} | cut -c5-6`
set sday = `echo ${StartDate} | cut -c7-8`
# Parse the End date
set eyear = `echo ${EndDate} | cut -c1-4`
set emonth = `echo ${EndDate} | cut -c5-6`
set eday = `echo ${EndDate} | cut -c7-8`


ln -sf $srcdir/sst2wrf sst2wrf 
cp $srcdir/../scripts/Get_dayofyear_daily.py .

./Get_dayofyear_daily.py $StartDate $EndDate $dt
# Creates Datetime.dat with all file dates between start and end dates
#
set Dates = `cat Datetime.dat`
#process with SST2WRF
#
set year = $syear
if ( $instr == 'mw' ) then 
  set prefix = ${instr}'.fusion.'
  set ver = 'v04.0'
  set suffix = '.v04.0.gz'
else if ( $instr == 'mw_ir' ) then 
  set prefix = ${instr}'.fusion.'
  set ver = 'v04.0'
  set suffix = '.v04.0.gz'
endif   

echo $Dates
#foreach sday ($Dates) 
foreach date ($Dates)
  set sday = `echo $date | cut -c5-7`
#
  echo $sday
  set Fname = ${prefix}${year}'.'${sday}${suffix}
cat > namelist.sst2wrf<<EOF 
! FILE:  namelist.sst2wrf
! AUTHOR:
! Eric Kemp, NASA SSSO/Northrop Grumman
! 16 Mar 2012 - First version 
! 30 Oct 2014 - Jossy Jacob
!
! DESCRIPTION:
! Lists input and output directories, number of days, dates, and
! names of SST files to process.
!-----------------------------------------------------------------------------------------

&input
  instrument = "${instr}",
  year = ${year},
  dayOfYear = ${sday}, 
  version = "${ver}",
  inputDirectory = "${outdir}",
/
&output 
  outputDirectory = "${WORKDIR}/",
  prefixWPS = "SSTRSS",
/
&fakeoutput 
  numFakeHours = 4, 
  fakeHours = 0, 6, 12, 18,
/  
EOF

# Run the sst2wrf 
./sst2wrf > sst2wrf${Fname}.log  
end  # for all Dates
echo 'Completed SST2WRF processing. '
#
#  the end
#

