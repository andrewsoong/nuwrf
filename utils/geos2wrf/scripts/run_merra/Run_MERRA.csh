#!/bin/csh
#
# Scripts to ftp Merra data and process for MERRA2WRF.  
# 30 Sept 2014 - Jossy Jacob
#======================================================
# ./get_MERRA.csh StartDate EndDate Rundir NUWRFDIR
# ./get_MERRA.csh 19890101 19890104 MERRA_data NUWRFDIR

#
#This program creates a dir data in your Rundir and ftp Merradata to this directory.
# Merra2wrf process data and output is in Rundir/data/merra2wrf/
#
#Make sure that you copy the merra2wrf executable into the Rundir
# Make sure that you have the code: Get_dates_daily.py in your Rundir

set StartDate = $1 
set EndDate = $2 
set odir = $3
set nuwrfdir = $4

# Initialize WORKDIR
set WORKDIR = ${odir}
#Make sure WORKDIR is an absolute path 
if ( "${odir}" == "." ) then
  set WORKDIR = `pwd`
endif

# Keep track of the directory names that will make it to the namelist file
# separately to make sure we capture relative paths
set namelist_outdir = ${odir}/data
set namelist_outdir1 = ${namelist_outdir}/merra2wrf
if ( ! -e ${nuwrfdir}/utils/bin/merra2wrf.x ) then 
    echo 'Error, executable merra2wrf.x not found'
    exit 1
endif
# link the executable from NUWRFDIR
ln -sf ${nuwrfdir}/utils/bin/merra2wrf.x .

# Keep track of directory names for the purpose of directory changes and creation
set outdir = ${WORKDIR}/data
set outdir1 = ${outdir}/merra2wrf
 mkdir $outdir 
 mkdir $outdir1 
#set StartDate =  '19800101'
#set EndDate =  '19800103'
set dt = 1 #day
# Parse the start date
set syear = `echo ${StartDate} | cut -c1-4`
set smonth = `echo ${StartDate} | cut -c5-6`
set sday = `echo ${StartDate} | cut -c7-8`
# Parse the End date
set eyear = `echo ${EndDate} | cut -c1-4`
set emonth = `echo ${EndDate} | cut -c5-6`
set eday = `echo ${EndDate} | cut -c7-8`

ln -sf ${nuwrfdir}/utils/geos2wrf/scripts/run_merra/Get_dates_daily.py .
./Get_dates_daily.py $StartDate $EndDate $dt
# Creates Datetime.dat with all filenames
#
set Dates = `cat Datetime.dat`
cd $outdir 

#Data set 1. const_2d_asm_Nx data (always in HDFEOS2 format):
#
   set MerraFname1 = 'MERRA300.prod.assim.const_2d_asm_Nx.00000000.hdf'
   if ! { /usr/bin/wget "ftp://goldsmr2.sci.gsfc.nasa.gov/data/s4pa//MERRA_MONTHLY/MAC0NXASM.5.2.0/1979/${MerraFname1}" } then
     echo 'Error downloading ' ${MerraFname1}
     exit 1
   else
     echo 'Downloaded ' ${MerraFname1}
   endif
   echo $Dates

foreach file ($Dates) 
  set year =  `echo $file | cut -c1-4` 
  set month =  `echo $file | cut -c5-6` 
  set day =  `echo $file | cut -c7-8` 
  echo '1 DATE: ' $year $month $day 
  set Merraname = 'MERRA100'
  if ( $year < 1993 ) set Merraname = 'MERRA100'
  if ( $year > 1992 && $year < 2001 ) set Merraname = 'MERRA200'
  if ( $year > 2000 ) set Merraname = 'MERRA300'
  echo $Merraname
  cd $outdir 
#Data set 2. inst6_3d_ana_Nv data (variable names are HDF4/netCDF or HDFEOS2):
#ftp://goldsmr3.sci.gsfc.nasa.gov/data/s4pa/MERRA/MAI6NVANA.5.2.0/1980/01/
#File:MERRA100.prod.assim.inst6_3d_ana_Nv.19800101.hdf
#
   set MerraFname2 = ${Merraname}.prod.assim.inst6_3d_ana_Nv.${year}${month}${day}.hdf
   if ! { /usr/bin/wget "ftp://goldsmr3.sci.gsfc.nasa.gov/data/s4pa/MERRA/MAI6NVANA.5.2.0/${year}/${month}/${MerraFname2}" } then
     echo 'Error downloading ' ${MerraFname2}
     exit 1
   else
     echo 'Downloaded ' ${MerraFname2}
   endif 

#Data set 3. inst6_3d_ana_Np data (variable names are HDF4/netCDF or HDFEOS2):
#Daily Product:
#ftp://goldsmr3.sci.gsfc.nasa.gov/data/s4pa/MERRA/MAI6NPANA.5.2.0/
#Diurnal Product:
#ftp://goldsmr3.sci.gsfc.nasa.gov/data/s4pa/MERRA_DIURNAL/MAIUNPANA.5.2.0/
#
   set MerraFname3 = ${Merraname}.prod.assim.inst6_3d_ana_Np.${year}${month}${day}.hdf
   if ! { /usr/bin/wget "ftp://goldsmr3.sci.gsfc.nasa.gov/data/s4pa/MERRA/MAI6NPANA.5.2.0/${year}/${month}/${MerraFname3}" } then
     echo 'Error downloading ' ${MerraFname3}
     exit 1
   else
     echo 'Downloaded ' ${MerraFname3}
   endif

#Data set 4. tavg1_2d_slv_Nx data (variable names are HDF4/netCDF or HDFEOS2):
#ftp://goldsmr2.sci.gsfc.nasa.gov/data/s4pa/MERRA/MAT1NXSLV.5.2.0/1980/01/
#File:MERRA100.prod.assim.tavg1_2d_slv_Nx.19800101.hdf
#
  set MerraFname4 = ${Merraname}.prod.assim.tavg1_2d_slv_Nx.${year}${month}${day}.hdf
  if ! { /usr/bin/wget "ftp://goldsmr2.sci.gsfc.nasa.gov/data/s4pa/MERRA/MAT1NXSLV.5.2.0/${year}/${month}/${MerraFname4}" } then
     echo 'Error downloading ' ${MerraFname4}
     exit 1
   else
     echo 'Downloaded ' ${MerraFname4}
   endif

#Data set 5. tavg1_2d_ocn_Nx data (variable names are HDF4/netCDF or HDFEOS2):
#
  set MerraFname5 = ${Merraname}.prod.assim.tavg1_2d_ocn_Nx.${year}${month}${day}.hdf
  if ! { /usr/bin/wget "ftp://goldsmr2.sci.gsfc.nasa.gov/data/s4pa/MERRA/MAT1NXOCN.5.2.0/${year}/${month}/${MerraFname5}" } then
     echo 'Error downloading ' ${MerraFname5}
     exit 1
   else
     echo 'Downloaded ' ${MerraFname5}
   endif

  set merradate = ${year}-${month}-${day}
  set merradate2 = ${year}${month}${day}
  echo $merradate 
  cd ${WORKDIR}
cat > namelist.merra2wrf_daily <<EOF 
! FILE:  namelist.merra2wrf
! AUTHOR:
! Eric Kemp, NASA SSSO/Northrop Grumman
! 16 Mar 2012 - First version 
! 30 Sept 2014 - Jossy Jacob
!
! DESCRIPTION:
! Lists input and output directories, number of days, dates, and
! names of MERRA files to process.
!------------------------------------------------------------------------------

&input

  ! Directory to write output
  outputDirectory = '${namelist_outdir1}',

  ! Directory with input MERRA files
  merraDirectory = '${namelist_outdir}',

  ! Format and name of const_2d_asm_Nx file
  merraFormat_const_2d_asm_Nx = 4,
  merraFile_const_2d_asm_Nx = '${MerraFname1}',  

  ! Number of days to process.  Note that each file type (excluding const_2d_asm_Nx) 
  ! will have one file per day.
  numberOfDays = 1,

  ! Dates of each day being processed (YYYY-MM-DD)
  !merraDates(1) = '2009-08-25',
  merraDates(1) = ${merradate},

  ! Format and Names of inst6_3d_ana_Nv files.
  merraFormat_inst6_3d_ana_Nv = 4,
  merraFiles_inst6_3d_ana_Nv(1) = '${MerraFname2}',

  ! Names of inst6_3d_ana_Np files.
  merraFormat_inst6_3d_ana_Np = 4,
  merraFiles_inst6_3d_ana_Np(1) = '${MerraFname3}',

  ! Names of tavg1_2d_slv_Nx files.
  merraFormat_tavg1_2d_slv_Nx = 4,
  merraFiles_tavg1_2d_slv_Nx(1) = '${MerraFname4}',

  ! Names of tavg1_2d_ocn_Nx files.
  merraFormat_tavg1_2d_ocn_Nx = 4,
  merraFiles_tavg1_2d_ocn_Nx(1) = '${MerraFname5}',

/
EOF

# Run the merra2wrf 
if ! { ./merra2wrf.x namelist.merra2wrf_daily } then
  echo 'Error running merra2wrf for ' ${merradate}
  exit 1
endif
echo 'Created output for ' ${merradate}
# Clean the data files
rm ${outdir}/${MerraFname2}
rm ${outdir}/${MerraFname3}
rm ${outdir}/${MerraFname4}
rm ${outdir}/${MerraFname5}
end  # for all Datetimes
rm ${outdir}/${MerraFname1}
echo 'Output files are in data/merra2wrf/'
echo 'Completed MERRA ftp and MERRA2WRF processing. '
#
#  the end
#

