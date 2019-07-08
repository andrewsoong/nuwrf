#!/bin/csh
#
# Scripts to fetch Merra2 data and process for MERRA2WRF.  
# Feb 2015 - Jossy Jacob
#======================================================
# Run command:  ./Run_MERRA2.csh StartDate EndDate Rundir NUWRFDIR
# Example:  ./Run_MERRA2.csh 19890101 19890104 MERRA_data NUWRFDIR
# Example2:  ./Run_MERRA2.csh 19890101 19890104 . NUWRFDIR
#
# This program creates a dir data in your Rundir and fetch Merradata to this 
# directory.
# Merra2wrf process data and output is in Rundir/data/merra2wrf/
#
# Make sure that you copy the merra2wrf executable into the Rundir
# Make sure that you have the code: Get_dates_daily.py in your Rundir

set StartDate = $1 
set EndDate = $2 
set odir = $3
set nuwrfdir = $4

if ($#argv != 4) then
    echo 'Usage:   ./Run_MERRA2.csh StartDate EndDate Rundir NUWRFDIR'
    echo 'Example: ./Run_MERRA2.csh 19890101 19890104 MERRA_data NUWRFDIR'
    exit 0
endif

# Initialize WORKDIR
set WORKDIR = ${odir}

# Make sure WORKDIR is an absolute path 
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

# Keep track of directory names for the purpose of directory changes and 
# creation
set outdir = ${WORKDIR}/data
set outdir1 = ${outdir}/merra2wrf
mkdir -p $outdir 
mkdir -p $outdir1 

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

echo $Dates

#EMK modification for regression testing
if ! $?MERRA2DIR then
    #set MERRA2DIR = /discover/nobackup/projects/gmao/merra2/merra2/scratch/
    set MERRA2DIR = /discover/nobackup/projects/gmao/merra2/data/products/
endif

foreach file ($Dates) 
    set year =  `echo $file | cut -c1-4` 
    set month =  `echo $file | cut -c5-6` 
    set day =  `echo $file | cut -c7-8` 
    echo '1 DATE: ' $year $month $day 
    set Merraname = 'MERRA2_100'
    if ( $year < 1992 ) set Merraname = 'MERRA2_100'
    if ( $year > 1991 && $year < 2001 ) set Merraname = 'MERRA2_200'
    if ( $year > 2000 && $year < 2011 ) set Merraname = 'MERRA2_300'
    if ( $year > 2010 ) set Merraname = 'MERRA2_400'
    echo $Merraname
    cd $outdir 

    #Data set 1. const_2d_asm_Nx data:
    #
    set MerraFname1 = ${Merraname}'.const_2d_asm_Nx.00000000.nc4'
    if ! { cp ${MERRA2DIR}/${Merraname}/${MerraFname1} ${outdir}/. } then
	echo 'Error Copying ' ${MerraFname1}
	exit 1
    else
	echo 'Copied ' ${MerraFname1}
    endif

    #Data set 2. inst6_3d_ana_Nv data :
    #
    set MerraFname2 = ${Merraname}.inst6_3d_ana_Nv.${year}${month}${day}.nc4
    if ! { cp ${MERRA2DIR}/${Merraname}/Y${year}/M${month}/${MerraFname2} ${outdir}/. } then
	echo 'Error Copying ' ${MerraFname2}
	exit 1
    else
	echo 'Copied ' ${MerraFname2}
    endif 

    #Data set 3. inst6_3d_ana_Np data :
    #
    set MerraFname3 = ${Merraname}.inst6_3d_ana_Np.${year}${month}${day}.nc4
    if ! { cp ${MERRA2DIR}/${Merraname}/Y${year}/M${month}/${MerraFname3} ${outdir}/. } then
	echo 'Error copying' ${MerraFname3}
	exit 1
    else
	echo 'Copied ' ${MerraFname3}
    endif

    #Data set 4. tavg1_2d_slv_Nx data :
    #
    set MerraFname4 = ${Merraname}.tavg1_2d_slv_Nx.${year}${month}${day}.nc4
    if ! { cp ${MERRA2DIR}/${Merraname}/Y${year}/M${month}/${MerraFname4} ${outdir}/. } then
	echo 'Error copying' ${MerraFname4}
	exit 1
    else
	echo 'Copied ' ${MerraFname4}
    endif

    #Data set 5. tavg1_2d_ocn_Nx data :
    #
    set MerraFname5 = ${Merraname}.tavg1_2d_ocn_Nx.${year}${month}${day}.nc4
    if ! { cp ${MERRA2DIR}/${Merraname}/Y${year}/M${month}/${MerraFname5} ${outdir}/. } then
	echo 'Error copying' ${MerraFname5}
	exit 1
    else
	echo 'Copied ' ${MerraFname5}
    endif

    set merradate = ${year}-${month}-${day}
    set merradate2 = ${year}${month}${day}
    echo $merradate 
    cd ${WORKDIR}
    cat > namelist.merra2wrf_daily <<EOF 
! FILE:  namelist.merra2wrf
! AUTHOR:
! Feb 2015 Jossy Jacob
! DESCRIPTION:
! Lists input and output directories, number of days, dates, and
! names of MERRA2 files to process.
!------------------------------------------------------------------------------

&input

  ! Directory to write output
  outputDirectory = '${namelist_outdir1}',

  ! Directory with input MERRA files
  merraDirectory = '${namelist_outdir}',

  ! Format and name of const_2d_asm_Nx file
  merraFormat_const_2d_asm_Nx = 2,
  merraFile_const_2d_asm_Nx = '${MerraFname1}',  

  ! Number of days to process.  Note that each file type (excluding const_2d_asm_Nx) 
  ! will have one file per day.
  numberOfDays = 1,

  ! Dates of each day being processed (YYYY-MM-DD)
  !merraDates(1) = '2009-08-25',
  merraDates(1) = ${merradate},

  ! Format and Names of inst6_3d_ana_Nv files.
  merraFormat_inst6_3d_ana_Nv = 2,
  merraFiles_inst6_3d_ana_Nv(1) = '${MerraFname2}',

  ! Names of inst6_3d_ana_Np files.
  merraFormat_inst6_3d_ana_Np = 2,
  merraFiles_inst6_3d_ana_Np(1) = '${MerraFname3}',

  ! Names of tavg1_2d_slv_Nx files.
  merraFormat_tavg1_2d_slv_Nx = 2,
  merraFiles_tavg1_2d_slv_Nx(1) = '${MerraFname4}',

  ! Names of tavg1_2d_ocn_Nx files.
  merraFormat_tavg1_2d_ocn_Nx = 2,
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
#  The end
#
exit 0

