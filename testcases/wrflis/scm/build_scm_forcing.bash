#!/bin/bash
# sets environment and command-line arguments for the ncl script
#
# NOTES/TODO: 
# Need to make the cdl file have the correct number layers - get from met_em* file. 

# NU-WRF NOTES:  
# 1.  Ensembles have not been tested.  Recommend ensSize=0.
# 2.  Forcing from met_em has not been tested.  Recommend scm_force=1 and
#     io_form_auxinput3=0 in namelist.input.
# 3.  If using WRF-LIS, ignore the output soil_init.txt file, and use the
#     LIS restart file in IDEAL (using the &lis block in namelist.input).
# 4.  Make sure $NCARG_ROOT is defined in environment so NCL scripts can
#     be run.

# --- BEGIN USER MODIFICATIONS ---
ensSize=0 # ensemble size
forceCDL=forcing_file.cdl # forcing file cdl
metPath=/gpfsm/dnb02/ccruz/scratch/nu-wrf/tutorials/scm
simLength=86400 # length of simulation in seconds
forcingInterval=-1800 # seconds, negative if don't need to interpolate
# following in C-style indexing (starts at 0)
xll=0; # lower left x index of mass grid square containing SCM
yll=0; # lower left y index of mass grid square containing SCM
randSeed1=29 # these will be used to the seed (not used for ensSize=0)
randSeed2=376201
# centerDate="" # valid initialization date or empty
centerDate="2006-07-14" # valid initialization date
centerTime="00:00:00"
forceArcRoot=/gpfsm/dnb02/ccruz/scratch/nu-wrf/tutorials/scm
# --- END OF USER MODIFICATIONS ---

# check for existence of ncl script
if [ ! -e build_scm_forcing.ncl ] ; then
    echo "ERROR, build_scm_forcing.ncl not found!" && exit 1
fi

# check for existence of ncl script
if [ ! -e $forceCDL ] ; then
    echo "ERROR, $forceCDL not found!" && exit 1
fi

# if not a specified date then make them all
if [ -z $centerDate ]; then
  initList=`ls ${metPath}/met_em*${centerTime}.nc`
else
  initList=${metPath}/met_em.d01.${centerDate}_${centerTime}.nc 
fi

export SIMLENGTH=$simLength
export METPATH=$metPath
export XLL=$xll
export YLL=$yll
export RANDSEEDa=$randSeed1
export RANDSEEDb=$randSeed2
[ $forcingInterval -gt 0 ] && export FORCING_INTERVAL=$forcingInterval

# big loop thru dates
for centerFile in $initList; do

  centerBase=`basename $centerFile .nc`
  centerDate=`echo $centerBase | cut -c 12-30`

  export CENTER_DATE=$centerDate

  # strip out base name of forcing file template
  fNameRoot=`basename $forceCDL .cdl`

  forceArcDir=$forceArcRoot/$centerDate
  [ ! -d forceArcDir ] && mkdir -p $forceArcDir

  # if more than 0 perturbations, copy then call ncl script
  imember=1
  while [ $imember -le $ensSize ]; do

    cmember=$imember
    [ $imember -lt 1000 ] && cmember=0$cmember
    [ $imember -lt 100  ] && cmember=0$cmember
    [ $imember -lt 10   ] && cmember=0$cmember

    forcingFile=${fNameRoot}_${cmember}.nc
    ncgen -o $forcingFile $forceCDL

    # call NCL script
    export FORCE_FILE_NAME=$forcingFile
    export ENSEMBLE_MEMBER=$imember
    ncl < build_scm_forcing.ncl

    [ $? -eq 0 ] || (echo FAILED on ensemble member $imember && exit 1)

    # time interpolate if needed
    if [ $forcingInterval -gt 0 ]; then
      ncl < time_interpolate_forcing.ncl
      /bin/mv -f forcing_temp.nc $forcingFile
    fi
  
    [ $? -eq 0 ] || (echo FAILED on time interpolation for ensemble member $imember && exit 1)

    # rename and concatenate some of the output
    cat surface_init.txt > $forceArcDir/input_sounding_${cmember}
    cat profile_init.txt >> $forceArcDir/input_sounding_${cmember}
    #/bin/cp -f soil_init.txt $forceArcDir/input_soil_${cmember}
    /bin/mv -f $forcingFile $forceArcDir

    ((imember++))

  done

  # finally, create the unperturbed one
  forcingFile=${fNameRoot}.nc
  ncgen -o $forcingFile $forceCDL

  export FORCE_FILE_NAME=$forcingFile
  export ENSEMBLE_MEMBER=0

  ncl < build_scm_forcing.ncl

  [ $? -eq 0 ] || (echo FAILED on control && exit 1)

  # time interpolate if needed
  if [ $forcingInterval -gt 0 ]; then
    ncl < time_interpolate_forcing.ncl
    /bin/mv -f forcing_temp.nc $forcingFile
  fi

  [ $? -eq 0 ] || (echo FAILED on time interpolation for control && exit 1)
 
  # rename and concatenate some of the output
  cat surface_init.txt > $forceArcDir/input_sounding
  cat profile_init.txt >> $forceArcDir/input_sounding
  /bin/cp -f soil_init.txt $forceArcDir/input_soil
  /bin/mv -f $forcingFile $forceArcDir

done # big loop through init dates

echo SUCCESS
exit 0
