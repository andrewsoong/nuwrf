#!/bin/sh
#PBS -S /bin/sh
#PBS -N ce
#PBS -l select=1:ncpus=1:mpiprocs=1:model=bro
#PBS -l walltime=0:10:00
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
# SCRIPT:  run_convert_emiss.pleiades.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NASA SSSO/SSAI
#                                                                              
# DESCRIPTION:                                                                 
# Sample batch script for running convert_emiss.exe on NASA ARC Pleiades
# supercomputer with PBS.
#
#------------------------------------------------------------------------------

#Customize for number of domains
numDomains=1

# When a batch script is started, it starts in the user's home directory.
# Change to the directory where job was submitted.
if [ ! -z $PBS_O_WORKDIR ] ; then
    cd $PBS_O_WORKDIR || exit 1
fi

# Load config file for modules and paths
source ./config.pleiades.sh || exit 1

# Go to work directory and make sure namelist.input is present.
if [ -z "$WORKDIR" ] ; then
    echo "ERROR, WORKDIR is not defined!"
    exit 1
fi
cd $WORKDIR || exit 1

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
        echo "ERROR, $file does not exist!"
        exit 1
    fi
done
cd $WORKDIR || exit 1

# For running with 2014 Goddard radiation
rm $WORKDIR/GODDARDRAD_SSLUT
ln -fs $NUWRFDIR/WRFV3/GODDARDRAD_SSLUT $WORKDIR/GODDARDRAD_SSLUT || exit 1
if [ ! -e $WORKDIR/GODDARDRAD_SSLUT ] ; then
    echo "ERROR, $WORKDIR/GODDARDRAD_SSLUT does not exist!"
    exit 1
fi

# Move namelist.input file if it already exists. We will replace it.
if [ -e namelist.input ] ; then
    mv namelist.input namelist.input.old || exit 1
fi

# Loop through each domain. convert_emiss only processes a single domain, so
# creative renaming is necessary to process multiple domains.
for domain in `seq 1 $numDomains` ; do

    echo "Processing domain ${domain}"

    if [ $domain -lt 10 ] ; then
        d=d0${domain}
    else
        d=d${domain}
    fi
    g=g${domain}
    
    if [ ! -e namelist.input.convert_emiss.${d} ] ; then
        echo "ERROR, namelist.input.convert_emiss.${d} not found!"
        exit 1
    fi
    cp namelist.input.convert_emiss.${d} namelist.input || exit 1

    if [ ! -e wrfinput_${d} ] ; then
        echo "ERROR, wrfinput_${d} does not exist!"
        exit 1
    fi

    mv wrfinput_${d} wrfinput_${d}.actual || exit 1
    ln -s wrfinput_${d}.actual wrfinput_d01 || exit 1

    # Symbolically link emissions files.
    for link in emissopt3_d01 emissfire_d01 wrf_gocart_backg ; do
    
	if [ $link = "emissopt3_d01" ] ; then
            abbrev="ab"
	elif [ $link = "emissfire_d01" ] ; then
            abbrev="bb"
	elif [ $link = "wrf_gocart_backg" ] ; then
            abbrev="gocartBG"
	else
            echo "Internal logic error, unknown symlink $link"
            exit 1
	fi

	# Create the symbolic link.
        # FIXME: Need better way of doing this.
        numfiles=`ls -l *${g}-${abbrev}.bin | wc -l`
        if [ $numfiles -ne 1 ] ; then
            echo $numfiles
            echo "ERROR, found multiple -${g}-${abbrev}.bin files!"
            echo "Do you really need a $link file?"
            exit 1
        fi
        targets=`ls *${g}-${abbrev}.bin`
        for target in $targets ; do
            ln -fs $target $link || exit 1
            if [ ! -e $link ] ; then
                echo "ERROR, $link does not exist!"
                exit 1
            fi  
        done    
    done    

    # Run convert_emiss
    # WARNING: This program only supports a single process even when compiled
    # with MPI.
    ln -fs $NUWRFDIR/WRFV3/chem/convert_emiss.exe convert_emiss.exe || exit 1
    if [ ! -e convert_emiss.exe ] ; then
	echo "ERROR, convert_emiss.exe does not exist!"
	exit 1
    fi	

    # Different MPI launch commands for different implementations.
    if [ "$USING_SGI_MPT" -eq 1 ] ; then	
	mpiexec -np 1 ./convert_emiss.exe || exit 1 
    else
	echo "ERROR, unknown MPI implementation, don't know how to launch!"
	exit 1
    fi

    # Remove symbolic link
    rm wrfinput_d01 || exit 1
    rm namelist.input || exit 1

    # Rename the output files to prevent overwriting from different grid
    if [ -e wrfbiochemi_d01 ] ; then
        mv wrfbiochemi_d01 wrfbiochemi_${d}.actual || exit 1
    fi
    if [ -e wrfchemi_d01 ] ; then
        mv wrfchemi_d01 wrfchemi_${d}.actual || exit 1
    fi
    if [ -e wrfchemi_gocart_bg_d01 ] ; then
        mv wrfchemi_gocart_bg_d01 wrfchemi_gocart_bg_${d}.actual || exit 1
    fi
    if [ -e wrffirechemi_d01 ] ; then
        mv wrffirechemi_d01 wrffirechemi_${d}.actual || exit 1
    fi

    # Move the RSL log files.
    if [ ! -e $WORKDIR/convert_emiss_logs ] ; then
        mkdir $WORKDIR/convert_emiss_logs || exit 1
    fi
    rsl_files=`ls rsl.*`
    for file in $rsl_files ; do
        mv $file $WORKDIR/convert_emiss_logs/${file}.${d} || exit 1
    done

done

# Restore the original file names
for domain in `seq 1 $numDomains` ; do
    if [ $domain -lt 10 ] ; then
        d=d0${domain}
    else
        d=d${domain}
    fi
    if [ -e wrfinput_${d}.actual ] ; then
        mv wrfinput_${d}.actual wrfinput_${d} || exit 1
    fi
    if [ -e wrfbiochemi_${d}.actual ] ; then
        mv wrfbiochemi_${d}.actual wrfbiochemi_${d} || exit 1
    fi
    if [ -e wrfchemi_${d}.actual ] ; then
        mv wrfchemi_${d}.actual wrfchemi_${d} || exit 1
    fi
    if [ -e wrfchemi_gocart_bg_${d}.actual ] ; then
        mv wrfchemi_gocart_bg_${d}.actual wrfchemi_gocart_bg_${d} || exit 1
    fi
    if [ -e wrffirechemi_${d}.actual ] ; then
        mv wrffirechemi_${d}.actual wrffirechemi_${d} || exit 1
    fi
done

# The end
exit 0

