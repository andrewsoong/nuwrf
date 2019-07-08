#!/usr/bin/env bash

# This script sets the path of the NU-WRF external libraries (i.e.
# zlib, netcdf4, hdf5, etc...) and the common modules (compilers) used
# by the external libraries and the NU-WRF build.
#
# NU-WRF porting notes:
# ---------------------
# If the user is attempting to port NU-WRF and "modules"(*) are not installed
# on their system then you need to make some changes in this file. Namely:
#
# 1) Remove "module" commands and references to them.
# 2) Make sure the compiler environment variables (look for export= in
#    this file) is set to a valid value on your system.
#
# If modules are installed, then just make sure path and names are
# correct
# 
# (*) http://modules.sourceforge.net/
#
for arg in "$@"; do
if [[ "$arg" = *"met"* ]];then 
   build_met="yes"
fi
done

get_vendors() {
   # LIBDIR_TAG path MUST have the form <some_path>/<COMPILER>-<MPI>
   local LIBDIR_TAG=$1
   libd=(${LIBDIR_TAG//\// })
   length=${#libd[@]}
   combo=${libd[length-1]}
   settings=(${combo//-/ })
   COMPILER_VENDOR=${settings[0]}
   MPI_VENDOR=${settings[1]}
}
set_vendors() {
   local LIBDIR_TAG=$1
   get_vendors $LIBDIR_TAG
   [[ "$COMPILER_VENDOR" == "gnu" ]] || [[ "$COMPILER_VENDOR" == "intel" ]]
   if [ $? != 0 ]; then
      echo "Invalid COMPILER_VENDOR: [$COMPILER_VENDOR]"
      echo "Run export COMPILER_VENDOR=x, where x=gnu or intel"
      exit
   fi
   [[ "${MPI_VENDOR}" == "sgimpt" ]] || [[ "${MPI_VENDOR}" == "openmpi" ]]  || [[ "${MPI_VENDOR}" == "mvapich2" ]] || [[ "${MPI_VENDOR}" == "intelmpi" ]]
   if [ $? != 0 ]; then
      echo "Invalid MPI_VENDOR: [$MPI_VENDOR]"
      echo "Run export MPI_VENDOR=x, where x=sgimpt, openmpi, mvapich2 or intelmpi"
      exit
   fi
}

MACHINE=$(uname -n)

#-------------
# Set defaults
#-------------

# LIBDIR: top location where third-party libraries are installed
# If LIBDIR is set then it will be used. Else use defaults below:
if [[ "$MACHINE" =~ "discover" || "$MACHINE" =~ "borg" ]]; then
   export platform="discover"
   [ -z "$LIBDIR" ] && LIBDIR_=/discover/nobackup/projects/nu-wrf/lib
   [ -z "$NUWRFDIR" ] && NUWRFDIR=/discover/nobackup/ccruz/devel/nu-wrf/code/nu-wrf
elif [[ "$MACHINE" =~ "pfe" ]]; then
   export platform="pleiades"
   [ -z "$LIBDIR" ] && LIBDIR_=/nobackupp8/nuwrf/lib
   [ -z "$NUWRFDIR" ] && NUWRFDIR=/nobackup/ccruz/code/nu-wrf.git
else
   echo "ERROR in $0:"
   echo " --> $MACHINE is not supported. To continue, replicate one of the "
   echo " --> working platform options making sure all the exported variables"
   echo " --> in this script are defined."
   exit 1
fi
[ -z "$LIBDIR" ] && echo "ENV: LIBDIR is not set. Use default installation on ${platform}"
LIBDIR=$LIBDIR_
echo "Using LIBDIR=$LIBDIR"

# LIBDIR_TAG: Specific installation (TAG) of third-party libraries
if [ -z "$LIBDIR_TAG" ]; then
   echo "ENV: LIBDIR_TAG is not set. Use default installation on ${platform}"
   if [ "${platform}" == "discover" ]; then
      if [ ! -z "$build_met" ]; then	   
         LIBDIR_TAG=${LIBDIR}/intel-sgimpt-bjerknes-p5
      else
         LIBDIR_TAG=${LIBDIR}/intel-intelmpi
      fi
   elif [ "${platform}" == "pleiades" ]; then
      LIBDIR_TAG=${LIBDIR}/intel-sgimpt
   fi
   get_vendors ${LIBDIR_TAG}
else
   set_vendors ${LIBDIR_TAG}
fi
echo "Using LIBDIR_TAG=$LIBDIR_TAG"
echo "Found COMPILER_VENDOR: $COMPILER_VENDOR"
echo "Found MPI_VENDOR     : $MPI_VENDOR"
export COMPILER_VENDOR
export MPI_VENDOR
export LIBDIR_TAG=$LIBDIR_TAG

# Set LD_LIBRARY_PATH for LIS/LDT/LVT
export LD_LIBRARY_PATH=$LIBDIR_TAG/fortrangis/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$LIBDIR_TAG/gdal/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$LIBDIR_TAG/jasper/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$LIBDIR_TAG/jpeg/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$LIBDIR_TAG/png/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$LIBDIR_TAG/netcdf4/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$LIBDIR_TAG/esmf/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$LIBDIR_TAG/hdf5/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$LIBDIR_TAG/hdf4/lib:$LD_LIBRARY_PATH
BASE_LLP=$LD_LIBRARY_PATH

#-------------------------
# Load environment modules
#-------------------------
source /usr/share/modules/init/bash
module purge
unset LD_LIBRARY_PATH

if [ "${platform}" == "discover" ]; then
   module load other/git-2.18.0
   module load other/cmake-3.8.2
   #module load other/python/GEOSpyD/Ana2019.03_py3.7
   # ** Note: Only needed with the default.cfg **
   # The following is a workaround. For some reason the Python dependency
   # generator which is part of the new LIS fails to produce the correct
   # dependencies when using the Python distributions ver 3.x.x.
   # So, I am hardwiring the path to a 2.7 version, which happens to
   # work as intended. 
   export PATH=/usr/local/other/SSSO_Ana-PyD/2.4.0_py2.7/bin:${PATH}
fi


if [ "$COMPILER_VENDOR" == "gnu" ]; then

   if [ "${platform}" == "discover" ]; then
      # For openmpi
      module load other/comp/gcc-6.3
   else
      echo "Option not available on machine $MACHINE"
      exit
   fi

   export CC=gcc
   export CXX=g++
   export FC=gfortran
   export F90=gfortran
   export F77=gfortran
   export CFLAGS="-fpic -O2"
   export FFLAGS="-fpic -O2"
   export CXXFLAGS="-fpic -O2"
   export ESMF_COMPILER=gfortran        
   if [ "$MPI_VENDOR" == "openmpi" ]; then
      module load other/mpi/openmpi/2.0.2-gcc-6.3
      export ESMF_COMM=openmpi
      export MPI_IMPLEMENTATION="OPENMPI"
      export mpi=openmpi
   elif [ "$MPI_VENDOR" == "sgimpt" ]; then
      module load mpi/sgi-mpt-2.16
      export ESMF_COMM=sgimpt
      export MPI_IMPLEMENTATION="SGI_MPT"
      export mpi=sgimpt
   elif [ "$MPI_VENDOR" == "mvapich2" ]; then
      module load other/mpi/mvapich2-2.1/gcc-4.9.2-sp3
      export ESMF_COMM=mvapich2
      export MPI_IMPLEMENTATION="MVAPICH2"
      export mpi=mvapich2
   else
      echo Illegal mpi choice
      usage
   fi
   export MPICC_CC=gcc
   export MPIF90_F90=gfortran
   export MPICXX_CXX=g++

   export NUWRF_FC=gfortran
   export compiler=gfortran
   export TRACEBACK_FLAG=-fbacktrace

elif [ "$COMPILER_VENDOR" == "intel" ]; then

   
   export CC=icc
   export CXX=icpc
   export FC=ifort
   export F90=ifort
   export F77=ifort
   if [ "${platform}" == "discover" ]; then
     if [ ! -z "$build_met" ]; then
        # Bjerknes-p5
        module load other/comp/gcc-5.3-sp3
        module load comp/intel-15.0.3.187
        module load lib/mkl-15.0.3.187
        module load mpi/sgi-mpt-2.12
     else
        if [ "$MPI_VENDOR" == "sgimpt" ]; then
           module load comp/intel-17.0.4.196
        elif [ "$MPI_VENDOR" == "intelmpi" ]; then
           module load other/comp/gcc-7.3
	   module load comp/intel-18.0.3.222
        fi
     fi
   else
     module load gcc/6.2
     module load comp-intel/2015.3.187
   fi
   if [ "$MPI_VENDOR" == "sgimpt" ]; then
      if [ "${platform}" == "discover" ]; then
         module load mpi/sgi-mpt-2.16
      else
         module load mpi-sgi/mpt
      fi
      export ESMF_COMM=sgimpt
      export MPICC_CC=icc
      export MPIF90_F90=ifort
      export MPICXX_CXX=icpc
      export MPI_IMPLEMENTATION="SGI_MPT"
   elif [ "$MPI_VENDOR" == "intelmpi" ]; then
      if [ "${platform}" == "discover" ]; then
	 module load mpi/impi-18.0.3.222
      else
         echo "Not available"
         exit
      fi
      export ESMF_COMM=intelmpi
      export MPI_IMPLEMENTATION="INTEL"
   else
      echo Illegal mpi choice
      usage
   fi
   
   export MPICC=mpicc
   export MPICXX=mpicxx
   export MPIF77=mpif90
   export MPIF90=mpif90
   export compiler=intel
   export TRACEBACK_FLAG=-traceback

   # When using baselibs.sh, the following are used by the ESMF build:
   export ESMF_COMPILER=intel
   
elif [ "$COMPILER_VENDOR" == "nag" ]; then
   
   module load comp/nag-6.1-6113
   module load other/comp/gcc-5.3-sp3
   export CC=gcc
   export CXX=g++
   export FC=nagfor
   export F90=nagfor
   export F77=nagfor
   export CFLAGS="-fpic -O2"
   export FFLAGS="-fpic -O2 -free -kind=byte -f2003"
   export CXXFLAGS="-fpic -O2"
   export ESMF_COMPILER=nag
   if [ "$MPI_VENDOR" == "mvapich2" ]; then
      module load other/mpi/mvapich2-2.2/nag-6.1-6113-gcc-5.3
      export ESMF_COMM=mvapich2
   else
      echo Illegal mpi choice
      usage
   fi
   
elif [ "$COMPILER_VENDOR" == "pgi" ]; then
   
   module load comp/pgi-16.9.0
   export CC=pgcc
   export CXX=pgc++
   export FC=pgf90
   export F90=pgf90
   export F77=pgf77
   export CFLAGS="-fpic -O2"
   export FFLAGS="-fpic -O2 -Mpreprocess -Mbyteswapio"
   export CXXFLAGS="-fpic -O2"
   export ESMF_COMPILER=pgi
   if [ "$MPI_VENDOR" == "openmpi" ]; then
      module load other/mpi/openmpi/2.0.1-pgi-16.9.0
      export ESMF_COMM=openmpi
   else
      echo Illegal mpi choice
      usage
   fi
   
else
   
   echo Illegal compiler choice
   usage
   
fi

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$BASE_LLP:/usr/lib64
rm -f $NUWRFDIR/.modules
module list 1> $NUWRFDIR/.modules 2>&1
