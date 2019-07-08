umask 022
ulimit -s unlimited
. /usr/share/modules/init/bash
module purge
unset LD_LIBRARY_PATH
module load other/comp/gcc-6.3
module load comp/intel-17.0.4.196
module load lib/mkl-17.0.4.196
module load mpi/sgi-mpt-2.16
module load other/SSSO_Ana-PyD/SApd_4.2.0_py3.5
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib64

# NUWRFDIR specifies the location of the NU-WRF source code
NUWRFDIR=<CHANGE THIS>
# WORKDIR specifies the location of the temporary directory
WORKDIR=<CHANGE THIS>

