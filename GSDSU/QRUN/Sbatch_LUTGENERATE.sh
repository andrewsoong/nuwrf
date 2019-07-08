#!/bin/csh
#SBATCH -J GSDSU@LUT
#SBATCH -A s0942
#SBATCH -N 45 -n 1260 --ntasks-per-node=28
#SBATCH -t 0:05:00
#SBATCH --mail-type=ALL
#SBATCH -o gsdsu_lut_run.txt
#SBATCH -e gsdsu_lut_err.txt

 module purge
 module load comp/intel-13.0.1.117
 module load mpi/impi-5.0.2.044
# module load mpi/sgi-mpt-2.11

 set EXE = 'GSDSU.x'
 set RUN_OPT_BEGIN = 'mpirun -np 1260'

 echo "${RUN_OPT_BEGIN} ./${EXE}"
 ${RUN_OPT_BEGIN} ./${EXE}

