#!/bin/csh
#SBATCH -J GSDSU@RUN
#SBATCH -A s0942
#SBATCH -N 1 -n 27 --ntasks-per-node=27 --constraint=hasw
#SBATCH -t 3:00:00
#SBATCH --mail-type=ALL
#SBATCH -o gsdsu_run.txt
#SBATCH -e gsdsu_err.txt

 limit stacksize unlimited

 module purge
 module load comp/intel-13.0.1.117
 module load mpi/impi-5.0.2.044
# module load mpi/sgi-mpt-2.11

 set EXE = 'GSDSU.x'
 set RUN_OPT_BEGIN = 'mpirun -np 27'

 echo "${RUN_OPT_BEGIN} ./${EXE}"
 ${RUN_OPT_BEGIN} ./${EXE}

