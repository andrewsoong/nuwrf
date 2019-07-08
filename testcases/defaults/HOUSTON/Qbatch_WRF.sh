#PBS -S /bin/csh
#PBS -N nuwrf
#PBS -l select=1:ncpus=24:mpiprocs=1:model=has+26:ncpus=24:mpiprocs=24:model=has
#PBS -l walltime=24:00:00
#PBS -W group_list=s1182
#PBS -m abe
#PBS -q long
#PBS -V
#PBS -e ./wrf_error.txt
#PBS -o ./wrf_run.txt

 module purge
 module load comp-intel/2013.5.192
 module load mpi-sgi/mpt

 set EXE = 'wrf.exe'
 echo $EXE
 set RUN_OPT_BEGIN = 'mpiexec -np 624'

 echo "${RUN_OPT_BEGIN} ./${EXE}"
 ${RUN_OPT_BEGIN} ./${EXE}

