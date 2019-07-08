#PBS -S /bin/csh
#PBS -N nuwrfreal
#PBS -l select=1:ncpus=24:mpiprocs=1:model=has+1:ncpus=24:mpiprocs=24:model=has
#PBS -l walltime=02:00:00
#PBS -W group_list=s1182
#PBS -m abe
#PBS -q devel
#PBS -V
#PBS -e ./real_error.txt
#PBS -o ./real_run.txt

# We completely purge the module environment variables and LD_LIBRARY_PATH 
# before loading only those specific variables that we need.
 module purge
 module load comp-intel/2013.5.192
 module load mpi-sgi/mpt

 set EXE = 'real.exe'
 echo $EXE
 set RUN_OPT_BEGIN = 'mpiexec -np 24'

 echo "${RUN_OPT_BEGIN} ./${EXE}"
 ${RUN_OPT_BEGIN} ./${EXE} 

