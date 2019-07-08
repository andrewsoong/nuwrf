#PBS -S /bin/csh
#PBS -N nuwrflis
#PBS -l select=10:ncpus=24
#PBS -l walltime=20:00:00
#PBS -W group_list=s1182
#PBS -m abe
#PBS -q long
#PBS -V
#PBS -e ./lis_error.txt
#PBS -o ./lis_run.txt

# We completely purge the module environment variables and LD_LIBRARY_PATH 
# before loading only those specific variables that we need.
 module purge
 module load comp-intel/2013.5.192
 module load mpi-sgi/mpt

# Make sure stacksize is unlimited
 ulimit -S unlimited

 set EXE = './LIS'
 echo $EXE
 set RUN_OPT_BEGIN = 'mpiexec -np 240'

 echo "${RUN_OPT_BEGIN} ./${EXE}"
 ${RUN_OPT_BEGIN} ./${EXE} 

