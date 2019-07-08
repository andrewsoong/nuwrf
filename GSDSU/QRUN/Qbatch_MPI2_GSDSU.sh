#PBS -S /bin/csh
#PBS -N gsdsu
#PBS -l select=4:ncpus=27:model=bro
####PBS -l select=1:ncpus=24:mpiprocs=1:model=bro+4:ncpus=12:mpiprocs=24:model=bro
#PBS -l walltime=02:00:00
#PBS -W group_list=s1183
#PBS -m abe
#PBS -q devel
#PBS -V
#PBS -e ./gsdsu_error.txt
#PBS -o ./gsdsu_run.txt

 rm gsdsu_error.txt
 rm gsdsu_run.txt

 module purge
 module load comp-intel/2016.2.181
 module load mpi-sgi/mpt.2.15r20 # up-to-date MPT module

 set EXE = 'GSDSU.x'
 echo $EXE
 set RUN_OPT_BEGIN = 'mpiexec -np 108'

 echo "${RUN_OPT_BEGIN} ./${EXE}"
 ${RUN_OPT_BEGIN} ./${EXE} 

