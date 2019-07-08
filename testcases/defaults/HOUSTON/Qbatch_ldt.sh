#PBS -S /bin/csh
#PBS -N nuwrfldt
#PBS -l select=1:ncpus=1
#PBS -l walltime=00:10:00
#PBS -W group_list=s1182
#PBS -m abe
#PBS -q devel
#PBS -V
#PBS -e ./ldt_error.txt
#PBS -o ./ldt_run.txt

 set EXE = 'LDT ldt.config.postlis_2dom_p6'
 echo $EXE
 set RUN_OPT_BEGIN = 'mpiexec -np 1'

 echo "${RUN_OPT_BEGIN} ./${EXE}"
 ${RUN_OPT_BEGIN} ./${EXE} 

