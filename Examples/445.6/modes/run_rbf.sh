#!/bin/bash
#PBS -W group_list=e1830
#PBS -N rbf
#PBS -r n
#PBS -j oe
#PBS -m abe
#PBS -l select=1:ncpus=28:mpiprocs=28:model=bro
#PBS -q devel
#PBS -l walltime=2:00:00

# This can be run from the commmand line or in PBS
# edit paths below

export project=foo
export cfd=../grid/foo_massoud_body1.dat
#export rbf=/home3/sjmassey/Source/rbf/2019.10.02/rbf
export rbf=rbf
#for large FEM try using -nk option with 500 to 2000 instead of -pk 
export pk=100


function run() {
  modenum=$(echo $1 |sed 's/[^0-9]//g')
  modenumfun=$(echo $modenum | sed 's/^0*//') #remove leading zeros for fun3d
  modefile=$(printf "%s_body1_mode%d.dat" $project $modenumfun)
  source_pointsfile=source_points_${modenumfun}.dat
  echo "$rbf -s $1 -d $cfd -i $modefile -pk $pk -wp $source_pointsfile >& $modenum.out"
  $rbf -s $1 -d $cfd -i $modefile -pk $pk -wp $source_pointsfile >& $modenum.out
}
export -f run

####################################################################

if [ ! -e fem ]; then
  ln -s ../fem .
fi

parallel_exe=$(which parallel 2>/dev/null)
if [ -z $parallel_exe ]; then
  echo '***********  Recommend installing gnu parallel. ************'
  for f in fem/mode*.txt
  do
    run $f
  done
else
  ls fem/mode*.txt | SHELL=/bin/bash parallel run
fi

# to plot
#./fem-rbf.sh
