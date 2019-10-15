#!/bin/bash
#PBS -W group_list=a1606
#PBS -N rbf
#PBS -r n
#PBS -j oe
#PBS -l select=1:ncpus=28:mpiprocs=28:model=bro
#PBS -q devel
#PBS -l walltime=2:00:00

# This can be run from the commmand line or in PBS

if [ ! -z $PBS_O_WORKDIR ]; then
  cd $PBS_O_WORKDIR
fi

# edit paths below
export project='aepw_coarse_tets_nc'
export cfd=../grid/all.dat   # full body surface
export wing=../grid/wing.dat # primary surface to be interpolated
export nk=1000 # for very small FEM use, pk=100 for 100% of nodes
               # runtime about an hour for nk=4000

function run() {
  modenum=$(echo $1 |sed 's/[^0-9]//g')
  modenumfun=$(echo $modenum | sed 's/^0*//') #remove leading zeros for fun3d
  modefile=$(printf "%s_body1_mode%d.dat" $project $modenumfun)
  source_pointsfile=source_points_${modenumfun}.dat
  rbf -s $1 -d $cfd -p $wing -b 0.05 -iz -i $modefile -nk $nk -wp $source_pointsfile >& $modenum.out
}
export -f run

####################################################################

if [ ! -e fem ]; then
  ln -s ../fem .
fi

while [ ! -f $cfd ] || [ ! -f $wing ]
do
  echo waiting for $cfd and $wing
  sleep 10
done

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
