#!/usr/bin/env zsh 
set -e 
setopt verbose

if [ ! -e aepw_coarse_tets_nc.cogsg ]; then
  wget https://c3.nasa.gov/dashlink/static/media/other/Coarse_tets_nc.tar.gz
  tar -zxvf Coarse_tets_nc.tar.gz
  rm aepw_coarse_tets_nc.cgns Coarse_tets_nc.tar.gz
fi

cat << "EOF" > run.pbs
#!/bin/bash
#PBS -W group_list=a1606
#PBS -N hirenasd
#PBS -r n
#PBS -j oe
#PBS -l select=4:ncpus=28:mpiprocs=28:model=bro
#PBS -q devel
#PBS -l walltime=1:00:00

cd $PBS_O_WORKDIR

export MPI_SHEPHERD=true
unset F_UFMTENDIAN

model=$(qstat -f $PBS_JOBID |grep Resource_List.select | sed 's/=/ /g' | awk '{ print $NF }')

module purge
module load tecplot/2018r1 mpi-hpe/mpt.2.17r13 comp-intel/2018.3.222 Suggar++/2.5.1_mpt-2.17r13_ifort-2018.3.222
if [ $model == "sky_ele" ]; then
  module load FUN3D/13.5_Skylake
else
  module load FUN3D/13.5
fi

mpiexec nodet_mpi --write_massoud_file --no_restart &> fun3d.out

mv aepw_coarse_tets_nc_massoud_body1.dat all.dat
mv aepw_coarse_tets_nc_massoud_body2.dat wing.dat
EOF

cat << "EOF" > fun3d.nml
&project
  project_rootname = 'aepw_coarse_tets_nc'
/

&raw_grid
  grid_format = 'vgrid'
/

&massoud_output
  n_bodies = 2
  nbndry(1) = 20 ! all
  boundary_list(1) = '1-12,19-26' !all
  nbndry(2) = 14 ! wing
  boundary_list(2) = '7-12,19-26' !wing
/

&code_run_control
  steps = 1
  restart_read = 'off'
/

&reference_physical_properties
  mach_number = 0.5
  reynolds_number = 100000
/

&grid_transform
  n_transforms = 1
  scale(:) = 0.001
/
EOF

qsub run.pbs

