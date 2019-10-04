#!/usr/bin/env bash
module purge
#module load intel_2019.1.144
module load intel_2019.2.187 
module load tecplot360ex-2018R2_64 
module list >& modules.log
exe_type=ifort
rm -f make.inc
ln -s make.inc.$exe_type make.inc
make clean
make 
