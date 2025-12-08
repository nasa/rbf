#!/usr/bin/env bash
module purge
module load intel/2019.5.281
module load tecplot/2025R1
module list >& modules.log
exe_type=ifort
rm -f make.inc
ln -s make.inc.$exe_type make.inc
make clean
make 
