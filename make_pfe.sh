#!/usr/bin/env bash
# Get module function defined
ENV=/usr/local/lib/global.kshrc; export ENV 
. $ENV          
module purge
#module load comp-intel/2018.0.128 this module doesn't work with tecplot
module load comp-intel/2018.3.222
module load tecplot/2018r1
module list >& modules.log
exe_type=ifort
rm -f make.inc
ln -s make.inc.$exe_type make.inc
make clean
make 
rm make.inc
