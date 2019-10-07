#!/usr/bin/env bash
if [ $(hostname | cut -c 1-3) = "pfe" ]; then
  echo pfe
  make_pfe.sh
  cp rbf ~/bin
  cp -a Examples/445.6 445.6.test
  cd 445.6.test/fem
  ./runme.sh
  cd ../modes
  run_rbf.sh
  cd ../..
  for f in 445.6.test/modes/*.dat
  do
    b=$(basename $f)
    diff -s $f /home3/sjmassey/Source/rbf/Examples/445.6/modes.gold/$b
  done
else
  echo not pfe
  exit
fi
