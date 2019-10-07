#!/usr/bin/env bash
function tecplot() {
#edit path or load appropriate module
  OIFS="$IFS"
  IFS=$'\n'
  if [ $(uname) = 'Linux' ]; then
    exe='tec360'
  else
    exe="/Applications/Tecplot 360 EX 2018 R1/Tecplot 360 EX 2018 R1.app/Contents/MacOS/Tecplot 360 EX 2018 R1"
  fi
  $exe $*
  rm -f tecplot.phy
  IFS="$OIFS"
}
export -f tecplot

tecplot -quiet -b convert-op2-txt.mcr

