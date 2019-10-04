#!/bin/bash
export dir
dir="."

function tecplot() {
  OIFS="$IFS"
  IFS=$'\n'
  if [ -e title.txt ]; then
    export title=$(cat title.txt)
  fi
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

function lay2png() {
  for file
  do
    if [ -z $expout ]; then
      expout=$(basename $file .lay).png
    fi
    if [ -z $title ]; then
      export title=$(basename $file .lay)
    fi
    echo "converting $file to $expout"          
  mytmpfile=$(openssl rand -hex 6).mcr
  cat << EOF > $mytmpfile
#!MC 1200
\$!OPENLAYOUT  "$file"
\$!EXPORTSETUP EXPORTREGION = ALLFRAMES
\$!EXPORTSETUP EXPORTFORMAT = PNG
\$!EXPORTSETUP IMAGEWIDTH = 1920
\$!EXPORTSETUP USESUPERSAMPLEANTIALIASING = YES
\$!EXPORTSETUP EXPORTFNAME = '$expout'
\$!EXPORT
  EXPORTREGION = ALLFRAMES
\$!Quit
EOF
    tecplot -quiet -b $mytmpfile 
    rm -f $mytmpfile 
  done
}
export -f lay2png

function plot() {
  export expout
  export infile
  export modelz
  mode=$(echo $1| sed 's/_/ /g' | awk '{ print $NF }' | sed 's/[^0-9]//g') #without leading zeros
  modelz=$(printf "%03d" $mode) #with leading zeros
  infile="fem/mode${modelz}.plt $1 $dir/source_points_$mode.dat"
  expout=${modelz}.png
  lay2png fem-rbf.lay 
}
export -f plot

###########################################################################

parallel_exe=$(which parallel 2>/dev/null)
if [ -z $parallel_exe ]; then
  echo 'Recommend installing gnu parallel.'
  for f in $dir/*body1_mode*.dat
  do
    plot $f
  done
else
  ls $dir/*body1_mode*.dat | SHELL=/bin/bash parallel -j 6 plot
fi

