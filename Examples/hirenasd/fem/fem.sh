#!/usr/bin/env zsh 
set -e
setopt verbose

if [ ! -e hirenasd_aepw_fem_nov2011.op2 ]; then
  wget https://aeroelasticity.larc.nasa.gov/wp-content/uploads/sites/8/2019/10/hirenasd_aepw_fem_nov2011.zip
  unzip hirenasd_aepw_fem_nov2011.zip
  if [ -e hirenasd_aepw_fem_nov2011.op2 ]; then
    rm hirenasd_aepw_fem_nov2011.zip
  fi 
fi

cat << "EOF" > convert-op2-txt.mcr
#!MC 1410
#edit following line for the op2 filename
$!ReadDataSet  '"StandardSyntax" "1.0" "FEALoaderVersion" "443" "FILENAME_File" "hirenasd_aepw_fem_nov2011.op2" "AutoAssignStrandIDs" "No"'
  DataSetReader = 'MSC/NASTRAN Output2 (FEA)'
#undo tecplot's transformation
$!ALTERDATA
  EQUATION = '{X}={X}-{X Displacement}'
$!ALTERDATA
  EQUATION = '{Y}={Y}-{Y Displacement}'
$!ALTERDATA
  EQUATION = '{Z}={Z}-{Z Displacement}'
#rename coordinate, displacement and rotation variables for plotting (the locations of displacements and rotation can vary)
$!RENAMEDATASETVAR
  VAR = 1
  NAME = 'x'
$!RENAMEDATASETVAR
  VAR = 2
  NAME = 'y'
$!RENAMEDATASETVAR
  VAR = 3
  NAME = 'z'
$!RENAMEDATASETVAR
  VAR = 14
  NAME = 'f1'
$!RENAMEDATASETVAR
  VAR = 15
  NAME = 'f2'
$!RENAMEDATASETVAR
  VAR = 16
  NAME = 'f3'
$!RENAMEDATASETVAR
  VAR = 17
  NAME = 'f4'
$!RENAMEDATASETVAR
  VAR = 18
  NAME = 'f5'
$!RENAMEDATASETVAR
  VAR = 19
  NAME = 'f6'
$!VARSET |ZONES_TO_WRITE| = |NUMZONES|
$!VARSET |ZONE| = 1
$!LOOP |ZONES_TO_WRITE|
  $!SYSTEM "echo 'mode|LOOP%3.3d|.plt'"
  $!WriteDataSet  "mode|LOOP%3.3d|.plt"
    IncludeText = No
    IncludeGeom = No
    AssociateLayoutWithDataFile = No
    ZoneList =  [|ZONE|]
    VarPositionList =  [1-3,14-19] #verify variable position vary in your .op2 file
    Binary = Yes
    UsePointFormat = No
    Precision = 9
    TecplotVersionToWrite = Tecplot2006
  $!VARSET |ZONE| += 1 
$!ENDLOOP
$!VARSET |ZONE| = 1
$!LOOP |ZONES_TO_WRITE|
  $!SYSTEM "echo 'mode|LOOP%3.3d|.txt'"
  $!ExtendedCommand
    CommandProcessorID = 'excsv'
    Command = 'FrOp=1:ZnCount=1:ZnList=[|ZONE|]:VarCount=9:VarList=[1-3,14-19]:Form=[1e17.9,2e17.9,3e17.9,4e17.9,5e17.9,6e17.9,7e17.9,8e17.9,9e17.9,]:ValSep=" ":FNAME="mode|LOOP%3.3d|.txt"'
  $!VARSET |ZONE| += 1 
$!ENDLOOP
EOF

tec360 -quiet -b convert-op2-txt.mcr
