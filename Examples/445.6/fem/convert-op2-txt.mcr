#!MC 1410
#edit following line for the op2 filename
$!ReadDataSet  '"StandardSyntax" "1.0" "FEALoaderVersion" "443" "FILENAME_File" "Agard445.6_weak.op2" "AutoAssignStrandIDs" "No"'
  DataSetReader = 'MSC/NASTRAN Output2 (FEA)'
#rename variables for plotting
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
  VAR = 5
  NAME = 'f1'
$!RENAMEDATASETVAR
  VAR = 6
  NAME = 'f2'
$!RENAMEDATASETVAR
  VAR = 7
  NAME = 'f3'
$!RENAMEDATASETVAR
  VAR = 8
  NAME = 'f4'
$!RENAMEDATASETVAR
  VAR = 9
  NAME = 'f5'
$!RENAMEDATASETVAR
  VAR = 10
  NAME = 'f6'
$!ALTERDATA
  EQUATION = 'V4=V11'
$!RENAMEDATASETVAR
  VAR = 4 
  NAME = 'id'
#undo tecplot's transformation
$!ALTERDATA
  EQUATION = '{x}={x}-{f1}'
$!ALTERDATA
  EQUATION = '{y}={y}-{f2}'
$!ALTERDATA
  EQUATION = '{z}={z}-{f3}'
$!VARSET |ZONES_TO_WRITE| = |NUMZONES|
$!VARSET |ZONE| = 1
$!LOOP |ZONES_TO_WRITE|
  $!SYSTEM "echo 'mode|LOOP%3.3d|.plt'"
  $!WRITEDATASET  "mode|LOOP%3.3d|.plt"
    INCLUDETEXT = NO
    INCLUDEGEOM = NO
    INCLUDEDATASHARELINKAGE = YES
    ZONELIST =  [|ZONE|]
    VARPOSITIONLIST =  [1-10]
    BINARY = YES
    USEPOINTFORMAT = NO
    PRECISION = 9
    TECPLOTVERSIONTOWRITE = TECPLOTCURRENT
  $!VARSET |ZONE| += 1 
$!ENDLOOP
$!DeleteVars  [4] #id not used by rbf, but kept above for backwards compatibility
$!VARSET |ZONE| = 1
$!LOOP |ZONES_TO_WRITE|
  $!SYSTEM "echo 'mode|LOOP%3.3d|.txt'"
  $!ExtendedCommand
    CommandProcessorID = 'excsv'
    Command = 'FrOp=1:ZnCount=1:ZnList=[|ZONE|]:VarCount=9:VarList=[1-9]:Form=[1e17.9,2e17.9,3e17.9,4e17.9,5e17.9,6e17.9,7e17.9,8e17.9,9e17.9,]:ValSep=" ":FNAME="mode|LOOP%3.3d|.txt"'
  $!VARSET |ZONE| += 1 
$!ENDLOOP
