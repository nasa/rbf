#!/usr/bin/env bash

set -e

# download grid and run fun3d to write out modal cfd surface points
cd grid
./grid.sh
cd ..

# download fem and process with tecplot (or your own code) to write input for rbf 
cd fem
./fem.sh
cd ..

# Run rbf to interpolate fem modes to cfd surface.
cd modes
./modes.sh
cd ..

# plot modes on FEM, source points used, and final modes on CFD mesh
cd plot
./plot.sh
