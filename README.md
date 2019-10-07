# rbf
Mode shape interpolation via radial basis functions

rbf Version: 2.3.0-2019.10.02

 Purpose:  Interpolate mode shapes from FEM modes to CFD surface.
   Usage:  rbf -s fem_mode_shape -d cfd_mesh -i cfd_mode_shape [options] 
 Options:  
           -s  source_fem_mode_shape_file
           -d  destination_mesh_file
           -i  interpolated_mode_shape_file
           -iz ignore points whose values are zero
           -nk number of source (fem) nodes to keep
           -pk percent of source (fem) nodes to keep
           -wp write fem_source_points for debugging
           -p  primary_surface_file
           -b  radial_blend_distance_in_grid_units for use with -p
           -x  xsym_blend_distance_in_grid_units
           -y  ysym_blend_distance_in_grid_units
           -z  zsym_blend_distance_in_grid_units
           -cs compute spring connectivity from rbf.nml
Requires:  
          1) tecplot formatted fem mode shapes with 
             variables x,y,z,id,f1,f2,f3,f4,f5,f6
Examples:  
           rbf -s fem/mode001.plt -d project_ddfdrive_body1.dat \
             -i project_body1_mode1.dat -nk 250 -y 4
           rbf -s fem/mode001.dat -d ddfdrive_allsurf.dat \
             -i project_body1_mode1.dat -p ddfdrive_wing_tail.dat -b 3 -pk 25
   Built:  Fri Oct  4 11:20:17 PDT 2019 on Linux 4.12.14-95.19.1.20190617-nasa
 Version:  2.3.0 (2019.10.02)
   About:  Steven.J.Massey@nasa.gov 
 
