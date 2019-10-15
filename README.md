# rbf
Mode shape interpolation via radial basis functions

See Examples/hirenasd for scripts showing all steps required to prepare FEM modes for use in FUN3D. The scripts will have to be adjusted for your PBS envirnoment. Also make that fun3d and tecplot are in your path. 

The following help is shown by typing ./rbf without any arguments.
<pre>
rbf Version: 2.3.0-2019.10.02

 Purpose:  Interpolate mode shapes from FEM modes to CFD surface.
   Usage:  ./rbf -s fem_mode_shape -d cfd_mesh -i cfd_mode_shape [options] 
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
          1) ascci formatted fem mode shapes with 
             variables x,y,z,f1,f2,f3,f4,f5,f6
Examples:  
           ./rbf -s fem/mode001.txt -d project_ddfdrive_body1.dat \
             -i project_body1_mode1.dat -nk 250 -y 4
           ./rbf -s fem/mode001.txt -d ddfdrive_allsurf.dat \
             -i project_body1_mode1.dat -p ddfdrive_wing_tail.dat -b 3 -pk 25
   Built:  Fri Oct  4 11:20:17 PDT 2019 on Linux 4.12.14-95.19.1.20190617-nasa
 Version:  2.3.0 (2019.10.02)
   About:  Steven.J.Massey@nasa.gov 
</pre>
Notices:
Copyright 2019 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. No copyright is claimed in the United States under Title 17, U.S. Code. All Other Rights Reserved.
 
The following license and copyright notices govern the noted 3rd party software package KDTREE2  included in the NASA SOFTWARE:
 
The KDTREE2 software is licensed under the terms of the Academic Free Software License, listed herein.  In addition, users of this software must give appropriate citation in relevant technical documentation or journal paper to the author, Matthew B. Kennel, Institute For Nonlinear Science, preferably via a reference to the www.arxiv.org repository of this document, {\tt www.arxiv.org e-print: physics/0408067}.  This requirement will be deemed to be advisory and not mandatory as is necessary to permit the free inclusion of the present software with any software licensed under the terms of any version of the GNU General Public License, or GNU Library General Public License.
 
Academic Free License
Version 1.1
 
This Academic Free License applies to any original work of authorship (the "Original Work") whose owner (the "Licensor") has placed the following notice immediately following the copyright notice for the Original Work: "Licensed under the Academic Free License version 1.1."
 
Grant of License. Licensor hereby grants to any person obtaining a copy of the Original Work ("You") a world-wide, royalty-free, non-exclusive, perpetual, non-sublicenseable license (1) to use, copy, modify, merge, publish, perform, distribute and/or sell copies of the Original Work and derivative works thereof, and (2) under patent claims owned or controlled by the Licensor that are embodied in the Original Work as furnished by the Licensor, to make, use, sell and offer for sale the Original Work and derivative works thereof, subject to the following conditions.
 
Right of Attribution. Redistributions of the Original Work must reproduce all copyright notices in the Original Work as furnished by the Licensor, both in the Original Work itself and in any documentation and/or other materials provided with the distribution of the Original Work in executable form.
 
Exclusions from License Grant. Neither the names of Licensor, nor the names of any contributors to the Original Work, nor any of their trademarks or service marks, may be used to endorse or promote products derived from this Original Work without express prior written permission of the Licensor.
 
WARRANTY AND DISCLAIMERS. LICENSOR WARRANTS THAT THE COPYRIGHT IN AND TO THE ORIGINAL WORK IS OWNED BY THE LICENSOR OR THAT THE ORIGINAL WORK IS DISTRIBUTED BY LICENSOR UNDER A VALID CURRENT LICENSE FROM THE COPYRIGHT OWNER. EXCEPT AS EXPRESSLY STATED IN THE IMMEDIATELY
PRECEEDING SENTENCE, THE ORIGINAL WORK IS PROVIDED UNDER THIS LICENSE ON AN "AS IS" BASIS, WITHOUT WARRANTY, EITHER EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, THE WARRANTY OF NON-INFRINGEMENT AND WARRANTIES THAT THE ORIGINAL WORK IS MERCHANTABLE OR FIT FOR A
PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY OF THE ORIGINAL WORK IS WITH YOU. THIS DISCLAIMER OF WARRANTY CONSTITUTES AN ESSENTIAL PART OF THIS LICENSE. NO LICENSE TO ORIGINAL WORK IS GRANTED HEREUNDER EXCEPT UNDER THIS DISCLAIMER.
 
LIMITATION OF LIABILITY. UNDER NO CIRCUMSTANCES AND UNDER NO LEGAL THEORY, WHETHER TORT (INCLUDING NEGLIGENCE), CONTRACT, OR OTHERWISE, SHALL THE LICENSOR BE LIABLE TO ANY PERSON FOR ANY DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES OF ANY CHARACTER ARISING
AS A RESULT OF THIS LICENSE OR THE USE OF THE ORIGINAL WORK INCLUDING, WITHOUT LIMITATION, DAMAGES FOR LOSS OF GOODWILL, WORK STOPPAGE, COMPUTER FAILURE OR MALFUNCTION, OR ANY AND ALL OTHER COMMERCIAL DAMAGES OR LOSSES, EVEN IF SUCH PERSON SHALL HAVE BEEN INFORMED OF THE
POSSIBILITY OF SUCH DAMAGES. THIS LIMITATION OF LIABILITY SHALL NOT APPLY TO LIABILITY FOR DEATH OR PERSONAL INJURY RESULTING FROM SUCH PARTY'S NEGLIGENCE TO THE EXTENT APPLICABLE LAW PROHIBITS SUCH LIMITATION. SOME JURISDICTIONS DO NOT ALLOW THE EXCLUSION OR LIMITATION OF INCIDENTAL OR CONSEQUENTIAL DAMAGES, SO THIS EXCLUSION AND LIMITATION MAY NOT APPLY TO YOU.
 
License to Source Code. The term "Source Code" means the preferred form of the Original Work for making modifications to it and all available documentation describing how to access and modify the Original Work. Licensor hereby agrees to provide a machine-readable copy of the Source Code of the Original Work along with each copy of the Original Work that Licensor distributes. Licensor reserves the right to satisfy this obligation by placing a machine-readable copy of the Source Code in an information repository reasonably calculated to permit inexpensive and convenient access by You for as long as Licensor continues to distribute the Original Work, and by publishing the address of that information repository in a notice immediately following the copyright notice that applies to the Original Work.
 
Mutual Termination for Patent Action. This License shall terminate automatically and You may no longer exercise any of the rights granted to You by this License if You file a lawsuit in any court alleging that any OSI Certified open source software that is licensed under any license containing this "Mutual Termination for Patent Action" clause infringes any patent claims that are essential to use that software.
 
This license is Copyright (C) 2002 Lawrence E. Rosen. All rights reserved. Permission is hereby granted to copy and distribute this license without modification. This license may not be modified without at the express written permission of its copyright owner.
 
Disclaimers
No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS." 
 
Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
