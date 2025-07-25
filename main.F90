! Notices:
! Copyright 2019 United States Government as represented by the Administrator 
! of the National Aeronautics and Space Administration. No copyright is 
! claimed in the United States under Title 17, U.S. Code. All Other Rights 
! Reserved.
!  
! The following license and copyright notices govern the noted 3rd party 
! software package KDTREE2  included in the NASA SOFTWARE:
!  
! The KDTREE2 software is licensed under the terms of the Academic Free 
! Software License, listed herein.  In addition, users of this software must 
! give appropriate citation in relevant technical documentation or journal 
! paper to the author, Matthew B. Kennel, Institute For Nonlinear Science, 
! preferably via a reference to the www.arxiv.org repository of this document, 
! {\tt www.arxiv.org e-print: physics/0408067}.  This requirement will be 
! deemed to be advisory and not mandatory as is necessary to permit the free 
! inclusion of the present software with any software licensed under the terms 
! of any version of the GNU General Public License, or GNU Library General 
! Public License.
!  
! Academic Free License
! Version 1.1
!  
! This Academic Free License applies to any original work of authorship (the 
! "Original Work") whose owner (the "Licensor") has placed the following 
! notice immediately following the copyright notice for the Original Work: 
! "Licensed under the Academic Free License version 1.1."
!  
! Grant of License. Licensor hereby grants to any person obtaining a copy of 
! the Original Work ("You") a world-wide, royalty-free, non-exclusive, 
! perpetual, non-sublicenseable license (1) to use, copy, modify, merge, 
! publish, perform, distribute and/or sell copies of the Original Work and 
! derivative works thereof, and (2) under patent claims owned or controlled by 
! the Licensor that are embodied in the Original Work as furnished by the 
! Licensor, to make, use, sell and offer for sale the Original Work and 
! derivative works thereof, subject to the following conditions.
!  
! Right of Attribution. Redistributions of the Original Work must reproduce 
! all copyright notices in the Original Work as furnished by the Licensor, 
! both in the Original Work itself and in any documentation and/or other 
! materials provided with the distribution of the Original Work in executable 
! form.
!  
! Exclusions from License Grant. Neither the names of Licensor, nor the names 
! of any contributors to the Original Work, nor any of their trademarks or 
! service marks, may be used to endorse or promote products derived from this 
! Original Work without express prior written permission of the Licensor.
!  
! WARRANTY AND DISCLAIMERS. LICENSOR WARRANTS THAT THE COPYRIGHT IN AND TO THE 
! ORIGINAL WORK IS OWNED BY THE LICENSOR OR THAT THE ORIGINAL WORK IS 
! DISTRIBUTED BY LICENSOR UNDER A VALID CURRENT LICENSE FROM THE COPYRIGHT 
! OWNER. EXCEPT AS EXPRESSLY STATED IN THE IMMEDIATELY PRECEEDING SENTENCE, 
! THE ORIGINAL WORK IS PROVIDED UNDER THIS LICENSE ON AN "AS IS" BASIS, 
! WITHOUT WARRANTY, EITHER EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, 
! THE WARRANTY OF NON-INFRINGEMENT AND WARRANTIES THAT THE ORIGINAL WORK IS 
! MERCHANTABLE OR FIT FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE 
! QUALITY OF THE ORIGINAL WORK IS WITH YOU. THIS DISCLAIMER OF WARRANTY 
! CONSTITUTES AN ESSENTIAL PART OF THIS LICENSE. NO LICENSE TO ORIGINAL WORK 
! IS GRANTED HEREUNDER EXCEPT UNDER THIS DISCLAIMER.  
! 
! LIMITATION OF LIABILITY. UNDER NO CIRCUMSTANCES AND UNDER NO LEGAL THEORY, 
! WHETHER TORT (INCLUDING NEGLIGENCE), CONTRACT, OR OTHERWISE, SHALL THE 
! LICENSOR BE LIABLE TO ANY PERSON FOR ANY DIRECT, INDIRECT, SPECIAL, 
! INCIDENTAL, OR CONSEQUENTIAL DAMAGES OF ANY CHARACTER ARISING AS A RESULT OF 
! THIS LICENSE OR THE USE OF THE ORIGINAL WORK INCLUDING, WITHOUT LIMITATION, 
! DAMAGES FOR LOSS OF GOODWILL, WORK STOPPAGE, COMPUTER FAILURE OR 
! MALFUNCTION, OR ANY AND ALL OTHER COMMERCIAL DAMAGES OR LOSSES, EVEN IF SUCH 
! PERSON SHALL HAVE BEEN INFORMED OF THE POSSIBILITY OF SUCH DAMAGES. THIS 
! LIMITATION OF LIABILITY SHALL NOT APPLY TO LIABILITY FOR DEATH OR PERSONAL 
! INJURY RESULTING FROM SUCH PARTY'S NEGLIGENCE TO THE EXTENT APPLICABLE LAW 
! PROHIBITS SUCH LIMITATION. SOME JURISDICTIONS DO NOT ALLOW THE EXCLUSION OR 
! LIMITATION OF INCIDENTAL OR CONSEQUENTIAL DAMAGES, SO THIS EXCLUSION AND 
! LIMITATION MAY NOT APPLY TO YOU.  
! 
! License to Source Code. The term "Source Code" means the preferred form of 
! the Original Work for making modifications to it and all available 
! documentation describing how to access and modify the Original Work. 
! Licensor hereby agrees to provide a machine-readable copy of the Source Code 
! of the Original Work along with each copy of the Original Work that Licensor 
! distributes. Licensor reserves the right to satisfy this obligation by 
! placing a machine-readable copy of the Source Code in an information 
! repository reasonably calculated to permit inexpensive and convenient access 
! by You for as long as Licensor continues to distribute the Original Work, 
! and by publishing the address of that information repository in a notice 
! immediately following the copyright notice that applies to the Original 
! Work.
!  
! Mutual Termination for Patent Action. This License shall terminate 
! automatically and You may no longer exercise any of the rights granted to 
! You by this License if You file a lawsuit in any court alleging that any OSI 
! Certified open source software that is licensed under any license containing 
! this "Mutual Termination for Patent Action" clause infringes any patent 
! claims that are essential to use that software.
!  
! This license is Copyright (C) 2002 Lawrence E. Rosen. All rights reserved. 
! Permission is hereby granted to copy and distribute this license without 
! modification. This license may not be modified without at the express 
! written permission of its copyright owner.
!  
! Disclaimers
! No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY 
! OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT 
! LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO 
! SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A 
! PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE 
! SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF 
! PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN 
! ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR 
! RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR 
! ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, 
! GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-
! PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS 
! IS."
!  
! Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST 
! THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS 
! ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN 
! ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, 
! INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S 
! USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE 
! UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY 
! PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY 
! FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS 
! AGREEMENT.
! 
program main
  use kdtree2_module, only : kdtree2_n_nearest, kdtree2, kdtree2_result,       &
                             kdkind, kdtree2_create
  use options,        only : parse_options
  use get_dims,       only : get_dims_data_point, get_dims_data_elem,          &
                             get_dims_interp
  use kinddefs,       only : dp, r8, i4
  use utils,          only : read_to_word, onlist, nlines, quicksort
  use rbf,            only : get_rbf_weights, get_rbf_interp
  use buildinfo,      only : build_time, build_dir
  use tec_types,      only : tec_node_data_type,                               &
                             tec_quad_connectivity_type
  use tecplot,        only : open_tec_file, get_number_zones,                  &
                             read_binary_tecplot_surface
  use string_utils,   only : geti

  implicit none
   
  type spring_data_type
    integer  :: number_nodes
    integer  :: number_elems
    real (r8), dimension(:,:), allocatable :: x ! coordinate
    integer , dimension(:), allocatable :: id  ! global id
    integer , dimension(:), allocatable :: idn ! nearest id (to be found)
    integer  :: body  ! my body number
    integer  :: bodyn ! nearest body number
    integer , dimension(:,:), allocatable :: elem
    real (r8), dimension(:), allocatable :: distance ! distance to nearest point
    real (r8), dimension(:,:), allocatable :: xn ! nearest coordinate
  end type spring_data_type
  
  type kdtree2_pointer ! created to allow array of pointers
    type(kdtree2), pointer :: p 
  end type kdtree2_pointer

  integer,   parameter, dimension(3) :: ver = [2, 4, 0]
  integer,   parameter :: dim_space = 3
  integer,   parameter :: dim_data  = 3
  integer,   parameter :: dim_elem  = 4
  integer,   parameter :: unit_source  = 1, unit_dest  = 2, unit_interp = 3
  integer,   parameter :: unit_primary = 4, unit_prune = 7
  integer,   parameter :: nresults = 8, n_target_default = 1000
  real (dp), parameter :: l_factor = 0.05 ! to estimate characteristic length
  integer,   parameter :: max_prune_iterations = 100 ! should never come close to this
!
! r0 should be larger than the typical separation of points but smaller than 
! the "outer scale" or feature size of the function that you are interpolating.
! p 142 NR 2007
!
!  real (dp), parameter :: r0 = 1e-6 ! found by trial and error 
!                                      (seems to be small) 
  real (dp)             :: r0

  character(len=256)    :: format0, format2, format1, exe
  character(len=2048)   :: filename_source, filename_dest, filename_connect, line
  character(len=2048)   :: filename_interp, filename_primary, filename_prune
  logical, dimension(3) :: skipxyz
  logical               :: ignore_zero, spring_connections
  logical               :: fexist
  character(len=3)      :: source_format ! fem mode format .plt .dat or .txt

  integer :: sym_flag
  integer :: tid, nthreads
  integer :: i, j, k, m, n, p, pq
  integer :: n_interp, n_data, nelem_interp, n_prune, n_primary
  integer :: nelem_primary, n_target
  integer :: percent_keep
  integer :: clock1, clock2, clock3, clock4, clock_rate
  integer :: iostatus
  integer :: number_of_spring_patches

  real (dp) :: zero_tol_double
  real (dp) :: zero_tol_single
  real (dp) :: xmin(dim_space), xmax(dim_space), time, l_max
  real (dp) :: fmin(dim_space), fmax(dim_space), min_distance
  real (dp) :: l_keep_squared, dist_min, dist, pi
  real (dp) :: l_blend, l_blend_sq, bf, xi, xir, reduction_factor, length_keep
  integer, parameter :: dim_alldata = 6
  real (dp) :: f(dim_alldata)

  real (kdkind)                                     :: query_vec(dim_space)
  type (kdtree2), pointer                           :: tree_primary
  type (kdtree2_result)                             :: results(nresults)
  type (kdtree2_pointer), allocatable, dimension(:) :: trees

  integer,   allocatable, dimension(:)   :: id_interp, id_data, id_primary
  integer,   allocatable, dimension(:,:) :: elem_interp

  real (dp), allocatable, dimension(:,:) :: x_primary

  real (dp), allocatable, dimension(:,:) :: x_interp
  real (dp), allocatable, dimension(:,:) :: fall_interp
  real (dp), allocatable, dimension(:)   :: f_interp

  real (dp), allocatable, dimension(:,:) :: x_data
  real (dp), allocatable, dimension(:,:) :: fall_data
  real (dp), allocatable, dimension(:)   :: f_data
  real (dp), allocatable, dimension(:)   :: distances 

  real (dp), allocatable, dimension(:,:) :: x_prune
  real (dp), allocatable, dimension(:,:) :: fall_prune
  real (dp), allocatable, dimension(:)   :: f_prune

  real (dp), allocatable :: weights(:)

  integer, parameter                                   :: zone_source = 1
  integer                                              :: number_zones
  integer                                              :: number_variables
  character(256)                                       :: title, variable_list 
  character(256), dimension(:), allocatable            :: zone_names
  character(256), dimension(:), allocatable            :: spring_patch_filenames
  integer, allocatable, dimension(:)                   :: spring_patch_body                                          
  real(r8)                                             :: solution_time
  type (tec_node_data_type), dimension(:), allocatable :: tec_node_data_source
  type (tec_quad_connectivity_type), dimension(:), allocatable ::              &
    tec_quad_connectivity_source

  type (spring_data_type), dimension(:), allocatable :: spring_patch_data

  

  namelist /spring_patches/ number_of_spring_patches, spring_patch_filenames,  &
                            spring_patch_body

  
  continue

  call system_clock(clock3, clock_rate)
  call get_command_argument(0, exe)
! following line causes seg fault in intel-openmp , not gfortran or intel serial
!  write(*,'(a," Version: ",i1,".",i1,".",i1)') trim(exe), ver 
  write(*,'(a," Version: ",i1,".",i1,".",i1,"-",a)') trim(exe), ver,           &
    trim(build_dir)

  pi=acos(-1.d0)
  zero_tol_double = epsilon(1.d0)
  zero_tol_single = epsilon(1.0)

  call parse_options(ver, filename_source, filename_dest, filename_interp,     &
                     filename_primary, filename_prune, length_keep,            &
                     percent_keep, n_target, sym_flag, l_blend, ignore_zero,   &
                     spring_connections)
! optionally compute spring connections and stop
  if (spring_connections) then
    inquire(file='rbf.nml',exist=fexist)
    if (.not.fexist) then 
      write(*,*) 'Please supply "rbf.nml" namelist file. See example.rbf.nml'
      open(1,file='example.rbf.nml',form='formatted')  
      write(1,'(a)') "&spring_patches"
      write(1,'(a)') "  number_of_spring_patches = 2"
      write(1,'(a)') "  spring_patch_filenames(1) = "                          &
                     //"'spring_foo_massoud_body1.dat'"
      write(1,'(a)') "  spring_patch_filenames(2) = "                          &
                     //"'spring_foo_massoud_body2.dat'"
      write(1,'(a)') "  spring_patch_body(1) = 1"
      write(1,'(a)') "  spring_patch_body(2) = 2"
      write(1,'(a)') "/" 
      close(1)
      stop
    else
  ! scan nml to get number_of_spring_patches
      open(1,file='rbf.nml')
      number_of_spring_patches = 0
      iostatus=0
      do while (iostatus==0) 
        read(1,'(a)',iostat=iostatus) line
        number_of_spring_patches=geti(line,'number_of_spring_patches')
        if (number_of_spring_patches /= 0) exit
      end do
      write(*,*) 'number_of_spring_patches = ',number_of_spring_patches
      if (number_of_spring_patches > 0 ) then
        allocate(spring_patch_filenames(number_of_spring_patches))
        allocate(spring_patch_body(number_of_spring_patches))
      end if 

      rewind(1)
      read(1,nml=spring_patches)
      close(1)
      allocate(spring_patch_data(number_of_spring_patches))
      allocate(trees(number_of_spring_patches))
! read in all patches
      do p = 1, number_of_spring_patches
        spring_patch_data(p)%body=spring_patch_body(p) 
        open(1,file=spring_patch_filenames(p),form='formatted', status='old')
        call get_dims_data_elem(1, spring_patch_data(p)%number_nodes,          &
                                   spring_patch_data(p)%number_elems)
        
        allocate(spring_patch_data(p)%                                         &
                 x(dim_space,spring_patch_data(p)%number_nodes))
        allocate(spring_patch_data(p)%                                         &
                 xn(dim_space,spring_patch_data(p)%number_nodes))
        allocate(spring_patch_data(p)%id(spring_patch_data(p)%number_nodes))
        allocate(spring_patch_data(p)%idn(spring_patch_data(p)%number_nodes))
        allocate(spring_patch_data(p)%                                         &
                 elem(dim_elem, spring_patch_data(p)%number_elems))
        allocate(spring_patch_data(p)%distance(spring_patch_data(p)%number_nodes))
        do n = 1, spring_patch_data(p)%number_nodes
          read(1, *) spring_patch_data(p)%x(1,n),                              &
                     spring_patch_data(p)%x(2,n),                              &
                     spring_patch_data(p)%x(3,n),                              &
                     spring_patch_data(p)%id(n)
        end do
        do n = 1, spring_patch_data(p)%number_elems
          read(1, *) spring_patch_data(p)%elem(1,n),                           &
                     spring_patch_data(p)%elem(2,n),                           &
                     spring_patch_data(p)%elem(3,n),                           &
                     spring_patch_data(p)%elem(4,n)
        end do
        close(1)
      end do
! fixme: check input patches
! patches cannot share boundaries or patch to themselves

! create trees for all patches
      do p = 1, number_of_spring_patches
        trees(p)%p => kdtree2_create(spring_patch_data(p)%x, rearrange=.true., sort=.true.)
      end do
! check nearest point from all patches to all other trees
! if nearest match global node is not in own patch keep(could be same on bounds)
! else take next closest       
! store 
! Note: dis from kdtree is dis**2

      do pq = 1, number_of_spring_patches
        do n = 1, spring_patch_data(pq)%number_nodes
          min_distance = huge(1.d0)
          query_vec(:) = spring_patch_data(pq)%x(:,n)
          do p = 1, number_of_spring_patches
            if (p.ne.pq) then 
              call kdtree2_n_nearest(tp=trees(p)%p, qv=query_vec, nn=1,        &
                                     results=results)
              if ((results(1)%dis < min_distance)) then
                spring_patch_data(pq)%idn(n) =                                 &
                                         spring_patch_data(p)%id(results(1)%idx)
                spring_patch_data(pq)%bodyn = spring_patch_data(p)%body 
                spring_patch_data(pq)%distance(n) = sqrt(results(1)%dis)
                spring_patch_data(pq)%xn(:,n) =                                &
                                        spring_patch_data(p)%x(:,results(1)%idx)
                min_distance = results(1)%dis
              end if
            end if  
          end do
        end do
      end do

! write new spring files
      format1='(2x, 3(e23.15e3, 1x), 3(i10, 1x),e23.15e3, 1x, 3(e23.15e3, 1x))'
      do p = 1, number_of_spring_patches
        write(filename_connect,'(a,i0,a,i0,a)') 'spring_connectivity_body_',   &
          spring_patch_data(p)%body,'_patch_',p,'.dat'
        write(*,*) trim(filename_connect)
        open(p,file=filename_connect)
        write(p,'(a)') 'title="surface points with nearest node (idn)"'
        write(p,'(a)') 'variables="x","y","z","id","idn" "bodyn",'//           &
                       '"distance","xn","yn","zn"'
        write(p,'(a,i0,a,i0,a,i0,a)') 'zone t="spring patch ',p,'", i=',       &
                                       spring_patch_data(p)%number_nodes,      &
                                ', j=', spring_patch_data(p)%number_elems,     &
                                ', f=fepoint,  solutiontime=0.0E+00, strandid=0'
        do n = 1, spring_patch_data(p)%number_nodes
          write(p,format1) spring_patch_data(p)%x(:,n),                        &
                           spring_patch_data(p)%id(n),                         &
                           spring_patch_data(p)%idn(n),                        &
                           spring_patch_data(p)%bodyn,                         &
                           spring_patch_data(p)%distance(n),                   &
                           spring_patch_data(p)%xn(:,n)
        end do
        do j = 1, spring_patch_data(p)%number_elems
          write(p, '(4I10)') (spring_patch_data(p)%elem(i, j), i=1, dim_elem)
        end do
        close(p)
      end do
! write spring segments for debugging
      do p = 1, number_of_spring_patches
        write(filename_connect,'(a,i0,a)') 'spring_segment_patch_',p,'.dat'
        write(*,*) trim(filename_connect)
        open(p,file=filename_connect)
        write(p,'(a)') 'title="spring segments per patch"'
        write(p,'(a)') 'variables="x","y","z","distance"'
        do n = 1, spring_patch_data(p)%number_nodes
          write(p,'(a,i0,a)') 'zone t="spring ',n,                             &
            '", i=2, j=1, f=fepoint,  solutiontime=0.0E+00, strandid=0'
          write(p,'(2x, 4(e23.15e3, 1x))') spring_patch_data(p)%x(:,n),        &
                                           spring_patch_data(p)%distance(n)
          write(p,'(2x, 4(e23.15e3, 1x))') spring_patch_data(p)%xn(:,n),       &
                                           spring_patch_data(p)%distance(n)
          write(p,'(a)') '1 2 1 2'
        end do   
        close(p)
      end do
    end if
    stop
  end if 

! interpolate mode shapes
  source_format = filename_source(len_trim(filename_source)-2:                &
                                   len_trim(filename_source))
  if (source_format=='plt') then
    write(*,*) 'reading binary tecplot format for fem modes'
  else if (source_format=='dat') then
    write(*,*) 'reading ascii tecplot format for fem modes'
  else if (source_format=='txt') then
    write(*,*) 'reading text format for fem modes'
  else 
    write(*,*) 'expecting suffix of .plt .dat or .txt for source fem modes.'
    write(*,*) 'source_format = ', source_format
    stop
  endif 

  if (source_format=='plt') then
    call open_tec_file(unit_source, filename_source)
  else 
    open(unit_source, file=filename_source, form='formatted', status='old')
  end if
  open(unit_dest,   file=filename_dest,   form='formatted', status='old')
  open(unit_interp, file=filename_interp, form='formatted', status='unknown')

  if (.not.(filename_primary==''))                                             &
    open(unit_primary, file=filename_primary, form='formatted', status='old')

  if (source_format=='plt') then
    call get_number_zones(unit_source, number_zones)
    write(*, *) 'number_zones = ',number_zones
    if (number_zones /= 1) then
      write(*,*) 'Warning: Expecting only 1 zone in mode shape input.'
    end if 
    call read_binary_tecplot_surface(unit_source, title, variable_list,        &
                                     zone_names, number_variables,             &
                                     number_zones,  solution_time,             &
                                     tec_node_data_source,                     &
                                     tec_quad_connectivity_source) 
    n_data = tec_node_data_source(zone_source)%number_nodes
  else if (source_format=='dat') then
    call get_dims_data_point(unit_source, n_data)
  else if (source_format=='txt') then
    n_data=nlines(unit_source) 
  end if

  write(*, *) 'n_data = ',n_data

  allocate (   x_data(dim_space, n_data),  f_data(n_data))
  allocate (distances(n_data))
  allocate (fall_data( dim_data, n_data), id_data(n_data))

  if (ignore_zero) then
    if (source_format=='plt') then
      n = 0
      do i = 1, n_data
        do m = 1, dim_alldata   
          f(m) = tec_node_data_source(zone_source)%variables(m+4,i)
        end do
        if (abs(f(1))+abs(f(2))+abs(f(3))+abs(f(4))+abs(f(5))+abs(f(6))        &             
             > zero_tol_single ) then
          n = n + 1
          x_data(1, n)    = tec_node_data_source(zone_source)%variables(1,i)
          x_data(2, n)    = tec_node_data_source(zone_source)%variables(2,i)
          x_data(3, n)    = tec_node_data_source(zone_source)%variables(3,i)
          id_data(n)      = tec_node_data_source(zone_source)%variables(4,i) 
          fall_data(1, n) = tec_node_data_source(zone_source)%variables(5,i)
          fall_data(2, n) = tec_node_data_source(zone_source)%variables(6,i)
          fall_data(3, n) = tec_node_data_source(zone_source)%variables(7,i)
        end if
      end do
      write(*,*) 'ignored ',n_data-n, ' points out of ', n_data
      n_data = n
    else if (source_format=='dat') then
      n = 0
      do i = 1, n_data
        n = n + 1
        read(unit_source, *) x_data(1, n), x_data(2, n), x_data(3, n),         &
                             id_data(n), (f(m),m=1,dim_alldata)
        if (abs(f(1))+abs(f(2))+abs(f(3))+abs(f(4))+abs(f(5))+abs(f(6))        &             
            > zero_tol_single ) then
          fall_data(1, n) = f(1)
          fall_data(2, n) = f(2)
          fall_data(3, n) = f(3)
        else
          n = n - 1
        end if
      end do
      write(*,*) 'ignored ',n_data-n, ' points out of ', n_data
      n_data = n
    else if (source_format=='txt') then
      rewind(unit_source)
      n = 0
      do i = 1, n_data
        n = n + 1
        read(unit_source, *) x_data(1, n), x_data(2, n), x_data(3, n),         &
                             (f(m),m=1,dim_alldata)
        if (abs(f(1))+abs(f(2))+abs(f(3))+abs(f(4))+abs(f(5))+abs(f(6))        &             
            > zero_tol_single ) then
          fall_data(1, n) = f(1)
          fall_data(2, n) = f(2)
          fall_data(3, n) = f(3)
        else
          n = n - 1
        end if
      end do
      write(*,*) 'ignored ',n_data-n, ' points out of ', n_data
      n_data = n
    end if
  else
    if (source_format=='plt') then
      do i = 1, n_data
        x_data(1, i)    = tec_node_data_source(zone_source)%variables(1,i)
        x_data(2, i)    = tec_node_data_source(zone_source)%variables(2,i)
        x_data(3, i)    = tec_node_data_source(zone_source)%variables(3,i)
        id_data(i)      = tec_node_data_source(zone_source)%variables(4,i) 
        fall_data(1, i) = tec_node_data_source(zone_source)%variables(5,i)
        fall_data(2, i) = tec_node_data_source(zone_source)%variables(6,i)
        fall_data(3, i) = tec_node_data_source(zone_source)%variables(7,i)
      end do
    else if (source_format=='dat') then
      do i = 1, n_data
        read(unit_source, *) x_data(1, i), x_data(2, i), x_data(3, i),         &
                             id_data(i), fall_data(1, i), fall_data(2, i),     &
                             fall_data(3, i)
      end do
    else if (source_format=='txt') then
      rewind(unit_source)
      do i = 1, n_data
        read(unit_source, *) x_data(1, i), x_data(2, i), x_data(3, i),         &
                             fall_data(1, i), fall_data(2, i), fall_data(3, i)
      end do
    end if
  end if 

! check for zeroed out displacements for component directions
  fmin = huge(0.d0)
  fmax = -fmin
  do m = 1, dim_space
    fmin(m) = minval( fall_data(m,:) )
    fmax(m) = maxval( fall_data(m,:) )
    write(*,*) m,fmin(m),fmax(m)
  end do
  
  !skipxyz(:) = .false.
  !do m = 1, dim_space
  !  if (abs(fmax(m)-fmin(m)) <= zero_tol_double ) then
  !    skipxyz(m) = .true.
  !    write(*,*) 'skipxyz(',m,')=',skipxyz(m)
  !  end if
  !end do

  !skipxyz(:) = (abs(fmax(:)-fmin(:)) <= zero_tol_double )
  skipxyz(:) = (abs(fmax(:)-fmin(:)) <= zero_tol_single )
  !write(*,*) 'skipxyz(:)=',skipxyz(:)

!read destination
  call get_dims_interp(unit_dest, n_interp, nelem_interp)
  write(*,*) 'n_interp, nelem_interp=',n_interp, nelem_interp
  allocate (x_interp(dim_space, n_interp), id_interp(n_interp),                &
            elem_interp(dim_elem, nelem_interp), f_interp(n_interp),           &
            fall_interp(dim_data, n_interp))
  do i = 1, n_interp
    read(unit_dest, *) x_interp(1, i), x_interp(2, i), x_interp(3, i),         &
      id_interp(i)
  end do
  do j = 1, nelem_interp
    read(unit_dest, *) (elem_interp(i, j), i=1, dim_elem)
  end do

! prune data based on spacing threshold for points included from the fem
! starting from large spacing based on extents 
  call system_clock(clock1, clock_rate)
  allocate (weights(n_data), x_prune(dim_space, n_data), f_prune(n_data),      &
            fall_prune(dim_data, n_data))

  if (length_keep >= 0) then   
    l_keep_squared = length_keep**2
    ! keep first point
    n_prune = 1
    x_prune(:, n_prune)    = x_data(:, 1)
    fall_prune(:, n_prune) = fall_data(:, 1)
    do i = 2, n_data
      dist_min = huge(0.d0)
      do j = 1, n_prune
        dist = sum( (x_data(:, i)-x_prune(:,j))**2)
        if (dist < dist_min) then
          dist_min = dist 
        end if
      end do
      if (dist_min > l_keep_squared) then
        n_prune = n_prune + 1
        x_prune(:, n_prune) = x_data(:, i)
        fall_prune(:, n_prune) = fall_data(:, i)
      end if
    end do
    write(*,*) 'n_prune = ',n_prune
  else
    if (percent_keep /= 0) then ! use percent if specified
      n_target = percent_keep * n_data/100
      write(*, *) 'using percent_keep: n_target = ', n_target
    else if (n_target == 0) then ! if neither is specified, use default
      n_target = min(n_target_default, n_data)
    endif
    write(*,*) 'n_target = ',n_target
    if (n_target == n_data) then  ! 100% 
      call system_clock(clock3, clock_rate)
      do i = 1, n_data
        dist_min = huge(0.d0)
        do j = 1, n_data
          if ( i /= j ) then
            dist = sum( (x_data(:, i)-x_data(:,j))**2)
            if (dist < dist_min) then
              dist_min = dist 
            end if
          end if
        end do
        distances(i) = dist_min
      end do
      call quicksort(distances,1,n_data)
      l_keep_squared=distances(n_data/2) !median distance **2
      call system_clock(clock4, clock_rate)
      time = dble(clock4-clock3) / clock_rate
      write(*, '("compute r0:", f8.2, " sec")') time
      n_prune = n_data
      x_prune = x_data
      fall_prune = fall_data
    else
    ! determine maximum length scale (l_max)
      do m = 1, dim_space
        xmin(m) = minval( x_data(m,:) )
        xmax(m) = maxval( x_data(m,:) )
      end do
      l_max = maxval( xmax(:)-xmin(:) )
     
      l_keep_squared = (l_max*l_factor)**2
      k = 0
      reduction_factor=1.0
      n_prune = 0 
      write(*,*) 'l_max, l_keep_squared=',l_max,l_keep_squared
      do while (n_prune < n_target )
        k = k + 1
        if ( k >  max_prune_iterations ) then
          write(*,*) 'exceeded max_prune_iterations = ', max_prune_iterations
          exit 
        end if 
    ! keep first point
        n_prune = 1
        x_prune(:, n_prune)    = x_data(:, 1)
        fall_prune(:, n_prune) = fall_data(:, 1)
        do i = 2, n_data
          dist_min = huge(0.d0)
          do j = 1, n_prune
            dist = sum( (x_data(:, i)-x_prune(:,j))**2)
            if (dist < dist_min) then
              dist_min = dist 
            end if
          end do
          if (dist_min > l_keep_squared) then
            n_prune = n_prune + 1
            x_prune(:, n_prune) = x_data(:, i)
            fall_prune(:, n_prune) = fall_data(:, i)
          end if
        end do
        if (float(n_prune)/float(n_target) < .25 ) then
          reduction_factor = .25
        else if (float(n_prune)/float(n_target) < .5 ) then
          reduction_factor = .5
        else if (float(n_prune)/float(n_target) < .75 ) then
          reduction_factor = .75
        else if (float(n_prune)/float(n_target) < .99 ) then
          reduction_factor = .98
        end if 
        l_keep_squared = l_keep_squared*reduction_factor
        !write(*,*) n_prune, n_target, l_keep_squared
      end do
      call system_clock(clock2, clock_rate)
      time = dble(clock2-clock1) / clock_rate
      write(*, '("prune:", f8.2, " sec")') time

      write(*, '(i3, a, i6, a)') nint(100*float(n_prune)/float(n_data)),         &
       '% source points kept. (', n_prune, ' points)' 
      write(*,*) 'prune iterations = ',k
    end if
  end if
  r0 = sqrt(l_keep_squared)
  write(*,*) 'r0 = ',r0

! write prune coordinates
  if (filename_prune/='') then
    open(unit_prune, file=filename_prune, form='formatted')
    write(unit_prune, '(a)') 'VARIABLES = "X" "Y" "Z" "f1" "f2" "f3"'
    write(unit_prune, '(a)') 'ZONE T="prune"'
    do i = 1, n_prune
      write(unit_prune, '(6E15.7)') x_prune(:, i), fall_prune(:, i)
    end do
    close(unit_prune)
  end if

! interpolate for each column of data
  do j = 1, dim_data
    if ( skipxyz(j) ) then
      !write(*, *) 'Skipping zeroed mode shape component', j
      write(*, *) 'Skipping interpolation of constant mode shape component',  &
                  j, 'f =',fall_data(j,1)
      fall_interp(j, :) =  fall_data(j,1) ! index 1 is arbitrary since constant
    else
      write(*, *) 'interpolating component:', j
      f_prune(:) = fall_prune(j, :) 
      call system_clock(clock1, clock_rate)
      call get_rbf_weights ( dim_space, n_prune, x_prune, r0,                  &
                             f_prune, weights )
      call system_clock(clock2, clock_rate)
      time = dble(clock2-clock1) / clock_rate
      write(*, *) 'get_rbf_weight: ', time, ' sec'

      call system_clock(clock1, clock_rate)
      call get_rbf_interp ( dim_space, n_prune, x_prune, r0,                   &
                            weights, n_interp, x_interp, f_interp )
      call system_clock(clock2, clock_rate)
      fall_interp(j, :) = f_interp(:) 
      time = dble(clock2-clock1) / clock_rate
      write(*, *) j, 'get_rbf_interp: ', time, ' sec'
    end if
  end do 

! handle points not on primary surface (wing)
  if (.not.(filename_primary=='')) then
    l_blend_sq = l_blend**2
! read primary (same structure as destination)
    call get_dims_interp(unit_primary, n_primary, nelem_primary)
    allocate (x_primary(dim_space, n_primary), id_primary(n_primary))
    do i = 1, n_primary
      read(unit_primary, *) x_primary(1, i), x_primary(2, i), x_primary(3, i), &
        id_primary(i)
    end do
! create kd tree of nodes primary data
    tree_primary => kdtree2_create(x_primary, rearrange=.true., sort=.false.)

    do i = 1, n_interp
      if (.not.onlist(id_interp(i), n_primary, id_primary)) then
        query_vec(:) = x_interp(:, i)
        call kdtree2_n_nearest(tp=tree_primary, qv=query_vec, nn=1,            &
          results=results)
        if ((results(1)%dis < l_blend_sq)) then
          xi = results(1)%dis/l_blend_sq
          xir = 1.d0-xi
          bf = 1.d0-xi !linear 
!          bf = (1.d0-xi)**4*(4.d0*xi+1.d0) ! c2
!          bf = 0.5*(1+cos(xi*pi)) ! smooth (both ends) cosine 
!          bf = 1.d0-(1.d0-xir)**4*(4*xir+1.d0) ! reverse profile c2, 
                                                ! more movement closer
! zero out nearest point on primary surface (may not be smooth if primary 
! is coarser--not likely)
          do j = 1, n_interp
            if (id_interp(j) == id_primary(results(1)%idx)) then
              fall_interp(:, i) = bf * fall_interp(:, j)
              !fall_interp(:, i) = bf
            end if
          end do
        else
          fall_interp(:, i) = 0.d0
        end if
      end if
    end do
  end if

! smoothly zero out points on x, y or z symmetry plane  
! zeroes out only out of plane values
! (assumes plane is at origin)
! not used with -p or -b options
  if ( (sym_flag > 0) .and. (sym_flag < 4) ) then
    do i = 1, n_interp
      if (abs(x_interp(sym_flag, i)) < l_blend ) then 
! assumes symmetry plane is at origin
        xi = abs(x_interp(sym_flag, i))/l_blend
        xir = 1.d0-xi
        bf = (1.d0-xir)**4*(4.d0*xir+1.d0) !c2
        fall_interp(sym_flag, i) = bf * fall_interp(sym_flag, i)
      end if
    end do
  end if


! write interpolated result
  format0 = "('zone t=', a, ', i=', i0, ', j=', i0, ', f=fepoint')"
  !format2 = '(2x, 3(e23.15e3, 1x), i10, 1x, 10(e23.15e3, 1x))'
  !format2 = '(2x, 3(e17.9e3, 1x), i10, 1x, 10(e17.9e3, 1x))'
  format2 = '(2x, 3(e23.15e3, 1x), i10, 1x, 10(e17.9e3, 1x))'
  write(unit_interp, '(a)') 'TITLE = "mode shapes via rbf"'
  write(unit_interp, '(a)') 'VARIABLES = "X" "Y" "Z" "Id" "f1" "f2" "f3"'
  write(unit_interp, format0) '"ID#1"', n_interp, nelem_interp
  do i = 1, n_interp
    write(unit_interp, format2) x_interp(1, i), x_interp(2, i), x_interp(3, i),&
      id_interp(i), fall_interp(1, i), fall_interp(2, i), fall_interp(3, i) 
  end do
  do j = 1, nelem_interp
    write(unit_interp, '(4I10)') (elem_interp(i, j), i=1, dim_elem)
  end do

  call system_clock(clock4, clock_rate)
  time = dble(clock4-clock3) / clock_rate
  write(*, '("rbf runtime:", f8.2, " sec")') time

end program main
