module usage
  use buildinfo, only : build_time, build_os, build_dir

  implicit none

  private

  public :: help 

contains

!=================================== USAGE ===================================80

  subroutine help(ver)

    implicit none

    character(len=256) :: exe
    integer :: ver(3)

    continue

    call get_command_argument(0, exe)
    write(*,'(/a)') ' Purpose:  Interpolate mode shapes from FEM modes '//     &
      'to CFD surface.'
    write(*,'(a)')  '   Usage:  '//trim(exe)//' -s fem_mode_shape '//          &
      '-d cfd_mesh -i cfd_mode_shape [options] '
    write(*,'(a)') ' Options:  '
    write(*,'(t12,a,t16,a)') '-s', 'source_fem_mode_shape_file'
    write(*,'(t12,a,t16,a)') '-d', 'destination_mesh_file'
    write(*,'(t12,a,t16,a)') '-i', 'interpolated_mode_shape_file'
    write(*,'(t12,a,t16,a)') '-iz', 'ignore points whose values are zero'
    write(*,'(t12,a,t16,a)') '-nk', 'number of source (fem) nodes to keep'
    write(*,'(t12,a,t16,a)') '-pk', 'percent of source (fem) nodes to keep'
    write(*,'(t12,a,t16,a)') '-wp', 'write fem_source_points for debugging' 
    write(*,'(t12,a,t16,a)') '-p', 'primary_surface_file' 
    write(*,'(t12,a,t16,a)') '-b', 'radial_blend_distance_in_grid_units '//    &
      'for use with -p' 
    write(*,'(t12,a,t16,a)') '-x', 'xsym_blend_distance_in_grid_units' 
    write(*,'(t12,a,t16,a)') '-y', 'ysym_blend_distance_in_grid_units' 
    write(*,'(t12,a,t16,a)') '-z', 'zsym_blend_distance_in_grid_units' 
    write(*,'(t12,a,t16,a)') '-cs', 'compute spring connectivity from rbf.nml'
    write(*,'(a)') 'Requires:  '
    write(*,'(a)') '          1) tecplot formatted fem mode shapes with '
    write(*,'(a)') '             variables x,y,z,id,f1,f2,f3,f4,f5,f6'
    write(*,'(a)') 'Examples:  '
    write(*,'(a)') '           '//trim(exe)//' -s fem/mode001.plt -d '//       &
      'project_ddfdrive_body1.dat \'
    write(*,'(t14,a)') '-i project_body1_mode1.dat -nk 250 -y 4'
    write(*,'(a)') '           '//trim(exe)//' -s fem/mode001.dat -d '//       &
      'ddfdrive_allsurf.dat \'
    write(*,'(t14,a)') '-i project_body1_mode1.dat -p ddfdrive_wing_tail.dat'//&
      ' -b 3 -pk 25'
    write(*,'(t4,"Built:  ",a," on ",a)') trim(build_time),trim(build_os)
    write(*,'(" Version:  ",i1,".",i1,".",i1," (",a,")")') ver, trim(build_dir)
    write(*,'(a)') '   About:  Steven.J.Massey@nasa.gov '
    write(*,*)
    stop
    return
  end subroutine help
end module usage
