module tecplot

  implicit none

  private

  public :: open_tec_file, get_number_zones, read_binary_tecplot_surface

contains

!============================== open_tec_file ================================80

subroutine open_tec_file(funit,filename)
 
  use kinddefs, only : i4,r4,r8

  integer        :: funit
  character*(*)  :: filename
  character(8)   :: magic_number ! magic number, version number
  integer(i4)    :: tecplot_one  ! = 1 if byte order is correct
  
  continue

  open(unit=funit, file=filename, form='unformatted', access='stream',      &
       convert='little_endian',status='old')

! magic_number 
  read(funit) magic_number

! endian indicator
  read(funit) tecplot_one
  if (tecplot_one /= 1 ) then
    close(funit)
    open(unit=funit, file=filename, form='unformatted', access='stream',      &
         convert='big_endian',status='old')
    read(funit) magic_number
    read(funit) tecplot_one
  end if
  if (tecplot_one /= 1 ) then
    write(*,*) 'problem with '//trim(filename)
    write(*,*) 'magic number = ',trim(magic_number)
    write(*,*) 'tecplot_one = ',tecplot_one
    write(*,*) 'you may need to unsetenv F_UFMTENDIAN '
    stop
  end if
!  write(*,*) 'magic number = ', trim(magic_number)  

end subroutine open_tec_file

!========================= get_number_zones ==================================80

subroutine get_number_zones(iunit, number_zones)

  use kinddefs, only : i4,r4,r8

  real(r4), parameter :: zone_marker = 299.0
  real(r4), parameter :: eoh_marker = 357.0
  integer :: iunit, number_zones, nthvar, i
  character(8) :: char8
  integer(i4)  :: int32
  integer(i4)  :: number_variables,data_packing
  real(r8)     :: float64
  real(r4)     :: marker

  continue 

  rewind(iunit) 
! header
  read(iunit) char8
  read(iunit) int32
  read(iunit) int32
  int32 = 1
  do while ( int32 /= 0)
    read(iunit) int32
  end do
  read(iunit) number_variables
  nthvar = 1
  do while ( nthvar <= number_variables)
    read(iunit) int32
    if ( int32 == 0 ) then
      nthvar = nthvar + 1
    end if
  end do
! zone headers
  number_zones = 0
  read(iunit) marker
  do while (marker == zone_marker)
    number_zones = number_zones + 1
    int32 = 1
    do while ( int32 /= 0)
      read(iunit) int32
    end do
! parent zone
    read(iunit) int32
! strand id
    read(iunit) int32
! solution time
    read(iunit) float64
! not used 
    read(iunit) int32
! zone type
    read(iunit) int32
! data packing
    read(iunit) data_packing
! as exist in fun3d/preplot generated plt file
    if ( data_packing == 1 ) then ! dealing with observed behavoir of preplot created bin
      do i = 1, number_variables
        read(iunit) int32
      end do
    end if
! raw local
    read(iunit) int32
! number_misc
    read(iunit) int32
! number_points
    read(iunit) int32
! number_elements
    read(iunit) int32
! icelldim, jcelldim, kcelldim
    read(iunit) int32,int32,int32
! aux data
    read(iunit) int32
    read(iunit) marker
  end do

  return

end subroutine get_number_zones

!========================== get_data_format ==================================80

subroutine get_data_format(iunit, data_format)

  use kinddefs, only : i4,r4,r8

  real(r4), parameter :: zone_marker = 299.0
  real(r4), parameter :: eoh_marker = 357.0
  integer :: iunit, number_zones, nthvar, i
  character(8) :: char8
  integer(i4)  :: int32
  integer(i4)  :: number_variables, data_packing, data_format
  real(r8)     :: float64
  real(r4)     :: marker

  continue 

  rewind(iunit) 
! header
  read(iunit) char8
  read(iunit) int32
  read(iunit) int32
  int32 = 1
  do while ( int32 /= 0)
    read(iunit) int32
  end do
  read(iunit) number_variables
  nthvar = 1
  do while ( nthvar <= number_variables)
    read(iunit) int32
    if ( int32 == 0 ) then
      nthvar = nthvar + 1
    end if
  end do
! zone headers
  number_zones = 0
  read(iunit) marker
  do while (marker == zone_marker)
    number_zones = number_zones + 1
    int32 = 1
    do while ( int32 /= 0)
      read(iunit) int32
    end do
! parent zone
    read(iunit) int32
! strand id
    read(iunit) int32
! solution time
    read(iunit) float64
! not used 
    read(iunit) int32
! zone type
    read(iunit) int32
! data packing
    read(iunit) data_packing
! as exist in fun3d/preplot generated plt file
    if ( data_packing == 1 ) then ! dealing with observed behavoir of preplot created bin
      do i = 1, number_variables
        read(iunit) int32
      end do
    end if
! raw local
    read(iunit) int32
! number_misc
    read(iunit) int32
! number_points
    read(iunit) int32
! number_elements
    read(iunit) int32
! icelldim, jcelldim, kcelldim
    read(iunit) int32,int32,int32
! aux data
    read(iunit) int32
    read(iunit) marker
  end do

! assume all variables in all zones are in same format
! 
! data marker
  read(iunit) marker
! data format
  read(iunit) data_format

  return

end subroutine get_data_format


!=========================== read_binary_tecplot_surface =====================80

  subroutine read_binary_tecplot_surface(funit, title, variable_list,          &
                                         zone_names, number_variables,         &
                                         number_zones, solution_time,          &
                                         tec_node_data, tec_quad_connectivity)
!
! Called after file has been opened scanned to determine number of zones
!
! Read a subset of binary tecplot file for surface output from fun3d or preplot 
! of fun3d created ascii.
! Differs from published binary format.
!

    use kinddefs, only : i4,r4,r8
    use tec_types

    implicit none

    integer :: funit
    integer :: i, j, nthvar, n, e, v 
    character(1), parameter :: nullchar = char(0) ! for C string termination
    integer :: number_zones, zone
    logical, parameter :: verbose = .true.

  ! tecplot binary
    character(8)   :: magic_number ! magic number, version number
    integer(i4)    :: tecplot_one       ! = 1 if byte order is correct
    integer(i4)    :: file_type           
    integer(i4)    :: ascii_letter
    integer(i4)    :: number_variables
    character(256) :: title,variable_list
    character(256), dimension(:), allocatable :: variable_names
    character(256), dimension(:), allocatable :: zone_names
    real(r4), parameter :: zone_marker = 299.0
    real(r4), parameter :: eoh_marker = 357.0
    real(r4) :: marker 
    real(r4) :: float32 
    integer(i4)    :: parent_zone
    integer(i4)    :: strand_id
    real(r8)       :: solution_time
    integer(i4)    :: not_used ! = -1 
    integer(i4)    :: zone_type
    integer(i4), dimension(:), allocatable    :: data_packing
    integer(i4), dimension(:), allocatable    :: variable_locations
    integer(i4)    :: raw_local
    integer(i4)    :: number_misc ! should be 0
    integer(i4)    :: icelldim, jcelldim, kcelldim
    integer(i4)    :: aux_data
    integer(i4)    :: data_format
    integer(i4)    :: has_passive_variables
    integer(i4), dimension(:), allocatable  :: is_variable_passive
    integer(i4)    :: has_variable_sharing
    integer(i4), dimension(:), allocatable  :: zone_to_share_variable_with
    integer(i4), dimension(:), allocatable  :: zone_to_share_connectivity_with
    real(r8), dimension(:,:), allocatable :: variable_min_max
    
    type (tec_node_data_type), dimension(:), allocatable :: tec_node_data
    type (tec_quad_connectivity_type), dimension(:), allocatable :: tec_quad_connectivity   

  ! allocate zonal data
    allocate (zone_names(number_zones))
    allocate (data_packing(number_zones))
    allocate (zone_to_share_connectivity_with(number_zones))
    allocate (tec_quad_connectivity(number_zones))
    allocate (tec_node_data(number_zones))

  ! read file from the start
    rewind(funit)
    read(funit) magic_number  
    read(funit) tecplot_one

  ! filetype
    read(funit) file_type
!    write(*,*) 'file_type = ',file_type
    
  ! title 
    ascii_letter = 1
    i = 0
    title=''
    do while ( ascii_letter /= 0)
      i = i + 1
      read(funit) ascii_letter
      if ( ascii_letter /= 0) title(i:i) = char(ascii_letter)
    end do
    if(verbose) write(*,'(a)') 'title = '//trim(title)
    
  ! number of variables 
    read(funit) number_variables
!    write(*,*) 'number_variables=',number_variables
    allocate( variable_names(number_variables) )
    allocate( variable_locations(number_variables) )
    allocate( is_variable_passive(number_variables) )
    allocate( zone_to_share_variable_with(number_variables) )
    allocate( variable_min_max(2,number_variables) )

  ! variable names
    i = 0
    nthvar = 1
    variable_names=''
    variable_list=''
    do while ( nthvar <= number_variables)
      i = i + 1
      read(funit) ascii_letter
      if ( ascii_letter /= 0 ) variable_names(nthvar)(i:i) = char(ascii_letter)
      if ( ascii_letter == 0 ) then
        nthvar = nthvar + 1
        i = 0
      end if
    end do
    do i = 1, number_variables
      variable_list = trim(adjustl(variable_list)) // ' ' //                  &
                      trim(adjustl(variable_names(i)))
    end do
    if(verbose) write(*,'(a)') 'variable_list = '//trim(variable_list)
    
  ! zone headers
    zone = 0
    read(funit) marker
    do while (marker == zone_marker)
      zone = zone + 1
      ascii_letter = 1
      i = 0
      zone_names(zone)=''
      do while ( ascii_letter /= 0)
        i = i + 1
        read(funit) ascii_letter
        if ( ascii_letter /= 0 ) zone_names(zone)(i:i) = char(ascii_letter)
      end do
!      write(*,*) 'zone_name=',trim(zone_names(zone))
  ! parent zone
      read(funit) parent_zone
!      write(*,*) 'parent_zone = ',parent_zone
  ! strand id
      read(funit) strand_id
!      write(*,*) 'strand_id=',strand_id
  ! solution time
      read(funit) solution_time
!      write(*,*) 'solution_time = ',solution_time
  ! not used 
      read(funit) not_used
!      write(*,*) 'not_used = ',not_used
  ! zone type
      read(funit) zone_type
!      write(*,*) 'zone_type=',zone_type
      if (zone_type /= 3) stop 'expecting fequadrilateral zone type'
  ! data packing
      read(funit) data_packing(zone)
!      write(*,*) 'data_packing=',data_packing(zone)
  !    if (data_packing(zone) /= 1) stop 'expecting point data packing'
  ! as exist in fun3d generated plt file
      if ( data_packing(zone) == 1 ) then ! dealing with observed behavoir of preplot created bin
        do i = 1, number_variables
          read(funit) variable_locations(i)
!          write(*,*) 'variable_locations=',variable_locations(i)
          if (variable_locations(i) /= 0) stop 'expecting data at nodes'
        end do
      end if
  ! raw local
      read(funit) raw_local
!      write(*,*) 'raw_local=',raw_local
      if (raw_local /= 0) stop 'raw_local /= 0'
  ! number_misc
      read(funit) number_misc
!      write(*,*) 'number_misc=',number_misc
      if (number_misc /= 0) stop 'number_misc /= 0'
  ! number_nodes
      read(funit) tec_node_data(zone)%number_nodes
!      write(*,*) 'tec_node_data(zone)%number_nodes=',tec_node_data(zone)%number_nodes
      allocate(tec_node_data(zone)%variables(number_variables,tec_node_data(zone)%number_nodes))
  ! number_elements
      read(funit) tec_quad_connectivity(zone)%number_elements
!      write(*,*) tec_quad_connectivity(zone)%number_elements
      allocate(tec_quad_connectivity(zone)%f2n(4,tec_quad_connectivity(zone)%number_elements))
      
  ! icelldim, jcelldim, kcelldim
      read(funit) icelldim, jcelldim, kcelldim
!      write(*,*) icelldim, jcelldim, kcelldim 
  ! aux data
      read(funit) aux_data
!      write(*,*) aux_data
      if (aux_data /= 0) stop 'expecting no aux data'

  !    read(funit) 
  !    write(*,*) 
      read(funit) marker
!      write(*,*)  marker
    end do
    if (marker /= eoh_marker) stop 'expecting end of header'


  !!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! data section
  !!!!!!!!!!!!!!!!!!!!!!!!!!!
    do zone = 1, number_zones
  ! data marker
      read(funit) marker
!      write(*,*)  marker
  ! data format (assuming all zones and all variables are in the same 
  ! format--not true in general).
      do i = 1, number_variables
        read(funit) data_format
!        write(*,*)  i,data_format
      end do
  ! has passive variables
      read(funit) has_passive_variables
!      write(*,*)  has_passive_variables
  ! is variable passive 
      do i = 1, number_variables
        if ( has_passive_variables /= 0 ) then
          read(funit) is_variable_passive(i)
!          write(*,*)  i,is_variable_passive(i)
        else
          is_variable_passive(i) = 0
        end if
      end do
  ! has variable sharing
      read(funit) has_variable_sharing
!      write(*,*)  has_variable_sharing
  ! zone number ot shar variable with
      do i = 1, number_variables
        if ( has_variable_sharing /= 0 ) then
          read(funit) zone_to_share_variable_with(i)
!          write(*,*)  i,zone_to_share_variable_with(i)
        else
          zone_to_share_variable_with(i) = -1
        end if
      end do
  ! zone_to_share_connectivity_with
      read(funit) zone_to_share_connectivity_with(zone)
!      write(*,*)  zone_to_share_connectivity_with(zone)
  ! min max values of non-shared, non-passive vars (all from fun3d should be)
      do v = 1, number_variables
        if ( ( is_variable_passive(v) == 0 ).and.( zone_to_share_variable_with(v) == -1 ) ) then
          read(funit) (variable_min_max(j,v),j=1,2)
!          write(*,*) v,(variable_min_max(j,v),j=1,2)
        end if 
      end do
  ! For fe zones and not fepolygon or fepolyhedron.
  ! Does not matter how data_packing is set, data is in point format.
  ! node data
      if (data_format == 1) then ! single precision
        do v = 1, number_variables
          do n = 1, tec_node_data(zone)%number_nodes
            read(funit) float32
! swapping order of incidies to be compatable with output_data array 
            tec_node_data(zone)%variables(v,n) = float32 
          end do
        end do
      else ! double precision
        do v = 1, number_variables
          do n = 1, tec_node_data(zone)%number_nodes
! swapping order of incidies to be compatable with output_data array 
            read(funit) tec_node_data(zone)%variables(v,n) 
          end do
        end do
     end if
  ! connectivity data for fequads only (zero based so add 1 to use in fortran or output via text or tecio)
      do e = 1, tec_quad_connectivity(zone)%number_elements
        read(funit) (tec_quad_connectivity(zone)%f2n(i,e),i=1,4)
      end do
      do e = 1, tec_quad_connectivity(zone)%number_elements
        do i = 1, 4
          tec_quad_connectivity(zone)%f2n(i,e) = tec_quad_connectivity(zone)%f2n(i,e)+1
        end do
      end do
    end do

  end subroutine read_binary_tecplot_surface
end module tecplot
