module tec_types

  use kinddefs, only : i4,r4,r8

  implicit none

  type tec_node_data_type
    integer(i4) :: number_nodes
    real(r8), dimension(:,:), allocatable :: variables
  end type tec_node_data_type

  type tec_quad_connectivity_type
    integer(i4) :: number_elements
    integer(i4), dimension(:,:), allocatable :: f2n
  end type tec_quad_connectivity_type

end module tec_types
