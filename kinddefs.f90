module kinddefs

  use, intrinsic ::  iso_c_binding, only : c_float, c_double,                  &
                                           c_int8_t, c_int16_t,                &
                                           c_int32_t, c_int64_t

  implicit none

  private

! single precision (IEEE 754)
  integer, parameter, public :: system_r4=c_float
  integer, parameter, public :: r4=system_r4  ! shorthand for system_r4

! double precision (IEEE 754)
  integer, parameter, public :: system_r8=c_double
  integer, parameter, public :: r8=system_r8  ! shorthand for system_r8

! one byte integer (greater than 10e2)
  integer, parameter, public :: system_i1=c_int8_t
  integer, parameter, public :: i1=system_i1

! two byte integer (greater than 10e3)
  integer, parameter, public :: system_i2=c_int16_t
  integer, parameter, public :: i2=system_i2

! four byte integer (greater than 10e5)
  integer, parameter, public :: system_i4=c_int32_t
  integer, parameter, public :: i4=system_i4
  integer, parameter, public :: max_i4 = huge(1_system_i4)

! eight byte integer (greater than 10e10)
  integer, parameter, public :: system_i8=c_int64_t
  integer, parameter, public :: i8=system_i8

  integer, parameter, public :: dp=system_r8 ! default precision

  integer, parameter, public :: default_integer_kind = kind(1)
  integer, parameter, public :: default_real_kind = kind(1.0)

end module kinddefs
