! various radial basis functions

module phi

  implicit none

  private

  public :: phi_mq, phi_imq, phi_tps, phi_g

contains

!---------------------------------- PHI_MQ -----------------------------------80
! Multiquadratic radial basis function.
!-----------------------------------------------------------------------------80
  subroutine phi_mq ( n, r, r0, v )

    use kinddefs, only : dp 

    integer,                intent(in)  :: n
    real(dp), dimension(n), intent(in)  :: r
    real(dp),               intent(in)  :: r0
    real(dp), dimension(n), intent(out) :: v

    real(dp) :: r02
   
    continue 
   
    r02 = r0**2
    v(:) = sqrt( r(:)**2 + r02 )

  end subroutine phi_mq

!---------------------------------- PHI_IMQ ----------------------------------80
! Inverse multiquadratic radial basis function.
!-----------------------------------------------------------------------------80
  subroutine phi_imq ( n, r, r0, v )

    use kinddefs, only : dp 

    integer,                intent(in)  :: n
    real(dp), dimension(n), intent(in)  :: r
    real(dp),               intent(in)  :: r0
    real(dp), dimension(n), intent(out) :: v

    real(dp) :: r02
   
    continue 

    r02 = r0**2
    v(:) = 1.d0/sqrt( r(:)**2 + r02 )


  end subroutine phi_imq

!---------------------------------- PHI_TPS ----------------------------------80
! Thin plate spline radial basis function.
!-----------------------------------------------------------------------------80
  subroutine phi_tps ( n, r, r0, v )

    use kinddefs, only : dp 

    integer,                intent(in)  :: n
    real(dp), dimension(n), intent(in)  :: r
    real(dp),               intent(in)  :: r0
    real(dp), dimension(n), intent(out) :: v

    v(:) = merge(r(:)**2 * log ( r(:) / r0 ), 0.d0, r(:) > 0.d0)

  end subroutine phi_tps

!------------------------------------ PHI_G ----------------------------------80
! Guassian radial basis function.
!-----------------------------------------------------------------------------80
  subroutine phi_g ( n, r, r0, v )

    use kinddefs, only : dp 

    integer,                intent(in)  :: n
    real(dp), dimension(n), intent(in)  :: r
    real(dp),               intent(in)  :: r0
    real(dp), dimension(n), intent(out) :: v

    real(dp) :: rr02 

    continue

    rr02 = r0**(-2)
    v(:) = exp(-0.5d0*r(:)**2*rr02)

  end subroutine phi_g

end module phi
