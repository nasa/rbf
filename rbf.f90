module rbf

  implicit none

  private 

  public :: get_rbf_weights, get_rbf_interp

contains

  subroutine get_rbf_weights(nspace, ndata, xdata, r0, fdata, w)

    use kinddefs, only : dp
    use phi,      only : phi_tps,  phi_imq
    use svd,      only : solve_svd

    integer,  intent(in)  :: nspace
    integer,  intent(in)  :: ndata
    real(dp), intent(in)  :: xdata(nspace,ndata)
    real(dp), intent(in)  :: r0
    real(dp), intent(in)  :: fdata(ndata)
    real(dp), intent(out) :: w(ndata)
    
    integer  :: i, j
    real(dp) :: a(ndata,ndata), r(ndata), v(ndata)

    do i = 1, ndata
      do j = 1, ndata
        r(j) = sqrt(sum((xdata(:,i)-xdata(:,j))**2))
      end do
      call phi_tps(ndata, r, r0, v)
      !call phi_imq(ndata, r, r0, v)
      a(i,:) = v(:)
    end do
    call solve_svd(ndata, ndata, a, fdata, w)

  end subroutine get_rbf_weights

  subroutine get_rbf_interp(nspace, ndata, xdata, r0, weights, ninterp,       &
                            xinterp, finterp)

    use phi,      only : phi_tps, phi_imq
    use kinddefs, only : dp

    integer,  intent(in)  :: nspace 
    integer,  intent(in)  :: ndata
    real(dp), intent(in)  :: xdata(nspace,ndata)
    integer,  intent(in)  :: ninterp
    real(dp), intent(in)  :: xinterp(nspace,ninterp)
    real(dp), intent(in)  :: r0
    real(dp), intent(in)  :: weights(ndata)
    real(dp), intent(out) :: finterp(ninterp)

    integer :: i, j
    real(dp) :: r(ndata), v(ndata)

    do i = 1, ninterp
      do j = 1, ndata
        r(j) = sqrt(sum((xinterp(:,i) - xdata(:,j))**2))
      end do
      call phi_tps(ndata, r, r0, v)
      !call phi_imq(ndata, r, r0, v)
      finterp(i) = dot_product(v, weights)
    end do

  end subroutine get_rbf_interp

end module rbf
