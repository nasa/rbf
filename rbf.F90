module rbf

  implicit none

  private 

  public :: get_rbf_weights, get_rbf_interp
  public :: build_rbf_matrix, solve_rbf_lu

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

    do i = 1, ndata
      do j = 1, ndata
        if (.not.(a(i,j)==a(i,j))) print *, 'NaN found at ',i,j
        if (abs(a(i,j)) > 1e20_dp) print *, 'Very large entry at ',i,j, a(i,j)
      end do
    end do

#ifdef USESVD
    call solve_svd(ndata, ndata, a, fdata, w) ! more robust, but much slower
#else
    call solve_rbf_lu(ndata, a, fdata, w)
#endif

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

  subroutine build_rbf_matrix(nspace, ndata, xdata, r0, a)
    use kinddefs, only: dp
    use phi, only: phi_tps
    integer, intent(in) :: nspace, ndata
    real(dp), intent(in) :: xdata(nspace,ndata)
    real(dp), intent(in) :: r0
    real(dp), intent(out) :: a(ndata, ndata)
    integer :: i, j
    real(dp) :: r(ndata), v(ndata)

    do i = 1, ndata
        do j = 1, ndata
            r(j) = sqrt(sum((xdata(:,i)-xdata(:,j))**2))
        end do
        call phi_tps(ndata, r, r0, v)
        a(i,:) = v(:)
    end do
  end subroutine build_rbf_matrix

  subroutine solve_rbf_lu(ndata, a, b, x)
    use kinddefs, only: dp, i4
    implicit none
    integer, intent(in) :: ndata
    real(dp), intent(in) :: a(ndata, ndata)
    real(dp), intent(in) :: b(ndata)
    real(dp), intent(out):: x(ndata)

    integer :: ipiv(ndata), info
    real(dp) :: aa(ndata, ndata), bb(ndata)
    ! Make copies because LAPACK overwrites input arrays
    aa = a
    bb = b
    call dgesv(ndata, 1, aa, ndata, ipiv, bb, ndata, info)
    x = bb
    if (info /= 0) then
        print *, 'ERROR: dgesv failed, info =', info
    end if
  end subroutine solve_rbf_lu


end module rbf
