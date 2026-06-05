module rbf

  implicit none

  private 

  public :: get_rbf_weights, get_rbf_interp
  public :: build_rbf_matrix, solve_rbf_lu, solve_rbf_lsd

contains

  subroutine get_rbf_weights(nspace, ndata, xdata, r0, fdata, w)

    use kinddefs, only : dp
    use phi,      only : phi_tps,  phi_imq
    use svd,      only : solve_svd

    integer ::  info, rank
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
      a(i,:) = v(:)
    end do

    do i = 1, ndata
      do j = 1, ndata
        if (.not.(a(i,j)==a(i,j))) print *, 'NaN found at ',i,j
        if (abs(a(i,j)) > 1e20_dp) print *, 'Very large entry at ',i,j, a(i,j)
      end do
    end do

#if defined(USESVD)
    call solve_svd(ndata, ndata, a, fdata, w)        ! robust, slow
#elif defined(USELSD)
    call solve_rbf_lsd(ndata, a, fdata, w, rank, info)
    if (info /= 0) then
      print *, 'ERROR: dgelsd failed, info =', info, ' falling back to SVD'
      call solve_svd(ndata, ndata, a, fdata, w)
    else if (rank < ndata) then
      print *, 'WARNING: rank-deficient system, rank=', rank, ' / ', ndata
    end if
#else
    call solve_rbf_lu(ndata, a, fdata, w, info)
    if (info /= 0) then
      print *, 'WARNING: dgesv failed, info =', info, ' trying dgelsd'
      call solve_rbf_lsd(ndata, a, fdata, w, rank, info)
      if (info /= 0) then
        print *, 'ERROR: dgelsd also failed, info =', info, ' calling SVD'
        call solve_svd(ndata, ndata, a, fdata, w)
      else if (rank < ndata) then
        print *, 'WARNING: rank-deficient system, rank=', rank, ' / ', ndata
      end if
    end if
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

  subroutine solve_rbf_lu(ndata, a, b, x, info)
    use kinddefs, only: dp
    implicit none
    integer,  intent(in)  :: ndata
    real(dp), intent(in)  :: a(ndata, ndata)
    real(dp), intent(in)  :: b(ndata)
    real(dp), intent(out) :: x(ndata)
    integer,  intent(out) :: info

    integer  :: ipiv(ndata)
    real(dp) :: aa(ndata, ndata), bb(ndata)
    real(dp) :: resid, bnorm, xnorm, tol

    aa = a
    bb = b
    call dgesv(ndata, 1, aa, ndata, ipiv, bb, ndata, info)
    x = bb

    if (info /= 0) return     ! caller will handle fallback

    ! ---- Independent residual check -------------------------------------
    ! Compute ||A*x - b|| / ||b||  using the ORIGINAL a, b (not overwritten)
    bnorm = sqrt(sum(b*b))
    xnorm = sqrt(sum(x*x))
    resid = sqrt(sum((matmul(a, x) - b)**2))
    if (bnorm > 0.0_dp) resid = resid / bnorm

    ! Tolerance: scale by problem size and condition tolerance.
    ! 1e-6 is generous; tighten to 1e-8 or 1e-10 to be stricter.
    tol = 1.0e-6_dp

    print '(a,es12.4,a,es12.4,a,es12.4)',                                       &
          ' dgesv check: ||Ax-b||/||b|| =', resid,                              &
          '  ||x|| =', xnorm, '  ||b|| =', bnorm

    if (resid > tol .or. xnorm > 1.0e12_dp * max(bnorm,1.0_dp)) then
       print *, 'WARNING: dgesv returned info=0 but residual is bad'
       print *, '         flagging as failure (info=-999)'
       info = -999    ! signal caller to use fallback
    end if
  end subroutine solve_rbf_lu

  ! ---------------------------------------------------------------------
  ! New: solve A x = b using LAPACK dgelsd (SVD-based least squares).
  ! Handles rank-deficient / ill-conditioned systems gracefully by
  ! truncating singular values smaller than rcond * sigma_max.
  ! ---------------------------------------------------------------------
  subroutine solve_rbf_lsd(ndata, a, b, x, rank, info)
    use kinddefs, only: dp
    implicit none
    integer,  intent(in)  :: ndata
    real(dp), intent(in)  :: a(ndata, ndata)
    real(dp), intent(in)  :: b(ndata)
    real(dp), intent(out) :: x(ndata)
    integer,  intent(out) :: rank
    integer,  intent(out) :: info

    real(dp), allocatable :: aa(:,:), bb(:), s(:), work(:)
    integer,  allocatable :: iwork(:)
    real(dp) :: rcond, wopt(1)
    integer  :: m, n, nrhs, lda, ldb, lwork, liwork, iwopt(1)

    m    = ndata
    n    = ndata
    nrhs = 1
    lda  = ndata
    ldb  = ndata

    ! Cutoff: singular values < rcond * sigma_max are treated as zero.
    ! 1e-12 is a reasonable default for double precision RBF problems;
    ! tighten or loosen as needed.
    rcond = 1.0e-12_dp

    allocate(aa(lda,n), bb(max(m,n)), s(min(m,n)))
    aa = a
    bb = 0.0_dp
    bb(1:m) = b

    ! ---- Workspace query ----
    lwork  = -1
    liwork = -1
    call dgelsd(m, n, nrhs, aa, lda, bb, ldb, s, rcond, rank,                 &
                wopt, lwork, iwopt, info)
    if (info /= 0) then
      print *, 'dgelsd workspace query failed, info=', info
      deallocate(aa, bb, s)
      return
    end if
    lwork  = int(wopt(1))
    liwork = iwopt(1)
    allocate(work(lwork), iwork(liwork))

    ! ---- Actual solve ----
    call dgelsd(m, n, nrhs, aa, lda, bb, ldb, s, rcond, rank,                 &
                work, lwork, iwork, info)

    if (info == 0) then
      x = bb(1:n)
      print '(a,i0,a,i0,a,es12.4,a,es12.4)',                                  &
            ' dgelsd: rank=', rank, '/', n,                                   &
            '  s_max=', s(1), '  s_min=', s(min(m,n))
    else
      print *, 'dgelsd solve failed, info=', info
    end if

    deallocate(aa, bb, s, work, iwork)
  end subroutine solve_rbf_lsd

end module rbf
