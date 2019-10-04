module svd

  implicit none

  private 
  
  public :: solve_svd

contains 

  subroutine solve_svd ( m, n, a, b, x )

    use kinddefs, only : dp

    integer,                  intent(in)  :: m
    integer,                  intent(in)  :: n
    real(dp), dimension(m,n), intent(in)  :: a
    real(dp), dimension(m),   intent(in)  :: b
    real(dp), dimension(n),   intent(out) :: x

    integer :: i
    integer :: lda, ldu, ldv, lwork, info

    real(dp),              dimension(m,n)        :: s
    real(dp),              dimension(n,m)        :: sp
    real(dp),              dimension(max(m+1,n)) :: sdiag
    real(dp),              dimension(n,m)        :: a_pseudo
    real(dp),              dimension(m,m)        :: u
    real(dp),              dimension(n,n)        :: v
    real(dp), allocatable, dimension(:)          :: work
    integer,  allocatable, dimension(:)          :: iwork

    integer clock1, clock2, clock_rate
    real(dp) time

    continue
    
    lda = m
    ldu = m
    ldv = n

    call system_clock(clock1, clock_rate)
    allocate ( iwork(8*min(m,n)) )
    lwork = 4*min(m,n)**2 + 6*min(m,n) + max(m,n)
    allocate ( work(1:lwork) )
    call dgesdd ( 'A', m, n, a, lda, sdiag, u, ldu, v, ldv, work,              &
                 lwork, iwork, info )
    call system_clock(clock2, clock_rate)
    time = dble(clock2-clock1) / clock_rate
    write(*,'("dgesdd:",g15.7," sec")') time
    if ( info /= 0 ) then
      write ( *, '(a)' ) '  The SVD could not be calculated.'
      write ( *, '(a)' ) '  Try rerunning with different nk.'
      stop
    end if

    s(:,:) = 0.d0
    do i = 1, min ( m, n )
      s(i,i) = sdiag(i)
    end do
    v = transpose ( v )
    sp(:,:) = 0.d0
    do i = 1, min ( m, n )
      if ( s(i,i) /= 0.d0 ) then
        sp(i,i) = 1.d0 / s(i,i)
      end if
    end do
    a_pseudo(:,:) = matmul ( v(:,:), matmul ( sp(:,:), transpose ( u(:,:) ) ) ) 
    x(:) = matmul ( a_pseudo(:,:), b(:) )

  end subroutine solve_svd

end module svd

