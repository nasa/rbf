module get_dims

  implicit none

  private

  public :: get_dims_data_point, get_dims_data_elem, get_dims_interp

contains

!======================== get_dims_data_point ================================80

  subroutine get_dims_data_point(funit,n_data)

    use string_utils,   only : geti
 
    implicit none

    integer :: funit, n_data, iostatus
    character(len=2048) :: line, word
   
    continue

    n_data = 0
    rewind(funit)
    do while (iostatus==0)
      read(funit,'(a)',iostat=iostatus) line
      n_data=geti(line,'i')
      if (n_data /= 0) exit
    end do
    if (n_data == 0) stop 'err n_data = 0'
 
  end subroutine get_dims_data_point

!========================= get_dims_data_elem ================================80
  subroutine get_dims_data_elem(funit,n_data,nelem_data)

    use string_utils,   only : geti
 
    implicit none

    integer :: funit, n_data,ic,nelem_data,iostatus
    character(len=2048) :: line,word
 
    continue   
  
    n_data = 0
    nelem_data=0
    rewind(funit)
    do while (iostatus==0)
      read(funit,'(a)',iostat=iostatus) line
      n_data=geti(line,'i')
      nelem_data=geti(line,'j')
      if (n_data /= 0) exit
    end do
    if (n_data == 0 .or. nelem_data == 0) stop 'err dims_data = 0'
  end subroutine get_dims_data_elem

!============================= get_dims_interp ===============================80

  subroutine get_dims_interp(funit,n_interp,nelem_interp)

    use utils,    only : nthword

    integer :: funit, n_interp,ic,nelem_interp
    character(len=2048) :: line,word
 
    continue 

    n_interp = 0
    nelem_interp = 0
    rewind(funit)
    ic = len('zone t')
    do
      read(funit, '(a)',end=1) line
      line = adjustl(line)
      if (line(:ic) == 'zone t') then
        word = nthword(line, 5)
        word = word(3:)
        read(word,*) n_interp
        word = nthword(line, 6)
        word = word(3:)
        read(word,*) nelem_interp
        return
       end if
    end do
    1 continue
    if (n_interp == 0.or.nelem_interp == 0) &
      stop 'err n_interp or nelem_interp = 0'
  end subroutine get_dims_interp
end module get_dims
