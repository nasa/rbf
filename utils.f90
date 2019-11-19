module utils

  implicit none

  private

  public :: nwords, nthword, read_to_word, distsq, onlist, nlines, quicksort

contains

! Return number of words in a phrase
  function nwords(string)
    character(len=*), intent(in)  :: string
    integer :: pos1, pos2, pe
    integer :: i1,i2,i3,i4 
    integer :: nwords
  ! initialize the null and tab characters.
    pe = len_trim(string) 
    pos1 = 1
    nwords = 0
    if (pe == 0) return
    do 
      i1=index(string(pos1:pe), ",")
      i2=index(string(pos1:pe), " ")
      i3=index(string(pos1:pe), achar(0))
      i4=index(string(pos1:pe), achar(9))
      if (i1 == 0 ) i1 = pe+1
      if (i2 == 0 ) i2 = pe+1
      if (i3 == 0 ) i3 = pe+1
      if (i4 == 0 ) i4 = pe+1
      if (min(i1,i2,i3,i4) > pe) then
         nwords = nwords + 1
         exit
      end if
      pos2=min(i1,i2,i3,i4) 
      if (pos2 > 1 ) then
        nwords = nwords + 1
      end if
      pos1 = pos2+pos1
    end do 
    return
  end function nwords

!  Return nth word in a phrase 
  function nthword(string,nth)
    character(len=*), intent(in)  :: string
    integer,          intent(in)  :: nth
    integer                       :: pos1, pos2, pe
    integer                       :: i1,i2,i3,i4 
    integer                       :: nwords
    character(len_trim(string))   :: nthword
  ! initialize the null and tab characters.
    pe = len_trim(string) 
    pos1 = 1
    nwords = 0
    if(pe == 0) then
      nthword = ""
      return
    end if
    do 
      i1=index(string(pos1:pe), ",")
      i2=index(string(pos1:pe), " ")
      i3=index(string(pos1:pe), achar(0))
      i4=index(string(pos1:pe), achar(9))
      if (i1 == 0 ) i1 = pe+1
      if (i2 == 0 ) i2 = pe+1
      if (i3 == 0 ) i3 = pe+1
      if (i4 == 0 ) i4 = pe+1
      if (min(i1,i2,i3,i4) > pe) then
         nwords = nwords + 1
         nthword=string(pos1:pe)
         if(nwords == nth) return
         exit
      end if
      pos2=min(i1,i2,i3,i4) 
      if (pos2 > 1 ) then
        nwords = nwords + 1
        nthword=string(pos1:pos1+pos2-2)
        if(nwords == nth) return
      end if
      pos1 = pos2+pos1
    end do 
    return
  end function nthword

!  Return next line with word in it.
  function read_to_word(iunit,word)
    character(len=*),   intent(in) :: word
    character(len=256)             :: line, read_to_word
    integer :: iunit,n

    do
      read(iunit,'(a)',end=1) line
      do n = 1, nwords(line) 
        if (trim(nthword(line,n)) == trim(word)) then
           read_to_word = line
           return
        end if
      end do
    end do
    1 continue
    read_to_word='unexpected end of file'
    return
  end function read_to_word

! Compute squared distance between two points.
  function distsq ( x1, y1, z1, x2, y2, z2)
    use kinddefs, only : dp
    real (dp) x1, y1, z1, x2, y2, z2, distsq
    distsq = (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2
  return
  end function distsq

! Return .true. if node already on nodelist.
  function onlist (node, listsize, nodelist)
    implicit none
    logical :: onlist
    integer :: node, listsize, i
    integer :: nodelist(listsize)
    onlist = .false.
    do i = 1, listsize
      if (node == nodelist(i)) then
        onlist = .true.
        return
      end if
    end do
    return 
  end function onlist

! Return number of non blank lines in file funit
  function nlines(funit)
    implicit none
    integer :: nlines, iostatus
    integer, intent(in) :: funit
    character(len=256)  :: line
    nlines = 0
    rewind(funit)
    iostatus=0
    do while (iostatus==0)
      read(funit,'(a)',iostat=iostatus) line
      if ((len_trim(line)).gt.0.and.(iostatus==0)) then
        nlines = nlines + 1
      end if 
    end do
    return
  end function nlines


  recursive subroutine quicksort(a, lo, hi)

    use kinddefs,       only : dp

    implicit none

    real (dp) :: a(*), pivot, tmp
    integer   :: lo, hi
    integer   :: i, j

    pivot = a( ( lo + hi ) / 2 )
    i = lo
    j = hi

    do
      do while ( a(i) < pivot )
        i = i + 1
      end do
      do while ( pivot < a(j) )
        j = j - 1
      end do
      if ( i >= j ) exit
        tmp = a(i)
        a(i) = a(j)
        a(j) = tmp
        i = i + 1
        j = j - 1
    end do

    if (lo < i - 1) call quicksort(a, lo, i - 1)
    if ( j + 1 < hi) call quicksort(a, j + 1, hi)

  end subroutine quicksort

end module utils
