module string_utils

  implicit none

  private

  public :: char2int, downcase, geti

  logical, public :: error_condition

  integer, public, parameter :: MAX_STR_LEN = 1024

  integer, parameter :: MAX_INT_WIDTH = 19 ! digits in 2**62

contains

!=============================== CHAR2INT ====================================80
!
! Convert character string to an integer
!
!  print*, char2int('123') !=> 123
!  print*, char2int('-1')  !=> -1
!  print*, char2int('7c')  !=> 7, error_condition = .true.
!  print*, char2int('1.2') !=> 1, error_condition = .true.
!  print*, char2int('a6')  !=> 0, error_condition = .true.
!
!=============================================================================80

  function char2int( string )

    integer                  :: char2int
    character(*), intent(in) :: string

    character(len_trim(adjustl(string))) :: sstring

    integer :: first_nondigit_index, io_status

    continue

    error_condition = .false.

    sstring = trim(adjustl(string))

    first_nondigit_index = verify( sstring, '+-0123456789' )

    select case (first_nondigit_index)
    case (0)  ! attempt conversion of entire string
      first_nondigit_index = len(sstring) + 1
    case (1:) ! trailing non-digits or no digits
      if ( first_nondigit_index <= len(sstring) ) error_condition = .true.
    end select

    read(sstring(1:first_nondigit_index-1),*,iostat=io_status) char2int

    if ( io_status /= 0 ) then ! no leading digits found or integer too big
      error_condition = .true.
      char2int = 0
    end if

  end function char2int

!=================================== DOWNCASE ================================80
!
! Convert upper case letters in a string to lower case letters.
!
!  print*, downcase('sTRING') !=> 'string'
!
!=============================================================================80

  function downcase(string)

    character(*), intent(in) :: string
    character(len(string))   :: downcase

    character(1) :: c
    integer      :: i

    continue

    downcase = string

    do i = 1,len(string)
      c = string(i:i)
      if ( c >= 'A' .and. c <= 'Z') downcase(i:i) = achar(iachar(c) + 32)
    end do

  end function downcase

!=================================== GETI ====================================80
!
! Grab the integer after a 'key_char = '
!
!  print*, geti( 'i = 1', 'i' )      !=> 1
!  print*, geti( 'J=213, K=7', 'j' ) !=> 213
!  print*, geti( '8', 'j' )          !=> 0
!
!=============================================================================80

  function geti( string, key )

    integer                   :: geti
    character(*), intent(in)  :: string
    character(*), intent(in)  :: key

    integer :: i, slen

    continue

    geti = 0

    i = index( downcase(string), downcase(key) )

    if ( i == 0 ) return

    i  = i + len(key)

    slen = len(string)

    if ( scan(adjustl(string(i:slen)),'=') == 1 ) then

      i = i + scan(string(i:slen),'=')
      geti = char2int(string(i:slen))

    else ! try looking for another occurrance of key FIXME: make recursive!

      i = index( downcase(string(i:slen)), downcase(key) ) + i - 1
      if ( i == 0 ) return
      i  = i + len(key)
      if ( scan(adjustl(string(i:slen)),'=') /= 1 ) return ! give up
      i = i + scan(string(i:slen),'=')
      geti = char2int(string(i:slen))

    end if

  end function geti

end module string_utils
