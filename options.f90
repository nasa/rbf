! commandline option parser 

module options

  implicit none

  private

  public :: parse_options

contains

!============================== PARSE_OPTIONS ================================80

  subroutine parse_options(ver, filename_source, filename_dest, &
                          filename_interp, filename_primary,    &
                          filename_prune, length_keep,          &
                          percent_keep, n_target, sym_flag,     &
                          l_blend, ignore_zero, spring_connections)
    use kinddefs, only : dp
    use usage,    only : help

    integer,     dimension(3) :: ver
    integer                   :: i, ii, n_target, percent_keep, sym_flag
    character(*)              :: filename_source
    character(*)              :: filename_dest
    character(*)              :: filename_interp
    character(*)              :: filename_primary
    character(*)              :: filename_prune
    character(len=2048)       :: word, wordtmp
    real(dp)                  :: l_blend, length_keep
    logical                   :: ignore_zero, spring_connections
  
  continue 

    filename_source=''
    filename_dest=''
    filename_interp=''
    filename_primary=''
    filename_prune=''
    n_target=0
    length_keep = -1.d0
    percent_keep = 0
    l_blend = 0.d0
    sym_flag = 0
    ignore_zero = .false.
    spring_connections = .false. 
    ii = 1
    do i = 1, command_argument_count()
      call get_command_argument(ii, word)
      if (word(:3).eq.'-cs') then
        spring_connections = .true. 
      else if (word(:2).eq.'-s') then
        ii = ii + 1
        call get_command_argument(ii, filename_source)
      else if (word(:2).eq.'-d') then
        ii = ii + 1
        call get_command_argument(ii, filename_dest)
      else if (word(:3).eq.'-iz') then 
        ignore_zero = .true. 
      else if (word(:2).eq.'-i') then
        ii = ii + 1
        call get_command_argument(ii, filename_interp)
      else if (word(:3).eq.'-wp') then
        ii = ii + 1
        call get_command_argument(ii, filename_prune)
      else if (word(:3).eq.'-lk') then
        ii = ii + 1
        call get_command_argument(ii, wordtmp)
        read(wordtmp,*) length_keep
      else if (word(:3).eq.'-pk') then
        ii = ii + 1
        call get_command_argument(ii, wordtmp)
        read(wordtmp,*) percent_keep
      else if (word(:3).eq.'-nk') then
        ii = ii + 1
        call get_command_argument(ii, wordtmp)
        read(wordtmp,*) n_target
      else if (word(:2).eq.'-p') then
        ii = ii + 1
        call get_command_argument(ii, filename_primary)
      else if (word(:2).eq.'-b') then
        ii = ii + 1
        call get_command_argument(ii, wordtmp)
        read(wordtmp,*) l_blend
! x symmetry bc with l_blend transition region
      else if (word(:2).eq.'-x') then 
        ii = ii + 1
        sym_flag=1
        call get_command_argument(ii, wordtmp)
        read(wordtmp,*) l_blend
! y symmetry bc with l_blend transition region
      else if (word(:2).eq.'-y') then 
        ii = ii + 1
        sym_flag=2
        call get_command_argument(ii, wordtmp)
        read(wordtmp,*) l_blend
! z symmetry bc with l_blend transition region
      else if (word(:2).eq.'-z') then 
        ii = ii + 1
        sym_flag=3
        call get_command_argument(ii, wordtmp)
        read(wordtmp,*) l_blend
      end if
      ii = ii + 1
    end do

    if (.not.spring_connections.and.                                         &
       (filename_source=='' .or. filename_dest=='' .or. filename_interp==''))&
      call help(ver)

    if ( (.not.(filename_primary=='')) .and. (l_blend == 0.d0) ) then
      write(*,*) 'WARNING: currrent blending distance between primary '//    &
        'and main surface is 0.' 
      write(*,*) 'Use -b BLEND_DISTANCE to set.'
    end if 

    if ( (.not.(filename_primary=='')) .and. (sym_flag /= 0) ) then
      write(*,*) 'ERROR: -p and -x or -y or -z are mutually exclusive. '//   &
        'Use -b with -p.' 
      call help(ver)
    end if
    
  end subroutine parse_options

end module options
