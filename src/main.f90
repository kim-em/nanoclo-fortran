program nanoclo_fortran
  use base
  use kernel, only: checker
  implicit none
  type(checker) :: ck
  character(:), allocatable :: path,arg
  integer :: n,i,ios,jobs
  logical :: scan_only
  scan_only=.false.
  path=''
  do i=1,command_argument_count()
    call get_command_argument(i,length=n)
    allocate(character(n) :: arg)
    call get_command_argument(i,arg)
    if (arg=='--scan') then
      scan_only=.true.
    else if (index(arg,'--jobs=')==1) then
      read(arg(8:),*,iostat=ios) jobs
      if (ios==0) then
        if (jobs<1 .or. jobs>256) ios=1
      end if
      if (ios/=0) then
        call ck%status%fail(kernel_error,'--jobs must be between 1 and 256')
      else
        ck%jobs=jobs
      end if
    else if (index(arg,'--nat-extension=')==1) then
      select case(arg(17:))
      case('true','1'); ck%nat_extension=.true.
      case('false','0'); ck%nat_extension=.false.
      case default; call ck%status%fail(kernel_error,'--nat-extension expects true or false')
      end select
    else if (index(arg,'--string-extension=')==1) then
      select case(arg(20:))
      case('true','1'); ck%string_extension=.true.
      case('false','0'); ck%string_extension=.false.
      case default; call ck%status%fail(kernel_error,'--string-extension expects true or false')
      end select
    else if (index(arg,'--axioms=')==1) then
      select case(arg(10:))
      case('all'); ck%standard_axioms=.false.
      case('standard'); ck%standard_axioms=.true.
      case default; call ck%status%fail(kernel_error,'--axioms expects all or standard')
      end select
    else if (arg=='--help') then
      print '(a)', 'usage: nanoclo-fortran [--scan] [--jobs=N] FILE'
      print '(a)', '--nat-extension=true|false --string-extension=true|false --axioms=all|standard'
      print '(a)', '--scan parses expression/name/level records and counts declarations; it does not check proofs.'
      stop
    else if (len(path)==0 .and. len(arg)>0) then
      if (arg(1:1)=='-') then
        call ck%status%fail(kernel_error,'unknown option: '//arg)
      else
        path=arg
      end if
    else
      call ck%status%fail(kernel_error,'expected one input path')
    end if
    deallocate(arg)
  end do
  if (len(path)==0) call ck%status%fail(kernel_error,'missing input path')
  ck%ctx%nat_extension=ck%nat_extension; ck%ctx%string_extension=ck%string_extension
  if (ck%status%code==accept) call ck%ctx%scan_file(path,ck%status)
  if (ck%status%code==accept) then
    if (scan_only) then
      print '(a,i0)', 'records=',ck%ctx%records
      print '(a,i0)', 'names=',ck%ctx%name_records
      print '(a,i0)', 'levels=',ck%ctx%level_records
      print '(a,i0)', 'expressions=',ck%ctx%expr_records
      print '(a,i0)', 'declarations=',ck%ctx%decl_records
      print '(a,i0)', 'interned_names=',ck%ctx%names%count
      print '(a,i0)', 'interned_levels=',ck%ctx%levels%count
      print '(a,i0)', 'interned_expressions=',ck%ctx%exprs%count
    else
      call ck%check_all()
    end if
  end if
  if (ck%status%code/=accept) then
    write(error_unit,'(a)') ck%status%message
    stop ck%status%code, quiet=.true.
  end if
end program
