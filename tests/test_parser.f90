program test_parser
  use parser
  implicit none
  type(json_tokens) :: tokens
  type(string_arena) :: pool
  type(status_t) :: status
  character, allocatable :: bytes(:)
  character(:), allocatable :: text
  integer :: i,k,id,checks=0
  call good('{"x":[null,true,false,-1.25e+2],"s":"\uD83D\uDE00\n\u0000"}')
  id=tokens%field(bytes,1,'s',status)
  text=tokens%string(bytes,id,status)
  call check(text==char(240)//char(159)//char(152)//char(128)//char(10)//char(0),'Unicode decode')
  k=pool%intern(text)
  call check(pool%intern(text)==k,'string interning')
  call check(pool%text(k)==text,'byte string roundtrip')
  call good('{"s":"é𝄞","n":4294967295}')
  id=tokens%field(bytes,1,'n',status)
  call check(tokens%uint(bytes,id,status,4294967295_int64)==4294967295_int64,'uint32 maximum')
  call good('{"x":1,"x":2}')
  id=tokens%field(bytes,1,'x',status)
  call check(status%code==decline,'duplicate queried field')
  call bad('')
  call bad('{')
  call bad('[')
  call bad('{"a"}')
  call bad('{"a":}')
  call bad('[1,]')
  call bad('{"a":1,}')
  call bad('true false')
  call bad('01')
  call bad('-')
  call bad('1.')
  call bad('1e+')
  call bad('"\x"')
  call bad('"\u123"')
  call bad('"\uD800"')
  call bad('"\uDC00"')
  call bad('"\uD800\u0000"')
  call bad('"'//char(0)//'"')
  call bad('"'//char(192)//char(128)//'"')
  call bad('"'//char(237)//char(160)//char(128)//'"')
  call bad('"'//char(244)//char(144)//char(128)//char(128)//'"')
  call bad('"'//char(240)//char(159)//'"')
  call good('2147483647')
  id=int(tokens%uint(bytes,1,status))
  call check(status%code==decline,'index overflow')
  call good('9223372036854775808')
  id=int(tokens%uint(bytes,1,status,huge(0_int64)))
  call check(status%code==decline,'int64 overflow')
  call good('[1,2,{"z":[]}]')
  call check(tokens%next(1)==tokens%count+1,'subtree successor')
  do i=1,2000
    block
      character(32) :: number
      write(number,'(i0)') i
      id=pool%intern(trim(number))
      call check(pool%text(id)==trim(number),'string pool growth')
    end block
  end do
  print '(a,i0,a)', 'parser: ',checks,' checks passed'
contains
  subroutine check(ok,label)
    logical, intent(in) :: ok
    character(*), intent(in) :: label
    if (.not.ok) then
      print *, 'FAIL: ',label
      if (allocated(status%message)) print *,status%message
      error stop 1
    end if
    checks=checks+1
  end subroutine
  subroutine good(s)
    character(*), intent(in) :: s
    integer :: j
    status=status_t()
    if (allocated(bytes)) deallocate(bytes)
    allocate(bytes(len(s)))
    do j=1,len(s)
      bytes(j)=s(j:j)
    end do
    call tokens%parse(bytes,status)
    call check(status%code==accept,'valid JSON '//s)
  end subroutine
  subroutine bad(s)
    character(*), intent(in) :: s
    integer :: j
    status=status_t()
    if (allocated(bytes)) deallocate(bytes)
    allocate(bytes(len(s)))
    do j=1,len(s)
      bytes(j)=s(j:j)
    end do
    call tokens%parse(bytes,status)
    call check(status%code==decline,'malformed JSON '//s)
  end subroutine
end program
