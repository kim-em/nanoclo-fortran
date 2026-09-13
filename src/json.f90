! Byte-oriented JSON tokenizer. Tokens and subtree links are reusable SoA
! arrays; no object trees, pointers, external JSON runtime, or fallback parser.
module json
  use base
  implicit none
  integer, parameter :: jobject=1,jarray=2,jstring=3,jnumber=4,jtrue=5,jfalse=6,jnull=7
  type :: json_tokens
    integer(int32), allocatable :: tag(:),first(:),last(:),next(:)
    integer :: count=0,pos=1
  contains
    procedure :: parse
    procedure :: value
    procedure :: string_token
    procedure :: add
    procedure :: field
    procedure :: equals
    procedure :: uint
    procedure :: string => decoded_string
  end type
contains
  subroutine whitespace(bytes,p)
    character, intent(in) :: bytes(:)
    integer, intent(inout) :: p
    do while (p<=size(bytes))
      select case(iachar(bytes(p)))
      case(9,10,13,32)
        p=p+1
      case default
        return
      end select
    end do
  end subroutine
  integer function add(self,tag) result(id)
    class(json_tokens), intent(inout) :: self
    integer, intent(in) :: tag
    id=self%count+1
    call grow(self%tag,id); call grow(self%first,id); call grow(self%last,id); call grow(self%next,id)
    self%count=id; self%tag(id)=tag; self%first(id)=self%pos; self%last(id)=self%pos; self%next(id)=id+1
  end function
  subroutine parse(self,bytes,status)
    class(json_tokens), intent(inout) :: self
    character, intent(in) :: bytes(:)
    type(status_t), intent(inout) :: status
    integer :: root
    self%count=0; self%pos=1
    if (size(bytes,kind=int64)>huge(root)) then
      call status%fail(decline,'JSON record exceeds index range'); return
    end if
    root=self%value(bytes,status,0)
    if (status%code/=accept) return
    call whitespace(bytes,self%pos)
    if (self%pos<=size(bytes)) call status%fail(decline,'trailing JSON data')
  end subroutine
  recursive integer function value(self,bytes,status,depth) result(id)
    class(json_tokens), intent(inout) :: self
    character, intent(in) :: bytes(:)
    type(status_t), intent(inout) :: status
    integer, intent(in) :: depth
    integer :: child,k,begin
    character :: closing
    id=0
    if (status%code/=accept) return
    if (depth>512) then
      call status%fail(decline,'JSON nesting exceeds supported depth'); return
    end if
    call whitespace(bytes,self%pos)
    if (self%pos>size(bytes)) then
      call status%fail(decline,'truncated JSON'); return
    end if
    select case(bytes(self%pos))
    case('{','[')
      closing=']'; k=jarray
      if (bytes(self%pos)=='{') then
        closing='}'; k=jobject
      end if
      id=self%add(k); self%pos=self%pos+1
      call whitespace(bytes,self%pos)
      if (self%pos>size(bytes)) then
        call status%fail(decline,'truncated JSON container'); return
      end if
      if (bytes(self%pos)/=closing) then
        do
          if (k==jobject) then
            child=self%string_token(bytes,status)
            if (status%code/=accept) return
            call whitespace(bytes,self%pos)
            if (self%pos>size(bytes)) then
              call status%fail(decline,'missing JSON colon'); return
            end if
            if (bytes(self%pos)/=':') then
              call status%fail(decline,'missing JSON colon'); return
            end if
            self%pos=self%pos+1
          end if
          child=self%value(bytes,status,depth+1)
          if (status%code/=accept) return
          call whitespace(bytes,self%pos)
          if (self%pos>size(bytes)) then
            call status%fail(decline,'truncated JSON container'); return
          end if
          if (bytes(self%pos)==closing) exit
          if (bytes(self%pos)/=',') then
            call status%fail(decline,'missing JSON comma'); return
          end if
          self%pos=self%pos+1
          call whitespace(bytes,self%pos)
        end do
      end if
      self%pos=self%pos+1
    case('"')
      id=self%string_token(bytes,status); return
    case('t')
      id=self%add(jtrue); call literal(bytes,self%pos,'true',status)
    case('f')
      id=self%add(jfalse); call literal(bytes,self%pos,'false',status)
    case('n')
      id=self%add(jnull); call literal(bytes,self%pos,'null',status)
    case('-','0':'9')
      id=self%add(jnumber); begin=self%pos
      if (bytes(self%pos)=='-') self%pos=self%pos+1
      if (self%pos>size(bytes)) then
        call status%fail(decline,'truncated JSON number'); return
      end if
      if (bytes(self%pos)=='0') then
        self%pos=self%pos+1
      else
        call digits(bytes,self%pos,status)
      end if
      if (self%pos<=size(bytes)) then
        if (bytes(self%pos)=='.') then
          self%pos=self%pos+1; call digits(bytes,self%pos,status)
        end if
      end if
      if (self%pos<=size(bytes)) then
        if (bytes(self%pos)=='e' .or. bytes(self%pos)=='E') then
          self%pos=self%pos+1
          if (self%pos<=size(bytes)) then
            if (bytes(self%pos)=='+' .or. bytes(self%pos)=='-') self%pos=self%pos+1
          end if
          call digits(bytes,self%pos,status)
        end if
      end if
    case default
      call status%fail(decline,'invalid JSON value'); return
    end select
    self%last(id)=self%pos-1; self%next(id)=self%count+1
  end function
  subroutine digits(bytes,p,status)
    character, intent(in) :: bytes(:)
    integer, intent(inout) :: p
    type(status_t), intent(inout) :: status
    integer :: begin
    begin=p
    do while(p<=size(bytes))
      if (bytes(p)<'0' .or. bytes(p)>'9') exit
      p=p+1
    end do
    if (p==begin) call status%fail(decline,'missing JSON digits')
  end subroutine
  subroutine literal(bytes,p,expected,status)
    character, intent(in) :: bytes(:)
    integer, intent(inout) :: p
    character(*), intent(in) :: expected
    type(status_t), intent(inout) :: status
    integer :: i
    if (p+len(expected)-1>size(bytes)) then
      call status%fail(decline,'truncated JSON literal'); return
    end if
    do i=1,len(expected)
      if (bytes(p+i-1)/=expected(i:i)) then
        call status%fail(decline,'invalid JSON literal'); return
      end if
    end do
    p=p+len(expected)
  end subroutine
  integer function string_token(self,bytes,status) result(id)
    class(json_tokens), intent(inout) :: self
    character, intent(in) :: bytes(:)
    type(status_t), intent(inout) :: status
    integer :: i,code,low
    id=0
    call whitespace(bytes,self%pos)
    if (self%pos>size(bytes)) then
      call status%fail(decline,'truncated JSON string'); return
    end if
    if (bytes(self%pos)/='"') then
      call status%fail(decline,'expected JSON string'); return
    end if
    id=self%add(jstring); self%pos=self%pos+1
    do while (self%pos<=size(bytes))
      select case(bytes(self%pos))
      case('"')
        self%last(id)=self%pos; self%pos=self%pos+1; return
      case(achar(92))
        self%pos=self%pos+1
        if (self%pos>size(bytes)) exit
        select case(bytes(self%pos))
        case('"',achar(92),'/','b','f','n','r','t')
          self%pos=self%pos+1
        case('u')
          self%pos=self%pos+1
          code=hex4(bytes,self%pos,status)
          if (status%code/=accept) return
          if (code>=55296 .and. code<=56319) then
            call literal(bytes,self%pos,achar(92)//'u',status)
            if (status%code/=accept) return
            low=hex4(bytes,self%pos,status)
            if (low<56320 .or. low>57343) call status%fail(decline,'invalid Unicode surrogate pair')
          else if (code>=56320 .and. code<=57343) then
            call status%fail(decline,'unpaired Unicode surrogate')
          end if
          if (status%code/=accept) return
        case default
          call status%fail(decline,'invalid JSON escape'); return
        end select
      case default
        i=iachar(bytes(self%pos))
        if (i<32) then
          call status%fail(decline,'control byte in JSON string'); return
        end if
        ! Validate raw UTF-8, rejecting overlong, surrogate, and out-of-range encodings.
        call utf8_skip(bytes,self%pos,status)
        if (status%code/=accept) return
      end select
    end do
    call status%fail(decline,'unterminated JSON string')
  end function
  subroutine utf8_skip(bytes,p,status)
    character, intent(in) :: bytes(:)
    integer, intent(inout) :: p
    type(status_t), intent(inout) :: status
    integer :: b,n,i,cp,mincp
    b=iachar(bytes(p)); n=0; cp=b; mincp=0
    if (b>=194 .and. b<=223) then
      n=1; cp=b-192; mincp=128
    else if (b>=224 .and. b<=239) then
      n=2; cp=b-224; mincp=2048
    else if (b>=240 .and. b<=244) then
      n=3; cp=b-240; mincp=65536
    else if (b>=128) then
      call status%fail(decline,'invalid UTF-8 leading byte'); return
    end if
    if (p+n>size(bytes)) then
      call status%fail(decline,'truncated UTF-8'); return
    end if
    do i=1,n
      b=iachar(bytes(p+i))
      if (b<128 .or. b>191) then
        call status%fail(decline,'invalid UTF-8 continuation'); return
      end if
      cp=cp*64+b-128
    end do
    if (cp<mincp .or. cp>1114111 .or. (cp>=55296 .and. cp<=57343)) then
      call status%fail(decline,'invalid UTF-8 code point'); return
    end if
    p=p+n+1
  end subroutine
  integer function hex4(bytes,p,status) result(code)
    character, intent(in) :: bytes(:)
    integer, intent(inout) :: p
    type(status_t), intent(inout) :: status
    integer :: i,d
    code=0
    if (p+3>size(bytes)) then
      call status%fail(decline,'truncated Unicode escape'); return
    end if
    do i=1,4
      select case(bytes(p))
      case('0':'9')
        d=iachar(bytes(p))-48
      case('a':'f')
        d=iachar(bytes(p))-87
      case('A':'F')
        d=iachar(bytes(p))-55
      case default
        call status%fail(decline,'invalid Unicode escape'); return
      end select
      code=16*code+d; p=p+1
    end do
  end function
  logical function equals(self,bytes,id,text) result(ok)
    class(json_tokens), intent(in) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: id
    character(*), intent(in) :: text
    integer :: i
    ok=.false.
    if (id<1 .or. id>self%count) return
    if (self%tag(id)/=jstring) return
    if (self%last(id)-self%first(id)-1/=len(text)) return
    do i=1,len(text)
      if (bytes(self%first(id)+i)/=text(i:i)) return
    end do
    ok=.true.
  end function
  integer function field(self,bytes,obj,key,status) result(id)
    class(json_tokens), intent(in) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: obj
    character(*), intent(in) :: key
    type(status_t), intent(inout) :: status
    integer :: i
    id=0
    if (status%code/=accept) return
    if (obj<1 .or. obj>self%count) then
      call status%fail(decline,'missing JSON object'); return
    end if
    if (self%tag(obj)/=jobject) then
      call status%fail(decline,'expected JSON object'); return
    end if
    i=obj+1
    do while(i<self%next(obj))
      if (self%equals(bytes,i,key)) then
        if (id/=0) then
          call status%fail(decline,'duplicate JSON field: '//key); return
        end if
        id=i+1
      end if
      i=self%next(i+1)
    end do
  end function
  integer(int64) function uint(self,bytes,id,status,limit) result(n)
    class(json_tokens), intent(in) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: id
    type(status_t), intent(inout) :: status
    integer(int64), optional, intent(in) :: limit
    integer(int64) :: bound
    integer :: i,d
    n=0; bound=2147483646_int64
    if (present(limit)) bound=limit
    if (status%code/=accept) return
    if (id<1 .or. id>self%count) then
      call status%fail(decline,'missing integer'); return
    end if
    if (self%tag(id)/=jnumber) then
      call status%fail(decline,'expected integer'); return
    end if
    do i=self%first(id),self%last(id)
      d=iachar(bytes(i))-48
      if (d<0 .or. d>9) then
        call status%fail(decline,'expected unsigned integer'); return
      end if
      if (n>(bound-d)/10 .or. int(d,int64)>bound) then
        call status%fail(decline,'integer outside supported range'); return
      end if
      n=n*10+d
    end do
  end function
  function decoded_string(self,bytes,id,status) result(text)
    class(json_tokens), intent(in) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: id
    type(status_t), intent(inout) :: status
    character(:), allocatable :: text,buffer
    integer :: p,n,cp,low
    text=''
    if (status%code/=accept) return
    if (id<1 .or. id>self%count) then
      call status%fail(decline,'missing string'); return
    end if
    if (self%tag(id)/=jstring) then
      call status%fail(decline,'expected string'); return
    end if
    allocate(character(self%last(id)-self%first(id)-1) :: buffer)
    p=self%first(id)+1; n=0
    do while(p<self%last(id))
      cp=iachar(bytes(p)); p=p+1
      if (cp/=92) then
        n=n+1; buffer(n:n)=achar(cp); cycle
      end if
      select case(bytes(p))
      case('b')
        cp=8
      case('f')
        cp=12
      case('n')
        cp=10
      case('r')
        cp=13
      case('t')
        cp=9
      case('u')
        p=p+1; cp=hex4(bytes,p,status)
        if (cp>=55296 .and. cp<=56319) then
          p=p+2; low=hex4(bytes,p,status)
          cp=65536+(cp-55296)*1024+low-56320
        end if
        call append_utf8(buffer,n,cp)
        cycle
      case default
        cp=iachar(bytes(p))
      end select
      p=p+1; n=n+1; buffer(n:n)=achar(cp)
    end do
    text=buffer(:n)
  end function
  subroutine append_utf8(buffer,n,cp)
    character(*), intent(inout) :: buffer
    integer, intent(inout) :: n
    integer, intent(in) :: cp
    if (cp<128) then
      buffer(n+1:n+1)=achar(cp); n=n+1
    else if (cp<2048) then
      buffer(n+1:n+2)=achar(192+cp/64)//achar(128+mod(cp,64)); n=n+2
    else if (cp<65536) then
      buffer(n+1:n+3)=achar(224+cp/4096)//achar(128+mod(cp/64,64))//achar(128+mod(cp,64)); n=n+3
    else
      buffer(n+1:n+4)=achar(240+cp/262144)//achar(128+mod(cp/4096,64))// &
        achar(128+mod(cp/64,64))//achar(128+mod(cp,64)); n=n+4
    end if
  end subroutine
end module
