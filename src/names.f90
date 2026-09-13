! Translation/layout modification of nanoclo src/name.rs, pinned in NOTICE.
module names
  use base
  use hash_table
  implicit none
  integer(int32), parameter :: name_anon=0, name_str=1, name_num=2, anonymous=1
  type :: name_arena
    integer(int32), allocatable :: tag(:),a(:),b(:),c(:)
    integer(int32) :: count=0
    type(table_t) :: nodes
  contains
    procedure :: init => names_init
    procedure :: intern => name_intern
    procedure :: str => name_string
    procedure :: num => name_number
    procedure :: concat => concat_name
    procedure :: replace_pfx
    procedure :: get_pfx
  end type
contains
  subroutine names_init(self)
    class(name_arena), intent(inout) :: self
    integer :: id
    id=self%intern(name_anon,0,0,0)
  end subroutine
  function name_intern(self,tag,a,b,c) result(id)
    class(name_arena), intent(inout) :: self
    integer(int32), intent(in) :: tag,a,b,c
    integer(int32) :: id
    include 'intern_node.inc'
  end function
  function name_string(self,prefix,string_id) result(id)
    class(name_arena), intent(inout) :: self
    integer(int32), intent(in) :: prefix,string_id
    integer(int32) :: id
    id=self%intern(name_str,prefix,string_id,0)
  end function
  function name_number(self,prefix,lo,hi) result(id)
    class(name_arena), intent(inout) :: self
    integer(int32), intent(in) :: prefix,lo
    integer(int32), optional, intent(in) :: hi
    integer(int32) :: id,h
    h=0
    if (present(hi)) h=hi
    id=self%intern(name_num,prefix,lo,h)
  end function
  recursive function concat_name(self,n1,n2) result(id)
    class(name_arena), intent(inout) :: self
    integer(int32), intent(in) :: n1,n2
    integer(int32) :: id,p,t,b,c
    t=self%tag(n2); p=self%a(n2); b=self%b(n2); c=self%c(n2)
    if (t==name_anon) then
      id=n1
    else
      p=self%concat(n1,p)
      id=self%intern(t,p,b,c)
    end if
  end function
  recursive function replace_pfx(self,n,outgoing,incoming) result(id)
    class(name_arena), intent(inout) :: self
    integer(int32), intent(in) :: n,outgoing,incoming
    integer(int32) :: id,p,t,b,c
    if (n==outgoing) then
      id=incoming; return
    end if
    t=self%tag(n); p=self%a(n); b=self%b(n); c=self%c(n)
    if (t==name_anon) then
      id=anonymous
    else
      p=self%replace_pfx(p,outgoing,incoming)
      id=self%intern(t,p,b,c)
    end if
  end function
  function get_pfx(self,n) result(id)
    class(name_arena), intent(in) :: self
    integer(int32), intent(in) :: n
    integer(int32) :: id
    id=n
    do while (self%tag(id)/=name_anon)
      if (self%a(id)==anonymous) return
      id=self%a(id)
    end do
  end function
end module
