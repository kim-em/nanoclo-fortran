module strings
  use base
  use hash_table
  implicit none
  type :: string_arena
    integer(int32), allocatable :: bytes(:),start(:),length(:),next(:)
    integer(int32) :: count=0,used=0
    type(table_t) :: buckets
  contains
    procedure :: intern => string_intern
    procedure :: text => string_text
  end type
contains
  function string_intern(self,text) result(id)
    class(string_arena), intent(inout) :: self
    character(*), intent(in) :: text
    integer(int32) :: id,h,head,i
    h=0
    do i=1,len(text)
      h=hash_words(h,iachar(text(i:i)),0,0)
    end do
    head=self%buckets%get(h,len(text),0,0)
    id=head
    do while(id/=0)
      do i=1,len(text)
        if (self%bytes(self%start(id)+i-1)/=iachar(text(i:i))) exit
      end do
      if (i>len(text)) return
      id=self%next(id)
    end do
    if (int(self%used,int64)+len(text)>huge(id)) error stop kernel_error
    id=self%count+1
    call grow(self%start,id); call grow(self%length,id); call grow(self%next,id)
    call grow(self%bytes,self%used+len(text))
    self%start(id)=self%used+1; self%length(id)=len(text); self%next(id)=head
    do i=1,len(text)
      self%bytes(self%used+i)=iachar(text(i:i))
    end do
    self%used=self%used+len(text); self%count=id
    call self%buckets%put(h,len(text),0,0,id)
  end function
  function string_text(self,id) result(text)
    class(string_arena), intent(in) :: self
    integer(int32), intent(in) :: id
    character(:), allocatable :: text
    integer :: i
    allocate(character(self%length(id)) :: text)
    do i=1,len(text)
      text(i:i)=achar(self%bytes(self%start(id)+i-1))
    end do
  end function
end module
