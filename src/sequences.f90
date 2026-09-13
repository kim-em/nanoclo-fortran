! Interned integer lists, used for universe argument lists and other variable
! sized payloads. Node one is nil; list equality is index equality.
module sequences
  use base
  use hash_table
  implicit none
  integer(int32), parameter :: list_nil=1
  type :: sequence_arena
    integer(int32), allocatable :: head(:),tail(:),length(:)
    integer(int32) :: count=0
    type(table_t) :: nodes
  contains
    procedure :: init
    procedure :: cons
    procedure :: from_array
  end type
contains
  subroutine init(self)
    class(sequence_arena), intent(inout) :: self
    if (self%count/=0) return
    call grow(self%head,1); call grow(self%tail,1); call grow(self%length,1)
    self%head(1)=0; self%tail(1)=1; self%length(1)=0; self%count=1
  end subroutine
  function cons(self,head,tail) result(id)
    class(sequence_arena), intent(inout) :: self
    integer(int32), intent(in) :: head,tail
    integer(int32) :: id
    id=self%nodes%get(head,tail,0,0)
    if (id/=0) return
    id=self%count+1
    call grow(self%head,id); call grow(self%tail,id); call grow(self%length,id)
    self%head(id)=head; self%tail(id)=tail; self%length(id)=self%length(tail)+1; self%count=id
    call self%nodes%put(head,tail,0,0,id)
  end function
  function from_array(self,items) result(id)
    class(sequence_arena), intent(inout) :: self
    integer(int32), intent(in) :: items(:)
    integer(int32) :: id,i
    id=list_nil
    do i=size(items),1,-1
      id=self%cons(items(i),id)
    end do
  end function
end module
