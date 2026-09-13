! Struct-of-arrays translation of nanoclo nbe.rs SpineNode and its interners.
module spines
  use base
  use hash_table
  implicit none
  integer(int32), parameter :: spine_empty=1,elim_app=1,elim_proj=2
  type :: spine_arena
    integer(int32), allocatable :: tag(:),a(:),b(:),parent(:),len(:)
    integer(int32) :: count=0
    type(table_t) :: spine_intern_app,spine_intern_proj
  contains
    procedure :: init
    procedure :: snoc
    procedure :: get
    procedure :: reset_decl
  end type
contains
  subroutine init(self)
    class(spine_arena), intent(inout) :: self
    if (self%count/=0) return
    call grow(self%tag,1); call grow(self%a,1); call grow(self%b,1); call grow(self%parent,1); call grow(self%len,1)
    self%tag(1)=elim_proj; self%a(1)=0; self%b(1)=0; self%parent(1)=1; self%len(1)=0; self%count=1
  end subroutine
  subroutine reset_decl(self)
    class(spine_arena), intent(inout) :: self
    self%count=1
    call self%spine_intern_app%clear(); call self%spine_intern_proj%clear()
  end subroutine
  integer function snoc(self,parent,tag,a,b) result(id)
    class(spine_arena), intent(inout) :: self
    integer(int32), intent(in) :: parent,tag,a,b
    select case(tag)
    case(elim_app)
      id=self%spine_intern_app%get(parent,a,0,0)
    case(elim_proj)
      id=self%spine_intern_proj%get(parent,a,b,0)
    case default
      error stop kernel_error
    end select
    if (id/=0) return
    id=self%count+1
    call grow(self%tag,id); call grow(self%a,id); call grow(self%b,id); call grow(self%parent,id); call grow(self%len,id)
    self%tag(id)=tag; self%a(id)=a; self%b(id)=b; self%parent(id)=parent; self%len(id)=self%len(parent)+1
    self%count=id
    if (tag==elim_app) then
      call self%spine_intern_app%put(parent,a,0,0,id)
    else
      call self%spine_intern_proj%put(parent,a,b,0,id)
    end if
  end function
  integer function get(self,spine,i) result(node)
    class(spine_arena), intent(in) :: self
    integer(int32), intent(in) :: spine,i
    integer :: steps
    node=0
    if (i<0 .or. i>=self%len(spine)) return
    steps=self%len(spine)-i-1; node=spine
    do while(steps>0)
      node=self%parent(node); steps=steps-1
    end do
  end function
end module
