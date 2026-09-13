! SoA translation of nanoclo InductiveCheckState and IndTyHeader/CtorHeader.
module inductive_state
  use base
  implicit none
  type :: inductive_check_state
    integer(int32), allocatable :: name(:),ty(:),ctors(:),indices(:),ind_const(:),major(:),motive(:),minors(:)
    integer(int32), allocatable :: ctor_name(:),ctor_ty(:)
    integer :: count=0,ctor_count=0,uparams=1,num_params=0,local_params=1,block_codom=0
    integer(int32), allocatable :: nested_name(:),nested_expr(:),nested_closed(:),rec_from(:),rec_to(:)
    integer :: base_count=0,base_names=1,nested_count=0,next_name=1
    integer :: rec_uparams=1,elim_level=0,all_names=1
    logical :: is_zero=.false.,is_nonzero=.false.,k_target=.false.,nested=.false.
  contains
    procedure :: add_type
    procedure :: add_ctor
    procedure :: reset
  end type
contains
  subroutine reset(self)
    class(inductive_check_state), intent(inout) :: self
    self%base_count=0; self%base_names=1; self%nested_count=0; self%next_name=1
    self%count=0; self%ctor_count=0; self%uparams=1; self%num_params=0; self%local_params=1
    self%block_codom=0; self%rec_uparams=1; self%elim_level=0; self%all_names=1
    self%is_zero=.false.; self%is_nonzero=.false.; self%k_target=.false.; self%nested=.false.
  end subroutine
  integer function add_type(self,n,ty,ctors) result(id)
    class(inductive_check_state), intent(inout) :: self
    integer, intent(in) :: n,ty,ctors
    id=self%count+1
    call grow(self%name,id); call grow(self%ty,id); call grow(self%ctors,id); call grow(self%indices,id)
    call grow(self%ind_const,id); call grow(self%major,id); call grow(self%motive,id); call grow(self%minors,id)
    self%name(id)=n; self%ty(id)=ty; self%ctors(id)=ctors; self%indices(id)=1; self%minors(id)=1
    self%ind_const(id)=0; self%major(id)=0; self%motive(id)=0; self%count=id
  end function
  integer function add_ctor(self,n,ty) result(id)
    class(inductive_check_state), intent(inout) :: self
    integer, intent(in) :: n,ty
    id=self%ctor_count+1
    call grow(self%ctor_name,id); call grow(self%ctor_ty,id)
    self%ctor_name(id)=n; self%ctor_ty(id)=ty; self%ctor_count=id
  end function
end module
