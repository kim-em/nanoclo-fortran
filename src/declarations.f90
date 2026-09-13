! SoA translation of nanoclo env.rs declaration headers and visibility cutoff.
module declarations
  use base
  use hash_table
  implicit none
  integer, parameter :: daxiom=1,ddefinition=2,dtheorem=3,dopaque=4,dquot=5,dinductive=6,dconstructor=7,drecursor=8
  integer, parameter :: hint_opaque=0,hint_regular=1,hint_abbrev=2
  type :: declaration_arena
    integer(int32), allocatable :: tag(:),name(:),uparams(:),ty(:),value(:),hint(:),height(:)
    integer(int32), allocatable :: num_params(:),num_indices(:),num_motives(:),num_minors(:)
    integer(int32), allocatable :: is_recursive(:),is_nested(:),is_k(:),all_names(:),ctor_names(:)
    integer(int32), allocatable :: inductive_name(:),ctor_idx(:),num_fields(:),rules(:),block_start(:),block_end(:)
    integer(int32), allocatable :: rule_ctor(:),rule_fields(:),rule_value(:)
    integer :: count=0,rule_count=0,temp_base=0,temp_rule_base=0
    type(table_t) :: by_name,temp_names
  contains
    procedure :: add
    procedure :: lookup
    procedure :: add_rule
    procedure :: begin_temp
    procedure :: end_temp
  end type
contains
  integer function add(self,tag,name,params,ty,value,hint,height,status) result(id)
    class(declaration_arena), intent(inout) :: self
    integer, intent(in) :: tag,name,params,ty,value,hint,height
    type(status_t), intent(inout) :: status
    id=0
    if (status%code/=accept) return
    if (self%temp_base==0 .and. self%by_name%get(name,0,0,0)/=0) then
      call status%fail(reject,'duplicate declaration name'); return
    end if
    id=self%count+1
    call grow(self%tag,id); call grow(self%name,id); call grow(self%uparams,id); call grow(self%ty,id)
    call grow(self%value,id); call grow(self%hint,id); call grow(self%height,id)
    self%tag(id)=tag; self%name(id)=name; self%uparams(id)=params; self%ty(id)=ty; self%value(id)=value
    self%hint(id)=hint; self%height(id)=height; self%count=id
    call grow(self%num_params,id); call grow(self%num_indices,id); call grow(self%num_motives,id)
    call grow(self%num_minors,id); call grow(self%is_recursive,id); call grow(self%is_nested,id)
    call grow(self%is_k,id); call grow(self%all_names,id); call grow(self%ctor_names,id)
    call grow(self%inductive_name,id); call grow(self%ctor_idx,id); call grow(self%num_fields,id)
    call grow(self%rules,id); call grow(self%block_start,id); call grow(self%block_end,id)
    self%num_params(id)=0; self%num_indices(id)=0; self%num_motives(id)=0; self%num_minors(id)=0
    self%is_recursive(id)=0; self%is_nested(id)=0; self%is_k(id)=0
    self%all_names(id)=1; self%ctor_names(id)=1; self%rules(id)=1
    self%inductive_name(id)=0; self%ctor_idx(id)=0; self%num_fields(id)=0
    self%block_start(id)=id; self%block_end(id)=id+1
    if (self%temp_base/=0) then
      call self%temp_names%put(name,0,0,0,id)
    else
      call self%by_name%put(name,0,0,0,id)
    end if
  end function
  integer function add_rule(self,ctor,fields,value) result(id)
    class(declaration_arena), intent(inout) :: self
    integer, intent(in) :: ctor,fields,value
    id=self%rule_count+1
    call grow(self%rule_ctor,id); call grow(self%rule_fields,id); call grow(self%rule_value,id)
    self%rule_ctor(id)=ctor; self%rule_fields(id)=fields; self%rule_value(id)=value; self%rule_count=id
  end function
  integer function lookup(self,name,cutoff) result(id)
    class(declaration_arena), intent(in) :: self
    integer, intent(in) :: name,cutoff
    id=self%temp_names%get(name,0,0,0)
    if (id/=0) return
    id=self%by_name%get(name,0,0,0)
    ! IDs start at one: while checking declaration cutoff, only earlier IDs
    ! are visible, exactly the Rust zero-based idx < cutoff condition.
    if (id>=cutoff) id=0
  end function
  subroutine begin_temp(self)
    class(declaration_arena), intent(inout) :: self
    if (self%temp_base/=0) error stop kernel_error
    self%temp_base=self%count+1; self%temp_rule_base=self%rule_count
  end subroutine
  subroutine end_temp(self)
    class(declaration_arena), intent(inout) :: self
    if (self%temp_base==0) return
    self%count=self%temp_base-1; self%rule_count=self%temp_rule_base; self%temp_base=0
    call self%temp_names%clear()
  end subroutine
  pure logical function hint_lt(k1,h1,k2,h2) result(lt)
    integer, intent(in) :: k1,h1,k2,h2
    lt=.false.
    if (k2==hint_opaque .or. k1==hint_abbrev) return
    if (k1==hint_opaque .or. k2==hint_abbrev) then
      lt=.true.
    else
      lt=h1<h2
    end if
  end function
end module
