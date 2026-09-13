! SoA translation of nanoclo nbe.rs Value, RigidHead, and Vals. Binder names
! and styles are stored, but excluded from lambda/pi interning as upstream.
module nbe
  use base
  use hash_table
  use spines, only: spine_arena,spine_empty
  implicit none
  integer(int32), parameter :: vrigid=1,vunfold=2,vlam=3,vpi=4,vsort=5,vnat=6,vstr=7,vthunk=8
  integer(int32), parameter :: hbvar=1,hlocal=2,hconst=3
  integer(int32), parameter :: caxiom=1,cctor=2,crecursor=3,cquot=4,cinductive=5
  type :: value_arena
    integer(int32), allocatable :: tag(:),head(:),spine(:),env(:),body(:),forced(:),domain(:), &
      binder_name(:),binder_style(:),binder_type(:),name(:),levels(:),level(:),literal(:),expr(:)
    integer(int32), allocatable :: head_tag(:),head_a(:),head_b(:),head_c(:)
    integer(int32) :: count=0,head_count=0
    type(spine_arena) :: spines
    type(table_t) :: head_intern,rigid_intern,unfold_intern,lam_intern,pi_intern,sort_intern,nat_intern,str_intern,thunk_intern
    ! Optional memo results will use 0 for absent and -1 for cached None;
    ! ordinary value references are positive.
    type(table_t) :: clo_val_cache,unfold_cache,const_val_cache,const_ty_cache,const_lvl_cache,rec_rule_cache, &
      iota_cache,struct_eta_cache,open_cache,type_cache,local_cache,conv_pos,conv_neg,conv_neg_probe,probe_fail
    integer(int32) :: probe_depth=0,in_conv=0
    integer(int64) :: probe_fuel=0,probe_escalate=0
    logical :: probe_aborted=.false.
  contains
    procedure :: init
    procedure :: alloc
    procedure :: mk_head
    procedure :: mk_rigid
    procedure :: mk_bvar
    procedure :: mk_unfold
    procedure :: mk_lam
    procedure :: mk_pi
    procedure :: mk_sort
    procedure :: mk_nat
    procedure :: mk_str
    procedure :: mk_thunk_keyed
    procedure :: set_forced
    procedure :: set_domain
    procedure :: reset_decl
  end type
contains
  subroutine init(self)
    class(value_arena), intent(inout) :: self
    call self%spines%init()
  end subroutine
  integer function alloc(self,tag) result(id)
    class(value_arena), intent(inout) :: self
    integer(int32), intent(in) :: tag
    id=self%count+1
    call grow(self%tag,id); self%tag(id)=0
    call grow(self%head,id); self%head(id)=0
    call grow(self%spine,id); self%spine(id)=0
    call grow(self%env,id); self%env(id)=0
    call grow(self%body,id); self%body(id)=0
    call grow(self%forced,id); self%forced(id)=0
    call grow(self%domain,id); self%domain(id)=0
    call grow(self%binder_name,id); self%binder_name(id)=0
    call grow(self%binder_style,id); self%binder_style(id)=0
    call grow(self%binder_type,id); self%binder_type(id)=0
    call grow(self%name,id); self%name(id)=0
    call grow(self%levels,id); self%levels(id)=0
    call grow(self%level,id); self%level(id)=0
    call grow(self%literal,id); self%literal(id)=0
    call grow(self%expr,id); self%expr(id)=0
    self%tag(id)=tag; self%count=id
  end function
  integer function mk_head(self,tag,a,b,c) result(id)
    class(value_arena), intent(inout) :: self
    integer(int32), intent(in) :: tag,a,b,c
    id=self%head_intern%get(tag,a,b,c)
    if (id/=0) return
    id=self%head_count+1
    call grow(self%head_tag,id); call grow(self%head_a,id); call grow(self%head_b,id); call grow(self%head_c,id)
    self%head_tag(id)=tag; self%head_a(id)=a; self%head_b(id)=b; self%head_c(id)=c; self%head_count=id
    call self%head_intern%put(tag,a,b,c,id)
  end function
  integer function mk_rigid(self,head,spine) result(id)
    class(value_arena), intent(inout) :: self
    integer(int32), intent(in) :: head,spine
    id=self%rigid_intern%get(head,spine,0,0)
    if (id/=0) return
    id=self%alloc(vrigid); self%head(id)=head; self%spine(id)=spine
    call self%rigid_intern%put(head,spine,0,0,id)
  end function
  integer function mk_bvar(self,level,ty) result(id)
    class(value_arena), intent(inout) :: self
    integer(int32), intent(in) :: level,ty
    integer :: head
    head=self%mk_head(hbvar,level,ty,0); id=self%mk_rigid(head,spine_empty)
  end function
  integer function mk_unfold(self,name,levels,spine) result(id)
    class(value_arena), intent(inout) :: self
    integer(int32), intent(in) :: name,levels,spine
    id=self%unfold_intern%get(name,levels,spine,0)
    if (id/=0) return
    id=self%alloc(vunfold); self%name(id)=name; self%levels(id)=levels; self%spine(id)=spine
    call self%unfold_intern%put(name,levels,spine,0,id)
  end function
  integer function mk_lam(self,name,style,ty,env,body) result(id)
    class(value_arena), intent(inout) :: self
    integer(int32), intent(in) :: name,style,ty,env,body
    id=self%lam_intern%get(ty,env,body,0)
    if (id/=0) return
    id=self%alloc(vlam)
    self%binder_name(id)=name; self%binder_style(id)=style; self%binder_type(id)=ty; self%env(id)=env; self%body(id)=body
    call self%lam_intern%put(ty,env,body,0,id)
  end function
  integer function mk_pi(self,name,style,domain,env,body) result(id)
    class(value_arena), intent(inout) :: self
    integer(int32), intent(in) :: name,style,domain,env,body
    id=self%pi_intern%get(domain,env,body,0)
    if (id/=0) return
    id=self%alloc(vpi)
    self%binder_name(id)=name; self%binder_style(id)=style; self%domain(id)=domain; self%env(id)=env; self%body(id)=body
    call self%pi_intern%put(domain,env,body,0,id)
  end function
  integer function mk_sort(self,ptr) result(id)
    class(value_arena), intent(inout) :: self
    integer(int32), intent(in) :: ptr
    id=self%sort_intern%get(ptr,0,0,0)
    if (id/=0) return
    id=self%alloc(vsort); self%level(id)=ptr
    call self%sort_intern%put(ptr,0,0,0,id)
  end function
  integer function mk_nat(self,ptr) result(id)
    class(value_arena), intent(inout) :: self
    integer(int32), intent(in) :: ptr
    id=self%nat_intern%get(ptr,0,0,0)
    if (id/=0) return
    id=self%alloc(vnat); self%literal(id)=ptr
    call self%nat_intern%put(ptr,0,0,0,id)
  end function
  integer function mk_str(self,ptr) result(id)
    class(value_arena), intent(inout) :: self
    integer(int32), intent(in) :: ptr
    id=self%str_intern%get(ptr,0,0,0)
    if (id/=0) return
    id=self%alloc(vstr); self%literal(id)=ptr
    call self%str_intern%put(ptr,0,0,0,id)
  end function
  integer function mk_thunk_keyed(self,key_env,env,expr) result(id)
    class(value_arena), intent(inout) :: self
    integer(int32), intent(in) :: key_env,env,expr
    id=self%thunk_intern%get(key_env,expr,0,0)
    if (id/=0) return
    id=self%alloc(vthunk); self%env(id)=env; self%expr(id)=expr
    call self%thunk_intern%put(key_env,expr,0,0,id)
  end function
  subroutine set_forced(self,v,r)
    class(value_arena), intent(inout) :: self
    integer(int32), intent(in) :: v,r
    if (self%tag(v)==vthunk .or. self%tag(v)==vunfold) self%forced(v)=r
  end subroutine
  subroutine set_domain(self,v,d)
    class(value_arena), intent(inout) :: self
    integer(int32), intent(in) :: v,d
    if (self%tag(v)==vlam) self%domain(v)=d
  end subroutine
  subroutine reset_decl(self)
    class(value_arena), intent(inout) :: self
    self%count=0; self%head_count=0
    call self%spines%reset_decl()
    call self%head_intern%clear()
    call self%rigid_intern%clear()
    call self%unfold_intern%clear()
    call self%lam_intern%clear()
    call self%pi_intern%clear()
    call self%sort_intern%clear()
    call self%nat_intern%clear()
    call self%str_intern%clear()
    call self%thunk_intern%clear()
    call self%clo_val_cache%clear()
    call self%unfold_cache%clear()
    call self%const_val_cache%clear()
    call self%const_ty_cache%clear()
    call self%const_lvl_cache%clear()
    call self%rec_rule_cache%clear()
    call self%iota_cache%clear()
    call self%struct_eta_cache%clear()
    call self%open_cache%clear()
    call self%type_cache%clear()
    call self%local_cache%clear()
    call self%conv_pos%clear()
    call self%conv_neg%clear()
    call self%conv_neg_probe%clear()
    call self%probe_fail%clear()
    self%probe_depth=0; self%probe_fuel=0; self%probe_escalate=0; self%probe_aborted=.false.; self%in_conv=0
  end subroutine
end module
