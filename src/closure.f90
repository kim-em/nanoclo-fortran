! Translation of the environment section of nanoclo src/closure.rs. Delayed
! entries are normalized before interning; Myers skew-binary jumps are exact.
module closure
  use base
  use hash_table
  use read_sets
  use expr, only: expr_arena,evar,esort,econst,eapp,epi,elam,elet,elocal,eproj,enat,estr
  implicit none
  integer(int32), parameter :: env_nil=1,entry_val=1,entry_neu=2,entry_v=3
  character(14), parameter :: counter_names(26)=[character(14) :: &
    'infer','whnf_core','whnf','def_eq','whnf_hit','whnf_miss','deq_hit','deq_miss', &
    'unfold_hit','unfold_miss','push_entry','eq_mod','eqm_hit','eqm_miss','eqm_fast','eqm_nomemo', &
    'inf_hit','inf_miss','inf_var_val','inf_var_neu','inf_local','inf_sort','inf_const', &
    'spec_fail','spec_abort','probe_fail_hit']
  type :: closure_t
    integer(int32) :: e=0,env=env_nil
  end type
  type :: env_arena
    integer(int32), allocatable :: entry_tag(:),entry(:),entry_env(:),parent(:),len(:),next_level(:),jump(:)
    integer(int32) :: count=0
    integer(int64) :: ctrs(26)=0
    type(table_t) :: eq_mod_cache,eqm_sides
    type(table_t) :: env_intern
    type(read_set_arena) :: reads
    type(table_t) :: proj_cache,proj_cache_w,view_cache,view_cache_w,view_table
    integer(int32), allocatable :: view_start(:),view_len(:),view_next(:), &
      view_entry_tag(:),view_entry(:),view_entry_env(:)
    integer :: view_count=0,view_used=0
  contains
    procedure :: init
    procedure :: push_entry
    procedure :: push_entry_v
    procedure :: intern
    procedure :: lookup
    procedure :: chase
    procedure :: norm_clo
    procedure :: read_key
    procedure :: key
    procedure :: project
    procedure :: view_of
    procedure :: intern_view
    procedure :: reset_decl
    procedure :: eq_mod
    procedure :: eq_mod_neu
    procedure :: eqm_side
  end type
contains
  subroutine init(self)
    class(env_arena), intent(inout) :: self
    if (self%count/=0) return
    call grow(self%entry_tag,1); call grow(self%entry,1); call grow(self%entry_env,1)
    call grow(self%parent,1); call grow(self%len,1); call grow(self%next_level,1); call grow(self%jump,1)
    self%entry_tag(1)=entry_val; self%entry(1)=0; self%entry_env(1)=env_nil
    self%parent(1)=env_nil; self%len(1)=0; self%next_level(1)=0; self%jump(1)=env_nil; self%count=1
  end subroutine
  integer function intern(self,parent,tag,e,venv,level) result(id)
    class(env_arena), intent(inout) :: self
    integer(int32), intent(in) :: parent,tag,e,venv,level
    integer :: j,d1,d2,jump
    id=self%env_intern%get(parent,tag,e,venv)
    if (id/=0) return
    j=self%jump(parent)
    d1=self%len(parent)-self%len(j); d2=self%len(j)-self%len(self%jump(j))
    jump=parent
    if (d1==d2) jump=self%jump(j)
    id=self%count+1
    call grow(self%entry_tag,id); call grow(self%entry,id); call grow(self%entry_env,id)
    call grow(self%parent,id); call grow(self%len,id); call grow(self%next_level,id); call grow(self%jump,id)
    self%entry_tag(id)=tag; self%entry(id)=e; self%entry_env(id)=venv; self%parent(id)=parent
    self%len(id)=self%len(parent)+1; self%next_level(id)=max(self%next_level(parent),level); self%jump(id)=jump
    self%count=id
    call self%env_intern%put(parent,tag,e,venv,id)
  end function
  integer function push_entry(self,dag,parent,tag,e,venv,status) result(id)
    class(env_arena), intent(inout) :: self
    type(expr_arena), intent(inout) :: dag
    integer(int32), intent(in) :: parent,tag,e,venv
    type(status_t), intent(inout) :: status
    integer :: normalized_e,normalized_env,level
    id=env_nil
    if (status%code/=accept) return
    self%ctrs(11)=self%ctrs(11)+1
    normalized_e=e; normalized_env=env_nil
    level=0
    select case(tag)
    case(entry_val)
      normalized_env=venv
      if (venv/=env_nil) call self%norm_clo(dag,normalized_e,normalized_env,status)
      if (status%code/=accept) return
    case(entry_neu,entry_v)
    case default
      call status%fail(kernel_error,'invalid environment entry tag'); return
    end select
    ! nanoclo checks interning before computing the entry level.
    id=self%env_intern%get(parent,tag,normalized_e,normalized_env)
    if (id/=0) return
    select case(tag)
    case(entry_val)
      level=max(dag%max_level(normalized_e),self%next_level(normalized_env))
    case(entry_neu)
      level=dag%max_level(e)
    end select
    id=self%intern(parent,tag,normalized_e,normalized_env,level)
  end function
  integer function push_entry_v(self,parent,v) result(id)
    class(env_arena), intent(inout) :: self
    integer(int32), intent(in) :: parent,v
    self%ctrs(11)=self%ctrs(11)+1
    id=self%intern(parent,entry_v,v,env_nil,0)
  end function
  integer function lookup(self,env,i,status) result(node)
    class(env_arena), intent(in) :: self
    integer(int32), intent(in) :: env,i
    type(status_t), intent(inout) :: status
    integer :: target,j
    node=0
    if (status%code/=accept) return
    if (env<1 .or. env>self%count) then
      call status%fail(kernel_error,'invalid environment index'); return
    end if
    if (i<0 .or. i>=self%len(env)) then
      call status%fail(reject,'loose bound variable outside environment'); return
    end if
    target=self%len(env)-1-i; node=env
    do while(self%len(node)/=target+1)
      j=self%jump(node)
      if (self%len(j)>target) then
        node=j
      else
        node=self%parent(node)
      end if
    end do
  end function
  subroutine chase(self,dag,e,env,status)
    class(env_arena), intent(in) :: self
    type(expr_arena), intent(in) :: dag
    integer(int32), intent(inout) :: e,env
    type(status_t), intent(inout) :: status
    integer :: node
    do while(dag%get_tag(e)==evar)
      node=self%lookup(env,dag%get_a(e),status)
      if (status%code/=accept) return
      select case(self%entry_tag(node))
      case(entry_neu)
        e=self%entry(node); env=env_nil; return
      case(entry_v)
        return
      case(entry_val)
        e=self%entry(node); env=self%entry_env(node)
      end select
    end do
  end subroutine
  subroutine norm_clo(self,dag,e,env,status)
    class(env_arena), intent(in) :: self
    type(expr_arena), intent(in) :: dag
    integer(int32), intent(inout) :: e,env
    type(status_t), intent(inout) :: status
    if (dag%get_tag(e)==evar) call self%chase(dag,e,env,status)
    if (status%code/=accept) return
    if (dag%get_nlbv(e)==0) env=env_nil
  end subroutine
  subroutine reset_decl(self)
    class(env_arena), intent(inout) :: self
    self%count=1
    call self%env_intern%clear()
    call self%eq_mod_cache%clear(); call self%eqm_sides%clear()
    call self%proj_cache%clear(); call self%proj_cache_w%clear()
    call self%view_cache%clear(); call self%view_cache_w%clear(); call self%view_table%clear()
    self%view_count=0; self%view_used=0
    ! Expression read sets remain valid while the expression DAG persists.
  end subroutine
  integer function read_key(self,dag,e,env) result(id)
    class(env_arena), intent(inout) :: self
    type(expr_arena), intent(in) :: dag
    integer(int32), intent(in) :: e,env
    type(uses_t) :: u
    id=env
    if (env==env_nil) return
    u=self%reads%uses_mask(dag,e)
    select case(u%tag)
    case(uses_mask_kind)
      if (u%mask==0) then
        id=env_nil; return
      end if
    case(uses_dense,uses_deep)
      return
    end select
    id=self%view_of(u,env)
    if (id==0) id=env
  end function
  ! Cache identity only: projection compacts/reverses the selected bindings.
  ! Evaluation must keep the original closure environment.
  function key(self,dag,c,status) result(k)
    class(env_arena), intent(inout) :: self
    type(expr_arena), intent(inout) :: dag
    type(closure_t), intent(in) :: c
    type(status_t), intent(inout) :: status
    type(closure_t) :: k
    type(uses_t) :: u
    k=c
    if (c%env==env_nil) return
    u=self%reads%uses_mask(dag,c%e)
    select case(u%tag)
    case(uses_dense,uses_deep)
      return
    case(uses_mask_kind)
      if (u%mask==0) then
        k%env=env_nil; return
      end if
    end select
    k%env=self%project(dag,u,c%env,status)
  end function
  integer function project(self,dag,u,env,status) result(id)
    class(env_arena), intent(inout) :: self
    type(expr_arena), intent(inout) :: dag
    type(uses_t), intent(in) :: u
    integer(int32), intent(in) :: env
    type(status_t), intent(inout) :: status
    integer :: picked(512),cur,d,top,n,lo,hi,node,e,venv,tag
    integer(int64) :: w(8)
    lo=low_word(u%mask); hi=low_word(shiftr(u%mask,32))
    if (u%tag==uses_mask_kind) then
      id=self%proj_cache%get(lo,hi,env,0)
    else
      id=self%proj_cache_w%get(u%wide,env,0,0)
    end if
    if (id/=0) return
    w=self%reads%words(u); n=8
    do while(n>1)
      if (w(n)/=0) exit
      n=n-1
    end do
    top=64*n-1-leadz(w(n)); picked=0; cur=env
    do d=0,top
      if (cur==env_nil) then
        if (u%tag==uses_wide) then
          id=env; return
        end if
        exit
      end if
      if (btest(w(d/64+1),mod(d,64))) picked(d+1)=cur
      cur=self%parent(cur)
    end do
    id=env_nil
    do d=0,top
      if (.not.btest(w(d/64+1),mod(d,64))) cycle
      node=picked(d+1)
      if (node==0) then
        id=env; return
      end if
      ! Copy before push_entry can grow all the environment arrays.
      tag=self%entry_tag(node); e=self%entry(node); venv=self%entry_env(node)
      id=self%push_entry(dag,id,tag,e,venv,status)
      if (status%code/=accept) return
    end do
    if (u%tag==uses_mask_kind) then
      call self%proj_cache%put(lo,hi,env,0,id)
    else
      call self%proj_cache_w%put(u%wide,env,0,0,id)
    end if
  end function
  integer function view_of(self,u,env) result(id)
    class(env_arena), intent(inout) :: self
    type(uses_t), intent(in) :: u
    integer(int32), intent(in) :: env
    integer :: picked(512),cur,d,top,n,lo,hi,num_picked
    integer(int64) :: w(8)
    lo=low_word(u%mask); hi=low_word(shiftr(u%mask,32))
    if (u%tag==uses_mask_kind) then
      id=self%view_cache%get(lo,hi,env,0)
    else
      id=self%view_cache_w%get(u%wide,env,0,0)
    end if
    if (id/=0) return
    w=self%reads%words(u); n=8
    do while(n>1)
      if (w(n)/=0) exit
      n=n-1
    end do
    top=64*n-1-leadz(w(n)); cur=env; num_picked=0
    do d=0,top
      if (cur==env_nil) then
        id=0; return
      end if
      if (btest(w(d/64+1),mod(d,64))) then
        num_picked=num_picked+1; picked(num_picked)=cur
      end if
      cur=self%parent(cur)
    end do
    id=self%intern_view(picked(:num_picked))
    if (u%tag==uses_mask_kind) then
      call self%view_cache%put(lo,hi,env,0,id)
    else
      call self%view_cache_w%put(u%wide,env,0,0,id)
    end if
  end function
  integer function intern_view(self,picked) result(id)
    class(env_arena), intent(inout) :: self
    integer(int32), intent(in) :: picked(:)
    integer :: h,i,node,head,k,start,n
    n=size(picked); h=0
    do i=1,n
      node=picked(i)
      h=hash_words(h,self%entry_tag(node),self%entry(node),self%entry_env(node))
    end do
    head=self%view_table%get(h,n,0,0); k=head
    do while(k/=0)
      start=self%view_start(k)
      do i=1,n
        node=picked(i)
        if (self%entry_tag(node)/=self%view_entry_tag(start+i-1)) exit
        if (self%entry(node)/=self%view_entry(start+i-1)) exit
        if (self%entry_env(node)/=self%view_entry_env(start+i-1)) exit
      end do
      if (i>n) then
        id=-k; return
      end if
      k=self%view_next(k)
    end do
    k=self%view_count+1
    call grow(self%view_start,k); call grow(self%view_len,k); call grow(self%view_next,k)
    call grow(self%view_entry_tag,self%view_used+n); call grow(self%view_entry,self%view_used+n)
    call grow(self%view_entry_env,self%view_used+n)
    start=self%view_used+1; self%view_start(k)=start; self%view_len(k)=n; self%view_next(k)=head
    do i=1,n
      node=picked(i)
      self%view_entry_tag(start+i-1)=self%entry_tag(node)
      self%view_entry(start+i-1)=self%entry(node)
      self%view_entry_env(start+i-1)=self%entry_env(node)
    end do
    self%view_used=self%view_used+n; self%view_count=k
    call self%view_table%put(h,n,0,0,k)
    ! Negative IDs tag read views; positive IDs are real environments.
    id=-k
  end function
  include 'eq_mod.inc'
end module
