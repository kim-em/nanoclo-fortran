! Translation of nanoclo src/expr.rs and util.rs constructors. Fields retain
! every Rust enum payload; hash collisions compare all fields before sharing.
module expr
  use base
  use hash_table
  use levels, only: level_arena
  use sequences, only: sequence_arena,list_nil
  implicit none
  integer(int32), parameter :: evar=1,esort=2,econst=3,eapp=4,epi=5,elam=6,elet=7,elocal=8, &
    eproj=9,enat=10,estr=11,fvar_level=0,fvar_unique=1
  ! The parsed expression arrays are immutable after sealing and shared by workers.
  integer(int32), allocatable, private :: export_tag(:),export_a(:),export_b(:),export_c(:),export_d(:),export_e(:),export_hash(:),export_nlbv(:),export_flags(:),export_next(:),export_origin(:)
  type(table_t), private :: export_nodes
  type :: expr_arena
    integer(int32), allocatable :: tag(:),a(:),b(:),c(:),d(:),e(:),hash(:),nlbv(:),flags(:),next(:),origin(:)
    integer(int32) :: count=0,current_origin=0,export_count=0
    logical :: shared_export=.false.
    type(table_t) :: nodes,inst_cache,abstr_cache,abstr_cache_levels,lvl_cache,subst_cache,dsubst_cache
  contains
    procedure :: scratch_capacity
    procedure :: seal_exports
    procedure :: clear_scratch
    procedure :: get_tag
    procedure :: get_a
    procedure :: get_b
    procedure :: get_c
    procedure :: get_d
    procedure :: get_e
    procedure :: get_hash
    procedure :: get_nlbv
    procedure :: get_flags
    procedure :: get_next
    procedure :: get_origin
    procedure :: intern
    procedure :: var
    procedure :: sort => mk_sort
    procedure :: const => mk_const
    procedure :: app => mk_app
    procedure :: binder
    procedure :: let_ => mk_let
    procedure :: local => mk_local
    procedure :: proj => mk_proj
    procedure :: inst
    procedure :: inst_aux
    procedure :: abstr
    procedure :: abstr_aux
    procedure :: abstr_levels
    procedure :: abstr_aux_levels
    procedure :: max_level
    procedure :: subst_expr_levels
    procedure :: subst_aux
  end type
contains
  integer function scratch_capacity(self) result(n)
    class(expr_arena), intent(in) :: self
    n=0
    if (allocated(self%tag)) n=size(self%tag)
  end function
  subroutine seal_exports(self)
    class(expr_arena), intent(inout) :: self
    if (self%shared_export) return
    call move_alloc(self%tag,export_tag)
    call move_alloc(self%a,export_a)
    call move_alloc(self%b,export_b)
    call move_alloc(self%c,export_c)
    call move_alloc(self%d,export_d)
    call move_alloc(self%e,export_e)
    call move_alloc(self%hash,export_hash)
    call move_alloc(self%nlbv,export_nlbv)
    call move_alloc(self%flags,export_flags)
    call move_alloc(self%next,export_next)
    call move_alloc(self%origin,export_origin)
    call self%nodes%move_to(export_nodes)
    self%export_count=self%count; self%shared_export=.true.; self%current_origin=1
  end subroutine
  subroutine clear_scratch(self)
    class(expr_arena), intent(inout) :: self
    if (.not.self%shared_export) return
    self%count=self%export_count
    call self%nodes%clear()
    call self%inst_cache%clear()
    call self%abstr_cache%clear()
    call self%abstr_cache_levels%clear()
    call self%lvl_cache%clear()
    call self%subst_cache%clear()
    call self%dsubst_cache%clear()
  end subroutine
  pure integer(int32) function get_tag(self,id) result(out)
    class(expr_arena), intent(in) :: self
    integer(int32), intent(in) :: id
    if (id<=self%export_count) then
      out=export_tag(id)
    else
      out=self%tag(id-self%export_count)
    end if
  end function
  pure integer(int32) function get_a(self,id) result(out)
    class(expr_arena), intent(in) :: self
    integer(int32), intent(in) :: id
    if (id<=self%export_count) then
      out=export_a(id)
    else
      out=self%a(id-self%export_count)
    end if
  end function
  pure integer(int32) function get_b(self,id) result(out)
    class(expr_arena), intent(in) :: self
    integer(int32), intent(in) :: id
    if (id<=self%export_count) then
      out=export_b(id)
    else
      out=self%b(id-self%export_count)
    end if
  end function
  pure integer(int32) function get_c(self,id) result(out)
    class(expr_arena), intent(in) :: self
    integer(int32), intent(in) :: id
    if (id<=self%export_count) then
      out=export_c(id)
    else
      out=self%c(id-self%export_count)
    end if
  end function
  pure integer(int32) function get_d(self,id) result(out)
    class(expr_arena), intent(in) :: self
    integer(int32), intent(in) :: id
    if (id<=self%export_count) then
      out=export_d(id)
    else
      out=self%d(id-self%export_count)
    end if
  end function
  pure integer(int32) function get_e(self,id) result(out)
    class(expr_arena), intent(in) :: self
    integer(int32), intent(in) :: id
    if (id<=self%export_count) then
      out=export_e(id)
    else
      out=self%e(id-self%export_count)
    end if
  end function
  pure integer(int32) function get_hash(self,id) result(out)
    class(expr_arena), intent(in) :: self
    integer(int32), intent(in) :: id
    if (id<=self%export_count) then
      out=export_hash(id)
    else
      out=self%hash(id-self%export_count)
    end if
  end function
  pure integer(int32) function get_nlbv(self,id) result(out)
    class(expr_arena), intent(in) :: self
    integer(int32), intent(in) :: id
    if (id<=self%export_count) then
      out=export_nlbv(id)
    else
      out=self%nlbv(id-self%export_count)
    end if
  end function
  pure integer(int32) function get_flags(self,id) result(out)
    class(expr_arena), intent(in) :: self
    integer(int32), intent(in) :: id
    if (id<=self%export_count) then
      out=export_flags(id)
    else
      out=self%flags(id-self%export_count)
    end if
  end function
  pure integer(int32) function get_next(self,id) result(out)
    class(expr_arena), intent(in) :: self
    integer(int32), intent(in) :: id
    if (id<=self%export_count) then
      out=export_next(id)
    else
      out=self%next(id-self%export_count)
    end if
  end function
  pure integer(int32) function get_origin(self,id) result(out)
    class(expr_arena), intent(in) :: self
    integer(int32), intent(in) :: id
    if (id<=self%export_count) then
      out=export_origin(id)
    else
      out=self%origin(id-self%export_count)
    end if
  end function
  function intern(self,tag,a,b,c,d,e) result(id)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: tag,a,b,c,d,e
    integer(int32) :: id,h,head,n,f,pos
    h=hash_words(hash_words(tag,a,b,c),d,e,0)
    if (self%shared_export) then
      id=export_nodes%get(h,0,0,0)
      do while(id/=0)
        if (export_tag(id)==tag .and. export_a(id)==a .and. export_b(id)==b .and. &
            export_c(id)==c .and. export_d(id)==d .and. export_e(id)==e) return
        id=export_next(id)
      end do
    end if
    head=self%nodes%get(h,0,0,0); id=head
    do while(id/=0)
      if (self%get_tag(id)==tag .and. self%get_a(id)==a .and. self%get_b(id)==b .and. &
          self%get_c(id)==c .and. self%get_d(id)==d .and. self%get_e(id)==e) return
      id=self%get_next(id)
    end do
    n=0; f=0
    select case(tag)
    case(evar)
      if (a<0 .or. a>=65535) error stop kernel_error
      n=a+1
    case(eapp)
      n=max(self%get_nlbv(a),self%get_nlbv(b)); f=ior(self%get_flags(a),self%get_flags(b))
    case(epi,elam)
      n=max(self%get_nlbv(c),max(0,self%get_nlbv(d)-1)); f=ior(self%get_flags(c),self%get_flags(d))
    case(elet)
      n=max(self%get_nlbv(c),self%get_nlbv(e),max(0,self%get_nlbv(d)-1))
      f=ior(ior(self%get_flags(c),self%get_flags(e)),self%get_flags(d))
    case(elocal)
      f=1
    case(eproj)
      n=self%get_nlbv(c); f=self%get_flags(c)
    case(esort,econst,enat,estr)
    case default
      error stop kernel_error
    end select
    id=self%count+1; pos=id-self%export_count
    call grow(self%tag,pos); call grow(self%a,pos); call grow(self%b,pos); call grow(self%c,pos)
    call grow(self%d,pos); call grow(self%e,pos); call grow(self%hash,pos); call grow(self%nlbv,pos)
    call grow(self%flags,pos); call grow(self%next,pos); call grow(self%origin,pos)
    self%tag(pos)=tag; self%a(pos)=a; self%b(pos)=b; self%c(pos)=c; self%d(pos)=d; self%e(pos)=e
    self%hash(pos)=h; self%nlbv(pos)=n; self%flags(pos)=f; self%next(pos)=head; self%origin(pos)=self%current_origin
    self%count=id
    call self%nodes%put(h,0,0,0,id)
  end function
  function var(self,idx) result(id)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: idx
    integer(int32) :: id
    id=self%intern(evar,idx,0,0,0,0)
  end function
  function mk_sort(self,level) result(id)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: level
    integer(int32) :: id
    id=self%intern(esort,level,0,0,0,0)
  end function
  function mk_const(self,name,levels) result(id)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: name,levels
    integer(int32) :: id
    id=self%intern(econst,name,levels,0,0,0)
  end function
  function mk_app(self,fun,arg) result(id)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: fun,arg
    integer(int32) :: id
    id=self%intern(eapp,fun,arg,0,0,0)
  end function
  function binder(self,tag,name,style,ty,body) result(id)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: tag,name,style,ty,body
    integer(int32) :: id
    id=self%intern(tag,name,style,ty,body,0)
  end function
  function mk_let(self,name,ty,val,body,nondep) result(id)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: name,ty,val,body
    logical, intent(in) :: nondep
    integer(int32) :: id
    id=self%intern(elet,name,merge(1,0,nondep),ty,body,val)
  end function
  function mk_local(self,name,style,ty,kind,serial) result(id)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: name,style,ty,kind,serial
    integer(int32) :: id
    id=self%intern(elocal,name,style,ty,kind,serial)
  end function
  function mk_proj(self,name,idx,structure) result(id)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: name,idx,structure
    integer(int32) :: id
    id=self%intern(eproj,name,idx,structure,0,0)
  end function
  function inst(self,e,substs) result(id)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: e,substs(:)
    integer(int32) :: id
    call self%inst_cache%clear()
    id=self%inst_aux(e,substs,0)
  end function
  recursive function inst_aux(self,e,substs,offset) result(id)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: e,substs(:),offset
    integer(int32) :: id,t,a,b,c,d,v,idx
    id=e
    if (self%get_nlbv(e)<=offset) return
    id=self%inst_cache%get(e,offset,0,0)
    if (id/=0) return
    t=self%get_tag(e); a=self%get_a(e); b=self%get_b(e); c=self%get_c(e); d=self%get_d(e); v=self%get_e(e)
    select case(t)
    case(evar)
      idx=size(substs)-(a-offset)
      id=e
      if (idx>=1 .and. idx<=size(substs)) id=substs(idx)
    case(eapp)
      a=self%inst_aux(a,substs,offset); b=self%inst_aux(b,substs,offset)
      id=self%app(a,b)
    case(epi,elam,elet)
      c=self%inst_aux(c,substs,offset)
      if (t==elet) v=self%inst_aux(v,substs,offset)
      d=self%inst_aux(d,substs,offset+1)
      id=self%intern(t,a,b,c,d,v)
    case(eproj)
      c=self%inst_aux(c,substs,offset); id=self%proj(a,b,c)
    case default
      error stop kernel_error
    end select
    call self%inst_cache%put(e,offset,0,0,id)
  end function
  function abstr(self,e,locals) result(id)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: e,locals(:)
    integer(int32) :: id
    call self%abstr_cache%clear()
    id=self%abstr_aux(e,locals,0)
  end function
  recursive function abstr_aux(self,e,locals,offset) result(id)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: e,locals(:),offset
    integer(int32) :: id,t,a,b,c,d,v,i
    id=e
    if (self%get_flags(e)==0) return
    id=self%abstr_cache%get(e,offset,0,0)
    if (id/=0) return
    t=self%get_tag(e); a=self%get_a(e); b=self%get_b(e); c=self%get_c(e); d=self%get_d(e); v=self%get_e(e)
    select case(t)
    case(elocal)
      id=e
      do i=size(locals),1,-1
        if (locals(i)==e) then
          id=self%var(size(locals)-i+offset); exit
        end if
      end do
    case(eapp)
      a=self%abstr_aux(a,locals,offset); b=self%abstr_aux(b,locals,offset); id=self%app(a,b)
    case(epi,elam,elet)
      c=self%abstr_aux(c,locals,offset)
      if (t==elet) v=self%abstr_aux(v,locals,offset)
      d=self%abstr_aux(d,locals,offset+1)
      id=self%intern(t,a,b,c,d,v)
    case(eproj)
      c=self%abstr_aux(c,locals,offset); id=self%proj(a,b,c)
    case default
      error stop kernel_error
    end select
    call self%abstr_cache%put(e,offset,0,0,id)
  end function
  function abstr_levels(self,e,start_pos,num_open_binders) result(id)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: e,start_pos,num_open_binders
    integer(int32) :: id
    call self%abstr_cache_levels%clear()
    id=self%abstr_aux_levels(e,start_pos,num_open_binders)
  end function
  recursive function abstr_aux_levels(self,e,start_pos,num_open_binders) result(id)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: e,start_pos,num_open_binders
    integer(int32) :: id,t,a,b,c,d,v
    id=e
    if (self%get_flags(e)==0) return
    id=self%abstr_cache_levels%get(e,start_pos,num_open_binders,0)
    if (id/=0) return
    t=self%get_tag(e); a=self%get_a(e); b=self%get_b(e); c=self%get_c(e); d=self%get_d(e); v=self%get_e(e)
    select case(t)
    case(elocal)
      id=e
      if (d==fvar_level .and. v>=start_pos) id=self%var(num_open_binders-v-1)
    case(eapp)
      a=self%abstr_aux_levels(a,start_pos,num_open_binders)
      b=self%abstr_aux_levels(b,start_pos,num_open_binders); id=self%app(a,b)
    case(epi,elam,elet)
      c=self%abstr_aux_levels(c,start_pos,num_open_binders)
      if (t==elet) v=self%abstr_aux_levels(v,start_pos,num_open_binders)
      d=self%abstr_aux_levels(d,start_pos,num_open_binders+1)
      id=self%intern(t,a,b,c,d,v)
    case(eproj)
      c=self%abstr_aux_levels(c,start_pos,num_open_binders); id=self%proj(a,b,c)
    case default
      error stop kernel_error
    end select
    call self%abstr_cache_levels%put(e,start_pos,num_open_binders,0,id)
  end function
  recursive function max_level(self,e) result(n)
    class(expr_arena), intent(inout) :: self
    integer(int32), intent(in) :: e
    integer(int32) :: n,a,b,c,d,v,t
    n=0
    if (self%get_flags(e)==0) return
    n=self%lvl_cache%get(e,0,0,0)
    if (n/=0) then
      n=n-1; return
    end if
    t=self%get_tag(e); a=self%get_a(e); b=self%get_b(e); c=self%get_c(e); d=self%get_d(e); v=self%get_e(e)
    select case(t)
    case(elocal)
      n=self%max_level(c)
      if (d==fvar_level) n=max(n,v+1)
    case(eapp)
      a=self%max_level(a); b=self%max_level(b); n=max(a,b)
    case(epi,elam,elet)
      c=self%max_level(c); d=self%max_level(d); n=max(c,d)
      if (t==elet) then
        v=self%max_level(v); n=max(n,v)
      end if
    case(eproj)
      n=self%max_level(c)
    end select
    call self%lvl_cache%put(e,0,0,0,n+1)
  end function
  function subst_expr_levels(self,levels,lists,e,ks,vs,status) result(id)
    class(expr_arena), intent(inout) :: self
    type(level_arena), intent(inout) :: levels
    type(sequence_arena), intent(inout) :: lists
    integer(int32), intent(in) :: e,ks,vs
    type(status_t), intent(inout) :: status
    integer(int32) :: id
    id=0
    if (status%code/=accept) return
    id=self%dsubst_cache%get(e,ks,vs,0)
    if (id/=0) return
    call self%subst_cache%clear()
    if (lists%length(ks)/=lists%length(vs)) then
      call status%fail(reject,'universe argument count mismatch'); return
    end if
    id=self%subst_aux(levels,lists,e,ks,vs,status)
    if (status%code==accept) call self%dsubst_cache%put(e,ks,vs,0,id)
  end function
  recursive function subst_aux(self,levels,lists,e,ks,vs,status) result(id)
    class(expr_arena), intent(inout) :: self
    type(level_arena), intent(inout) :: levels
    type(sequence_arena), intent(inout) :: lists
    integer(int32), intent(in) :: e,ks,vs
    type(status_t), intent(inout) :: status
    integer(int32) :: id,t,a,b,c,d,v
    id=0
    if (status%code/=accept) return
    id=self%subst_cache%get(e,ks,vs,0)
    if (id/=0) return
    t=self%get_tag(e); a=self%get_a(e); b=self%get_b(e); c=self%get_c(e); d=self%get_d(e); v=self%get_e(e)
    select case(t)
    case(evar,enat,estr)
      id=e
    case(esort)
      a=levels%subst_level_ids(lists,a,ks,vs); id=self%sort(a)
    case(econst)
      b=levels%subst_levels_ids(lists,b,ks,vs); id=self%const(a,b)
    case(eapp)
      a=self%subst_aux(levels,lists,a,ks,vs,status); b=self%subst_aux(levels,lists,b,ks,vs,status)
      if (status%code/=accept) return
      id=self%app(a,b)
    case(epi,elam,elet)
      c=self%subst_aux(levels,lists,c,ks,vs,status)
      if (t==elet) v=self%subst_aux(levels,lists,v,ks,vs,status)
      d=self%subst_aux(levels,lists,d,ks,vs,status)
      if (status%code/=accept) return
      id=self%intern(t,a,b,c,d,v)
    case(eproj)
      c=self%subst_aux(levels,lists,c,ks,vs,status)
      if (status%code/=accept) return
      id=self%proj(a,b,c)
    case(elocal)
      call status%fail(kernel_error,'universe substitution found local in declaration'); return
    case default
      call status%fail(kernel_error,'invalid expression in universe substitution'); return
    end select
    call self%subst_cache%put(e,ks,vs,0,id)
  end function
end module
