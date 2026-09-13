  recursive integer function nb_type(self,depth,v) result(t)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,v
    integer :: cur,l,h,s,n,ls,base,e
    t=0; cur=self%nb_force(depth,v)
    if (self%status%code/=accept) return
    t=self%nb%type_cache%get(cur,0,0,0)
    if (t/=0) return
    select case(self%nb%tag(cur))
    case(vsort)
      l=self%nb%level(cur); l=self%ctx%levels%succ(l); l=self%ctx%levels%simplify(l); t=self%nb%mk_sort(l)
    case(vrigid)
      h=self%nb%head(cur); s=self%nb%spine(cur)
      select case(self%nb%head_tag(h))
      case(hbvar)
        t=self%nb%head_b(h)
      case(hlocal)
        e=self%nb%head_a(h); e=self%ctx%exprs%get_c(e); t=self%nb_eval(depth,env_nil,e)
      case(hconst)
        n=self%nb%head_b(h); ls=self%nb%head_c(h); t=self%nb_const_type(n,ls)
      end select
      base=self%nb%mk_rigid(h,spine_empty); t=self%nb_spine_type(depth,t,base,s)
    case(vunfold)
      n=self%nb%name(cur); ls=self%nb%levels(cur); s=self%nb%spine(cur)
      t=self%nb_const_type(n,ls); base=self%nb%mk_unfold(n,ls,spine_empty)
      t=self%nb_spine_type(depth,t,base,s)
    case(vnat,vstr)
      n=merge(self%nat_name,self%string_name,self%nb%tag(cur)==vnat)
      if (n==0) then
        call self%status%fail(reject,'literal type is absent'); return
      end if
      h=self%nb%mk_head(hconst,cinductive,n,list_nil); t=self%nb%mk_rigid(h,spine_empty)
    case default
      call self%status%fail(kernel_error,'value type requested for binder')
    end select
    if (self%status%code==accept) call self%nb%type_cache%put(cur,0,0,0,t)
  end function
  recursive integer function nb_const_type(self,n,ls) result(v)
    class(checker), intent(inout) :: self
    integer, intent(in) :: n,ls
    integer :: id,e,params
    v=0
    if (self%status%code/=accept) return
    v=self%nb%const_ty_cache%get(n,ls,0,0)
    if (v/=0) return
    id=self%ctx%decls%lookup(n,self%cutoff)
    if (id==0) then
      call self%status%fail(reject,'unknown constant in value type'); return
    end if
    e=self%ctx%decls%ty(id); params=self%ctx%decls%uparams(id)
    e=self%ctx%exprs%subst_expr_levels(self%ctx%levels,self%ctx%lists,e,params,ls,self%status)
    v=self%nb_eval(0,env_nil,e)
    if (self%status%code==accept) call self%nb%const_ty_cache%put(n,ls,0,0,v)
  end function
  recursive integer function nb_spine_type(self,depth,ty,base,s) result(t)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,ty,base,s
    integer :: mark,node,i,prev,a,cur,n,idx
    t=ty; prev=base; mark=self%work_top; node=s
    do while(node/=spine_empty)
      i=self%work_slot(); self%w1(i)=node; node=self%nb%spines%parent(node)
    end do
    do i=self%work_top,mark+1,-1
      node=self%w1(i)
      if (self%status%code/=accept) exit
      if (self%nb%spines%tag(node)/=elim_app) then
        n=self%nb%spines%a(node); idx=self%nb%spines%b(node)
        t=self%nb_field_type(depth,prev,t,n,idx)
        if (t==0) then
          call self%status%fail(reject,'projected a non-structure in spine type'); exit
        end if
        prev=self%nb_proj(depth,n,idx,prev); cycle
      end if
      a=self%nb%spines%a(node); cur=self%nb_whnf(depth,t)
      if (self%status%code/=accept) exit
      if (self%nb%tag(cur)/=vpi) then
        call self%status%fail(reject,'applied a non-function in spine type'); exit
      end if
      t=self%nb_open(depth,cur,a); prev=self%nb_apply(depth,prev,a)
    end do
    self%work_top=mark
  end function
  recursive integer function nb_type_level(self,depth,ty) result(l)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,ty
    integer :: v,dom,ld,fresh,cod,lc,h,n,ls
    l=0; v=self%nb_force(depth,ty)
    if (self%status%code/=accept) return
    select case(self%nb%tag(v))
    case(vsort)
      l=self%nb%level(v); l=self%ctx%levels%succ(l); l=self%ctx%levels%simplify(l)
    case(vpi)
      dom=self%nb%domain(v); ld=self%nb_type_level(depth,dom)
      if (ld==0 .or. self%status%code/=accept) return
      dom=self%nb_force(depth,dom); fresh=self%nb%mk_bvar(depth,dom)
      cod=self%nb_open(depth+1,v,fresh); cod=self%nb_whnf(depth+1,cod)
      lc=self%nb_type_level(depth+1,cod)
      if (lc==0 .or. self%status%code/=accept) return
      l=self%ctx%levels%imax(ld,lc); l=self%ctx%levels%simplify(l)
    case(vrigid)
      h=self%nb%head(v)
      if (self%nb%head_tag(h)==hconst) then
        n=self%nb%head_b(h); ls=self%nb%head_c(h); l=self%nb_const_level(n,ls)
        if (l/=0) return
      end if
      l=self%nb_sort_of(depth,v)
    case(vunfold)
      n=self%nb%name(v); ls=self%nb%levels(v)
      l=self%nb_sort_of(depth,v)
      if (l==0) l=self%nb_const_level(n,ls)
    end select
  end function
  recursive integer function nb_sort_of(self,depth,ty) result(l)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,ty
    integer :: t
    l=0; t=self%nb_type(depth,ty); t=self%nb_whnf(depth,t)
    if (self%status%code/=accept) return
    if (self%nb%tag(t)==vsort) then
      l=self%nb%level(t); l=self%ctx%levels%simplify(l)
    end if
  end function
  recursive integer function nb_const_level(self,n,ls) result(l)
    class(checker), intent(inout) :: self
    integer, intent(in) :: n,ls
    integer :: cur,d,dom,fresh,cached
    l=0
    if (self%status%code/=accept) return
    cached=self%nb%const_lvl_cache%get(n,ls,0,0)
    if (cached/=0) then
      l=max(0,cached); return
    end if
    cur=self%nb_const_type(n,ls); d=0
    do while(self%status%code==accept)
      cur=self%nb_whnf(d,cur)
      if (self%status%code/=accept) return
      select case(self%nb%tag(cur))
      case(vpi)
        dom=self%nb%domain(cur); dom=self%nb_force(d,dom)
        if (self%status%code/=accept) return
        fresh=self%nb%mk_bvar(d,dom); cur=self%nb_open(d+1,cur,fresh); d=d+1
      case(vsort)
        l=self%nb%level(cur); l=self%ctx%levels%simplify(l); exit
      case default
        exit
      end select
    end do
    if (self%status%code==accept) call self%nb%const_lvl_cache%put(n,ls,0,0,merge(l,-1,l/=0))
  end function
