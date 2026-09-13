! Declaration checking and closure inference translated from nanoclo tc.rs.
  recursive integer function infer(self,e,check) result(t)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e
    logical, intent(in) :: check
    type(closure_t) :: c
    c=self%infer_go(closure_t(e,env_nil),check); t=self%reify(c)
  end function
  recursive function infer_go(self,c,check) result(out)
    class(checker), intent(inout) :: self
    type(closure_t), intent(in) :: c
    logical, intent(in) :: check
    type(closure_t) :: out,key
    integer :: t,a,b,node,e,env,cache,v,ty
    out=closure_t()
    if (self%status%code/=accept) return
    self%rp%ctrs(1)=self%rp%ctrs(1)+1
    t=self%ctx%exprs%get_tag(c%e); a=self%ctx%exprs%get_a(c%e); b=self%ctx%exprs%get_b(c%e)
    select case(t)
    case(evar)
      node=self%rp%lookup(c%env,a,self%status)
      if (self%status%code/=accept) return
      e=self%rp%entry(node); env=self%rp%entry_env(node)
      select case(self%rp%entry_tag(node))
      case(entry_neu)
        self%rp%ctrs(20)=self%rp%ctrs(20)+1; out=closure_t(self%ctx%exprs%get_c(e),env_nil)
      case(ev_val)
        self%rp%ctrs(19)=self%rp%ctrs(19)+1; out=self%infer_go(closure_t(e,env),check)
      case(entry_v)
        self%rp%ctrs(19)=self%rp%ctrs(19)+1; v=self%nb_type(0,e); ty=self%nb_readback(v); out=closure_t(ty,env_nil)
      end select
    case(elocal)
      self%rp%ctrs(21)=self%rp%ctrs(21)+1; out=closure_t(self%ctx%exprs%get_c(c%e),env_nil)
    case(esort)
      self%rp%ctrs(22)=self%rp%ctrs(22)+1
      if (check) call self%check_level(a)
      if (self%status%code/=accept) return
      a=self%ctx%levels%succ(a); out=closure_t(self%ctx%exprs%sort(a),env_nil)
    case(econst)
      self%rp%ctrs(23)=self%rp%ctrs(23)+1
      ty=0
      if (self%ctx%decls%temp_base==0) ty=self%g_inst_ty%get(c%e,0,0,0)
      if (ty/=0) then
        if (check) call self%check_levels(b)
        out=closure_t(ty,env_nil); return
      end if
      ty=self%infer_const(a,b,check)
      if (self%status%code/=accept) return
      if (self%ctx%decls%temp_base==0) call self%g_inst_ty%put(c%e,0,0,0,ty)
      out=closure_t(ty,env_nil)
    case(enat,estr)
      if (t==enat) then
        if (.not.self%nat_extension) call self%status%fail(reject,'Nat extension disabled')
        a=self%nat_name
      else
        if (.not.self%string_extension) call self%status%fail(reject,'String extension disabled')
        a=self%string_name
      end if
      if (a==0) call self%status%fail(reject,'literal type is absent')
      if (self%status%code==accept) out=closure_t(self%ctx%exprs%const(a,list_nil),env_nil)
    case(elam,epi,elet,eapp,eproj)
      key=self%rp%key(self%ctx%exprs,c,self%status)
      if (self%status%code/=accept) return
      if (check) then
        cache=self%infer_cache_check%get(key%e,key%env,0,0)
      else
        cache=self%infer_cache_only%get(key%e,key%env,0,0)
      end if
      if (cache/=0) then
        self%rp%ctrs(17)=self%rp%ctrs(17)+1; out=closure_t(self%clo_e(cache),self%clo_env(cache)); return
      end if
      self%rp%ctrs(18)=self%rp%ctrs(18)+1
      select case(t)
      case(elam)
        out=self%infer_lambda(c,check)
      case(epi)
        out=self%infer_pi(c,check)
      case(elet)
        out=self%infer_let(c,check)
      case(eapp)
        out=self%infer_s(c,check)
      case(eproj)
        e=self%ctx%exprs%get_c(c%e); out=self%infer_proj(a,b,closure_t(e,c%env),check)
      end select
      if (self%status%code/=accept) return
      cache=self%cache_closure(out)
      if (check) then
        call self%infer_cache_check%put(key%e,key%env,0,0,cache)
      else
        call self%infer_cache_only%put(key%e,key%env,0,0,cache)
      end if
    end select
  end function
  recursive subroutine check_level(self,l)
    class(checker), intent(inout) :: self
    integer, intent(in) :: l
    integer :: a,b,k
    if (self%status%code/=accept .or. self%current_decl==0) return
    a=self%ctx%levels%a(l); b=self%ctx%levels%b(l)
    select case(self%ctx%levels%tag(l))
    case(lsucc)
      call self%check_level(a)
    case(lmax,limax)
      call self%check_level(a); call self%check_level(b)
    case(lparam)
      k=self%ctx%decls%uparams(self%current_decl)
      do while(k/=list_nil)
        if (self%ctx%lists%head(k)==l) return
        k=self%ctx%lists%tail(k)
      end do
      call self%status%fail(reject,'undefined universe parameter')
    end select
  end subroutine
  subroutine check_levels(self,ls)
    class(checker), intent(inout) :: self
    integer, intent(in) :: ls
    integer :: k,l
    k=ls
    do while(k/=list_nil)
      l=self%ctx%lists%head(k); call self%check_level(l); k=self%ctx%lists%tail(k)
    end do
  end subroutine
  integer function infer_const(self,n,ls,check) result(ty)
    class(checker), intent(inout) :: self
    integer, intent(in) :: n,ls
    logical, intent(in) :: check
    integer :: id,params,e
    ty=0
    if (self%status%code/=accept) return
    id=self%ctx%decls%lookup(n,self%cutoff)
    if (id==0) then
      call self%status%fail(reject,'unknown or not-yet-visible constant'); return
    end if
    params=self%ctx%decls%uparams(id); e=self%ctx%decls%ty(id)
    if (self%ctx%lists%length(params)/=self%ctx%lists%length(ls)) then
      call self%status%fail(reject,'incorrect number of universe levels'); return
    end if
    if (check) call self%check_levels(ls)
    ty=self%ctx%exprs%subst_expr_levels(self%ctx%levels,self%ctx%lists,e,params,ls,self%status)
  end function
  recursive function infer_lambda(self,c,check) result(out)
    class(checker), intent(inout) :: self
    type(closure_t), intent(in) :: c
    logical, intent(in) :: check
    type(closure_t) :: out,bt,dty
    integer :: mark,env,e,n,start,name,style,domain,body,d,l,fv,slot,r,i,u
    out=closure_t(); mark=self%work_top; env=c%env; e=c%e; n=0; start=0
    do while(self%ctx%exprs%get_tag(e)==elam)
      name=self%ctx%exprs%get_a(e); style=self%ctx%exprs%get_b(e); domain=self%ctx%exprs%get_c(e); body=self%ctx%exprs%get_d(e)
      d=self%reify(closure_t(domain,env))
      if (check) then
        dty=self%infer_go(closure_t(d,env_nil),check); u=self%ensure_sort_clo(dty)
      end if
      if (self%status%code/=accept) exit
      l=max(self%ctx%exprs%max_level(e),self%rp%next_level(env)); l=max(l,self%ctx%exprs%max_level(d))
      if (n==0) start=l
      fv=self%ctx%exprs%local(anonymous,0,d,fvar_level,l)
      env=self%rp%push_entry(self%ctx%exprs,env,entry_neu,fv,env_nil,self%status)
      slot=self%work_slot(); self%w1(slot)=name; self%w2(slot)=style; self%w3(slot)=d
      n=n+1; e=body
    end do
    bt=self%infer_go(closure_t(e,env),check); r=self%reify(bt); r=self%cheap_beta_reduce(r)
    if (self%status%code==accept) then
      r=self%ctx%exprs%abstr_aux_levels(r,start,start+n)
      do i=n,1,-1
        name=self%w1(mark+i); style=self%w2(mark+i); d=self%w3(mark+i)
        d=self%ctx%exprs%abstr_aux_levels(d,start,start+i-1)
        r=self%ctx%exprs%binder(epi,name,style,d,r)
      end do
      out=closure_t(r,env_nil)
    end if
    self%work_top=mark
  end function
  recursive function infer_pi(self,c,check) result(out)
    class(checker), intent(inout) :: self
    type(closure_t), intent(in) :: c
    logical, intent(in) :: check
    type(closure_t) :: out,dty,bt
    integer :: domain,body,d,u,l,fv,env,s
    out=closure_t()
    domain=self%ctx%exprs%get_c(c%e); body=self%ctx%exprs%get_d(c%e)
    d=self%reify(closure_t(domain,c%env)); dty=self%infer_go(closure_t(d,env_nil),check)
    u=self%ensure_sort_clo(dty)
    if (self%status%code/=accept) return
    l=max(self%ctx%exprs%max_level(c%e),self%rp%next_level(c%env)); l=max(l,self%ctx%exprs%max_level(d))
    fv=self%ctx%exprs%local(anonymous,0,d,fvar_level,l)
    env=self%rp%push_entry(self%ctx%exprs,c%env,entry_neu,fv,env_nil,self%status)
    bt=self%infer_go(closure_t(body,env),check); s=self%ensure_sort_clo(bt)
    if (self%status%code/=accept) return
    l=self%ctx%levels%imax(u,s); l=self%ctx%levels%simplify(l); out=closure_t(self%ctx%exprs%sort(l),env_nil)
  end function
  recursive function infer_let(self,c,check) result(out)
    class(checker), intent(inout) :: self
    type(closure_t), intent(in) :: c
    logical, intent(in) :: check
    type(closure_t) :: out,ty,vty
    integer :: t,val,body,u,env,e
    logical :: ok
    out=closure_t(); t=self%ctx%exprs%get_c(c%e); val=self%ctx%exprs%get_e(c%e); body=self%ctx%exprs%get_d(c%e)
    t=self%reify(closure_t(t,c%env))
    if (check) then
      ty=self%infer_go(closure_t(t,env_nil),check); u=self%ensure_sort_clo(ty)
      vty=self%infer_go(closure_t(val,c%env),check); e=self%reify(vty)
      ok=self%is_def_eq(closure_t(e,env_nil),closure_t(t,env_nil))
      if (.not.ok) call self%status%fail(reject,'let type mismatch')
    end if
    if (self%status%code/=accept) return
    env=self%rp%push_entry(self%ctx%exprs,c%env,ev_val,val,c%env,self%status)
    out=self%infer_go(closure_t(body,env),check)
  end function
  recursive function infer_s(self,c,check) result(out)
    class(checker), intent(inout) :: self
    type(closure_t), intent(in) :: c
    logical, intent(in) :: check
    type(closure_t) :: out,fty,aty,arg
    integer :: mark,e,slot,i,dom_e,dom_v,pi_env,body,fv,a,b,r,env
    logical :: ok
    out=closure_t(); mark=self%work_top; e=c%e
    do while(self%ctx%exprs%get_tag(e)==eapp)
      slot=self%work_slot(); self%w1(slot)=self%ctx%exprs%get_b(e); e=self%ctx%exprs%get_a(e)
    end do
    fty=self%infer_go(closure_t(e,c%env),check)
    do i=self%work_top,mark+1,-1
      if (self%status%code/=accept) exit
      arg=closure_t(self%w1(i),c%env); dom_e=0; dom_v=0
      if (self%ctx%exprs%get_tag(fty%e)==epi) then
        dom_e=self%ctx%exprs%get_c(fty%e); pi_env=fty%env; body=self%ctx%exprs%get_d(fty%e)
      else
        fv=self%nb_of_clo(fty); fv=self%nb_force(0,fv); fv=self%nb_whnf(0,fv)
        if (self%status%code/=accept) exit
        if (self%nb%tag(fv)/=vpi) then
          call self%status%fail(reject,'function expected'); exit
        end if
        dom_v=self%nb%domain(fv); pi_env=self%nb%env(fv); body=self%nb%body(fv)
      end if
      if (check) then
        aty=self%infer_go(arg,check)
        if (dom_e/=0) then
          self%rp%ctrs(4)=self%rp%ctrs(4)+1
          ok=self%rp%eq_mod(self%ctx%exprs,dom_e,pi_env,0,aty%e,aty%env,0,self%status)
          if (.not.ok .and. self%status%code==accept) then
            r=self%reify(aty); a=self%nb_of_clo(closure_t(dom_e,pi_env)); b=self%nb_of_clo(closure_t(r,env_nil))
            ok=self%nb_conv(0,a,b)
          end if
        else
          a=self%nb_of_clo(aty); ok=self%nb_conv(0,a,dom_v)
        end if
        if (.not.ok) call self%status%fail(reject,'application type mismatch')
      end if
      if (self%status%code/=accept) exit
      env=self%rp%push_entry(self%ctx%exprs,pi_env,ev_val,arg%e,arg%env,self%status)
      fty=closure_t(body,env)
    end do
    if (self%status%code==accept) out=fty
    self%work_top=mark
  end function
  recursive integer function ensure_sort_clo(self,c) result(l)
    class(checker), intent(inout) :: self
    type(closure_t), intent(in) :: c
    integer :: v
    l=0
    if (self%status%code/=accept) return
    if (self%ctx%exprs%get_tag(c%e)==esort) then
      l=self%ctx%exprs%get_a(c%e); return
    end if
    v=self%nb_of_clo(c); v=self%nb_force(0,v); v=self%nb_whnf(0,v)
    if (self%status%code/=accept) return
    if (self%nb%tag(v)==vsort) then
      l=self%nb%level(v)
    else
      call self%status%fail(reject,'type must infer to a sort')
    end if
  end function
  recursive integer function ensure_sort(self,e) result(l)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e
    integer :: v,r
    l=0
    if (self%status%code/=accept) return
    if (self%ctx%exprs%get_tag(e)==esort) then
      l=self%ctx%exprs%get_a(e); return
    end if
    v=self%nb_of_clo(closure_t(e,env_nil)); v=self%nb_force(0,v); v=self%nb_whnf(0,v); r=self%nb_readback(v)
    if (self%status%code/=accept) return
    if (self%ctx%exprs%get_tag(r)==esort) then
      l=self%ctx%exprs%get_a(r); l=self%ctx%levels%simplify(l)
    else
      call self%status%fail(reject,'type must infer to a sort')
    end if
  end function
  recursive integer function nb_of_clo(self,c) result(v)
    class(checker), intent(inout) :: self
    type(closure_t), intent(in) :: c
    v=0
    if (self%status%code/=accept) return
    v=self%nb%clo_val_cache%get(c%e,c%env,0,0)
    if (v/=0) return
    v=self%nb_eval(0,c%env,c%e)
    if (self%status%code==accept) call self%nb%clo_val_cache%put(c%e,c%env,0,0,v)
  end function
  recursive logical function is_def_eq(self,a,b) result(ok)
    class(checker), intent(inout) :: self
    type(closure_t), intent(in) :: a,b
    integer :: x,y
    ok=.false.
    if (self%status%code/=accept) return
    self%rp%ctrs(4)=self%rp%ctrs(4)+1
    x=self%nb_of_clo(a); y=self%nb_of_clo(b); ok=self%nb_conv(0,x,y)
  end function
  integer function cheap_beta_reduce(self,e) result(r)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e
    integer :: mark,cur,slot,n,i,j,idx
    r=e
    if (self%status%code/=accept) return
    if (self%ctx%exprs%get_tag(e)/=eapp) return
    mark=self%work_top; cur=e; n=0
    do while(self%ctx%exprs%get_tag(cur)==eapp)
      slot=self%work_slot(); self%w1(slot)=self%ctx%exprs%get_b(cur); cur=self%ctx%exprs%get_a(cur); n=n+1
    end do
    i=0
    do while(i<n)
      if (self%ctx%exprs%get_tag(cur)/=elam) exit
      cur=self%ctx%exprs%get_d(cur); i=i+1
    end do
    if (i>0) then
      if (self%ctx%exprs%get_nlbv(cur)==0) then
        r=cur
      else if (self%ctx%exprs%get_tag(cur)==evar) then
        idx=self%ctx%exprs%get_a(cur)
        if (idx<i) r=self%w1(mark+n-i+1+idx)
      end if
      if (r/=e) then
        do j=n-i,1,-1
          idx=self%w1(mark+j); r=self%ctx%exprs%app(r,idx)
        end do
      end if
    end if
    self%work_top=mark
  end function
