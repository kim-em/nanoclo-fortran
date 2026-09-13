! Evaluation/readback translated from nanoclo nbe_eval.rs.
  recursive integer function nb_eval(self,depth,env_in,e) result(v)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,env_in,e
    integer :: env,t,a,b,c,d,x,cur,mark,n,i,h,s,kind
    logical :: batchable
    v=0
    if (self%status%code/=accept) return
    t=self%ctx%exprs%get_tag(e); a=self%ctx%exprs%get_a(e); b=self%ctx%exprs%get_b(e)
    c=self%ctx%exprs%get_c(e); d=self%ctx%exprs%get_d(e)
    env=env_in
    if (self%ctx%exprs%get_nlbv(e)==0) env=env_nil
    select case(t)
    case(evar)
      x=self%rp%lookup(env,a,self%status)
      if (self%status%code/=accept) return
      v=self%entry_val(x); v=self%nb_force(depth,v)
    case(esort)
      a=self%ctx%levels%simplify(a); v=self%nb%mk_sort(a)
    case(enat)
      v=self%nb%mk_nat(a)
    case(estr)
      v=self%nb%mk_str(a)
    case(elocal)
      v=self%nb_local(e)
    case(econst)
      v=self%nb_const(a,b)
    case(eapp)
      mark=self%work_top; n=0; cur=e
      do while(self%ctx%exprs%get_tag(cur)==eapp)
        x=self%work_slot(); self%w1(x)=self%ctx%exprs%get_b(cur)
        cur=self%ctx%exprs%get_a(cur); n=n+1
      end do
      v=self%nb_eval(depth,env,cur); i=n
      do while(i>0 .and. self%status%code==accept)
        kind=self%nb%tag(v)
        batchable=kind==vrigid .or. kind==vunfold
        if (kind==vrigid .and. self%nat_extension) then
          h=self%nb%head(v)
          if (self%nb%head_tag(h)==hconst .and. self%nb%head_a(h)==cctor) then
            if (self%nb%head_b(h)==self%nat_ops(1)) batchable=.false.
          end if
        else if (kind==vunfold .and. self%nat_extension) then
          a=self%nb%name(v)
          if (self%nat_op(a)/=0) batchable=.false.
        end if
        if (batchable) then
          s=self%nb%spine(v)
          if (kind==vrigid) then
            h=self%nb%head(v)
          else
            a=self%nb%name(v); b=self%nb%levels(v)
          end if
          do while(i>0 .and. self%status%code==accept)
            x=self%w1(mark+i); i=i-1
            x=self%nb_delay(depth,env,x)
            if (self%status%code/=accept) exit
            s=self%nb%spines%snoc(s,elim_app,x,0)
          end do
          if (self%status%code/=accept) exit
          if (kind==vrigid) then
            v=self%nb%mk_rigid(h,s)
          else
            v=self%nb%mk_unfold(a,b,s)
          end if
          exit
        end if
        x=self%w1(mark+i); i=i-1
        x=self%nb_delay(depth,env,x); v=self%nb_apply(depth,v,x)
      end do
      self%work_top=mark
    case(elam)
      v=self%nb%mk_lam(a,b,c,env,d)
    case(epi)
      x=self%nb_delay(depth,env,c)
      if (self%status%code==accept) v=self%nb%mk_pi(a,b,x,env,d)
    case(elet)
      cur=e
      do while(self%ctx%exprs%get_tag(cur)==elet)
        a=self%ctx%exprs%get_e(cur); b=self%ctx%exprs%get_d(cur)
        x=self%nb_eval(depth,env,a)
        if (self%status%code/=accept) return
        env=self%rp%push_entry_v(env,x); cur=b
      end do
      v=self%nb_eval(depth,env,cur)
    case(eproj)
      x=self%nb_eval(depth,env,c); v=self%nb_proj(depth,a,b,x)
    case default
      call self%status%fail(kernel_error,'invalid expression tag in evaluation')
    end select
  end function
  recursive integer function nb_delay(self,depth,env,e) result(v)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,env,e
    integer :: actual_env
    v=0
    if (self%status%code/=accept) return
    actual_env=env
    if (self%ctx%exprs%get_nlbv(e)==0) actual_env=env_nil
    select case(self%ctx%exprs%get_tag(e))
    case(evar,esort,enat,estr,elocal,econst)
      v=self%nb_eval(depth,actual_env,e)
    case default
      v=self%nb_thunk(actual_env,e)
    end select
  end function
  integer function nb_thunk(self,env,e) result(v)
    class(checker), intent(inout) :: self
    integer, intent(in) :: env,e
    integer :: key
    v=0
    if (self%status%code/=accept) return
    key=self%rp%read_key(self%ctx%exprs,e,env)
    v=self%nb%mk_thunk_keyed(key,env,e)
  end function
  integer function entry_val(self,node) result(v)
    class(checker), intent(inout) :: self
    integer, intent(in) :: node
    integer :: e,env
    v=0
    if (self%status%code/=accept) return
    e=self%rp%entry(node); env=self%rp%entry_env(node)
    select case(self%rp%entry_tag(node))
    case(entry_v)
      v=e
    case(ev_val)
      if (self%ctx%exprs%get_nlbv(e)==0) env=env_nil
      v=self%nb_thunk(env,e)
    case(entry_neu)
      v=self%nb_local(e)
    end select
  end function
  integer function nb_local(self,e) result(v)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e
    integer :: head
    v=self%nb%local_cache%get(e,0,0,0)
    if (v/=0) return
    head=self%nb%mk_head(hlocal,e,0,0); v=self%nb%mk_rigid(head,spine_empty)
    call self%nb%local_cache%put(e,0,0,0,v)
  end function
  integer function nb_const(self,name,ls) result(v)
    class(checker), intent(inout) :: self
    integer, intent(in) :: name,ls
    integer :: id,kind,h
    v=0
    if (self%status%code/=accept) return
    v=self%nb%const_val_cache%get(name,ls,0,0)
    if (v/=0) return
    if (name==self%reduce_nat .or. name==self%reduce_bool) then
      call self%status%fail(reject,'native reduction is unsupported'); return
    end if
    id=self%ctx%decls%lookup(name,self%cutoff); kind=caxiom
    if (id/=0) then
      select case(self%ctx%decls%tag(id))
      case(ddefinition,dtheorem)
        v=self%nb%mk_unfold(name,ls,spine_empty)
      case(dconstructor)
        kind=cctor
      case(drecursor)
        kind=crecursor
      case(dquot)
        kind=cquot
      case(dinductive)
        kind=cinductive
      end select
    end if
    if (v==0) then
      h=self%nb%mk_head(hconst,kind,name,ls); v=self%nb%mk_rigid(h,spine_empty)
    end if
    call self%nb%const_val_cache%put(name,ls,0,0,v)
  end function
  recursive integer function nb_force(self,depth,v) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,v
    integer :: e,env
    out=0
    if (self%status%code/=accept) return
    out=v
    if (self%nb%tag(v)/=vthunk) return
    out=self%nb%forced(v)
    if (out/=0) return
    e=self%nb%expr(v); env=self%nb%env(v)
    out=self%nb_eval(depth,env,e); out=self%nb_force(depth,out)
    if (self%status%code==accept) call self%nb%set_forced(v,out)
  end function
  recursive integer function nb_apply(self,depth,f,a) result(v)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,f,a
    integer :: env,body,h,s,n,ls,f2,num,p
    v=0
    if (self%status%code/=accept) return
    select case(self%nb%tag(f))
    case(vlam)
      env=self%nb%env(f); body=self%nb%body(f)
      env=self%rp%push_entry_v(env,a); v=self%nb_eval(depth,env,body)
    case(vrigid)
      h=self%nb%head(f); s=self%nb%spine(f)
      s=self%nb%spines%snoc(s,elim_app,a,0)
      if (self%nat_extension .and. self%nb%head_tag(h)==hconst .and. self%nb%head_a(h)==cctor) then
        if (self%nb%head_b(h)==self%nat_ops(1)) then
          num=self%nb_bignum(depth,a,.false.)
          if (self%status%code/=accept) return
          if (num/=0) then
            p=self%ctx%bignums%add(num,self%big_one); v=self%nb%mk_nat(p); return
          end if
        end if
      end if
      v=self%nb%mk_rigid(h,s)
    case(vunfold)
      n=self%nb%name(f); ls=self%nb%levels(f); s=self%nb%spine(f)
      s=self%nb%spines%snoc(s,elim_app,a,0)
      if (self%nat_extension .and. self%nat_op(n)/=0) then
        v=self%nb_nat_red(depth,n,s,.false.)
        if (v/=0 .or. self%status%code/=accept) return
      end if
      v=self%nb%mk_unfold(n,ls,s)
    case(vthunk)
      f2=self%nb_force(depth,f); v=self%nb_apply(depth,f2,a)
    case default
      call self%status%fail(reject,'application of non-function')
    end select
  end function
  recursive integer function nb_lam_domain(self,depth,v) result(d)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,v
    integer :: e,env
    d=0
    if (self%status%code/=accept) return
    select case(self%nb%tag(v))
    case(vlam)
      d=self%nb%domain(v)
      if (d/=0) return
      e=self%nb%binder_type(v); env=self%nb%env(v)
      d=self%nb_eval(depth,env,e)
      if (self%status%code==accept) call self%nb%set_domain(v,d)
    case(vpi)
      e=self%nb%domain(v); d=self%nb_force(depth,e)
    case default
      call self%status%fail(kernel_error,'domain requested for non-binder')
    end select
  end function
  recursive integer function nb_open(self,depth,binder,a) result(v)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,binder,a
    integer :: env,body
    v=0
    if (self%status%code/=accept) return
    env=self%nb%env(binder); body=self%nb%body(binder)
    env=self%rp%push_entry_v(env,a); v=self%nb_eval(depth,env,body)
  end function
  recursive integer function nb_whnf(self,depth,v) result(cur)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,v
    integer :: next,h
    cur=v
    do while(self%status%code==accept)
      cur=self%nb_force(depth,cur)
      if (self%status%code/=accept) return
      select case(self%nb%tag(cur))
      case(vunfold)
        next=self%nb_unfold(depth,cur)
        if (next==cur) return
        cur=next
      case(vrigid)
        h=self%nb%head(cur)
        if (self%nb%head_tag(h)==hconst) then
          if (self%nb%head_a(h)==crecursor .or. self%nb%head_a(h)==cquot) then
            next=self%nb_iota(depth,cur)
            if (next==0) return
            cur=next; cycle
          end if
        end if
        return
      case default
        return
      end select
    end do
  end function
  recursive integer function nb_unfold(self,depth,v) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,v
    out=self%nb_unfold_go(depth,v,.false.)
  end function
  recursive integer function nb_unfold_demand(self,depth,v) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,v
    out=self%nb_unfold_go(depth,v,self%nb%probe_depth==0)
  end function
  recursive integer function nb_unfold_go(self,depth,v,force) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,v
    logical, intent(in) :: force
    integer :: n,ls,s
    logical :: deferred
    out=0
    if (self%status%code/=accept) return
    out=v
    if (self%nb%tag(v)/=vunfold) return
    out=self%nb%forced(v)
    if (out/=0) return
    n=self%nb%name(v); ls=self%nb%levels(v); s=self%nb%spine(v)
    if (self%nat_extension .and. self%nat_op(n)/=0) then
      out=self%nb_nat_red(depth,n,s,.true.)
      if (self%status%code/=accept) return
      if (out/=0) then
        call self%nb%set_forced(v,out); return
      end if
      if (.not.force) then
        deferred=self%nb_nat_defer(depth,n,s)
        if (deferred) then
          out=v; return
        end if
      end if
    end if
    out=self%nb_unfold_const(n,ls)
    if (self%status%code/=accept) return
    if (out==0) then
      out=v
    else
      out=self%nb_replay(depth,out,s)
    end if
    if (self%status%code==accept) call self%nb%set_forced(v,out)
  end function
  recursive integer function nb_unfold_const(self,n,ls) result(v)
    class(checker), intent(inout) :: self
    integer, intent(in) :: n,ls
    integer :: id,params,e,key,cached
    v=0
    if (self%status%code/=accept) return
    cached=self%nb%unfold_cache%get(n,ls,0,0)
    if (cached/=0) then
      v=max(0,cached); return
    end if
    id=self%ctx%decls%lookup(n,self%cutoff)
    if (id/=0) then
      if (self%ctx%decls%tag(id)==ddefinition .or. self%ctx%decls%tag(id)==dtheorem) then
        params=self%ctx%decls%uparams(id); e=self%ctx%decls%value(id)
        if (self%ctx%lists%length(params)==self%ctx%lists%length(ls)) then
          if (ls/=list_nil .and. self%ctx%decls%temp_base/=0) then
            e=self%ctx%exprs%subst_expr_levels(self%ctx%levels,self%ctx%lists,e,params,ls,self%status)
          else if (ls/=list_nil) then
            key=self%ctx%exprs%const(n,ls); cached=self%g_unfold%get(key,0,0,0)
            if (cached/=0) then
              e=cached
            else
              e=self%ctx%exprs%subst_expr_levels(self%ctx%levels,self%ctx%lists,e,params,ls,self%status)
              if (self%status%code==accept) call self%g_unfold%put(key,0,0,0,e)
            end if
          end if
          v=self%nb_eval(0,env_nil,e)
        end if
      end if
    end if
    if (self%status%code==accept) call self%nb%unfold_cache%put(n,ls,0,0,merge(v,-1,v/=0))
  end function
  recursive integer function nb_replay(self,depth,base,s) result(v)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,base,s
    integer :: p,t,a,b
    v=base
    if (self%status%code/=accept .or. s==spine_empty) return
    p=self%nb%spines%parent(s); t=self%nb%spines%tag(s); a=self%nb%spines%a(s); b=self%nb%spines%b(s)
    v=self%nb_replay(depth,base,p)
    if (t==elim_app) then
      v=self%nb_apply(depth,v,a)
    else
      v=self%nb_proj(depth,a,b,v)
    end if
  end function
  recursive integer function nb_proj(self,depth,name,idx,v) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,name,idx,v
    integer :: cur,s,h,n,ls,id,node,a
    out=0; cur=self%nb_whnf(depth,v)
    if (self%status%code/=accept) return
    s=self%nb%spine(cur)
    select case(self%nb%tag(cur))
    case(vrigid)
      h=self%nb%head(cur)
      if (self%nb%head_tag(h)==hconst .and. self%nb%head_a(h)==cctor) then
        n=self%nb%head_b(h); id=self%ctx%decls%lookup(n,self%cutoff)
        if (id/=0) then
          if (self%ctx%decls%tag(id)==dconstructor .and. self%ctx%decls%inductive_name(id)==name) then
            node=self%nb%spines%get(s,self%ctx%decls%num_params(id)+idx)
            if (node/=0) then
              if (self%nb%spines%tag(node)==elim_app) then
                a=self%nb%spines%a(node); out=self%nb_force(depth,a); return
              end if
            end if
          end if
        end if
      end if
      s=self%nb%spines%snoc(s,elim_proj,name,idx); out=self%nb%mk_rigid(h,s)
    case(vunfold)
      n=self%nb%name(cur); ls=self%nb%levels(cur)
      s=self%nb%spines%snoc(s,elim_proj,name,idx); out=self%nb%mk_unfold(n,ls,s)
    case(vnat,vstr)
      a=self%nb%literal(cur)
      if (self%nb%tag(cur)==vnat) then
        cur=self%nb_nat_to_ctor(a)
      else
        cur=self%nb_str_to_ctor(depth,a)
      end if
      if (self%status%code/=accept) return
      if (cur==0) then
        call self%status%fail(reject,'literal constructor unavailable'); return
      end if
      out=self%nb_proj(depth,name,idx,cur)
    case default
      call self%status%fail(reject,'projection of non-structure')
    end select
  end function
