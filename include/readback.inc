  recursive integer function reify(self,c) result(e)
    class(checker), intent(inout) :: self
    type(closure_t), intent(in) :: c
    e=0
    if (self%status%code/=accept) return
    e=c%e
    if (c%env==env_nil .or. self%ctx%exprs%get_nlbv(e)==0) return
    e=self%reify_go(c%env,0,e)
  end function
  recursive integer function reify_go(self,env,off,e) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: env,off,e
    integer :: t,a,b,c,d,v,node,e2,env2
    out=0
    if (self%status%code/=accept) return
    out=e
    if (self%ctx%exprs%get_nlbv(e)<=off) return
    out=self%reify_go_cache%get(e,env,off,0)
    if (out/=0) return
    t=self%ctx%exprs%get_tag(e); a=self%ctx%exprs%get_a(e); b=self%ctx%exprs%get_b(e)
    c=self%ctx%exprs%get_c(e); d=self%ctx%exprs%get_d(e); v=self%ctx%exprs%get_e(e)
    select case(t)
    case(evar)
      node=self%rp%lookup(env,a-off,self%status)
      if (self%status%code/=accept) return
      e2=self%rp%entry(node); env2=self%rp%entry_env(node)
      select case(self%rp%entry_tag(node))
      case(entry_v)
        out=self%nb_readback(e2)
      case(entry_neu)
        out=e2
      case(ev_val)
        out=self%reify(closure_t(e2,env2))
      end select
    case(eapp)
      a=self%reify_go(env,off,a); b=self%reify_go(env,off,b)
      if (self%status%code==accept) out=self%ctx%exprs%app(a,b)
    case(elam,epi,elet)
      c=self%reify_go(env,off,c)
      if (t==elet) v=self%reify_go(env,off,v)
      d=self%reify_go(env,off+1,d)
      if (self%status%code==accept) out=self%ctx%exprs%intern(t,a,b,c,d,v)
    case(eproj)
      c=self%reify_go(env,off,c)
      if (self%status%code==accept) out=self%ctx%exprs%proj(a,b,c)
    case default
      out=e
    end select
    if (self%status%code==accept) call self%reify_go_cache%put(e,env,off,0,out)
  end function
  recursive integer function nb_readback(self,v) result(e)
    class(checker), intent(inout) :: self
    integer, intent(in) :: v
    integer :: t,n,style,ty,body,env,ls,s,h,domain
    e=0
    if (self%status%code/=accept) return
    t=self%nb%tag(v); env=self%nb%env(v); body=self%nb%body(v)
    n=self%nb%binder_name(v); style=self%nb%binder_style(v)
    select case(t)
    case(vthunk)
      ty=self%nb%expr(v); e=self%reify(closure_t(ty,env))
    case(vnat)
      ty=self%nb%literal(v); e=self%ctx%exprs%intern(enat,ty,0,0,0,0)
    case(vstr)
      ty=self%nb%literal(v); e=self%ctx%exprs%intern(estr,ty,0,0,0,0)
    case(vsort)
      ty=self%nb%level(v); ty=self%ctx%levels%simplify(ty); e=self%ctx%exprs%sort(ty)
    case(vlam)
      ty=self%nb%binder_type(v); e=self%ctx%exprs%binder(elam,n,style,ty,body)
      e=self%reify(closure_t(e,env))
    case(vpi)
      domain=self%nb%domain(v); ty=self%nb_readback(domain)
      if (self%status%code/=accept) return
      e=self%ctx%exprs%binder(epi,n,style,ty,body); e=self%reify(closure_t(e,env))
    case(vunfold)
      n=self%nb%name(v); ls=self%nb%levels(v); s=self%nb%spine(v)
      e=self%ctx%exprs%const(n,ls); e=self%nb_readback_spine(e,s)
    case(vrigid)
      h=self%nb%head(v); s=self%nb%spine(v)
      select case(self%nb%head_tag(h))
      case(hlocal)
        e=self%nb%head_a(h)
      case(hconst)
        n=self%nb%head_b(h); ls=self%nb%head_c(h); e=self%ctx%exprs%const(n,ls)
      case default
        call self%status%fail(kernel_error,'conversion-local variable escaped into readback'); return
      end select
      e=self%nb_readback_spine(e,s)
    end select
  end function
  recursive integer function nb_readback_spine(self,head,s) result(e)
    class(checker), intent(inout) :: self
    integer, intent(in) :: head,s
    integer :: p,t,a,b
    e=head
    if (self%status%code/=accept .or. s==spine_empty) return
    p=self%nb%spines%parent(s); t=self%nb%spines%tag(s); a=self%nb%spines%a(s); b=self%nb%spines%b(s)
    e=self%nb_readback_spine(head,p)
    if (t==elim_app) then
      a=self%nb_readback(a)
      if (self%status%code==accept) e=self%ctx%exprs%app(e,a)
    else
      if (self%status%code==accept) e=self%ctx%exprs%proj(a,b,e)
    end if
  end function
