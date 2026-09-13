! Projection inference retains nanoclo's closure path and Prop restrictions.
  recursive logical function is_prop_of(self,e) result(ok)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e
    type(closure_t) :: sort
    integer :: cached,v,l
    ok=.false.
    if (self%status%code/=accept) return
    cached=self%prop_cache%get(e,0,0,0)
    if (cached/=0) then
      ok=cached==1; return
    end if
    sort=self%infer_go(closure_t(e,env_nil),.false.); v=self%nb_of_clo(sort)
    v=self%nb_force(0,v); v=self%nb_whnf(0,v)
    if (self%status%code/=accept) return
    if (self%nb%tag(v)==vsort) then
      l=self%nb%level(v); ok=self%ctx%levels%is_zero(l)
    end if
    call self%prop_cache%put(e,0,0,0,merge(1,2,ok))
  end function
  recursive logical function may_be_prop_of(self,ty) result(ok)
    class(checker), intent(inout) :: self
    type(closure_t), intent(in) :: ty
    type(closure_t) :: sort
    integer :: v,l
    ok=.false.; sort=self%infer_go(ty,.false.); v=self%nb_of_clo(sort)
    v=self%nb_force(0,v); v=self%nb_whnf(0,v)
    if (self%status%code/=accept) return
    if (self%nb%tag(v)==vsort) then
      l=self%nb%level(v); ok=.not.self%ctx%levels%is_never_zero(l)
    end if
  end function
  recursive function infer_proj(self,n,idx,strukt,check) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: n,idx
    type(closure_t), intent(in) :: strukt
    logical, intent(in) :: check
    type(closure_t) :: out,sty,r
    integer :: mark,v,h,ls,s,args,ind,np,ni,ctor,id,ty,params,i,env,body,dom,d,bv,proj,senv,a
    logical :: prop,ok
    out=closure_t(); mark=self%work_top
    attempt: block
      sty=self%infer_go(strukt,check); v=self%nb_of_clo(sty); v=self%nb_force(0,v); v=self%nb_whnf(0,v)
      if (self%status%code/=accept) exit attempt
      if (self%nb%tag(v)/=vrigid) exit attempt
      h=self%nb%head(v)
      if (self%nb%head_tag(h)/=hconst .or. self%nb%head_b(h)/=n) exit attempt
      ls=self%nb%head_c(h); s=self%nb%spine(v); ind=self%ctx%decls%lookup(n,self%cutoff)
      if (ind==0) exit attempt
      if (self%ctx%decls%tag(ind)/=dinductive) exit attempt
      if (self%ctx%lists%length(self%ctx%decls%ctor_names(ind))/=1) exit attempt
      np=self%ctx%decls%num_params(ind); ni=self%ctx%decls%num_indices(ind); args=self%spine_args(s)
      if (args<0 .or. self%nb%spines%len(s)/=np+ni) exit attempt
      ctor=self%ctx%lists%head(self%ctx%decls%ctor_names(ind)); id=self%ctx%decls%lookup(ctor,self%cutoff)
      if (id==0) exit attempt
      ty=self%ctx%decls%ty(id); params=self%ctx%decls%uparams(id)
      ty=self%ctx%exprs%subst_expr_levels(self%ctx%levels,self%ctx%lists,ty,params,ls,self%status)
      r=closure_t(ty,env_nil)
      do i=1,np
        v=self%nb_of_clo(r); v=self%nb_force(0,v); v=self%nb_whnf(0,v)
        if (self%status%code/=accept) exit attempt
        if (self%nb%tag(v)/=vpi) exit attempt
        env=self%nb%env(v); body=self%nb%body(v); a=self%w1(args+i)
        env=self%rp%push_entry_v(env,a); r=closure_t(body,env)
      end do
      prop=self%may_be_prop_of(sty)
      do i=0,idx-1
        v=self%nb_of_clo(r); v=self%nb_force(0,v); v=self%nb_whnf(0,v)
        if (self%status%code/=accept) exit attempt
        if (self%nb%tag(v)/=vpi) exit attempt
        env=self%nb%env(v); body=self%nb%body(v); dom=self%nb%domain(v)
        if (self%ctx%exprs%get_nlbv(body)>0 .and. prop) then
          d=self%nb_readback(dom); ok=self%is_prop_of(d)
          if (.not.ok) exit attempt
        end if
        bv=self%ctx%exprs%var(0); proj=self%ctx%exprs%proj(n,i,bv)
        senv=self%rp%push_entry(self%ctx%exprs,env_nil,ev_val,strukt%e,strukt%env,self%status)
        env=self%rp%push_entry(self%ctx%exprs,env,ev_val,proj,senv,self%status)
        r=closure_t(body,env)
      end do
      v=self%nb_of_clo(r); v=self%nb_force(0,v); v=self%nb_whnf(0,v)
      if (self%status%code/=accept) exit attempt
      if (self%nb%tag(v)/=vpi) exit attempt
      dom=self%nb%domain(v); d=self%nb_readback(dom)
      if (prop) then
        ok=self%is_prop_of(d)
        if (.not.ok) exit attempt
      end if
      out=closure_t(d,env_nil)
    end block attempt
    self%work_top=mark
    if (out%e==0) call self%status%fail(reject,'invalid structure projection or Prop elimination')
  end function
