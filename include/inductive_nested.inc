! Specialization and restoration from nanoclo src/inductive.rs.
  integer function expr_list_subst(self,e,locals,abstract) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e,locals
    logical, intent(in) :: abstract
    integer :: mark,k,n,i,x
    mark=self%work_top; n=self%ctx%lists%length(locals); k=locals
    do i=1,n
      x=self%work_slot(); self%w3(x)=self%ctx%lists%head(k); k=self%ctx%lists%tail(k)
    end do
    if (abstract) then
      out=self%ctx%exprs%abstr(e,self%w3(mark+1:mark+n))
    else
      out=self%ctx%exprs%inst(e,self%w3(mark+1:mark+n))
    end if
    self%work_top=mark
  end function
  integer function inst_forall_params(self,e,np,args) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e,np,args
    integer :: i,k,a,params
    out=e; k=args; params=list_nil
    do i=1,np
      if (self%ctx%exprs%get_tag(out)/=epi .or. k==list_nil) then
        call self%status%fail(reject,'nested container parameter telescope'); return
      end if
      out=self%ctx%exprs%get_d(out); a=self%ctx%lists%head(k)
      params=self%ctx%lists%cons(a,params); k=self%ctx%lists%tail(k)
    end do
    params=self%list_reverse(params); out=self%expr_list_subst(out,params,.false.)
  end function
  integer function replace_if_nested(self,e,outgoing) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e,outgoing
    integer :: fn,args,n,id,k,i,arg,np,params,rest,ip,ls,j,ind,js,aux,base,ty,ks,cs,c,cn,ct,ci,ctors,ignored
    logical :: nested,loose,found
    out=0
    if (self%ctx%exprs%get_tag(e)/=eapp) return
    call self%unfold_apps(e,fn,args)
    if (self%ctx%exprs%get_tag(fn)/=econst) return
    n=self%ctx%exprs%get_a(fn); ls=self%ctx%exprs%get_b(fn); id=self%ctx%decls%lookup(n,self%cutoff)
    if (id==0) return
    if (self%ctx%decls%tag(id)/=dinductive) return
    np=self%ctx%decls%num_params(id)
    if (np>self%ctx%lists%length(args)) return
    k=args; nested=.false.; loose=.false.; params=list_nil
    do i=1,np
      arg=self%ctx%lists%head(k); loose=loose .or. self%ctx%exprs%get_nlbv(arg)/=0
      found=self%find_const(arg,self%ist%all_names,.false.); nested=nested .or. found
      params=self%ctx%lists%cons(arg,params); k=self%ctx%lists%tail(k)
    end do
    if (.not.nested) return
    if (loose) then
      call self%status%fail(reject,'loose variable in nested type'); return
    end if
    rest=k; params=self%list_reverse(params); ip=self%fold_apps(fn,params)
    ip=self%expr_list_subst(ip,outgoing,.true.); ip=self%expr_list_subst(ip,self%ist%local_params,.false.)
    do i=1,self%ist%nested_count
      if (self%ist%nested_expr(i)/=ip) cycle
      aux=self%ist%nested_name(i); fn=self%ctx%exprs%const(aux,self%ist%uparams)
      fn=self%fold_apps(fn,outgoing); out=self%fold_apps(fn,rest); return
    end do
    j=self%ctx%decls%all_names(id)
    do while(j/=list_nil .and. self%status%code==accept)
      arg=self%ctx%lists%head(j); ind=self%ctx%decls%lookup(arg,self%cutoff)
      if (ind==0) then
        call self%status%fail(reject,'missing nested mutual'); return
      end if
      if (self%ctx%decls%tag(ind)/=dinductive) then
        call self%status%fail(reject,'nested mutual is not inductive'); return
      end if
      fn=self%ctx%exprs%const(arg,ls); js=self%fold_apps(fn,params)
      base=self%name_id('_nested'); base=self%ctx%names%concat(base,arg)
      do
        aux=self%index_name(base,self%ist%next_name); self%ist%next_name=self%ist%next_name+1
        if (self%ctx%decls%by_name%get(aux,0,0,0)==0) exit
      end do
      ty=self%ctx%decls%ty(ind); ks=self%ctx%decls%uparams(ind)
      ty=self%ctx%exprs%subst_expr_levels(self%ctx%levels,self%ctx%lists,ty,ks,ls,self%status)
      ty=self%inst_forall_params(ty,np,args); ty=self%abstr_binders(epi,outgoing,ty)
      js=self%expr_list_subst(js,outgoing,.true.); js=self%expr_list_subst(js,self%ist%local_params,.false.)
      i=self%ist%nested_count+1; self%ist%nested_count=i
      call grow(self%ist%nested_name,i); call grow(self%ist%nested_expr,i); call grow(self%ist%nested_closed,i)
      self%ist%nested_name(i)=aux; self%ist%nested_expr(i)=js
      if (arg==n) then
        fn=self%ctx%exprs%const(aux,self%ist%uparams); fn=self%fold_apps(fn,outgoing); out=self%fold_apps(fn,rest)
      end if
      cs=self%ctx%decls%ctor_names(ind); ctors=list_nil
      do while(cs/=list_nil .and. self%status%code==accept)
        cn=self%ctx%lists%head(cs); c=self%ctx%decls%lookup(cn,self%cutoff)
        if (c==0) then
          call self%status%fail(reject,'missing nested constructor'); return
        end if
        if (self%ctx%decls%tag(c)/=dconstructor) then
          call self%status%fail(reject,'nested constructor expected'); return
        end if
        cn=self%ctx%names%replace_pfx(cn,arg,aux); ct=self%ctx%decls%ty(c); ks=self%ctx%decls%uparams(c)
        ct=self%ctx%exprs%subst_expr_levels(self%ctx%levels,self%ctx%lists,ct,ks,ls,self%status)
        ct=self%inst_forall_params(ct,np,args); ct=self%abstr_binders(epi,outgoing,ct)
        ci=self%ist%add_ctor(cn,ct); ctors=self%ctx%lists%cons(ci,ctors); cs=self%ctx%lists%tail(cs)
      end do
      ctors=self%list_reverse(ctors); ignored=self%ist%add_type(aux,ty,ctors)
      ! Keep the specialization search's set in the same insertion order as Rust.
      k=self%list_reverse(self%ist%all_names); k=self%ctx%lists%cons(aux,k); self%ist%all_names=self%list_reverse(k)
      j=self%ctx%lists%tail(j)
    end do
  end function
  recursive integer function replace_all_nested(self,e,outgoing) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e,outgoing
    integer :: t,a,b,c,d,v
    out=0
    if (self%status%code/=accept) return
    out=self%replace_if_nested(e,outgoing)
    if (out/=0 .or. self%status%code/=accept) return
    t=self%ctx%exprs%get_tag(e); a=self%ctx%exprs%get_a(e); b=self%ctx%exprs%get_b(e); c=self%ctx%exprs%get_c(e)
    d=self%ctx%exprs%get_d(e); v=self%ctx%exprs%get_e(e); out=e
    select case(t)
    case(eapp)
      a=self%replace_all_nested(a,outgoing); b=self%replace_all_nested(b,outgoing)
    case(epi,elam,elet)
      c=self%replace_all_nested(c,outgoing)
      if (t==elet) v=self%replace_all_nested(v,outgoing)
      d=self%replace_all_nested(d,outgoing)
    case(eproj)
      c=self%replace_all_nested(c,outgoing)
    case default
      return
    end select
    if (self%status%code==accept) out=self%ctx%exprs%intern(t,a,b,c,d,v)
  end function
  subroutine specialize_nested(self)
    class(checker), intent(inout) :: self
    integer :: params,tail,i,k,c,e,ty,np
    self%ist%base_count=self%ist%count; self%ist%base_names=self%ist%all_names
    e=self%ist%ty(1); np=self%ist%num_params
    call self%get_local_params(e,np,params,tail); self%ist%local_params=params
    i=1
    do while(i<=self%ist%count .and. self%status%code==accept)
      k=self%ist%ctors(i)
      do while(k/=list_nil .and. self%status%code==accept)
        c=self%ctx%lists%head(k); ty=self%ist%ctor_ty(c)
        call self%get_local_params(ty,np,params,tail)
        e=self%replace_all_nested(tail,params); e=self%abstr_binders(epi,params,e)
        if (self%status%code/=accept) return
        if (self%ctx%exprs%get_flags(e)/=0) call self%status%fail(reject,'free variable in specialized constructor')
        self%ist%ctor_ty(c)=e; k=self%ctx%lists%tail(k)
      end do
      i=i+1
    end do
    do i=1,self%ist%nested_count
      e=self%ist%nested_expr(i); self%ist%nested_closed(i)=self%expr_list_subst(e,self%ist%local_params,.true.)
    end do
    self%ist%nested=self%ist%nested_count/=0
    do i=1,self%ist%count
      if (self%ctx%exprs%get_flags(self%ist%ty(i))/=0) call self%status%fail(reject,'free variable in inductive type')
    end do
  end subroutine
  integer function restored_rec_name(self,n) result(out)
    class(checker), intent(in) :: self
    integer, intent(in) :: n
    integer :: i
    out=n
    do i=1,self%ist%nested_count
      if (self%ist%rec_from(i)==n) then
        out=self%ist%rec_to(i); return
      end if
    end do
  end function
  integer function restore_ctor_name(self,n) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: n
    integer :: id,p,i,fn,args,e,base
    out=0; id=self%ctx%decls%lookup(n,self%cutoff)
    if (id/=0) then
      if (self%ctx%decls%tag(id)==dconstructor) then
        p=self%ctx%decls%inductive_name(id)
        do i=1,self%ist%nested_count
          if (self%ist%nested_name(i)/=p) cycle
          e=self%ist%nested_closed(i); call self%unfold_apps(e,fn,args); base=self%ctx%exprs%get_a(fn)
          out=self%ctx%names%replace_pfx(n,p,base); return
        end do
      end if
    end if
    call self%status%fail(reject,'missing specialized constructor')
  end function
  integer function restore_f(self,e,locals) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e,locals
    integer :: n,new,fn,args,tail,i,inner,id,p,nested,base,ia,ls
    out=0
    if (self%ctx%exprs%get_tag(e)==econst) then
      n=self%ctx%exprs%get_a(e); new=self%restored_rec_name(n)
      if (new/=n) then
        ls=self%ctx%exprs%get_b(e); out=self%ctx%exprs%const(new,ls); return
      end if
    end if
    call self%unfold_apps(e,fn,args)
    if (self%ctx%exprs%get_tag(fn)/=econst) return
    n=self%ctx%exprs%get_a(fn); nested=0; p=0
    do i=1,self%ist%nested_count
      if (self%ist%nested_name(i)==n) nested=i
    end do
    if (nested==0) then
      id=self%ctx%decls%lookup(n,self%cutoff)
      if (id==0) return
      if (self%ctx%decls%tag(id)/=dconstructor) return
      p=self%ctx%decls%inductive_name(id)
      do i=1,self%ist%nested_count
        if (self%ist%nested_name(i)==p) nested=i
      end do
      if (nested==0) return
    end if
    tail=args
    do i=1,self%ist%num_params
      if (tail==list_nil) then
        call self%status%fail(reject,'under-applied specialized type'); return
      end if
      tail=self%ctx%lists%tail(tail)
    end do
    inner=self%ist%nested_closed(nested); inner=self%expr_list_subst(inner,locals,.false.)
    if (p/=0) then
      call self%unfold_apps(inner,fn,ia)
      if (self%ctx%exprs%get_tag(fn)/=econst) then
        call self%status%fail(reject,'restored container is not constant'); return
      end if
      base=self%ctx%exprs%get_a(fn); ls=self%ctx%exprs%get_b(fn); new=self%ctx%names%replace_pfx(n,p,base)
      fn=self%ctx%exprs%const(new,ls); inner=self%fold_apps(fn,ia)
    end if
    out=self%fold_apps(inner,tail)
  end function
  recursive integer function restore_replace(self,e,locals) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e,locals
    integer :: t,a,b,c,d,v
    out=0
    if (self%status%code/=accept) return
    out=self%restore_f(e,locals)
    if (out/=0 .or. self%status%code/=accept) return
    t=self%ctx%exprs%get_tag(e); a=self%ctx%exprs%get_a(e); b=self%ctx%exprs%get_b(e); c=self%ctx%exprs%get_c(e)
    d=self%ctx%exprs%get_d(e); v=self%ctx%exprs%get_e(e); out=e
    select case(t)
    case(eapp)
      a=self%restore_replace(a,locals); b=self%restore_replace(b,locals)
    case(epi,elam,elet)
      c=self%restore_replace(c,locals)
      if (t==elet) v=self%restore_replace(v,locals)
      d=self%restore_replace(d,locals)
    case(eproj)
      c=self%restore_replace(c,locals)
    case default
      return
    end select
    if (self%status%code==accept) out=self%ctx%exprs%intern(t,a,b,c,d,v)
  end function
  integer function restore_e(self,e) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e
    integer :: locals,i,t,n,s,ty,b,x,tag
    out=e; locals=list_nil; tag=merge(epi,elam,self%ctx%exprs%get_tag(e)==epi)
    do i=1,self%ist%num_params
      t=self%ctx%exprs%get_tag(out)
      if (t/=epi .and. t/=elam) then
        call self%status%fail(reject,'restoring parameter telescope'); return
      end if
      n=self%ctx%exprs%get_a(out); s=self%ctx%exprs%get_b(out); ty=self%ctx%exprs%get_c(out); b=self%ctx%exprs%get_d(out)
      x=self%mk_unique(n,s,ty); out=self%ctx%exprs%inst(b,[x]); locals=self%ctx%lists%cons(x,locals)
    end do
    locals=self%list_reverse(locals); out=self%restore_replace(out,locals); out=self%abstr_binders(tag,locals,out)
  end function
  subroutine check_restored_recursor(self,new)
    class(checker), intent(inout) :: self
    integer, intent(in) :: new
    integer :: n,resolved,old,ty,e,k,r,vals,ctors,v,c,ro,x,a,b
    logical :: ok
    n=self%ctx%decls%name(new); resolved=self%restored_rec_name(n)
    e=self%ctx%decls%ty(new); ty=self%restore_e(e)
    k=self%ctx%decls%rules(new); vals=list_nil; ctors=list_nil
    do while(k/=list_nil .and. self%status%code==accept)
      r=self%ctx%lists%head(k); e=self%ctx%decls%rule_value(r); v=self%restore_e(e)
      c=self%ctx%decls%rule_ctor(r)
      if (n/=resolved) c=self%restore_ctor_name(c)
      vals=self%ctx%lists%cons(v,vals); ctors=self%ctx%lists%cons(c,ctors); k=self%ctx%lists%tail(k)
    end do
    if (self%status%code/=accept) return
    vals=self%list_reverse(vals); ctors=self%list_reverse(ctors)
    old=self%ctx%decls%by_name%get(resolved,0,0,0)
    if (old==0) return
    if (self%ctx%decls%tag(old)/=drecursor) return
    ok=self%ctx%decls%num_params(old)==self%ctx%decls%num_params(new) .and. &
      self%ctx%decls%num_indices(old)==self%ctx%decls%num_indices(new) .and. &
      self%ctx%decls%num_motives(old)==self%ctx%decls%num_motives(new) .and. &
      self%ctx%decls%num_minors(old)==self%ctx%decls%num_minors(new) .and. &
      self%ctx%decls%is_k(old)==self%ctx%decls%is_k(new)
    a=self%ctx%decls%all_names(old); b=self%ist%base_names; ok=ok .and. self%list_set_equal(a,b)
    if (.not.ok) then
      call self%status%fail(reject,'restored recursor metadata mismatch'); return
    end if
    e=self%ctx%decls%ty(old); call self%assert_def_eq(e,ty)
    if (self%status%code/=accept) return
    ro=self%ctx%decls%rules(old)
    if (self%ctx%lists%length(ro)/=self%ctx%lists%length(vals)) then
      call self%status%fail(reject,'restored recursor rule count mismatch'); return
    end if
    do while(ro/=list_nil .and. self%status%code==accept)
      x=self%ctx%lists%head(ro); c=self%ctx%lists%head(ctors)
      if (self%ctx%decls%rule_ctor(x)/=c) then
        call self%status%fail(reject,'restored recursor constructor mismatch'); return
      end if
      e=self%ctx%decls%rule_value(x); v=self%ctx%lists%head(vals); call self%assert_def_eq(e,v)
      ro=self%ctx%lists%tail(ro); vals=self%ctx%lists%tail(vals); ctors=self%ctx%lists%tail(ctors)
    end do
  end subroutine
  subroutine restore_and_check(self,recs)
    class(checker), intent(inout) :: self
    integer, intent(in) :: recs
    integer :: i,n,old,new,a,b,k,c,e
    logical :: ok
    do i=1,self%ist%base_count
      n=self%ist%name(i); old=self%ctx%decls%by_name%get(n,0,0,0); new=self%ctx%decls%temp_names%get(n,0,0,0)
      if (old==0 .or. new==0) then
        call self%status%fail(reject,'missing reconstructed inductive'); return
      end if
      ok=self%ctx%decls%num_params(old)==self%ctx%decls%num_params(new) .and. &
        self%ctx%decls%num_indices(old)==self%ctx%decls%num_indices(new) .and. &
        self%ctx%decls%is_nested(old)==self%ctx%decls%is_nested(new)
      a=self%ctx%decls%ctor_names(old); b=self%ctx%decls%ctor_names(new); ok=ok .and. self%list_set_equal(a,b)
      k=self%ctx%decls%all_names(old); b=self%ctx%decls%all_names(new)
      do while(k/=list_nil)
        a=self%ctx%lists%head(k); ok=ok .and. self%list_contains(b,a); k=self%ctx%lists%tail(k)
      end do
      if (.not.ok) then
        call self%status%fail(reject,'nested inductive metadata mismatch'); return
      end if
      a=self%ctx%decls%ty(old); b=self%ctx%decls%ty(new); call self%assert_def_eq(a,b)
      if (self%status%code/=accept) return
      k=self%ist%ctors(i)
      do while(k/=list_nil)
        c=self%ctx%lists%head(k); n=self%ist%ctor_name(c)
        old=self%ctx%decls%by_name%get(n,0,0,0); new=self%ctx%decls%temp_names%get(n,0,0,0)
        if (old==0 .or. new==0) then
          call self%status%fail(reject,'missing reconstructed constructor'); return
        end if
        ok=self%ctx%decls%inductive_name(old)==self%ctx%decls%inductive_name(new) .and. &
          self%ctx%decls%num_params(old)==self%ctx%decls%num_params(new) .and. &
          self%ctx%decls%ctor_idx(old)==self%ctx%decls%ctor_idx(new) .and. &
          self%ctx%decls%num_fields(old)==self%ctx%decls%num_fields(new)
        if (.not.ok) then
          call self%status%fail(reject,'restored constructor metadata mismatch'); return
        end if
        e=self%ctx%decls%ty(new); b=self%restore_e(e); a=self%ctx%decls%ty(old); call self%assert_def_eq(a,b)
        if (self%status%code/=accept) return
        k=self%ctx%lists%tail(k)
      end do
    end do
    k=recs
    do while(k/=list_nil .and. self%status%code==accept)
      new=self%ctx%lists%head(k); call self%check_restored_recursor(new); k=self%ctx%lists%tail(k)
    end do
  end subroutine
