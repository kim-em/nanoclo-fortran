! Recursor construction from nanoclo inductive.rs, using interned list IDs.
  logical function large_elim_test(self) result(ok)
    class(checker), intent(inout) :: self
    integer :: k,c,cur,remaining,n,s,ty,body,local,l,nonprop,fn,args
    ok=.true.
    if (self%ist%is_nonzero) return
    ok=.false.
    if (self%ist%count/=1) return
    k=self%ist%ctors(1)
    if (k==list_nil) then
      ok=.true.; return
    end if
    if (self%ctx%lists%length(k)/=1) return
    c=self%ctx%lists%head(k); cur=self%ist%ctor_ty(c); remaining=self%ist%num_params; nonprop=list_nil
    do while(self%status%code==accept)
      if (self%ctx%exprs%get_tag(cur)/=epi) exit
      n=self%ctx%exprs%get_a(cur); s=self%ctx%exprs%get_b(cur); ty=self%ctx%exprs%get_c(cur); body=self%ctx%exprs%get_d(cur)
      local=self%mk_unique(n,s,ty); cur=self%ctx%exprs%inst(body,[local])
      if (remaining>0) then
        remaining=remaining-1
      else
        l=self%ensure_infers_sort(ty)
        if (self%status%code/=accept) return
        if (.not.self%ctx%levels%is_zero(l)) nonprop=self%ctx%lists%cons(local,nonprop)
      end if
    end do
    if (self%status%code/=accept) return
    call self%unfold_apps(cur,fn,args); k=nonprop
    do while(k/=list_nil)
      if (.not.self%list_contains(args,self%ctx%lists%head(k))) return
      k=self%ctx%lists%tail(k)
    end do
    ok=.true.
  end function
  subroutine mk_elim_level(self)
    class(checker), intent(inout) :: self
    integer :: n,base,l,i
    logical :: large
    large=self%large_elim_test()
    if (self%status%code/=accept) return
    self%ist%rec_uparams=self%ist%uparams; self%ist%elim_level=1
    if (.not.large) return
    base=self%name_id('u'); n=base; i=0
    do
      l=self%ctx%levels%param(n)
      if (.not.self%list_contains(self%ist%uparams,l)) exit
      i=i+1; n=self%index_name(base,i)
    end do
    self%ist%elim_level=l; self%ist%rec_uparams=self%ctx%lists%cons(l,self%ist%uparams)
  end subroutine
  subroutine init_k_target(self)
    class(checker), intent(inout) :: self
    integer :: k,c,e
    self%ist%k_target=.false.
    if (.not.self%ist%is_zero .or. self%ist%count/=1) return
    k=self%ist%ctors(1)
    if (self%ctx%lists%length(k)/=1) return
    c=self%ctx%lists%head(k); e=self%ist%ctor_ty(c)
    self%ist%k_target=self%pi_telescope_size(e)==self%ist%num_params
  end subroutine
  subroutine mk_majors_motives(self)
    class(checker), intent(inout) :: self
    integer :: i,e,ls,n,base,major
    do i=1,self%ist%count
      e=self%ist%ind_const(i); ls=self%ist%local_params; e=self%fold_apps(e,ls)
      ls=self%ist%indices(i); e=self%fold_apps(e,ls); n=self%name_id('t')
      self%ist%major(i)=self%mk_unique(n,0,e)
    end do
    do i=1,self%ist%count
      e=self%ctx%exprs%sort(self%ist%elim_level); major=self%ist%major(i)
      e=self%abstr_binder(epi,major,e); ls=self%ist%indices(i); e=self%abstr_binders(epi,ls,e)
      base=self%name_id('motive'); n=base
      if (self%ist%count>1) n=self%index_name(base,i)
      self%ist%motive(i)=self%mk_unique(n,1,e)
    end do
  end subroutine
  recursive integer function is_rec_argument(self,e) result(idx)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e
    integer :: cur,n,s,ty,body,local
    idx=0; cur=self%whnf(e)
    if (self%status%code/=accept) return
    if (self%ctx%exprs%get_tag(cur)==epi) then
      n=self%ctx%exprs%get_a(cur); s=self%ctx%exprs%get_b(cur); ty=self%ctx%exprs%get_c(cur); body=self%ctx%exprs%get_d(cur)
      local=self%mk_unique(n,s,ty); cur=self%ctx%exprs%inst(body,[local]); idx=self%is_rec_argument(cur)
    else
      idx=self%which_valid_ind_app(cur)
    end if
  end function
  subroutine sep_ctor_args(self,e,tail,all_args,rec_args)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e
    integer, intent(out) :: tail,all_args,rec_args
    integer :: k,local,n,s,ty,body,idx
    tail=e; all_args=list_nil; rec_args=list_nil; k=self%ist%local_params
    do while(k/=list_nil .and. self%status%code==accept)
      if (self%ctx%exprs%get_tag(tail)/=epi) then
        call self%status%fail(reject,'missing constructor parameter'); return
      end if
      local=self%ctx%lists%head(k); body=self%ctx%exprs%get_d(tail)
      tail=self%ctx%exprs%inst(body,[local]); k=self%ctx%lists%tail(k)
    end do
    do while(self%status%code==accept)
      if (self%ctx%exprs%get_tag(tail)/=epi) exit
      n=self%ctx%exprs%get_a(tail); s=self%ctx%exprs%get_b(tail); ty=self%ctx%exprs%get_c(tail); body=self%ctx%exprs%get_d(tail)
      local=self%mk_unique(n,s,ty); tail=self%ctx%exprs%inst(body,[local]); all_args=self%ctx%lists%cons(local,all_args)
      idx=self%is_rec_argument(ty)
      if (idx/=0) rec_args=self%ctx%lists%cons(local,rec_args)
    end do
    all_args=self%list_reverse(all_args); rec_args=self%list_reverse(rec_args)
  end subroutine
  subroutine handle_rec_args_aux(self,e,tail,xs)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e
    integer, intent(out) :: tail,xs
    integer :: n,s,ty,body,local
    tail=e; xs=list_nil
    do while(self%status%code==accept)
      if (self%ctx%exprs%get_tag(tail)/=epi) exit
      n=self%ctx%exprs%get_a(tail); s=self%ctx%exprs%get_b(tail); ty=self%ctx%exprs%get_c(tail); body=self%ctx%exprs%get_d(tail)
      local=self%mk_unique(n,s,ty); tail=self%ctx%exprs%inst(body,[local]); tail=self%whnf(tail)
      xs=self%ctx%lists%cons(local,xs)
    end do
    xs=self%list_reverse(xs)
  end subroutine
  subroutine get_i_indices(self,e,idx,args)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e
    integer, intent(out) :: idx,args
    integer :: fn,i
    args=list_nil; idx=self%which_valid_ind_app(e)
    if (idx==0) then
      call self%status%fail(reject,'invalid recursive result type'); return
    end if
    call self%unfold_apps(e,fn,args)
    do i=1,self%ist%num_params
      args=self%ctx%lists%tail(args)
    end do
    ! Rust returns these reversed as a stack, then every caller reverses
    ! them before use. Our cons list is already in application order.
  end subroutine
  integer function handle_rec_args_minor(self,ctor_idx,rec_args) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: ctor_idx,rec_args
    integer :: k,rec_arg,ty,tail,xs,idx,args,motive,lhs,rhs,vty,n,local,i
    out=list_nil; k=rec_args; i=0
    do while(k/=list_nil .and. self%status%code==accept)
      rec_arg=self%ctx%lists%head(k); ty=self%infer(rec_arg,.false.); ty=self%whnf(ty)
      call self%handle_rec_args_aux(ty,tail,xs)
      if (self%status%code/=accept) return
      call self%get_i_indices(tail,idx,args)
      if (self%status%code/=accept) return
      motive=self%ist%motive(idx); lhs=self%fold_apps(motive,args); rhs=self%fold_apps(rec_arg,xs)
      lhs=self%ctx%exprs%app(lhs,rhs); vty=self%abstr_binders(epi,xs,lhs)
      n=self%name_id('v'); n=self%index_name(n,ctor_idx); n=self%index_name(n,i)
      local=self%mk_unique(n,0,vty); out=self%ctx%lists%cons(local,out)
      k=self%ctx%lists%tail(k); i=i+1
    end do
    out=self%list_reverse(out)
  end function
  subroutine mk_minors(self)
    class(checker), intent(inout) :: self
    integer :: i,k,c,ctor_idx,e,tail,all_args,rec_args,idx,args,motive,capp,minor_ty,n,vs,minor,out,ls
    do i=1,self%ist%count
      k=self%ist%ctors(i); ctor_idx=0; out=list_nil
      do while(k/=list_nil .and. self%status%code==accept)
        c=self%ctx%lists%head(k); e=self%ist%ctor_ty(c)
        call self%sep_ctor_args(e,tail,all_args,rec_args)
        if (self%status%code/=accept) return
        call self%get_i_indices(tail,idx,args)
        if (self%status%code/=accept) return
        motive=self%ist%motive(idx); n=self%ist%ctor_name(c); ls=self%ist%uparams
        capp=self%ctx%exprs%const(n,ls); ls=self%ist%local_params; capp=self%fold_apps(capp,ls)
        capp=self%fold_apps(capp,all_args); e=self%fold_apps(motive,args); e=self%ctx%exprs%app(e,capp)
        vs=self%handle_rec_args_minor(ctor_idx,rec_args)
        minor_ty=self%abstr_binders(epi,vs,e); minor_ty=self%abstr_binders(epi,all_args,minor_ty)
        if (self%ctx%names%tag(n)==name_str) then
          ls=self%ctx%names%b(n); n=self%ctx%names%str(anonymous,ls)
        else
          n=self%name_id('m'); n=self%index_name(n,ctor_idx)
        end if
        if (self%status%code/=accept) return
        minor=self%mk_unique(n,0,minor_ty); out=self%ctx%lists%cons(minor,out)
        k=self%ctx%lists%tail(k); ctor_idx=ctor_idx+1
      end do
      self%ist%minors(i)=self%list_reverse(out)
    end do
  end subroutine
  integer function all_motives(self) result(out)
    class(checker), intent(inout) :: self
    integer :: i,m
    out=list_nil
    do i=self%ist%count,1,-1
      m=self%ist%motive(i); out=self%ctx%lists%cons(m,out)
    end do
  end function
  integer function all_minors(self) result(out)
    class(checker), intent(inout) :: self
    integer :: i,k,m
    out=list_nil
    do i=1,self%ist%count
      k=self%ist%minors(i)
      do while(k/=list_nil)
        m=self%ctx%lists%head(k); out=self%ctx%lists%cons(m,out); k=self%ctx%lists%tail(k)
      end do
    end do
    out=self%list_reverse(out)
  end function
  integer function handle_rec_rule_args(self,rec_args,minors,motives) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: rec_args,minors,motives
    integer :: k,arg,ty,tail,xs,idx,args,n,s,e,rhs,ls
    out=list_nil; k=rec_args; s=self%ctx%strings%intern('rec')
    do while(k/=list_nil .and. self%status%code==accept)
      arg=self%ctx%lists%head(k); ty=self%infer(arg,.false.); ty=self%whnf(ty)
      call self%handle_rec_args_aux(ty,tail,xs)
      if (self%status%code/=accept) return
      call self%get_i_indices(tail,idx,args)
      if (self%status%code/=accept) return
      n=self%ist%name(idx); n=self%ctx%names%str(n,s); ls=self%ist%rec_uparams; e=self%ctx%exprs%const(n,ls)
      ls=self%ist%local_params; e=self%fold_apps(e,ls); e=self%fold_apps(e,motives); e=self%fold_apps(e,minors)
      e=self%fold_apps(e,args); rhs=self%fold_apps(arg,xs); e=self%ctx%exprs%app(e,rhs)
      e=self%abstr_binders(elam,xs,e); out=self%ctx%lists%cons(e,out); k=self%ctx%lists%tail(k)
    end do
    out=self%list_reverse(out)
  end function
  integer function mk_rec_rule(self,c,minors,motives,minor) result(rule)
    class(checker), intent(inout) :: self
    integer, intent(in) :: c,minors,motives,minor
    integer :: e,tail,args,rec_args,handled,ls,n,nf
    rule=0; e=self%ist%ctor_ty(c); call self%sep_ctor_args(e,tail,args,rec_args)
    if (self%status%code/=accept) return
    handled=self%handle_rec_rule_args(rec_args,minors,motives)
    e=self%fold_apps(minor,args); e=self%fold_apps(e,handled); e=self%abstr_binders(elam,args,e)
    e=self%abstr_binders(elam,minors,e); e=self%abstr_binders(elam,motives,e)
    ls=self%ist%local_params; e=self%abstr_binders(elam,ls,e); n=self%ist%ctor_name(c)
    nf=self%pi_telescope_size(self%ist%ctor_ty(c))-self%ist%num_params
    if (nf<0 .or. nf>65535) then
      call self%status%fail(reject,'constructor field count overflow'); return
    end if
    if (self%status%code==accept) rule=self%ctx%decls%add_rule(n,nf,e)
  end function
  integer function mk_recursors(self) result(recs)
    class(checker), intent(inout) :: self
    integer :: minors,motives,i,k,c,ms,minor,rule,rules,groups,e,major,ls,n,s,id
    recs=list_nil; groups=list_nil; minors=self%all_minors(); motives=self%all_motives(); ms=minors
    ! Construct all rules before constructing any recursor, preserving phase order.
    do i=1,self%ist%count
      k=self%ist%ctors(i); rules=list_nil
      do while(k/=list_nil .and. self%status%code==accept)
        c=self%ctx%lists%head(k); minor=self%ctx%lists%head(ms); rule=self%mk_rec_rule(c,minors,motives,minor)
        if (self%status%code/=accept) return
        rules=self%ctx%lists%cons(rule,rules); k=self%ctx%lists%tail(k); ms=self%ctx%lists%tail(ms)
      end do
      rules=self%list_reverse(rules); groups=self%ctx%lists%cons(rules,groups)
    end do
    groups=self%list_reverse(groups); s=self%ctx%strings%intern('rec')
    do i=1,self%ist%count
      e=self%ist%motive(i); ls=self%ist%indices(i); e=self%fold_apps(e,ls); major=self%ist%major(i)
      e=self%ctx%exprs%app(e,major); e=self%abstr_binder(epi,major,e); e=self%abstr_binders(epi,ls,e)
      e=self%abstr_binders(epi,minors,e); e=self%abstr_binders(epi,motives,e)
      ls=self%ist%local_params; e=self%abstr_binders(epi,ls,e); n=self%ist%name(i); n=self%ctx%names%str(n,s)
      ls=self%ist%rec_uparams; id=self%ctx%decls%add(drecursor,n,ls,e,0,hint_opaque,0,self%status)
      if (self%status%code/=accept) return
      self%ctx%decls%all_names(id)=self%ist%all_names; self%ctx%decls%num_params(id)=self%ist%num_params
      self%ctx%decls%num_indices(id)=self%ctx%lists%length(self%ist%indices(i))
      self%ctx%decls%num_motives(id)=self%ist%count; self%ctx%decls%num_minors(id)=self%ctx%lists%length(minors)
      self%ctx%decls%rules(id)=self%ctx%lists%head(groups); groups=self%ctx%lists%tail(groups)
      self%ctx%decls%is_k(id)=merge(1,0,self%ist%k_target); recs=self%ctx%lists%cons(id,recs)
    end do
    recs=self%list_reverse(recs)
  end function
