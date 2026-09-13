! Inductive/Quot value reduction translated from nanoclo nbe_eval.rs.
! Work frames replace temporary Rust vectors; callers restore work_top.
  integer function spine_args(self,s) result(base)
    class(checker), intent(inout) :: self
    integer, intent(in) :: s
    integer :: node,n,i
    base=self%work_top; n=self%nb%spines%len(s)
    if (n==0) return
    call grow(self%w1,base+n); call grow(self%w2,base+n); call grow(self%w3,base+n)
    node=s
    do i=n,1,-1
      if (self%nb%spines%tag(node)/=elim_app) then
        base=-1; return
      end if
      self%w1(base+i)=self%nb%spines%a(node); node=self%nb%spines%parent(node)
    end do
    self%work_top=base+n
  end function
  integer function as_inductive(self,v) result(h)
    class(checker), intent(in) :: self
    integer, intent(in) :: v
    h=0
    if (self%status%code/=accept) return
    if (self%nb%tag(v)/=vrigid) return
    h=self%nb%head(v)
    if (self%nb%head_tag(h)/=hconst .or. self%nb%head_a(h)/=cinductive) h=0
  end function
  integer function get_structure(self,n,rec_ok) result(id)
    class(checker), intent(in) :: self
    integer, intent(in) :: n
    logical, intent(in) :: rec_ok
    id=self%ctx%decls%lookup(n,self%cutoff)
    if (id==0) return
    if (self%ctx%decls%tag(id)/=dinductive) then
      id=0; return
    end if
    if (self%ctx%decls%num_indices(id)/=0 .or. &
        self%ctx%lists%length(self%ctx%decls%ctor_names(id))/=1 .or. &
        (.not.rec_ok .and. self%ctx%decls%is_recursive(id)/=0)) id=0
  end function
  integer function major_idx(self,rec) result(idx)
    class(checker), intent(in) :: self
    integer, intent(in) :: rec
    idx=self%ctx%decls%num_params(rec)+self%ctx%decls%num_motives(rec)+ &
        self%ctx%decls%num_minors(rec)+self%ctx%decls%num_indices(rec)
  end function
  integer function get_major_induct(self,rec) result(n)
    class(checker), intent(in) :: self
    integer, intent(in) :: rec
    integer :: e,i
    n=0; e=self%ctx%decls%ty(rec)
    do i=0,self%major_idx(rec)
      if (self%ctx%exprs%get_tag(e)/=epi) return
      if (i==self%major_idx(rec)) exit
      e=self%ctx%exprs%get_d(e)
    end do
    e=self%ctx%exprs%get_c(e)
    do while(self%ctx%exprs%get_tag(e)==eapp)
      e=self%ctx%exprs%get_a(e)
    end do
    if (self%ctx%exprs%get_tag(e)==econst) n=self%ctx%exprs%get_a(e)
  end function
  recursive integer function nb_iota(self,depth,v) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,v
    integer :: cached,mark
    out=0
    if (self%status%code/=accept) return
    cached=self%nb%iota_cache%get(v,0,0,0)
    if (cached/=0) then
      out=max(0,cached); return
    end if
    mark=self%work_top; out=self%nb_iota_go(depth,v); self%work_top=mark
    if (self%status%code==accept) call self%nb%iota_cache%put(v,0,0,0,merge(out,-1,out/=0))
  end function
  recursive integer function nb_iota_go(self,depth,v) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,v
    integer :: h,n,ls,s,args,nargs,rec,pos,major,kctor
    out=0
    if (self%nb%tag(v)/=vrigid) return
    h=self%nb%head(v)
    if (self%nb%head_tag(h)/=hconst) return
    n=self%nb%head_b(h); ls=self%nb%head_c(h); s=self%nb%spine(v)
    args=self%spine_args(s)
    if (args<0) return
    nargs=self%nb%spines%len(s)
    select case(self%nb%head_a(h))
    case(crecursor)
      rec=self%ctx%decls%lookup(n,self%cutoff)
      if (rec==0) return
      if (self%ctx%decls%tag(rec)/=drecursor) return
      if (self%ctx%lists%length(self%ctx%decls%uparams(rec))/=self%ctx%lists%length(ls)) return
      pos=self%major_idx(rec)
      if (nargs<=pos) return
      major=self%w1(args+pos+1)
      if (self%ctx%decls%is_k(rec)/=0) then
        major=self%nb_force(depth,major); kctor=self%nb_k_reduce(depth,major,rec)
        if (self%status%code/=accept) return
        if (kctor/=0) then
          out=self%nb_fire_recursor(depth,rec,ls,args,nargs,kctor)
          if (out/=0 .or. self%status%code/=accept) return
        end if
      end if
      major=self%w1(args+pos+1); major=self%nb_whnf(depth,major)
      if (self%status%code/=accept) return
      out=self%nb_fire_recursor(depth,rec,ls,args,nargs,major)
    case(cquot)
      pos=-1
      if (n==self%quot_lift) pos=5
      if (n==self%quot_ind) pos=4
      if (pos<0 .or. nargs<=pos) return
      major=self%w1(args+pos+1); major=self%nb_whnf(depth,major)
      if (self%status%code/=accept) return
      out=self%nb_fire_quot(depth,n,args,nargs,major)
    end select
  end function
  recursive integer function nb_fire_quot(self,depth,n,args,nargs,major) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,n,args,nargs,major
    integer :: h,s,margs,rest,i,a
    out=0
    if (self%nb%tag(major)/=vrigid) return
    h=self%nb%head(major)
    if (self%nb%head_tag(h)/=hconst .or. self%nb%head_a(h)/=cquot) return
    if (self%nb%head_b(h)/=self%quot_mk) return
    s=self%nb%spine(major); margs=self%spine_args(s)
    if (margs<0 .or. self%nb%spines%len(s)/=3) return
    rest=merge(6,5,n==self%quot_lift)
    out=self%w1(args+4); a=self%w1(margs+3); out=self%nb_apply(depth,out,a)
    do i=rest+1,nargs
      a=self%w1(args+i); out=self%nb_apply(depth,out,a)
    end do
  end function
  recursive integer function nb_fire_recursor(self,depth,rec,ls,args,nargs,major) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,rec,ls,args,nargs,major
    integer :: cur,h,ctor,s,cargs,nc,k,rule,nfields,body,params,nprefix,i,a,next
    out=0; cur=major
    k=self%ctx%decls%all_names(rec)
    if (self%nat_extension .and. self%ctx%lists%head(k)==self%nat_name .and. self%nb%tag(cur)==vnat) then
      a=self%nb%literal(cur); out=self%nb_nat_rec(depth,rec,ls,args,nargs,a); return
    end if
    next=0
    if (self%nb%tag(cur)==vnat) then
      a=self%nb%literal(cur); next=self%nb_nat_to_ctor(a)
    else if (self%nb%tag(cur)==vstr) then
      a=self%nb%literal(cur); next=self%nb_str_to_ctor(depth,a)
    end if
    if (next==0 .and. self%status%code==accept) next=self%nb_k_reduce(depth,cur,rec)
    if (self%status%code/=accept) return
    if (next==0) next=self%nb_struct_eta_reduce(depth,cur,rec)
    if (self%status%code/=accept) return
    if (next/=0) cur=next
    if (self%nb%tag(cur)/=vrigid) return
    h=self%nb%head(cur)
    if (self%nb%head_tag(h)/=hconst .or. self%nb%head_a(h)/=cctor) return
    ctor=self%nb%head_b(h); s=self%nb%spine(cur); cargs=self%spine_args(s)
    if (cargs<0) return
    nc=self%nb%spines%len(s); k=self%ctx%decls%rules(rec); rule=0
    do while(k/=list_nil)
      i=self%ctx%lists%head(k)
      if (self%ctx%decls%rule_ctor(i)==ctor) then
        rule=i; exit
      end if
      k=self%ctx%lists%tail(k)
    end do
    if (rule==0) return
    nfields=self%ctx%decls%rule_fields(rule)
    if (nc<nfields) return
    body=self%ctx%decls%rule_value(rule); out=self%nb%rec_rule_cache%get(body,ls,0,0)
    if (out==0) then
      params=self%ctx%decls%uparams(rec)
      i=self%ctx%exprs%subst_expr_levels(self%ctx%levels,self%ctx%lists,body,params,ls,self%status)
      out=self%nb_eval(0,env_nil,i)
      if (self%status%code/=accept) return
      call self%nb%rec_rule_cache%put(body,ls,0,0,out)
    end if
    nprefix=self%ctx%decls%num_params(rec)+self%ctx%decls%num_motives(rec)+self%ctx%decls%num_minors(rec)
    do i=1,nprefix
      a=self%w1(args+i); out=self%nb_apply(depth,out,a)
    end do
    do i=nc-nfields+1,nc
      a=self%w1(cargs+i); out=self%nb_apply(depth,out,a)
    end do
    do i=self%major_idx(rec)+2,nargs
      a=self%w1(args+i); out=self%nb_apply(depth,out,a)
    end do
  end function
  recursive integer function nb_k_reduce(self,depth,major,rec) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,major,rec
    integer :: mark,ty,h,n,ls,args,s,ind,ctor,k,rule,nf,take,i,a,new_ty
    logical :: ok
    out=0
    if (self%ctx%decls%is_k(rec)==0 .or. self%status%code/=accept) return
    if (self%nb%tag(major)/=vrigid .and. self%nb%tag(major)/=vunfold) return
    mark=self%work_top
    attempt: block
      ty=self%nb_type(depth,major); ty=self%nb_whnf(depth,ty); h=self%as_inductive(ty)
      if (h==0) exit attempt
      n=self%nb%head_b(h); ls=self%nb%head_c(h); s=self%nb%spine(ty); args=self%spine_args(s)
      if (args<0 .or. n/=self%get_major_induct(rec)) exit attempt
      ind=self%ctx%decls%lookup(n,self%cutoff)
      if (ind==0) exit attempt
      if (self%ctx%decls%tag(ind)/=dinductive) exit attempt
      k=self%ctx%decls%ctor_names(ind)
      if (k==list_nil) exit attempt
      ctor=self%ctx%lists%head(k); nf=0; k=self%ctx%decls%rules(rec)
      do while(k/=list_nil)
        rule=self%ctx%lists%head(k)
        if (self%ctx%decls%rule_ctor(rule)==ctor) then
          nf=self%ctx%decls%rule_fields(rule); exit
        end if
        k=self%ctx%lists%tail(k)
      end do
      take=min(self%ctx%decls%num_params(rec)+nf,self%nb%spines%len(s))
      h=self%nb%mk_head(hconst,cctor,ctor,ls); i=self%nb%mk_rigid(h,spine_empty)
      do k=1,take
        a=self%w1(args+k); i=self%nb_apply(depth,i,a)
      end do
      new_ty=self%nb_type(depth,i); ok=self%nb_conv(depth,ty,new_ty)
      if (ok .and. self%status%code==accept) out=i
    end block attempt
    self%work_top=mark
  end function
  recursive integer function nb_struct_eta_reduce(self,depth,major,rec) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,major,rec
    integer :: n,id,cached,mark
    out=0
    if (self%status%code/=accept) return
    if (self%nb%tag(major)/=vrigid .and. self%nb%tag(major)/=vunfold) return
    n=self%get_major_induct(rec)
    if (n==0) return
    id=self%get_structure(n,.false.)
    if (id==0) return
    cached=self%nb%struct_eta_cache%get(major,n,0,0)
    if (cached/=0) then
      out=max(0,cached); return
    end if
    mark=self%work_top
    out=self%nb_struct_eta_go(depth,major,n,self%ctx%decls%num_params(rec))
    self%work_top=mark
    if (self%status%code==accept) call self%nb%struct_eta_cache%put(major,n,0,0,merge(out,-1,out/=0))
  end function
  recursive integer function nb_struct_eta_go(self,depth,major,n,np) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,major,n,np
    integer :: ty,h,ls,args,s,l,ind,ctor,id,nf,i,a
    out=0; ty=self%nb_type(depth,major); ty=self%nb_whnf(depth,ty); h=self%as_inductive(ty)
    if (h==0) return
    if (self%nb%head_b(h)/=n) return
    ls=self%nb%head_c(h); s=self%nb%spine(ty); args=self%spine_args(s)
    if (args<0) return
    l=self%nb_type_level(depth,ty)
    if (self%status%code/=accept) return
    if (l/=0) then
      if (.not.self%ctx%levels%is_never_zero(l)) return
    end if
    ind=self%ctx%decls%lookup(n,self%cutoff)
    if (ind==0) return
    ctor=self%ctx%lists%head(self%ctx%decls%ctor_names(ind)); id=self%ctx%decls%lookup(ctor,self%cutoff)
    if (id==0) return
    if (self%ctx%decls%tag(id)/=dconstructor) return
    nf=self%ctx%decls%num_fields(id); h=self%nb%mk_head(hconst,cctor,ctor,ls); out=self%nb%mk_rigid(h,spine_empty)
    do i=1,min(np,self%nb%spines%len(s))
      a=self%w1(args+i); out=self%nb_apply(depth,out,a)
    end do
    do i=0,nf-1
      a=self%nb_proj(depth,n,i,major); out=self%nb_apply(depth,out,a)
    end do
  end function
  recursive integer function nb_field_type(self,depth,value,ty,n,idx) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,value,ty,n,idx
    integer :: mark,t,h,ls,s,args,ind,np,ctor,id,cty,params,cur,cf,i,a,dom
    out=0; mark=self%work_top
    attempt: block
      t=self%nb_whnf(depth,ty); h=self%as_inductive(t)
      if (h==0) exit attempt
      if (self%nb%head_b(h)/=n) exit attempt
      ls=self%nb%head_c(h); s=self%nb%spine(t); args=self%spine_args(s)
      if (args<0) exit attempt
      ind=self%get_structure(n,.true.)
      if (ind==0) exit attempt
      np=self%ctx%decls%num_params(ind); ctor=self%ctx%lists%head(self%ctx%decls%ctor_names(ind))
      id=self%ctx%decls%lookup(ctor,self%cutoff)
      if (id==0) exit attempt
      cty=self%ctx%decls%ty(id); params=self%ctx%decls%uparams(id)
      cty=self%ctx%exprs%subst_expr_levels(self%ctx%levels,self%ctx%lists,cty,params,ls,self%status)
      cur=self%nb_eval(depth,env_nil,cty)
      do i=1,np
        cf=self%nb_whnf(depth,cur)
        if (self%status%code/=accept) exit attempt
        if (self%nb%tag(cf)/=vpi .or. i>self%nb%spines%len(s)) exit attempt
        a=self%w1(args+i); cur=self%nb_open(depth,cf,a)
      end do
      do i=0,idx-1
        cf=self%nb_whnf(depth,cur)
        if (self%status%code/=accept) exit attempt
        if (self%nb%tag(cf)/=vpi) exit attempt
        a=self%nb_proj(depth,n,i,value); cur=self%nb_open(depth,cf,a)
      end do
      cf=self%nb_whnf(depth,cur)
      if (self%status%code/=accept) exit attempt
      if (self%nb%tag(cf)/=vpi) exit attempt
      dom=self%nb%domain(cf); out=self%nb_force(depth,dom)
    end block attempt
    self%work_top=mark
  end function
