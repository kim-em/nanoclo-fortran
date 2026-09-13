! Validation phases from nanoclo inductive.rs.
  subroutine check_declar_info(self,id)
    class(checker), intent(inout) :: self
    integer, intent(in) :: id
    integer :: k,j,l,e,ty,sort
    if (self%status%code/=accept) return
    k=self%ctx%decls%uparams(id)
    do while(k/=list_nil)
      l=self%ctx%lists%head(k)
      if (self%ctx%levels%tag(l)/=lparam) call self%status%fail(reject,'non-parameter declaration universe')
      j=self%ctx%lists%tail(k)
      do while(j/=list_nil)
        if (self%ctx%lists%head(j)==l) call self%status%fail(reject,'duplicate universe parameter')
        j=self%ctx%lists%tail(j)
      end do
      k=self%ctx%lists%tail(k)
    end do
    e=self%ctx%decls%ty(id)
    if (self%ctx%exprs%get_flags(e)/=0) call self%status%fail(reject,'free variable in declaration type')
    ty=self%infer(e,.true.); sort=self%ensure_sort(ty)
    if (self%status%code==accept .and. self%ctx%decls%tag(id)==dtheorem) then
      if (.not.self%ctx%levels%is_zero(sort)) call self%status%fail(reject,'theorem type must be a proposition')
    end if
  end subroutine
  subroutine collect_mutuals(self,id)
    class(checker), intent(inout) :: self
    integer, intent(in) :: id
    integer :: k,n,ind,ctors,j,ctor,ci,ls,ty,ignored
    call self%ist%reset()
    self%ist%uparams=self%ctx%decls%uparams(id); self%ist%num_params=self%ctx%decls%num_params(id)
    self%ist%all_names=self%ctx%decls%all_names(id); k=self%ist%all_names
    do while(k/=list_nil)
      n=self%ctx%lists%head(k); ind=self%ctx%decls%lookup(n,self%cutoff)
      if (ind==0) then
        call self%status%fail(reject,'mutual type not found'); return
      end if
      if (self%ctx%decls%tag(ind)/=dinductive) then
        call self%status%fail(reject,'mutual member is not inductive'); return
      end if
      ty=self%ctx%decls%ty(ind); ctors=self%ctx%decls%ctor_names(ind); ls=list_nil; j=ctors
      do while(j/=list_nil)
        n=self%ctx%lists%head(j); ctor=self%ctx%decls%lookup(n,self%cutoff)
        if (ctor==0) then
          call self%status%fail(reject,'constructor not found'); return
        end if
        if (self%ctx%decls%tag(ctor)/=dconstructor) then
          call self%status%fail(reject,'constructor list contains non-constructor'); return
        end if
        ci=self%ist%add_ctor(n,self%ctx%decls%ty(ctor)); ls=self%ctx%lists%cons(ci,ls)
        j=self%ctx%lists%tail(j)
      end do
      ls=self%list_reverse(ls); n=self%ctx%decls%name(ind); ignored=self%ist%add_type(n,ty,ls)
      k=self%ctx%lists%tail(k)
    end do
    if (self%ist%count==0) call self%status%fail(reject,'empty mutual type list')
  end subroutine
  subroutine check_inductive_initial(self,id)
    class(checker), intent(inout) :: self
    integer, intent(in) :: id
    integer :: names,ctors,k,n,c,e,b,pfx,ns,ind
    logical :: rec,found
    names=self%ctx%decls%all_names(id); ctors=self%ctx%decls%ctor_names(id); rec=.false.; k=ctors
    do while(k/=list_nil)
      n=self%ctx%lists%head(k); c=self%ctx%decls%by_name%get(n,0,0,0)
      if (c==0) then
        call self%status%fail(reject,'missing inductive constructor'); return
      end if
      if (self%ctx%decls%tag(c)/=dconstructor) then
        call self%status%fail(reject,'expected constructor'); return
      end if
      e=self%ctx%decls%ty(c)
      do while(self%ctx%exprs%get_tag(e)==epi)
        b=self%ctx%exprs%get_c(e); found=self%find_const(b,names,.false.)
        if (found) then
          rec=.true.; exit
        end if
        e=self%ctx%exprs%get_d(e)
      end do
      if (rec) exit
      k=self%ctx%lists%tail(k)
    end do
    if (rec .neqv. (self%ctx%decls%is_recursive(id)/=0)) then
      call self%status%fail(reject,'incorrect inductive recursion flag'); return
    end if
    pfx=self%name_id('_nested'); ns=self%ctx%lists%cons(pfx,list_nil)
    e=self%ctx%decls%ty(id); found=self%find_const(e,ns,.true.)
    if (found) call self%status%fail(reject,'exported nested prefix')
    k=names
    do while(k/=list_nil)
      n=self%ctx%lists%head(k); ind=self%ctx%decls%by_name%get(n,0,0,0)
      if (ind==0) then
        call self%status%fail(reject,'missing mutual inductive'); return
      end if
      if (self%ctx%decls%tag(ind)/=dinductive) then
        call self%status%fail(reject,'invalid mutual inductive'); return
      end if
      e=self%ctx%decls%ty(ind); found=self%find_const(e,ns,.true.)
      if (found) call self%status%fail(reject,'exported nested prefix')
      k=self%ctx%lists%tail(k)
    end do
    k=ctors
    do while(k/=list_nil)
      n=self%ctx%lists%head(k); c=self%ctx%decls%by_name%get(n,0,0,0); e=self%ctx%decls%ty(c)
      found=self%find_const(e,ns,.true.)
      if (found) call self%status%fail(reject,'exported nested prefix')
      k=self%ctx%lists%tail(k)
    end do
  end subroutine
  subroutine check_inductive_specs(self)
    class(checker), intent(inout) :: self
    integer :: i,e,k,indices,n,style,ty,body,local,l,ity
    do i=1,self%ist%count
      e=self%ist%ty(i); e=self%whnf(e); k=self%ist%local_params; indices=list_nil
      do while(self%status%code==accept)
        if (self%ctx%exprs%get_tag(e)/=epi) exit
        n=self%ctx%exprs%get_a(e); style=self%ctx%exprs%get_b(e); ty=self%ctx%exprs%get_c(e); body=self%ctx%exprs%get_d(e)
        if (k/=list_nil) then
          local=self%ctx%lists%head(k); k=self%ctx%lists%tail(k)
          if (i==1) then
            ity=self%ctx%exprs%get_c(local); call self%assert_def_eq(ty,ity)
          end if
        else
          local=self%mk_unique(n,style,ty); indices=self%ctx%lists%cons(local,indices)
        end if
        if (self%status%code/=accept) return
        e=self%ctx%exprs%inst(body,[local]); e=self%whnf(e)
      end do
      l=self%ensure_sort(e)
      if (self%status%code/=accept) return
      if (i==1) then
        self%ist%block_codom=l; self%ist%is_nonzero=self%ctx%levels%is_nonzero(l)
        self%ist%is_zero=self%ctx%levels%is_zero(l)
      else if (.not.self%ctx%levels%eq_antisymm(l,self%ist%block_codom)) then
        call self%status%fail(reject,'mutual inductive universe mismatch'); return
      end if
      self%ist%indices(i)=self%list_reverse(indices)
      n=self%ist%name(i); l=self%ist%uparams; self%ist%ind_const(i)=self%ctx%exprs%const(n,l)
    end do
  end subroutine
  logical function is_valid_ind_app(self,idx,e) result(ok)
    class(checker), intent(inout) :: self
    integer, intent(in) :: idx,e
    integer :: fn,args,ls,k,p,a,b,i,np
    ok=.false.; call self%unfold_apps(e,fn,args)
    if (self%ctx%exprs%get_tag(fn)/=econst) return
    if (self%ctx%exprs%get_a(fn)/=self%ist%name(idx)) return
    ls=self%ctx%exprs%get_b(fn)
    if (.not.self%levels_equal(ls,self%ist%uparams)) return
    np=self%ctx%lists%length(self%ist%local_params)
    if (self%ctx%lists%length(args)/=np+self%ctx%lists%length(self%ist%indices(idx))) return
    k=args
    do i=1,np
      k=self%ctx%lists%tail(k)
    end do
    do while(k/=list_nil)
      a=self%ctx%lists%head(k)
      if (self%find_const(a,self%ist%all_names,.false.)) return
      k=self%ctx%lists%tail(k)
    end do
    k=args; p=self%ist%local_params
    do while(p/=list_nil)
      a=self%ctx%lists%head(k); b=self%ctx%lists%head(p)
      if (a/=b) return
      k=self%ctx%lists%tail(k); p=self%ctx%lists%tail(p)
    end do
    ok=.true.
  end function
  integer function which_valid_ind_app(self,e) result(idx)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e
    integer :: i
    idx=0
    do i=1,self%ist%count
      if (self%is_valid_ind_app(i,e)) then
        idx=i; return
      end if
    end do
  end function
  subroutine check_positivity(self,e)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e
    integer :: cur,n,s,ty,body,local
    cur=e
    do while(self%status%code==accept)
      cur=self%whnf(cur)
      if (self%status%code/=accept) return
      if (.not.self%find_const(cur,self%ist%all_names,.false.)) return
      if (self%ctx%exprs%get_tag(cur)/=epi) then
        if (self%which_valid_ind_app(cur)==0) call self%status%fail(reject,'invalid recursive argument')
        return
      end if
      n=self%ctx%exprs%get_a(cur); s=self%ctx%exprs%get_b(cur); ty=self%ctx%exprs%get_c(cur); body=self%ctx%exprs%get_d(cur)
      if (self%find_const(ty,self%ist%all_names,.false.)) then
        call self%status%fail(reject,'non-positive inductive occurrence'); return
      end if
      local=self%mk_unique(n,s,ty); cur=self%ctx%exprs%inst(body,[local])
    end do
  end subroutine
  subroutine check_ctor(self,idx,e)
    class(checker), intent(inout) :: self
    integer, intent(in) :: idx,e
    integer :: k,cur,local,ty,body,l,n,s,localty
    cur=e; k=self%ist%local_params
    do while(k/=list_nil .and. self%status%code==accept)
      local=self%ctx%lists%head(k)
      if (self%ctx%exprs%get_tag(cur)/=epi) then
        call self%status%fail(reject,'constructor parameter missing'); return
      end if
      ty=self%ctx%exprs%get_c(cur); body=self%ctx%exprs%get_d(cur); localty=self%ctx%exprs%get_c(local)
      call self%assert_def_eq(ty,localty)
      if (self%status%code/=accept) return
      cur=self%ctx%exprs%inst(body,[local]); k=self%ctx%lists%tail(k)
    end do
    do while(self%status%code==accept)
      if (self%ctx%exprs%get_tag(cur)/=epi) exit
      n=self%ctx%exprs%get_a(cur); s=self%ctx%exprs%get_b(cur); ty=self%ctx%exprs%get_c(cur); body=self%ctx%exprs%get_d(cur)
      l=self%ensure_infers_sort(ty)
      if (self%status%code/=accept) return
      if (.not.self%ist%is_zero) then
        if (.not.self%ctx%levels%leq(l,self%ist%block_codom)) then
          call self%status%fail(reject,'constructor field universe too large'); return
        end if
      end if
      call self%check_positivity(ty)
      if (self%status%code/=accept) return
      local=self%mk_unique(n,s,ty); cur=self%ctx%exprs%inst(body,[local])
    end do
    if (self%status%code/=accept) return
    if (.not.self%is_valid_ind_app(idx,cur)) call self%status%fail(reject,'invalid constructor result')
  end subroutine
