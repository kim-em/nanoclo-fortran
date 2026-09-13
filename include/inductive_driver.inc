  subroutine mk_ind_types_extension(self)
    class(checker), intent(inout) :: self
    integer :: i,k,c,ctors,n,ty,ls,id
    call self%ctx%decls%begin_temp()
    do i=1,self%ist%count
      k=self%ist%ctors(i); ctors=list_nil
      do while(k/=list_nil)
        c=self%ctx%lists%head(k); n=self%ist%ctor_name(c); ctors=self%ctx%lists%cons(n,ctors)
        k=self%ctx%lists%tail(k)
      end do
      ctors=self%list_reverse(ctors); n=self%ist%name(i); ty=self%ist%ty(i); ls=self%ist%uparams
      id=self%ctx%decls%add(dinductive,n,ls,ty,0,hint_opaque,0,self%status)
      if (self%status%code/=accept) return
      self%ctx%decls%all_names(id)=self%ist%all_names; self%ctx%decls%ctor_names(id)=ctors
      self%ctx%decls%num_params(id)=self%ist%num_params
      self%ctx%decls%num_indices(id)=self%ctx%lists%length(self%ist%indices(i))
      self%ctx%decls%is_nested(id)=merge(1,0,self%ist%nested)
    end do
  end subroutine
  subroutine mk_ctors_extension(self)
    class(checker), intent(inout) :: self
    integer :: i,k,c,n,ty,ls,id,idx,nf
    do i=1,self%ist%count
      k=self%ist%ctors(i); idx=0
      do while(k/=list_nil)
        c=self%ctx%lists%head(k); n=self%ist%ctor_name(c); ty=self%ist%ctor_ty(c); ls=self%ist%uparams
        nf=self%pi_telescope_size(ty)-self%ist%num_params
        if (nf<0 .or. nf>65535) then
          call self%status%fail(reject,'constructor field count overflow'); return
        end if
        id=self%ctx%decls%add(dconstructor,n,ls,ty,0,hint_opaque,0,self%status)
        if (self%status%code/=accept) return
        self%ctx%decls%inductive_name(id)=self%ist%name(i); self%ctx%decls%num_params(id)=self%ist%num_params
        self%ctx%decls%ctor_idx(id)=idx; self%ctx%decls%num_fields(id)=nf
        k=self%ctx%lists%tail(k); idx=idx+1
      end do
    end do
  end subroutine
  subroutine check_recursor_names(self,id,recs)
    class(checker), intent(inout) :: self
    integer, intent(in) :: id,recs
    integer :: k,r,derived,imported,i,n,ind
    derived=list_nil; imported=list_nil; k=recs; ind=self%ctx%decls%name(id)
    do while(k/=list_nil)
      r=self%ctx%lists%head(k); n=self%ctx%decls%name(r)
      if (self%ist%nested) n=self%restored_rec_name(n)
      derived=self%ctx%lists%cons(n,derived)
      k=self%ctx%lists%tail(k)
    end do
    do i=1,self%ctx%decls%temp_base-1
      if (self%ctx%decls%tag(i)/=drecursor) cycle
      if (.not.self%list_contains(self%ctx%decls%all_names(i),ind)) cycle
      n=self%ctx%decls%name(i); imported=self%ctx%lists%cons(n,imported)
    end do
    if (.not.self%list_set_equal(derived,imported)) call self%status%fail(reject,'incorrect derived recursor names')
  end subroutine
  subroutine compare_inductive_exports(self,recs)
    class(checker), intent(inout) :: self
    integer, intent(in) :: recs
    integer :: i,n,old,new,k,c,a,b,e,ls,ks,ro,rn,x,y,vo,vn
    logical :: ok
    do i=1,self%ist%count
      n=self%ist%name(i); old=self%ctx%decls%by_name%get(n,0,0,0); new=self%ctx%decls%temp_names%get(n,0,0,0)
      if (old==0 .or. new==0) then
        call self%status%fail(reject,'missing reconstructed inductive'); return
      end if
      ok=self%ctx%decls%num_params(old)==self%ctx%decls%num_params(new) .and. &
        self%ctx%decls%num_indices(old)==self%ctx%decls%num_indices(new) .and. &
        self%ctx%decls%is_nested(old)==self%ctx%decls%is_nested(new)
      a=self%ctx%decls%ctor_names(old); b=self%ctx%decls%ctor_names(new)
      ok=ok .and. self%list_set_equal(a,b)
      a=self%ctx%decls%all_names(old); b=self%ctx%decls%all_names(new)
      ok=ok .and. self%list_set_equal(a,b)
      if (.not.ok) then
        call self%status%fail(reject,'inductive metadata mismatch'); return
      end if
      a=self%ctx%decls%ty(old); b=self%ctx%decls%ty(new); call self%assert_def_eq(a,b)
      if (self%status%code/=accept) return
    end do
    do i=1,self%ist%count
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
          call self%status%fail(reject,'constructor metadata mismatch'); return
        end if
        a=self%ctx%decls%ty(old); b=self%ctx%decls%ty(new); call self%assert_def_eq(a,b)
        if (self%status%code/=accept) return
        k=self%ctx%lists%tail(k)
      end do
    end do
    k=recs
    do while(k/=list_nil)
      new=self%ctx%lists%head(k); n=self%ctx%decls%name(new); old=self%ctx%decls%by_name%get(n,0,0,0)
      if (old==0) then
        call self%status%fail(reject,'missing recursor'); return
      end if
      if (self%ctx%decls%tag(old)/=drecursor) then
        call self%status%fail(reject,'expected recursor'); return
      end if
      ok=self%ctx%decls%num_params(old)==self%ctx%decls%num_params(new) .and. &
        self%ctx%decls%num_indices(old)==self%ctx%decls%num_indices(new) .and. &
        self%ctx%decls%num_motives(old)==self%ctx%decls%num_motives(new) .and. &
        self%ctx%decls%num_minors(old)==self%ctx%decls%num_minors(new) .and. &
        self%ctx%decls%is_k(old)==self%ctx%decls%is_k(new)
      a=self%ctx%decls%all_names(old); b=self%ctx%decls%all_names(new); ok=ok .and. self%list_set_equal(a,b)
      if (.not.ok) then
        call self%status%fail(reject,'recursor metadata mismatch'); return
      end if
      e=self%ctx%decls%ty(old); ks=self%ctx%decls%uparams(old); ls=self%ist%rec_uparams
      e=self%ctx%exprs%subst_expr_levels(self%ctx%levels,self%ctx%lists,e,ks,ls,self%status)
      b=self%ctx%decls%ty(new); call self%assert_def_eq(e,b)
      if (self%status%code/=accept) return
      ro=self%ctx%decls%rules(old); rn=self%ctx%decls%rules(new)
      if (self%ctx%lists%length(ro)/=self%ctx%lists%length(rn)) then
        call self%status%fail(reject,'recursor rule count mismatch'); return
      end if
      do while(ro/=list_nil)
        x=self%ctx%lists%head(ro); y=self%ctx%lists%head(rn)
        if (self%ctx%decls%rule_ctor(x)/=self%ctx%decls%rule_ctor(y) .or. &
            self%ctx%decls%rule_fields(x)/=self%ctx%decls%rule_fields(y)) then
          call self%status%fail(reject,'recursor rule metadata mismatch'); return
        end if
        vo=self%ctx%decls%rule_value(x); vn=self%ctx%decls%rule_value(y)
        vn=self%ctx%exprs%subst_expr_levels(self%ctx%levels,self%ctx%lists,vn,ls,ks,self%status)
        call self%assert_def_eq(vo,vn)
        if (self%status%code/=accept) return
        ro=self%ctx%lists%tail(ro); rn=self%ctx%lists%tail(rn)
      end do
      k=self%ctx%lists%tail(k)
    end do
  end subroutine
  subroutine check_inductive(self,id)
    class(checker), intent(inout) :: self
    integer, intent(in) :: id
    integer :: i,k,c,e,recs,n,base,rs
    call self%check_inductive_initial(id)
    if (self%status%code/=accept) return
    self%cutoff=self%ctx%decls%block_end(id); self%current_decl=0
    call self%check_declar_info(id)
    if (self%status%code/=accept) return
    call self%collect_mutuals(id)
    if (self%status%code/=accept) return
    call self%specialize_nested()
    if (self%status%code/=accept) return
    call self%check_inductive_specs()
    if (self%status%code/=accept) return
    call self%mk_ind_types_extension()
    phases: block
      if (self%status%code/=accept) exit phases
      do i=1,self%ist%count
        k=self%ist%ctors(i)
        do while(k/=list_nil)
          c=self%ctx%lists%head(k); e=self%ist%ctor_ty(c); call self%check_ctor(i,e)
          if (self%status%code/=accept) exit phases
          k=self%ctx%lists%tail(k)
        end do
      end do
      call self%mk_ctors_extension()
      if (self%status%code/=accept) exit phases
      call self%mk_elim_level()
      if (self%status%code/=accept) exit phases
      call self%init_k_target(); call self%mk_majors_motives(); call self%mk_minors()
      if (self%status%code/=accept) exit phases
      recs=self%mk_recursors()
      if (self%status%code/=accept) exit phases
      if (self%ist%nested) then
        rs=self%ctx%strings%intern('rec'); n=self%ist%name(1); base=self%ctx%names%str(n,rs)
        call grow(self%ist%rec_from,self%ist%nested_count); call grow(self%ist%rec_to,self%ist%nested_count)
        do i=1,self%ist%nested_count
          n=self%ist%nested_name(i); self%ist%rec_from(i)=self%ctx%names%str(n,rs)
          self%ist%rec_to(i)=self%index_name(base,i)
        end do
      end if
      call self%check_recursor_names(id,recs)
      if (self%status%code/=accept) exit phases
      if (self%ist%nested) then
        call self%restore_and_check(recs)
      else
        call self%compare_inductive_exports(recs)
      end if
    end block phases
    call self%ctx%decls%end_temp()
  end subroutine
  subroutine check_ctor_or_rec(self,id)
    class(checker), intent(inout) :: self
    integer, intent(in) :: id
    integer :: n,parent,k,ind
    call self%check_declar_info(id)
    if (self%status%code/=accept) return
    if (self%ctx%decls%tag(id)==dconstructor) then
      n=self%ctx%decls%inductive_name(id); parent=self%ctx%decls%by_name%get(n,0,0,0)
      if (parent==0) then
        call self%status%fail(reject,'constructor parent not found'); return
      end if
      if (self%ctx%decls%tag(parent)/=dinductive) then
        call self%status%fail(reject,'constructor parent is not inductive'); return
      end if
      n=self%ctx%decls%name(id)
      if (.not.self%list_contains(self%ctx%decls%ctor_names(parent),n)) &
        call self%status%fail(reject,'constructor absent from parent specification')
    else
      k=self%ctx%decls%all_names(id)
      if (k==list_nil) then
        call self%status%fail(reject,'recursor has no inductive types'); return
      end if
      n=self%ctx%lists%head(k); ind=self%ctx%decls%by_name%get(n,0,0,0)
      if (ind==0) then
        call self%status%fail(reject,'recursor inductive not found'); return
      end if
      if (self%ctx%decls%tag(ind)/=dinductive) then
        call self%status%fail(reject,'recursor first type is not inductive'); return
      end if
      do while(k/=list_nil)
        n=self%ctx%lists%head(k); ind=self%ctx%decls%by_name%get(n,0,0,0)
        if (ind==0 .or. ind>=id) then
          call self%status%fail(reject,'recursor exported before inductive type'); return
        end if
        k=self%ctx%lists%tail(k)
      end do
    end if
  end subroutine
