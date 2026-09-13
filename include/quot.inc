! Quot declaration reconstruction translated from nanoclo quot.rs.
  integer function expr_apps(self,fn,args) result(e)
    class(checker), intent(inout) :: self
    integer, intent(in) :: fn,args(:)
    integer :: i
    e=fn
    do i=1,size(args)
      e=self%ctx%exprs%app(e,args(i))
    end do
  end function
  integer function expr_binders(self,tag,locals,body) result(e)
    class(checker), intent(inout) :: self
    integer, intent(in) :: tag,locals(:),body
    integer :: i
    e=body
    do i=size(locals),1,-1
      e=self%abstr_binder(tag,locals(i),e)
    end do
  end function
  integer function arrows(self,domains,body) result(e)
    class(checker), intent(inout) :: self
    integer, intent(in) :: domains(:),body
    integer :: i
    e=body
    do i=size(domains),1,-1
      e=self%ctx%exprs%binder(epi,anonymous,0,domains(i),e)
    end do
  end function
  subroutine check_eq_for_quot(self)
    class(checker), intent(inout) :: self
    integer :: n,cn,alpha_name,a_name,prop,id,params,eqc,u,alpha,a,expected,ctor,old_current,ty
    n=self%name_id('Eq'); cn=self%name_id('Eq.refl'); alpha_name=self%name_id('α'); a_name=self%name_id('a')
    prop=self%ctx%exprs%sort(1); id=self%ctx%decls%lookup(n,self%cutoff)
    if (id==0) then
      call self%status%fail(reject,'Quot requires Eq'); return
    end if
    if (self%ctx%decls%tag(id)/=dinductive .or. self%ctx%decls%num_params(id)/=2) then
      call self%status%fail(reject,'invalid Eq prerequisite'); return
    end if
    params=self%ctx%decls%uparams(id)
    if (self%ctx%lists%length(params)/=1) then
      call self%status%fail(reject,'Eq must have one universe parameter'); return
    end if
    eqc=self%ctx%exprs%const(n,params); u=self%ctx%lists%head(params); u=self%ctx%exprs%sort(u)
    alpha=self%mk_unique(alpha_name,1,u); expected=self%arrows([alpha,alpha],prop)
    expected=self%abstr_binder(epi,alpha,expected); old_current=self%current_decl; self%current_decl=id
    ty=self%ctx%decls%ty(id); call self%assert_def_eq(ty,expected); self%current_decl=old_current
    if (self%status%code/=accept) return
    ctor=self%ctx%decls%ctor_names(id)
    if (self%ctx%lists%length(ctor)/=1) then
      call self%status%fail(reject,'Eq must have one constructor'); return
    end if
    if (self%ctx%lists%head(ctor)/=cn) then
      call self%status%fail(reject,'Eq constructor must be Eq.refl'); return
    end if
    id=self%ctx%decls%lookup(cn,self%cutoff)
    if (id==0) then
      call self%status%fail(reject,'missing Eq.refl'); return
    end if
    if (self%ctx%decls%tag(id)/=dconstructor) then
      call self%status%fail(reject,'Eq.refl is not a constructor'); return
    end if
    params=self%ctx%decls%uparams(id)
    if (self%ctx%lists%length(params)/=1) then
      call self%status%fail(reject,'Eq.refl must have one universe parameter'); return
    end if
    u=self%ctx%lists%head(params); u=self%ctx%exprs%sort(u)
    alpha=self%mk_unique(alpha_name,1,u); a=self%mk_unique(a_name,0,alpha)
    expected=self%expr_apps(eqc,[alpha,a,a]); expected=self%expr_binders(epi,[alpha,a],expected)
    self%current_decl=id; ty=self%ctx%decls%ty(id); call self%assert_def_eq(ty,expected); self%current_decl=old_current
  end subroutine
  subroutine check_quot(self,id)
    class(checker), intent(inout) :: self
    integer, intent(in) :: id
    integer :: prop,u,v,qn,su,sv,lu,lv,nalpha,nbeta,nr,nf,na,nbb,nq,alpha,beta,r,f,a,b,aar,ab,qt,qmk,eqc
    integer :: expected_q,expected_mk,qar,fa,fb,eqapp,rab,lift_inner,expected,lhs,rhs,blocal,qlocal,ty,n
    prop=self%ctx%exprs%sort(1); u=self%name_id('u'); v=self%name_id('v'); nq=self%name_id('q')
    u=self%ctx%levels%param(u); v=self%ctx%levels%param(v)
    su=self%ctx%exprs%sort(u); sv=self%ctx%exprs%sort(v)
    lu=self%ctx%lists%cons(u,list_nil); lv=self%ctx%lists%cons(v,list_nil); qn=self%name_id('Quot')
    nalpha=self%name_id('A'); nbeta=self%name_id('B'); nr=self%name_id('r'); nf=self%name_id('f')
    na=self%name_id('a'); nbb=self%name_id('b')
    alpha=self%mk_unique(nalpha,1,su); beta=self%mk_unique(nbeta,1,sv)
    aar=self%arrows([alpha,alpha],prop); ab=self%arrows([alpha],beta)
    r=self%mk_unique(nr,0,aar); f=self%mk_unique(nf,0,ab)
    a=self%mk_unique(na,0,alpha); b=self%mk_unique(nbb,0,alpha)
    expected_q=self%expr_binders(epi,[alpha,r],su)
    qt=self%ctx%exprs%const(qn,lu); qar=self%expr_apps(qt,[alpha,r])
    expected_mk=self%arrows([alpha],qar); expected_mk=self%expr_binders(epi,[alpha,r],expected_mk)
    qmk=self%ctx%exprs%const(self%quot_mk,lu); n=self%name_id('Eq'); eqc=self%ctx%exprs%const(n,lv)
    fa=self%ctx%exprs%app(f,a); fb=self%ctx%exprs%app(f,b)
    eqapp=self%expr_apps(eqc,[beta,fa,fb]); rab=self%expr_apps(r,[a,b])
    lift_inner=self%arrows([rab],eqapp); lift_inner=self%expr_binders(epi,[a,b],lift_inner)
    n=self%ctx%decls%name(id); expected=0
    if (n==qn) then
      expected=expected_q
    else if (n==self%quot_mk) then
      expected=expected_mk
    else if (n==self%quot_lift) then
      call self%check_eq_for_quot()
      if (self%status%code/=accept) return
      expected=self%arrows([lift_inner,qar],beta); expected=self%expr_binders(epi,[alpha,r,beta,f],expected)
    else if (n==self%quot_ind) then
      ty=self%arrows([qar],prop); blocal=self%mk_unique(nbeta,1,ty); qlocal=self%mk_unique(nq,0,qar)
      lhs=self%expr_apps(qmk,[alpha,r,a]); lhs=self%ctx%exprs%app(blocal,lhs); lhs=self%abstr_binder(epi,a,lhs)
      rhs=self%ctx%exprs%app(blocal,qlocal); rhs=self%abstr_binder(epi,qlocal,rhs)
      expected=self%arrows([lhs],rhs); expected=self%expr_binders(epi,[alpha,r,blocal],expected)
    else
      call self%status%fail(reject,'invalid quotient declaration name'); return
    end if
    ty=self%ctx%decls%ty(id); call self%assert_def_eq(ty,expected)
  end subroutine
