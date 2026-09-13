! These tests install trusted declarations directly to test reduction independently
! of the still-disabled inductive validator. They are not acceptance evidence.
program test_inductive
  use kernel, only: checker
  use base
  use declarations, only: dinductive,dconstructor,drecursor,dquot,hint_opaque
  use expr, only: epi,elam,fvar_unique
  use closure, only: closure_t,env_nil
  use nbe, only: vrigid
  use spines, only: spine_empty,elim_proj
  implicit none
  type(checker) :: ck
  type(closure_t) :: inferred
  integer :: checks=0,serial=0,sort0,sort1,sort2,l,ind,ctor,rec,n,mk,rn,st,ce,re,a,x,s,m,f,mt,ft,rt,rule
  integer :: pv,motive,minor,major,call_e,v,result,sv,field0,field1,t,neutral,y,k,nq,cq,rq,q,qe,ql,mq,fq
  integer :: ignored,head,sp,pre,oldcount
  logical :: ok
  call ck%init()
  sort0=ck%ctx%exprs%sort(1); l=ck%ctx%levels%succ(1); sort1=ck%ctx%exprs%sort(l)
  l=ck%ctx%levels%succ(l); sort2=ck%ctx%exprs%sort(l)
  ! S is a dependent pair with fields A : Type and a : A.
  n=ck%name_id('S'); mk=ck%name_id('S.mk'); rn=ck%name_id('S.rec')
  st=ck%ctx%exprs%const(n,1); ce=ck%ctx%exprs%const(mk,1); re=ck%ctx%exprs%const(rn,1)
  a=local(sort1); x=local(a); s=local(st)
  ind=decl(dinductive,n,sort2); ctor=decl(dconstructor,mk,pis([a,x],st))
  ck%ctx%decls%ctor_names(ind)=ck%ctx%lists%cons(mk,1)
  ck%ctx%decls%all_names(ind)=ck%ctx%lists%cons(n,1)
  ck%ctx%decls%inductive_name(ctor)=n; ck%ctx%decls%num_fields(ctor)=2
  mt=pis([s],sort2); m=local(mt)
  major=apps(ce,[a,x]); ft=pis([a,x],apps(m,[major])); f=local(ft)
  rt=pis([m,f,s],apps(m,[s])); rec=decl(drecursor,rn,rt)
  ck%ctx%decls%num_motives(rec)=1; ck%ctx%decls%num_minors(rec)=1
  ck%ctx%decls%all_names(rec)=ck%ctx%lists%cons(n,1)
  rule=lams([m,f,a,x],apps(f,[a,x])); k=ck%ctx%decls%add_rule(mk,2,rule)
  ck%ctx%decls%rules(rec)=ck%ctx%lists%cons(k,1)
  pv=local(sort0); motive=lams([s],sort1); minor=lams([a,x],a)
  major=apps(ce,[sort0,pv]); call_e=apps(re,[motive,minor,major])
  v=ck%nb_eval(0,env_nil,call_e); pre=ck%work_top; result=ck%nb_whnf(0,v)
  call check(ck%status%code==accept,'constructor recursor evaluation succeeds')
  call check(ck%nb_readback(result)==sort0,'dependent constructor rule substitutes both fields')
  call check(ck%work_top==pre,'iota releases work frame')
  t=ck%nb_eval(0,env_nil,sort0); ok=ck%nb_conv(0,v,t)
  call check(ok .and. ck%status%code==accept,'conversion fires recursor against non-recursor')
  oldcount=ck%nb%count; t=ck%nb_whnf(0,v)
  call check(t==result .and. ck%nb%count==oldcount,'iota memo reuses result')
  sv=ck%nb_eval(0,env_nil,major); field0=ck%nb_proj(0,n,0,sv); field1=ck%nb_proj(0,n,1,sv)
  t=ck%nb_readback(field0); result=ck%nb_readback(field1)
  call check(t==sort0 .and. result==pv,'dependent fields project')
  t=ck%nb_type(0,sv); result=ck%nb_field_type(0,sv,t,n,1)
  call check(ck%nb_readback(result)==sort0,'value field type supplies earlier projection')
  t=ck%ctx%exprs%proj(n,1,major); inferred=ck%infer_go(closure_t(t,env_nil),.true.)
  call check(ck%status%code==accept .and. inferred%e==sort0,'closure projection inference')
  ! A neutral S eta-expands into S.mk (.0 s) (.1 s), including dependent field types.
  neutral=local(st); sv=ck%nb_eval(0,env_nil,neutral)
  field0=ck%nb_proj(0,n,0,sv); field1=ck%nb_proj(0,n,1,sv)
  t=ck%nb_type(0,field1)
  call check(ck%nb_conv(0,t,field0),'neutral dependent projection has projected type')
  y=ck%nb_const(mk,1); y=ck%nb_apply(0,y,field0); y=ck%nb_apply(0,y,field1)
  ok=ck%nb_conv(0,sv,y)
  call check(ok .and. ck%status%code==accept,'dependent structure eta conversion')
  call_e=apps(re,[motive,minor,neutral]); v=ck%nb_eval(0,env_nil,call_e); result=ck%nb_whnf(0,v)
  call check(ck%nb_conv(0,result,field0),'structure recursor fires on neutral major')
  ! Wrong-name and missing-field projections remain stuck at evaluation time.
  t=ck%nb_proj(0,ck%quot_mk,0,y); head=ck%nb%head(t); sp=ck%nb%spine(t)
  call check(ck%nb%tag(t)==vrigid .and. ck%nb%spines%tag(sp)==elim_proj,'wrong-name projection stays stuck')
  t=ck%nb_proj(0,n,5,y); sp=ck%nb%spine(t)
  call check(ck%nb%spines%tag(sp)==elim_proj,'missing-field projection stays stuck')
  ! K reduces a neutral proof without unfolding it to its constructor.
  nq=ck%name_id('Q'); cq=ck%name_id('Q.mk'); rq=ck%name_id('Q.rec')
  qe=ck%ctx%exprs%const(nq,1); ql=ck%ctx%exprs%const(cq,1)
  ind=decl(dinductive,nq,sort0); ctor=decl(dconstructor,cq,qe)
  ck%ctx%decls%ctor_names(ind)=ck%ctx%lists%cons(cq,1)
  ck%ctx%decls%all_names(ind)=ck%ctx%lists%cons(nq,1)
  ck%ctx%decls%inductive_name(ctor)=nq
  q=local(qe); mq=local(pis([q],sort1)); fq=local(apps(mq,[ql]))
  rec=decl(drecursor,rq,pis([mq,fq,q],apps(mq,[q])))
  ck%ctx%decls%num_motives(rec)=1; ck%ctx%decls%num_minors(rec)=1; ck%ctx%decls%is_k(rec)=1
  ck%ctx%decls%all_names(rec)=ck%ctx%lists%cons(nq,1)
  rule=lams([mq,fq],fq); k=ck%ctx%decls%add_rule(cq,0,rule); ck%ctx%decls%rules(rec)=ck%ctx%lists%cons(k,1)
  re=ck%ctx%exprs%const(rq,1); motive=lams([q],sort0); call_e=apps(re,[motive,pv,q])
  v=ck%nb_eval(0,env_nil,call_e); result=ck%nb_whnf(0,v)
  t=ck%nb_readback(result)
  call check(ck%status%code==accept .and. t==pv,'K reduction on neutral proof')
  ! Partial recursors and spines containing a projection do not iota-reduce.
  v=ck%nb_const(rq,1); result=ck%nb_iota(0,v)
  call check(result==0 .and. ck%nb%iota_cache%get(v,0,0,0)==-1,'partial recursor caches None')
  sp=ck%nb%spines%snoc(spine_empty,elim_proj,nq,0); head=ck%nb%head(v); v=ck%nb%mk_rigid(head,sp)
  result=ck%nb_iota(0,v)
  call check(result==0 .and. ck%work_top==0,'projection spine cannot fire recursor')
  ! Quot.lift and Quot.ind share the Quot.mk payload rule, with distinct offsets.
  ignored=decl(dquot,ck%quot_mk,sort1); ignored=decl(dquot,ck%quot_lift,sort1)
  ignored=decl(dquot,ck%quot_ind,sort1)
  major=ck%nb_const(ck%quot_mk,1); t=ck%nb_eval(0,env_nil,sort0)
  major=ck%nb_apply(0,major,t); major=ck%nb_apply(0,major,t); major=ck%nb_apply(0,major,t)
  x=local(sort1); minor=lams([x],x); f=ck%nb_eval(0,env_nil,minor)
  do k=1,2
    v=ck%nb_const(merge(ck%quot_lift,ck%quot_ind,k==1),1)
    do l=1,3
      v=ck%nb_apply(0,v,t)
    end do
    v=ck%nb_apply(0,v,f)
    if (k==1) v=ck%nb_apply(0,v,t)
    v=ck%nb_apply(0,v,major); result=ck%nb_whnf(0,v)
    call check(result==t .and. ck%status%code==accept,'quotient payload reduction')
  end do
  call check(ck%work_top==0,'all nested work frames released')
  ! Projections from Prop may only reveal proof fields. S with the same
  ! constructor but codomain Prop is an existential package, not a data pair.
  ind=ck%ctx%decls%lookup(n,huge(0)); ck%ctx%decls%ty(ind)=sort0
  ! Test-only mutation invalidates the persistent instantiated type cache.
  call ck%g_inst_ty%clear()
  call ck%reset_decl(); t=ck%ctx%exprs%proj(n,1,neutral)
  inferred=ck%infer_go(closure_t(t,env_nil),.true.)
  call check(ck%status%code==reject,'Prop projection cannot depend on an earlier data field')
  ck%status=status_t(); call ck%reset_decl(); t=ck%ctx%exprs%proj(n,0,neutral)
  inferred=ck%infer_go(closure_t(t,env_nil),.true.)
  call check(ck%status%code==reject,'Prop projection cannot reveal a data field')
  ck%status=status_t(); ck%ctx%decls%ty(ind)=sort2; call ck%g_inst_ty%clear(); call ck%reset_decl()
  t=ck%ctx%exprs%proj(n,2,neutral); inferred=ck%infer_go(closure_t(t,env_nil),.true.)
  call check(ck%status%code==reject,'out-of-range projection is rejected by inference')
  ck%status=status_t(); call ck%reset_decl()
  t=ck%ctx%exprs%proj(nq,0,neutral); inferred=ck%infer_go(closure_t(t,env_nil),.true.)
  call check(ck%status%code==reject,'projection type name must match structure type')
  print '(a,i0,a)', 'inductive reduction: ',checks,' checks passed'
contains
  integer function local(ty) result(e)
    integer, intent(in) :: ty
    serial=serial+1; e=ck%ctx%exprs%local(1,0,ty,fvar_unique,serial)
  end function
  integer function apps(fn,args) result(e)
    integer, intent(in) :: fn,args(:)
    integer :: i
    e=fn
    do i=1,size(args)
      e=ck%ctx%exprs%app(e,args(i))
    end do
  end function
  integer function binders(tag,locals,body) result(e)
    integer, intent(in) :: tag,locals(:),body
    integer :: i,ty,b
    e=body
    do i=size(locals),1,-1
      ty=ck%ctx%exprs%get_c(locals(i)); b=ck%ctx%exprs%abstr(e,[locals(i)])
      e=ck%ctx%exprs%binder(tag,1,0,ty,b)
    end do
  end function
  integer function pis(locals,body) result(e)
    integer, intent(in) :: locals(:),body
    e=binders(epi,locals,body)
  end function
  integer function lams(locals,body) result(e)
    integer, intent(in) :: locals(:),body
    e=binders(elam,locals,body)
  end function
  integer function decl(tag,name,ty) result(id)
    integer, intent(in) :: tag,name,ty
    id=ck%ctx%decls%add(tag,name,1,ty,0,hint_opaque,0,ck%status)
  end function
  subroutine check(ok,label)
    logical, intent(in) :: ok
    character(*), intent(in) :: label
    if (.not.ok) then
      print *, 'FAIL: ',label
      if (allocated(ck%status%message)) print *,ck%status%message
      error stop 1
    end if
    checks=checks+1
  end subroutine
end program
