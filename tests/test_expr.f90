program test_expr
  use base
  use levels, only: level_arena,zero
  use sequences, only: sequence_arena
  use expr, only: expr_arena,epi,elam,fvar_level,fvar_unique
  implicit none
  type(expr_arena) :: dag,worker
  type(level_arena) :: universes
  type(sequence_arena) :: lists
  type(status_t) :: status
  integer :: u,q,one,two,params,args,sort_u,sort_one,constant,constant2,substituted
  integer :: prop,x,y,v0,v1,v2,body,pi,term,a,b,c,i,n,checks=0,seed=71
  integer :: terms(1000),level_local,unique_local
  prop=dag%sort(1)
  x=dag%local(2,0,prop,fvar_unique,1)
  y=dag%local(3,0,prop,fvar_unique,2)
  v0=dag%var(0); v1=dag%var(1); v2=dag%var(2)
  body=dag%app(v1,v0)
  pi=dag%binder(epi,1,0,prop,body)
  call check(dag%get_nlbv(body)==2 .and. dag%get_nlbv(pi)==1,'loose bound variable cache')
  call check(dag%get_flags(pi)==0 .and. dag%get_flags(x)==1,'free variable cache')
  term=dag%inst(body,[x,y]); a=dag%app(x,y)
  call check(term==a,'instantiate newest substitution at index zero')
  b=dag%app(x,v0); b=dag%binder(epi,1,0,prop,b)
  call check(dag%inst(pi,[x])==b,'instantiate under binder')
  call check(dag%inst(v2,[x])==v2,'out of range substitution remains unchanged')
  call check(dag%abstr(a,[x,y])==body,'abstract unique locals')
  call check(dag%abstr(b,[x])==pi,'abstract under binder')
  term=dag%let_(1,prop,x,y,.false.)
  a=dag%abstr(term,[x,y]); b=dag%inst(a,[x,y])
  call check(b==term,'let abstraction roundtrip')
  a=dag%proj(4,1,term); b=dag%abstr(a,[x,y]); b=dag%inst(b,[x,y])
  call check(b==a,'projection abstraction roundtrip')
  level_local=dag%local(2,0,prop,fvar_level,7)
  unique_local=dag%local(3,0,level_local,fvar_unique,3)
  call check(dag%max_level(unique_local)==8,'max_level traverses unique local type')
  term=dag%app(level_local,unique_local)
  a=dag%abstr_levels(term,7,8); b=dag%app(v0,unique_local)
  call check(a==b,'level abstraction preserves unique locals')
  a=dag%abstr_levels(term,8,8)
  call check(a==term,'level abstraction cutoff')
  pi=dag%binder(elam,1,0,prop,term)
  a=dag%abstr_levels(pi,7,8); b=dag%app(v1,unique_local); b=dag%binder(elam,1,0,prop,b)
  call check(a==b,'level abstraction binder depth')
  dag%current_origin=1
  terms(1:3)=[prop,x,y]
  do i=4,size(terms)
    a=terms(random_index(i-1)); b=terms(random_index(i-1)); c=terms(random_index(i-1))
    select case(mod(i,5))
    case(0)
      term=dag%app(a,b)
    case(1)
      term=dag%binder(epi,2,mod(i,4),a,b)
    case(2)
      term=dag%binder(elam,3,mod(i,4),a,b)
    case(3)
      term=dag%let_(2,a,b,c,mod(i,2)==0)
    case(4)
      term=dag%proj(4,mod(i,3),a)
    end select
    terms(i)=term
    n=dag%count
    a=dag%abstr(term,[x,y]); b=dag%inst(a,[x,y])
    call check(b==term,'generated abstraction/instantiation roundtrip')
    call check(dag%count>=n,'arena only appends')
  end do
  call check(dag%get_origin(prop)==0 .and. dag%get_origin(term)==1,'export/scratch origin')
  call universes%init(); call lists%init()
  u=universes%param(2); q=universes%param(3); one=universes%succ(zero); two=universes%succ(one)
  params=lists%from_array([u,q]); args=lists%from_array([one,two])
  sort_u=dag%sort(u); sort_one=dag%sort(one)
  constant=dag%const(4,params); constant2=dag%const(4,args)
  body=dag%app(constant,sort_u); pi=dag%binder(epi,2,1,sort_u,body)
  term=dag%let_(3,sort_u,body,pi,.true.); term=dag%proj(5,0,term)
  a=dag%app(constant2,sort_one); b=dag%binder(epi,2,1,sort_one,a)
  b=dag%let_(3,sort_one,a,b,.true.); b=dag%proj(5,0,b)
  substituted=dag%subst_expr_levels(universes,lists,term,params,args,status)
  call check(status%code==accept .and. substituted==b,'universe substitution through expression forms')
  n=dag%count
  a=dag%subst_expr_levels(universes,lists,term,params,args,status)
  call check(a==b .and. dag%count==n,'persistent declaration substitution cache')
  args=lists%from_array([one])
  a=dag%subst_expr_levels(universes,lists,term,params,args,status)
  call check(status%code==reject,'universe argument arity mismatch')
  n=dag%count
  call dag%seal_exports()
  call check(dag%scratch_capacity()==0,'sealing moves export arrays instead of copying')
  call check(dag%get_nlbv(v2)==3 .and. dag%get_a(body)==constant,'export references remain readable')
  a=dag%app(constant,sort_u)
  call check(a==body .and. dag%count==n,'interning probes shared exports first')
  worker=dag
  a=dag%var(65534); b=worker%var(65533)
  call check(a==b .and. dag%get_a(a)/=worker%get_a(b),'worker scratch spaces are independent')
  call dag%clear_scratch()
  call check(dag%count==n .and. worker%count==n+1,'scratch reset preserves other workers')
  a=dag%var(65532)
  call check(dag%get_a(a)==65532 .and. dag%get_nlbv(v2)==3,'reused scratch index preserves export storage')
  print '(a,i0,a)', 'expressions: ',checks,' checks passed'
contains
  subroutine check(ok,label)
    logical, intent(in) :: ok
    character(*), intent(in) :: label
    if (.not.ok) then
      print *, 'FAIL: ',label
      error stop 1
    end if
    checks=checks+1
  end subroutine
  integer function random_index(bound) result(value)
    integer, intent(in) :: bound
    seed=mod(seed*251+17,65521)
    value=mod(seed,bound)+1
  end function
end program
