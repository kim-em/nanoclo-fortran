program test_read_sets
  use closure
  use expr, only: expr_arena,elam
  implicit none
  type(expr_arena) :: dag
  type(env_arena) :: envs
  type(status_t) :: status
  type(uses_t) :: u,a
  type(closure_t) :: k1,k2
  integer :: p,e,v,i,j,chain1,chain2,key1,key2,checks=0,seed=81,terms(400),x,y
  integer(int64) :: before,w(8)
  logical :: expected(512),actual(512)
  call envs%init()
  p=dag%sort(1)
  v=dag%var(63); u=envs%reads%uses_mask(dag,v)
  call check(u%tag==uses_mask_kind .and. u%mask==ibset(0_int64,63),'sign-bit read mask')
  a=envs%reads%under_uses(u)
  call check(a%mask==ibset(0_int64,62),'logical right shift')
  v=dag%var(64); u=envs%reads%uses_mask(dag,v)
  call check(u%tag==uses_wide,'wide read at index 64')
  a=envs%reads%under_uses(u)
  call check(a%tag==uses_mask_kind .and. a%mask==ibset(0_int64,63),'wide under binder demotion')
  v=dag%var(512); u=envs%reads%uses_mask(dag,v)
  call check(u%tag==uses_deep,'read beyond wide bound')
  e=dag%binder(elam,1,0,p,v); u=envs%reads%uses_mask(dag,e)
  call check(u%tag==uses_deep,'deep set stays conservative under binder')
  e=dag%var(0)
  do i=1,511
    v=dag%var(i); e=dag%app(e,v)
    u=envs%reads%uses_mask(dag,e)
    call check(u%tag==uses_dense,'dense classification')
    a=envs%reads%uses_undense(dag,e); w=envs%reads%words(a)
    do j=0,i
      call check(btest(w(j/64+1),mod(j,64)),'undense expands every bit')
    end do
  end do
  terms(1)=p
  do i=2,100
    terms(i)=dag%var(mod(i*37,500))
  end do
  do i=101,size(terms)
    x=terms(random_index(i-1)); y=terms(random_index(i-1))
    if (mod(i,3)==0) then
      terms(i)=dag%binder(elam,1,0,x,y)
    else
      terms(i)=dag%app(x,y)
    end if
    expected=.false.; call collect(terms(i),0,expected)
    u=envs%reads%uses_undense(dag,terms(i)); w=envs%reads%words(u)
    do j=1,512
      actual(j)=btest(w(shiftr(j-1,6)+1),mod(j-1,64))
    end do
    call check(all(actual.eqv.expected),'read set matches independent occurrence traversal')
  end do
  ! Different unread entries, identical entries read at index 1 and 64.
  chain1=env_nil; chain2=env_nil
  do i=1,70
    x=i; y=i+1000
    if (i==69 .or. i==6) y=x
    chain1=envs%push_entry_v(chain1,x); chain2=envs%push_entry_v(chain2,y)
  end do
  e=dag%var(1); v=dag%var(64); e=dag%app(e,v)
  key1=envs%read_key(dag,e,chain1); key2=envs%read_key(dag,e,chain2)
  call check(key1<0 .and. key1==key2,'wide read views share across irrelevant bindings')
  before=envs%ctrs(11)
  k1=envs%key(dag,closure_t(e,chain1),status)
  call check(envs%ctrs(11)-before==2,'projection pushes only read entries')
  k2=envs%key(dag,closure_t(e,chain2),status)
  call check(k1%env==k2%env .and. envs%len(k1%env)==2,'wide projected keys agree')
  before=envs%ctrs(11); k1=envs%key(dag,closure_t(e,chain1),status)
  call check(envs%ctrs(11)==before,'projection memo prevents repeat pushes')
  e=dag%var(1)
  key1=envs%read_key(dag,e,chain1); key2=envs%read_key(dag,e,chain2)
  call check(key1<0 .and. key1==key2,'mask read views agree')
  k1=envs%key(dag,closure_t(e,chain1),status); k2=envs%key(dag,closure_t(e,chain2),status)
  call check(k1%env==k2%env,'mask projected keys agree')
  e=dag%var(0)
  call check(envs%read_key(dag,e,chain1)==chain1,'dense keeps whole environment')
  call check(envs%read_key(dag,p,chain1)==env_nil,'closed read key')
  e=dag%var(100)
  call check(envs%read_key(dag,e,chain1)==chain1,'short view falls back to whole environment')
  k1=envs%key(dag,closure_t(e,chain1),status)
  call check(k1%env==chain1,'short projection falls back')
  call check(status%code==accept,'key operations succeed')
  call envs%reset_decl()
  call check(envs%count==1 .and. envs%view_count==0,'reset clears environment and view arenas')
  call check(envs%reads%wide_count>0,'reset preserves expression read masks')
  call envs%reads%reset_const_caches()
  call check(envs%reads%wide_count==0,'DAG reset clears wide read sets')
  print '(a,i0,a)', 'read sets: ',checks,' checks passed'
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
    seed=mod(seed*251+17,65521); value=mod(seed,bound)+1
  end function
  recursive subroutine collect(e,off,out)
    use expr, only: evar,eapp,epi,elam
    integer, intent(in) :: e,off
    logical, intent(inout) :: out(512)
    integer :: n
    select case(dag%get_tag(e))
    case(evar)
      n=dag%get_a(e)-off
      if (n>=0 .and. n<512) out(n+1)=.true.
    case(eapp)
      call collect(dag%get_a(e),off,out); call collect(dag%get_b(e),off,out)
    case(epi,elam)
      call collect(dag%get_c(e),off,out); call collect(dag%get_d(e),off+1,out)
    end select
  end subroutine
end program
