program test_eq_mod
  use closure
  use expr, only: expr_arena,fvar_unique,elam,epi
  implicit none
  type(expr_arena) :: dag
  type(env_arena) :: envs
  type(status_t) :: status
  integer :: p,x,y,v0,v1,env,a,b,c,e,envv,i,checks=0,seed=13,terms(300)
  integer(int64) :: hits
  logical :: ok
  call envs%init()
  p=dag%sort(1); x=dag%local(2,0,p,fvar_unique,1); y=dag%local(3,0,p,fvar_unique,2)
  v0=dag%var(0); v1=dag%var(1)
  env=envs%push_entry(dag,env_nil,entry_neu,x,env_nil,status)
  env=envs%push_entry(dag,env,entry_neu,y,env_nil,status)
  a=dag%app(v1,v0); b=dag%app(x,y); c=dag%app(y,x)
  ok=envs%eq_mod(dag,a,env,0,b,env_nil,0,status)
  call check(ok,'application modulo environment')
  hits=envs%ctrs(13)
  ok=envs%eq_mod(dag,b,env_nil,0,a,env,0,status)
  call check(ok .and. envs%ctrs(13)==hits+1,'symmetric positive memo')
  ok=envs%eq_mod(dag,a,env,0,c,env_nil,0,status)
  call check(.not.ok,'different application arguments')
  hits=envs%ctrs(13)
  ok=envs%eq_mod(dag,c,env_nil,0,a,env,0,status)
  call check(.not.ok .and. envs%ctrs(13)==hits+1,'symmetric negative memo')
  envv=envs%push_entry_v(env_nil,42)
  ok=envs%eq_mod(dag,v0,envv,0,v0,envv,0,status)
  call check(.not.ok .and. status%code==accept,'evaluated binding defers to value comparison')
  e=dag%binder(elam,1,0,p,a)
  b=dag%inst(e,[x,y])
  ok=envs%eq_mod(dag,e,env,0,b,env_nil,0,status)
  call check(ok,'binder offsets distinguish inner and outer variables')
  terms(1:4)=[v0,v1,p,x]
  do i=5,size(terms)
    a=terms(random_index(i-1)); b=terms(random_index(i-1))
    select case(mod(i,4))
    case(0)
      e=dag%app(a,b)
    case(1)
      e=dag%binder(epi,2,0,a,b)
    case(2)
      e=dag%let_(3,p,a,b,.false.)
    case(3)
      e=dag%proj(4,0,a)
    end select
    terms(i)=e
    c=dag%inst(e,[x,y])
    ok=envs%eq_mod(dag,e,env,0,c,env_nil,0,status)
    call check(ok,'eq_mod agrees with explicit instantiation')
  end do
  call check(status%code==accept,'valid closure comparisons')
  call envs%reset_decl()
  call check(envs%eq_mod_cache%count==0 .and. envs%eqm_sides%count==0,'memo reset drops environment references')
  print '(a,i0,a)', 'eq_mod: ',checks,' checks passed'
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
end program
