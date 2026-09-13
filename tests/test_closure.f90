program test_closure
  use closure
  use expr, only: expr_arena,fvar_level
  implicit none
  type(env_arena) :: envs
  type(expr_arena) :: dag
  type(status_t) :: status
  integer :: prop,local,v0,env,env2,env3,id,i,j,node,e,venv,checks=0
  integer :: chain(65535)
  call envs%init()
  prop=dag%sort(1); local=dag%local(1,0,prop,fvar_level,5); v0=dag%var(0)
  env=envs%push_entry(dag,env_nil,entry_neu,local,env_nil,status)
  call check(envs%len(env)==1 .and. envs%next_level(env)==6,'neutral next_level')
  env2=envs%push_entry(dag,env_nil,entry_val,v0,env,status)
  env3=envs%push_entry(dag,env_nil,entry_val,local,env_nil,status)
  call check(env2==env3,'normalize delayed variable before interning')
  env2=envs%push_entry(dag,env_nil,entry_val,prop,env,status)
  env3=envs%push_entry(dag,env_nil,entry_val,prop,env_nil,status)
  call check(env2==env3,'closed delayed entry drops environment')
  env2=envs%push_entry_v(env,42)
  call check(envs%next_level(env2)==6,'evaluated entry inherits next_level')
  e=v0; venv=env2
  call envs%norm_clo(dag,e,venv,status)
  call check(e==v0 .and. venv==env2,'normalization stops at evaluated entry')
  env=env_nil
  do i=1,size(chain)
    env=envs%push_entry_v(env,i)
    chain(i)=env
  end do
  do i=1,size(chain)
    node=envs%lookup(env,i-1,status)
    call check(envs%entry(node)==size(chain)-i+1,'Myers lookup deep chain')
    id=envs%push_entry_v(envs%parent(chain(i)),i)
    call check(id==chain(i),'environment interning after growth')
  end do
  do i=1,size(chain),61
    do j=0,i-1,37
      node=envs%lookup(chain(i),j,status)
      call check(envs%entry(node)==i-j,'Myers lookup intermediate chain')
    end do
  end do
  call check(status%code==accept,'valid environment operations')
  node=envs%lookup(env_nil,0,status)
  call check(status%code==reject .and. node==0,'unbound variable rejects')
  call check(envs%ctrs(11)==2_int64*size(chain)+6,'push_entry counter includes cache hits')
  print '(a,i0,a)', 'closures: ',checks,' checks passed'
contains
  subroutine check(ok,label)
    logical, intent(in) :: ok
    character(*), intent(in) :: label
    if (.not.ok) then
      print *, 'FAIL: ',label
      if (allocated(status%message)) print *, status%message
      error stop 1
    end if
    checks=checks+1
  end subroutine
end program
