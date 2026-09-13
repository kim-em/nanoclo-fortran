program test_eval
  use kernel, only: checker
  use closure, only: env_nil
  use nbe, only: vsort,vthunk
  use spines, only: spine_empty,elim_app
  use expr, only: elam,epi
  use base
  implicit none
  type(checker) :: ck
  integer :: prop,v0,idlam,app,v,a,b,thunk,n,i,body1,body2,pi1,pi2,sx,sy,checks=0
  integer(int64) :: fuel
  logical :: ok
  call ck%init()
  prop=ck%ctx%exprs%sort(1); v0=ck%ctx%exprs%var(0)
  idlam=ck%ctx%exprs%binder(elam,1,0,prop,v0)
  app=ck%ctx%exprs%app(idlam,prop)
  v=ck%nb_eval(0,env_nil,app)
  call check(ck%status%code==accept .and. ck%nb%tag(v)==vsort,'beta reduction')
  a=ck%nb_readback(v)
  call check(a==prop,'readback beta result')
  thunk=ck%nb_thunk(env_nil,app)
  call check(ck%nb%tag(thunk)==vthunk .and. ck%nb%forced(thunk)==0,'unforced thunk')
  a=ck%nb_force(0,thunk); n=ck%nb%count; b=ck%nb_force(0,thunk)
  call check(a==v .and. b==v .and. ck%nb%count==n,'force evaluates once')
  ! A discarded composite argument stays unevaluated even if its evaluation
  ! would fail. Declaration inference still checks its type separately.
  a=ck%ctx%exprs%binder(elam,1,0,prop,prop); b=ck%ctx%exprs%app(prop,prop); app=ck%ctx%exprs%app(a,b)
  v=ck%nb_eval(0,env_nil,app)
  call check(ck%status%code==accept .and. ck%nb%tag(v)==vsort,'discarded argument remains delayed')
  ! Distinct syntactic bodies yield distinct Pi graphs with equal meanings.
  body1=prop; body2=prop
  do i=1,12
    body1=ck%ctx%exprs%binder(epi,1,0,prop,body1)
    body2=ck%ctx%exprs%binder(epi,2,1,prop,body2)
  end do
  pi1=ck%nb_eval(0,env_nil,body1); pi2=ck%nb_eval(0,env_nil,body2)
  sx=ck%nb%spines%snoc(spine_empty,elim_app,pi1,0); sy=ck%nb%spines%snoc(spine_empty,elim_app,pi2,0)
  ! Seed a small grant to exercise the same exhaustion/deepening mechanism
  ! without requiring thousands of nested stack frames in the unit test.
  ck%nb%probe_escalate=4
  ok=ck%nb_spine_probe(0,sx,sy)
  call check(.not.ok .and. ck%nb%probe_depth==0 .and. .not.ck%nb%probe_aborted,'probe exhaustion unwinds scope')
  call check(ck%nb%probe_escalate==8 .and. ck%nb%conv_neg_probe%count==0,'abort deepens and drops scoped failures')
  fuel=ck%nb%probe_fuel
  ok=ck%nb_spine_probe(0,sx,sy)
  call check(.not.ok .and. ck%nb%probe_fuel==fuel .and. ck%rp%ctrs(26)==1,'failed probe is not retried')
  ok=ck%nb_conv(0,pi1,pi2)
  call check(ok .and. ck%status%code==accept,'aborted probe does not poison ordinary conversion')
  print '(a,i0,a)', 'evaluation: ',checks,' checks passed'
contains
  subroutine check(ok,label)
    logical, intent(in) :: ok
    character(*), intent(in) :: label
    if (.not.ok) then
      print *, 'FAIL: ',label
      if (allocated(ck%status%message)) print *, ck%status%message
      error stop 1
    end if
    checks=checks+1
  end subroutine
end program
