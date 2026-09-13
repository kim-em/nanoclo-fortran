program test_nbe
  use nbe, only: value_arena,vthunk,hconst,caxiom,cctor
  use spines, only: spine_empty,elim_app,elim_proj
  use base
  implicit none
  type(value_arena) :: nb
  integer :: s,p,l,l2,v,t,t2,h,h2,r,r2,i,node,checks=0,ids(5000)
  call nb%init()
  s=nb%mk_sort(1)
  l=nb%mk_lam(10,0,17,1,18); l2=nb%mk_lam(99,3,17,1,18)
  call check(l==l2,'lambda interning ignores binder annotations')
  call check(nb%binder_name(l)==10 .and. nb%binder_style(l)==0,'first binder annotations survive')
  call nb%set_domain(l,s)
  call check(nb%domain(l)==s,'lambda domain memo')
  p=nb%mk_pi(10,0,s,1,18); l2=nb%mk_pi(99,3,s,1,18)
  call check(p==l2 .and. p/=l,'pi interning separate from lambda')
  t=nb%mk_thunk_keyed(-8,100,17); t2=nb%mk_thunk_keyed(-8,200,17)
  call check(t==t2 .and. nb%env(t)==100,'thunk keeps original env while sharing read key')
  call nb%set_forced(t,s)
  call check(nb%forced(t2)==s,'shared thunk sees forced memo')
  v=nb%mk_thunk_keyed(-9,100,17)
  call check(v/=t,'different read view separates thunks')
  v=nb%mk_unfold(11,1,spine_empty)
  call nb%set_forced(v,s)
  call check(nb%forced(v)==s,'unfold memo')
  l2=nb%mk_unfold(11,1,spine_empty)
  call check(l2==v,'unfold interning ignores forced cell')
  h=nb%mk_head(hconst,caxiom,11,1); h2=nb%mk_head(hconst,cctor,11,1)
  r=nb%mk_rigid(h,spine_empty); r2=nb%mk_rigid(h2,spine_empty)
  call check(r/=r2 .and. r/=v,'constant kinds and unfolded values stay distinct')
  call nb%set_forced(r,s)
  call check(nb%forced(r)==0,'rigid nodes have no forced memo')
  p=spine_empty
  do i=1,size(ids)
    ids(i)=nb%mk_sort(i+1)
    p=nb%spines%snoc(p,elim_app,ids(i),0)
  end do
  do i=1,size(ids)
    call check(nb%mk_sort(i+1)==ids(i),'value interning across growth')
    node=nb%spines%get(p,i-1)
    call check(nb%spines%a(node)==ids(i),'spine application ordering')
  end do
  node=nb%spines%snoc(p,elim_proj,42,3)
  l2=nb%spines%get(node,size(ids))
  call check(nb%spines%tag(l2)==elim_proj .and. nb%spines%a(l2)==42 .and. nb%spines%b(l2)==3,'spine projection payload')
  call check(nb%spines%get(node,size(ids)+1)==0,'spine out of range')
  call check(nb%spines%snoc(p,elim_proj,42,3)==node,'spine projection interning')
  call check(nb%forced(t)==s .and. nb%domain(l)==s,'memo cells survive growth')
  call nb%conv_pos%put(s,t,0,0,1)
  call nb%unfold_cache%put(11,1,0,0,s)
  nb%probe_depth=1; nb%probe_fuel=99; nb%probe_escalate=8192; nb%probe_aborted=.true.
  call nb%reset_decl()
  call check(nb%count==0 .and. nb%head_count==0 .and. nb%spines%count==1,'reset graph arenas')
  call check(nb%conv_pos%count==0 .and. nb%unfold_cache%count==0,'reset references in memo tables')
  call check(nb%probe_depth==0 .and. nb%probe_fuel==0 .and. .not.nb%probe_aborted,'reset probe state')
  t=nb%mk_thunk_keyed(-1,1,1)
  call check(nb%tag(t)==vthunk .and. nb%forced(t)==0,'reused arena slot clears stale memo')
  print '(a,i0,a)', 'value graph: ',checks,' checks passed'
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
end program
