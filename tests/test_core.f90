! Translated semantic tests from nanoclo src/tests/level.rs, plus storage tests.
program test_core
  use levels
  use names
  implicit none
  type(level_arena) :: l
  type(name_arena) :: names_dag
  type(table_t) :: table,moved
  integer :: i,s,ss,m,sm,a,b,p,q,im,sim,ssim,lhs,rhs,small,large,u,v,w,base_id,n
  integer :: ps,qs,pl,ql,checks=0
  integer :: generated(350),seed=193,tag,ix,iy,original_count
  logical :: comparison,oracle
  call l%init()
  call names_dag%init()
  call check(l%count==1 .and. names_dag%count==1,'root indices')
  a=names_dag%str(anonymous,14)
  b=names_dag%num(a,17)
  call check(names_dag%str(anonymous,14)==a,'name interning')
  call check(names_dag%get_pfx(b)==a,'get_pfx')
  lhs=names_dag%replace_pfx(b,a,anonymous); rhs=names_dag%num(anonymous,17)
  call check(lhs==rhs,'replace_pfx')
  call check(names_dag%concat(a,anonymous)==a,'concat anonymous')
  do i=1,20000
    call table%put(i,-i,mod(i,7),0,i+1)
  end do
  do i=1,20000
    call check(table%get(i,-i,mod(i,7),0)==i+1,'table resize/collisions')
  end do
  call table%put(1,-1,1,0,99)
  call check(table%count==20000 .and. table%get(1,-1,1,0)==99,'table replace')
  call check(table%get(20001,-20001,0,0)==0,'table missing')
  call table%clear()
  call check(table%get(1,-1,1,0)==0 .and. table%count==0,'table clear')
  call table%put(7,8,9,10,11)
  call moved%put(1,2,3,4,5)
  call table%move_to(moved)
  call check(moved%get(7,8,9,10)==11 .and. moved%get(1,2,3,4)==0,'table ownership transfer')
  call check(table%get(7,8,9,10)==0 .and. table%count==0,'moved-from table empty')
  call table%put(1,2,3,4,5)
  call check(table%get(1,2,3,4)==5 .and. moved%get(7,8,9,10)==11,'moved-from table reuse')
  call table%put(huge(1),not(huge(1)),-1,0,17)
  call table%put(not(huge(1)),huge(1),0,-1,18)
  call check(table%get(huge(1),not(huge(1)),-1,0)==17,'full-width signed hash keys')
  call check(table%get(not(huge(1)),huge(1),0,-1)==18,'distinct full-width hash keys')
  s=l%succ(zero); ss=l%succ(s); m=l%max(s,s); sm=l%succ(m)
  call check(l%leq(s,m),'leq_test0 forward')
  call check(l%leq(m,s),'leq_test0 reverse')
  call check(l%eq_antisymm(s,m),'leq_test0 equality')
  im=l%imax(ss,zero)
  call check(l%leq(im,zero),'leq_test1')
  call check(l%eq_antisymm(zero,im),'leq_test1 equality')
  p=l%param(2); q=l%param(3)
  call check(.not.l%leq(p,q),'leq_test2/3 forward')
  call check(.not.l%leq(q,p),'leq_test2/3 reverse')
  im=l%imax(p,q); sim=l%succ(im); ssim=l%succ(sim)
  call check(l%leq(im,im),'imax reflexivity')
  call check(l%leq(im,sim),'imax succ')
  call check(l%leq(im,ssim),'imax succ succ')
  call check(l%leq(sim,ssim),'imax succ monotone')
  call check(.not.l%leq(ssim,im),'imax strict 1')
  call check(.not.l%leq(ssim,sim),'imax strict 2')
  do i=1,100
    u=next_random(256); v=next_random(256); w=next_random(256)
    small=min(u,v); large=max(u,v)
    a=l%level_n(p,small); b=l%level_n(p,large)
    call check(l%leq(a,b),'leq_test4')
    ps=l%level_n(p,small); qs=l%level_n(q,small)
    pl=l%level_n(p,large); ql=l%level_n(q,large)
    lhs=l%max(ps,qs); lhs=l%level_n(lhs,small)
    rhs=l%max(pl,ql); rhs=l%level_n(rhs,large)
    call check(l%leq(lhs,rhs),'leq_test5')
    lhs=l%imax(ps,qs); lhs=l%level_n(lhs,small)
    rhs=l%imax(pl,ql); rhs=l%level_n(rhs,large)
    call check(l%leq(lhs,rhs),'leq_test6')
    a=l%level_n(p,u); b=l%level_n(q,v+1)
    lhs=l%imax(a,b); lhs=l%level_n(lhs,w)
    rhs=l%max(a,b); rhs=l%level_n(rhs,w)
    call check(l%eq_antisymm(lhs,rhs),'leq_test7')
  end do
  call check(l%eq_antisymm(ss,sm),'eq_test1')
  call check(l%eq_antisymm_many([ss],[sm]),'eq_many_test1')
  call check(.not.l%eq_antisymm_many([ss],[sm,sm]),'eq_many different lengths')
  call l%level_succs(ss,base_id,n)
  call check(base_id==zero .and. n==2,'debug_test0 successors')
  call l%level_succs(sm,base_id,n)
  call check(base_id==m .and. n==1,'debug_test1 successors')
  call check(l%no_dupes_all_params([p,q]),'parameter list')
  call check(.not.l%no_dupes_all_params([p,p]),'duplicate parameter')
  call check(.not.l%no_dupes_all_params([zero,p]),'non parameter')
  im=l%imax(p,q)
  call check(l%all_uparams_defined(im,[p,q]),'defined parameters')
  call check(.not.l%all_uparams_defined(im,[p]),'undefined parameter')
  a=l%subst_level(im,[p,q],[ss,zero])
  call check(l%is_zero(a),'substitution')
  call check(l%is_nonzero(ss),'nonzero')
  call check(.not.l%is_never_zero(im),'may be prop')
  ! Independent numeric semantics exercise nested max/imax branches, with
  ! valuations exceeding the maximum syntactic successor depth in this DAG.
  generated(1:3)=[zero,p,q]
  do i=4,size(generated)
    tag=next_random(3)+1
    ix=next_random(i-1)+1; iy=next_random(i-1)+1
    generated(i)=l%intern(tag,generated(ix),merge(generated(iy),0,tag/=lsucc),0)
  end do
  original_count=l%count
  do i=1,1500
    a=generated(next_random(size(generated))+1); b=generated(next_random(size(generated))+1)
    comparison=l%leq(a,b)
    oracle=.true.
    do u=0,12
      do v=0,12
        if (evaluate(a,u,v)>evaluate(b,u,v)) oracle=.false.
      end do
    end do
    call check(comparison.eqv.oracle,'leq numeric oracle')
  end do
  call check(l%count>=original_count,'generated comparisons')
  print '(a,i0,a)', 'core: ',checks,' checks passed'
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
  function next_random(bound) result(value)
    integer, intent(in) :: bound
    integer :: value
    seed=mod(seed*251+17,65521)
    value=mod(seed,bound)
  end function
  recursive function evaluate(id,pvalue,qvalue) result(value)
    integer, intent(in) :: id,pvalue,qvalue
    integer :: value,left,right
    select case(l%tag(id))
    case(lzero)
      value=0
    case(lparam)
      value=merge(pvalue,qvalue,l%a(id)==2)
    case(lsucc)
      value=evaluate(l%a(id),pvalue,qvalue)+1
    case(lmax,limax)
      left=evaluate(l%a(id),pvalue,qvalue); right=evaluate(l%b(id),pvalue,qvalue)
      value=max(left,right)
      if (l%tag(id)==limax .and. right==0) value=0
    case default
      error stop 1
    end select
  end function
end program
