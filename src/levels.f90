! Fortran translation of nanoclo src/level.rs. Decision order, simplification,
! and imax case splitting follow commit 4cdd12f; storage is struct-of-arrays.
module levels
  use base
  use hash_table
  use sequences, only: sequence_arena,list_nil
  implicit none
  integer(int32), parameter :: lzero=0, lsucc=1, lmax=2, limax=3, lparam=4, zero=1
  type :: level_arena
    integer(int32), allocatable :: tag(:),a(:),b(:),c(:)
    integer(int32) :: count=0
    type(table_t) :: nodes, simplify_cache
  contains
    procedure :: init => levels_init
    procedure :: intern => level_intern
    procedure :: succ
    procedure :: max => level_max
    procedure :: imax
    procedure :: param
    procedure :: level_n
    procedure :: level_succs
    procedure :: simplify
    procedure :: combining
    procedure :: subst_level_ids
    procedure :: subst_levels_ids
    procedure :: subst_level
    procedure :: all_uparams_defined
    procedure :: no_dupes_all_params
    procedure :: leq
    procedure :: leq_core
    procedure :: leq_imax_by_cases
    procedure :: eq_antisymm
    procedure :: eq_antisymm_many
    procedure :: is_zero
    procedure :: is_one
    procedure :: is_nonzero
    procedure :: is_never_zero
  end type
contains
  subroutine levels_init(self)
    class(level_arena), intent(inout) :: self
    integer :: id
    id=self%intern(lzero,0,0,0)
  end subroutine
  function level_intern(self,tag,a,b,c) result(id)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: tag,a,b,c
    integer(int32) :: id
    include 'intern_node.inc'
  end function
  function succ(self,a) result(id)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: a
    integer(int32) :: id
    id=self%intern(lsucc,a,0,0)
  end function
  function level_max(self,a,b) result(id)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: a,b
    integer(int32) :: id
    id=self%intern(lmax,a,b,0)
  end function
  function imax(self,a,b) result(id)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: a,b
    integer(int32) :: id
    id=self%intern(limax,a,b,0)
  end function
  function param(self,name) result(id)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: name
    integer(int32) :: id
    id=self%intern(lparam,name,0,0)
  end function
  function level_n(self,base,n) result(id)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: base,n
    integer(int32) :: id,i
    id=base
    do i=1,n
      id=self%succ(id)
    end do
  end function
  subroutine level_succs(self,l,base,n)
    class(level_arena), intent(in) :: self
    integer(int32), intent(in) :: l
    integer(int32), intent(out) :: base,n
    base=l; n=0
    do while (self%tag(base)==lsucc)
      base=self%a(base); n=n+1
    end do
  end subroutine
  recursive function combining(self,l,r) result(id)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: l,r
    integer(int32) :: id,a,b
    if (self%tag(l)==lzero) then
      id=r
    else if (self%tag(r)==lzero) then
      id=l
    else if (self%tag(l)==lsucc .and. self%tag(r)==lsucc) then
      a=self%a(l); b=self%a(r)
      id=self%combining(a,b)
      id=self%succ(id)
    else
      id=self%max(l,r)
    end if
  end function
  recursive function simplify(self,l) result(id)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: l
    integer(int32) :: id,a,b,t
    id=self%simplify_cache%get(l,0,0,0)
    if (id/=0) return
    t=self%tag(l); a=self%a(l); b=self%b(l)
    select case(t)
    case(lzero,lparam)
      id=l
    case(lsucc)
      a=self%simplify(a); id=self%succ(a)
    case(lmax)
      a=self%simplify(a); b=self%simplify(b); id=self%combining(a,b)
    case(limax)
      a=self%simplify(a); b=self%simplify(b)
      if (self%is_zero(a)) then
        id=b
      else if (self%is_one(a)) then
        id=b
      else
        select case(self%tag(b))
        case(lzero)
          id=b
        case(lsucc)
          id=self%combining(a,b)
        case default
          id=self%imax(a,b)
        end select
      end if
    case default
      error stop kernel_error
    end select
    call self%simplify_cache%put(l,0,0,0,id)
  end function
  recursive function subst_level(self,l,ks,vs) result(id)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: l,ks(:),vs(:)
    integer(int32) :: id,t,a,b,i
    t=self%tag(l); a=self%a(l); b=self%b(l)
    select case(t)
    case(lzero)
      id=zero
    case(lsucc)
      a=self%subst_level(a,ks,vs); id=self%succ(a)
    case(lmax,limax)
      a=self%subst_level(a,ks,vs); b=self%subst_level(b,ks,vs)
      id=self%intern(t,a,b,0)
    case(lparam)
      id=l
      do i=1,min(size(ks),size(vs))
        if (l==ks(i)) then
          id=vs(i); return
        end if
      end do
    case default
      error stop kernel_error
    end select
  end function
  recursive logical function all_uparams_defined(self,l,params) result(ok)
    class(level_arena), intent(in) :: self
    integer(int32), intent(in) :: l,params(:)
    select case(self%tag(l))
    case(lzero)
      ok=.true.
    case(lsucc)
      ok=self%all_uparams_defined(self%a(l),params)
    case(lmax,limax)
      ok=self%all_uparams_defined(self%a(l),params)
      if (ok) ok=self%all_uparams_defined(self%b(l),params)
    case(lparam)
      ok=any(params==l)
    case default
      error stop kernel_error
    end select
  end function
  logical function no_dupes_all_params(self,ls) result(ok)
    class(level_arena), intent(in) :: self
    integer(int32), intent(in) :: ls(:)
    integer :: i
    ok=.false.
    do i=1,size(ls)
      if (self%tag(ls(i))/=lparam) return
      if (any(ls(:i-1)==ls(i))) return
    end do
    ok=.true.
  end function
  recursive logical function leq_imax_by_cases(self,p,l,r,diff) result(ok)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: p,l,r
    integer(int64), intent(in) :: diff
    integer(int32) :: s,l0,r0,ls,rs
    s=self%succ(p)
    l0=self%subst_level(l,[p],[zero]); l0=self%simplify(l0)
    r0=self%subst_level(r,[p],[zero]); r0=self%simplify(r0)
    ls=self%subst_level(l,[p],[s]); ls=self%simplify(ls)
    rs=self%subst_level(r,[p],[s]); rs=self%simplify(rs)
    ok=self%leq_core(l0,r0,diff)
    if (ok) ok=self%leq_core(ls,rs,diff)
  end function
  recursive logical function leq_core(self,l,r,diff) result(ok)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: l,r
    integer(int64), intent(in) :: diff
    integer(int32) :: lt,rt,a,b,x,y,j,k,u,v,m
    lt=self%tag(l); rt=self%tag(r)
    a=self%a(l); b=self%b(l); x=self%a(r); y=self%b(r)
    if (lt==lzero .and. diff>=0) then
      ok=.true.
    else if (rt==lzero .and. diff<0) then
      ok=.false.
    else if (lt==lparam .and. rt==lparam) then
      ok=a==x .and. diff>=0
    else if (lt==lparam .and. rt==lzero) then
      ok=.false.
    else if (lt==lzero .and. rt==lparam) then
      ok=diff>=0
    else if (lt==lsucc) then
      ok=self%leq_core(a,r,diff-1)
    else if (rt==lsucc) then
      ok=self%leq_core(l,x,diff+1)
    else if (lt==lmax) then
      ok=self%leq_core(a,r,diff)
      if (ok) ok=self%leq_core(b,r,diff)
    else if ((lt==lparam .or. lt==lzero) .and. rt==lmax) then
      ok=self%leq_core(l,x,diff)
      if (.not.ok) ok=self%leq_core(l,y,diff)
    else if (lt==limax .and. rt==limax .and. a==x .and. b==y .and. diff>=0) then
      ok=.true.
    else
      ! Fortran does not short circuit: test node kind before indexing b/y.
      if (lt==limax) then
        if (self%tag(b)==lparam) then
          ok=self%leq_imax_by_cases(b,l,r,diff); return
        end if
      end if
      if (rt==limax) then
        if (self%tag(y)==lparam) then
          ok=self%leq_imax_by_cases(y,l,r,diff); return
        end if
      end if
      if (lt==limax) then
        if (self%tag(b)==limax .or. self%tag(b)==lmax) then
          j=self%a(b); k=self%b(b)
          if (self%tag(b)==limax) then
            u=self%imax(a,k); v=self%imax(j,k); m=self%max(u,v)
          else
            u=self%imax(a,j); v=self%imax(a,k); m=self%max(u,v); m=self%simplify(m)
          end if
          ok=self%leq_core(m,r,diff); return
        end if
      end if
      if (rt==limax) then
        if (self%tag(y)==limax .or. self%tag(y)==lmax) then
          j=self%a(y); k=self%b(y)
          if (self%tag(y)==limax) then
            u=self%imax(x,k); v=self%imax(j,k); m=self%max(u,v)
          else
            u=self%imax(x,j); v=self%imax(x,k); m=self%max(u,v); m=self%simplify(m)
          end if
          ok=self%leq_core(l,m,diff); return
        end if
      end if
      error stop kernel_error
    end if
  end function
  recursive logical function leq(self,l,r) result(ok)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: l,r
    integer(int32) :: a,b
    ok=.true.
    if (l==r) return
    a=self%simplify(l); b=self%simplify(r)
    ok=self%leq_core(a,b,0_int64)
  end function
  logical function eq_antisymm(self,l,r) result(ok)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: l,r
    ok=.true.
    if (l==r) return
    ok=self%leq(l,r)
    if (ok) ok=self%leq(r,l)
  end function
  logical function eq_antisymm_many(self,xs,ys) result(ok)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: xs(:),ys(:)
    integer :: i
    ok=.false.
    if (size(xs)/=size(ys)) return
    do i=1,size(xs)
      if (.not.self%eq_antisymm(xs(i),ys(i))) return
    end do
    ok=.true.
  end function
  recursive logical function is_zero(self,l) result(ok)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: l
    ok=self%leq(l,zero)
  end function
  recursive logical function is_one(self,l) result(ok)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: l
    integer(int32) :: a
    ok=.false.
    if (self%tag(l)/=lsucc) return
    a=self%a(l); ok=self%is_zero(a)
  end function
  logical function is_nonzero(self,l) result(ok)
    class(level_arena), intent(inout) :: self
    integer(int32), intent(in) :: l
    integer(int32) :: one
    one=self%succ(zero); ok=self%leq(one,l)
  end function
  pure recursive logical function is_never_zero(self,l) result(ok)
    class(level_arena), intent(in) :: self
    integer(int32), intent(in) :: l
    select case(self%tag(l))
    case(lzero,lparam)
      ok=.false.
    case(lsucc)
      ok=.true.
    case(lmax)
      ok=self%is_never_zero(self%a(l)) .or. self%is_never_zero(self%b(l))
    case(limax)
      ok=self%is_never_zero(self%b(l))
    case default
      error stop kernel_error
    end select
  end function
  recursive function subst_level_ids(self,lists,l,ks,vs) result(id)
    class(level_arena), intent(inout) :: self
    type(sequence_arena), intent(in) :: lists
    integer(int32), intent(in) :: l,ks,vs
    integer(int32) :: id,t,a,b,k,v
    t=self%tag(l); a=self%a(l); b=self%b(l)
    select case(t)
    case(lzero)
      id=zero
    case(lsucc)
      a=self%subst_level_ids(lists,a,ks,vs); id=self%succ(a)
    case(lmax,limax)
      a=self%subst_level_ids(lists,a,ks,vs); b=self%subst_level_ids(lists,b,ks,vs)
      id=self%intern(t,a,b,0)
    case(lparam)
      id=l; k=ks; v=vs
      do while(k/=list_nil .and. v/=list_nil)
        if (l==lists%head(k)) then
          id=lists%head(v); return
        end if
        k=lists%tail(k); v=lists%tail(v)
      end do
    case default
      error stop kernel_error
    end select
  end function
  recursive function subst_levels_ids(self,lists,ls,ks,vs) result(id)
    class(level_arena), intent(inout) :: self
    type(sequence_arena), intent(inout) :: lists
    integer(int32), intent(in) :: ls,ks,vs
    integer(int32) :: id,h,t
    id=list_nil
    if (ls==list_nil) return
    h=lists%head(ls); t=lists%tail(ls)
    h=self%subst_level_ids(lists,h,ks,vs)
    t=self%subst_levels_ids(lists,t,ks,vs)
    id=lists%cons(h,t)
  end function
end module
