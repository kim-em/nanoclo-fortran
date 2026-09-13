! Translation of uses_mask/uses_undense/join_uses/under_uses in nanoclo
! closure.rs. Wide sets use eight parallel word arrays, and fixed-size stack
! temporaries replace Rust's temporary Vec allocations.
module read_sets
  use base
  use hash_table
  use expr, only: expr_arena,evar,eapp,elam,epi,elet,eproj
  implicit none
  integer, parameter :: uses_dense=1,uses_mask_kind=2,uses_wide=3,uses_deep=4,max_uses_words=8
  type :: uses_t
    integer(int32) :: tag=uses_mask_kind,wide=0
    integer(int64) :: mask=0
  end type
  type :: read_set_arena
    integer(int32), allocatable :: cache_tag(:),cache_lo(:),cache_hi(:),cache_wide(:)
    integer(int32), allocatable :: wide_len(:),wide_next(:)
    integer(int32), allocatable :: w1lo(:),w1hi(:),w2lo(:),w2hi(:),w3lo(:),w3hi(:),w4lo(:),w4hi(:), &
      w5lo(:),w5hi(:),w6lo(:),w6hi(:),w7lo(:),w7hi(:),w8lo(:),w8hi(:)
    integer :: wide_count=0,cache_count=0
    type(table_t) :: wide_intern,cache_index
  contains
    procedure :: uses_mask
    procedure :: uses_undense
    procedure :: intern_uses
    procedure :: join_uses
    procedure :: under_uses
    procedure :: words
    procedure :: reset_const_caches
  end type
contains
  pure integer(int32) function low_word(m) result(lo)
    integer(int64), intent(in) :: m
    lo=int(iand(m,2147483647_int64),int32)
    if (btest(m,31)) lo=ibset(lo,31)
  end function
  pure integer(int64) function pair_word(lo,hi) result(m)
    integer(int32), intent(in) :: lo,hi
    m=ior(iand(int(lo,int64),4294967295_int64),shiftl(int(hi,int64),32))
  end function
  pure integer(int64) function full_mask(n) result(m)
    integer, intent(in) :: n
    m=shiftr(not(0_int64),64-n)
  end function
  subroutine reset_const_caches(self)
    class(read_set_arena), intent(inout) :: self
    call self%cache_index%clear(); self%cache_count=0
    self%wide_count=0
    call self%wide_intern%clear()
  end subroutine
  function words(self,u) result(w)
    class(read_set_arena), intent(in) :: self
    type(uses_t), intent(in) :: u
    integer(int64) :: w(8)
    integer :: id
    w=0
    if (u%tag==uses_mask_kind) then
      w(1)=u%mask
    else if (u%tag==uses_wide) then
      id=u%wide
      w=[pair_word(self%w1lo(id),self%w1hi(id)),pair_word(self%w2lo(id),self%w2hi(id)), &
        pair_word(self%w3lo(id),self%w3hi(id)),pair_word(self%w4lo(id),self%w4hi(id)), &
        pair_word(self%w5lo(id),self%w5hi(id)),pair_word(self%w6lo(id),self%w6hi(id)), &
        pair_word(self%w7lo(id),self%w7hi(id)),pair_word(self%w8lo(id),self%w8hi(id))]
    else
      error stop kernel_error
    end if
  end function
  function intern_uses(self,input) result(u)
    class(read_set_arena), intent(inout) :: self
    integer(int64), intent(in) :: input(:)
    type(uses_t) :: u,old
    integer(int64) :: w(8),previous(8)
    integer :: n,i,h,head,id
    n=size(input)
    do while(n>1)
      if (input(n)/=0) exit
      n=n-1
    end do
    u=uses_t()
    if (n==0) return
    if (n==1) then
      u%mask=input(1); return
    end if
    if (n>max_uses_words) then
      u%tag=uses_deep; return
    end if
    w=0; w(:n)=input(:n); h=0
    do i=1,n
      h=hash_words(h,low_word(w(i)),low_word(shiftr(w(i),32)),0)
    end do
    head=self%wide_intern%get(h,n,0,0); id=head
    do while(id/=0)
      old=uses_t(uses_wide,id,0_int64); previous=self%words(old)
      if (all(previous==w)) then
        u=old; return
      end if
      id=self%wide_next(id)
    end do
    id=self%wide_count+1
    call grow(self%wide_len,id); call grow(self%wide_next,id)
    call grow(self%w1lo,id); call grow(self%w1hi,id)
    self%w1lo(id)=low_word(w(1)); self%w1hi(id)=low_word(shiftr(w(1),32))
    call grow(self%w2lo,id); call grow(self%w2hi,id)
    self%w2lo(id)=low_word(w(2)); self%w2hi(id)=low_word(shiftr(w(2),32))
    call grow(self%w3lo,id); call grow(self%w3hi,id)
    self%w3lo(id)=low_word(w(3)); self%w3hi(id)=low_word(shiftr(w(3),32))
    call grow(self%w4lo,id); call grow(self%w4hi,id)
    self%w4lo(id)=low_word(w(4)); self%w4hi(id)=low_word(shiftr(w(4),32))
    call grow(self%w5lo,id); call grow(self%w5hi,id)
    self%w5lo(id)=low_word(w(5)); self%w5hi(id)=low_word(shiftr(w(5),32))
    call grow(self%w6lo,id); call grow(self%w6hi,id)
    self%w6lo(id)=low_word(w(6)); self%w6hi(id)=low_word(shiftr(w(6),32))
    call grow(self%w7lo,id); call grow(self%w7hi,id)
    self%w7lo(id)=low_word(w(7)); self%w7hi(id)=low_word(shiftr(w(7),32))
    call grow(self%w8lo,id); call grow(self%w8hi,id)
    self%w8lo(id)=low_word(w(8)); self%w8hi(id)=low_word(shiftr(w(8),32))
    self%wide_len(id)=n; self%wide_next(id)=head; self%wide_count=id
    call self%wide_intern%put(h,n,0,0,id)
    u=uses_t(uses_wide,id,0_int64)
  end function
  function join_uses(self,a,b) result(u)
    class(read_set_arena), intent(inout) :: self
    type(uses_t), intent(in) :: a,b
    type(uses_t) :: u
    integer(int64) :: x(8),y(8)
    if (a%tag==uses_mask_kind .and. b%tag==uses_mask_kind) then
      u=uses_t(uses_mask_kind,0,ior(a%mask,b%mask))
    else if (a%tag==uses_deep .or. b%tag==uses_deep) then
      u=uses_t(uses_deep,0,0_int64)
    else if (a%tag==uses_dense .or. b%tag==uses_dense) then
      error stop kernel_error
    else if (a%tag==uses_wide .and. b%tag==uses_wide .and. a%wide==b%wide) then
      u=a
    else
      x=self%words(a); y=self%words(b)
      u=self%intern_uses(ior(x,y))
    end if
  end function
  function under_uses(self,a) result(u)
    class(read_set_arena), intent(inout) :: self
    type(uses_t), intent(in) :: a
    type(uses_t) :: u
    integer(int64) :: w(8)
    integer :: i
    select case(a%tag)
    case(uses_mask_kind)
      u=uses_t(uses_mask_kind,0,shiftr(a%mask,1))
    case(uses_deep)
      u=a
    case(uses_wide)
      w=self%words(a)
      do i=1,7
        w(i)=ior(shiftr(w(i),1),shiftl(w(i+1),63))
      end do
      w(8)=shiftr(w(8),1)
      u=self%intern_uses(w)
    case default
      error stop kernel_error
    end select
  end function
  recursive function uses_undense(self,dag,e) result(u)
    class(read_set_arena), intent(inout) :: self
    type(expr_arena), intent(in) :: dag
    integer(int32), intent(in) :: e
    type(uses_t) :: u
    integer(int64) :: w(8)
    integer :: n,i
    u=self%uses_mask(dag,e)
    if (u%tag/=uses_dense) return
    n=dag%get_nlbv(e)
    if (n<=64) then
      u=uses_t(uses_mask_kind,0,full_mask(n)); return
    end if
    if (n>512) then
      u=uses_t(uses_deep,0,0_int64); return
    end if
    w=0
    do i=1,(n+63)/64
      w(i)=full_mask(min(64,n-64*(i-1)))
    end do
    u=self%intern_uses(w)
  end function
  recursive function uses_mask(self,dag,e) result(u)
    class(read_set_arena), intent(inout) :: self
    type(expr_arena), intent(in) :: dag
    integer(int32), intent(in) :: e
    type(uses_t) :: u,a,b,c
    integer(int64) :: w(8)
    integer :: n,i,idx
    logical :: full
    u=uses_t(); n=dag%get_nlbv(e)
    if (n==0) return
    idx=self%cache_index%get(e,0,0,0)
    if (idx/=0) then
      u=uses_t(self%cache_tag(idx),self%cache_wide(idx),pair_word(self%cache_lo(idx),self%cache_hi(idx))); return
    end if
    select case(dag%get_tag(e))
    case(evar)
      idx=dag%get_a(e)
      if (idx<64) then
        u%mask=ibset(0_int64,idx)
      else if (idx<512) then
        w=0; w(idx/64+1)=ibset(0_int64,mod(idx,64)); u=self%intern_uses(w)
      else
        u%tag=uses_deep
      end if
    case(eapp)
      a=self%uses_undense(dag,dag%get_a(e)); b=self%uses_undense(dag,dag%get_b(e)); u=self%join_uses(a,b)
    case(elam,epi)
      a=self%uses_undense(dag,dag%get_c(e)); b=self%uses_undense(dag,dag%get_d(e))
      b=self%under_uses(b); u=self%join_uses(a,b)
    case(elet)
      a=self%uses_undense(dag,dag%get_c(e)); b=self%uses_undense(dag,dag%get_e(e))
      c=self%uses_undense(dag,dag%get_d(e)); c=self%under_uses(c)
      a=self%join_uses(a,b); u=self%join_uses(a,c)
    case(eproj)
      u=self%uses_undense(dag,dag%get_c(e))
    end select
    if (u%tag==uses_mask_kind .and. n<=64) then
      if (u%mask==full_mask(n)) u=uses_t(uses_dense,0,0_int64)
    else if (u%tag==uses_wide) then
      w=self%words(u); full=self%wide_len(u%wide)==(n+63)/64
      if (full) then
        do i=1,self%wide_len(u%wide)
          if (w(i)/=full_mask(min(64,n-64*(i-1)))) full=.false.
        end do
      end if
      if (full) u=uses_t(uses_dense,0,0_int64)
    end if
    idx=self%cache_count+1; self%cache_count=idx
    call self%cache_index%put(e,0,0,0,idx)
    call grow(self%cache_tag,idx); call grow(self%cache_lo,idx); call grow(self%cache_hi,idx); call grow(self%cache_wide,idx)
    self%cache_tag(idx)=u%tag; self%cache_lo(idx)=low_word(u%mask)
    self%cache_hi(idx)=low_word(shiftr(u%mask,32)); self%cache_wide(idx)=u%wide
  end function
end module
