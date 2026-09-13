! Nat primitive order, deferral, literal recursors and successor handling from nanoclo.
  integer function nat_op(self,n) result(op)
    class(checker), intent(in) :: self
    integer, intent(in) :: n
    integer :: i
    op=0
    do i=1,15
      if (n==self%nat_ops(i)) then
        op=i; return
      end if
    end do
  end function
  recursive integer function nb_nat_red(self,depth,n,s,deep) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,n,s
    logical, intent(in) :: deep
    integer :: mark,args,count,op,x,y,a,b,z,r,h,bname
    integer(int64) :: amount
    out=0; mark=self%work_top; args=self%spine_args(s)
    if (args<0) return
    reduction: block
      count=self%nb%spines%len(s); op=self%nat_op(n)
      if (op==1 .and. count==1) then
        a=self%w1(args+1); x=self%nb_bignum(depth,a,deep)
        if (x==0 .or. self%status%code/=accept) exit reduction
        z=self%ctx%bignums%add(x,self%big_one); out=self%nb%mk_nat(z); exit reduction
      end if
      if (count/=2 .or. op<=1) exit reduction
      b=self%w1(args+2); y=self%nb_bignum(depth,b,deep)
      if (y==0 .or. self%status%code/=accept) exit reduction
      amount=self%ctx%bignums%small(y)
      if (op==5 .or. op==14) then
        if (amount<0 .or. amount>16777216) exit reduction
      end if
      a=self%w1(args+1); x=self%nb_bignum(depth,a,deep)
      if (x==0 .or. self%status%code/=accept) exit reduction
      select case(op)
      case(2)
        z=self%ctx%bignums%add(x,y)
      case(3)
        z=self%ctx%bignums%sub(x,y)
      case(4)
        z=self%ctx%bignums%mul(x,y)
      case(5)
        z=self%ctx%bignums%power(x,int(amount))
      case(6,7)
        call self%ctx%bignums%divmod(x,y,z,r)
        if (op==6) z=r
      case(8,9)
        if (op==8) then
          bname=merge(self%bool_true,self%bool_false,x==y)
        else
          bname=merge(self%bool_true,self%bool_false,self%ctx%bignums%compare(x,y)<=0)
        end if
        if (bname==0) exit reduction
        h=self%nb%mk_head(hconst,cctor,bname,list_nil); out=self%nb%mk_rigid(h,spine_empty); exit reduction
      case(10,11,12)
        z=self%ctx%bignums%bitop(op-9,x,y)
      case(13)
        z=self%ctx%bignums%gcd(x,y)
      case(14,15)
        z=self%ctx%bignums%shift(x,amount,op==14)
      case default
        exit reduction
      end select
      out=self%nb%mk_nat(z)
    end block reduction
    self%work_top=mark
  end function
  recursive logical function nb_nat_defer(self,depth,n,s) result(yes)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,n,s
    integer :: op,node,a,p
    yes=.false.; op=self%nat_op(n)
    if (op<2 .or. op>5 .or. self%nb%spines%len(s)/=2) return
    node=self%nb%spines%get(s,0)
    if (self%nb%spines%tag(node)/=elim_app) return
    node=self%nb%spines%get(s,1)
    if (self%nb%spines%tag(node)/=elim_app) return
    a=self%nb%spines%a(node); a=self%nb_force(depth,a)
    if (self%status%code/=accept) return
    if (self%nb%tag(a)==vnat) then
      p=self%nb%literal(a); yes=self%ctx%bignums%bits(p)>8
    end if
  end function
  recursive integer function nb_bignum(self,depth,v,deep) result(num)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,v
    logical, intent(in) :: deep
    integer :: cur,h,n,s,node,a,next,p
    integer(int64) :: succs
    num=0; succs=0; cur=self%nb_force(depth,v)
    do while(self%status%code==accept)
      select case(self%nb%tag(cur))
      case(vnat)
        p=self%ctx%bignums%from_u64(succs); n=self%nb%literal(cur); num=self%ctx%bignums%add(n,p); return
      case(vrigid)
        h=self%nb%head(cur); s=self%nb%spine(cur)
        if (self%nb%head_tag(h)/=hconst) return
        if (self%nb%head_a(h)==cctor) then
          n=self%nb%head_b(h)
          if (n==self%nat_zero .and. self%nb%spines%len(s)==0) then
            num=self%ctx%bignums%from_u64(succs); return
          end if
          if (n==self%nat_ops(1) .and. self%nb%spines%len(s)==1) then
            node=self%nb%spines%get(s,0)
            if (self%nb%spines%tag(node)==elim_app) then
              succs=succs+1; a=self%nb%spines%a(node); cur=self%nb_force(depth,a); cycle
            end if
          end if
          return
        end if
        if (.not.self%iota_head(h)) return
      case(vunfold)
        next=self%nb%forced(cur)
        if (next/=0 .and. next/=cur) then
          cur=next; cycle
        end if
      case default
        return
      end select
      if (.not.deep) return
      if (self%nb_is_open(depth,cur)) return
      next=self%nb_whnf(depth,cur)
      if (next==cur) return
      cur=next
    end do
  end function
  recursive logical function nb_is_open(self,depth,v) result(yes)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,v
    integer :: cur,cached,h,s,mark,node,i,a
    yes=.false.; cur=self%nb_force(depth,v)
    if (self%status%code/=accept) return
    cached=self%nb%open_cache%get(cur,0,0,0)
    if (cached/=0) then
      yes=cached==1; return
    end if
    if (self%nb%tag(cur)==vrigid) then
      h=self%nb%head(cur)
      yes=self%nb%head_tag(h)==hbvar .or. self%nb%head_tag(h)==hlocal
    end if
    if (.not.yes .and. (self%nb%tag(cur)==vrigid .or. self%nb%tag(cur)==vunfold)) then
      s=self%nb%spine(cur); mark=self%work_top; node=s
      do while(node/=spine_empty)
        i=self%work_slot(); self%w1(i)=node; node=self%nb%spines%parent(node)
      end do
      do i=self%work_top,mark+1,-1
        node=self%w1(i)
        if (self%nb%spines%tag(node)/=elim_app) cycle
        a=self%nb%spines%a(node); yes=self%nb_is_open(depth,a)
        if (yes .or. self%status%code/=accept) exit
      end do
      self%work_top=mark
    end if
    if (self%status%code==accept) call self%nb%open_cache%put(cur,0,0,0,merge(1,2,yes))
  end function
  recursive integer function nb_nat_to_ctor(self,n) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: n
    integer :: h,s,p,pv
    out=0
    if (.not.self%nat_extension) return
    if (n==1) then
      if (self%nat_zero==0) return
      h=self%nb%mk_head(hconst,cctor,self%nat_zero,list_nil); out=self%nb%mk_rigid(h,spine_empty)
    else
      if (self%nat_ops(1)==0) return
      p=self%ctx%bignums%sub(n,self%big_one); pv=self%nb%mk_nat(p)
      s=self%nb%spines%snoc(spine_empty,elim_app,pv,0); h=self%nb%mk_head(hconst,cctor,self%nat_ops(1),list_nil)
      out=self%nb%mk_rigid(h,s)
    end if
  end function
  recursive integer function nb_nat_rec(self,depth,rec,ls,args,nargs,n) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,rec,ls,args,nargs,n
    integer :: start,mi,idx,i,a,p,pv,succ_case,ih,h,step
    start=self%ctx%decls%num_params(rec); mi=self%ctx%decls%num_motives(rec); idx=self%major_idx(rec)
    if (n==1) then
      out=self%w1(args+start+mi+1)
    else
      p=self%ctx%bignums%sub(n,self%big_one); pv=self%nb%mk_nat(p)
      a=self%w1(args+start+mi+2); succ_case=self%nb_force(depth,a)
      h=self%nb%mk_head(hconst,crecursor,self%ctx%decls%name(rec),ls); ih=self%nb%mk_rigid(h,spine_empty)
      do i=1,idx
        a=self%w1(args+i); ih=self%nb_apply(depth,ih,a)
      end do
      ih=self%nb_apply(depth,ih,pv); step=self%nb_apply(depth,succ_case,pv); out=self%nb_apply(depth,step,ih)
    end if
    do i=idx+2,nargs
      a=self%w1(args+i); out=self%nb_apply(depth,out,a)
    end do
  end function
  logical function nb_may_be_nat(self,v) result(yes)
    class(checker), intent(in) :: self
    integer, intent(in) :: v
    integer :: h,n
    yes=self%nb%tag(v)==vnat
    if (self%nb%tag(v)/=vrigid) return
    h=self%nb%head(v)
    if (self%nb%head_tag(h)/=hconst .or. self%nb%head_a(h)/=cctor) return
    n=self%nb%head_b(h); yes=n==self%nat_zero .or. n==self%nat_ops(1)
  end function
  logical function nb_is_nat_zero(self,v) result(yes)
    class(checker), intent(in) :: self
    integer, intent(in) :: v
    integer :: h
    yes=.false.
    if (self%nb%tag(v)==vnat) then
      yes=self%nb%literal(v)==1
    else if (self%nb%tag(v)==vrigid) then
      h=self%nb%head(v)
      if (self%nb%head_tag(h)==hconst .and. self%nb%head_a(h)==cctor) &
        yes=self%nb%head_b(h)==self%nat_zero .and. self%nb%spine(v)==spine_empty
    end if
  end function
  integer function nb_nat_pred(self,v) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: v
    integer :: h,s,node,p
    out=0
    if (self%nb%tag(v)==vnat) then
      p=self%nb%literal(v)
      if (p==1) return
      p=self%ctx%bignums%sub(p,self%big_one); out=self%nb%mk_nat(p)
    else if (self%nb%tag(v)==vrigid) then
      h=self%nb%head(v); s=self%nb%spine(v)
      if (self%nb%head_tag(h)/=hconst .or. self%nb%head_a(h)/=cctor) return
      if (self%nb%head_b(h)/=self%nat_ops(1) .or. self%nb%spines%len(s)/=1) return
      node=self%nb%spines%get(s,0)
      if (self%nb%spines%tag(node)==elim_app) out=self%nb%spines%a(node)
    end if
  end function
  recursive integer function nb_conv_nat(self,rigid,depth,x,y) result(answer)
    class(checker), intent(inout) :: self
    logical, intent(in) :: rigid
    integer, intent(in) :: depth,x,y
    integer :: px,py
    logical :: ok
    answer=-1
    if (.not.self%nb_may_be_nat(x) .and. .not.self%nb_may_be_nat(y)) return
    if (self%nb%tag(x)==vnat .and. self%nb%tag(y)==vnat) return
    if (self%nb_is_nat_zero(x) .and. self%nb_is_nat_zero(y)) then
      answer=1; return
    end if
    px=self%nb_nat_pred(x); py=self%nb_nat_pred(y)
    if (px/=0 .and. py/=0) then
      ok=self%nb_unify(rigid,depth,px,py); answer=merge(1,0,ok)
    end if
  end function
