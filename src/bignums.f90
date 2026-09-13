! Canonical base-2^32 naturals. All limbs and references are int32 SoA indices;
! unsigned arithmetic uses bounded int64 intermediates, including debug builds.
module bignums
  use base
  use hash_table
  implicit none
  integer(int64), parameter :: limb_mask=4294967295_int64,radix=4294967296_int64,half_radix=65536_int64
  integer, parameter :: big_zero=1
  type :: bignum_arena
    integer(int32), allocatable :: limbs(:),start(:),length(:),next(:),work(:),du(:),dv(:),dq(:)
    integer :: count=0,used=0
    type(table_t) :: buckets
  contains
    procedure :: init
    procedure :: intern
    procedure :: from_decimal
    procedure :: from_u64
    procedure :: decimal
    procedure :: word
    procedure :: compare
    procedure :: bits
    procedure :: small
    procedure :: add
    procedure :: sub
    procedure :: mul
    procedure :: divmod
    procedure :: gcd
    procedure :: bitop
    procedure :: shift
    procedure :: power
    procedure :: intern_halves
  end type
contains
  pure integer(int32) function pack_word(x) result(w)
    integer(int64), intent(in) :: x
    w=int(iand(x,2147483647_int64),int32)
    if (btest(x,31)) w=ibset(w,31)
  end function
  pure integer(int64) function unsigned(w) result(x)
    integer(int32), intent(in) :: w
    x=iand(int(w,int64),limb_mask)
  end function
  subroutine init(self)
    class(bignum_arena), intent(inout) :: self
    integer :: ignored
    if (self%count/=0) return
    call grow(self%work,1); ignored=self%intern(self%work(:0))
  end subroutine
  integer function intern(self,words) result(id)
    class(bignum_arena), intent(inout) :: self
    integer(int32), intent(in) :: words(:)
    integer :: n,h,head,i
    n=size(words)
    do while(n>0)
      if (words(n)/=0) exit
      n=n-1
    end do
    h=0
    do i=1,n
      h=hash_words(h,words(i),0,0)
    end do
    head=self%buckets%get(h,n,0,0); id=head
    do while(id/=0)
      do i=1,n
        if (self%limbs(self%start(id)+i-1)/=words(i)) exit
      end do
      if (i>n) return
      id=self%next(id)
    end do
    if (int(self%used,int64)+n>huge(id)) error stop kernel_error
    id=self%count+1; call grow(self%start,id); call grow(self%length,id); call grow(self%next,id)
    call grow(self%limbs,self%used+n)
    self%start(id)=self%used+1; self%length(id)=n; self%next(id)=head
    self%limbs(self%used+1:self%used+n)=words(:n); self%used=self%used+n; self%count=id
    call self%buckets%put(h,n,0,0,id)
  end function
  integer function from_u64(self,x) result(id)
    class(bignum_arena), intent(inout) :: self
    integer(int64), intent(in) :: x
    integer(int32) :: words(2)
    words=[pack_word(x),pack_word(shiftr(x,32))]; id=self%intern(words)
  end function
  integer function from_decimal(self,text,status) result(id)
    class(bignum_arena), intent(inout) :: self
    character(*), intent(in) :: text
    type(status_t), intent(inout) :: status
    integer :: i,j,n,d
    integer(int64) :: carry,t
    id=0; n=0
    if (status%code/=accept) return
    call grow(self%work,1)
    if (len(text)==0) then
      call status%fail(decline,'empty natural literal'); return
    end if
    do i=1,len(text)
      d=iachar(text(i:i))-iachar('0')
      if (d<0 .or. d>9) then
        call status%fail(decline,'invalid natural literal'); return
      end if
      carry=int(d,int64)
      do j=1,n
        t=unsigned(self%work(j))*10+carry; self%work(j)=pack_word(t); carry=shiftr(t,32)
      end do
      if (carry/=0) then
        n=n+1; call grow(self%work,n); self%work(n)=pack_word(carry)
      end if
    end do
    id=self%intern(self%work(:n))
  end function
  pure integer(int64) function word(self,id,i) result(w)
    class(bignum_arena), intent(in) :: self
    integer, intent(in) :: id,i
    w=0
    if (i<1 .or. i>self%length(id)) return
    w=unsigned(self%limbs(self%start(id)+i-1))
  end function
  integer function compare(self,x,y) result(c)
    class(bignum_arena), intent(in) :: self
    integer, intent(in) :: x,y
    integer :: i
    integer(int64) :: a,b
    c=0
    if (self%length(x)/=self%length(y)) then
      c=merge(1,-1,self%length(x)>self%length(y)); return
    end if
    do i=self%length(x),1,-1
      a=self%word(x,i); b=self%word(y,i)
      if (a/=b) then
        c=merge(1,-1,a>b); return
      end if
    end do
  end function
  integer(int64) function bits(self,x) result(n)
    class(bignum_arena), intent(in) :: self
    integer, intent(in) :: x
    integer :: len
    len=self%length(x); n=0
    if (len>0) n=int(len-1,int64)*32+64-leadz(self%word(x,len))
  end function
  integer(int64) function small(self,x) result(n)
    class(bignum_arena), intent(in) :: self
    integer, intent(in) :: x
    n=-1
    if (self%bits(x)>63) return
    n=self%word(x,1)+shiftl(self%word(x,2),32)
  end function
  integer function add(self,x,y) result(id)
    class(bignum_arena), intent(inout) :: self
    integer, intent(in) :: x,y
    integer :: n,i
    integer(int64) :: carry,t
    n=max(self%length(x),self%length(y)); call grow(self%work,n+1); carry=0
    do i=1,n
      t=self%word(x,i)+self%word(y,i)+carry; self%work(i)=pack_word(t); carry=shiftr(t,32)
    end do
    self%work(n+1)=pack_word(carry); id=self%intern(self%work(:n+1))
  end function
  integer function sub(self,x,y) result(id)
    class(bignum_arena), intent(inout) :: self
    integer, intent(in) :: x,y
    integer :: n,i
    integer(int64) :: borrow,t
    id=big_zero
    if (self%compare(x,y)<=0) return
    n=self%length(x); call grow(self%work,n); borrow=0
    do i=1,n
      t=self%word(x,i)-self%word(y,i)-borrow; borrow=merge(1_int64,0_int64,t<0)
      if (t<0) t=t+radix
      self%work(i)=pack_word(t)
    end do
    id=self%intern(self%work(:n))
  end function
  integer function mul(self,x,y) result(id)
    class(bignum_arena), intent(inout) :: self
    integer, intent(in) :: x,y
    integer :: nx,ny,i,j
    integer(int64) :: a,b,a0,a1,b0,b1,p1,lo,hi,t,carry
    id=big_zero; nx=self%length(x); ny=self%length(y)
    if (nx==0 .or. ny==0) return
    call grow(self%work,nx+ny); self%work(:nx+ny)=0
    do i=1,nx
      a=self%word(x,i); a0=iand(a,65535_int64); a1=shiftr(a,16); carry=0
      do j=1,ny
        b=self%word(y,j); b0=iand(b,65535_int64); b1=shiftr(b,16)
        p1=a1*b0+a0*b1; lo=a0*b0+shiftl(iand(p1,65535_int64),16)
        hi=a1*b1+shiftr(p1,16)+shiftr(lo,32); lo=iand(lo,limb_mask)
        t=lo+unsigned(self%work(i+j-1))+carry
        self%work(i+j-1)=pack_word(t); carry=hi+shiftr(t,32)
      end do
      self%work(i+ny)=pack_word(carry)
    end do
    id=self%intern(self%work(:nx+ny))
  end function
  integer function bitop(self,op,x,y) result(id)
    class(bignum_arena), intent(inout) :: self
    integer, intent(in) :: op,x,y
    integer :: n,i
    integer(int64) :: a,b,t
    n=max(self%length(x),self%length(y)); call grow(self%work,max(1,n))
    do i=1,n
      a=self%word(x,i); b=self%word(y,i)
      select case(op)
      case(1)
        t=iand(a,b)
      case(2)
        t=ior(a,b)
      case default
        t=ieor(a,b)
      end select
      self%work(i)=pack_word(t)
    end do
    id=self%intern(self%work(:n))
  end function
  integer function shift(self,x,amount,left) result(id)
    class(bignum_arena), intent(inout) :: self
    integer, intent(in) :: x
    integer(int64), intent(in) :: amount
    logical, intent(in) :: left
    integer :: n,q,r,i,outn
    integer(int64) :: t,carry
    id=big_zero; n=self%length(x)
    if (n==0) return
    if (.not.left) then
      if (amount<0 .or. amount>=self%bits(x)) return
    end if
    if (amount<0 .or. amount/32>huge(0)-n-1) error stop kernel_error
    q=int(amount/32); r=int(mod(amount,32_int64))
    if (left) then
      outn=n+q+1; call grow(self%work,outn); self%work(:outn)=0; carry=0
      do i=1,n
        t=shiftl(self%word(x,i),r)+carry; self%work(q+i)=pack_word(t); carry=shiftr(t,32)
      end do
      self%work(outn)=pack_word(carry)
    else
      outn=n-q; call grow(self%work,outn)
      do i=1,outn
        t=shiftr(self%word(x,i+q),r)
        if (r/=0) t=ior(t,shiftl(self%word(x,i+q+1),32-r))
        self%work(i)=pack_word(t)
      end do
    end if
    id=self%intern(self%work(:outn))
  end function
  integer function power(self,x,exponent) result(id)
    class(bignum_arena), intent(inout) :: self
    integer, intent(in) :: x,exponent
    integer :: n,b
    id=self%from_u64(1_int64); n=exponent; b=x
    do while(n>0)
      if (btest(n,0)) id=self%mul(id,b)
      n=shiftr(n,1)
      if (n/=0) b=self%mul(b,b)
    end do
  end function
  integer function intern_halves(self,digits,n) result(id)
    class(bignum_arena), intent(inout) :: self
    integer(int32), intent(in) :: digits(:)
    integer, intent(in) :: n
    integer :: i,nw
    integer(int64) :: t
    nw=(n+1)/2; call grow(self%work,max(1,nw))
    do i=1,nw
      t=int(digits(2*i-1),int64)
      if (2*i<=n) t=t+shiftl(int(digits(2*i),int64),16)
      self%work(i)=pack_word(t)
    end do
    id=self%intern(self%work(:nw))
  end function
  subroutine divmod(self,x,y,q,r)
    class(bignum_arena), intent(inout) :: self
    integer, intent(in) :: x,y
    integer, intent(out) :: q,r
    integer :: nx,ny,i,j,shift_bits
    integer(int64) :: carry,t,top,qhat,rhat,borrow,p
    q=big_zero; r=x
    if (y==big_zero .or. self%compare(x,y)<0) return
    ! Knuth division D in base 2^16 makes every trial product fit in int64.
    ! The stored representation remains base 2^32; only division scratch uses halves.
    nx=int((self%bits(x)+15)/16); ny=int((self%bits(y)+15)/16)
    call grow(self%du,nx+1); call grow(self%dv,ny); call grow(self%dq,nx-ny+1)
    do i=1,nx
      t=self%word(x,(i+1)/2); self%du(i)=int(iand(shiftr(t,16*mod(i+1,2)),65535_int64))
    end do
    do i=1,ny
      t=self%word(y,(i+1)/2); self%dv(i)=int(iand(shiftr(t,16*mod(i+1,2)),65535_int64))
    end do
    shift_bits=leadz(self%dv(ny))-16; carry=0
    do i=1,nx
      t=shiftl(int(self%du(i),int64),shift_bits)+carry; self%du(i)=int(iand(t,65535_int64)); carry=shiftr(t,16)
    end do
    self%du(nx+1)=int(carry); carry=0
    do i=1,ny
      t=shiftl(int(self%dv(i),int64),shift_bits)+carry; self%dv(i)=int(iand(t,65535_int64)); carry=shiftr(t,16)
    end do
    do j=nx-ny,0,-1
      top=int(self%du(j+ny+1),int64)*half_radix+self%du(j+ny)
      qhat=top/self%dv(ny); rhat=mod(top,int(self%dv(ny),int64))
      do
        if (qhat<half_radix) then
          if (ny==1) exit
          if (qhat*self%dv(ny-1)<=rhat*half_radix+self%du(j+ny-1)) exit
        end if
        qhat=qhat-1; rhat=rhat+self%dv(ny)
        if (rhat>=half_radix) exit
      end do
      borrow=0
      do i=1,ny
        p=qhat*self%dv(i)+borrow; t=int(self%du(j+i),int64)-iand(p,65535_int64)
        borrow=shiftr(p,16)
        if (t<0) then
          t=t+half_radix; borrow=borrow+1
        end if
        self%du(j+i)=int(t)
      end do
      t=int(self%du(j+ny+1),int64)-borrow; self%du(j+ny+1)=int(modulo(t,half_radix))
      if (t<0) then
        qhat=qhat-1; carry=0
        do i=1,ny
          t=int(self%du(j+i),int64)+self%dv(i)+carry
          self%du(j+i)=int(iand(t,65535_int64)); carry=shiftr(t,16)
        end do
        self%du(j+ny+1)=int(modulo(int(self%du(j+ny+1),int64)+carry,half_radix))
      end if
      self%dq(j+1)=int(qhat)
    end do
    carry=0
    do i=ny,1,-1
      t=int(self%du(i),int64)+shiftl(carry,16); self%du(i)=int(shiftr(t,shift_bits))
      carry=iand(t,shiftl(1_int64,shift_bits)-1)
    end do
    q=self%intern_halves(self%dq,nx-ny+1); r=self%intern_halves(self%du,ny)
  end subroutine
  integer function gcd(self,x,y) result(id)
    class(bignum_arena), intent(inout) :: self
    integer, intent(in) :: x,y
    integer :: a,b,q,r
    a=x; b=y
    do while(b/=big_zero)
      call self%divmod(a,b,q,r); a=b; b=r
    end do
    id=a
  end function
  function decimal(self,x) result(text)
    class(bignum_arena), intent(inout) :: self
    integer, intent(in) :: x
    character(:), allocatable :: text
    character(9) :: chunk
    character(16) :: first
    integer :: n,i,parts,j
    integer(int64) :: rem,t
    text='0'; n=self%length(x)
    if (n==0) return
    call grow(self%work,n); self%work(:n)=self%limbs(self%start(x):self%start(x)+n-1); parts=0
    do while(n>0)
      rem=0
      do i=n,1,-1
        t=rem*radix+unsigned(self%work(i)); self%work(i)=pack_word(t/1000000000_int64); rem=mod(t,1000000000_int64)
      end do
      parts=parts+1; call grow(self%dq,parts); self%dq(parts)=int(rem)
      do while(n>0)
        if (self%work(n)/=0) exit
        n=n-1
      end do
    end do
    write(first,'(i0)') self%dq(parts); text=trim(first)
    do j=parts-1,1,-1
      write(chunk,'(i9.9)') self%dq(j); text=text//chunk
    end do
  end function
end module
