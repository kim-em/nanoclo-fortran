! Four-word open-addressing table shared by all arena and memo-table instances.
module hash_table
  use base
  implicit none
  private
  public :: table_t, hash_words
  type :: table_t
    integer(int32), allocatable :: k1(:), k2(:), k3(:), k4(:), value(:)
    integer(int32) :: count=0,mask=-1,limit=0
  contains
    procedure, non_overridable :: get => table_get
    procedure, non_overridable :: put => table_put
    procedure, non_overridable :: clear => table_clear
    procedure, non_overridable :: reserve => table_reserve
    procedure, non_overridable :: move_to => table_move_to
  end type
contains
  ! All arithmetic is representable: hash mixing uses bit shifts/xor, with
  ! unsigned 32-bit words widened before multiplication and masked afterwards.
  pure function hash_words(a,b,c,d) result(h)
    integer(int32), value, intent(in) :: a,b,c,d
    integer(int32) :: h
    integer(int64) :: x
    integer :: i
    integer(int32) :: words(4)
    words=[a,b,c,d]
    x=2166136261_int64
    do i=1,4
      x=ieor(x,iand(int(words(i),int64),4294967295_int64))
      x=iand(x*16777619_int64,4294967295_int64)
      x=ieor(x,shiftr(x,16))
    end do
    h=int(iand(x,2147483647_int64),int32)
  end function

  ! Bucket placement is independent of the structural hashes stored by arenas.
  ! Widen signed keys before multiplying: |int32| * uint32 fits signed int64.
  ! Fold the high product bits down so high key bits affect small tables too.
  ! Constant zero key words disappear when the compiler specializes this code.
  pure integer(int32) function bucket_hash(a,b,c,d) result(h)
    integer(int32), value, intent(in) :: a,b,c,d
    integer(int64) :: x
    x=ieor(int(a,int64)*2654435761_int64,int(b,int64)*2246822519_int64)
    x=ieor(x,int(c,int64)*3266489917_int64)
    x=ieor(x,int(d,int64)*668265263_int64)
    x=ieor(x,shiftr(x,32))
    h=int(iand(x,2147483647_int64),int32)
  end function

  function table_get(self,a,b,c,d) result(v)
    class(table_t), intent(in) :: self
    integer(int32), value, intent(in) :: a,b,c,d
    integer(int32) :: v, slot, mask
    v=0
    mask=self%mask
    if (mask<0) return
    slot=iand(bucket_hash(a,b,c,d),mask)+1
    do while (self%value(slot) /= 0)
      if (self%k1(slot)==a .and. self%k2(slot)==b .and. self%k3(slot)==c .and. self%k4(slot)==d) then
        v=self%value(slot); return
      end if
      slot=iand(slot,mask)+1
    end do
  end function

  subroutine table_clear(self)
    class(table_t), intent(inout) :: self
    self%count=0
    if (allocated(self%value)) self%value=0
  end subroutine

  subroutine table_move_to(self,destination)
    class(table_t), intent(inout) :: self
    type(table_t), intent(inout) :: destination
    call move_alloc(self%k1,destination%k1)
    call move_alloc(self%k2,destination%k2)
    call move_alloc(self%k3,destination%k3)
    call move_alloc(self%k4,destination%k4)
    call move_alloc(self%value,destination%value)
    destination%count=self%count; destination%mask=self%mask; destination%limit=self%limit
    self%count=0; self%mask=-1; self%limit=0
  end subroutine

  subroutine table_reserve(self,need)
    class(table_t), intent(inout) :: self
    integer, intent(in) :: need
    type(table_t) :: fresh
    integer :: cap,i,ierr
    cap=16
    if (allocated(self%value)) then
      if (int(need,int64)*10 < int(size(self%value),int64)*7) return
      cap=size(self%value)
    end if
    do while (int(need,int64)*10 >= int(cap,int64)*7)
      if (cap > shiftr(huge(cap),1)) error stop kernel_error
      cap=cap*2
    end do
    allocate(fresh%k1(cap),fresh%k2(cap),fresh%k3(cap),fresh%k4(cap),fresh%value(cap),stat=ierr)
    if (ierr/=0) error stop kernel_error
    fresh%value=0
    fresh%mask=cap-1
    fresh%limit=int((int(cap,int64)*7-1)/10,int32)
    if (allocated(self%value)) then
      do i=1,size(self%value)
        if (self%value(i)/=0) call put_reserved(fresh,self%k1(i),self%k2(i),self%k3(i),self%k4(i),self%value(i))
      end do
    end if
    call move_alloc(fresh%k1,self%k1)
    call move_alloc(fresh%k2,self%k2)
    call move_alloc(fresh%k3,self%k3)
    call move_alloc(fresh%k4,self%k4)
    call move_alloc(fresh%value,self%value)
    self%count=fresh%count
    self%mask=fresh%mask; self%limit=fresh%limit
  end subroutine

  subroutine table_put(self,a,b,c,d,v)
    class(table_t), intent(inout) :: self
    integer(int32), value, intent(in) :: a,b,c,d,v
    if (v==0) error stop kernel_error
    if (self%count>=self%limit) call self%reserve(self%count+1)
    call put_reserved(self,a,b,c,d,v)
  end subroutine

  subroutine put_reserved(self,a,b,c,d,v)
    class(table_t), intent(inout) :: self
    integer(int32), value, intent(in) :: a,b,c,d,v
    integer :: slot,mask
    mask=self%mask
    slot=iand(bucket_hash(a,b,c,d),mask)+1
    do while (self%value(slot)/=0)
      if (self%k1(slot)==a .and. self%k2(slot)==b .and. self%k3(slot)==c .and. self%k4(slot)==d) exit
      slot=iand(slot,mask)+1
    end do
    if (self%value(slot)==0) self%count=self%count+1
    self%k1(slot)=a; self%k2(slot)=b; self%k3(slot)=c; self%k4(slot)=d; self%value(slot)=v
  end subroutine
end module
