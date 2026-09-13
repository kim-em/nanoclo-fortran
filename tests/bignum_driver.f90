program bignum_driver
  use bignums
  implicit none
  type(bignum_arena) :: bn
  type(status_t) :: status
  character(32) :: op
  character(4096) :: xs,ys
  integer :: ios,x,y,z,r
  integer(int64) :: n
  call bn%init()
  do
    read(*,*,iostat=ios) op,xs,ys
    if (ios/=0) exit
    x=bn%from_decimal(trim(xs),status); y=bn%from_decimal(trim(ys),status)
    if (status%code/=accept) error stop 1
    select case(trim(op))
    case('add')
      z=bn%add(x,y)
    case('sub')
      z=bn%sub(x,y)
    case('mul')
      z=bn%mul(x,y)
    case('div','mod')
      call bn%divmod(x,y,z,r)
      if (trim(op)=='mod') z=r
    case('gcd')
      z=bn%gcd(x,y)
    case('and')
      z=bn%bitop(1,x,y)
    case('or')
      z=bn%bitop(2,x,y)
    case('xor')
      z=bn%bitop(3,x,y)
    case('shl','shr')
      n=bn%small(y); z=bn%shift(x,n,trim(op)=='shl')
    case('pow')
      n=bn%small(y); z=bn%power(x,int(n))
    case default
      error stop 1
    end select
    print '(a)',bn%decimal(z)
  end do
end program
