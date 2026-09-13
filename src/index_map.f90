! Export indices are normally contiguous. Keep the common case in a compact
! integer array, with a hash-table fallback for genuinely sparse indices.
module index_map
  use base
  use hash_table
  implicit none
  type :: index_map_t
    integer(int32), allocatable :: dense(:)
    integer :: count=0
    type(table_t) :: sparse
  contains
    procedure :: get
    procedure :: put
  end type
contains
  integer function get(self,a,b,c,d) result(v)
    class(index_map_t), intent(in) :: self
    integer, intent(in) :: a,b,c,d
    v=0
    if (ior(b,ior(c,d))/=0) error stop kernel_error
    if (a<0) return
    if (allocated(self%dense)) then
      if (a<size(self%dense)) then
        v=self%dense(a+1)
        if (v/=0) return
      end if
    end if
    v=self%sparse%get(a,0,0,0)
  end function
  subroutine put(self,a,b,c,d,v)
    class(index_map_t), intent(inout) :: self
    integer, intent(in) :: a,b,c,d,v
    integer :: old
    if (a<0 .or. ior(b,ior(c,d))/=0) error stop kernel_error
    old=self%get(a,0,0,0)
    if (old==0) self%count=self%count+1
    if (a<huge(a) .and. int(a,int64)<max(1024_int64,4_int64*self%count)) then
      call grow(self%dense,a+1); self%dense(a+1)=v
    else
      call self%sparse%put(a,0,0,0,v)
    end if
  end subroutine
end module
