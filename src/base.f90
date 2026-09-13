module base
  use iso_fortran_env, only: int8, int32, int64, error_unit
  implicit none
  integer, parameter :: accept=0, reject=1, decline=2, kernel_error=3
  type :: status_t
    integer :: code=accept
    character(:), allocatable :: message
  contains
    procedure :: fail
  end type
contains
  subroutine fail(self, code, message)
    class(status_t), intent(inout) :: self
    integer, intent(in) :: code
    character(*), intent(in) :: message
    if (self%code /= accept) return
    self%code=code
    self%message=message
  end subroutine

  ! Growth is the only allocation on the arena insertion path. Index zero is
  ! reserved for missing references; each arena explicitly initializes its roots.
  subroutine grow(a, need)
    integer(int32), allocatable, intent(inout) :: a(:)
    integer, intent(in) :: need
    ! Keep the common capacity check small enough to inline at insertion sites.
    if (allocated(a)) then
      if (size(a)>=need) return
    end if
    call grow_storage(a,need)
  end subroutine

  subroutine grow_storage(a, need)
    integer(int32), allocatable, intent(inout) :: a(:)
    integer, intent(in) :: need
    integer(int32), allocatable :: tmp(:)
    integer :: n, old, ierr
    old=0
    if (allocated(a)) old=size(a)
    if (old >= need) return
    if (need < 0 .or. old > shiftr(huge(old),1)) error stop kernel_error
    n=max(16, need, 2*old)
    allocate(tmp(n), stat=ierr)
    if (ierr /= 0) error stop kernel_error
    tmp=0
    if (old > 0) tmp(:old)=a
    call move_alloc(tmp,a)
  end subroutine

  subroutine read_bytes(path, bytes, status)
    character(*), intent(in) :: path
    character, allocatable, intent(out) :: bytes(:)
    type(status_t), intent(inout) :: status
    integer :: u, ios
    integer(int64) :: n
    character(512) :: msg
    open(newunit=u,file=path,access='stream',form='unformatted',status='old',action='read',iostat=ios,iomsg=msg)
    if (ios /= 0) then
      call status%fail(kernel_error,'open: '//trim(msg)); return
    end if
    inquire(unit=u,size=n,iostat=ios)
    if (ios /= 0 .or. n < 0) then
      close(u); call status%fail(kernel_error,'cannot determine input size'); return
    end if
    allocate(bytes(n),stat=ios)
    if (ios /= 0) then
      close(u); call status%fail(kernel_error,'cannot allocate input buffer'); return
    end if
    read(u,iostat=ios,iomsg=msg) bytes
    close(u)
    if (ios /= 0) call status%fail(kernel_error,'read: '//trim(msg))
  end subroutine
end module
