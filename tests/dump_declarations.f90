! Test-only inspector: lets Python compare every parsed metadata field with JSON.
program dump_declarations
  use kernel, only: checker
  use base
  use index_map
  use hash_table, only: table_t
  use levels, only: lparam
  implicit none
  type(checker) :: ck
  character, allocatable :: bytes(:)
  character(:), allocatable :: path
  integer :: n,i
  call get_command_argument(1,length=n); allocate(character(n)::path); call get_command_argument(1,path)
  call read_bytes(path,bytes,ck%status); call ck%ctx%scan(bytes,ck%status)
  if (ck%status%code/=accept) stop ck%status%code,quiet=.true.
  call mapping('N',ck%ctx%name_map); call mapping('E',ck%ctx%expr_map)
  do i=1,ck%ctx%levels%count
    if (ck%ctx%levels%tag(i)==lparam) print '(a,2(1x,i0))','P',ck%ctx%levels%a(i),i
  end do
  do i=1,ck%ctx%lists%count
    print '(a,3(1x,i0))','S',i,ck%ctx%lists%head(i),ck%ctx%lists%tail(i)
  end do
  do i=1,ck%ctx%decls%count
    associate(d=>ck%ctx%decls)
      print '(a,22(1x,i0))','D',d%tag(i),d%name(i),d%uparams(i),d%ty(i),d%value(i),d%hint(i),d%height(i), &
        d%num_params(i),d%num_indices(i),d%num_motives(i),d%num_minors(i),d%is_recursive(i),d%is_nested(i), &
        d%is_k(i),d%all_names(i),d%ctor_names(i),d%inductive_name(i),d%ctor_idx(i),d%num_fields(i), &
        d%rules(i),d%block_start(i),d%block_end(i)
    end associate
  end do
  do i=1,ck%ctx%decls%rule_count
    print '(a,4(1x,i0))','R',i,ck%ctx%decls%rule_ctor(i),ck%ctx%decls%rule_fields(i),ck%ctx%decls%rule_value(i)
  end do
contains
  subroutine mapping(kind,t)
    character, intent(in) :: kind
    type(index_map_t), intent(in) :: t
    integer :: j
    if (allocated(t%dense)) then
      do j=1,size(t%dense)
        if (t%dense(j)>0) print '(a,2(1x,i0))',kind,j-1,t%dense(j)
      end do
    end if
    if (.not.allocated(t%sparse%value)) return
    do j=1,size(t%sparse%value)
      if (t%sparse%value(j)>0) print '(a,2(1x,i0))',kind,t%sparse%k1(j),t%sparse%value(j)
    end do
  end subroutine
end program
