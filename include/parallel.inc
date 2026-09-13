  subroutine check_declaration(self,i)
    class(checker), intent(inout) :: self
    integer, intent(in) :: i
    integer :: e,ty
    logical :: ok
    character(:), allocatable :: name
    call self%reset_decl(); self%current_decl=i; self%cutoff=i; self%rp%ctrs=0
    select case(self%ctx%decls%tag(i))
    case(dinductive)
      call self%check_inductive(i)
    case(dconstructor,drecursor)
      call self%check_ctor_or_rec(i)
    case(dquot)
      call self%check_quot(i)
    case default
      if (self%standard_axioms .and. self%ctx%decls%tag(i)==daxiom) then
        name=self%name_text(self%ctx%decls%name(i))
        if (name/='propext' .and. name/='Quot.sound' .and. name/='Classical.choice') then
          call self%status%fail(reject,'axiom excluded by --axioms=standard'); return
        end if
      end if
      call self%check_declar_info(i)
      if (self%ctx%decls%tag(i)/=daxiom .and. self%status%code==accept) then
        e=self%ctx%decls%value(i); ty=self%infer(e,.true.)
        e=self%ctx%decls%ty(i); ok=self%is_def_eq(closure_t(ty,env_nil),closure_t(e,env_nil))
        if (.not.ok) call self%status%fail(reject,'declaration value has wrong type')
      end if
    end select
  end subroutine
  subroutine report_decl_counters(self,i,ctrs,declctrs,allctrs)
    class(checker), intent(in) :: self
    integer, intent(in) :: i
    integer(int64), intent(in) :: ctrs(26)
    logical, intent(in) :: declctrs,allctrs
    integer :: j
    character(:), allocatable :: name
    if (.not.declctrs .and. .not.allctrs) return
    name=self%name_text(self%ctx%decls%name(i))
    if (declctrs) then
      write(error_unit,'(a,a,i0,a,a,3(a,i0))') 'DC',achar(9),i-1,achar(9),name, &
        achar(9),ctrs(2),achar(9),ctrs(4),achar(9),ctrs(1)
    end if
    if (allctrs) then
      write(error_unit,'(a,a,i0,a,a)',advance='no') 'DC26',achar(9),i-1,achar(9),name
      do j=1,26
        write(error_unit,'(a,i0)',advance='no') achar(9),ctrs(j)
      end do
      write(error_unit,*)
    end if
  end subroutine
  subroutine check_parallel(self,declctrs,allctrs)
    class(checker), intent(inout) :: self
    logical, intent(in) :: declctrs,allctrs
    type(checker), allocatable :: workers(:)
    integer(int64), allocatable :: counters(:,:)
    integer :: i,t,failed,skip,num_decls,failed_decl
    logical :: reporting
    character(:), allocatable :: name
    num_decls=self%ctx%decls%count; reporting=declctrs .or. allctrs; failed=0; failed_decl=0
    allocate(workers(self%jobs),source=self)
    do t=1,self%jobs
      workers(t)%persist_dag=.false.
    end do
    if (reporting) then
      allocate(counters(26,num_decls)); counters=0
    end if
    !$omp parallel num_threads(self%jobs) default(none) &
    !$omp shared(self,workers,counters,reporting,failed,num_decls,failed_decl) private(i,t,skip)
    t=omp_get_thread_num()+1
    !$omp do schedule(dynamic)
    do i=1,num_decls
      !$omp atomic read
      skip=failed
      if (skip/=0) cycle
      call workers(t)%check_declaration(i)
      workers(t)%totals=workers(t)%totals+workers(t)%rp%ctrs
      if (reporting) counters(:,i)=workers(t)%rp%ctrs
      if (workers(t)%status%code/=accept) then
        !$omp critical (checker_failure)
        if (self%status%code==accept) then
          self%status=workers(t)%status
          failed_decl=i
        end if
        !$omp end critical (checker_failure)
        !$omp atomic write
        failed=1
      end if
    end do
    !$omp end do
    !$omp end parallel
    if (failed_decl/=0) then
      name=self%name_text(self%ctx%decls%name(failed_decl)); self%status%message=name//': '//self%status%message
    end if
    do t=1,self%jobs
      self%totals=self%totals+workers(t)%totals
    end do
    if (reporting) then
      do i=1,num_decls
        call self%report_decl_counters(i,counters(:,i),declctrs,allctrs)
      end do
    end if
  end subroutine
