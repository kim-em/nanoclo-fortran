  subroutine init(self)
    class(checker), intent(inout) :: self
    integer :: limit,i
    character(14), parameter :: nat_names(15)=[character(14)::'Nat.succ','Nat.add','Nat.sub','Nat.mul','Nat.pow', &
      'Nat.mod','Nat.div','Nat.beq','Nat.ble','Nat.land','Nat.lor','Nat.xor','Nat.gcd','Nat.shiftLeft','Nat.shiftRight']
    limit=self%ctx%names%count
    call self%ctx%bignums%init(); call self%ctx%names%init(); call self%ctx%levels%init(); call self%ctx%lists%init()
    call self%rp%init(); call self%nb%init()
    self%reduce_nat=self%name_id('Lean.reduceNat'); self%reduce_bool=self%name_id('Lean.reduceBool')
    self%quot_lift=self%name_id('Quot.lift'); self%quot_ind=self%name_id('Quot.ind'); self%quot_mk=self%name_id('Quot.mk')
    self%nat_name=self%cached_name('Nat',limit); self%nat_zero=self%cached_name('Nat.zero',limit)
    do i=1,15
      self%nat_ops(i)=self%cached_name(trim(nat_names(i)),limit)
    end do
    self%bool_true=self%cached_name('Bool.true',limit); self%bool_false=self%cached_name('Bool.false',limit)
    self%string_name=self%cached_name('String',limit); self%string_mk=self%cached_name('String.mk',limit)
    self%string_of_list=self%cached_name('String.ofList',limit); self%char_name=self%cached_name('Char',limit)
    self%char_of_nat=self%cached_name('Char.ofNat',limit); self%list_nil_name=self%cached_name('List.nil',limit)
    self%list_cons_name=self%cached_name('List.cons',limit); self%big_one=self%ctx%bignums%from_u64(1_int64)
  end subroutine
  integer function cached_name(self,text,limit) result(id)
    class(checker), intent(inout) :: self
    character(*), intent(in) :: text
    integer, intent(in) :: limit
    id=self%name_id(text)
    if (id>limit) id=0
  end function
  integer function name_id(self,text) result(id)
    class(checker), intent(inout) :: self
    character(*), intent(in) :: text
    integer :: first,i,s
    id=anonymous; first=1
    do i=1,len(text)+1
      if (i<=len(text)) then
        if (text(i:i)/='.') cycle
      end if
      s=self%ctx%strings%intern(text(first:i-1)); id=self%ctx%names%str(id,s); first=i+1
    end do
  end function
  recursive function name_text(self,id) result(text)
    class(checker), intent(in) :: self
    integer, intent(in) :: id
    character(:), allocatable :: text,prefix,suffix
    character(32) :: num
    integer(int64) :: n
    if (id==anonymous) then
      text=''; return
    end if
    prefix=self%name_text(self%ctx%names%a(id))
    if (self%ctx%names%tag(id)==name_str) then
      suffix=self%ctx%strings%text(self%ctx%names%b(id))
    else
      n=ior(iand(int(self%ctx%names%b(id),int64),4294967295_int64),shiftl(int(self%ctx%names%c(id),int64),32))
      write(num,'(i0)') n; suffix=trim(num)
    end if
    text=suffix
    if (len(prefix)>0) text=prefix//'.'//suffix
  end function
  integer function work_slot(self) result(i)
    class(checker), intent(inout) :: self
    i=self%work_top+1
    call grow(self%w1,i); call grow(self%w2,i); call grow(self%w3,i)
    self%work_top=i
  end function
  integer function cache_closure(self,c) result(id)
    class(checker), intent(inout) :: self
    type(closure_t), intent(in) :: c
    id=self%clo_intern%get(c%e,c%env,0,0)
    if (id/=0) return
    id=self%clo_count+1
    call grow(self%clo_e,id); call grow(self%clo_env,id)
    self%clo_e(id)=c%e; self%clo_env(id)=c%env; self%clo_count=id
    call self%clo_intern%put(c%e,c%env,0,0,id)
  end function
  logical function levels_equal(self,xs,ys) result(ok)
    class(checker), intent(inout) :: self
    integer, intent(in) :: xs,ys
    integer :: x,y,a,b
    ok=.true.
    if (xs==ys) return
    ok=.false.
    if (self%ctx%lists%length(xs)/=self%ctx%lists%length(ys)) return
    x=xs; y=ys
    do while(x/=list_nil)
      a=self%ctx%lists%head(x); b=self%ctx%lists%head(y)
      if (.not.self%ctx%levels%eq_antisymm(a,b)) return
      x=self%ctx%lists%tail(x); y=self%ctx%lists%tail(y)
    end do
    ok=.true.
  end function
  subroutine reset_decl(self)
    class(checker), intent(inout) :: self
    if (self%ctx%exprs%shared_export) then
      if (.not.self%persist_dag .or. self%ctx%exprs%count-self%ctx%exprs%export_count>self%dag_epoch) then
        call self%ctx%exprs%clear_scratch(); call self%rp%reads%reset_const_caches()
        call self%g_unfold%clear(); call self%g_inst_ty%clear()
      end if
    end if
    call self%rp%reset_decl(); call self%nb%reset_decl(); self%unique_counter=0
    call self%reify_go_cache%clear(); call self%infer_cache_check%clear(); call self%infer_cache_only%clear()
    call self%prop_cache%clear()
    call self%clo_intern%clear(); self%clo_count=0; self%work_top=0
  end subroutine
  subroutine check_all(self)
    class(checker), intent(inout) :: self
    integer :: i,n,envstat
    logical :: declctrs,totalctrs,allctrs
    character(:), allocatable :: name
    if (self%status%code/=accept) return
    call self%ctx%release_parser_storage()
    call self%ctx%exprs%seal_exports()
    call self%init()
    call get_environment_variable('NANOCLO_DECLCTRS',length=n,status=envstat); declctrs=envstat==0
    call get_environment_variable('NANOCLO_CTRS',length=n,status=envstat); totalctrs=envstat==0
    call get_environment_variable('NANOCLO_DECLCTRS_ALL',length=n,status=envstat); allctrs=envstat==0
    self%totals=0
    if (self%jobs>1) then
      call self%check_parallel(declctrs,allctrs)
    else
      do i=1,self%ctx%decls%count
        call self%check_declaration(i)
        call self%report_decl_counters(i,self%rp%ctrs,declctrs,allctrs)
        self%totals=self%totals+self%rp%ctrs
        if (self%status%code/=accept) then
          name=self%name_text(self%ctx%decls%name(i)); self%status%message=name//': '//self%status%message; exit
        end if
      end do
    end if
    if (totalctrs) then
      write(error_unit,'(a)',advance='no') 'CTRS '
      do i=1,26
        write(error_unit,'(a,a,i0,a)',advance='no') trim(counter_names(i)),'=',self%totals(i),' '
      end do
      write(error_unit,*)
    end if
  end subroutine
