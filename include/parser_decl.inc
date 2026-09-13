  integer function uparams_list(self,bytes,obj,status) result(id)
    class(export_parser), intent(inout) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: obj
    type(status_t), intent(inout) :: status
    integer :: n,t,idx,name
    id=list_nil
    if (status%code/=accept) return
    if (obj<1 .or. obj>self%tokens%count) then
      call status%fail(decline,'missing universe parameters'); return
    end if
    if (self%tokens%tag(obj)/=jarray) then
      call status%fail(decline,'universe parameters must be a list'); return
    end if
    n=0; t=obj+1
    do while(t<self%tokens%next(obj))
      idx=int(self%tokens%uint(bytes,t,status)); name=self%lookup(self%name_map,idx,status)
      if (status%code/=accept) return
      n=n+1; call grow(self%scratch_idxs,n)
      self%scratch_idxs(n)=self%levels%param(name)
      t=self%tokens%next(t)
    end do
    if (n>0) id=self%lists%from_array(self%scratch_idxs(:n))
  end function
  subroutine decl_record(self,bytes,obj,kind,status)
    class(export_parser), intent(inout) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: obj
    character(*), intent(in) :: kind
    type(status_t), intent(inout) :: status
    integer :: tag,n,params,ty,val,hint,height,t,idx,id
    character(:), allocatable :: text
    if (kind=='inductive') then
      call self%inductive_record(bytes,obj,status); return
    else if (kind=='ctor' .or. kind=='rec') then
      call status%fail(decline,'constructor/recursor outside inductive block'); return
    end if
    idx=self%indexed_uint(bytes,obj,'name',status); n=self%lookup(self%name_map,idx,status)
    t=self%tokens%field(bytes,obj,'levelParams',status); params=self%uparams_list(bytes,t,status)
    ty=self%expr_ref(bytes,obj,'type',status)
    val=0; hint=hint_opaque; height=0
    select case(kind)
    case('axiom','opaque')
      tag=daxiom
      if (kind=='opaque') then
        tag=dopaque; val=self%expr_ref(bytes,obj,'value',status)
      end if
      t=self%tokens%field(bytes,obj,'isUnsafe',status)
      if (status%code/=accept) return
      if (t==0) then
        call status%fail(decline,'missing isUnsafe'); return
      end if
      if (self%tokens%tag(t)==jtrue) then
        call status%fail(reject,'unsafe declaration'); return
      else if (self%tokens%tag(t)/=jfalse) then
        call status%fail(decline,'isUnsafe must be Boolean'); return
      end if
    case('thm','def')
      tag=dtheorem; val=self%expr_ref(bytes,obj,'value',status)
      if (kind=='def') then
        tag=ddefinition
        t=self%tokens%field(bytes,obj,'safety',status); text=self%tokens%string(bytes,t,status)
        if (status%code/=accept) return
        if (text=='unsafe' .or. text=='partial') then
          call status%fail(reject,'unsafe or partial definition'); return
        else if (text/='safe') then
          call status%fail(decline,'unknown definition safety'); return
        end if
        t=self%tokens%field(bytes,obj,'hints',status)
        if (status%code/=accept) return
        if (t==0) then
          call status%fail(decline,'missing reducibility hint'); return
        end if
        if (self%tokens%tag(t)==jstring) then
          text=self%tokens%string(bytes,t,status)
          select case(text)
          case('opaque')
            hint=hint_opaque
          case('abbrev')
            hint=hint_abbrev
          case default
            call status%fail(decline,'unknown reducibility hint'); return
          end select
        else
          t=self%tokens%field(bytes,t,'regular',status)
          height=int(self%tokens%uint(bytes,t,status,65535_int64)); hint=hint_regular
        end if
      end if
    case('quot')
      tag=dquot
      t=self%tokens%field(bytes,obj,'kind',status); text=self%tokens%string(bytes,t,status)
      if (status%code/=accept) return
      if (text/='type' .and. text/='ctor' .and. text/='lift' .and. text/='ind') then
        call status%fail(decline,'unknown quotient kind'); return
      end if
    case default
      call status%fail(decline,'unsupported declaration'); return
    end select
    if (status%code/=accept) return
    id=self%decls%add(tag,n,params,ty,val,hint,height,status)
  end subroutine
  integer function array_field(self,bytes,obj,key,status) result(t)
    class(export_parser), intent(in) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: obj
    character(*), intent(in) :: key
    type(status_t), intent(inout) :: status
    t=self%tokens%field(bytes,obj,key,status)
    if (status%code/=accept) return
    if (t==0) then
      call status%fail(decline,'missing '//key); return
    end if
    if (self%tokens%tag(t)/=jarray) call status%fail(decline,key//' must be an array')
  end function
  integer function bool_field(self,bytes,obj,key,status) result(value)
    class(export_parser), intent(in) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: obj
    character(*), intent(in) :: key
    type(status_t), intent(inout) :: status
    integer :: t
    value=0; t=self%tokens%field(bytes,obj,key,status)
    if (status%code/=accept) return
    if (t==0) then
      call status%fail(decline,'missing '//key); return
    end if
    if (self%tokens%tag(t)/=jtrue .and. self%tokens%tag(t)/=jfalse) then
      call status%fail(decline,key//' must be Boolean'); return
    end if
    value=merge(1,0,self%tokens%tag(t)==jtrue)
  end function
  integer function u16_field(self,bytes,obj,key,status) result(value)
    class(export_parser), intent(in) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: obj
    character(*), intent(in) :: key
    type(status_t), intent(inout) :: status
    integer :: t
    t=self%tokens%field(bytes,obj,key,status)
    value=int(self%tokens%uint(bytes,t,status,65535_int64))
  end function
  integer function name_list(self,bytes,obj,key,status) result(id)
    class(export_parser), intent(inout) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: obj
    character(*), intent(in) :: key
    type(status_t), intent(inout) :: status
    integer :: t,n,idx,arr
    id=list_nil; arr=self%array_field(bytes,obj,key,status)
    if (status%code/=accept) return
    t=arr+1; n=0
    do while(t<self%tokens%next(arr))
      idx=int(self%tokens%uint(bytes,t,status)); n=n+1; call grow(self%scratch_idxs,n)
      self%scratch_idxs(n)=self%lookup(self%name_map,idx,status)
      if (status%code/=accept) return
      t=self%tokens%next(t)
    end do
    if (n>0) id=self%lists%from_array(self%scratch_idxs(:n))
  end function
  subroutine inductive_record(self,bytes,obj,status)
    class(export_parser), intent(inout) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: obj
    type(status_t), intent(inout) :: status
    integer :: first,last,arr,t,kind,i
    character(5), parameter :: fields(3)=[character(5)::'types','ctors','recs']
    first=self%decls%count+1
    ! Export insertion order is types, constructors, recursors regardless of
    ! JSON field order. Every member sees the same complete mutual block.
    do kind=1,3
      arr=self%array_field(bytes,obj,trim(fields(kind)),status)
      if (status%code/=accept) return
      t=arr+1
      do while(t<self%tokens%next(arr))
        call self%inductive_member(bytes,t,dinductive+kind-1,status)
        if (status%code/=accept) return
        t=self%tokens%next(t)
      end do
    end do
    last=self%decls%count+1
    do i=first,last-1
      self%decls%block_start(i)=first; self%decls%block_end(i)=last
    end do
  end subroutine
  subroutine inductive_member(self,bytes,obj,kind,status)
    class(export_parser), intent(inout) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: obj,kind
    type(status_t), intent(inout) :: status
    integer :: id,idx,n,params,ty,t,unsafe,all,ctors,np,ni,nm,nn,rec,nest,k,parent,cidx,nf,rules,arr,r,rc,rf,rv
    if (status%code/=accept) return
    idx=self%indexed_uint(bytes,obj,'name',status); n=self%lookup(self%name_map,idx,status)
    t=self%tokens%field(bytes,obj,'levelParams',status); params=self%uparams_list(bytes,t,status)
    ty=self%expr_ref(bytes,obj,'type',status); unsafe=self%bool_field(bytes,obj,'isUnsafe',status)
    if (status%code/=accept) return
    if (unsafe/=0) then
      call status%fail(reject,'unsafe inductive declaration'); return
    end if
    np=self%u16_field(bytes,obj,'numParams',status)
    all=list_nil; ctors=list_nil; rules=list_nil
    ni=0; nm=0; nn=0; rec=0; nest=0; k=0; parent=0; cidx=0; nf=0
    select case(kind)
    case(dinductive)
      all=self%name_list(bytes,obj,'all',status); ctors=self%name_list(bytes,obj,'ctors',status)
      rec=self%bool_field(bytes,obj,'isRec',status)
      t=self%bool_field(bytes,obj,'isReflexive',status) ! Required by serde; unused by the kernel.
      ni=self%u16_field(bytes,obj,'numIndices',status)
      nest=self%u16_field(bytes,obj,'numNested',status); nest=merge(1,0,nest>0)
    case(dconstructor)
      idx=self%indexed_uint(bytes,obj,'induct',status); parent=self%lookup(self%name_map,idx,status)
      cidx=self%u16_field(bytes,obj,'cidx',status); nf=self%u16_field(bytes,obj,'numFields',status)
    case(drecursor)
      all=self%name_list(bytes,obj,'all',status)
      ni=self%u16_field(bytes,obj,'numIndices',status); nm=self%u16_field(bytes,obj,'numMotives',status)
      nn=self%u16_field(bytes,obj,'numMinors',status); k=self%bool_field(bytes,obj,'k',status)
      arr=self%array_field(bytes,obj,'rules',status)
      if (status%code/=accept) return
      t=arr+1; n=0
      do while(t<self%tokens%next(arr))
        idx=self%indexed_uint(bytes,t,'ctor',status); rc=self%lookup(self%name_map,idx,status)
        rf=self%u16_field(bytes,t,'nfields',status); rv=self%expr_ref(bytes,t,'rhs',status)
        if (status%code/=accept) return
        r=self%decls%add_rule(rc,rf,rv); n=n+1; call grow(self%scratch_idxs,n); self%scratch_idxs(n)=r
        t=self%tokens%next(t)
      end do
      if (n>0) rules=self%lists%from_array(self%scratch_idxs(:n))
    end select
    if (status%code/=accept) return
    idx=self%indexed_uint(bytes,obj,'name',status); n=self%lookup(self%name_map,idx,status)
    id=self%decls%add(kind,n,params,ty,0,hint_opaque,0,status)
    if (status%code/=accept) return
    self%decls%num_params(id)=np; self%decls%num_indices(id)=ni
    self%decls%num_motives(id)=nm; self%decls%num_minors(id)=nn
    self%decls%is_recursive(id)=rec; self%decls%is_nested(id)=nest; self%decls%is_k(id)=k
    self%decls%all_names(id)=all; self%decls%ctor_names(id)=ctors; self%decls%rules(id)=rules
    self%decls%inductive_name(id)=parent; self%decls%ctor_idx(id)=cidx; self%decls%num_fields(id)=nf
  end subroutine
