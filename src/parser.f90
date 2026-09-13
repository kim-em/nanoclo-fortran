! Parser foundation translated from nanoclo src/parser.rs. Export indices are
! sparse and independent of interned IDs. Root export indices are both zero.
module parser
  use base
  use bignums, only: bignum_arena
  use hash_table
  use index_map
  use names
  use levels
  use strings
  use json
  use declarations, only: declaration_arena,daxiom,ddefinition,dtheorem,dopaque,dquot,dinductive,dconstructor,drecursor,hint_opaque,hint_regular,hint_abbrev
  use expr, only: expr_arena,evar,esort,econst,eapp,epi,elam,elet,eproj,enat,estr
  use sequences, only: sequence_arena,list_nil
  implicit none
  type :: export_parser
    logical :: nat_extension=.true.,string_extension=.true.
    type(bignum_arena) :: bignums
    type(declaration_arena) :: decls
    type(name_arena) :: names
    type(level_arena) :: levels
    type(expr_arena) :: exprs
    type(sequence_arena) :: lists
    integer(int32), allocatable :: scratch_idxs(:)
    type(string_arena) :: strings
    type(index_map_t) :: name_map,level_map,expr_map
    type(json_tokens) :: tokens
    integer(int64) :: records=0,name_records=0,level_records=0,expr_records=0,decl_records=0
  contains
    procedure :: release_parser_storage
    procedure :: scan
    procedure :: scan_file
    procedure :: begin_scan
    procedure :: scan_line
    procedure :: end_scan
    procedure :: record
    procedure :: lookup
    procedure :: expr_record
    procedure :: expr_ref
    procedure :: level_list
    procedure :: decl_record
    procedure :: uparams_list
    procedure :: name_list
    procedure :: bool_field
    procedure :: u16_field
    procedure :: array_field
    procedure :: inductive_record
    procedure :: inductive_member
    procedure :: indexed_uint
  end type
contains
  subroutine release_parser_storage(self)
    class(export_parser), intent(inout) :: self
    self%name_map=index_map_t(); self%level_map=index_map_t(); self%expr_map=index_map_t()
    self%tokens=json_tokens()
    if (allocated(self%scratch_idxs)) deallocate(self%scratch_idxs)
  end subroutine
  subroutine begin_scan(self)
    class(export_parser), intent(inout) :: self
    call self%bignums%init(); call self%names%init(); call self%levels%init(); call self%lists%init()
    call self%name_map%put(0,0,0,0,anonymous)
    call self%level_map%put(0,0,0,0,zero)
  end subroutine
  subroutine scan_line(self,bytes,status)
    class(export_parser), intent(inout) :: self
    character, intent(in) :: bytes(:)
    type(status_t), intent(inout) :: status
    character(32) :: line_text
    call self%tokens%parse(bytes,status)
    if (status%code==accept) call self%record(bytes,status)
    if (status%code/=accept) then
      write(line_text,'(i0)') self%records+1
      status%message='line '//trim(line_text)//': '//status%message
      return
    end if
    self%records=self%records+1
  end subroutine
  subroutine end_scan(self,status)
    class(export_parser), intent(inout) :: self
    type(status_t), intent(inout) :: status
    if (self%records==0) call status%fail(decline,'missing export metadata')
    self%exprs%current_origin=1
  end subroutine
  subroutine scan(self,bytes,status)
    class(export_parser), intent(inout) :: self
    character, intent(in) :: bytes(:)
    type(status_t), intent(inout) :: status
    integer(int64) :: first,last
    call self%begin_scan()
    first=1
    do while(first<=size(bytes,kind=int64))
      last=first
      do while(last<=size(bytes,kind=int64))
        if (bytes(last)==achar(10)) exit
        last=last+1
      end do
      call self%scan_line(bytes(first:last-1),status)
      if (status%code/=accept) return
      first=last+1
    end do
    call self%end_scan(status)
  end subroutine
  subroutine scan_file(self,path,status)
    class(export_parser), intent(inout) :: self
    character(*), intent(in) :: path
    type(status_t), intent(inout) :: status
    character, allocatable :: buffer(:),larger(:)
    integer, parameter :: chunk=1048576
    integer :: unit,ios,used,first,last,nread,capacity,new_capacity
    integer(int64) :: file_size,offset
    character(512) :: message
    open(newunit=unit,file=path,access='stream',form='unformatted',status='old',action='read', &
         iostat=ios,iomsg=message)
    if (ios/=0) then
      call status%fail(kernel_error,'open: '//trim(message)); return
    end if
    inquire(unit=unit,size=file_size,iostat=ios,iomsg=message)
    if (ios/=0 .or. file_size<0) then
      close(unit); call status%fail(kernel_error,'cannot determine input size'); return
    end if
    capacity=chunk
    allocate(buffer(capacity),stat=ios)
    if (ios/=0) then
      close(unit); call status%fail(kernel_error,'cannot allocate input buffer'); return
    end if
    call self%begin_scan()
    used=0; offset=0; last=1
    do while(offset<file_size)
      if (used==capacity) then
        ! Only a single long record can grow the buffer beyond one chunk.
        if (capacity==huge(capacity)) then
          call status%fail(decline,'JSON record exceeds index range'); exit
        end if
        new_capacity=int(min(2_int64*capacity,int(huge(capacity),int64)))
        allocate(larger(new_capacity),stat=ios)
        if (ios/=0) then
          call status%fail(kernel_error,'cannot grow input buffer'); exit
        end if
        larger(:used)=buffer(:used)
        call move_alloc(larger,buffer); capacity=new_capacity
      end if
      nread=int(min(int(capacity-used,int64),file_size-offset))
      read(unit,iostat=ios,iomsg=message) buffer(used+1:used+nread)
      if (ios/=0) then
        call status%fail(kernel_error,'read: '//trim(message)); exit
      end if
      used=used+nread; offset=offset+nread; first=1
      ! Resume at the first new byte: do not rescan an incomplete long record.
      do while(last<=used)
        if (buffer(last)==achar(10)) then
          call self%scan_line(buffer(first:last-1),status)
          if (status%code/=accept) exit
          first=last+1
        end if
        last=last+1
      end do
      if (status%code/=accept) exit
      used=used-first+1
      if (used>0 .and. first>1) buffer(:used)=buffer(first:first+used-1)
      last=used+1
    end do
    close(unit)
    if (status%code/=accept) return
    if (used>0) call self%scan_line(buffer(:used),status)
    if (status%code==accept) call self%end_scan(status)
  end subroutine
  integer function indexed_uint(self,bytes,obj,key,status) result(n)
    class(export_parser), intent(in) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: obj
    character(*), intent(in) :: key
    type(status_t), intent(inout) :: status
    integer :: token
    token=self%tokens%field(bytes,obj,key,status)
    n=int(self%tokens%uint(bytes,token,status))
  end function
  integer function lookup(self,map,idx,status) result(id)
    class(export_parser), intent(in) :: self
    type(index_map_t), intent(in) :: map
    integer, intent(in) :: idx
    type(status_t), intent(inout) :: status
    id=map%get(idx,0,0,0)
    if (id==0) call status%fail(decline,'undefined export reference')
  end function
  subroutine record(self,bytes,status)
    class(export_parser), intent(inout) :: self
    character, intent(in) :: bytes(:)
    type(status_t), intent(inout) :: status
    integer :: t,obj,fmt,version,key,idx,a,b,id,str_id,kind,pre,value,count_fields
    integer(int64) :: num
    character(:), allocatable :: text,record_kind
    if (self%tokens%tag(1)/=jobject) then
      call status%fail(decline,'export record must be an object'); return
    end if
    if (self%records==0) then
      obj=self%tokens%field(bytes,1,'meta',status)
      fmt=self%tokens%field(bytes,obj,'format',status)
      version=self%tokens%field(bytes,fmt,'version',status)
      text=self%tokens%string(bytes,version,status)
      if (status%code/=accept) return
      ! Same stable format range as nanoclo: >=3.1.0 and <3.2.0.
      if (len(text)<5) then
        call status%fail(decline,'unsupported export format'); return
      end if
      if (text(:4)/='3.1.') then
        call status%fail(decline,'unsupported export format'); return
      end if
      do t=5,len(text)
        if (text(t:t)<'0' .or. text(t:t)>'9') then
          call status%fail(decline,'unsupported export format'); return
        end if
      end do
      if (len(text)>5 .and. text(5:5)=='0') call status%fail(decline,'invalid format version')
      return
    end if
    key=0; obj=0; count_fields=0; record_kind=''
    t=2
    do while(t<self%tokens%next(1))
      count_fields=count_fields+1
      text=self%tokens%string(bytes,t,status)
      if (text=='in' .or. text=='il' .or. text=='ie') then
        if (key/=0) then
          call status%fail(decline,'multiple export index fields'); return
        end if
        key=t
      else
        if (obj/=0) then
          call status%fail(decline,'multiple export payloads'); return
        end if
        obj=t+1; record_kind=text
      end if
      t=self%tokens%next(t+1)
    end do
    if (obj==0) then
      call status%fail(decline,'missing export payload'); return
    end if
    select case(record_kind)
    case('str','num')
      if (.not.self%tokens%equals(bytes,key,'in')) then
        call status%fail(decline,'name requires in index'); return
      end if
      idx=int(self%tokens%uint(bytes,key+1,status))
      if (self%name_map%get(idx,0,0,0)/=0) call status%fail(decline,'duplicate name index')
      pre=self%indexed_uint(bytes,obj,'pre',status)
      a=self%lookup(self%name_map,pre,status)
      if (record_kind=='str') then
        str_id=self%tokens%field(bytes,obj,'str',status)
        text=self%tokens%string(bytes,str_id,status)
        if (status%code/=accept) return
        b=self%strings%intern(text)
        id=self%names%str(a,b)
      else
        value=self%tokens%field(bytes,obj,'i',status)
        num=self%tokens%uint(bytes,value,status,4294967295_int64)
        if (status%code/=accept) return
        ! Preserve unsigned bits without an out-of-range integer conversion.
        b=int(iand(num,2147483647_int64),int32)
        if (btest(num,31)) b=ibset(b,31)
        id=self%names%num(a,b)
      end if
      call self%name_map%put(idx,0,0,0,id)
      self%name_records=self%name_records+1
    case('succ','max','imax','param')
      if (.not.self%tokens%equals(bytes,key,'il')) then
        call status%fail(decline,'level requires il index'); return
      end if
      idx=int(self%tokens%uint(bytes,key+1,status))
      if (self%level_map%get(idx,0,0,0)/=0) call status%fail(decline,'duplicate level index')
      b=0
      select case(record_kind)
      case('succ','param')
        pre=int(self%tokens%uint(bytes,obj,status))
        if (record_kind=='succ') then
          a=self%lookup(self%level_map,pre,status); kind=lsucc
        else
          a=self%lookup(self%name_map,pre,status); kind=lparam
        end if
      case default
        if (self%tokens%tag(obj)/=jarray .or. self%tokens%next(obj)/=obj+3) then
          call status%fail(decline,'max/imax requires two levels'); return
        end if
        pre=int(self%tokens%uint(bytes,obj+1,status)); a=self%lookup(self%level_map,pre,status)
        pre=int(self%tokens%uint(bytes,obj+2,status)); b=self%lookup(self%level_map,pre,status)
        kind=lmax
        if (record_kind=='imax') kind=limax
      end select
      if (status%code/=accept) return
      id=self%levels%intern(kind,a,b,0)
      call self%level_map%put(idx,0,0,0,id)
      self%level_records=self%level_records+1
    case('natVal','strVal','mdata','letE','const','app','forallE','lam','proj','sort','bvar')
      if (.not.self%tokens%equals(bytes,key,'ie')) then
        call status%fail(decline,'expression requires ie index'); return
      end if
      idx=int(self%tokens%uint(bytes,key+1,status))
      if (self%expr_map%get(idx,0,0,0)/=0) call status%fail(decline,'duplicate expression index')
      if (status%code/=accept) return
      id=self%expr_record(bytes,obj,record_kind,status)
      if (status%code/=accept) return
      call self%expr_map%put(idx,0,0,0,id)
      self%expr_records=self%expr_records+1
    case('axiom','thm','def','opaque','quot','inductive','ctor','rec')
      if (key/=0) then
        call status%fail(decline,'unexpected declaration index'); return
      end if
      call self%decl_record(bytes,obj,record_kind,status)
      self%decl_records=self%decl_records+1
    case default
      call status%fail(decline,'unsupported export record: '//record_kind)
    end select
  end subroutine
  integer function expr_ref(self,bytes,obj,key,status) result(id)
    class(export_parser), intent(in) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: obj
    character(*), intent(in) :: key
    type(status_t), intent(inout) :: status
    integer :: idx
    idx=self%indexed_uint(bytes,obj,key,status)
    id=self%lookup(self%expr_map,idx,status)
  end function
  integer function level_list(self,bytes,obj,status) result(id)
    class(export_parser), intent(inout) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: obj
    type(status_t), intent(inout) :: status
    integer :: n,t,idx
    id=list_nil
    if (status%code/=accept) return
    if (obj<1 .or. obj>self%tokens%count) then
      call status%fail(decline,'missing level list'); return
    end if
    if (self%tokens%tag(obj)/=jarray) then
      call status%fail(decline,'expected level list'); return
    end if
    n=0; t=obj+1
    do while(t<self%tokens%next(obj))
      idx=int(self%tokens%uint(bytes,t,status))
      n=n+1; call grow(self%scratch_idxs,n)
      self%scratch_idxs(n)=self%lookup(self%level_map,idx,status)
      if (status%code/=accept) return
      t=self%tokens%next(t)
    end do
    if (n>0) id=self%lists%from_array(self%scratch_idxs(:n))
  end function
  integer function expr_record(self,bytes,obj,kind,status) result(id)
    class(export_parser), intent(inout) :: self
    character, intent(in) :: bytes(:)
    integer, intent(in) :: obj
    character(*), intent(in) :: kind
    type(status_t), intent(inout) :: status
    integer :: a,b,c,d,e,t,tag,idx,i
    character(:), allocatable :: text
    id=0; a=0; b=0; c=0; d=0; e=0
    select case(kind)
    case('bvar')
      a=int(self%tokens%uint(bytes,obj,status,65534_int64)); tag=evar
    case('sort')
      idx=int(self%tokens%uint(bytes,obj,status))
      a=self%lookup(self%level_map,idx,status); tag=esort
    case('const')
      idx=self%indexed_uint(bytes,obj,'name',status); a=self%lookup(self%name_map,idx,status)
      t=self%tokens%field(bytes,obj,'us',status); b=self%level_list(bytes,t,status); tag=econst
    case('app')
      a=self%expr_ref(bytes,obj,'fn',status); b=self%expr_ref(bytes,obj,'arg',status); tag=eapp
    case('forallE','lam','letE')
      idx=self%indexed_uint(bytes,obj,'name',status); a=self%lookup(self%name_map,idx,status)
      c=self%expr_ref(bytes,obj,'type',status); d=self%expr_ref(bytes,obj,'body',status)
      if (kind=='letE') then
        e=self%expr_ref(bytes,obj,'value',status)
        t=self%tokens%field(bytes,obj,'nondep',status)
        if (status%code/=accept) return
        if (t==0) then
          call status%fail(decline,'missing nondep flag'); return
        end if
        if (self%tokens%tag(t)/=jtrue .and. self%tokens%tag(t)/=jfalse) then
          call status%fail(decline,'nondep must be Boolean'); return
        end if
        b=merge(1,0,self%tokens%tag(t)==jtrue); tag=elet
      else
        t=self%tokens%field(bytes,obj,'binderInfo',status)
        text=self%tokens%string(bytes,t,status)
        select case(text)
        case('default')
          b=0
        case('implicit')
          b=1
        case('strictImplicit')
          b=2
        case('instImplicit')
          b=3
        case default
          call status%fail(decline,'unknown binder style'); return
        end select
        tag=epi
        if (kind=='lam') tag=elam
      end if
    case('proj')
      idx=self%indexed_uint(bytes,obj,'typeName',status); a=self%lookup(self%name_map,idx,status)
      b=self%indexed_uint(bytes,obj,'idx',status)
      c=self%expr_ref(bytes,obj,'struct',status); tag=eproj
    case('natVal','strVal')
      if ((kind=='natVal' .and. .not.self%nat_extension) .or. &
          (kind=='strVal' .and. .not.self%string_extension)) then
        call status%fail(reject,'literal extension disabled by execution configuration'); return
      end if
      text=self%tokens%string(bytes,obj,status)
      if (status%code/=accept) return
      tag=estr
      if (kind=='natVal') then
        tag=enat
        if (len(text)==0) then
          call status%fail(decline,'empty natural literal'); return
        end if
        do i=1,len(text)
          if (text(i:i)<'0' .or. text(i:i)>'9') then
            call status%fail(decline,'invalid natural literal'); return
          end if
        end do
        i=1
        do while(i<len(text))
          if (text(i:i)/='0') exit
          i=i+1
        end do
        text=text(i:)
      end if
      if (tag==enat) then
        a=self%bignums%from_decimal(text,status)
      else
        a=self%strings%intern(text)
      end if
    case('mdata')
      id=self%expr_ref(bytes,obj,'expr',status); return
    case default
      call status%fail(decline,'unsupported expression'); return
    end select
    if (status%code/=accept) return
    id=self%exprs%intern(tag,a,b,c,d,e)
  end function
  include 'parser_decl.inc'
end module
