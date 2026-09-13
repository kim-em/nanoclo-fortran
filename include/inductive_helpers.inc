  integer function mk_unique(self,n,style,ty) result(e)
    class(checker), intent(inout) :: self
    integer, intent(in) :: n,style,ty
    e=self%ctx%exprs%local(n,style,ty,fvar_unique,self%unique_counter)
    self%unique_counter=self%unique_counter+1
  end function
  integer function index_name(self,n,idx) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: n,idx
    character(32) :: num
    character(:), allocatable :: str
    integer :: p,s
    write(num,'(i0)') idx
    p=n; str=''
    if (self%ctx%names%tag(n)==name_str) then
      p=self%ctx%names%a(n); str=self%ctx%strings%text(self%ctx%names%b(n))
    end if
    s=self%ctx%strings%intern(str//'_'//trim(num)); out=self%ctx%names%str(p,s)
  end function
  integer function list_reverse(self,xs) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: xs
    integer :: k,a
    out=list_nil; k=xs
    do while(k/=list_nil)
      a=self%ctx%lists%head(k); out=self%ctx%lists%cons(a,out); k=self%ctx%lists%tail(k)
    end do
  end function
  logical function list_contains(self,xs,x) result(ok)
    class(checker), intent(in) :: self
    integer, intent(in) :: xs,x
    integer :: k
    ok=.true.; k=xs
    do while(k/=list_nil)
      if (self%ctx%lists%head(k)==x) return
      k=self%ctx%lists%tail(k)
    end do
    ok=.false.
  end function
  logical function list_set_equal(self,xs,ys) result(ok)
    class(checker), intent(in) :: self
    integer, intent(in) :: xs,ys
    integer :: i,k,ls(2),other(2)
    ok=.false.; ls=[xs,ys]; other=[ys,xs]
    do i=1,2
      k=ls(i)
      do while(k/=list_nil)
        if (.not.self%list_contains(other(i),self%ctx%lists%head(k))) return
        k=self%ctx%lists%tail(k)
      end do
    end do
    ok=.true.
  end function
  integer function fold_apps(self,fn,args) result(e)
    class(checker), intent(inout) :: self
    integer, intent(in) :: fn,args
    integer :: k,a
    e=fn; k=args
    do while(k/=list_nil)
      a=self%ctx%lists%head(k); e=self%ctx%exprs%app(e,a); k=self%ctx%lists%tail(k)
    end do
  end function
  subroutine unfold_apps(self,e,fn,args)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e
    integer, intent(out) :: fn,args
    integer :: a
    fn=e; args=list_nil
    do while(self%ctx%exprs%get_tag(fn)==eapp)
      a=self%ctx%exprs%get_b(fn); args=self%ctx%lists%cons(a,args); fn=self%ctx%exprs%get_a(fn)
    end do
  end subroutine
  integer function abstr_binder(self,tag,local,body) result(e)
    class(checker), intent(inout) :: self
    integer, intent(in) :: tag,local,body
    integer :: n,s,ty,b
    e=0
    if (self%status%code/=accept) return
    if (self%ctx%exprs%get_tag(local)/=elocal) then
      call self%status%fail(kernel_error,'abstraction binder is not local'); return
    end if
    n=self%ctx%exprs%get_a(local); s=self%ctx%exprs%get_b(local); ty=self%ctx%exprs%get_c(local)
    b=self%ctx%exprs%abstr(body,[local]); e=self%ctx%exprs%binder(tag,n,s,ty,b)
  end function
  integer function abstr_binders(self,tag,locals,body) result(e)
    class(checker), intent(inout) :: self
    integer, intent(in) :: tag,locals,body
    integer :: k,a
    e=body; k=self%list_reverse(locals)
    do while(k/=list_nil .and. self%status%code==accept)
      a=self%ctx%lists%head(k); e=self%abstr_binder(tag,a,e); k=self%ctx%lists%tail(k)
    end do
  end function
  recursive integer function whnf(self,e) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e
    integer :: v,l
    v=self%nb_of_clo(closure_t(e,env_nil)); v=self%nb_force(0,v); v=self%nb_whnf(0,v)
    out=self%nb_readback(v)
    if (self%status%code/=accept) return
    if (self%ctx%exprs%get_tag(out)==esort) then
      l=self%ctx%exprs%get_a(out); l=self%ctx%levels%simplify(l); out=self%ctx%exprs%sort(l)
    end if
  end function
  subroutine assert_def_eq(self,a,b)
    class(checker), intent(inout) :: self
    integer, intent(in) :: a,b
    logical :: ok
    ok=self%is_def_eq(closure_t(a,env_nil),closure_t(b,env_nil))
    if (.not.ok) call self%status%fail(reject,'declaration reconstruction mismatch')
  end subroutine
  integer function ensure_infers_sort(self,e) result(l)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e
    integer :: t
    t=self%infer(e,.true.); l=self%ensure_sort(t)
  end function
  integer function pi_telescope_size(self,e) result(n)
    class(checker), intent(in) :: self
    integer, intent(in) :: e
    integer :: cur
    cur=e; n=0
    do while(self%ctx%exprs%get_tag(cur)==epi)
      n=n+1; cur=self%ctx%exprs%get_d(cur)
    end do
  end function
  subroutine get_local_params(self,e,np,params,tail)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e,np
    integer, intent(out) :: params,tail
    integer :: i,n,s,ty,body,local
    params=list_nil; tail=e
    do i=1,np
      if (self%status%code/=accept) return
      if (self%ctx%exprs%get_tag(tail)/=epi) then
        call self%status%fail(reject,'exhausted parameter telescope'); return
      end if
      n=self%ctx%exprs%get_a(tail); s=self%ctx%exprs%get_b(tail); ty=self%ctx%exprs%get_c(tail); body=self%ctx%exprs%get_d(tail)
      local=self%mk_unique(n,s,ty); tail=self%ctx%exprs%inst(body,[local]); tail=self%whnf(tail)
      params=self%ctx%lists%cons(local,params)
    end do
    params=self%list_reverse(params)
  end subroutine
  logical function find_const(self,e,ns,prefix) result(ok)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e,ns
    logical, intent(in) :: prefix
    call self%find_cache%clear(); ok=self%find_const_go(e,ns,prefix)
  end function
  recursive logical function find_const_go(self,e,ns,prefix) result(ok)
    class(checker), intent(inout) :: self
    integer, intent(in) :: e,ns
    logical, intent(in) :: prefix
    integer :: cached,t,a,b,c,d,v,n
    cached=self%find_cache%get(e,0,0,0)
    if (cached/=0) then
      ok=cached==1; return
    end if
    t=self%ctx%exprs%get_tag(e); a=self%ctx%exprs%get_a(e); b=self%ctx%exprs%get_b(e)
    c=self%ctx%exprs%get_c(e); d=self%ctx%exprs%get_d(e); v=self%ctx%exprs%get_e(e); ok=.false.
    if (t==econst .or. (prefix .and. t==eproj)) then
      n=a
      if (prefix) then
        do while(self%ctx%names%a(n)/=anonymous .and. n/=anonymous)
          n=self%ctx%names%a(n)
        end do
      end if
      ok=self%list_contains(ns,n)
    end if
    if (.not.ok) then
      select case(t)
      case(eapp)
        ok=self%find_const_go(a,ns,prefix)
        if (.not.ok) ok=self%find_const_go(b,ns,prefix)
      case(epi,elam,elet)
        ok=self%find_const_go(c,ns,prefix)
        if (.not.ok .and. t==elet) ok=self%find_const_go(v,ns,prefix)
        if (.not.ok) ok=self%find_const_go(d,ns,prefix)
      case(elocal,eproj)
        ok=self%find_const_go(c,ns,prefix)
      end select
    end if
    call self%find_cache%put(e,0,0,0,merge(1,2,ok))
  end function
