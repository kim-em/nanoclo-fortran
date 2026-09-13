! String literals expand through String.ofList and Char.ofNat as in expr.rs.
  recursive integer function nb_str_to_ctor(self,depth,s) result(out)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,s
    integer :: empty,ls,cc,co,cn,cons,e,x,p,first,last,i,j,b,cp,lead
    out=0
    if (.not.self%nat_extension .or. .not.self%string_extension) return
    if (self%char_name==0 .or. self%char_of_nat==0 .or. self%list_nil_name==0 .or. &
        self%list_cons_name==0 .or. self%string_of_list==0) return
    empty=list_nil; ls=self%ctx%lists%cons(1,list_nil)
    cc=self%ctx%exprs%const(self%char_name,empty); co=self%ctx%exprs%const(self%char_of_nat,empty)
    cn=self%ctx%exprs%const(self%list_nil_name,ls); e=self%ctx%exprs%app(cn,cc)
    cons=self%ctx%exprs%const(self%list_cons_name,ls); cons=self%ctx%exprs%app(cons,cc)
    first=self%ctx%strings%start(s); last=first+self%ctx%strings%length(s)-1
    do while(last>=first)
      i=last
      do while(i>first)
        b=self%ctx%strings%bytes(i)
        if (b<128 .or. b>=192) exit
        i=i-1
      end do
      lead=self%ctx%strings%bytes(i)
      select case(last-i)
      case(0)
        cp=lead
      case(1)
        cp=iand(lead,31)
      case(2)
        cp=iand(lead,15)
      case(3)
        cp=iand(lead,7)
      case default
        call self%status%fail(kernel_error,'invalid internal UTF-8 string'); return
      end select
      do j=i+1,last
        cp=cp*64+iand(self%ctx%strings%bytes(j),63)
      end do
      p=self%ctx%bignums%from_u64(int(cp,int64)); x=self%ctx%exprs%intern(enat,p,0,0,0,0)
      x=self%ctx%exprs%app(co,x); x=self%ctx%exprs%app(cons,x); e=self%ctx%exprs%app(x,e); last=i-1
    end do
    x=self%ctx%exprs%const(self%string_of_list,empty); e=self%ctx%exprs%app(x,e)
    out=self%nb_eval(depth,env_nil,e); out=self%nb_whnf(depth,out)
  end function
  logical function nb_is_string_ctor(self,v) result(yes)
    class(checker), intent(in) :: self
    integer, intent(in) :: v
    integer :: n,h
    yes=.false.
    if (self%nb%tag(v)==vunfold) then
      n=self%nb%name(v)
    else if (self%nb%tag(v)==vrigid) then
      h=self%nb%head(v)
      if (self%nb%head_tag(h)/=hconst) return
      n=self%nb%head_b(h)
    else
      return
    end if
    yes=n==self%string_mk .or. n==self%string_of_list
  end function
  recursive integer function nb_conv_str(self,rigid,depth,x,y) result(answer)
    class(checker), intent(inout) :: self
    logical, intent(in) :: rigid
    integer, intent(in) :: depth,x,y
    logical :: lit_x,lit_y,ok
    integer :: lit,other,s,c
    answer=-1; lit_x=self%nb%tag(x)==vstr; lit_y=self%nb%tag(y)==vstr
    if (lit_x .eqv. lit_y) return
    lit=merge(x,y,lit_x); other=merge(y,x,lit_x)
    if (.not.self%nb_is_string_ctor(other)) return
    s=self%nb%literal(lit); c=self%nb_str_to_ctor(depth,s)
    if (c==0 .or. self%status%code/=accept) return
    ok=self%nb_unify(rigid,depth,c,other); answer=merge(1,0,ok)
  end function
