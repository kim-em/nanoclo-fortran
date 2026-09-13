! Conversion order and speculative spine probes translated from nbe_conv.rs.
! Inductive/literal-specific routes are connected in M4/M5, not replaced by
! eager normalization or a different conversion algorithm.
  recursive logical function nb_conv(self,depth,x,y) result(ok)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,x,y
    self%nb%in_conv=self%nb%in_conv+1
    ok=self%nb_unify(.true.,depth,x,y)
    self%nb%in_conv=self%nb%in_conv-1
  end function
  logical function cacheable(self,v) result(ok)
    class(checker), intent(in) :: self
    integer, intent(in) :: v
    integer :: t,h
    t=self%nb%tag(v); ok=t==vpi .or. t==vlam .or. t==vunfold
    if (t==vrigid) then
      h=self%nb%head(v)
      ok=self%nb%head_tag(h)==hconst .and. (self%nb%head_a(h)==crecursor .or. self%nb%head_a(h)==cquot)
    end if
  end function
  recursive logical function nb_unify(self,rigid,depth,x_in,y_in) result(ok)
    class(checker), intent(inout) :: self
    logical, intent(in) :: rigid
    integer, intent(in) :: depth,x_in,y_in
    integer :: x,y,a,b
    logical :: neg
    ok=.false.
    if (self%status%code/=accept) return
    self%rp%ctrs(12)=self%rp%ctrs(12)+1
    x=self%nb_force(depth,x_in); y=self%nb_force(depth,y_in)
    if (self%status%code/=accept) return
    if (x==y) then
      ok=.true.; return
    end if
    if (self%nb%probe_depth>0) then
      if (self%nb%probe_aborted) return
      if (self%nb%probe_fuel==0) then
        self%nb%probe_aborted=.true.; self%rp%ctrs(24)=self%rp%ctrs(24)+1; return
      end if
      self%nb%probe_fuel=self%nb%probe_fuel-1
    end if
    if (.not.(self%cacheable(x) .or. self%cacheable(y))) then
      ok=self%nb_unify_go(rigid,depth,x,y); return
    end if
    a=min(x,y); b=max(x,y)
    if (self%nb%conv_pos%get(a,b,0,0)/=0) then
      ok=.true.; return
    end if
    neg=self%nb%tag(x)/=vlam .and. self%nb%tag(y)/=vlam
    if (rigid .and. neg) then
      if (self%nb%conv_neg%get(a,b,0,0)/=0) return
      if (self%nb%probe_depth>0) then
        if (self%nb%conv_neg_probe%get(a,b,0,0)/=0) return
      end if
    end if
    ok=self%nb_unify_go(rigid,depth,x,y)
    if (self%status%code/=accept .or. self%nb%probe_aborted) return
    if (ok) then
      call self%nb%conv_pos%put(a,b,0,0,1)
    else if (rigid .and. neg) then
      if (self%nb%probe_depth==0) then
        call self%nb%conv_neg%put(a,b,0,0,1)
      else
        call self%nb%conv_neg_probe%put(a,b,0,0,1)
      end if
    end if
  end function
  recursive logical function nb_unify_go(self,rigid,depth,x,y) result(ok)
    class(checker), intent(inout) :: self
    logical, intent(in) :: rigid
    integer, intent(in) :: depth,x,y
    integer :: tx,ty,dom,fresh,lhs,rhs,answer
    ok=.false.; answer=self%nb_conv_nat(rigid,depth,x,y)
    if (answer/=-1 .or. self%status%code/=accept) then
      ok=answer==1; return
    end if
    answer=self%nb_conv_str(rigid,depth,x,y)
    if (answer/=-1 .or. self%status%code/=accept) then
      ok=answer==1; return
    end if
    ok=self%nb_unify_direct(rigid,depth,x,y)
    if (ok .or. .not.rigid .or. self%status%code/=accept) return
    ok=self%nb_proof_irrel(depth,x,y)
    if (ok .or. self%status%code/=accept) return
    tx=self%nb%tag(x); ty=self%nb%tag(y)
    if (tx==vlam .and. ty/=vlam) then
      dom=self%nb_lam_domain(depth,x)
      if (self%status%code/=accept) return
      fresh=self%nb%mk_bvar(depth,dom)
      lhs=self%nb_open(depth+1,x,fresh); rhs=self%nb_apply(depth+1,y,fresh)
      ok=self%nb_unify(.true.,depth+1,lhs,rhs); return
    else if (tx/=vlam .and. ty==vlam) then
      dom=self%nb_lam_domain(depth,y)
      if (self%status%code/=accept) return
      fresh=self%nb%mk_bvar(depth,dom)
      lhs=self%nb_apply(depth+1,x,fresh); rhs=self%nb_open(depth+1,y,fresh)
      ok=self%nb_unify(.true.,depth+1,lhs,rhs); return
    end if
    ok=self%nb_struct_eta(depth,x,y)
  end function
  recursive logical function nb_unify_direct(self,rigid,depth,x,y) result(ok)
    class(checker), intent(inout) :: self
    logical, intent(in) :: rigid
    integer, intent(in) :: depth,x,y
    integer :: tx,ty,hx,hy,sx,sy,lx,ly,nx,ny,dx,dy,fresh,bx,by,kx,ky,ix,iy
    logical :: matches
    ok=.false.
    tx=self%nb%tag(x); ty=self%nb%tag(y)
    if (tx==vsort .and. ty==vsort) then
      lx=self%nb%level(x); ly=self%nb%level(y); ok=self%ctx%levels%eq_antisymm(lx,ly)
    else if ((tx==vnat .and. ty==vnat) .or. (tx==vstr .and. ty==vstr)) then
      ok=self%nb%literal(x)==self%nb%literal(y)
    else if (tx==vpi .and. ty==vpi) then
      dx=self%nb%domain(x); dy=self%nb%domain(y)
      ok=self%nb_unify(rigid,depth,dx,dy)
      if (.not.ok .or. self%status%code/=accept) return
      dx=self%nb_force(depth,dx)
      if (self%status%code/=accept) return
      fresh=self%nb%mk_bvar(depth,dx)
      bx=self%nb_open(depth+1,x,fresh); by=self%nb_open(depth+1,y,fresh)
      ok=self%nb_unify(rigid,depth+1,bx,by)
    else if (tx==vlam .and. ty==vlam) then
      dx=self%nb_lam_domain(depth,x)
      if (self%status%code/=accept) return
      fresh=self%nb%mk_bvar(depth,dx)
      bx=self%nb_open(depth+1,x,fresh); by=self%nb_open(depth+1,y,fresh)
      ok=self%nb_unify(rigid,depth+1,bx,by)
    else if (tx==vrigid .and. ty==vrigid) then
      hx=self%nb%head(x); hy=self%nb%head(y); sx=self%nb%spine(x); sy=self%nb%spine(y)
      kx=self%nb%head_tag(hx); ky=self%nb%head_tag(hy); matches=.false.
      if (kx==hconst .and. ky==hconst) then
        if (self%nb%head_a(hx)==self%nb%head_a(hy) .and. self%nb%head_b(hx)==self%nb%head_b(hy)) then
          lx=self%nb%head_c(hx); ly=self%nb%head_c(hy); matches=self%levels_equal(lx,ly)
        end if
      end if
      if (self%iota_head(hx) .or. self%iota_head(hy)) then
        ok=self%nb_unify_iota(rigid,depth,x,y,matches,sx,sy); return
      end if
      if (kx==hconst .or. ky==hconst) then
        ok=matches
      else
        ok=kx==ky .and. self%nb%head_a(hx)==self%nb%head_a(hy)
      end if
      if (ok) ok=self%nb_unify_spine(rigid,depth,sx,sy)
    else if (tx==vunfold .and. ty==vunfold) then
      nx=self%nb%name(x); ny=self%nb%name(y); lx=self%nb%levels(x); ly=self%nb%levels(y)
      sx=self%nb%spine(x); sy=self%nb%spine(y); matches=.false.
      if (nx==ny) matches=self%levels_equal(lx,ly)
      if (.not.rigid) then
        if (matches) ok=self%nb_unify_spine(.false.,depth,sx,sy)
        return
      end if
      if (matches) then
        ok=self%nb_spine_probe(depth,sx,sy)
        if (ok) return
      end if
      ok=self%nb_proof_irrel(depth,x,y)
      if (ok .or. self%status%code/=accept) return
      if (matches) then
        ok=self%nb_unfold_pair(depth,x,y); self%nb%probe_escalate=0; return
      end if
      ix=self%ctx%decls%lookup(nx,self%cutoff); iy=self%ctx%decls%lookup(ny,self%cutoff)
      kx=hint_opaque; ky=hint_opaque; hx=0; hy=0
      if (ix/=0) then
        if (self%ctx%decls%tag(ix)==ddefinition) then
          kx=self%ctx%decls%hint(ix); hx=self%ctx%decls%height(ix)
        end if
      end if
      if (iy/=0) then
        if (self%ctx%decls%tag(iy)==ddefinition) then
          ky=self%ctx%decls%hint(iy); hy=self%ctx%decls%height(iy)
        end if
      end if
      if (hint_lt(kx,hx,ky,hy)) then
        ok=self%nb_unfold_one(.false.,depth,x,y)
      else if (hint_lt(ky,hy,kx,hx)) then
        ok=self%nb_unfold_one(.true.,depth,x,y)
      else
        ok=self%nb_unfold_pair(depth,x,y)
      end if
    else if (tx==vunfold .and. rigid) then
      ok=self%nb_proof_irrel(depth,x,y)
      if (ok .or. self%status%code/=accept) return
      bx=self%nb_unfold(depth,x)
      if (bx==x) then
        bx=self%nb_unfold_demand(depth,x)
        if (bx==x) return
      end if
      ok=self%nb_unify(.true.,depth,bx,y)
    else if (ty==vunfold .and. rigid) then
      ok=self%nb_proof_irrel(depth,x,y)
      if (ok .or. self%status%code/=accept) return
      by=self%nb_unfold(depth,y)
      if (by==y) then
        by=self%nb_unfold_demand(depth,y)
        if (by==y) return
      end if
      ok=self%nb_unify(.true.,depth,x,by)
    else if (rigid) then
      if (tx==vrigid) then
        hx=self%nb%head(x)
        if (self%iota_head(hx)) then
          ok=self%nb_proof_irrel(depth,x,y)
          if (ok .or. self%status%code/=accept) return
          bx=self%nb_iota(depth,x)
          if (bx/=0) ok=self%nb_unify(.true.,depth,bx,y)
          return
        end if
      end if
      if (ty==vrigid) then
        hy=self%nb%head(y)
        if (self%iota_head(hy)) then
          ok=self%nb_proof_irrel(depth,x,y)
          if (ok .or. self%status%code/=accept) return
          by=self%nb_iota(depth,y)
          if (by/=0) ok=self%nb_unify(.true.,depth,x,by)
        end if
      end if
    end if
  end function
  logical function iota_head(self,h) result(ok)
    class(checker), intent(in) :: self
    integer, intent(in) :: h
    ok=self%nb%head_tag(h)==hconst .and. (self%nb%head_a(h)==crecursor .or. self%nb%head_a(h)==cquot)
  end function
  recursive logical function nb_unfold_one(self,left,depth,x,y) result(ok)
    class(checker), intent(inout) :: self
    logical, intent(in) :: left
    integer, intent(in) :: depth,x,y
    integer :: first,second,f,s
    ok=.false.; first=merge(x,y,left); second=merge(y,x,left)
    f=self%nb_unfold(depth,first)
    if (self%status%code/=accept) return
    if (f/=first) then
      if (left) then
        ok=self%nb_unify(.true.,depth,f,y)
      else
        ok=self%nb_unify(.true.,depth,x,f)
      end if
      return
    end if
    s=self%nb_unfold(depth,second)
    if (self%status%code/=accept) return
    if (s/=second) then
      if (left) then
        ok=self%nb_unify(.true.,depth,first,s)
      else
        ok=self%nb_unify(.true.,depth,s,first)
      end if
      return
    end if
    f=self%nb_unfold_demand(depth,first)
    if (f==first .or. self%status%code/=accept) return
    if (left) then
      ok=self%nb_unify(.true.,depth,f,second)
    else
      ok=self%nb_unify(.true.,depth,second,f)
    end if
  end function
  recursive logical function nb_unfold_pair(self,depth,x,y) result(ok)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,x,y
    integer :: a,b
    ok=.false.; a=self%nb_unfold(depth,x); b=self%nb_unfold(depth,y)
    if (self%status%code/=accept) return
    if (a==x .and. b==y) then
      a=self%nb_unfold_demand(depth,x); b=self%nb_unfold_demand(depth,y)
      if (a==x .and. b==y) return
    end if
    ok=self%nb_unify(.true.,depth,a,b)
  end function
  recursive logical function nb_spine_probe(self,depth,sx,sy) result(ok)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,sx,sy
    integer :: a,b
    integer(int64) :: granted
    a=min(sx,sy); b=max(sx,sy); ok=.false.
    if (self%nb%probe_fail%get(a,b,0,0)/=0) then
      self%rp%ctrs(26)=self%rp%ctrs(26)+1; return
    end if
    granted=0
    if (self%nb%probe_depth==0) then
      granted=4096
      if (self%nb%probe_escalate>0) then
        granted=self%nb%probe_escalate; self%nb%probe_escalate=0
      end if
      self%nb%probe_fuel=granted; self%nb%probe_aborted=.false.
    end if
    self%nb%probe_depth=self%nb%probe_depth+1
    ok=self%nb_unify_spine(.true.,depth,sx,sy)
    self%nb%probe_depth=self%nb%probe_depth-1
    if (self%nb%probe_depth==0) then
      call self%nb%conv_neg_probe%clear()
      if (self%nb%probe_aborted) then
        self%nb%probe_aborted=.false.; self%rp%ctrs(25)=self%rp%ctrs(25)+1
        call self%nb%probe_fail%put(a,b,0,0,1)
        self%nb%probe_escalate=min(granted*2,1048576_int64); ok=.false.; return
      end if
      if (.not.ok .and. self%status%code==accept) call self%nb%probe_fail%put(a,b,0,0,1)
    end if
  end function
  recursive logical function nb_unify_spine(self,rigid,depth,sx,sy) result(ok)
    class(checker), intent(inout) :: self
    logical, intent(in) :: rigid
    integer, intent(in) :: depth,sx,sy
    ok=.false.
    if (self%status%code/=accept) return
    if (sx==sy) then
      ok=.true.; return
    end if
    if (self%nb%spines%len(sx)/=self%nb%spines%len(sy)) return
    ok=self%nb_unify_spine_go(rigid,depth,sx,sy)
  end function
  recursive logical function nb_unify_spine_go(self,rigid,depth,sx,sy) result(ok)
    class(checker), intent(inout) :: self
    logical, intent(in) :: rigid
    integer, intent(in) :: depth,sx,sy
    integer :: px,py,tx,ty,ax,ay,bx,by
    ok=.true.
    if (sx==sy) return
    px=self%nb%spines%parent(sx); py=self%nb%spines%parent(sy)
    tx=self%nb%spines%tag(sx); ty=self%nb%spines%tag(sy)
    ax=self%nb%spines%a(sx); ay=self%nb%spines%a(sy); bx=self%nb%spines%b(sx); by=self%nb%spines%b(sy)
    ok=self%nb_unify_spine_go(rigid,depth,px,py)
    if (.not.ok .or. self%status%code/=accept) return
    ok=.false.
    if (tx==elim_app .and. ty==elim_app) then
      ok=self%nb_unify(rigid,depth,ax,ay)
    else if (tx==elim_proj .and. ty==elim_proj) then
      ok=ax==ay .and. bx==by
    end if
  end function
  recursive logical function nb_proof_irrel(self,depth,x,y) result(ok)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,x,y
    integer :: tx,ty,lam,domain,fresh,a,b,l
    ok=.false.
    if (self%status%code/=accept) return
    tx=self%nb%tag(x); ty=self%nb%tag(y)
    if (tx==vlam .or. ty==vlam) then
      lam=merge(x,y,tx==vlam); domain=self%nb_lam_domain(depth,lam)
      if (self%status%code/=accept) return
      fresh=self%nb%mk_bvar(depth,domain)
      a=self%nb_apply(depth+1,x,fresh); b=self%nb_apply(depth+1,y,fresh)
      ok=self%nb_proof_irrel(depth+1,a,b); return
    end if
    if (tx/=vrigid .and. tx/=vunfold) return
    if (ty/=vrigid .and. ty/=vunfold) return
    a=self%nb_type(depth,x); l=self%nb_type_level(depth,a)
    if (l==0 .or. self%status%code/=accept) return
    if (.not.self%ctx%levels%is_zero(l)) return
    b=self%nb_type(depth,y); l=self%nb_type_level(depth,b)
    if (l==0 .or. self%status%code/=accept) return
    if (.not.self%ctx%levels%is_zero(l)) return
    ok=self%nb_unify(.true.,depth,a,b)
  end function
  recursive logical function nb_unify_iota(self,rigid,depth,x,y,heads_match,sx,sy) result(ok)
    class(checker), intent(inout) :: self
    logical, intent(in) :: rigid,heads_match
    integer, intent(in) :: depth,x,y,sx,sy
    integer :: x2,y2
    ok=.false.
    if (.not.rigid) then
      if (heads_match) ok=self%nb_unify_spine(.false.,depth,sx,sy)
      return
    end if
    if (heads_match) then
      ok=self%nb_spine_probe(depth,sx,sy)
      if (ok) return
    end if
    ok=self%nb_proof_irrel(depth,x,y)
    if (ok .or. self%status%code/=accept) return
    x2=self%nb_iota(depth,x); y2=self%nb_iota(depth,y)
    if (self%status%code/=accept) return
    if (x2==0) x2=x
    if (y2==0) y2=y
    if (x2/=x .or. y2/=y) then
      ok=self%nb_unify(.true.,depth,x2,y2); self%nb%probe_escalate=0; return
    end if
    if (heads_match) ok=self%nb_unify_spine(.true.,depth,sx,sy)
  end function
  recursive logical function nb_struct_eta(self,depth,x,y) result(ok)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,x,y
    integer :: vs(2),i,v,t,h,n,id,ctor
    ok=.false.; vs=[x,y]
    do i=1,2
      if (self%status%code/=accept) return
      v=vs(i)
      if (self%nb%tag(v)==vpi .or. self%nb%tag(v)==vlam) cycle
      t=self%nb_type(depth,v); t=self%nb_whnf(depth,t); h=self%as_inductive(t)
      if (h==0) cycle
      n=self%nb%head_b(h); id=self%get_structure(n,.false.)
      if (id==0) cycle
      ctor=self%ctx%lists%head(self%ctx%decls%ctor_names(id)); id=self%ctx%decls%lookup(ctor,self%cutoff)
      if (id/=0) then
        if (self%ctx%decls%tag(id)==dconstructor .and. self%ctx%decls%num_fields(id)==0) then
          ok=.true.; return
        end if
      end if
      ok=self%nb_eta_struct(depth,n,x,y)
      if (ok .or. self%status%code/=accept) return
      ok=self%nb_eta_struct(depth,n,y,x)
      if (ok) return
    end do
  end function
  recursive logical function nb_eta_struct(self,depth,n,x,y) result(ok)
    class(checker), intent(inout) :: self
    integer, intent(in) :: depth,n,x,y
    integer :: h,ctor,id,np,nf,s,args,mark,i,a,proj
    ok=.false.
    if (self%status%code/=accept) return
    if (self%nb%tag(y)/=vrigid) return
    h=self%nb%head(y)
    if (self%nb%head_tag(h)/=hconst .or. self%nb%head_a(h)/=cctor) return
    ctor=self%nb%head_b(h); id=self%ctx%decls%lookup(ctor,self%cutoff)
    if (id==0) return
    if (self%ctx%decls%tag(id)/=dconstructor .or. self%ctx%decls%inductive_name(id)/=n) return
    np=self%ctx%decls%num_params(id); nf=self%ctx%decls%num_fields(id); s=self%nb%spine(y)
    if (self%nb%spines%len(s)/=np+nf) return
    mark=self%work_top; args=self%spine_args(s)
    if (args<0) return
    ok=.true.
    do i=0,nf-1
      proj=self%nb_proj(depth,n,i,x); a=self%w1(args+np+i+1); ok=self%nb_unify(.true.,depth,proj,a)
      if (.not.ok .or. self%status%code/=accept) exit
    end do
    self%work_top=mark
  end function
