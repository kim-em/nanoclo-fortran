! Direct translation of nanoclo closure.rs eq_mod. Pair keys contain interned
! (expr,env,offset) triples, with the same env/expr ordering as the Rust key.
  integer function eqm_side(self,e,env,off) result(id)
    class(env_arena), intent(inout) :: self
    integer(int32), intent(in) :: e,env,off
    id=self%eqm_sides%get(e,env,off,0)
    if (id/=0) return
    id=self%eqm_sides%count+1
    call self%eqm_sides%put(e,env,off,0,id)
  end function
  pure logical function is_composite(tag) result(yes)
    integer(int32), intent(in) :: tag
    yes=tag==eapp .or. tag==elam .or. tag==epi .or. tag==elet .or. tag==eproj
  end function
  recursive logical function eq_mod(self,dag,ae,aenv,aoff,be,benv,boff,status) result(ok)
    class(env_arena), intent(inout) :: self
    type(expr_arena), intent(in) :: dag
    integer(int32), intent(in) :: ae,aenv,aoff,be,benv,boff
    type(status_t), intent(inout) :: status
    integer :: at,bt,node,e2,env2,albr,blbr,kaenv,kbenv,kaoff,kboff,ka,kb,cache,tmp
    logical :: composite
    ok=.false.
    if (status%code/=accept) return
    self%ctrs(12)=self%ctrs(12)+1
    at=dag%get_tag(ae)
    if (at==evar) then
      if (dag%get_a(ae)>=aoff) then
        node=self%lookup(aenv,dag%get_a(ae)-aoff,status)
        if (status%code/=accept) return
        e2=self%entry(node); env2=self%entry_env(node)
        select case(self%entry_tag(node))
        case(entry_val)
          ok=self%eq_mod(dag,e2,env2,0,be,benv,boff,status)
        case(entry_neu)
          ok=self%eq_mod_neu(dag,e2,be,benv,boff,status)
        end select
        return
      end if
    end if
    bt=dag%get_tag(be)
    if (bt==evar) then
      if (dag%get_a(be)>=boff) then
        node=self%lookup(benv,dag%get_a(be)-boff,status)
        if (status%code/=accept) return
        e2=self%entry(node); env2=self%entry_env(node)
        select case(self%entry_tag(node))
        case(entry_val)
          ok=self%eq_mod(dag,ae,aenv,aoff,e2,env2,0,status)
        case(entry_neu)
          ok=self%eq_mod_neu(dag,e2,ae,aenv,aoff,status)
        end select
        return
      end if
    end if
    albr=dag%get_nlbv(ae); blbr=dag%get_nlbv(be)
    if (albr<=aoff .and. blbr<=boff .and. aoff==boff) then
      self%ctrs(15)=self%ctrs(15)+1; ok=ae==be; return
    end if
    if (ae==be .and. aenv==benv .and. aoff==boff) then
      self%ctrs(15)=self%ctrs(15)+1; ok=.true.; return
    end if
    if (ae==be .and. albr<=min(aoff,boff)) then
      self%ctrs(15)=self%ctrs(15)+1; ok=.true.; return
    end if
    composite=is_composite(at) .or. is_composite(bt)
    if (composite) then
      kaenv=aenv; kaoff=aoff; kbenv=benv; kboff=boff
      if (albr<=aoff) then
        kaenv=env_nil; kaoff=0
      end if
      if (blbr<=boff) then
        kbenv=env_nil; kboff=0
      end if
      ka=self%eqm_side(ae,kaenv,kaoff); kb=self%eqm_side(be,kbenv,kboff)
      if (kaenv>kbenv .or. (kaenv==kbenv .and. ae>be)) then
        tmp=ka; ka=kb; kb=tmp
      end if
      cache=self%eq_mod_cache%get(ka,kb,0,0)
      if (cache/=0) then
        self%ctrs(13)=self%ctrs(13)+1; ok=cache==1; return
      end if
      self%ctrs(14)=self%ctrs(14)+1
    else
      self%ctrs(16)=self%ctrs(16)+1
    end if
    if (at==bt) then
      select case(at)
      case(evar)
        ok=dag%get_a(ae)==dag%get_a(be)
      case(elocal)
        ok=ae==be
      case(econst)
        ok=dag%get_a(ae)==dag%get_a(be) .and. dag%get_b(ae)==dag%get_b(be)
      case(esort,enat,estr)
        ok=dag%get_a(ae)==dag%get_a(be)
      case(eapp)
        ok=self%eq_mod(dag,dag%get_a(ae),aenv,aoff,dag%get_a(be),benv,boff,status)
        if (ok) ok=self%eq_mod(dag,dag%get_b(ae),aenv,aoff,dag%get_b(be),benv,boff,status)
      case(elam,epi,elet)
        ok=self%eq_mod(dag,dag%get_c(ae),aenv,aoff,dag%get_c(be),benv,boff,status)
        if (ok .and. at==elet) ok=self%eq_mod(dag,dag%get_e(ae),aenv,aoff,dag%get_e(be),benv,boff,status)
        if (ok) ok=self%eq_mod(dag,dag%get_d(ae),aenv,aoff+1,dag%get_d(be),benv,boff+1,status)
      case(eproj)
        ok=dag%get_a(ae)==dag%get_a(be) .and. dag%get_b(ae)==dag%get_b(be)
        if (ok) ok=self%eq_mod(dag,dag%get_c(ae),aenv,aoff,dag%get_c(be),benv,boff,status)
      end select
    end if
    if (composite .and. status%code==accept) call self%eq_mod_cache%put(ka,kb,0,0,merge(1,2,ok))
  end function
  recursive logical function eq_mod_neu(self,dag,fv,e,env,off,status) result(ok)
    class(env_arena), intent(inout) :: self
    type(expr_arena), intent(in) :: dag
    integer(int32), intent(in) :: fv,e,env,off
    type(status_t), intent(inout) :: status
    integer :: node,e2,env2
    ok=.false.
    if (status%code/=accept) return
    if (dag%get_tag(e)==evar) then
      if (dag%get_a(e)>=off) then
        node=self%lookup(env,dag%get_a(e)-off,status)
        if (status%code/=accept) return
        e2=self%entry(node); env2=self%entry_env(node)
        select case(self%entry_tag(node))
        case(entry_val)
          ok=self%eq_mod_neu(dag,fv,e2,env2,0,status)
        case(entry_neu)
          ok=fv==e2
        end select
      end if
      return
    end if
    ok=e==fv
  end function
