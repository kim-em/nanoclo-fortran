! Included inside an arena's intern(self,tag,a,b,c) function. Arena fields are
! parallel arrays; the shared key layout is (tag,a,b,c), never a derived node.
    id=self%nodes%get(tag,a,b,c)
    if (id/=0) return
    id=self%count+1
    call grow(self%tag,id)
    call grow(self%a,id)
    call grow(self%b,id)
    call grow(self%c,id)
    self%tag(id)=tag; self%a(id)=a; self%b(id)=b; self%c(id)=c
    self%count=id
    call self%nodes%put(tag,a,b,c,id)
