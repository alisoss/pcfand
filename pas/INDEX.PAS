
{ XString }

procedure XString.Clear; assembler;
asm les di,Self; mov es:[di].byte,0 end;
procedure XString.StoreReal(R:real; KF:KeyFldDPtr);
var A:array[1..20] of byte; n:word; b:boolean;
const TabF:array[1..18]of byte=(1,1,2,2,3,3,4,4,4,5,5,6,6,6,7,7,8,8);
begin with KF^.FldD^ do begin
  if Typ in ['R','D'] then begin
    b:=KF^.Descend; if R<0 then begin b:=not b; R:=-R end;
    Self.StoreD(R,b); exit end;
  if Flg and f_Comma=0 then R:=R*Power10[M]; n:=L-1; if M>0 then dec(n); n:=TabF[n];
  FixFromReal(R,A,n); StoreF(A,n,KF^.Descend);
end end;
procedure XString.StoreStr(V:string; KF:KeyFldDPtr);
var n:word;
begin with KF^.FldD^ do begin
  while ord(V[0])<L do if M=LeftJust then V:=V+' ' else V:=' '+V;
  if Typ='N' then begin
    Pack(V[1],V,L); n:=(L+1)div 2; StoreN(V,n,KF^.Descend) end
  else StoreA(V[1],L,KF^.CompLex,KF^.Descend);
end end;
procedure XString.StoreBool(B:boolean; KF:KeyFldDPtr);
begin
  StoreN(B,1,KF^.Descend);
end;
procedure XString.StoreKF(KF:KeyFldDPtr);
var F:FieldDPtr;
begin
  F:=KF^.FldD; case F^.FrmlTyp of
   'S':StoreStr(_shorts(F),KF);
   'R':StoreReal(_r(F),KF);
   'B':StoreBool(_b(F),KF) end;
end;
procedure XString.PackKF(KF:KeyFldDPtr);
begin
  Clear; while KF<>nil do begin StoreKF(KF); KF:=KF^.Chain end;
end;
function XString.PackFrml(FL:FrmlList; KF:KeyFldDPtr):boolean;
var Z:FrmlPtr;
begin
  Clear; while FL<>nil do begin Z:=FL^.Frml; case KF^.FldD^.FrmlTyp of
   'S':StoreStr(RunShortStr(Z),KF);
   'R':StoreReal(RunReal(Z),KF);
   'B':StoreBool(RunBool(Z),KF) end;
   KF:=KF^.Chain; FL:=FL^.Chain end;
  PackFrml:=KF<>nil;
end;
procedure NegateESDI; assembler;
asm  jcxz @2; @1:not es:[di].byte; inc di; loop @1; @2:
end;
procedure XString.StoreD(var R; Descend:boolean); assembler;
asm  les di,Self; mov al,es:[di]; mov bl,al; add al,6;
     cmp al,bl; jb @1; mov es:[di],al; push ds;
     xor bh,bh; lds si,R; mov al,[si]; mov ah,[si+5]; inc di; add di,bx;
     shl ah,1; cmc; rcr al,1; rcr ah,1; mov es:[di].word,ax;
     mov ax,[si+3]; xchg al,ah; mov es:[di+2].word,ax;
     mov ax,[si+1]; xchg al,ah; mov es:[di+4].word,ax; pop ds;
     cmp Descend,0; je @1; mov cx,6; call NegateESDI; @1:
end;
procedure XString.StoreN(var N; Len:word; Descend:boolean); assembler;
asm  les di,Self; xor bh,bh; mov bl,es:[di]; mov cx,Len;
     mov al,bl; add al,cl; cmp al,bl; jb @1; mov es:[di],al; push ds;
     inc di; add di,bx; push di; cld; lds si,N; rep movsb; pop di; pop ds;
     cmp Descend,0; je @1; mov cx,Len; call NegateESDI; @1:
end;
procedure XString.StoreF(var F; Len:word; Descend:boolean); assembler;
asm  les di,Self; xor bh,bh; mov bl,es:[di]; mov cx,Len; mov al,bl;
     add al,cl; cmp al,bl; jb @1; mov es:[di],al; push ds;
     inc di; add di,bx; push di;
     cld; lds si,F; lodsb; shl al,1; cmc; rcr al,1; stosb; dec cx;
     rep movsb; pop di; pop ds;
     cmp Descend,0; je @1; mov cx,Len; call NegateESDI; @1:
end;
procedure XString.StoreA(var A; Len:word; CompLex,Descend:boolean); assembler;
var V:string;
asm  lea di,V; mov cx,Len; cld; cmp CompLex,0; jne @1;
     push ds; lds si,A; mov ax,ss; mov es,ax;
     mov ax,cx; stosb; rep movsb; pop ds; jmp @2;
@1:  les si,A; dec si; mov dx,0; call TranslateOrd;
@2:  mov si,ax;
@3:  cmp si,0; je @4; cmp V[si].byte,20H; jne @4; dec si; jmp @3;
@4:  cmp si,ax; je @5; inc si; mov V[si].byte,1fH;
@5:  mov cx,si; les di,Self; xor bh,bh; mov bl,es:[di]; mov al,bl;
     add al,cl; cmp al,bl; jb @6; mov es:[di],al; inc di; add di,bx;
     push ds; push cx; push di;
     mov ax,ss; mov ds,ax; lea si,V[1]; rep movsb; pop di; pop cx; pop ds;
     cmp Descend,0; je @6; call NegateESDI; @6:
end;
{$ifdef FandSQL}
procedure XString.GetF(Off,Len:word; Descend:boolean; var Buf); assembler;
asm  push ds; lds si,Self; add si,Off; mov cx,Len; les di,Buf; cld;
     lodsb; shl al,1; cmc; rcr al,1; stosb; dec cx; rep movsb;
     cmp Descend,0; je @1; les di,Buf; mov cx,Len; call NegateESDI;
@1:  pop ds;
end;
procedure XString.GetD(Off:word; Descend:boolean; var R); assembler;
asm  push ds; lds si,Self; add si,Off; les di,R; cld;
     lodsw; shl ah,1; rcl al,1; cmc; rcr ah,1;
     mov es:[di],al; mov es:[di+5],ah;
     lodsw; xchg al,ah; mov es:[di+3],ax;
     lodsw; xchg al,ah; mov es:[di+1],ax;
     cmp Descend,0; je @1; mov cx,6; call NegateESDI;
@1:  pop ds;
end;
procedure XString.GetN(Off,Len:word; Descend:boolean; var Buf); assembler;
asm  push ds; lds si,Self; add si,Off; mov cx,Len; les di,Buf; cld;
     rep movsb;
     cmp Descend,0; je @1; les di,Buf; mov cx,Len; call NegateESDI;
@1:  pop ds;
end;
function XString.GetA(Off,Len:word; CompLex,Descend:boolean; var Buf):word; assembler;
asm  push ds; lds si,Self; add si,Off; mov cx,Len; mov dx,cx; les di,Buf; cld;
@1:  lodsb; cmp Descend,0; je @2; not al;
@2:  cmp al,1fH; je @5;
     cmp CompLex,0; je @3; call TranslateOrdBack; jmp @4;
@3:  stosb;
@4:  loop @1;
     jmp @6;
@5:  inc dx; sub dx,cx; mov al,20H; rep stosb;
@6:  mov ax,dx; pop ds;
end;
{$endif}


{ XItem }

function XItem.GetN:longint; assembler;
asm les bx,Self; mov ax,es:[bx]; mov dl,es:[bx+2]; xor dh,dh end;

procedure XItem.PutN(N:longint); assembler;
asm les bx,Self; mov ax,N.word; mov es:[bx],ax; mov al,N[2].byte;
    mov es:[bx+2],al end;
                                                                
function XItem.GetM(O:word):word; assembler;
asm les bx,Self; add bx,O; xor ah,ah; mov al,es:[bx] end;

procedure XItem.PutM(O:word;M:word); assembler;
asm les bx,Self; add bx,O; mov ax,M; mov es:[bx],al end;

function XItem.GetL(O:word):word; assembler;
asm les bx,Self; add bx,O; xor ah,ah; mov al,es:[bx+1] end;

procedure XItem.PutL(O:word;L:word); assembler;
asm les bx,Self; add bx,O; mov ax,L; mov es:[bx+1],al end;

function XItem.Next(O:word):XItemPtr; assembler;
asm les bx,Self; add bx,O; xor ah,ah; mov al,es:[bx+1]; add ax,bx; add ax,2;
    mov dx,es end;

function XItem.UpdStr(O:word;S:StringPtr):word; assembler;
asm  push ds; lds bx,Self; les di,S; cld; add bx,O;
     mov al,[bx];{M} add al,[bx+1];{L} stosb;
     mov al,[bx]; xor ah,ah; add di,ax; lea si,[bx+2];
     xor ch,ch; mov cl,[bx+1]; rep movsb; mov ax,si; pop ds;
end;


{  XPage }

function XPage.Off:word;
begin if IsLeaf then Off:=oLeaf else Off:=oNotLeaf end;

function XPage.XI(I:word):XItemPtr;
var x:XItemPtr; o:word;
begin
  x:=XItemPtr(@A); o:=Off; while I>1 do begin x:=x^.Next(o); dec(I) end;  XI:=x;
end;

function XPage.EndOff:word;
var x:XItemPtr; xofs:word absolute x;
begin x:=XI(NItems+1); EndOff:=xofs end;

function XPage.Underflow:boolean;
begin Underflow:=EndOff-ofs(A)<(XPageSize-XPageOverHead) div 2 end;

function XPage.Overflow:boolean;
begin Overflow:=EndOff-ofs(Self)>XPageSize end;

function XPage.StrI(I:word):string;
var x:XItemPtr; xofs:word absolute x; o,j:word; s:StringPtr;
begin
  x:=XItemPtr(@A); o:=Off;
  asm les di,@result; mov s.word,di; mov s[2].word,es end;
  if I>NItems then s^[0]:=#0 else
  for j:=1 to I do xofs:=x^.UpdStr(o,s);
end;

function XPage.SumN:longint;
var i,o:word; n:longint; x:XItemPtr;
begin
  if IsLeaf then begin SumN:=NItems;exit end;  n:=0; x:=XItemPtr(@A); o:=Off;
  for i:=1 to NItems do begin inc(n,x^.GetN); x:=x^.Next(o) end;
  SumN:=n end;

procedure XPage.Insert(I:word;var SS; var XX:XItemPtr);
var S:string absolute SS;
var x,x2:XItemPtr; xofs:word absolute x; x2ofs:word absolute x2;
    m,m2,o,oE,l,l2,sz:word;  d:integer;
begin
  o:=Off; oE:=EndOff; inc(NItems); x:=XI(I);
  m:=0; if I>1 then m:=SLeadEqu(StrI(I-1),S); l:=length(S)-m; sz:=o+2+l;
  if I<NItems then begin
    x2:=x; m2:=SLeadEqu(StrI(I),S); d:=m2-x^.GetM(o);
    if d>0 then begin l2:=x^.GetL(o); inc(x2ofs,d); Move(x^,x2^,o);
      x2^.PutM(o,m2); x2^.PutL(o,l2-d); dec(sz,d) end;
    Move(x2^,ptr(seg(x2^),x2ofs+sz)^,oE-x2ofs) end;
  XX:=x; x^.PutM(o,m); x^.PutL(o,l); inc(xofs,o+2); Move(S[m+1],x^,l);
end;

procedure XPage.InsDownIndex(I:word;Page:longint;P:XPagePtr);
var s:string; x:XItemPtr;
begin
  s:=P^.StrI(P^.NItems); Insert(I,s,x); x^.PutN(P^.SumN);
  x^.DownPage:=Page;
end;

procedure XPage.Delete(I:word);
var x,x1,x2:XItemPtr;
    xofs:word absolute x; x1ofs:word absolute x1; x2ofs:word absolute x2;
    o,oE:word; d:integer;
begin
  o:=Off; oE:=EndOff; x:=XI(I);
  if I<NItems then begin x2:=x^.Next(o); d:=x2^.GetM(o)-x^.GetM(o);
    if d<=0 then Move(x2^,x^,oE-x2ofs)
    else begin Move(x2^,x^,o); x^.PutL(o,x2^.GetL(o)+d); x1:=x;
      inc(x1ofs,o+2+d); inc(x2ofs,o+2); Move(x2^,x1^,oE-x2ofs) end;
    x:=XI(NItems) end;
  FillChar(x^,oE-xofs,0); dec(NItems);
end;

procedure XPage.AddPage(P:XPagePtr);
var x,x1,xE:XItemPtr; xofs:word absolute x; o,oE,m,l:word;
begin
  GreaterPage:=P^.GreaterPage; if P^.NItems=0 then exit;
  xE:=XI(NItems+1); oE:=P^.EndOff; o:=Off; x:=XItemPtr(@P^.A);
  if NItems>0 then begin
    m:=SLeadEqu(StrI(NItems),P^.StrI(1));
    if m>0 then begin
      l:=x^.GetL(o)-m; x1:=x; inc(xofs,m); Move(x1^,x^,o);
      x^.PutM(o,m); x^.PutL(o,l) end end;
  Move(x^,xE^,oE-xofs); inc(NItems,P^.NItems);
end;

procedure XPage.SplitPage(P:XPagePtr;ThisPage:longint);
var x,x1,x2:XItemPtr;
    xofs:word absolute x; x1ofs:word absolute x1; x2ofs:word absolute x2;
    o,oA,oE,n:word; s:^string;
begin
  x:=XItemPtr(@A); x1:=x; o:=Off; oA:=xofs; oE:=EndOff; n:=0;
  while xofs-oA < oE-xofs+x^.GetM(o) do begin x:=x^.Next(o); inc(n) end;
  FillChar(P^,XPageSize,0); Move(x1^,P^.A,xofs-oA);
  s:=ptr(seg(x1^),oA+o+1); s^:=StrI(n+1); Move(x^,x1^,o); x1^.PutM(o,0);
  x1:=x1^.Next(o); x:=x^.Next(o); Move(x^,x1^,oE-xofs);
  P^.NItems:=n; dec(NItems,n); xofs:=EndOff; FillChar(x^,oE-xofs,0);
  if IsLeaf then P^.GreaterPage:=ThisPage else P^.GreaterPage:=0;
  P^.IsLeaf:=IsLeaf;
end;

{ XWFile }

procedure XWFile.Err(N:word);
begin
  if @Self=@XWork then begin SetMsgPar(FandWorkXName); RunError(n) end
  else begin CFile^.XF^.SetNotValid; CFileMsg(n,'X'); CloseGoExit end;
end;

procedure XWFile.TestErr;
begin
  if HandleError<>0 then Err(700+HandleError);
end;

function XWFile.UsedFileSize:longint;
begin
  UsedFileSize:=longint(MaxPage+1) shl XPageShft;
end;

function XWFile.NotCached:boolean;
begin NotCached:= (@Self<>@XWork) and CFile^.NotCached end;

procedure XWFile.RdPage(P:XPagePtr;N:longint);
begin
  if (N=0) or (N>MaxPage) then Err(831);
  RdWrCache(true,Handle,NotCached,N shl XPageShft,XPageSize,P^);
end;
procedure XWFile.WrPage(P:XPagePtr;N:longint);
begin
  if UpdLockCnt>0 then Err(645);
  RdWrCache(false,Handle,NotCached,N shl XPageShft,XPageSize,P^);
end;
function XWFile.NewPage(P:XPagePtr):longint;
begin
  if FreeRoot<>0 then begin
    NewPage:=FreeRoot; RdPage(P,FreeRoot); FreeRoot:=P^.GreaterPage end
  else begin
    inc(MaxPage); if MaxPage>$1fffff then Err(887); NewPage:=MaxPage end;
  FillChar(P^,XPageSize,0);
end;
procedure XWFile.ReleasePage(P:XPagePtr;N:longint);
begin
  FillChar(P^,XPageSize,0); P^.GreaterPage:=FreeRoot; FreeRoot:=N;
  WrPage(P,N);
end;

{ XFile }

procedure XFile.SetEmpty;
var k:KeyDPtr; n:longint; p:XPagePtr;
begin
  p:=GetZStore(XPageSize); WrPage(p,0);
  p^.IsLeaf:=true; FreeRoot:=0; NRecs:=0; k:=CFile^.Keys;
  while k<>nil do begin
    n:=k^.IndexRoot; MaxPage:=n; WrPage(p,n); k:=k^.Chain end;
  ReleaseStore(p); WrPrefix;
end;
procedure XFile.RdPrefix;
begin
  RdWrCache(true,Handle,NotCached,2,18,FreeRoot);
end;
procedure XFile.WrPrefix;
const Signum:word=$04FF;
begin
  RdWrCache(false,Handle,NotCached,0,2,Signum);
  NRecsAbs:=CFile^.NRecs; NrKeys:=CFile^.GetNrKeys;
  RdWrCache(false,Handle,NotCached,2,18,FreeRoot);
end;
procedure XFile.SetNotValid;
begin
  NotValid:=true; MaxPage:=0; WrPrefix; SaveCache(0);
end;


{ XKey }

function XKey.XF:XWFilePtr;
begin
  if InWork then XF:=@XWork else XF:=CFile^.XF;
end;
function XKey.NRecs:longint;
begin
  if InWork then NRecs:=NR else NRecs:=CFile^.XF^.NRecs;
end;

function XKey.Search(var XX:XString;AfterEqu:boolean;var RecNr:longint):boolean;
var page:longint; p:XPagePtr; x:XItemPtr; o:word; iItem,nItems:word;
    result:char;
label 1,2;
begin
  p:=GetStore(XPageSize);
  XPathN:=1; page:=IndexRoot; AfterEqu:=AfterEqu and Duplic;
1:XPath[XPathN].Page:=page;  XF^.RdPage(p,page);
  x:=XItemPtr(@p^.A); o:=p^.Off; nItems:=p^.NItems;
  if nItems=0 then begin RecNr:=CFile^.NRecs+1; XPath[1].I:=1; goto 2 end;
  asm
     push ds; cld; les bx,x; mov iItem,1; mov dx,1;
@@1: add bx,o; xor ax,ax; mov al,es:[bx]; cmp dx,ax; jna @@5; {first different <= prefix length?}
     mov dx,ax; lds si,XX; xor ax,ax; lodsb; sub ax,dx; add si,dx;
     mov ah,es:[bx+1]; {string length} lea di,[bx+2]; {string addr}
     xor cx,cx; mov cl,ah; cmp ah,al; jna @@2; mov cl,al;  {min length}
@@2: add dx,cx; xor ch,ch; {set zero flag}
     repe cmpsb; jb @@8; ja @@4; cmp al,ah; jb @@8; ja @@3;
     cmp AfterEqu,0; je @@7;
@@3: inc dx;
@@4: sub dx,cx;
@@5: mov ax,iItem; cmp ax,nItems; je @@6; {last item?}
     inc ax; mov iItem,ax;
     xor ax,ax; mov al,es:[bx+1]; add ax,2; add bx,ax;  {next item}
     jmp @@1;
@@6: mov al,_gt; inc iItem; jmp @@9;
@@7: mov al,_equ; jmp @@9;
@@8: mov al,_lt;
@@9: mov result,al; sub bx,o; mov x.word,bx; pop ds;
  end;
  XPath[XPathN].I:=iItem;
  if p^.IsLeaf then begin
    if iItem>nItems then RecNr:=CFile^.NRecs+1 else RecNr:=x^.GetN;
    if Result=_equ then
      if                        {$ifdef FandSQL}
       not CFile^.IsSQLFile and {$endif}
       ((RecNr=0) or (RecNr>CFile^.NRecs)) then XF^.Err(833)
      else Search:=true
    else
2:    Search:=false;
    ReleaseStore(p); exit end;
  if iItem>nItems then page:=p^.GreaterPage else page:=x^.DownPage;
  inc(XPathN); goto 1;
end;

function XKey.SearchIntvl(var XX:XString;AfterEqu:boolean;var RecNr:longint):boolean;
begin
  SearchIntvl:=Search(XX,AfterEqu,RecNr) or Intervaltest and (RecNr<=CFile^.NRecs);
end;

function XKey.PathToNr:longint;
var i,j:word; n:longint; p:XPagePtr; x:XItemPtr;
begin
  p:=GetStore(XPageSize); n:=0; for j:=1 to XPathN-1 do begin
    XF^.RdPage(p,XPath[j].Page); x:=XitemPtr(@P^.A);
    for i:=1 to XPath[j].I-1 do begin inc(n,x^.GetN); x:=x^.Next(oNotLeaf) end end;
  inc(n,XPath[XPathN].I); if (n>NRecs+1) then XF^.Err(834);
  PathToNr:=n; ReleaseStore(p);
end;

procedure XKey.NrToPath(I:longint);
var j:word; page:longint; p:XPagePtr; x:XItemPtr;
label 1;
begin
  p:=GetStore(XPageSize); page:=IndexRoot; XPathN:=0;
1:XF^.RdPage(p,page); inc(XPathN); XPath[XPathN].Page:=page;
  if p^.IsLeaf then begin if I>p^.NItems+1 then XF^.Err(837);
    XPath[XPathN].I:=I; ReleaseStore(p); exit end;
  x:=XItemPtr(@p^.A); for j:=1 to p^.NItems do begin
    if I<=x^.GetN then begin XPath[XPathN].I:=j; page:=x^.DownPage; goto 1 end;
    dec(I,x^.GetN); x:=x^.Next(oNotLeaf) end;
  XPath[XPathN].I:=p^.NItems+1; page:=p^.GreaterPage; goto 1;
end;

function XKey.PathToRecNr:longint;
var p:XPagePtr; recnr:longint;
begin
  p:=GetStore(XPageSize); with XPath[XPathN] do begin
    XF^.RdPage(p,Page); recnr:=p^.XI(I)^.GetN; PathToRecNr:=recnr;
    if (recnr=0) or (recnr>CFile^.NRecs) then XF^.Err(835) end;
  ReleaseStore(p);
end;

function XKey.RecNrToPath(var XX:XString;RecNr:longint):boolean;
function IncPath(J:word;var Pg:longint):boolean;
var p:XPagePtr;
label 1,2;
begin
  p:=GetStore(XPageSize);
  IncPath:=false; if J=0 then goto 2; with XPath[J] do begin
1:  XF^.RdPage(p,Page);
    if I>p^.NItems then
      if IncPath(J-1,Page) then begin I:=0; goto 1 end else goto 2;
    inc(I);
    if I>p^.NItems then
      if p^.GreaterPage=0 then begin
        I:=0; if IncPath(J-1,Page) then goto 1; goto 2 end
      else Pg:=p^.GreaterPage
    else Pg:=p^.XI(I)^.DownPage end;
  IncPath:=true;
2:ReleaseStore(p);
end;
var p:XPagePtr; x:XItemPtr; n:longint;
label 1,2,3;
begin
  XX.PackKF(KFlds); Search(XX,false,n); p:=GetStore(XPageSize);
  RecNrToPath:=false; with XPath[XPathN] do begin
1:  XF^.RdPage(p,Page); x:=p^.XI(I); if p^.StrI(I)<>XX.S then goto 3;
2:  if x^.GetN=RecNr then begin RecNrToPath:=true; goto 3 end;
    inc(I); if I>P^.NItems then begin
      if IncPath(XPathN-1,Page) then begin I:=1; goto 1 end end
    else begin
      x:=x^.Next(oLeaf); if x^.GetL(oLeaf)<>0 then goto 3; goto 2 end end;
3:ReleaseStore(p);
end;

function XKey.NrToRecNr(I:longint):longint;
begin
  NrToPath(I); NrToRecNr:=PathToRecNr;
end;

function XKey.NrToStr(I:longint):string;
var p:XPagePtr;
begin
  p:=GetStore(XPageSize); NrToPath(I); with XPath[XPathN] do begin
    XF^.RdPage(p,Page); NrToStr:=p^.StrI(I) end;
  ReleaseStore(p);
end;

function XKey.RecNrToNr(RecNr:longint):longint;
var x:XString;
begin
  if RecNrToPath(x,RecNr) then RecNrToNr:=PathToNr else RecNrToNr:=0;
end;

function XKey.FindNr(var X:XString;var IndexNr:longint):boolean;
var n:longint;
begin FindNr:=Search(X,false,n);IndexNr:=PathToNr end;

procedure XKey.InsertOnPath(var XX:XString;RecNr:longint);
procedure InsertItem(var XX:XString;P,UpP:XPagePtr;Page:longint;I:word;
  var X:XItemPtr; var UpPage:longint);
begin
  P^.Insert(I,XX.S,X); UpPage:=0;
  if P^.Overflow then begin
    UpPage:=XF^.NewPage(UpP); P^.SplitPage(UpP,Page);
    if I<=UpP^.NItems then X:=UpP^.XI(I) else X:=P^.XI(I-UpP^.NItems);
    XX.S:=UpP^.StrI(UpP^.NItems) end;
end;
procedure ChainPrevLeaf(P:XPagePtr;N:longint);
var page:longint; i,j:word;
label 1;
begin
  for j:=XPathN-1 downto 1 do if XPath[j].I>1 then begin
    xf^.RdPage(P,XPath[j].Page); i:=XPath[j].I-1;
1:  page:=P^.XI(i)^.DownPage; xf^.RdPage(P,page);
    if P^.IsLeaf then begin P^.GreaterPage:=N; xf^.WrPage(P,page); exit end;
    i:=P^.NItems; goto 1 end;
end;
var i,j:word; page,page1,uppage,downpage:longint; x:XItemPtr; n,upsum:longint;
    p,p1,upp:XPagePtr;
begin
  p:=GetStore(2*XPageSize); p1:=GetStore(2*XPageSize); upp:=GetStore(2*XPageSize);
  for j:=XPathN downto 1 do begin
    page:=XPath[J].Page; xf^.RdPage(p,page); i:=XPath[j].I;
    if p^.IsLeaf then begin
      InsertItem(XX,p,upp,page,i,x,uppage); x^.PutN(RecNr) end
    else begin
      if i<=p^.NItems then begin
        x:=p^.XI(i); n:=x^.GetN+1; if uppage<>0 then dec(n,upsum);
        x^.PutN(n) end;
      if uppage<>0 then begin
        downpage:=uppage; InsertItem(XX,p,upp,page,i,x,uppage);
        x^.DownPage:=downpage; x^.PutN(upsum) end end;
    xf^.WrPage(p,page); if uppage<>0 then begin
      xf^.WrPage(upp,uppage); upsum:=upp^.SumN;
      if upp^.IsLeaf then ChainPrevLeaf(p1,uppage) end;
  end;
  if uppage<>0 then begin
    page1:=xf^.NewPage(p1); p1^.GreaterPage:=page1;
    p1^.InsDownIndex(1,uppage,upp); xf^.WrPage(p,page1); xf^.WrPage(p1,page);
    if upp^.IsLeaf then begin
      upp^.GreaterPage:=page1; xf^.WrPage(upp,uppage) end end;
  ReleaseStore(p);
end;

function XKey.Insert(RecNr:longint;Try:boolean):boolean;
var N,XNr:longint; x:XString;
begin
  x.PackKF(KFlds);
  if Search(x,true,N) then
    if Try then begin Insert:=false; exit end
    else begin XFNotValid; CFileError(822) end;
   InsertOnPath(x,RecNr); Insert:=true;
end;

procedure XKey.DeleteOnPath;
procedure BalancePages(P1,P2:XPagePtr; var Released:boolean);
var n:longint; sz:word;
begin
  n:=P1^.GreaterPage; P1^.AddPage(P2); sz:=P1^.EndOff-ofs(P1^);
  if sz<=XPageSize then Released:=true else begin
    Released:=false; Move(P1^,P2^,sz); P2^.SplitPage(P1,n) end;
end;
procedure XIDown(P,P1:XPagePtr;I:word; var Page1:longint);
begin
  if I>P^.NItems then Page1:=P^.GreaterPage else Page1:=P^.XI(I)^.DownPage;
  XF^.RdPage(P1,Page1);
end;
var i,j,i1,i2:word; page,page1,page2,uppage:longint;  pp:pointer;
    p,p1,p2,px:XPagePtr; upp:XPagePtr absolute p2;
    x:XItemPtr; released:boolean;  n:longint;
begin
  MarkStore(pp);
  p:=GetStore(2*XPageSize); p1:=GetStore(2*XPageSize); p2:=GetStore(2*XPageSize);
  for j:=XPathN downto 1 do begin
    page:=XPath[j].Page; xf^.RdPage(p,page); i:=XPath[J].I;
    if p^.IsLeaf then p^.Delete(i)
    else if upp^.Underflow then begin
      xf^.WrPage(upp,uppage);
      i1:=i-1; i2:=i; if i1=0 then begin i1:=1; i2:=2 end;
      XIDown(p,p1,i1,page1); XIDown(p,p2,i2,page2);
      BalancePages(p1,p2,released);
      xf^.WrPage(p1,page1); p^.Delete(i1);
      if released then begin
        xf^.ReleasePage(p2,page2);
        if i1>p^.NItems then p^.GreaterPage:=page1
        else begin p^.InsDownIndex(i1,page1,p1); p^.Delete(i2) end end
      else begin
        xf^.WrPage(p2,page2); p^.InsDownIndex(i1,page1,p1);
        if i2<=p^.NItems then begin
          p^.Delete(i2); p^.InsDownIndex(i2,page2,p2) end end end
    else begin
      if upp^.Overflow then begin
        page1:=xf^.NewPage(p1); upp^.SplitPage(p1,uppage);
        xf^.WrPage(p1,page1); p^.InsDownIndex(i,page1,p1); inc(i) end;
      xf^.WrPage(upp,uppage);
      if i<=p^.NItems then begin
        p^.Delete(i); p^.InsDownIndex(i,uppage,upp) end end;
    uppage:=page; px:=upp; upp:=p; p:=px end;
  if upp^.Overflow then begin
    page1:=xf^.NewPage(p1); upp^.SplitPage(p1,uppage); page:=xf^.NewPage(p);
    p^.GreaterPage:=page; p^.InsDownIndex(1,page1,p1);
    xf^.WrPage(p1,page1); xf^.WrPage(p,uppage); xf^.WrPage(upp,page) end
  else begin
    page1:=upp^.GreaterPage; if (upp^.NItems=0) and (page1>0) then begin
      xf^.RdPage(p1,page1); Move(p1^,upp^,XPageSize);
      xf^.ReleasePage(p1,page1) end;
    xf^.WrPage(upp,uppage) end;
  ReleaseStore(pp);
end;
function XKey.Delete(RecNr:longint):boolean;
var xx:XString; b:boolean;
begin
  b:=RecNrToPath(xx,RecNr); if b then DeleteOnPath; Delete:=b;
end;


{ XWKey }

procedure XWKey.Open(KF:KeyFldDPtr;Dupl,Intvl:boolean);
var p:XPagePtr;
begin
  KFlds:=KF; Duplic:=Dupl; InWork:=true; Intervaltest:=Intvl; NR:=0;
  p:=GetStore(sizeof(p^)); IndexRoot:=XF^.NewPage(p);
  p^.IsLeaf:=true; XF^.WrPage(p,IndexRoot); ReleaseStore(p); IndexLen:=0;
  while KF<>nil do begin inc(IndexLen,KF^.FldD^.NBytes); KF:=KF^.Chain end;
end;

procedure XWKey.ReleaseTree(Page:longint;IsClose:boolean);
var p:XPagePtr; i,n:word;
begin
  if (Page=0) or (Page>XF^.MaxPage) then exit; p:=GetStore(XPageSize);
  XF^.RdPage(p,Page); if not p^.IsLeaf then begin
   n:=p^.NItems; for i:=1 to n do begin
     ReleaseTree(p^.XI(i)^.DownPage,IsClose); XF^.RdPage(p,Page) end;
   if p^.GreaterPage<>0 then ReleaseTree(p^.GreaterPage,IsClose) end;
  if (Page<>IndexRoot) or IsClose then
    XF^.ReleasePage(p,Page)
  else begin
    fillchar(p^,XPageSize,0); p^.IsLeaf:=true; XF^.WrPage(p,Page) end;
  ReleaseStore(p);
end;
procedure XWKey.Release;
begin
  ReleaseTree(IndexRoot,false); NR:=0;
end;
procedure XWKey.Close;
begin
  ReleaseTree(IndexRoot,true); IndexRoot:=0;
end;

procedure XWKey.OneRecIdx(KF:KeyFldDPtr;N:longint);
begin
  Open(KF,true,false); Insert(N,true); inc(NR);
end;

procedure XWKey.InsertAtNr(I,RecNr:longint);
var x:XString;
begin
  x.PackKF(KFlds); inc(NR); NrToPath(I); InsertOnPath(x,RecNr);
end;

function XWKey.InsertGetNr(RecNr:longint):longint;
var x:XString; n:longint;
begin
  inc(NR); x.PackKF(KFlds); Search(x,true,n); InsertGetNr:=PathToNr;
  InsertOnPath(x,RecNr);
end;

procedure XWKey.DeleteAtNr(I:longint);
begin
  NrToPath(I); DeleteOnPath; dec(NR);
end;

procedure XWKey.AddToRecNr(RecNr:longint;Dif:integer);
var p:XPagePtr; x:XItemPtr; pg:longint; j,n:integer; nn:longint;
begin
  if NRecs=0 then exit; NrToPath(1);
  p:=GetStore(sizeof(p^)); with XPath[XPathN] do begin pg:=Page; j:=I end;
  repeat XF^.RdPage(p,pg); n:=p^.NItems-j+1; x:=p^.XI(j);
    while n>0 do begin
      nn:=x^.GetN; if nn>=RecNr then x^.PutN(nn+Dif);
      x:=x^.Next(oLeaf); dec(n) end;
    XF^.WrPage(p,pg); pg:=p^.GreaterPage; j:=1;
  until pg=0;
  ReleaseStore(p);
end;


{ CFile[^.XF] }

procedure TestXFExist;
var xf:XFilePtr;
begin
  xf:=CFile^.XF; if (xf<>nil) and xf^.NotValid then begin
    if xf^.NoCreate then CFileError(819); CreateIndexFile end;
end;

function XNRecs(K:KeyDPtr):longint;
  begin if (CFile^.Typ='X') and (K<>nil) then
           begin TestXFexist;XNRecs:=CFile^.XF^.NRecs end
        else XNRecs:=CFile^.NRecs end;

procedure RecallRec(RecNr:longint);
  var K:KeyDPtr;
  begin TestXFExist; inc(CFile^.XF^.NRecs); K:=CFile^.Keys;
        while K<>nil do begin K^.Insert(RecNr,false); K:=K^.Chain end;
        ClearDeletedFlag; WriteRec(RecNr) end;

procedure TryInsertAllIndexes(RecNr:longint);
var K,K1:KeyDPtr; p:pointer;
label 1;
begin
  TestXFExist;
  MarkStore(p); K:=CFile^.Keys; while K<>nil do begin
    if not K^.Insert(RecNr,true) then goto 1; K:=K^.Chain end;
  inc(CFile^.XF^.NRecs); exit;
1:ReleaseStore(p); K1:=CFile^.Keys; while (K1<>nil) and (K1<>K) do begin
    K1^.Delete(RecNr);K1:=K1^.Chain end;
  SetDeletedFlag; WriteRec(RecNr);
  with CFile^.XF^ do if FirstDupl then begin
    SetMsgPar(CFile^.Name); WrLLF10Msg(828); FirstDupl:=false end;
end;

procedure DeleteAllIndexes(RecNr:longint);
  var K:KeyDPtr;
  begin K:=CFile^.Keys; while K<>nil do begin
        K^.Delete(RecNr); K:=K^.Chain end end;
procedure DeleteXRec(RecNr:longint;DelT:boolean);
begin
  TestXFExist;
  DeleteAllIndexes(RecNr); if DelT then DelAllDifTFlds(CRecPtr,nil);
  SetDeletedFlag; WriteRec(RecNr); dec(CFile^.XF^.NRecs);
end;
procedure OverwrXRec(RecNr:longint;P2,P:pointer);
var x,x2:XString; K:KeyDPtr;
begin
  CRecPtr:=P2; if DeletedFlag then begin CRecPtr:=P; RecallRec(RecNr); exit end;
  TestXFExist; K:=CFile^.Keys; while K<>nil do begin
    CRecPtr:=P; x.PackKF(K^.KFlds); CRecPtr:=P2; x2.PackKF(K^.KFlds);
    if x.S<>x2.S then begin
      K^.Delete(RecNr); CRecPtr:=P; K^.Insert(RecNr,false) end;
    K:=K^.Chain end;
  CRecPtr:=P; WriteRec(RecNr);
end;


{ XScan }

constructor XScan.Init(aFD:FileDPtr;aKey:KeyDPtr;aKIRoot:KeyInDPtr;aWithT:boolean);
begin
  TObject.Init;
  FD:=aFD; Key:=aKey; KIRoot:=aKIRoot; withT:=aWithT; {$ifdef FandSQL}
  if aFD^.IsSQLFile then begin
    if (aKey<>nil) and aKey^.InWork then begin P:=GetStore(XPageSize); Kind:=3 end
    else Kind:=4 end else {$endif}
  begin if (aKey<>nil) then begin
    P:=GetStore(XPageSize); Kind:=1; if aKIRoot<>nil then Kind:=2 end end;
end;

procedure AddFFs(K:KeyDPtr;var s:string255);
var i,l:word;
begin
  l:=minw(K^.IndexLen+1,255);
  for i:=length(s)+1 to l do s[i]:=#$ff; s[0]:=char(l);
end;

procedure CompKIFrml(K:KeyDPtr;KI:KeyInDPtr;AddFF:boolean);
var x:XString; b:boolean; i:integer;
begin while KI<>nil do begin
  b:=x.PackFrml(KI^.FL1,K^.KFlds); KI^.X1^:=x.S;
  if KI^.FL2<>nil then x.PackFrml(KI^.FL2,K^.KFlds);
  if AddFF then AddFFs(K,x.S);
  KI^.X2^:=x.S; KI:=KI^.Chain end;
end;

procedure XScan.Reset(aBool:FrmlPtr;SQLFilter:boolean);
var k:KeyInDPtr; n:longint; xx:XString; b:boolean;
begin
  CFile:=FD; Bool:=aBool;
  if SQLFilter then if CFile^.IsSQLFile then hasSQLFilter:=true else Bool:=nil;
  case Kind of
   0:NRecs:=CFile^.NRecs;
   1,3:begin if not Key^.InWork then TestXFExist; NRecs:=Key^.NRecs end;
   2:begin if not Key^.InWork then TestXFExist;
       CompKIFrml(Key,KIRoot,true); NRecs:=0; k:=KIRoot; while k<>nil do begin
         Key^.FindNr(XStringPtr(k^.X1)^,k^.XNrBeg);
         b:=Key^.FindNr(XStringPtr(k^.X2)^,n);
         k^.N:=0; if n>=k^.XNrBeg then k^.N:=n-k^.XNrBeg+ord(b);
         inc(NRecs,k^.N); k:=k^.Chain end end;          {$ifdef FandSQL}
   4:begin CompKIFrml(Key,KIRoot,false); New(SQLStreamPtr(Strm),Init); IRec:=1 end;   {$endif}
  end;
  SeekRec(0);
end;

procedure XScan.ResetSort(aSK:KeyFldDPtr;var BoolZ:FrmlPtr;OldMd:LockMode;SQLFilter:boolean);
var m:LockMode;
begin
  if Kind=4 then begin SK:=aSK;
    if SQLFilter then begin Reset(BoolZ,true); BoolZ:=nil end
    else Reset(nil,false); exit end;
  if aSK<>nil then begin
    Reset(BoolZ,false); ScanSubstWIndex(@Self,aSK,'S'); BoolZ:=nil end
  else Reset(nil,false);
  with CFile^ do if NotCached then begin
    case Kind of
     0: begin m:=NoCrMode; if XF<>nil then m:=NoExclMode end;
     1: begin m:=OldMd; if Key^.InWork then m:=NoExclMode end;
     else exit end;
    m:=LockMode(maxw(ord(m),ord(OldMd)));
    if m<>OldMd then ChangeLMode(m,0,true) end;
end;

procedure XScan.SubstWIndex(WK:WKeyDPtr);
begin
  Key:=WK; if Kind<>3 then Kind:=1; if P=nil then P:=GetStore(XPageSize);
  NRecs:=Key^.NRecs; Bool:=nil; SeekRec(0); TempWX:=true;
end;

procedure XScan.ResetOwner(XX:XStringPtr;aBool:FrmlPtr);
var n:longint; b:boolean;
begin
  CFile:=FD; Bool:=aBool;
  {$ifdef FandSQL}
  if Kind=4 then begin           { not on .SQL with Workindex }
    KIRoot:=GetZStore(sizeof(KIRoot^));
    KIRoot^.X1:=StoreStr(XX^.S); KIRoot^.X2:=StoreStr(XX^.S);
    New(SQLStreamPtr(Strm),Init); IRec:=1 end
  else                           {$endif}
  begin TestXFExist; KIRoot:=GetZStore(sizeof(KIRoot^));
      Key^.FindNr(XX^,KIRoot^.XNrBeg); AddFFs(Key,XX^.S);
      b:=Key^.FindNr(XX^,n); NRecs:=n-KIRoot^.XNrBeg+ord(b);
      KIRoot^.N:=NRecs; Kind:=2 end;
  SeekRec(0);
end;
procedure XScan.ResetOwnerIndex(LD:LinkDPtr;LV:LocVarPtr;aBool:FrmlPtr);
var k:WKeyDPtr;
begin
  CFile:=FD; TestXFExist; Bool:=aBool; OwnerLV:=LV; Kind:=2;
  if not EquKFlds(WKeyDPtr(LV^.RecPtr)^.KFlds,LD^.ToKey^.KFlds) then RunError(1181);
  SeekRec(0);
end;
                                           {$ifdef FandSQL}
procedure XScan.ResetSQLTxt(Z:FrmlPtr);
var s:LongStrPtr;
begin
  New(SQLStreamPtr(Strm),Init); s:=RunLongStr(Z);
  SQLStreamPtr(Strm)^.InpResetTxt(s); ReleaseStore(s); EOF:=false;
end;                                        {$endif}

procedure XScan.ResetLV(aRP:pointer);
begin
  Strm:=aRP; Kind:=5; NRecs:=1;
end;

procedure XScan.Close;
begin
  CFile:=FD; {$ifdef FandSQL}
  if Kind=4 then with SQLStreamPtr(Strm)^ do begin InpClose; Done end;{$endif}
  if TempWX then WKeyDPtr(Key)^.Close;
end;

procedure XScan.SeekOnPage(Page:longint;I:word);
begin
  Key^.XF^.RdPage(P,Page); NOnPg:=P^.NItems-I+1;
  if Kind=2 then begin if NOnPg>NOfKI then NOnPg:=NOfKI; dec(NOfKI,NOnPg) end;
  X:=P^.XI(I);
end;

procedure XScan.NextIntvl;
var xx:XString; b:boolean; n,nBeg:longint; k:WKeyDPtr;
begin
  if OwnerLV<>nil then begin
    k:=WKeyDPtr(OwnerLV^.RecPtr);
    while iOKey<k^.NRecs do begin
      inc(iOKey); CFile:=OwnerLV^.FD; xx.S:=k^.NrToStr(iOKey); CFile:=FD;
      Key^.FindNr(xx,nBeg); AddFFs(Key,xx.S); b:=Key^.FindNr(xx,n);
      n:=n-nBeg+ord(b); if n>0 then begin NOfKI:=n;
        Key^.NrToPath(nBeg); with XPath[XPathN] do SeekOnPage(Page,I); exit end end;
    NRecs:=IRec{EOF} end
  else begin
     repeat KI:=KI^.Chain until (KI=nil) or (KI^.N>0);
     if KI<>nil then SeekOnKI(0) end;
end;

procedure XScan.SeekOnKI(I:longint);
begin
  NOfKI:=KI^.N-I; Key^.NrToPath(KI^.XNrBeg+I);
  with XPath[XPathN] do SeekOnPage(Page,I);
end;

procedure XScan.SeekRec(I:longint);
var k:KeyInDPtr; z:FrmlPtr;
begin
  CFile:=FD;                                         {$ifdef FandSQL}
  if Kind=4 then begin
   if I<>IRec then with SQLStreamPtr(Strm)^ do begin
     if NotFrst then InpClose; NotFrst:=true;
     if hasSQLFilter then z:=Bool else z:=nil;
     InpReset(Key,SK,KIRoot,z,withT);
     EOF:=AtEnd; IRec:=0; NRecs:=$20000000 end;
   exit end;                                         {$endif}
  if (Kind=2) and (OwnerLV<>nil) then begin
     IRec:=0; NRecs:=$20000000; iOKey:=0; NextIntvl; EOF:=I>=NRecs; exit end;
  IRec:=I; EOF:=I>=NRecs; if not EOF then
  case Kind of
    1,3:begin Key^.NrToPath(I+1); with XPath[XPathN] do SeekOnPage(Page,I) end;
    2:begin k:=KIRoot; while I>=k^.N do begin dec(I,k^.N); k:=k^.Chain end;
      KI:=k; SeekOnKI(I) end;
  end
end;

procedure XScan.GetRec;
var xx:XString;
label 1,2,3;
begin
  CFile:=FD; {$ifdef FandSQL}
  if Kind=4 then begin
    repeat EOF:=not SQLStreamPtr(Strm)^.GetRec
    until EOF or hasSQLFilter or RunBool(Bool);
    inc(IRec); exit end; {$endif}
1:EOF:=IRec>=NRecs; if not EOF then begin
    inc(IRec); case Kind of
     0:begin RecNr:=IRec; goto 2 end;
     1,2:begin RecNr:=X^.GetN; dec(NOnPg);
         if NOnPg>0 then X:=X^.Next(oLeaf)
         else if (Kind=2) and (NOfKI=0) then NextIntvl
         else if P^.GreaterPage>0 then SeekOnPage(P^.GreaterPage,1);
2:       ReadRec(RecNr); if DeletedFlag then goto 1;
3:       if not RunBool(Bool) then goto 1 end;               {$ifdef FandSQL}
     3:begin dec(NOnPg); xx.S:=P^.StrI(P^.NItems-NOnPg);
       if (NOnPg=0) and (P^.GreaterPage>0) then SeekOnPage(P^.GreaterPage,1);
       if not Strm1^.SelectXRec(Key,@xx,_equ,withT) then goto 1; goto 3 end; {$endif}
     5:move(Strm^,CRecPtr^,CFile^.RecLen+1);
   end end;
end;