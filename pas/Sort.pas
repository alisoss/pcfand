{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit sort;

interface

uses drivers,base,access,rdrun;

procedure CreateIndexFile;
procedure CreateWIndex(Scan:XScanPtr;K:WKeyDPtr;Typ:char);
procedure ScanSubstWIndex(Scan:XScanPtr;SK:KeyFldDPtr;Typ:char);
procedure SortAndSubst(SK:KeyFldDPtr);
procedure GetIndex(PD:InstrPtr);
procedure CopyIndex(K:WKeyDPtr;FromK:KeyDPtr);

implementation

uses obaseww,oaccess,runfrml;

procedure Ovr; far;
assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push cs; push ax;
end;

{ the index is sorted by key value and input order(IR) !! }

type
  WRecPtr=^WRec;
  WRec=object            { record on WPage }
    N:array[1..3]of byte; IR:array[1..3]of byte; X:XString;
    function GetN:longint;
    procedure PutN(NN:longint);
    procedure PutIR(II:longint);
    function Comp(R:WRecPtr):word;
  end;
  WPagePtr=^WPage;
  WPage=object        { ca. 64k pages in work file }
    NxtChain,Chain:longint;
    NRecs:word;
    A:byte;
    procedure Sort(N,RecLen:word);
  end;
  WorkFilePtr = ^WorkFile;
  WorkFile = object(TObject)
    Handle,RecLen,
    MaxOnWPage,WPageSize:word;
    MaxWPage,WRoot,NChains,PgWritten:longint;
    WBaseSize:longint;
    PW,PW1,PW2:WPagePtr;
    FreeNr:array[1..5]of longint;
    NFreeNr:word;
    IRec,RecNr:longint;
    KFRoot:KeyFldDPtr;
    constructor Init;
    destructor Done; virtual;
    procedure Reset(KF:KeyFldDPtr; RestBytes:longint; Typ:char; NRecs:longint);
    procedure SortMerge;
    function GetCRec:boolean; virtual;
    procedure Output(R:WRecPtr); virtual;
  private
    procedure TestErr;
    function GetFreeNr:longint;
    procedure Merge;
    procedure Merge2Chains(Pg1,Pg2,Pg,Nxt:longint);
    procedure PutFreeNr(N:longint);
    procedure ReadWPage(W:WPagePtr;Pg:longint);
    procedure WriteWPage(N:word;Pg,Nxt,Chn:longint);
  end;

  XWorkFilePtr = ^XWorkFile;
  XXPagePtr=^XXPage;
  XXPage=object          { for building XPage }
    Chain:XXPagePtr;
    XW:XWorkFilePtr;
    Off,MaxOff:word;
    LastIndex:string;
    LastRecNr,Sum:longint;
    IsLeaf:boolean;
    GreaterPage:longint;
    NItems:word;
    A:array[1..XPageSize-XPageOverHead] of byte;
    procedure Reset(OwnerXW:XWorkFilePtr);
    procedure PutN(var N);
    procedure PutDownPage(DownPage:longint);
    procedure PutMLX(M,L:byte);
    procedure ClearRest;
    procedure PageFull;
    procedure AddToLeaf(R:WRecPtr;KD:KeyDPtr);
    procedure AddToUpper(P:XXPagePtr;DownPage:longint);
  end;

  XWorkFile = object(WorkFile)
    PX:XXPagePtr;
    KD:KeyDPtr;
    Scan:XScanPtr;
    MsgWritten:boolean;
    NxtXPage:longint;
    XF:XWFilePtr;
    XPP:XPagePtr;
    constructor Init(AScan:XScanPtr;AK:KeyDPtr);
    procedure Main(Typ:char);
    procedure CopyIndex(K:KeyDPtr;KF:KeyFldDPtr;Typ:char);
    function GetCRec:boolean; virtual;
    procedure Output(R:WRecPtr); virtual;
   private
    procedure FinishIndex;
  end;


{ WRec }

function WRec.GetN:longint; assembler;
asm les di,Self; mov ax,es:[di]; mov dl,es:[di+2]; xor dh,dh end;

procedure WRec.PutN(NN:longint); assembler;
asm les di,Self; mov ax,NN.word; cld; stosw; mov al,NN[2].byte;
    stosb end;

procedure WRec.PutIR(II:longint); assembler;
asm les di,Self; add di,3; mov ax,II.word; cld; stosw; mov al,II[2].byte;
    stosb end;

function WRec.Comp(R:WRecPtr):word; assembler;
asm push ds; cld; lds si,Self; mov al,[si+6]; add si,7;
    les di,R; mov ah,es:[di+6]; add di,7;
    xor ch,ch; mov cl,al; cmp ah,al; ja @1; mov cl,ah;
@1: jcxz @11; repe cmpsb; jb @2; ja @3;
@11:cmp al,ah; jb @2; ja @3;   {compare X}
    mov si,Self.word; mov di,R.word; mov al,ds:[si+5]; cmp al,es:[di+5]; {compare IR}
    jb @2; ja @3; mov ax,ds:[si+3]; cmp ax,es:[di+3]; jb @2; ja @3;
    mov ax,1; jmp @4;
@2: mov ax,2; jmp @4;
@3: mov ax,4;
@4: pop ds;
end;

{ WPage }

procedure WPage.Sort(N,RecLen:word);
procedure PushWord(W:word);inline($90);
function PopWord:word;inline($58);
var X,Y,Z,V:WRecPtr;
  oX:word absolute X; oY:word absolute Y; oZ:word absolute Z;
  oA,cx,cy,OldSP,CurSP:word; iX,iY,R,L:integer;
label 1,2;
begin
  if N<=1 then exit;  V:=GetStore(sizeof(WRec));
  X:=WRecPtr(@A); dec(PtrRec(X).Seg,$10); inc(oX,$100); {prevent negative ofs}
  Y:=X; Z:=X; oA:=oX;
  asm mov OldSP,sp end;
  PushWord(0); PushWord(N-1);
  repeat
    R:=PopWord; L:=PopWord;
    repeat oZ:=oA+((L+R)shr 1)*RecLen; MyMove(Z^,V^,RecLen);
      oX:=oA+L*RecLen; oY:=oA+R*RecLen;
      repeat
      1:cx:=X^.Comp(V);
        if cx=ord(_lt) then begin inc(oX,RecLen); goto 1 end;
      2:cy:=V^.Comp(Y);
        if cy=ord(_lt) then begin dec(oY,RecLen);goto 2 end;
        if oX<=oY then begin
          if (cx or cy)<>ord(_equ) then ExChange(X,Y,RecLen);
          inc(oX,RecLen); dec(oY,RecLen) end;
      until oX>oY;
      iX:=(oX-oA) div RecLen;
      if oX-RecLen>oY then iY:=iX-2 else iY:=iX-1;
      if iY=L then L:=iX
      else if iX=R then R:=iY
      else if iY-L<R-iX then begin  {push longest interval on stack}
        if iX<R then begin PushWord(iX); PushWord(R) end; R:=iY end
      else begin
        if L<iY then begin PushWord(L); PushWord(iY) end; L:=iX end;
    until L>=R;
    asm mov CurSP,sp end;
  until OldSP=CurSP;
  ReleaseStore(V);
end;


{ WorkFile }


constructor WorkFile.Init;
begin
  TObject.Init;
  WBaseSize:=MaxWSize; Handle:=WorkHandle;
end;

destructor WorkFile.Done;
begin
  MaxWSize:=WBaseSize; TruncH(Handle,MaxWSize); FlushH(Handle);
end;

procedure WorkFile.TestErr;
begin
  if HandleError<>0 then begin SetMsgPar(FandWorkName); RunError(700+HandleError) end;
end;

procedure WorkFile.Reset(KF:KeyFldDPtr; RestBytes:longint; Typ:char; NRecs:longint);
var bytes:longint; pages:longint;
const kB60=$0F000;
begin
  KFRoot:=KF; RecLen:=7; while KF<>nil do begin
    with KF^.FldD^ do if Typ='D' then inc(RecLen,6) else inc(RecLen,NBytes);
    KF:=KF^.Chain end;
  bytes:=(StoreAvail-RestBytes-sizeof(WRec)) div 3;
  if bytes<4096 then RunError(624);
  if bytes<kb60 then WPageSize:=word(bytes) and $F000 else WPageSize:=kb60;
  MaxOnWPage:=(WPageSize-sizeof(WPage)+1) div RecLen;
  if integer(MaxOnWPage)<4 then RunError(624);
  MaxWPage:=0; NFreeNr:=0;
  PW:=GetStore(WPageSize);
  WRoot:=GetFreeNr;
  pages:=(NRecs+MaxOnWPage-1) div MaxOnWPage;
  asm mov dx,pages.word; mov bx,dx; mov cx,dx; mov ax,1;
   @1:add cx,dx; test bx,1; jz @2; sub cx,ax; shr bx,1; add bx,1; jmp @3;
   @2:shr bx,1;            { how many pages must be written ? }
   @3:shl ax,1; cmp bx,1; ja @1; cmp cx,0; jne @4; mov cx,1;
   @4:mov pages.word,cx end;
  RunMsgOn(Typ,pages);
end;

procedure WorkFile.SortMerge;
var r:WRecPtr; rofs:word absolute r; n:word; pg,nxt:longint;
begin
  PgWritten:=0;
  n:=0; r:=WRecPtr(@PW^.A); nxt:=WRoot; NChains:=1;
  while GetCRec do begin
    if n=MaxOnWPage then begin
      PW^.Sort(n,RecLen); pg:=nxt; nxt:=GetFreeNr; inc(NChains);
      WriteWPage(n,pg,nxt,0);
      n:=0; r:=WRecPtr(@PW^.A) end;
    r^.PutN(RecNr); r^.PutIR(IRec); r^.X.PackKF(KFRoot);
    inc(n); inc(rofs,RecLen) end;
  PW^.Sort(n,RecLen);
  WriteWPage(n,nxt,0,0);
  if NChains>1 then
    Merge;
  RunMsgOff;
end;

function WorkFile.GetCRec:boolean;
begin
end;

procedure WorkFile.Output(R:WRecPtr);
begin
end;

function WorkFile.GetFreeNr:longint;
begin
  if NFreeNr>0 then begin GetFreeNr:=FreeNr[NFreeNr]; dec(NFreeNr) end
  else begin inc(MaxWPage); inc(MaxWSize,WPageSize); GetFreeNr:=MaxWPage end;
end;

procedure WorkFile.Merge;
var npairs,new,nxtnew,pg1,pg2,nxt:longint;
label 1,2;
begin
  nxt:=0; PW1:=GetStore(WPageSize); PW2:=GetStore(WPageSize);
1:if NChains=1 then exit;
  npairs:=NChains div 2; pg1:=WRoot;
  if NChains=2 then new:=0 else begin WRoot:=GetFreeNr; new:=WRoot end;
  if nxt>0 then begin pg2:=pg1; pg1:=nxt; ReadWPage(PW1,pg1); goto 2 end;
  while npairs>0 do begin
    ReadWPage(PW1,pg1); pg2:=PW1^.NxtChain;
2:  ReadWPage(PW2,pg2); nxt:=PW2^.NxtChain;
    if npairs=1 then nxtnew:=0 else nxtnew:=GetFreeNr;
    dec(NChains);
    Merge2Chains(pg1,pg2,new,nxtnew);
    dec(npairs); pg1:=nxt; new:=nxtnew end;
  goto 1;
end;

procedure WorkFile.Merge2Chains(Pg1,Pg2,Pg,Nxt:longint);
var r1,r2,r:WRecPtr;
  r1ofs:word absolute r1; r2ofs:word absolute r2; rofs:word absolute r;
  max1ofs,max2ofs,maxofs:word;  eof1,eof2:boolean;
  chn:longint; l:word; w1,w2,w:WPagePtr;
label 1,2,3,4;
begin
  w1:=PW1; w2:=PW2; w:=PW; l:=RecLen; eof1:=false; eof2:=false;
  r1:=WRecPtr(@w1^.A); r2:=WRecPtr(@w2^.A); r:=WRecPtr(@w^.A);
  max1ofs:=r1ofs+w1^.NRecs*l; max2ofs:=r2ofs+w2^.NRecs*l;
  maxofs:=rofs+MaxOnWPage*l;
1:if rofs=maxofs then begin
    chn:=GetFreeNr; WriteWPage(MaxOnWPage,Pg,Nxt,chn); Pg:=chn; Nxt:=0;
    r:=WRecPtr(@w^.A) end;
  if eof1 then goto 3;
  if eof2 then goto 2;
  if r1^.Comp(r2)=ord(_gt) then goto 3;
2:MyMove(r1^,r^,l); inc(rofs,l); inc(r1ofs,l);
  if r1ofs=max1ofs then begin
    PutFreeNr(Pg1); Pg1:=w1^.Chain;
    if Pg1<>0 then begin
      ReadWPage(w1,Pg1); r1:=WRecPtr(@w1^.A); max1ofs:=r1ofs+w1^.NRecs*l end
    else if eof2 then goto 4 else eof1:=true end;
  goto 1;
3:MyMove(r2^,r^,l); inc(rofs,l); inc(r2ofs,l);
  if r2ofs=max2ofs then begin
    PutFreeNr(Pg2); Pg2:=w2^.Chain;
    if Pg2<>0 then begin
      ReadWPage(w2,Pg2); r2:=WRecPtr(@w2^.A); max2ofs:=r2ofs+w2^.Nrecs*l end
    else if eof1 then goto 4 else eof2:=true end;
  goto 1;
4:WriteWPage((rofs-ofs(w^.A))div l,Pg,Nxt,0);
end;

procedure WorkFile.PutFreeNr(N:longint);
begin
  inc(NFreeNr); FreeNr[NFreeNr]:=N;
end;

procedure WorkFile.ReadWPage(W:WPagePtr;Pg:longint);
begin
  SeekH(Handle,WBaseSize+(Pg-1)*WPageSize);
  ReadH(Handle,WPageSize,W^); TestErr;
end;
procedure WorkFile.WriteWPage(N:word;Pg,Nxt,Chn:longint);
var r:WRecPtr; rofs:word absolute r;
begin
  inc(PgWritten); RunMsgN(PgWritten);
  if NChains=1 then begin
    r:=WRecPtr(@PW^.A); while N>0 do begin
     Output(r); dec(N); inc(rofs,Reclen) end end
  else begin
    with PW^ do begin NRecs:=N; NxtChain:=Nxt; Chain:=Chn end;
    SeekH(Handle,WBaseSize+(Pg-1)*WPageSize);
    WriteH(Handle,WPageSize,PW^); TestErr end;
end;

{ XXPage }

procedure XXPage.Reset(OwnerXW:XWorkFilePtr);
begin
  XW:=OwnerXW; Sum:=0; NItems:=0;
  MaxOff:=ofs(Chain)+sizeof(XXPage); Off:=ofs(A) end;

procedure XXPage.PutN(var N); assembler;
asm push ds; cld; les bx,Self; mov di,es:[bx].XXPage.Off;
    lds si,N; lodsw; stosw; lodsb; stosb;
    mov es:[bx].XXPage.Off,di; pop ds end;

procedure XXPage.PutDownPage(DownPage:longint); assembler;
asm cld; les bx,Self; mov di,es:[bx].XXPage.Off;
    mov ax,DownPage.word; stosw; mov ax,DownPage[2].word; stosw;
    mov es:[bx].XXPage.Off,di end;

procedure XXPage.PutMLX(M,L:byte); assembler;
asm push ds; cld; les bx,Self; mov di,es:[bx].XXPage.Off;
    mov al,M; stosb; mov al,L; stosb;
    mov ax,es; mov ds,ax; lea si,es:[bx].XXPage.LastIndex;
    inc si; xor ch,ch; mov cl,M; add si,cx; mov cl,L; rep movsb;
    mov es:[bx].XXPage.Off,di; pop ds end;

procedure XXPage.ClearRest; assembler;
asm les bx,Self; mov di,es:[bx].XXPage.Off; mov cx,es:[bx].XXPage.MaxOff;
    sub cx,di; jcxz @1; cld; mov al,0; rep stosb; @1: end;

procedure XXPage.PageFull;
var n:longint;
begin with XW^ do begin
  ClearRest;
  if Chain=nil then begin
    Chain:=GetZStore(sizeof(XXPage)); Chain^.Reset(XW) end;
  if IsLeaf then n:=NxtXPage else n:=XF^.NewPage(XPP);
  Chain^.AddToUpper(@Self,n);
  if IsLeaf then begin NxtXPage:=XF^.NewPage(XPP); GreaterPage:=NxtXPage end;
  XF^.WrPage(XPagePtr(@IsLeaf),n);
end end;

procedure XXPage.AddToUpper(P:XXPagePtr;DownPage:longint);
var l,m:word;
label 1;
begin
1:m:=0; l:=length(P^.LastIndex);
  if (l>0) and (NItems>0) then begin
    m:=SLeadEqu(P^.LastIndex,LastIndex); l:=l-m end;
  if Off+9+l>MaxOff then begin
    PageFull; Reset(XW); goto 1 end;
  LastIndex:=P^.LastIndex; inc(Sum,P^.Sum); inc(NItems); PutN(P^.Sum);
  PutDownPage(DownPage); PutMLX(m,l);
end;


procedure XXPage.AddToLeaf(R:WRecPtr;KD:KeyDPtr);
var m,l:byte; k:KeyDPtr; n:longint;
label 1;
begin
1:m:=0; l:=length(R^.X.S); n:=R^.GetN;
  if (l>0) and (NItems>0) then begin
    m:=SLeadEqu(R^.X.S,LastIndex);
    if (m=l) and (m=length(LastIndex)) then begin
      if n=LastRecNr then exit; { overlapping intervals from  key in .. }
      if not KD^.InWork and not KD^.Duplic then begin
        if not XW^.MsgWritten then begin
          SetMsgPar(CFile^.Name);
          if IsTestRun then begin if not PromptYN(832) then GoExit end
          else WrLLF10Msg(828);
          XW^.MsgWritten:=true end;
        ReadRec(n);
        k:=CFile^.Keys; while (k<>KD) do begin
          k^.Delete(n); k:=k^.Chain end;
        SetDeletedFlag; WriteRec(n); exit end end;
    l:=l-m end;
  if Off+5+l>MaxOff then begin
    PageFull; Reset(XW); goto 1 end;
  LastIndex:=R^.X.S; LastRecNr:=n; inc(Sum);
  inc(NItems); PutN(n); PutMLX(m,l);
end;

{ XWorkFile }
                 
constructor XWorkFile.Init(AScan:XScanPtr;AK:KeyDPtr);
begin
  WorkFile.Init;
  Scan:=AScan; CFile:=Scan^.FD; KD:=AK; XF:=AK^.XF;
end;
procedure XWorkFile.Main(Typ:char);
var R:WRecPtr; k:KeyDPtr; kf:KeyFldDPtr; p:XPagePtr; frst:boolean;
begin
  XPP:=GetStore(XPageSize); NxtXPage:=XF^.NewPage(XPP);
  MsgWritten:=false; frst:=true;
  while KD<>nil do begin
    PX:=GetZStore(sizeof(XXPage)); PX^.Reset(@Self); PX^.IsLeaf:=true;
    k:=Scan^.Key; kf:=KD^.KFlds;
    if (Scan^.Kind=1) and {$ifdef FandSQL} not Scan^.FD^.IsSQLFile and {$endif}
       (Scan^.Bool=nil) and (EquKFlds(k^.KFlds,kf) or (kf=nil))
      then CopyIndex(k,kf,Typ)
    else begin
      if frst then frst:=false else Scan^.SeekRec(0);
      Reset(KD^.KFlds,sizeof(XXPage)*9,Typ,Scan^.NRecs); SortMerge end;
    FinishIndex;
    ReleaseStore(PX); KD:=KD^.Chain end;
  XF^.ReleasePage(XPP,NxtXPage); ReleaseStore(XPP);
end;

procedure XWorkFile.CopyIndex(K:KeyDPtr;KF:KeyFldDPtr;Typ:char);
var r:WRecPtr; p:XPagePtr; page:longint; n:word; i:longint;
    x:XItemPtr; xofs:word absolute x;
begin
  r:=GetStore(sizeof(WRec)); r^.X.S:=''; p:=GetStore(XPageSize);
  K^.NrToPath(1); page:=XPath[XPathN].Page; RunMsgOn(Typ,K^.NRecs); i:=0;
  while page<>0 do begin
    K^.XF^.RdPage(p,page); x:=XItemPtr(@p^.A); n:=p^.NItems; while n>0 do begin
      r^.PutN(x^.GetN);
      if KF=nil then x:=x^.Next(oLeaf)
      else xofs:=x^.UpdStr(oLeaf,StringPtr(@r^.X.S));
      OutPut(r); dec(n) end;
    inc(i,p^.NItems); RunMsgN(i); page:=p^.GreaterPage end;
  RunMsgOff;
end;

function XWorkFile.GetCRec:boolean;
begin
  Scan^.GetRec; GetCRec:= not Scan^.EOF; RecNr:=Scan^.RecNr; IRec:=Scan^.IRec;
end;

procedure XWorkFile.Output(R:WRecPtr);
begin
  PX^.AddToLeaf(R,KD);
end;

procedure XWorkFile.FinishIndex;
var sum,n:longint; p,p1:XXPagePtr;
label 1;
begin
  n:=0; sum:=0; p:=PX;
1:sum:=sum+p^.Sum;
  p^.ClearRest; p^.GreaterPage:=n; p1:=p^.Chain;
  if p1=nil then n:=KD^.IndexRoot else n:=NxtXPage;
  XF^.WrPage(XPagePtr(@p^.IsLeaf),n); p:=p1;
  if p<>nil then begin NxtXPage:=XF^.NewPage(XPP); goto 1 end;
  if KD^.InWork then WKeyDPtr(KD)^.NR:=sum else XFilePtr(XF)^.NRecs:=sum;
end;




procedure CreateIndexFile;
var er:ExitRecord; cr,p:pointer; md:LockMode; fail:boolean;
    XW:XWorkFilePtr; Scan:XScanPtr;   XF:XFilePtr;
label 1;
begin
  NewExit(Ovr,er); goto 1; MarkStore(p); fail:=true;
  XF:=CFile^.XF; cr:=CRecPtr; CRecPtr:=GetRecSpace;
  md:=NewLMode(RdMode); TryLockN(0,0); {ClearCacheCFile;}
  if XF^.Handle=$FF then RunError(903);
  XF^.RdPrefix;
  if XF^.NotValid then begin
    XF^.SetEmpty;
    New(Scan,Init(CFile,nil,nil,false)); Scan^.Reset(nil,false);
    New(XW,Init(Scan,CFile^.Keys));
    XW^.Main('X');
    XW^.Done;
    XF^.NotValid:=false; XF^.WrPrefix;
    if not SaveCache(0) then GoExit; {FlushHandles;} end;
  fail:=false;
1:RestoreExit(er); ReleaseStore(p); CRecPtr:=cr;
  if fail then begin XF^.SetNotValid; XF^.NoCreate:=true end;
  UnLockN(0); OldLMode(md);
  if fail then GoExit;
end;


procedure CreateWIndex(Scan:XScanPtr;K:WKeyDPtr;Typ:char);
var XW:XWorkFilePtr; cr,p:pointer;
begin
  MarkStore(p); cr:=CRecPtr; CRecPtr:=GetRecSpace;
  New(XW,Init(Scan,K));
  XW^.Main(Typ);
  XW^.Done;
  CRecPtr:=cr; ReleaseStore(p);
end;

procedure ScanSubstWIndex(Scan:XScanPtr;SK:KeyFldDPtr;Typ:char);
var k2:WKeyDPtr; k:KeyDPtr; kf,kf2,kfroot:KeyFldDPtr; n:word;
begin
  k2:=GetZStore(sizeof(k2^));
  if Scan^.FD^.IsSQLFile and (Scan^.Kind=3){F6-autoreport & sort}then begin
    k:=Scan^.Key; n:=k^.IndexLen; kf:=SK; while kf<>nil do begin
      inc(n,kf^.FldD^.NBytes); kf:=kf^.Chain end;
    if n>255 then begin WrLLF10Msg(155); ReleaseStore(k2); exit end;
    kf:=k^.KFlds; kfroot:=nil; while kf<>nil do begin
      kf2:=GetStore(sizeof(KeyFldD)); kf2^:=kf^; ChainLast(kfroot,kf2);
      kf:=kf^.Chain end;
    kf2^.Chain:=SK; SK:=kfroot end;
  k2^.Open(SK,true,false);
  CreateWIndex(Scan,k2,Typ); Scan^.SubstWIndex(k2);
end;

procedure SortAndSubst(SK:KeyFldDPtr);
var Scan:XScanPtr; p:pointer; cf,FD2:FileDPtr;
begin
  MarkStore(p); cf:=CFile; CRecPtr:=GetRecSpace;
  New(Scan,Init(CFile,nil,nil,false)); Scan^.Reset(nil,false);
  ScanSubstWIndex(Scan,SK,'S');
  FD2:=OpenDuplF(false);  RunMsgOn('S',Scan^.NRecs);
  Scan^.GetRec;
  while not Scan^.EOF do begin
    RunMsgN(Scan^.IRec); CFile:=FD2; PutRec; Scan^.GetRec end;
  if not SaveCache(0) then GoExit;
  CFile:=cf; SubstDuplF(FD2,false); Scan^.Close; RunMsgOff; ReleaseStore(p);
end;

procedure GetIndex(PD:InstrPtr);
var Scan:XScanPtr; p:pointer; lv,lv2:LocVarPtr; kNew,k:WKeyDPtr; nr:longint;
    md,md1:LockMode; cond:FrmlPtr; ld:LinkDPtr; kf:KeyFldDPtr; x:XString;
label 1;
begin
  MarkStore(p); lv:=PD^.giLV; CFile:=lv^.FD; k:=WKeyDPtr(lv^.RecPtr);
  md:=NewLMode(RdMode);
  if PD^.giMode=' ' then begin
    ld:=PD^.giLD; kf:=ld^.ToKey^.KFlds; lv2:=PD^.giLV2;
    New(Scan,Init(CFile,PD^.giKD,PD^.giKIRoot,false));
    cond:=RunEvalFrml(PD^.giCond);
    case PD^.giOwnerTyp of
     'i':Scan^.ResetOwnerIndex(ld,lv2,cond);
     'r':begin CFile:=ld^.ToFD; CRecPtr:=lv2^.RecPtr; x.PackKF(kf); goto 1 end;
     'F':begin CFile:=ld^.ToFD; md:=NewLMode(RdMode); CRecPtr:=GetRecSpace;
             ReadRec(RunInt(FrmlPtr(PD^.giLV2))); x.PackKF(kf);
             ReleaseStore(CRecPtr); OldLMode(md);
1:           CFile:=lv^.FD; Scan^.ResetOwner(@x,cond) end;
     else Scan^.Reset(cond,PD^.giSQLFilter) end;
    kf:=PD^.giKFlds; if kf=nil then kf:=k^.KFlds;
    kNew:=GetZStore(sizeof(kNew^)); kNew^.Open(kf,true,false);
    CreateWIndex(Scan,kNew,'X'); k^.Close; k^:=kNew^ end
  else begin
    CRecPtr:=GetRecSpace; nr:=RunInt(PD^.giCond);
    if (nr>0) and (nr<=CFile^.NRecs) then begin ReadRec(nr);
      if PD^.giMode='+' then begin
        if not DeletedFlag then begin
          x.PackKF(k^.KFlds); if not k^.RecNrToPath(x,nr) then begin
            k^.InsertOnPath(x,nr); inc(k^.NR) end end end
    else if k^.Delete(nr) then dec(k^.NR) end end;
  OldLMode(md); ReleaseStore(p);
end;

procedure CopyIndex(K:WKeyDPtr;FromK:KeyDPtr);
var Scan:XScanPtr; p:pointer; md:LockMode;
begin
  K^.Release; MarkStore(p); md:=NewLMode(RdMode);
  New(Scan,Init(CFile,FromK,nil,false)); Scan^.Reset(nil,false);
  CreateWIndex(Scan,K,'W');
  OldLMode(md); ReleaseStore(p);
end;


end.