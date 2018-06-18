{$S-}
procedure ResetCFileUpdH;
begin with CFile^ do begin
  ResetUpdHandle(Handle); if Typ='X' then ResetUpdHandle(XF^.Handle);
  if TF<>nil then ResetUpdHandle(TF^.Handle);
end end;
procedure ClearCacheCFile;
begin with CFile^ do begin
  ClearCacheH(Handle); if Typ='X' then ClearCacheH(XF^.Handle);
  if TF<>nil then ClearCacheH(TF^.Handle);
end end;
procedure CloseClearHCFile;
begin with CFile^ do begin
  CloseClearH(Handle); if Typ='X' then CloseClearH(XF^.Handle);
  if TF<>nil then CloseClearH(TF^.Handle);
end end;

{$ifdef FandNetV}
const TransLock=$0A000501;  { locked while state transition }
      ModeLock =$0A000000;  { base for mode locking }
      RecLock  =$0B000000;  { base for record locking }

procedure ModeLockBnds(Mode:LockMode;var Pos:longint;var Len:word);
var n:longint;
begin
  case Mode of       { hi=how much bytes, low= first byte }
    NoExclMode : n:=$00010000+LANNode;
    NoDelMode  : n:=$00010100+LANNode;
    NoCrMode   : n:=$00010200+LANNode;
    RdMode     : n:=$00010300+LANNode;
    WrMode     : n:=$00FF0300;
    CrMode     : n:=$01FF0200;
    DelMode    : n:=$02FF0100;
    ExclMode   : n:=$03FF0000;
  end;
  Pos:=ModeLock+LongRec(n).Lo; Len:=LongRec(n).Hi;
end;
{$S+}

function ChangeLMode(Mode:LockMode;Kind:word;RdPref:boolean):boolean;
var
  pos,oldpos:longint; h,len,oldlen,count,d:word; w,w1:longint; oldmode:LockMode;
label 1,2;
begin
  if not CFile^.IsShared then begin         {neu!!}
    ChangeLMode:=true; CFile^.LMode:=Mode; exit end;
  ChangeLMode:=false; oldmode:=CFile^.LMode; h:=CFile^.Handle;
  if oldmode>=WrMode then begin
    if Mode<WrMode then WrPrefixes;
    if oldmode=ExclMode then begin SaveCache(0); ClearCacheCFile end;
    if Mode<WrMode then ResetCFileUpdH end;
  w:=0; count:=0;
1:if Mode<>NullMode then
    if not TryLockH(h,TransLock,1) then begin
2:    if Kind=2 then exit; {Kind: 0-wait, 1-wait until ESC, 2-no wait}
      inc(count);
      if count<=spec.LockRetries then d:=spec.LockDelay
      else begin d:=spec.NetDelay; SetCPathVol;
        Set2MsgPar(CPath,LockModeTxt[Mode]); w1:=PushWrLLMsg(825,Kind=1);
        if w=0 then w:=w1 else TWork.Delete(w1); LockBeep end;
      if KbdTimer(spec.NetDelay,Kind) then goto 1;
      if w<>0 then PopW(w); exit end;
  if oldmode<>NullMode then begin
    ModeLockBnds(oldmode,oldpos,oldlen); UnLockH(h,oldpos,oldlen) end;
  if Mode<>NullMode then begin
    ModeLockBnds(Mode,pos,len); if not TryLockH(h,pos,len) then begin
      if oldmode<>NullMode then TryLockH(h,oldpos,oldlen);
      UnLockH(h,TransLock,1); goto 2 end;
    UnLockH(h,TransLock,1) end;
  if w<>0 then PopW(w);
  CFile^.LMode:=Mode;
  if (oldmode<RdMode) and (Mode>=RdMode) and RdPref then RdPrefixes;
  ChangeLMode:=true;
end;
{$else}
function ChangeLMode(Mode:LockMode;Kind:word;RdPref:boolean):boolean;
  begin ChangeLMode:=true; CFile^.LMode:=Mode end;
{$endif}

function TryLMode(Mode:LockMode;var OldMode:LockMode;Kind:word):boolean;
begin with CFile^ do begin TryLMode:=true;
{$ifdef FandSQL} if IsSQLFile then begin
    OldMode:=LMode; if Mode>LMode then LMode:=Mode end else  {$endif}
  begin if Handle=$FF then OpenCreateF(Shared); OldMode:=LMode;
    if Mode>LMode then TryLMode:=ChangeLMode(Mode,Kind,true) end end;
end;

function NewLMode(Mode:LockMode):LockMode;
var md:LockMode;
begin TryLMode(Mode,md,0); NewLMode:=md end;

procedure OldLMode(Mode:LockMode);
begin with CFile^ do begin
{$ifdef FandSQL} if IsSQLFile then begin LMode:=Mode; exit end; {$endif}
  if Handle=$FF then exit;
  if Mode<>LMode then ChangeLMode(Mode,0,true);
end end;


function TryLockN(N:longint;Kind:word):boolean;
var w,w1:longint; m:word;
const XTxt:string[3]='CrX';
label 1;
begin
  TryLockN:=true;  {$ifdef FandSQL} if CFile^.IsSQLFile then exit; {$endif}
{$ifdef FandNetV}
  if not CFile^.IsShared then exit; w:=0;
1:if not TryLockH(CFile^.Handle,RecLock+N,1) then begin
    if Kind<>2 then begin   {Kind: 0-wait, 1-wait until ESC, 2-no wait}
      m:=826; if N=0 then begin SetCPathVol; Set2MsgPar(CPath,XTxt); m:=825 end;
      w1:=PushWrLLMsg(m,Kind=1); if w=0 then w:=w1 else TWork.Delete(w1);
      {beep; don't disturb}
      if KbdTimer(spec.NetDelay,Kind) then goto 1 end;
    TryLockN:=false end;
  if w<>0 then PopW(w);
{$endif}
end;

procedure UnLockN(N:longint);
begin with CFile^ do begin
{$ifdef FandSQL} if IsSQLFile then exit; {$endif}
{$ifdef FandNetV}
  if (Handle=$FF) or not IsShared then exit;
  UnLockH(Handle,RecLock+N,1);
{$endif}
end end;



procedure CExtToT;
begin
  if SEquUpcase(CExt,'.RDB') then
     CExt:='.TTT' else
  if SEquUpcase(CExt,'.DBF') then
    if CFile^.TF^.Format=FptFormat then CExt:='.FPT' else CExt:='.DBT'
  else CExt[2]:='T';
  CPath:=CDir+CName+CExt;
end;
procedure CExtToX;
  begin CExt[2]:='X';CPath:=CDir+CName+CExt end;
procedure CloseGoExit;
  begin CloseClearHCFile; GoExit end;
procedure TestCFileError;
  begin if HandleError<>0 then CFileError(700+HandleError) end;
procedure TestCPathError;
var n:word;
begin if HandleError<>0 then begin
  n:=700+HandleError; if (n=705) and (CPath[length(CPath)]='\') then n:=840;
  SetMsgPar(CPath); RunError(n);
end end;


{ TFile }


type
  TT1Page=record
    Signum, OldMaxPage:word;
    FreePart:longint;
    Rsrvd1, CompileProc, CompileAll:boolean;
    IRec:word;
    FreeRoot, MaxPage:longint;   {eldest version=>Pw:array[1..40] of char;}
    TimeStmp:real; HasCoproc:boolean; Rsrvd2:array[1..25]of char;
    Version:array[1..4]of char;
    LicText:array[1..105] of byte; Sum:byte;
    X1:array[1..295] of char;
    LicNr:word;
    X2:array[1..11] of char;
    PwNew:array[1..40] of char;
    Time:byte end;

procedure TFile.Err(n:word;ex:boolean);
begin
  if IsWork then begin                                   
    SetMsgPar(FandWorkTName); WrLLF10Msg(n); if ex then GoExit end
  else begin CFileMsg(n,'T'); if ex then CloseGoExit end;
end;
procedure TFile.TestErr;
begin
  if HandleError<>0 then Err(700+HandleError,true);
end;

function TFile.NotCached:boolean;
begin NotCached:= not IsWork and CFile^.NotCached end;

function TFile.UsedFileSize:longint;
begin
  if Format=FptFormat then UsedFileSize:=Freepart*BlockSize
  else UsedFileSize:=longint(MaxPage+1) shl MPageShft;
end;


procedure TFile.RdPrefix(Chk:boolean);
procedure GetMLen;
  begin MLen:=(MaxPage+1)shl MPageShft end;
var
  T:TT1Page;
  TX:array[1..512] of byte absolute T;
  TNxtAvailPage:longint absolute T;                               { .DBT }
  FptHd:record FreePart:longint; X,BlockSize:word end absolute T; { .FPT }
  sum:byte; FS,ML,RS:longint; i,n:word;
label 1;
begin
  if Chk then begin FS:=FileSizeH(Handle); if FS<=512 then begin
    FillChar(PwCode,40,'@'); Code(PwCode,40); SetEmpty; exit end end;
  RdWrCache(true,Handle,NotCached,0,512,T); RS:=RandSeed; LicenseNr:=0;
  if Format=DbtFormat then begin
    MaxPage:=TNxtAvailPage-1; GetMLen; exit end;
  if Format=FptFormat then begin
    FreePart:=SwapLong(FptHd.FreePart); BlockSize:=Swap(FptHd.BlockSize); exit end;
  Move(T.FreePart,FreePart,23);
  if not IsWork and (CFile=Chpt) and ((T.HasCoproc<>HasCoproc) or
    (CompArea(@version[1],@T.Version,4)<>ord(_equ))) then CompileAll:=true;
  if T.OldMaxPage=$ffff then goto 1
  else begin
    FreeRoot:=0;
    if FreePart>0 then begin
      if not Chk then FS:=FileSizeH(Handle); ML:=FS;
      MaxPage:=(FS-1)shr MPageShft; GetMLen end
    else begin
      FreePart:=-FreePart; MaxPage:=T.OldMaxPage;
1:    GetMLen; ML:=MLen; if not Chk then FS:=ML end end;
  if IRec>=$6000 then begin IRec:= IRec-$2000;
    if not IsWork and (CFile^.Typ='0') then LicenseNr:=T.LicNr end;
  if IRec>=$4000 then begin
    IRec:=IRec-$4000;
    RandSeed:=ML+T.Time;
    for i:=14 to 511 do TX[i]:=TX[i] xor Random(255);
    Move(T.PwNew,PwCode,40) end
  else begin RandSeed:=ML; for i:=14 to 53 do TX[i]:=TX[i] xor Random(255);
    Move(T.FreeRoot{Pw},PwCode,40) end;
  Code(PwCode,40);
  if (FreePart<MPageSize) or (FreePart>ML) or (FS<ML) or
     (FreeRoot>MaxPage) or (MaxPage=0) then begin
    Err(893,false); MaxPage:=(FS-1)shr MPageShft; FreeRoot:=0; GetMLen;
    FreePart:=NewPage(true); SetUpdHandle(Handle) end;
  FillChar(T,512,0); RandSeed:=RS;
end;


procedure TFile.WrPrefix;
var
  T:TT1Page;
  TX:array[1..512] of byte absolute T;
  TNxtAvailPage:longint absolute T;                               { .DBT }
  FptHd:record FreePart:longint; X,BlockSize:word end absolute T; { .FPT }
  Pw:array[1..40]of char;  Time:byte absolute 0:$46C;
  i,n:word; sum:byte; RS:longint;
const EmptyPw:PwCodeArr='@@@@@@@@@@@@@@@@@@@@';
label 1;
begin
  if Format=DbtFormat then begin
    FillChar(T,512,' '); TNxtAvailPage:=MaxPage+1; goto 1 end;
  if Format=FptFormat then begin
    FillChar(T,512,0); FptHd.FreePart:=SwapLong(FreePart);
    FptHd.BlockSize:=Swap(BlockSize); goto 1 end;
  FillChar(T,512,'@');
  Move(PwCode,Pw,40); Code(Pw,40); RS:=RandSeed;
  if LicenseNr<>0 then for i:=1 to 20 do Pw[i]:=char(Random(255));
  n:=$4000; T.Time:=Time; Move(Pw,T.PwNew,40); RandSeed:=MLen+T.Time;
  for i:=14 to 511 do TX[i]:=TX[i] xor Random(255); T.LicNr:=LicenseNr;
  if LicenseNr<>0 then begin
    n:=$6000; sum:=T.LicNr;
    for i:=1 to 105 do sum:=sum+T.LicText[i]; T.Sum:=sum end;
  Move(FreePart,T.FreePart,23);
  T.OldMaxPage:=$ffff; T.Signum:=1; inc(T.IRec,n);
  Move(version[1],T.Version,4); T.HasCoproc:=HasCoproc;
  RandSeed:=RS;
1:RdWrCache(false,Handle,NotCached,0,512,T);
end;

procedure TFile.SetEmpty;
var
  X:array[0..MPageSize-1]of byte; XL:integer absolute X;
begin
  if Format=DbtFormat then begin MaxPage:=0; WrPrefix; exit end;
  if Format=FptFormat then begin Freepart:=8; BlockSize:=64; WrPrefix; exit end;
  FreeRoot:=0; MaxPage:=1; FreePart:=MPageSize; MLen:=2*MPageSize;
  WrPrefix;
  FillChar(X,MPageSize,0); XL:=-510;
  RdWrCache(false,Handle,NotCached,MPageSize,MPageSize,X);
end;

procedure TFile.Create;
begin
  Handle:=OpenH(_isoverwritefile,Exclusive); TestErr;
  IRec:=1; LicenseNr:=0; FillChar(PwCode,40,'@'); Code(PwCode,40);
  SetEmpty;
end;

function TFile.NewPage(NegL:boolean):longint;
var PosPg:longint; X:array[0..MPageSize-1]of byte; L:longint absolute X;
label 1;
begin
  if FreeRoot<>0 then begin
    PosPg:=FreeRoot shl MPageShft;
    RdWrCache(true,Handle,NotCached,PosPg,4,FreeRoot);
    if FreeRoot>MaxPage then begin
       Err(888,false);
       FreeRoot:=0; goto 1 end end
  else begin
1:  inc(MaxPage); inc(MLen,MPageSize); PosPg:=MaxPage shl MPageShft end;
  FillChar(X,MPageSize,0); if NegL then L:=-510;
  RdWrCache(false,Handle,NotCached,PosPg,MPageSize,X);
  NewPage:=PosPg;
end;

procedure TFile.ReleasePage(PosPg:longint);
var X:array[0..MPageSize-1]of byte; Next:longint absolute X;
begin
  FillChar(X,MPageSize,0); Next:=FreeRoot;
  RdWrCache(false,Handle,NotCached,PosPg,MPageSize,X);
  FreeRoot:=PosPg shr MPageShft;
end;


procedure TFile.Delete(Pos:longint);
var
  PosPg,NxtPg:longint; PosI:word; N:integer; l:word;
  X:array[0..MPageSize-1]of byte; XL:integer absolute X;
  wp:WordPtr; wpofs:word absolute wp; IsLongTxt:boolean;
label 1,2,3,4;
begin
  if Pos<=0 then exit;
  if (Format<>T00Format) or NotCached then exit;
  if (Pos<MPageSize) or (Pos>=MLen) then begin Err(889,false); exit end;
  PosPg:=Pos and ($FFFFFFFF shl MPageShft); PosI:=Pos and (MPageSize-1);
  RdWrCache(true,Handle,NotCached,PosPg,MPageSize,X);
  wp:=WordPtr(@X[PosI]); l:=wp^;
  if l<=MPageSize-2 then begin       { small text on 1 page}
    wp^:=-integer(l); N:=0; wp:=WordPtr(@X);
    while N<MPageSize-2 do begin
      if integer(wp^)>0 then begin FillChar(X[PosI+2],l,0); goto 1 end;
      inc(N,-integer(wp^)+2); inc(wpofs,-integer(wp^)+2) end;
    if (FreePart>=PosPg) and (FreePart<PosPg+MPageSize) then begin
      FillChar(X,MPageSize,0); XL:=-510; FreePart:=PosPg;
1:    RdWrCache(false,Handle,NotCached,PosPg,MPageSize,X) end
    else ReleasePage(PosPg)  end
  else begin                        { long text on more than 1 page }
    if PosI<>0 then goto 3;
2:  l:=word(XL); if l>MaxLStrLen+1 then begin
3:    Err(889,false); exit end;
    IsLongTxt:=(l=MaxLStrLen+1); inc(l,2);
4:  ReleasePage(PosPg);
    if (l>MPageSize) or IsLongTxt then begin
      PosPg:=LongintPtr(@X[MPageSize-4])^;
      if (PosPg<MPageSize) or (PosPg+MPageSize>MLen) then begin
        Err(888,false); exit end;
      RdWrCache(true,Handle,NotCached,PosPg,MPageSize,X);
      if (l<=MPageSize) then goto 2; dec(l,MPageSize-4); goto 4 end;
    end;
end;

procedure TFile.RdWr(ReadOp:boolean;Pos:longint;N:word;var X);
var
  Rest,L:word; NxtPg:longint; P:pointer; POfs:word absolute P;
begin
  Rest:=MPageSize-(word(Pos) and (MPageSize-1)); P:=@X;
  while N>Rest do begin
    L:=Rest-4;
    RdWrCache(ReadOp,Handle,NotCached,Pos,L,P^);
    inc(POfs,L); dec(N,L);
    if not ReadOp then NxtPg:=NewPage(false);
    RdWrCache(ReadOp,Handle,NotCached,Pos+L,4,NxtPg);
    Pos:=NxtPg;
    if ReadOp and ((Pos<MPageSize) or (Pos+MPageSize>MLen)) then begin
      Err(890,false); FillChar(P^,N,' '); exit end;
    Rest:=MPageSize end;
  RdWrCache(ReadOp,Handle,NotCached,Pos,N,P^);
end;

function TFile.Read(StackNr:word;Pos:longint):LongStrPtr;
var s:LongStrPtr; i,l:word; p:CharArrPtr; pofs:word absolute p;
    FptD:record Typ,Len:longint end;
label 0,1,11,2;
begin
  dec(Pos,LicenseNr);
  if Pos<=0 {OldTxt=-1 in RDB!} then goto 11
  else case Format of
    DbtFormat: begin
      s:=GetStore(32770); Pos:=Pos shl MPageShft; p:=@s^.A; l:=0;
      while l<=32768-MPageSize do begin
        RdWrCache(true,Handle,NotCached,Pos,MPageSize,p^);
        for i:=1 to MPageSize do begin if p^[i]=^z then goto 0; inc(l) end;
        inc(pofs,MPageSize); inc(Pos,MPageSize) end;
      dec(l);
0:    s^.LL:=l; ReleaseStore(@s^.A[l+1]) end;
    FptFormat: begin
      Pos:=Pos*BlockSize; RdWrCache(true,Handle,NotCached,Pos,sizeof(FptD),FptD);
      if SwapLong(FptD.Typ)<>1{text} then goto 11
      else begin
        l:=SwapLong(FptD.Len) and $7FFF; s:=GetStore(l+2); s^.LL:=l;
        RdWrCache(true,Handle,NotCached,Pos+sizeof(FptD),l,s^.A) end end;
    else
      if (Pos<MPageSize) or (Pos>=MLen) then goto 1;
      RdWrCache(true,Handle,NotCached,Pos,2,l);
      if l>MaxLStrLen+1 then begin
1:      Err(891,false);
11:     if StackNr=1 then s:=GetStore(2) else s:=GetStore2(2); s^.LL:=0;
        goto 2 end;
      if l=MaxLStrLen+1 then dec(l);
      if StackNr=1 then s:=GetStore(l+2) else s:=GetStore2(l+2); s^.LL:=l;
      RdWr(true,Pos+2,l,s^.A) end;
2:Read:=s;
end;


function TFile.Store(S:LongStrPtr):longint;
var Rest:integer; l,M:word; N:longint; p:pointer; pos:longint;
    X:array[0..MPageSize]of char;
    FptD:record Typ,Len:longint end;
label 1;
begin
  l:=S^.LL; if l=0 then begin Store:=0; exit end;
  if Format=DbtFormat then begin
    Pos:=MaxPage+1; N:=Pos shl MPageShft; if l>$7fff then l:=$7fff;
    RdWrCache(false,Handle,NotCached,N,l,S^.A);
    FillChar(X,MPageSize,' '); X[0]:=^z; X[1]:=^z;
    rest:=MPageSize-(l+2) mod MPageSize;
    RdWrCache(false,Handle,NotCached,N+l,rest+2,X);
    inc(MaxPage,(l+2+rest) div MPageSize); goto 1 end;
  if Format=FptFormat then begin
    Pos:=FreePart; N:=FreePart*BlockSize; if l>$7fff then l:=$7fff;
    FreePart:=FreePart + (sizeof(FptD)+l-1) div BlockSize +1;
    FptD.Typ:=SwapLong(1); FptD.Len:=SwapLong(l);
    RdWrCache(false,Handle,NotCached,N,sizeof(FptD),FptD); inc(N,sizeof(FptD));
    RdWrCache(false,Handle,NotCached,N,l,s^.A); inc(N,l);
    L:=FreePart*BlockSize-N; if L>0 then begin
      p:=GetStore(L); FillChar(p^,L,' ');
      RdWrCache(false,Handle,NotCached,N,L,p^); ReleaseStore(p) end;
    goto 1 end;
  if l>MaxLStrLen then l:=MaxLStrLen;
  if l>MPageSize-2 then Pos:=NewPage(false)   { long text }
  else begin                                  { short text }
    Rest:=MPageSize-FreePart mod MPageSize;
    if l+2<=Rest then Pos:=FreePart
    else begin Pos:=NewPage(false); FreePart:=Pos; Rest:=MPageSize end;
    if l+4>=Rest then FreePart:=NewPage(false)
    else begin inc(FreePart,l+2); Rest:=l+4-Rest;
      RdWrCache(false,Handle,NotCached,FreePart,2,Rest) end;
    end;
  RdWrCache(false,Handle,NotCached,Pos,2,l);
  RdWr(false,Pos+2,l,S^.A);
1:Store:=pos;
end;


                   { FILE MANAGEMENT }


function FileD.UsedFileSize:longint;
var n:longint;
begin
  n:=longint(NRecs)*RecLen+FrstDispl; if Typ='D' then inc(n);
  UsedFileSize:=n;
end;

function FileD.IsShared:boolean;
begin IsShared:=(UMode=Shared) or (UMode=RdShared) end;

function FileD.NotCached:boolean; assembler;
asm  les di,Self; xor ax,ax; mov dl,es:[di].FileD.UMode;
     cmp dl,Shared; je @1; cmp dl,RdShared; jne @2;
@1:  cmp es:[di].FileD.LMode,ExclMode; je @2;
     mov ax,1;
@2:  end;
function FileD.GetNrKeys:word;
var k:KeyDPtr; n:word;
begin
  n:=0; k:=Keys; while k<>nil do begin inc(n); k:=k^.Chain end; GetNrKeys:=n;
end;


function RdPrefix: word;
var
  X6:record NRs:longint;RLen:word end; X8:record NRs,RLen:word end;
  XD:record Ver:byte; Date:array[1..3]of byte; NRecs:longint;
            HdLen:word; RecLen:word end;
begin
  RdPrefix:=$ffff; with CFile^ do case Typ of
   '8':begin RdWrCache(true,Handle,NotCached,0,4,X8); NRecs:=X8.NRs;
       if RecLen<>X8.RLen then begin RdPrefix:=X8.RLen; exit end end;
   'D':begin RdWrCache(true,Handle,NotCached,0,12,XD);
       NRecs:=XD.NRecs;
       if (RecLen<>XD.RecLen) then begin RdPrefix:=XD.RecLen; exit end;
       FrstDispl:=XD.HdLen end;
   else
       RdWrCache(true,Handle,NotCached,0,6,X6); NRecs:=abs(X6.NRs);
       if (X6.NRs<0) and (Typ<>'X') or (X6.NRs>0) and (Typ='X')
          or (RecLen<>X6.RLen) then begin RdPrefix:=X6.RLen; exit end end;
end;

procedure RdPrefixes;
begin
  if RdPrefix<>$ffff then CFileError(883);
  with CFile^ do begin
    if (XF<>nil) and (XF^.Handle<>$FF) then XF^.RdPrefix;
    if (TF<>nil) then TF^.RdPrefix(false) end;
end;

procedure WrPrefix;
procedure WrDBaseHd;
var P:^DBaseHd; PA:CharArrPtr absolute P; F:FieldDPtr; i,n,y,m,d,w:word;
    s:string[11];
const CtrlZ:char=^z;
begin
  P:=GetZStore(CFile^.FrstDispl); F:=CFile^.FldD; n:=0;
  while F<>nil do begin
    if F^.Flg and f_Stored<>0 then begin inc(n);
      with P^.Flds[n] do begin
        case F^.Typ of
          'F':begin Typ:='N'; Dec:=F^.M end; 'N':Typ:='N';
          'A':Typ:='C'; 'D':Typ:='D'; 'B':Typ:='L';
          'T':Typ:='M' end;
        Len:=F^.NBytes; Displ:=F^.Displ;
        s:=F^.Name; for i:=1 to length(s) do s[i]:=upcase(s[i]);
        StrLPCopy(Name,s,11);
      end end;
    F:=F^.Chain end;
  with P^ do begin
    if CFile^.TF<>nil then
      if CFile^.TF^.Format=FptFormat then Ver:=$f5 else Ver:=$83
    else Ver:=$03;
    RecLen:=CFile^.RecLen;
    SplitDate(Today,d,m,y); Date[1]:=y-1900; Date[2]:=m; Date[3]:=d;
    NRecs:=CFile^.NRecs; HdLen:=CFile^.FrstDispl;
    PA^[(HdLen div 32)*32+1]:=^m end;
  with CFile^ do begin
    RdWrCache(false,Handle,NotCached,0,FrstDispl,P^);
    RdWrCache(false,Handle,NotCached,longint(NRecs)*RecLen+FrstDispl,1,CtrlZ) end;
  ReleaseStore(P);
end;
var Pfx6:record NRs:longint;RLen:word end;
    Pfx8:record NRs,RLen:word end absolute Pfx6;
begin
  with CFile^ do begin if IsUpdHandle(Handle) then
    case Typ of
     '8':begin Pfx8.RLen:=RecLen;Pfx8.NRs:=NRecs;
           RdWrCache(false,Handle,NotCached,0,4,Pfx8) end;
     'D':WrDBaseHd;
     else Pfx6.RLen:=RecLen;
           if Typ='X' then Pfx6.NRs:=-NRecs else Pfx6.NRs:=NRecs;
           RdWrCache(false,Handle,NotCached,0,6,Pfx6);
end end end;

procedure WrPrefixes;
begin
  WrPrefix; with CFile^ do begin
    if (TF<>nil) and IsUpdHandle(TF^.Handle) then TF^.WrPrefix;
    if (Typ='X') and (XF^.Handle<>$ff){call from CopyDuplF} and
       (IsUpdHandle(XF^.Handle) or IsUpdHandle(Handle)) then XF^.WrPrefix end;
end;


procedure XFNotValid;
var XF:XFilePtr;
begin
  XF:=CFile^.XF; if XF=nil then exit;
  if XF^.Handle=$FF then RunError(903); XF^.SetNotValid;
end;

