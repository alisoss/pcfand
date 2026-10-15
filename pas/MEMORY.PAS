                       {  XMS + CACHE + MEMORY  }
{$ifdef FPC}
const
  ArenaSize = 256 * 1024 * 1024;
var
  Arena1Buf: PByte;
  Arena1Pos: PtrUInt;
  Arena1End: PtrUInt;
  Arena2Buf: PByte;
  Arena2Pos: PtrUInt;
  Arena2Start: PtrUInt;
function Normalize(L:longint):pointer; begin Normalize := pointer(PtrUInt(L)) end;
function AbsAdr(P:pointer):longint; begin AbsAdr := longint(PtrUInt(P)) end;
procedure CloseXMS;
begin end;
procedure MoveToXMS(NPage:word; var Src);
begin end;
procedure MoveFromXMS(NPage:word; var Dest);
begin end;
procedure SetMyHeapEnd;
begin end;
function CacheExist:boolean;
begin CacheExist := NCachePages > 0 end;
procedure NewCachePage(var ZLast:CachePagePtr; Z:CachePagePtr);
begin Z^.Handle:=$FF; Z^.Upd:=false; Z^.Chain:=nil; inc(NCachePages);
      ZLast^.Chain:=Z; ZLast:=Z end;
procedure FormatCache;
var ZLast: CachePagePtr; i: integer; p: CachePagePtr;
begin
  NCachePages := 0;
  ZLast := CachePagePtr(@CacheRoot);
  Stack2Ptr := nil;
  CacheEnd := nil;
  for i := 1 to 64 do begin
    GetMem(p, SizeOf(CachePage));
    p^.Handle := $FF; p^.Upd := false; p^.Chain := nil;
    inc(NCachePages);
    ZLast^.Chain := p; ZLast := p;
  end;
end;
procedure ResetCachePages;
var Z: CachePagePtr;
begin
  Z := CacheRoot;
  while Z <> nil do begin Z^.Handle := $FF; Z^.Upd := false; Z := Z^.Chain end;
end;
function WrCPage(Handle:word;N:longint;var Buf;ErrH:word):boolean;
var err:word;
begin
  SeekH(Handle,N shl CachePageShft); WriteH(Handle,CachePageSize,Buf);
  WrCPage:=true;
  if (HandleError<>0) and (ErrH<>Handle) then begin
    err:=HandleError; SaveCache(Handle);
    SetCPathForH(Handle); SetMsgPar(CPath); WrLLF10Msg(700+err);
    WrCPage:=false end;
end;
function WriteCachePage(Z:CachePagePtr;ErrH:word):boolean;
begin WriteCachePage:=WrCPage(Z^.Handle,Z^.HPage and $FFFFFF,Z^.Arr,ErrH) end;
procedure ReadCachePage(Z:CachePagePtr);
var err,h,n:word;
begin
  h:=Z^.Handle; SeekH(h,(Z^.HPage and $FFFFFF) shl CachePageShft);
  n:=ReadH(h,CachePageSize,Z^.Arr);
  if HandleError<>0 then begin
    err:=HandleError; SetCPathForH(h); SetMsgPar(CPath);
    RunError(700+err) end;
end;
function Cache(Handle:byte;Page:longint):CachePagePtr;
var Found: boolean; Y, Z: CachePagePtr;
    HPage: longint;
begin
  HPage := Page or (longint(Handle) shl 24);
  Found := false;
  Y := CachePagePtr(@CacheRoot);
  Z := CacheRoot;
  while Z <> nil do begin
    if Z^.HPage = HPage then begin
      Found := true;
      break;
    end;
    if Z^.Chain = nil then break;
    Y := Z;
    Z := Z^.Chain;
  end;
  if (Z <> nil) and (Z <> CacheRoot) then begin
    Y^.Chain := Z^.Chain;
    Z^.Chain := CacheRoot;
    CacheRoot := Z;
  end;
  if not Found then begin
    if Z^.Upd and (Z^.Handle <> $FF) then begin
      Z^.Upd := false;
      if IsHandle(Z^.Handle) then
        if not WriteCachePage(Z, 0) then GoExit;
    end;
    Z^.HPage := HPage;
    ReadCachePage(Z);
  end;
  Cache := Z;
end;
procedure LockCache;
begin end;
procedure UnLockCache;
begin end;
function SaveCache(ErrH:word):boolean;
var Z:CachePagePtr;
begin
  Z:=CacheRoot; SaveCache:=true;
  while Z<>nil do begin
    if Z^.Upd and (Z^.Handle<>$FF) and ((ErrH=0) or (Z^.Handle=ErrH))
    {$ifdef FPC} and IsHandle(Z^.Handle) {$endif}
    and not WriteCachePage(Z,ErrH) then SaveCache:=false;
    Z^.Upd:=false; Z:=Z^.Chain end;
end;
procedure ClearCacheH(h:word);
var Z:CachePagePtr;
begin
  if h=$FF then exit; Z:=CacheRoot;
  while Z<>nil do begin
    if Z^.Handle=h then begin Z^.Upd:=false; Z^.Handle:=$FF end;
    Z:=Z^.Chain end;
end;
procedure SubstHandle(h1,h2:word);
var Z:CachePagePtr;
begin
  Z:=CacheRoot;
  while Z<>nil do begin
    if Z^.Handle=h1 then Z^.Handle:=h2; Z:=Z^.Chain end;
end;
procedure FreeCachePage(Z:CachePagePtr);
var Z1,Z2:CachePagePtr;
begin
  if NCachePages<=1 then RunError(624); dec(NCachePages);
  Z1:=CachePagePtr(@CacheRoot); Z2:=CacheRoot;
  while Z2<>nil do begin
    if Z2=Z then begin
      Z1^.Chain:=Z2^.Chain;
      if Z2^.Upd then begin
        if not WriteCachePage(Z2,0) then GoExit end;
      exit end;
    Z1:=Z2; Z2:=Z2^.Chain end;
end;
procedure ExpandCacheUp;
begin end;
procedure ExpandCacheDown;
begin end;
function HeapErrFun(Size:word):integer; begin HeapErrFun := 0 end;
function GetStore(Size:StoreSize):pointer;
var p: pointer;
begin
  if (Arena1Pos < PtrUInt(Arena1Buf)) or (Arena1Pos + Size > Arena1End) then RunError(624);
  p := pointer(Arena1Pos);
  inc(Arena1Pos, Size);
  GetStore := p;
end;
function GetZStore(Size:StoreSize):pointer;
var p: pointer;
begin p := GetStore(Size); FillChar(p^, Size, 0); GetZStore := p end;
procedure AlignParagraph; begin Arena1Pos := (Arena1Pos + 15) and not PtrUInt(15) end;
function GetStore2(Size:StoreSize):pointer;
begin
  dec(Arena2Pos, Size);
  if (Arena2Pos < Arena2Start) or (Arena2Pos > Arena2Start+ArenaSize) then RunError(624);
  GetStore2 := pointer(Arena2Pos);
end;
function GetZStore2(Size:StoreSize):pointer;
var p: pointer;
begin p := GetStore2(Size); FillChar(p^, Size, 0); GetZStore2 := p end;
function StoreStr(S:string):StringPtr;
var p: StringPtr;
    len: byte;
begin
  len := Length(S);
  p := GetStore(len + 1);
  Move(S, p^, len + 1);
  StoreStr := p;
end;
procedure MarkStore(var p:pointer);
begin p := pointer(Arena1Pos) end;
procedure MarkStore2(var p:pointer);
begin p := pointer(Arena2Pos) end;
procedure MarkBoth(var p,p2:pointer);
begin p := pointer(Arena1Pos); p2 := pointer(Arena2Pos) end;
function InArena1(p:pointer):boolean;
begin InArena1 := (PtrUInt(p)>=PtrUInt(Arena1Buf)) and (PtrUInt(p)<=Arena1End) end;
function InArena2(p:pointer):boolean;
begin InArena2 := (PtrUInt(p)>=Arena2Start) and (PtrUInt(p)<=Arena2Start+ArenaSize) end;
procedure ReleaseStore(p:pointer); begin if not InArena1(p) then exit; Arena1Pos := PtrUInt(p) end;
procedure ReleaseAfterLongStr(p:pointer);
var lsp: LongStrPtr;
begin
  if not InArena1(p) then exit;
  lsp := LongStrPtr(p);
  Arena1Pos := PtrUInt(p) + lsp^.LL + 2;
  if Arena1Pos > Arena1End then Arena1Pos := Arena1End;
end;
procedure ReleaseStore2(p:pointer); begin if not InArena2(p) then exit; Arena2Pos := PtrUInt(p) end;
procedure ReleaseBoth(p,p2:pointer);
begin ReleaseStore(p); ReleaseStore2(p2) end;
function StoreAvail:longint;
var avail: PtrUInt;
begin
  if Arena1End > Arena1Pos then avail := Arena1End - Arena1Pos else avail := 0;
  if avail > PtrUInt(MaxLongint) then avail := PtrUInt(MaxLongint);
  StoreAvail := longint(avail);
end;
procedure AlignLongStr; begin Arena1Pos := (Arena1Pos + 15) and not PtrUInt(15) end;
procedure AlignSegment(Skip:word);
begin Arena1Pos := ((Arena1Pos + Skip + $FFFF) and not PtrUInt($FFFF)) - Skip end;
function HeapTop:pointer;
begin HeapTop := pointer(Arena1Pos) end;
                         {  EXIT  }
procedure NewExit(POvr:PProcedure; var Buf:ExitRecord);
begin
  Move(ExitBuf, Buf, sizeof(ExitRecord));
  ExitBuf.OvrEx := POvr;
  ExitBuf.ExP := ExitP;
  ExitBuf.BrkP := BreakP;
  ExitBuf.mBP := MyBP;
  GoExitFired := false;
end;
procedure GoExit;
var i:integer; allZero:boolean; bp:PByte;
begin
  ExitP := ExitBuf.ExP;
  BreakP := ExitBuf.BrkP;
  MyBP := ExitBuf.mBP;
  GoExitFired := true;
  allZero:=true; bp:=@ExitBuf.JmpBuf;
  for i:=0 to sizeof(ExitBuf.JmpBuf)-1 do
    if bp[i]<>0 then begin allZero:=false; break end;
  if allZero then halt(2);
  LongJmp(ExitBuf.JmpBuf, 1);
end;
procedure RestoreExit(var Buf:ExitRecord);
begin Move(Buf, ExitBuf, sizeof(ExitRecord)) end;
procedure KeepInMemory(const P:array of pointer);
begin end;
procedure InitMemoryArenas;
begin
  GetMem(Arena1Buf, ArenaSize);
  Arena1Pos := PtrUInt(Arena1Buf);
  Arena1End := Arena1Pos + ArenaSize;
  GetMem(Arena2Buf, ArenaSize);
  Arena2Start := PtrUInt(Arena2Buf);
  Arena2Pos := Arena2Start + ArenaSize;
end;
function FandShellRun(const cmd:ansistring):integer;
begin
{$ifdef UNIX}
  FandShellRun := (fpSystem(cmd) shr 8) and 255;
{$else}
  FandShellRun := SysUtils.ExecuteProcess('cmd.exe','/c '+cmd);
{$endif}
end;
function DosPathMap(const P:ansistring):ansistring;
begin DosPathMap := UnixPath(P) end;
{ BP7 tried the name, then .EXE and .COM: in the current directory, then
  on the PATH }
function FindProg(const Path:ansistring):ansistring;
const Ext:array[0..2] of string[4]=('','.EXE','.COM');
var p,q:ansistring; i,k:integer;
begin
  p := Path;
{$ifdef UNIX}
  for i := 1 to length(p) do if p[i] = '\' then p[i] := '/';
  k := 0;
{$else}
  k := 2;
{$endif}
  for i := 0 to k do begin
    q := p + Ext[i];
    if FileExists(q) then begin FindProg := q; exit end;
    if pos(DirectorySeparator,q) = 0 then begin
      q := FileSearch(q, GetEnv('PATH'));
      if q <> '' then begin FindProg := q; exit end end end;
  FindProg := '';
end;
function OSshell(Path:PathStr;CmdLine:String127;
   NoCancel,FreeMm,LdFont,TextMd:boolean):boolean;
var
  shellCmd, dosLine, prog: ansistring;
  dosRc: integer; dir0: string;
  rc: longint;                { a wait status: 16 bits cut exit codes > 127 }
  i, ErrNr: integer;
begin
  ErrNr := 0; LastExitCode := 0;
  if Path = '' then
    shellCmd := CmdLine
  else
    shellCmd := Path + ' ' + CmdLine;
  while (length(shellCmd) > 0) and (shellCmd[1] = ' ') do
    delete(shellCmd, 1, 1);
{$ifdef UNIX}
  for i := 1 to length(shellCmd) do
    if shellCmd[i] = '\' then shellCmd[i] := '/';
{$endif}
  dosLine := shellCmd;
  SaveCache(0);
  CallCloseFandFiles(false);
  FandDosShell := TShellRun(@FandShellRun);
  FandDosPathMap := TPathMap(@DosPathMap);
  GetDir(0,dir0);                      { a CD in the command lasts until its end }
  if shellCmd = '' then begin          { exec('','') - BP7's COMSPEC: a shell }
    RdMsg(003); TtyForChild(MsgLine);
{$ifdef UNIX}
    prog := GetEnv('SHELL'); if prog = '' then prog := '/bin/sh';
    rc := fpSystem(prog);
{$else}
    rc := SysUtils.ExecuteProcess(GetEnv('COMSPEC'), '') shl 8;
{$endif}
    TtyForFand end
  else if FandDosCmd(dosLine,dosRc) then rc := dosRc shl 8
  else begin
    { a named program that is not there: BP7's 605, whatever nocancel says }
    if Path <> '' then begin
      prog := FindProg(Path);
      if prog = '' then ErrNr := 605
{$ifdef UNIX}
      { DOS looked in the current directory first, sh does not }
      else if (pos('/',Path) = 0) and FileExists(prog) and (FileSearch(Path,GetEnv('PATH')) = '') then
        shellCmd := './' + shellCmd
{$endif}
      end;
    if ErrNr = 0 then begin
      TtyForChild('');
{$ifdef UNIX}
      rc := fpSystem(shellCmd);
{$else}
      rc := SysUtils.ExecuteProcess('cmd.exe', '/c ' + shellCmd) shl 8;
{$endif}
      TtyForFand end end;
  if ErrNr = 0 then
    if rc < 0 then ErrNr := 605
    else begin
      LastExitCode := (rc shr 8) and 255;
      { a named program's error ends the procedure unless nocancel (BP7) }
      if (Path <> '') and not NoCancel and (LastExitCode <> 0) then ErrNr := 606 end;
  {$I-} ChDir(dir0); {$I+} if IOResult <> 0 then ;
  ResetCachePages;
  CallOpenFandFiles(false);
  if ErrNr = 605 then LastExitCode := 127;            { as sh says it }
  if (ErrNr <> 0) and not OSShellQuiet then begin SetMsgPar(Path); WrLLF10Msg(ErrNr) end;
  OSShell := ErrNr = 0;
end;
{$else}
                        {  XMS + CACHE + MEMORY  }

function Normalize(L:longint):pointer; assembler;
asm
  mov dx,L[2].word; mov cl,12; shl dx,cl;
  mov bx,L.word; mov ax,bx; and ax,000FH;
  mov cl,4; shr bx,cl; add dx,bx;
end;

function AbsAdr(P:pointer):longint; assembler;
asm
  mov ax,P[2].word; mov dx,ax; mov cl,12; shr dx,cl;
  mov cl,4; shl ax,cl; add ax,P.word; adc dx,0
end;


procedure CloseXMS;
begin
  if XMSCachePages>0 then
  asm mov ah,0AH; mov dx,XMSHandle; call [XMSFun] end;
end;
                      
procedure MoveToXMS(NPage:word; var Src); assembler;
asm mov ax,NPage; mov cl,CachePageShft; sub cl,8; shl ax,cl;
    mov XMSOut.Dest[1].byte,al; mov XMSOut.Dest[2].byte,ah;
    les di,Src; mov XMSOut.Src.word,di; mov XMSOut.Src[2].word,es;
    push ds; lea si,XMSOut; cld;
    mov ah,0bH; call [XMSFun]; or ax,ax; je @1; mov bl,0;
@1: mov XMSError,bl; pop ds;
end;

procedure MoveFromXMS(NPage:word; var Dest); assembler;
asm mov ax,NPage; mov cl,CachePageShft; sub cl,8; shl ax,cl;
    mov XMSIn.Src[1].byte,al; mov XMSIn.Src[2].byte,ah;
    les di,Dest; mov XMSIn.Dest.word,di; mov XMSIn.Dest[2].word,es;
    push ds; lea si,XMSIn; cld;
    mov ah,0bH; call [XMSFun]; or ax,ax; je @1; mov bl,0;
@1: mov XMSError,bl; pop ds;
end;


function CacheExist:boolean;
  begin CacheExist:= NCachePages>0 end;

procedure SetMyHeapEnd;
begin MyHeapEnd:=ptr(PtrRec(CacheEnd).Seg-CachePageSz,PtrRec(CacheEnd).Ofs) end;

procedure NewCachePage(var ZLast:CachePagePtr; Z:CachePagePtr);
begin Z^.Handle:=$FF; Z^.Upd:=false; Z^.Chain:=nil; inc(NCachePages);
      ZLast^.Chain:=Z; ZLast:=Z end;

procedure FormatCache;
var ZLast:CachePagePtr;
begin
  NCachePages:=0; ZLast:=CachePagePtr(@CacheRoot); Stack2Ptr:=MemEnd;
  CacheEnd:=ptr(PtrRec(MemEnd).Seg-InitStack2Sz, 0); HeapEnd:=CacheEnd;
  while PtrRec(HeapEnd).Seg-CachePageSz >= PtrRec(HeapPtr).Seg+InitStackSz do begin
    dec(PtrRec(HeapEnd).Seg, CachePageSz); NewCachePage(ZLast,HeapEnd) end;
  SetMyHeapEnd;
end;


function WrCPage(Handle:word;N:longint;var Buf;ErrH:word):boolean;
var err:word;
begin
  SeekH(Handle,N shl CachePageShft); WriteH(Handle,CachePageSize,Buf);
  WrCPage:=true;
  if (HandleError<>0) and (ErrH<>Handle) then begin
    err:=HandleError; SaveCache(Handle);
    SetCPathForH(Handle); SetMsgPar(CPath); WrLLF10Msg(700+err);
    WrCPage:=false end;
end;
function WriteCachePage(Z:CachePagePtr;ErrH:word):boolean;
begin
  WriteCachePage:=WrCPage(Z^.Handle,Z^.HPage and $FFFFFF,Z^.Arr,ErrH);
end;

procedure ReadCachePage(Z:CachePagePtr);
var err,h,n:word;
begin
  h:=Z^.Handle; SeekH(h,(Z^.HPage and $FFFFFF) shl CachePageShft);
  n:=ReadH(h,CachePageSize,Z^.Arr);
  if HandleError<>0 then begin
    err:=HandleError; SetCPathForH(h); SetMsgPar(CPath);
    RunError(700+err) end;
end;

function Cache(Handle:byte;Page:longint):CachePagePtr;
var
  Found,b:boolean; Y,Z:CachePagePtr; Y1,Z1:XMSCacheDPtr;
  n:longint;
begin
  Found:=false;
  asm push ds;
    mov dh,Handle; mov Page[3].byte,dh; mov dl,Page[2].byte; mov ax,Page.word;
    lea si,CacheRoot;
@1: mov cx,ds; mov bx,si; lds si,[si].CachePage.Chain;
    cmp [si].CachePage.HPage.word,ax;     jne @2;
    cmp [si].CachePage.HPage[2].word,dx;  jne @2;
    mov Found,1;                          jmp @3;
@2: cmp [si].CachePage.Chain[2].word,0;   jne @1;
@3: mov Z.word,si; mov Z[2].word,ds; mov Y.word,bx; mov Y[2].word,cx;
    pop ds
  end;
  if Z<>CacheRoot then begin
    Y^.Chain:=Z^.Chain; Z^.Chain:=CacheRoot; CacheRoot:=Z end;
  if not Found and (XMSCachePages>0) then begin
    asm push ds;
      mov dx,Page[2].word; mov ax,Page.word;
      lea si,XMSCacheRoot;
  @1: mov cx,ds; mov bx,si; lds si,[si].XMSCacheD.Chain;
      cmp [si].XMSCacheD.HPage.word,ax;     jne @2;
      cmp [si].XMSCacheD.HPage[2].word,dx;  jne @2;
      mov Found,1;                          jmp @3;
  @2: cmp [si].XMSCacheD.Chain[2].word,0;   jne @1;
  @3: mov Z1.word,si; mov Z1[2].word,ds; mov Y1.word,bx; mov Y1[2].word,cx;
      pop ds
    end;
    if Z1<>XMSCacheRoot then begin
      Y1^.Chain:=Z1^.Chain; Z1^.Chain:=XMSCacheRoot; XMSCacheRoot:=Z1 end;
    if Found or Z1^.Upd then begin
      MoveFromXMS(Z1^.XMSPage,XMSCacheBuf^);
      if Z^.Handle<>$FF then MoveToXMS(Z1^.XMSPage,Z^.Arr);
      Move(XMSCacheBuf^,Z^.Arr,CachePageSize) end
    else if Z^.Handle<>$FF then MoveToXMS(Z1^.XMSPage,Z^.Arr);
    n:=Z^.HPage;b:=Z^.Upd; Z^.HPage:=Z1^.HPage;Z^.Upd:=Z1^.Upd;
    Z1^.HPage:=n; Z1^.Upd:=b;
    end;
  if not Found then begin
    if Z^.Upd and (Z^.Handle<>$FF) then begin
      Z^.Upd:=false;if not WriteCachePage(Z,0) then GoExit end;
    Z^.HPage:=Page; ReadCachePage(Z) end;
  Cache:=Z;
end;

procedure LockCache;
begin end;

procedure UnLockCache;
begin end;

function SaveCache(ErrH:word):boolean;
var Z:CachePagePtr; Z1:XMSCacheDPtr;
begin
  Z:=CacheRoot; SaveCache:=true;
  while Z<>nil do begin
    if Z^.Upd and (Z^.Handle<>$FF) and ((ErrH=0) or (Z^.Handle=ErrH)) and
       not WriteCachePage(Z,ErrH) then SaveCache:=false;
    Z^.Upd:=false; Z:=Z^.Chain end;
  Z1:=XMSCacheRoot;
  while Z1<>nil do begin
    if Z1^.Upd and (Z1^.Handle<>$FF) and ((ErrH=0) or (Z1^.Handle=ErrH)) then begin
      MoveFromXMS(Z1^.XMSPage,XMSCacheBuf^);
      if not WrCPage(Z1^.Handle,Z1^.HPage and $FFFFFF,XMSCacheBuf^,ErrH) then
        SaveCache:=false end;
    Z1^.Upd:=false; Z1:=Z1^.Chain end;
end;

procedure ClearCacheH(h:word);
var Z:CachePagePtr; Z1:XMSCacheDPtr;
begin
  if h=$FF then exit; Z:=CacheRoot;
  while Z<>nil do begin
    if Z^.Handle=h then begin Z^.Upd:=false; Z^.Handle:=$FF end;
    Z:=Z^.Chain end;
  Z1:=XMSCacheRoot;
  while Z1<>nil do begin
    if Z1^.Handle=h then begin Z1^.Upd:=false; Z1^.Handle:=$FF end;
    Z1:=Z1^.Chain end;
end;

procedure SubstHandle(h1,h2:word);
var Z:CachePagePtr; Z1:XMSCacheDPtr;
begin
  Z:=CacheRoot;
  while Z<>nil do begin
    if Z^.Handle=h1 then Z^.Handle:=h2; Z:=Z^.Chain end;
  Z1:=XMSCacheRoot;
  while Z1<>nil do begin
    if Z1^.Handle=h1 then Z1^.Handle:=h2; Z1:=Z1^.Chain end;
end;

procedure FreeCachePage(Z:CachePagePtr);
var Z1,Z2:CachePagePtr;
begin
  if NCachePages<=1 then RunError(624); dec(NCachePages);
  Z1:=CachePagePtr(@CacheRoot); Z2:=CacheRoot;
  while Z2<>nil do begin
    if Z2=Z then begin
      Z1^.Chain:=Z2^.Chain;
      if Z2^.Upd then begin
        if not WriteCachePage(Z2,0) then GoExit end;
      exit end;
    Z1:=Z2; Z2:=Z2^.Chain end;
end;
procedure ExpandCacheUp;
var XLast:CachePagePtr;
begin
  XLast:=CacheRoot; while XLast^.Chain<>nil do XLast:=XLast^.Chain;
  while PtrRec(CacheEnd).Seg+CachePageSz<=PtrRec(Stack2Ptr).Seg-InitStack2Sz do
    begin NewCachePage(XLast,CacheEnd); inc(PtrRec(CacheEnd).Seg,CachePageSz) end;
  SetMyHeapEnd;
end;
procedure ExpandCacheDown;
var XLast:CachePagePtr;
begin
  XLast:=CacheRoot; while XLast^.Chain<>nil do XLast:=XLast^.Chain;
  while PtrRec(HeapEnd).Seg-CachePageSz>=PtrRec(HeapPtr).Seg+InitStackSz do
    begin dec(PtrRec(HeapEnd).Seg,CachePageSz); NewCachePage(XLast,HeapEnd)
end end;


function HeapErrFun(Size:word):integer;
var i,n:word;
begin
  if Size=0 then exit;
  n:=(PtrRec(HeapPtr).Seg+1+(Size shr 4)-PtrRec(HeapEnd).Seg)
     div CachePageSz + 1;
  for i:=1 to n do begin
    FreeCachePage(HeapEnd); inc(PtrRec(HeapEnd).Seg, CachePageSz) end;
  HeapErrFun:=2;
end;

function GetStore(Size:word):pointer; assembler;
asm
@1: mov dx,HeapPtr[2].word; mov ax,HeapPtr.word; mov bx,Size; mov si,bx;
    mov cl,4; shr bx,cl; add dx,bx;
    and si,000FH; add ax,si; add al,0F0H; adc dx,0; and al,0FH;
    cmp dx,HeapEnd[2].word; jb @3; ja @2; cmp ax,HeapEnd.word; jna @3;
@2: push Size; call HeapErrFun; jmp @1;
@3: xchg ax,HeapPtr.word; xchg dx,HeapPtr[2].word;
end;
function GetZStore(Size:word):pointer; assembler;
asm
   push Size; call GetStore;
   mov bx,ax; mov es,dx; mov di,ax; mov al,0; mov cx,Size; cld; rep stosb;
   mov ax,bx;
end;
procedure AlignParagraph; assembler;
asm  mov cx,HeapPtr.word; jcxz @1;
     mov ax,16; sub ax,cx; push ax; call GetStore; @1: end;

function GetStore2(Size:word):pointer;
var i,n:word; p,p2:pointer;
begin
  p:=Normalize(AbsAdr(Stack2Ptr)-Size); 
  if PtrRec(p).Seg<PtrRec(CacheEnd).Seg then begin
    n:=(PtrRec(CacheEnd).Seg-PtrRec(p).Seg) div CachePageSz +1;
    for i:=1 to n do begin
      p2:=ptr(PtrRec(CacheEnd).Seg-CachePageSz, 0); FreeCachePage(p2);
      CacheEnd:=p2 end;
    SetMyHeapEnd end;
  GetStore2:=p; Stack2Ptr:=p;
end;               
function GetZStore2(Size:word):pointer; assembler;
asm
   push Size; call GetStore2;
   mov bx,ax; mov es,dx; mov di,ax; mov al,0; mov cx,Size; cld; rep stosb;
   mov ax,bx;
end;
function StoreStr(S:string):StringPtr; assembler;
asm  les di,S; xor ch,ch; mov cl,es:[di]; inc cx; push cx;
     push cx; call GetStore;
     mov es,dx; mov di,ax; pop cx; push ds; lds si,S; cld; rep movsb;
     pop ds end;

{$S-}
procedure MarkStore(var p:pointer);
  begin p:=HeapPtr end;
procedure MarkStore2(var p:pointer);
  begin p:=Stack2Ptr end;
procedure MarkBoth(var p,p2:pointer);
  begin p:=HeapPtr; p2:=Stack2Ptr end;

procedure ReleaseStore(p:pointer); assembler;
asm  les di,p;  { normalize and release }
     mov ax,di; and di,000FH; mov cl,4; shr ax,cl; mov dx,es; add dx,ax;
     mov HeapPtr.word,di; mov HeapPtr[2].word,dx;
                { shorten FreeList }
     mov di,Offset ds:FreeList; mov ax,ds; mov es,ax;
@1:  mov si,di; mov bx,es; les di,es:[di]; mov dx,es;
     cmp dx,0; je @4; cmp dx,HeapPtr[2].word; jnb @3; mov ax,di;
     add ax,es:[di+4].word; add dx,es:[di+6].word;  {FreeElPtr+SizePtr}
     add al,0f0H; adc dx,0; and al,0fH;
     cmp dx,HeapPtr[2].word; jb @1; ja @2;
     cmp ax,HeapPtr.word; jb @1;
@2:  mov HeapPtr.word,di; mov HeapPtr[2].word,es;
@3:  mov es,bx; mov es:[si].word,0; mov es:[si+2].word,0;
@4:  mov ax,HeapEnd[2].word; sub ax,HeapPtr[2].word;
     cmp ax,MinStackSz; jle @5; call ExpandCacheDown;
@5:
end;
procedure ReleaseAfterLongStr(p:pointer); assembler;
asm les di,p; add di,es:[di].word; add di,2; mov ax,di; and di,000FH;
    mov cl,4; shr ax,cl; mov dx,es; add dx,ax;
    mov HeapPtr.word,di; mov HeapPtr[2].word,dx;
end;
procedure ReleaseStore2(p:pointer); assembler;
asm les di,p;  { normalize and release }
    mov ax,di; and di,000FH; mov cl,4; shr ax,cl; mov dx,es; add dx,ax;
    mov Stack2Ptr.word,di; mov Stack2Ptr[2].word,dx;
    sub dx,CacheEnd[2].word; cmp dx,MinStack2Sz; jle @1;
    call ExpandCacheUp;
@1: end;
procedure ReleaseBoth(p,p2:pointer);
  begin ReleaseStore(p); ReleaseStore2(p2) end;
{$S+}

function StoreAvail:longint;
begin
  StoreAvail:=longint((NCachePages-1)*CachePageSz
              +PtrRec(HeapEnd).Seg-PtrRec(HeapPtr).Seg-2) shl 4;
end;
procedure AlignLongStr;
var n:integer;
begin
  n:=14-PtrRec(HeapPtr).Ofs; if n<0 then inc(n,16); GetStore(n);
end;
procedure AlignSegment(Skip:word);
begin end;
function HeapTop:pointer;
begin HeapTop := HeapPtr end;


                         {  EXIT  }


procedure NewExit(POvr:PProcedure; var Buf:ExitRecord);
begin
  Move(ExitBuf,Buf,sizeof(ExitRecord));
  asm  les  di, POvr
       mov  ExitBuf.OvrEx.word, di
       mov  ExitBuf.OvrEx[2].word, es
       mov  ax, [bp].word
       mov  ExitBuf.rBP,ax
       mov  ax, [bp+2].word
       mov  ExitBuf.rIP, ax
       mov  ax, [bp+4].word
       mov  ExitBuf.rCS, ax
       lea  ax, [bp+14]
       mov  ExitBuf.rSP, ax   
       mov  ExitBuf.rDS, ds
       add  [bp+2].word, 2
       les  di, ExitBuf.rIP.pointer
       cmp  es:[di].byte, 0e9H  { jump 16? }
       jne  @1
       inc  [bp+2].word
    @1:end;
  if not (Mem[ExitBuf.rCS:ExitBuf.rIP] in [$EB, $E9]) then
    begin Move(Buf,ExitBuf,sizeof(ExitRecord));RunError(901) end;
  ExitBuf.ExP:=ExitP; ExitBuf.BrkP:=BreakP; ExitBuf.mBP:=MyBP;
end;

procedure GoExit; assembler;
asm    mov al,ExitBuf.ExP; mov ExitP,al;
       mov al,ExitBuf.BrkP; mov BreakP,al;
       les di,ExitBuf.mBP; mov MyBP.word,di; mov MyBP[2].word,es;
       push ExitBuf.rSP
       push ExitBuf.rCS
       push ExitBuf.rIP
       push ExitBuf.rDS
       push ExitBuf.rBP
       call ExitBuf.OvrEx
end;

procedure RestoreExit(var Buf:ExitRecord);
  begin Move(Buf,ExitBuf,sizeof(ExitRecord)) end;


function OSshell(Path:PathStr;CmdLine:String127;
       NoCancel,FreeMm,LdFont,TextMd:boolean):boolean;
var rr:registers;
    OldMyHeapOrg,OldHeapPtr,OldMemEnd,Env,EnvrPtr,p:pointer;
    EndOfMemSeg,HeapPtrSeg,h,l,retcode:word;
    ErrNr,i,iExt:word; er:ExitRecord;
    CurrDrive:byte; CurrPath:array[1..64]of char;
    ProgName:PathStr;  s:string[16];
    FCB1,FCB2:array[1..16] of byte;
    EPB:record EnvSeg:word; Param:StringPtr; pFCB1,pFCB2:pointer end;
    d:DirStr; n:NameStr; e:ExtStr;
    nbytes,nn:longint;
    OldScrSeg,NewEnvSeg,w:word;
const Ext:array[0..2]of string[4]=('','.EXE','.COM');
label 1,2;
begin
  SaveCache(0);
  NewExit(NoOvr,er); goto 2;{ current exit will be overwritten by DMLConnect }
  if IsGraphMode and TextMd then OldScrSeg:=ScrTextMode(true,true)
  else TextMd:=false;
  if Path='' then begin
    ProgName:=getenv('COMSPEC');
    if CmdLine<>'' then CmdLine:='/C '+CmdLine
    else begin TextAttr:=$07;window(1,1,TxtCols,TxtRows);clrscr;
               RdMsg(003); write(MsgLine,'...') end end
  else begin
    for i:=0 to 2 do begin
    ProgName:=FSearch(Path+Ext[i],getenv('PATH'));
    if ProgName<>'' then goto 1 end;
    ProgName:=Path; ErrNr:=605; goto 2;
1:  FSplit(ProgName,d,n,e);
    if not SEquUpcase(e,Ext[1]) and not SEquUpcase(e,Ext[2]) then begin
       ErrNr:=646; goto 2 end end;
  ProgName:=FExpand(ProgName)+#0; ErrNr:=0;
  CrsNorm; CrsIntrDone; BreakIntrDone;
  DoneMouseEvents;
  if length(CmdLine)=127 then dec(CmdLine[0]);
  CmdLine[length(CmdLine)+1]:=#13;
  str(_CallDMLAddr:6, s); s:='DMLADDR='+s; s[15]:=#0; s[16]:=#0;
  OldMyHeapOrg:=MyHeapOrg; OldHeapPtr:=HeapPtr; OldMemEnd:=MemEnd;
  HeapPtrSeg:=seg(HeapPtr^)+1; EndOfMemSeg:=CurPSP^.FreeSeg;
  RdMsg(902);
  CallCloseFandFiles(false);
  NCachePages:=0;
  asm mov ah,19H; int 21H; mov CurrDrive,al end; { get CurrDrive }
  FillChar(CurrPath,sizeof(CurrPath),0); CurrPath[1]:='\';
  asm push ds; mov ax,ss; mov ds,ax; lea si,CurrPath; inc si; mov dl,0;
      mov ah,47H; int 21H; pop ds end;           { get CurrPath  }
  if FreeMm then begin
    p:=ptr(OvrHeapOrg,0); nbytes:=longint(HeapPtrSeg-OvrHeapOrg)*16;
    SeekH(WorkHandle,0); WriteLongH(WorkHandle,nbytes,p);
    NewEnvSeg:=OvrHeapOrg end
  else begin
    CloseH(WorkHandle); CloseH(ResFile.Handle); CloseH(OvrHandle);
    UnExtendHandles;
    NewEnvSeg:=HeapPtrSeg end;
  EnvrPtr:=ptr(CurPSP^.EnvrSeg,0);
    {count curr.environm. length, copy and add new}
    { pass DMLinit addr by exec in environment 'DMLADDR=xxxxxx' }
  asm  les di,EnvrPtr; cld; mov cx,7fffH; mov al,0;
    @1:cmp es:[di],al; je @2; repne scasb; jmp @1; @2:mov L,di end;
  Env:=ptr(NewEnvSeg,0); Move(EnvrPtr^,ptr(NewEnvSeg,0)^,L);
  Move(s[1],ptr(NewEnvSeg,L)^,16); w:=NewEnvSeg+1+((L+15)shr 4);
  rr.ES:=PrefixSeg; rr.BX:=w-PrefixSeg; rr.AH:=$4A; MsDos(rr);
     { modify allocated memory , free beginning with HeapPtrSeg or OvrHeapOrg  }
  swapvectors;
  asm  mov Fand_ss,ss; mov Fand_sp,sp; mov Fand_bp,bp;
       mov ax,ss; mov es,ax; mov ds,ax;
       mov ax,Env[2].word; mov EPB.EnvSeg,ax;
       lea si,CmdLine; mov EPB.Param.word,si; mov EPB.Param[2].word,ss;
       inc si; lea di,FCB1; mov EPB.pFCB1.word,di; mov EPB.pFCB1[2].word,ss;
       mov ax,2901H; int 21H; { fill FCB1 }
       lea di,FCB2; mov EPB.pFCB2.word,di; mov EPB.pFCB2[2].word,ss;
       mov ax,2901H; int 21H; { fill FCB2 }
       lea dx,ProgName; inc dx; lea bx,EPB;
       mov ax,4b00H; int 21H;   { Exec }
       mov dx,SEG @Data; mov ds,dx; mov ss,Fand_ss; mov sp,Fand_sp;
       mov bp,Fand_bp; jc @1; xor ax,ax; @1:mov DosError,ax;
       mov ah,4dH; int 21H; {get child return value }
       mov LastExitCode.word,ax; mov LastExitCode[2].word,0 end;
  swapvectors;
  if DosError=8 then ErrNr:=610
  else if DosError<>0 then ErrNr:=605
  else if not NoCancel and (LastExitCode<>0) then ErrNr:=606;
  rr.DL:=CurrDrive;rr.AH:=$0E;MsDos(rr);  { set CurrDrive and CurrPath }
  rr.DS:=Seg(CurrPath);rr.DX:=Ofs(CurrPath);rr.AH:=$3B;MsDos(rr);
  CurPSP:=ptr(PrefixSeg,0);
  MyHeapOrg:=OldMyHeapOrg;HeapPtr:=OldHeapPtr;MemEnd:=OldMemEnd;
  MyHeapEnd:=MemEnd;
  rr.ES:=PrefixSeg;rr.BX:=EndOfMemSeg-PrefixSeg;rr.AH:=$4A;MsDos(rr);
      { modify allocated memory to the old size }
  if rr.Flags and FCarry <>0 then begin
    write('FAND - memory allocation error (resident routines?)');wait;halt end;
  if FreeMm then begin
    SeekH(WorkHandle,0); ReadLongH(WorkHandle,nbytes,p) end
  else begin
    ExtendHandles; OpenOvrFile; OpenResFile; OpenWorkH end;
  FormatCache;
  CallOpenFandFiles(false);
  CrsHide; CrsIntrInit; BreakIntrInit;
  InitMouseEvents;
2:RestoreExit(er);
  if TextMd then ScrGraphMode(true,OldScrSeg)
  else if LdFont then LoadVideoFont;
  if ErrNr<>0 then begin SetMsgPar(ProgName); WrLLF10Msg(ErrNr) end;
  OSShell:=ErrNr=0;
end;{$endif}
