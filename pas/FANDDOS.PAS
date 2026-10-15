{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L-}
unit fanddos;
{ DOS commands and .BAT files, run inside the binary }
interface
function FandDosCmd(const Line:ansistring; var ExitCode:integer):boolean;
type TShellRun = function(const cmd:ansistring):integer;
var FandDosShell:TShellRun;
procedure FandFileTime(Stamp:longint; var y,mo,d,hh,mi,ss:word);
type
  TArgs = array[1..16] of ansistring;
  TDosExtra = function(const Nm,Ext,Dir:ansistring; var Av:TArgs; nAv:integer;
                       var ExitCode:integer):boolean;
var FandDosExtra:TDosExtra;
var FandDosNotice:ansistring;
{ FAND's path mapping (drive letters, \) for the built-in commands' paths }
type TPathMap = function(const P:ansistring):ansistring;
var FandDosPathMap:TPathMap;
function FandMaskMatch(const Nm,Mask:ansistring):boolean;
function FandSlashed(const P:ansistring):ansistring;
function FandWithSlash(const P:ansistring):ansistring;
function FandDirPart(const P:ansistring):ansistring;
function FandNamePart(const P:ansistring):ansistring;
function FandUpStr(const S:ansistring):ansistring;
function FandIntStr(V:int64):ansistring;
function FandReadWhole(const Nm:ansistring):ansistring;
function FandReadFile(const Nm:ansistring; var S:ansistring):boolean;
function FandWriteWhole(const Nm,Data:ansistring; Append:boolean):boolean;
function FandReplaceFile(const Nm,Data:ansistring):integer;
function FandFileExists(const Nm:ansistring):boolean;
function FandDirExists(const Nm:ansistring):boolean;
function FandRename(const Old,New:ansistring):boolean;
function FandListFiles(const Pattern:ansistring; DirsOnly,FullPath:boolean):ansistring;
function FandTotalSize(const Pattern:ansistring; var Total:int64;
                       var Newest:longint):boolean;
procedure FandDeleteFiles(const Pattern:ansistring);
function FandSearchDown(const Dir,Mask:ansistring; Depth:integer):ansistring;
function FandStamp(T:longint):ansistring;
implementation
uses sysutils{$ifdef UNIX},BaseUnix{$endif};
{$ifdef UNIX}
type
  PTmRec = ^TTmRec;
  TTmRec = record
    sec,min,hour,mday,mon,year,wday,yday,isdst:longint;
    gmtoff:int64; zone:pchar;
  end;
function localtime_r(tp:Pint64; res:PTmRec):PTmRec; cdecl; external 'c' name 'localtime_r';
{$endif}
procedure FandFileTime(Stamp:longint; var y,mo,d,hh,mi,ss:word);
{$ifdef UNIX}
var t:int64; r:TTmRec;
begin
  t := Stamp;
  if localtime_r(@t,@r)=nil then begin
    y := 1980; mo := 1; d := 1; hh := 0; mi := 0; ss := 0; exit end;
  y := r.year+1900; mo := r.mon+1; d := r.mday;
  hh := r.hour; mi := r.min; ss := r.sec;
end;
{$else}
var dt:TDateTime; ms:word;
begin dt := FileDateToDateTime(Stamp); DecodeDate(dt,y,mo,d); DecodeTime(dt,hh,mi,ss,ms) end;
{$endif}
const
  CRLF = #13#10;
  acSize = 1; acListPath = 2; acListName = 3; acDelete = 4; acCopy = 5;
var
  gText:ansistring;
  gTotal:int64;
  gNewest:longint;
  gFound:boolean;
  gCount:integer;
  gDest:ansistring;
function UpStr(const s:ansistring):ansistring;
var i:integer; r:ansistring;
begin
  r := s;
  for i := 1 to length(r) do
    if r[i] in ['a'..'z'] then r[i] := chr(ord(r[i])-32);
  UpStr := r;
end;
function Trim2(const s:ansistring):ansistring;
var a,b:integer;
begin
  a := 1; b := length(s);
  while (a<=b) and ((s[a]=' ') or (s[a]=#9)) do inc(a);
  while (b>=a) and ((s[b]=' ') or (s[b]=#9)) do dec(b);
  Trim2 := copy(s,a,b-a+1);
end;
function IntStr(v:int64):ansistring;
var s:ansistring;
begin str(v,s); IntStr := s end;
function Pad0(v,w:integer):ansistring;
var s:ansistring;
begin s := IntStr(v); while length(s)<w do s := '0'+s; Pad0 := s end;
procedure AddLine(const s:ansistring);
begin gText := gText + s + CRLF end;
function MaskMatch(const nm,mk:ansistring):boolean;
var i,j,si,sj:integer;
begin
  MaskMatch := false;
  i := 1; j := 1; si := 0; sj := 0;
  while i<=length(nm) do begin
    if (j<=length(mk)) and ((mk[j]='?') or (UpCase(mk[j])=UpCase(nm[i]))) then begin
      inc(i); inc(j) end
    else if (j<=length(mk)) and (mk[j]='*') then begin
      si := i; sj := j; inc(j) end
    else if sj>0 then begin
      inc(si); i := si; j := sj+1 end
    else exit;
  end;
  while (j<=length(mk)) and (mk[j]='*') do inc(j);
  MaskMatch := j>length(mk);
end;
function DirPart(const p:ansistring):ansistring;
var i:integer;
begin
  DirPart := '';
  for i := length(p) downto 1 do
    if (p[i]='/') or (p[i]='\') then begin DirPart := copy(p,1,i); exit end;
end;
function NamePart(const p:ansistring):ansistring;
begin NamePart := copy(p,length(DirPart(p))+1,length(p)) end;
function Slashed(const p:ansistring):ansistring;
var i:integer; r:ansistring;
begin
  r := p;
  for i := 1 to length(r) do
    if (r[i]='\') or (r[i]='/') then r[i] := DirectorySeparator;
  Slashed := r;
end;
function DosPath(const p:ansistring):ansistring;
begin
  if (p<>'') and (@FandDosPathMap<>nil) then DosPath := FandDosPathMap(p)
  else DosPath := Slashed(p);
end;
function WithSlash(const p:ansistring):ansistring;
begin
  WithSlash := p;
  if (p<>'') and (p[length(p)]<>DirectorySeparator) then
    WithSlash := p + DirectorySeparator;
end;
function ReadFile(const nm:ansistring; var s:ansistring):boolean;
var f:file; n:longint; fm:byte; ok:boolean;
begin
  ReadFile := false; s := '';
  if nm='' then exit;
  fm := FileMode; FileMode := 0;            { read-only: a read-only file reads too }
  {$I-} assign(f,nm); reset(f,1); FileMode := fm; if IOResult<>0 then exit;
  n := FileSize(f);
  SetLength(s,n); if n>0 then BlockRead(f,s[1],n);
  ok := IOResult=0;
  close(f); if IOResult<>0 then ; {$I+}
  if not ok then s := '';
  ReadFile := ok;
end;
function ReadWhole(const nm:ansistring):ansistring;
var s:ansistring;
begin ReadFile(nm,s); ReadWhole := s end;
function WriteWhole(const nm,data:ansistring; append:boolean):boolean;
var f:file;
begin
  WriteWhole := false;
  if nm='' then exit;
  WriteWhole := true;
  {$I-}
  assign(f,nm);
  if append then begin
    reset(f,1);
    if IOResult<>0 then rewrite(f,1) else seek(f,FileSize(f));
  end else rewrite(f,1);
  if IOResult<>0 then begin WriteWhole := false; exit end;
  if data<>'' then BlockWrite(f,data[1],length(data));
  if IOResult<>0 then WriteWhole := false;
  close(f);
  if IOResult<>0 then ;
  {$I+}
end;
function CopyOne(const src,dst:ansistring):boolean;
var s:ansistring;
begin CopyOne := ReadFile(src,s) and WriteWhole(dst,s,false) end;
function IsNul(const nm:ansistring):boolean;
var u:ansistring;
begin u := UpStr(nm); IsNul := (u='NUL') or (u='NUL:') or (u='/DEV/NULL') end;
procedure ForEachFile(const pattern:ansistring; wantDir:boolean; act:integer);
var sr:TSearchRec; dir,mk,full:ansistring; every,isdir:boolean;
begin
  dir := DirPart(pattern); mk := NamePart(pattern);
  if mk='' then mk := '*';
  every := mk='*.*';
  if dir='' then dir := '.'+DirectorySeparator;
  if SysUtils.FindFirst(dir+'*',faAnyFile,sr)<>0 then begin
    SysUtils.FindClose(sr); exit end;
  repeat
    if (sr.Name='.') or (sr.Name='..') then continue;
    isdir := (sr.Attr and faDirectory)<>0;
    if isdir<>wantDir then continue;
    if not (every or MaskMatch(sr.Name,mk)) then continue;
    full := dir + sr.Name;
    case act of
      acSize: begin
        if not gFound then begin gFound := true; gTotal := 0 end;
        inc(gTotal,sr.Size);
        if sr.Time>gNewest then gNewest := sr.Time;
      end;
      acListPath: AddLine(full);
      acListName: AddLine(sr.Name);
      acDelete:   SysUtils.DeleteFile(full);
      acCopy:     if CopyOne(full,WithSlash(gDest)+sr.Name) then inc(gCount);
    end;
  until SysUtils.FindNext(sr)<>0;
  SysUtils.FindClose(sr);
end;
procedure SearchDown(const dir,mk:ansistring; depth:integer);
var sr:TSearchRec;
begin
  if depth<0 then exit;
  if SysUtils.FindFirst(dir+'*',faAnyFile,sr)<>0 then begin
    SysUtils.FindClose(sr); exit end;
  repeat
    if (sr.Name='.') or (sr.Name='..') then continue;
    if (sr.Attr and faDirectory)<>0 then
      SearchDown(WithSlash(dir+sr.Name),mk,depth-1)
    else if MaskMatch(sr.Name,mk) then AddLine(dir+sr.Name);
  until SysUtils.FindNext(sr)<>0;
  SysUtils.FindClose(sr);
end;
function StampOf(t:longint):ansistring;
var y,m,d,hh,mm,ss:word;
begin
  FandFileTime(t,y,m,d,hh,mm,ss);
  StampOf := Pad0(d,2)+'.'+Pad0(m,2)+'.'+Pad0(y,4)+' '+Pad0(hh,2)+':'+Pad0(mm,2);
end;
procedure SortText;
var l:array[1..512] of ansistring; n,i,j,p:integer; s,t:ansistring;
begin
  n := 0; s := gText;
  while (s<>'') and (n<512) do begin
    p := pos(CRLF,s);
    if p=0 then begin inc(n); l[n] := s; s := '' end
    else begin inc(n); l[n] := copy(s,1,p-1); s := copy(s,p+2,length(s)) end;
  end;
  for i := 2 to n do begin
    t := l[i]; j := i-1;
    while (j>=1) and (UpStr(l[j])>UpStr(t)) do begin l[j+1] := l[j]; dec(j) end;
    l[j+1] := t;
  end;
  gText := '';
  for i := 1 to n do gText := gText + l[i] + CRLF;
end;
function FandMaskMatch(const Nm,Mask:ansistring):boolean;
begin FandMaskMatch := MaskMatch(Nm,Mask) end;
function FandSlashed(const P:ansistring):ansistring;
begin FandSlashed := Slashed(P) end;
function FandWithSlash(const P:ansistring):ansistring;
begin FandWithSlash := WithSlash(P) end;
function FandDirPart(const P:ansistring):ansistring;
begin FandDirPart := DirPart(P) end;
function FandNamePart(const P:ansistring):ansistring;
begin FandNamePart := NamePart(P) end;
function FandUpStr(const S:ansistring):ansistring;
begin FandUpStr := UpStr(S) end;
function FandIntStr(V:int64):ansistring;
begin FandIntStr := IntStr(V) end;
function FandReadWhole(const Nm:ansistring):ansistring;
begin FandReadWhole := ReadWhole(Nm) end;
function FandReadFile(const Nm:ansistring; var S:ansistring):boolean;
begin FandReadFile := ReadFile(Nm,S) end;
function FandWriteWhole(const Nm,Data:ansistring; Append:boolean):boolean;
begin FandWriteWhole := WriteWhole(Nm,Data,Append) end;
{ Nm's new contents go into a new file renamed over it, so that a failed
  write leaves Nm as it was; a link, or another owner's file, is rewritten
  in place. 0, or the I/O error (5: may not write). }
function FandReplaceFile(const Nm,Data:ansistring):integer;
var tmp:ansistring; e:integer;
    {$ifdef UNIX} st:TStat; {$endif}
  function Put(const fn:ansistring):integer;
  var f:file; r:integer;
  begin
    {$I-} assign(f,fn); rewrite(f,1); r := IOResult;
    if r=0 then begin
      if Data<>'' then BlockWrite(f,Data[1],length(Data));
      r := IOResult; close(f);
      if r=0 then r := IOResult else if IOResult<>0 then ;
    end; {$I+}
    Put := r;
  end;
begin
  {$ifdef UNIX}
  if fpAccess(pchar(Nm),W_OK)<>0 then begin FandReplaceFile := 5; exit end;
  if (fpLStat(pchar(Nm),st)=0) and (fpS_ISLNK(st.st_mode) or (st.st_nlink>1)
     or (st.st_uid<>fpGetEUid)) then begin FandReplaceFile := Put(Nm); exit end;
  {$endif}
  tmp := Nm+'.fand~';
  e := Put(tmp);
  if e=0 then begin
    {$ifdef UNIX} if fpStat(pchar(Nm),st)=0 then fpChmod(pchar(tmp),st.st_mode and $FFF); {$endif}
    if not RenameFile(tmp,Nm) then
      {$ifdef MSWINDOWS} if not (DeleteFile(Nm) and RenameFile(tmp,Nm)) then {$endif}
        e := 5 end;
  if e<>0 then DeleteFile(tmp);
  FandReplaceFile := e;
end;
function FandFileExists(const Nm:ansistring):boolean;
begin FandFileExists := FileExists(Nm) end;
function FandDirExists(const Nm:ansistring):boolean;
begin FandDirExists := DirectoryExists(Nm) end;
function FandRename(const Old,New:ansistring):boolean;
begin FandRename := RenameFile(Old,New) end;
function FandListFiles(const Pattern:ansistring; DirsOnly,FullPath:boolean):ansistring;
var act:integer;
begin
  gText := '';
  if FullPath then act := acListPath else act := acListName;
  ForEachFile(Pattern,DirsOnly,act);
  SortText;
  FandListFiles := gText; gText := '';
end;
function FandTotalSize(const Pattern:ansistring; var Total:int64;
                       var Newest:longint):boolean;
begin
  gFound := false; gTotal := 0; gNewest := 0;
  ForEachFile(Pattern,false,acSize);
  Total := gTotal; Newest := gNewest;
  FandTotalSize := gFound;
end;
procedure FandDeleteFiles(const Pattern:ansistring);
begin ForEachFile(Pattern,false,acDelete) end;
function FandSearchDown(const Dir,Mask:ansistring; Depth:integer):ansistring;
begin
  gText := '';
  SearchDown(Dir,Mask,Depth);
  SortText;
  FandSearchDown := gText; gText := '';
end;
function FandStamp(T:longint):ansistring;
begin FandStamp := StampOf(T) end;
const BatDepth:integer=0;
function RunBat(const path:ansistring; var av:TArgs; nA:integer):integer;
var
  src,ln,w,rest,lbl,a,b:ansistring;
  lines:array[1..400] of ansistring;
  n,i,p,k:integer;
  neg,cond,skipping:boolean;
  rc:integer;
  function Word1(var s:ansistring):ansistring;
  var q:integer;
  begin
    s := Trim2(s);
    q := pos(' ',s);
    if q=0 then begin Word1 := s; s := '' end
    else begin Word1 := copy(s,1,q-1); s := Trim2(copy(s,q+1,length(s))) end;
  end;
  function Subst(const s:ansistring):ansistring;
  var q:integer; r,rep_:ansistring;
  begin
    r := '';
    q := 1;
    while q<=length(s) do begin
      if (s[q]='%') and (q<length(s)) and (s[q+1] in ['1'..'9']) then begin
        k := ord(s[q+1])-ord('0');
        if k<=nA then rep_ := av[k] else rep_ := '';
        r := r + rep_; inc(q,2);
      end else begin r := r + s[q]; inc(q) end;
    end;
    Subst := r;
  end;
  function AnyMatch(const mask:ansistring):boolean;
  var sr:TSearchRec; dir,mk:ansistring; ok,every:boolean;
  begin
    AnyMatch := false;
    dir := DirPart(mask); mk := NamePart(mask);
    if mk='' then mk := '*';
    every := mk='*.*';
    if dir='' then dir := '.'+DirectorySeparator;
    ok := false;
    if SysUtils.FindFirst(dir+'*',faAnyFile,sr)=0 then
      repeat
        if (sr.Name='.') or (sr.Name='..') then continue;
        if every or MaskMatch(sr.Name,mk) then begin ok := true; break end;
      until SysUtils.FindNext(sr)<>0;
    SysUtils.FindClose(sr);
    AnyMatch := ok;
  end;
  procedure Exec(const cmd:ansistring);
  const Internal=' ECHO ECHO. REM CD CHDIR SET CLS PAUSE MD MKDIR RD RMDIR DEL ERASE'+
                 ' COPY REN RENAME MOVE TYPE VER PATH PROMPT MODE DIR ';
  var r:integer; c1,w1:ansistring; q:integer;
  begin
    if cmd='' then exit;
    c1 := cmd; w1 := UpStr(Word1(c1));
    q := pos('>',w1); if q>0 then w1 := copy(w1,1,q-1);
    if FandDosCmd(cmd,r) then begin
      { DOS's own commands leave ERRORLEVEL as the last program set it }
      if pos(' '+w1+' ',Internal)=0 then rc := r; exit end;
    if @FandDosShell<>nil then rc := FandDosShell(cmd);
  end;
  { cmd starts a batch file: its name completed with .BAT }
  function Batch(var cmd:ansistring):boolean;
  var r1,w1:ansistring;
  begin
    r1 := cmd; w1 := Word1(r1); Batch := true;
    if UpStr(ExtractFileExt(w1))='.BAT' then exit;
    if FileExists(DosPath(w1)+'.BAT') then begin cmd := w1+'.BAT '+r1; exit end;
    if FileExists(DosPath(w1)+'.bat') then begin cmd := w1+'.bat '+r1; exit end;
    Batch := false;
  end;
  { as DOS: CALL runs a batch and comes back, a batch named without CALL
    takes over - true: this one ends }
  function RunCmd(cmd:ansistring):boolean;
  var r1:ansistring;
  begin
    RunCmd := false; r1 := cmd;
    if UpStr(Word1(r1))='CALL' then begin Batch(r1); Exec(r1); exit end;
    RunCmd := Batch(cmd); Exec(cmd);
  end;
begin
  RunBat := 0; rc := 0;
  if BatDepth>=16 then begin RunBat := 8; exit end;     { CALLs without end }
  src := ReadWhole(path);
  if src='' then begin RunBat := 9; exit end;
  inc(BatDepth);
  n := 0; ln := '';
  for i := 1 to length(src) do
    if src[i]=#10 then begin
      if n<400 then begin inc(n); lines[n] := ln end; ln := '' end
    else if src[i]<>#13 then ln := ln + src[i];
  if (ln<>'') and (n<400) then begin inc(n); lines[n] := ln end;
  lbl := ''; i := 1;
  while i<=n do begin
    ln := Trim2(lines[i]); inc(i);
    while (ln<>'') and (ln[1]='@') do ln := Trim2(copy(ln,2,length(ln)));
    if ln='' then continue;
    if ln[1]=':' then begin
      if (lbl<>'') and (UpStr(Trim2(copy(ln,2,length(ln))))=lbl) then lbl := '';
      continue;
    end;
    skipping := lbl<>'';
    if skipping then continue;
    ln := Subst(ln);
    rest := ln;
    w := UpStr(Word1(rest));
    if (w='REM') or (w='PAUSE') or (w='CLS') then continue;
    if (w='ECHO') or (w='ECHO.') then begin          { to the screen: nothing }
      if pos('>',ln)>0 then Exec(ln); continue end;
    if w='GOTO' then begin
      lbl := UpStr(Word1(rest)); i := 1; continue;
    end;
    if w='IF' then begin
      neg := false;
      a := Word1(rest);
      if UpStr(a)='NOT' then begin neg := true; a := Word1(rest) end;
      if UpStr(a)='EXIST' then begin
        b := Word1(rest);
        cond := AnyMatch(DosPath(b));
      end else if UpStr(a)='ERRORLEVEL' then begin
        b := Word1(rest);
        cond := rc >= StrToIntDef(b,0);
      end else begin
        p := pos('==',a);
        if p=0 then continue;
        b := copy(a,p+2,length(a)); a := copy(a,1,p-1);
        cond := a=b;
      end;
      if neg then cond := not cond;
      if not cond then continue;
      w := UpStr(Word1(rest));
      if w='GOTO' then begin lbl := UpStr(Word1(rest)); i := 1; continue end;
      if w='' then continue;
      if RunCmd(w+' '+rest) then break;
      continue;
    end;
    if RunCmd(ln) then break;
  end;
  dec(BatDepth);
  RunBat := rc;
end;
function FandDosCmd(const Line:ansistring; var ExitCode:integer):boolean;
var
  s,tok,cmd,nm,ext,redir,dst:ansistring;
  av:TArgs;
  nArg,i,p:integer;
  app:boolean;
  function Arg(n:integer):ansistring;
  begin if (n>=1) and (n<=nArg) then Arg := av[n] else Arg := '' end;
begin
  FandDosCmd := false;
  ExitCode := 0;
  gText := '';
  s := Trim2(Line);
  if s='' then begin FandDosCmd := true; exit end;
  redir := ''; app := false;
  p := 0;
  for i := 1 to length(s) do if s[i]='>' then p := i;
  if p>0 then begin
    app := (p>1) and (s[p-1]='>');
    redir := Trim2(copy(s,p+1,length(s)));
    if app then s := Trim2(copy(s,1,p-2)) else s := Trim2(copy(s,1,p-1));
  end;
  nArg := 0; cmd := '';
  while s<>'' do begin
    p := pos(' ',s);
    if p=0 then begin tok := s; s := '' end
    else begin tok := copy(s,1,p-1); s := Trim2(copy(s,p+1,length(s))) end;
    if tok='' then continue;
    if cmd='' then cmd := tok
    else if nArg<16 then begin inc(nArg); av[nArg] := tok end;
  end;
  if cmd='' then begin FandDosCmd := true; exit end;
  if (length(cmd)>2) and (UpStr(copy(cmd,1,2))='CD') and (cmd[3] in ['.','\','/']) then begin
    for i := nArg downto 1 do if i<16 then av[i+1] := av[i];
    av[1] := copy(cmd,3,length(cmd)); if nArg<16 then inc(nArg); cmd := 'CD' end;
  nm := UpStr(NamePart(cmd));
  ext := ''; p := 0;
  for i := length(nm) downto 1 do if nm[i]='.' then begin p := i; break end;
  if p>0 then begin ext := copy(nm,p+1,length(nm)); nm := copy(nm,1,p-1) end;
  if ext='BAT' then begin
    s := DosPath(cmd);
    if not FileExists(s) then begin
      gText := '';
      ForEachFile(WithSlash(DirPart(s))+NamePart(s),false,acListPath);
      if gText<>'' then begin
        p := pos(CRLF,gText);
        if p>0 then s := copy(gText,1,p-1);
      end;
      gText := '';
    end;
    if not FileExists(s) then begin ExitCode := 9; FandDosCmd := true; exit end;
    ExitCode := RunBat(s,av,nArg);
    FandDosCmd := true;
    exit;
  end;
  if Arg(1)='$' then begin
    for i := 1 to nArg-1 do av[i] := av[i+1];
    dec(nArg);
  end;
  if ext<>'' then begin
    if @FandDosExtra<>nil then
      FandDosCmd := FandDosExtra(nm,ext,FandSlashed(DirPart(cmd)),av,nArg,ExitCode);
    exit;
  end;
  FandDosCmd := true;
  if nm='VER' then
    AddLine('MS-DOS Version 6.22')
  else if (nm='SET') or (nm='SETCFG') then begin
    for i := 1 to GetEnvironmentVariableCount do
      AddLine(GetEnvironmentString(i));
  end
  else if (nm='MD') or (nm='MKDIR') or (nm='MAKEDIR') then begin
    for i := 1 to nArg do
      if not ForceDirectories(DosPath(av[i])) then ExitCode := 1;
  end
  else if (nm='RD') or (nm='RMDIR') then
    for i := 1 to nArg do RemoveDir(DosPath(av[i]))
  else if (nm='DEL') or (nm='ERASE') then
    for i := 1 to nArg do ForEachFile(DosPath(av[i]),false,acDelete)
  else if nm='COPY' then begin
    { DOS switches (/Y /-Y /B /A /V ...) are not file names }
    i := 1; while i<=nArg do
      if (length(av[i])<=3) and (av[i][1]='/') and (UpCase(av[i][length(av[i])]) in ['A'..'Z']) then begin
        for p := i to nArg-1 do av[p] := av[p+1]; dec(nArg) end
      else inc(i);
    dst := DosPath(Arg(2));
    if (length(dst)>=4) and (copy(dst,length(dst)-3,2)=DirectorySeparator+'*') then
      dst := copy(dst,1,length(dst)-4);
    if (length(dst)>=2) and (copy(dst,length(dst)-1,2)=DirectorySeparator+'*') then
      dst := copy(dst,1,length(dst)-2);
    if DirectoryExists(dst) then begin
      gCount := 0; gDest := dst;
      ForEachFile(DosPath(Arg(1)),false,acCopy);
      if gCount=0 then ExitCode := 1;
    end else
      if not CopyOne(DosPath(Arg(1)),dst) then ExitCode := 1;
  end
  else if (nm='REN') or (nm='RENAME') or (nm='MOVE') then begin
    if not RenameFile(DosPath(Arg(1)),DosPath(Arg(2))) then ExitCode := 1;
  end
  else if nm='TYPE' then
    for i := 1 to nArg do gText := gText + ReadWhole(DosPath(av[i]))
  else if nm='ECHO' then begin
    s := '';
    for i := 1 to nArg do begin
      if s<>'' then s := s+' ';
      s := s+av[i];
    end;
    AddLine(s);
  end
  else if nm='CLS' then
    gText := #27'[2J'#27'[H'
  else if (nm='CD') or (nm='CHDIR') then begin
    { for the rest of this EXEC: OSshell puts FAND's directory back, as BP7 }
    if nArg=0 then AddLine(GetCurrentDir)
    else if not SetCurrentDir(DosPath(Arg(1))) then ExitCode := 1 end
  else if (length(nm)=2) and (nm[2]=':') then
    begin end                                     { a drive change }
  else if (nm='EXIT') or (nm='PAUSE') or
          (nm='REM') or (nm='PATH') or (nm='PROMPT') or (nm='MODE') or
          (nm='KEYB') or (nm='SHARE') or (nm='MOUNT') or (nm='DIR') then
    begin end
  else begin
    FandDosCmd := false;
    if @FandDosExtra<>nil then
      FandDosCmd := FandDosExtra(nm,ext,FandSlashed(DirPart(cmd)),av,nArg,ExitCode);
    exit;
  end;
  if redir<>'' then begin
    if not IsNul(redir) then
      if not WriteWhole(DosPath(redir),gText,app) then ExitCode := 1;
  end else
    if gText<>'' then write(gText);
end;
end.
