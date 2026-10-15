{$I Switches}
{$A-,B-,F+,G-,I-,O+,R-,S+,V+,X+,L+}
unit runbatch;
{ fand TASK --source-in, --source-out; see HOWTO.txt }
interface
function FandBatchRun(const Task,Op,Arg:string):integer;
implementation
uses dos, sysutils, base, drivers, access, oaccess, obaseww, rdrun, runproj,
     fandcp, fanddos;
procedure Ovr; begin end;
var
  Opened:boolean;
  Aborted:boolean;
  Outcome:integer;
  Warnings:longint;
procedure Say(const Utf8:ansistring);
begin writeln(StdErr,'fand: ',Utf8) end;
procedure Warn(const Utf8:ansistring);
begin Say(Utf8); inc(Warnings) end;
function U(const S:ansistring):ansistring;
begin U:=S852ToUtf8(S) end;
function TrimRightCh(const S:ansistring; C:char):ansistring;
var n:longint;
begin n:=length(S); while (n>0) and (S[n]=C) do dec(n); TrimRightCh:=copy(S,1,n) end;
function IStr(N:int64):ansistring;
var s:ansistring;
begin str(N,s); IStr:=s end;
function LongToAnsi(X:LongStrPtr):ansistring;
var s:ansistring;
begin SetLength(s,X^.LL); if X^.LL>0 then move(X^.A,s[1],X^.LL); LongToAnsi:=s end;
function AnsiToLong(const S:ansistring):LongStrPtr;
var x:LongStrPtr; n:longint;
begin
  n:=length(S); if n>$FFF0 then n:=$FFF0;
  x:=GetStore(n+2); x^.LL:=n; if n>0 then move(S[1],x^.A,n); AnsiToLong:=x;
end;
procedure ReportCompileErr;
var s,t,r:ansistring; line,col,i:longint; x:LongStrPtr; p:pointer; cf:FileDPtr; cr:pointer;
begin
  s:=MsgLine;
  if (InpRdbPos.R=CRdb) and (InpRdbPos.IRec>0) and not CRdb^.Encrypted then begin
    cf:=CFile; cr:=CRecPtr; MarkStore(p);
    CFile:=Chpt; CRecPtr:=GetRecSpace; ReadRec(InpRdbPos.IRec);
    x:=_LongS(ChptTxt); t:=LongToAnsi(x);
    line:=1; col:=1;
    for i:=1 to CurrPos-1 do
      if i>length(t) then break
      else if t[i]=#13 then begin inc(line); col:=1 end
      else if t[i]<>#10 then inc(col);
    if CRdb^.ChainBack<>nil then r:=U(CRdb^.FD^.Name)+': ' else r:='';
    Warn(r+'chapter '+IStr(InpRdbPos.IRec)+' '+TrimRightCh(_ShortS(ChptTyp),' ')+' '+
         U(TrimRightCh(_ShortS(ChptName),' '))+', line '+IStr(line)+' column '+IStr(col)+
         ': '+U(s));
    ReleaseStore(p); CFile:=cf; CRecPtr:=cr end
  else Warn(U(s));
end;
function Compiled:boolean;
begin
  CompileFD:=true;
  if CompileRdb(false,true,false) then Compiled:=true
  else begin Compiled:=false; ReportCompileErr end;
end;
function SplitSourceName(const F:ansistring; var Typ:char; var Name:ansistring):boolean;
var b:ansistring; i:longint;
begin
  SplitSourceName:=false;
  if not SameText(ExtractFileExt(F),'.txt') then exit;
  b:=ChangeFileExt(ExtractFileName(F),'');
  i:=1; while (i<=length(b)) and (b[i] in ['0'..'9']) do inc(i);
  if (i=1) or (i+2>length(b)) or (b[i]<>'-') or (b[i+2]<>'-') then exit;
  Typ:=b[i+1]; Name:=copy(b,i+3,maxlongint);
  SplitSourceName:=true;
end;
function SourceToFand(const S:ansistring):ansistring;
var t,r:ansistring; i:longint;
begin
  t:=S; if copy(t,1,3)=#$EF#$BB#$BF then delete(t,1,3);
  t:=SUtf8To852(t); r:='';
  for i:=1 to length(t) do
    if t[i]=#10 then begin
      if (i=1) or (t[i-1]<>#13) then r:=r+#13; r:=r+#10 end
    else r:=r+t[i];
  SourceToFand:=r;
end;
function FandToSource(const S:ansistring):ansistring;
var r:ansistring; i:longint;
begin
  r:='';
  for i:=1 to length(S) do
    if (S[i]=#13) and (i<length(S)) and (S[i+1]=#10) then
    else if S[i]=#13 then r:=r+#10
    else r:=r+S[i];
  FandToSource:=U(r);
end;
procedure SourceIn(const Task,Dir:string);
var sr:TSearchRec; names,texts,oldNm,oldDcl:array of ansistring; i,j:longint; t:ansistring;
    typ:char; nm:ansistring; x:LongStrPtr; p:pointer; md:LockMode;
begin
  names:=nil;
  if SysUtils.FindFirst(IncludeTrailingPathDelimiter(Dir)+'*',faAnyFile,sr)=0 then begin
    repeat
      if SplitSourceName(sr.Name,typ,nm) then begin
        SetLength(names,length(names)+1); names[high(names)]:=sr.Name end;
    until SysUtils.FindNext(sr)<>0;
    SysUtils.FindClose(sr) end;
  if length(names)=0 then begin
    Say('no chapter files (NNN-T-Name.txt) in '+Dir); exit end;
  for i:=0 to high(names)-1 do for j:=i+1 to high(names) do
    if names[j]<names[i] then begin t:=names[i]; names[i]:=names[j]; names[j]:=t end;
  SetLength(texts,length(names));             { all of them before the RDB is emptied }
  for i:=0 to high(names) do
    if not FandReadFile(IncludeTrailingPathDelimiter(Dir)+names[i],texts[i]) then begin
      Say('cannot read '+names[i]+'; nothing stored'); exit end;
  IsTestRun:=true;
  CreateOpenChpt(Task,true); Opened:=true;
  if CRdb^.Encrypted then begin
    Say(Task+': the chapters are password-protected; not replaced'); exit end;
  CFile:=Chpt; CRecPtr:=Chpt^.RecPtr;
  { the F chapters' declarations now: the compile converts their data
    files to the new ones (MergeOldNew) }
  oldNm:=nil; oldDcl:=nil;
  for i:=1 to Chpt^.NRecs do begin
    ReadRec(i);
    if TrimRightCh(_ShortS(ChptTyp),' ')='F' then begin
      MarkStore(p); SetLength(oldNm,length(oldNm)+1); SetLength(oldDcl,length(oldDcl)+1);
      oldNm[high(oldNm)]:=UpperCase(TrimRightCh(_ShortS(ChptName),' '));
      oldDcl[high(oldDcl)]:=LongToAnsi(_LongS(ChptTxt)); ReleaseStore(p) end end;
  md:=RewriteF(false);
  for i:=0 to high(names) do begin
    SplitSourceName(names[i],typ,nm);
    if length(nm)>12 then Warn(names[i]+': the name is longer than 12 characters');
    t:=SourceToFand(texts[i]);
    if length(t)>$FFF0 then Warn(names[i]+': longer than a chapter can be, cut');
    MarkStore(p);
    ZeroAllFlds; S_(ChptTyp,typ); S_(ChptName,SUtf8To852(nm)); B_(ChptVerif,false);
    x:=AnsiToLong(t); StoreChptTxt(ChptTxt,x,false);
    if typ='F' then
      for j:=0 to high(oldNm) do if oldNm[j]=UpperCase(SUtf8To852(nm)) then begin
        SetOldDcl(AnsiToLong(oldDcl[j])); break end;
    PutRec;
    ReleaseStore(p);
  end;
  OldLMode(md); SetCompileAll; SaveFiles;
  Say(Task+': '+IStr(length(names))+' chapters from '+Dir);
  if Compiled then Outcome:=0
  else Say('the task does not compile (the chapters are stored)');
end;
procedure SourceOut(const Task,Dir:string);
var i:longint; typ,nm,f:ansistring; x:LongStrPtr; p:pointer; c:longint;
begin
  CreateOpenChpt(Task,false); Opened:=true;
  if CRdb^.Encrypted then begin
    Say(Task+': the chapters are password-protected; not written'); exit end;
  if not ForceDirectories(Dir) then begin Say('cannot create '+Dir); exit end;
  CFile:=Chpt; CRecPtr:=Chpt^.RecPtr; c:=0;
  for i:=1 to Chpt^.NRecs do begin
    MarkStore(p); ReadRec(i);
    typ:=TrimRightCh(_ShortS(ChptTyp),' '); if typ='' then typ:='_';
    nm:=U(TrimRightCh(_ShortS(ChptName),' '));
    f:=Format('%.3d-%s-%s.txt',[i,typ,nm]);
    f:=StringReplace(StringReplace(f,'/','_',[rfReplaceAll]),'\','_',[rfReplaceAll]);
    x:=_LongS(ChptTxt);
    if not FandWriteWhole(IncludeTrailingPathDelimiter(Dir)+f,FandToSource(LongToAnsi(x)),false) then
      Warn('cannot write '+f) else inc(c);
    ReleaseStore(p) end;
  Say(Task+': '+IStr(c)+' chapters to '+Dir);
  if Warnings=0 then Outcome:=0 else Outcome:=2;
end;
procedure Usage;
begin
  Say('usage: fand TASK --source-in DIR | --source-out DIR');
end;
function FandBatchRun(const Task,Op,Arg:string):integer;
var er:ExitRecord;
label 9;
begin
  Outcome:=1; Warnings:=0; Opened:=false; Aborted:=false; BatchMsgCount:=0;
  BatchQuiet:=true;
  if (Task='--help') or (Arg='') then begin Usage; FandBatchRun:=2; exit end;
  Move(ExitBuf,er,sizeof(ExitRecord)); ExitBuf.OvrEx:=Ovr; ExitBuf.ExP:=ExitP; ExitBuf.BrkP:=BreakP; ExitBuf.mBP:=MyBP; if SetJmp(ExitBuf.JmpBuf)<>0 then begin GoExitFired:=true; Aborted:=true; goto 9 end else GoExitFired:=false;
  if Op='--source-in' then SourceIn(Task,Arg)
  else if Op='--source-out' then SourceOut(Task,Arg)
  else begin Usage; Outcome:=2 end;
9:RestoreExit(er);
  if Aborted then Outcome:=1;
  if Opened then begin
    Opened:=false;
    Move(ExitBuf,er,sizeof(ExitRecord)); ExitBuf.OvrEx:=Ovr; ExitBuf.ExP:=ExitP; ExitBuf.BrkP:=BreakP; ExitBuf.mBP:=MyBP; if SetJmp(ExitBuf.JmpBuf)<>0 then begin GoExitFired:=true; Outcome:=1; RestoreExit(er) end else begin GoExitFired:=false;
    CloseChpt; RestoreExit(er) end end;
  if (Outcome=0) and (BatchMsgCount>0) then Outcome:=2;
  FandBatchRun:=Outcome;
end;
end.
