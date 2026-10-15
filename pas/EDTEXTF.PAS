function RdNextPart:boolean;
  var ppa:CharArrPtr; L11,L1,iL,MI:word;
      BL,FSize,Rest,Max,Pos:longint;
      Pass:word;
  label 1;
  procedure LastLine(from,num:word;var Ind,Count:word);
    var C:^char; COfs:word absolute C;
        i:word;
    begin Count:=0;Ind:=from;C:=@T^[from];
      for i:=1 to num do
        begin {$ifdef FPC}inc(C){$else}inc(COfs){$endif};if C^=_CR then begin inc(Count);Ind:=from+i end end;
      if (Count>0)and(T^[Ind+1]=_LF) then inc(Ind);
    end;
  begin      {*****NextPart****}
    Max:=MinL(MaxLenT,StoreAvail+LenT);Pass:=Max-(Max shr 3);
{    Pass:=MaxLenT-(MaxLenT shr 3);}
    Part.MovL:=0;Part.MovI:=0;MI:=0;RdNextPart:=false;
    if AllRd then exit;
    if Part.LenP=0 then begin LenT:=0;T:=GetStore(0) end;
    with Part do
      begin Pos:=PosP;BL:=LineP;
        if LenT<=(Pass shr 1) then goto 1;
        LastLine(0,LenT-(Pass shr 1),MI,MovL);     { 28kB+1 radek}
        SetColorOrd(ColorP,1,MI+1);
        inc(Pos,MI);dec(LenT,MI);
        move(T^[MI+1],T^[1],LenT);ReleaseStore(@T^[LenT+1]);
      end;
    with Part do begin LineP:=BL+MovL;PosP:=Pos;MovI:=MI end;
  1:inc(Pos,LenT);
    FSize:=FileSizeH(TxtFh); AllRd:=false;iL:=LenT;
    repeat Rest:=FSize-Pos;
      if Rest>$1000 then L11:=$1000 else L11:=Rest;
      Max:=StoreAvail;if Max>$400 then dec(Max,$400);
      if L11>Max then L11:=Max;
      ppa:=GetStore(L11); L1:=L11;
      if L1>0 then
        begin SeekH(TxtFh,Pos);ReadH(TxtFh,L1,ppa^);
{          if ppa^[L1]=^z then
            begin ReleaseStore(@ppa^[L1]);dec(L1) end} end;
      AllRd:=Pos+L11>=FSize;
      inc(LenT,L1);inc(Pos,L1);
    until (LenT>Pass) or AllRd or (L11=Max);
    LastLine(iL,LenT-iL-1,iL,L1);
    if AllRd then iL:=LenT;
    if (iL<LenT) then begin LenT:=iL;AllRd:=false end;
    with Part do begin LenP:=LenT;UpdP:=false end;
    if (T^[LenT]=^z)and AllRd then dec(LenT);
    if (LenT<=1) then exit;  {????????}
    ReleaseStore(@T^[succ(LenT)]);
    RdNextPart:=true;
  end;


function RdPredPart:boolean;
  var ppa:CharArrPtr; L1,L11,MI:word;
      BL,FSize,Rest,Max,Pos:longint;
      Pass:word;
  label 1;
  procedure FirstLine(from,num:word;var Ind,Count:word);
    var C:^char; COfs:word absolute C;
        i:word;
    begin Count:=0;Ind:=from-1;C:=@T^[from];
      for i:=0 to num-1 do
        begin {$ifdef FPC}dec(C){$else}dec(COfs){$endif};if C^=_CR then begin inc(Count);Ind:=from-i end end;
      if (Count>0)and(T^[Ind+1]=_LF) then inc(Ind);
    end;
  begin      {*****PredPart****}
    Max:=MinL(MaxLenT,StoreAvail+LenT);Pass:=Max-(Max shr 3);
{    Pass:=MaxLenT-(MaxLenT shr 3);}
    Part.MovL:=0;MI:=0;RdPredPart:=false;
    if Part.PosP=0 then exit;
    with Part do
      begin Pos:=PosP;BL:=LineP;
        if LenT<=(Pass shr 1) then goto 1;
        FirstLine(LenT+1,LenT-(Pass shr 1),L1,L11);
        if L1<LenT then
          begin AllRd:=false;LenT:=L1;ReleaseStore(@T^[LenT+1]) end;
      end;
  1:L11:=LenT;
    repeat
      if Pos>$1000 then L1:=$1000 else L1:=Pos;
      Max:=StoreAvail;if Max>$400 then dec(Max,$400);
      if L1>Max then L1:=Max;ppa:=GetStore(L1); move(T^,T^[L1+1],LenT);
      if L1>0 then
        begin SeekH(TxtFh,Pos-L1);ReadH(TxtFh,L1,T^) end;
      inc(LenT,L1);dec(Pos,L1);
    until (LenT>Pass)or(Pos=0)or(L1=Max);
    L11:=LenT-L11;FirstLine(L11+1,L11,MI,Part.MovL);
    if Pos=0 then MI:=L11
    else if Part.MovL>0 then begin dec(Part.MovL);MI:=L11-MI end;
    L1:=L11-MI;dec(LenT,L1);inc(Pos,L1);
    if L1>0 then
      begin move(T^[L1+1],T^,LenT);
        ReleaseStore(@T^[LenT+1]) end;
    with Part do
      begin PosP:=Pos;LineP:=BL-MovL;LenP:=LenT;MovI:=MI;UpdP:=false;
        SetColorOrd(ColorP,1,MI+1) end;
    if (LenT=0) then exit;  {????????}
    RdPredPart:=true;
  end;

procedure UpdateFile;
  var n,HErr:word; d,pos1,pos:longint;  p:pointer;
  begin
    n:=$FFF0; if n>StoreAvail then n:=StoreAvail; p:=GetStore(n);
    d:=longint(LenT)-Part.LenP; HErr:=0;
    pos1:=Part.PosP+Part.LenP;
    if d>0 then
      begin pos:=AbsLenT;
        while pos>pos1 do begin if pos-pos1<n then n:=pos-pos1;dec(pos,n);
                          SeekH(TxtFh,pos);ReadH(TxtFh,n,p^);
                          SeekH(TxtFh,pos+d);WriteH(TxtFh,n,p^);
                          if HandleError<>0 then HErr:=HandleError end;
      end
    else if d<0 then
      begin pos:=pos1;
        while pos<AbsLenT do
          begin if pos+n>AbsLenT then n:=AbsLenT-pos;
            SeekH(TxtFh,pos);ReadH(TxtFh,n,p^);
            SeekH(TxtFh,pos+d);WriteH(TxtFh,n,p^);inc(pos,n) end;
        TruncH(TxtFh,AbsLenT+d)
      end;
    ReleaseStore(p);
    SeekH(TxtFh,Part.PosP);if LenT>0 then WriteH(TxtFh,LenT,T^);
    if HandleError<>0 then HErr:=HandleError;
    FlushH(TxtFh); AbsLenT:=FileSizeH(TxtFh);
    if HErr<>0 then begin SetMsgPar(TxtPath);WrLLF10Msg(700+HErr) end;
    with Part do begin UpdP:=false;LenP:=LenT;
                   if (PosP<400)and(LenP>$400) then UpdPHead:=false end;
  end;

procedure RdPart;
  begin LenT:=Part.LenP;T:=GetStore(LenT);if LenT=0 then exit;
    SeekH(TxtFh,Part.PosP);ReadH(TxtFh,LenT,T^) end;
procedure NullChangePart;
  begin ChangePart:=false;Part.MovI:=0;Part.MovL:=0 end;
procedure RdFirstPart;
  begin NullChangePart;
    with Part do begin PosP:=0;LineP:=0;LenP:=0;ColorP:='' end;
    AllRd:=false;ChangePart:=RdNextPart;
  end;

{$ifdef FPC}
{ A text with Unix line ends, in UTF-8 or with a UTF-8 BOM is edited as a
  CR LF, CP852 work copy - but only when converting the copy back gives the
  file's own bytes; anything else (CR LF mixed in, characters CP852 lacks)
  is edited as it is. The copy goes back into the file on F9, F6, an exit
  procedure and closing; one that cannot go back is kept beside FANDWORK. }
var TxtWork,TxtKeep,TxtView,TxtUtf8,TxtLF,TxtBom,TxtUnsaved:boolean;
    TxtOrig,TxtWorkFor,TxtNew:ansistring;
const Utf8Bom=#$EF#$BB#$BF;
function Utf8Valid(const S:ansistring; var Multi:boolean):boolean;
var i,n:longint; c:byte;
begin
  Utf8Valid:=false; Multi:=false; i:=1;
  while i<=length(S) do begin
    c:=ord(S[i]);
    if c<$80 then n:=0 else if c and $E0=$C0 then n:=1
    else if c and $F0=$E0 then n:=2 else if c and $F8=$F0 then n:=3 else exit;
    if i+n>length(S) then exit;
    while n>0 do begin inc(i); if ord(S[i]) and $C0<>$80 then exit; dec(n); Multi:=true end;
    inc(i) end;
  Utf8Valid:=true;
end;
function LfToCrLf(const S:ansistring):ansistring;
var r:ansistring; i,j:longint;
begin
  j:=0; for i:=1 to length(S) do if S[i]=#10 then inc(j);
  SetLength(r,length(S)+j); j:=0;
  for i:=1 to length(S) do begin
    if S[i]=#10 then begin inc(j); r[j]:=#13 end;
    inc(j); r[j]:=S[i] end;
  LfToCrLf:=r;
end;
function CrLfToLf(const S:ansistring):ansistring;
var r:ansistring; i,j:longint;
begin
  SetLength(r,length(S)); j:=0;
  for i:=1 to length(S) do
    if (S[i]<>#13) or (i=length(S)) or (S[i+1]<>#10) then begin inc(j); r[j]:=S[i] end;
  SetLength(r,j); CrLfToLf:=r;
end;
function ToWork(S:ansistring):ansistring;
begin
  if TxtBom then delete(S,1,3);
  if TxtUtf8 then S:=SUtf8To852(S);
  if TxtLF then S:=LfToCrLf(S);
  ToWork:=S;
end;
function FromWork(S:ansistring):ansistring;
begin
  if TxtLF then S:=CrLfToLf(S);
  if TxtUtf8 then S:=S852ToUtf8(S);
  if TxtBom then S:=Utf8Bom+S;
  FromWork:=S;
end;
function WorkTxtPath:string;
begin WorkTxtPath:=copy(FandWorkName,1,length(FandWorkName)-3)+'E$$' end;
function CanWrite(const Nm:ansistring):boolean;
var f:file; fm:byte;
begin
  fm:=FileMode; FileMode:=2;
  {$I-} assign(f,Nm); reset(f,1); FileMode:=fm;
  CanWrite:=IOResult=0; close(f); if IOResult<>0 then ; {$I+}
end;
procedure PrepareTxt(Mode:char);
var s,w:ansistring; multi:boolean;
begin
  if TxtKeep and TxtWork and (TxtWorkFor=CPath) then      { kept over F6 }
    begin TxtKeep:=false; CPath:=WorkTxtPath; exit end;
  TxtKeep:=false; TxtWork:=false;
  if not FandReadFile(UnixPath(CPath),TxtOrig) or (TxtOrig='') then exit;
  s:=TxtOrig; TxtBom:=copy(s,1,3)=Utf8Bom; if TxtBom then delete(s,1,3);
  TxtLF:=(pos(#10,s)>0) and (pos(#13,s)=0);
  { an ASCII text with Unix line ends is taken for UTF-8 as well }
  TxtUtf8:=Utf8Valid(s,multi) and (multi or TxtBom or TxtLF);
  if not (TxtBom or TxtUtf8 or TxtLF) then exit;
  w:=ToWork(TxtOrig); if FromWork(w)<>TxtOrig then exit;
  { a file FAND may not write opens as it is, and fails as in BP7 }
  TxtView:=Mode=ViewM; if not TxtView and not CanWrite(UnixPath(CPath)) then exit;
  if not FandWriteWhole(WorkTxtPath,w,false) then exit;
  TxtWork:=true; TxtWorkFor:=CPath; CPath:=WorkTxtPath;
end;
{ the work copy, read through the editor's handle, back into the file }
function SaveTxtBack:boolean;
var w,s:ansistring; n,i:longint; k:word; e:integer;
begin
  SaveTxtBack:=true; TxtUnsaved:=false; if not TxtWork or TxtView then exit;
  n:=FileSizeH(TxtFh); SetLength(w,n); i:=0; SeekH(TxtFh,0);
  while (i<n) and (HandleError=0) do begin
    k:=MinL(n-i,$8000); ReadH(TxtFh,k,w[i+1]); inc(i,k) end;
  if HandleError<>0 then e:=HandleError
  else begin
    s:=FromWork(w); if s=TxtOrig then exit;
    e:=FandReplaceFile(UnixPath(TxtPath),s);
    if e=0 then TxtOrig:=s else begin TxtNew:=s; TxtUnsaved:=true end end;
  if e<>0 then begin
    SaveTxtBack:=false; SetMsgPar(TxtPath); WrLLF10Msg(700+e) end;
end;
procedure CloseTxtFh;
var ok:boolean; i:integer; s:string;
begin
  ok:=SaveTxtBack; CloseH(TxtFh);
  if not TxtWork or TxtKeep then exit; TxtWork:=false;
  if ok then begin DeleteFile(WorkTxtPath); exit end;
  { not lost: FANDWORK.E00.. keeps it as the file would be, and the message
    says where }
  i:=0;
  repeat s:=copy(WorkTxtPath,1,length(WorkTxtPath)-2)+chr(48+i div 10)+chr(48+i mod 10);
    inc(i) until (i=100) or not FandFileExists(UnixPath(s));
  if FandFileExists(UnixPath(s)) then s:=WorkTxtPath
  else if TxtUnsaved and FandWriteWhole(UnixPath(s),TxtNew,false) then DeleteFile(WorkTxtPath)
  else if not FandRename(UnixPath(WorkTxtPath),UnixPath(s)) then s:=WorkTxtPath;
  TxtNew:=''; Set2MsgPar(FExpand(s),TxtPath); WrLLF10Msg(829);
end;
{$else}
procedure CloseTxtFh; begin CloseH(TxtFh) end;
{$endif}
procedure OpenTxtFh(Mode:char);
  var UM:FileUseMode;
  begin
    CPath:=TxtPath; CVol:=TxtVol;TestMountVol(CPath[1]);
    {$ifdef FPC}PrepareTxt(Mode);{$endif}
    if Mode=ViewM then UM:=RdOnly else UM:=Exclusive;
    TxtFH:=OpenH(_isoldnewfile,UM);
    if HandleError<>0 then begin
      SetMsgPar(CPath);RunError(700+HandleError) end;
    AbsLenT:=FileSizeH(TxtFH);
  end;
