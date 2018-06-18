uses dos;
function OpenH(path:pathstr):word;
var r:registers; p:pointer;
begin
  path:=path+#0; with r do begin
    AH:=$3D; DS:=seg(path); DX:=succ(ofs(path)); CX:=0; AL:=18;
    MSDos(r); if Flags and FCarry <> 0 then begin
      writeln('file doesn''t exist'); halt end;
    OpenH:=AX;
end end;

procedure CloseH(handle:word);
var r:registers;
begin with r do begin
  AH:=$3E; BX:=handle; MsDos(r);
end end;

procedure SeekH(handle:word;pos:longint);
var r:registers;
begin with r do begin
  AH:=$42; AL:=0; BX:=handle;
  CX:=memW[seg(pos):ofs(pos)+2]; DX:=memW[seg(pos):ofs(pos)];
  MsDos(r);
end end;

procedure ReadH(handle,bytes:word;var buffer);
var r:registers;
begin with r do begin
  AH:=$3F; DS:=seg(buffer); DX:=ofs(buffer); CX:=bytes; BX:=handle;
  MsDos(r);
end end;

procedure WriteH(handle,bytes:word;var buffer);
var r:registers;
begin with r do begin
  AH:=$40; DS:=seg(buffer); DX:=ofs(buffer); CX:=bytes; BX:=handle;
  MsDos(r);
end end;


const MPageShft=9;

var
  h:word; Size:longint; p:PathStr; d:DirStr; n:NameStr; e:ExtStr;
  T:record
    OldFreeRoot,OldMaxPage:word;
    FreePart:longint;
    Rsrvd1, CompileProc, CompileAll:boolean;
    IRec:word;
    FreeRoot, MaxPage:longint;   {Pw:array[1..40] of char;}
    Rsrvd2:array[1..32]of char;
    Version:array[1..4]of char;
    LicText:array[1..105] of byte; Sum:byte;
    X1:array[1..295] of char;
    LicNr:word;
    X2:array[1..11] of char;
    PwNew:array[1..40] of byte;
    Time:byte;
  end;
  FreePage:longint absolute T;

var Pw:array[1..40] of byte; Pw1:array[1..20] of char absolute Pw;
    ML:longint; i,j:word;
label 1;
begin
  FSplit(FExpand(paramstr(1)),d,n,e);
  if e='' then e:='.TTT' else if (length(e)<4) or (e[2]<>'T') then begin
    writeln('incorrect file name (.TTT or .Tnn expected)');halt end;
  h:=OpenH(d+n+e);
  SeekH(h,0); ReadH(h,512,T);
  if FreePage<>$FFFF0001 then begin
    if T.FreePart>=0 then exit; T.FreePart:=-T.FreePart;
    ML:=longint(T.OldMaxPage+1)shl MPageShft; goto 1 end;
  ML:=longint(T.MaxPage+1)shl MPageShft; RandSeed:=ML+T.Time;
  for i:=14 to 471 do j:=Random(255);
  for i:=1 to 40 do Pw[i]:=T.PwNew[i] xor Random(255);
  if Pw1='@@@@@@@@@@@@@@@@@@@@' then begin
    RandSeed:=ML; for i:=1 to 40 do Pw[i]:=Pw[i] xor Random(255);
    Move(Pw,T.FreeRoot,40); dec(T.IRec,$4000) end;
1:FreePage:=ML;
  SeekH(h,0); WriteH(h,512,T); SeekH(h,ML); WriteH(h,0,T);
  CloseH(h);
end.