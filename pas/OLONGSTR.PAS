{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit olongstr;

interface

uses base,access;

function GetTxt(Z:FrmlPtr):LongStrPtr;
function CopyTFFromGetTxt(TF:TFilePtr;Z:FrmlPtr):longint;
function CopyTFString(TF:TFilePtr;FD2:FileDPtr;TF2:TFilePtr;Pos2:longint):longint;
procedure CopyTFStringToH(h:word);

implementation

uses oaccess,runfrml;

procedure GetTxtPrepare(Z:FrmlPtr;var h:word;var off,len:longint);
var l:longint;
begin
  off:=0; if Z^.P1<>nil then begin off:=RunInt(Z^.P1)-1; if off<0 then off:=0 end;
  SetTxtPathVol(Z^.TxtPath,Z^.TxtCatIRec); TestMountVol(CPath[1]);
  h:=OpenH(_isoldfile,RdOnly); if HandleError<>0 then begin
    if HandleError=2 then begin h:=$ff; len:=0; exit end; TestCPathError end;
  len:=FileSizeH(h); LastExitCode:=0; if off>=len then off:=len; dec(len,off);
  if Z^.P2<>nil then begin
    l:=RunInt(Z^.P2); if l<0 then l:=0; if l<len then len:=l; end;
  SeekH(h,off);
end;
function GetTxt(Z:FrmlPtr):LongStrPtr;
var h:word; len,off:longint; s:LongStrPtr;
begin
  GetTxtPrepare(Z,h,off,len);
  if len>MaxLStrLen then begin len:=MaxLStrLen; LastExitCode:=1 end;
  s:=GetStore(len+2); LastTxtPos:=off+len; if len>0 then ReadH(h,len,s^.A);
  s^.LL:=len; CloseH(h); GetTxt:=s;
end;
function CopyTFFromGetTxt(TF:TFilePtr;Z:FrmlPtr):longint;
var md:LockMode; len,off,pos,nxtpos:longint; h,n,l,i:word; rest:integer;
    X:array[0..MPageSize]of byte; ll:word absolute X; continued:boolean;
label 1,3,4;
begin
  GetTxtPrepare(Z,h,off,len); LastTxtPos:=off+len;
  if len=0 then begin CopyTFFromGetTxt:=0; CloseH(h); exit end;
  if not TF^.IsWork then md:=NewLMode(WrMode);
  if len<=MPageSize-2 then begin { short text }
    l:=word(len); ReadH(h,l,X); rest:=MPageSize-TF^.FreePart mod MPageSize;
    if l+2<=rest then pos:=TF^.FreePart
    else begin pos:=TF^.NewPage(false); TF^.FreePart:=pos; rest:=MPageSize end;
    if l+4>=rest then TF^.FreePart:=TF^.NewPage(false)
    else begin inc(TF^.FreePart,l+2); rest:=l+4-rest;
      RdWrCache(false,TF^.Handle,TF^.NotCached,TF^.FreePart,2,rest) end;
    RdWrCache(false,TF^.Handle,TF^.NotCached,pos,2,l);
    RdWrCache(false,TF^.Handle,TF^.NotCached,pos+2,l,X); CopyTFFromGetTxt:=pos; goto 4 end;
  pos:=TF^.NewPage(false); CopyTFFromGetTxt:=pos;
1:if len>MaxLStrLen then begin
    l:=MaxLStrLen; ll:=l+1; len:=len-l; continued:=true end
  else begin l:=word(len); ll:=l; len:=0; continued:=false end;
  i:=2;
3:if (l>MPageSize-i) or continued then begin
    n:=MPageSize-4-i; if n>l then n:=l; ReadH(h,n,X[i]); i:=0;
    nxtPos:=TF^.NewPage(false); LongintPtr(@X[MPageSize-4])^:=nxtPos;
    RdWrCache(false,TF^.Handle,TF^.NotCached,pos,MPageSize,X); pos:=nxtpos;
    dec(l,n); if continued and (l=0) then goto 1; goto 3 end;
  ReadH(h,l,X[i]); RdWrCache(false,TF^.Handle,TF^.NotCached,pos,MPageSize,X);
4:if not TF^.IsWork then OldLMode(md); CloseH(h);
end;

procedure CopyTFStringToH(h:word);
var cf:FileDPtr; tf:TFilePtr; i,l,n:word; isLongTxt:boolean; pos:longint;
    X:array[0..MPageSize]of byte; ll:word absolute X; md2:LockMode;
label 1,2,3,4;
begin
  pos:=TF02Pos; if pos=0 then exit; cf:=CFile;
  CFile:=TFD02; tf:=TF02; if not tf^.IsWork then md2:=NewLMode(RdMode);
  RdWrCache(true,tf^.Handle,tf^.NotCached,pos,2,l);
  if l<=MPageSize-2 then begin { short text }
    RdWrCache(true,tf^.Handle,tf^.NotCached,pos+2,l,X); WriteH(h,l,X);
    goto 4 end;
  if (pos mod MPageSize)<>0 then goto 2;
  RdWrCache(true,tf^.Handle,tf^.NotCached,pos,MPageSize,X);
1:if l>MaxLStrLen+1 then begin
2:  tf^.Err(889,false); goto 4 end;
  isLongTxt:=(l=MaxLStrLen+1); if isLongTxt then dec(l); i:=2;
3:if (l>MPageSize-i) or isLongTxt then begin
    n:=MPageSize-4-i; if n>l then n:=l; WriteH(h,n,X[i]);
    pos:=LongintPtr(@X[MPageSize-4])^;
    if (pos<MPageSize) or (pos+MPageSize>tf^.MLen) or (pos mod MPageSize<>0) then begin
      tf^.Err(888,false); goto 4 end;
    RdWrCache(true,tf^.Handle,tf^.NotCached,pos,MPageSize,X);
    if (l<=MPageSize-i) then begin l:=ll; goto 1 end;
    dec(l,n); i:=0; goto 3 end;
  WriteH(h,l,X[i]);
4:if not tf^.IsWork then OldLMode(md2); CFile:=cf;
end;

function CopyTFString(TF:TFilePtr;FD2:FileDPtr;TF2:TFilePtr;Pos2:longint):longint;
var cf:FileDPtr; l:word; rest:integer; isLongTxt,frst:boolean;
    pos,nxtpos:longint; md,md2:LockMode;
    X:array[0..MPageSize]of byte; ll:word absolute X;
label 0,1,2,3,4;
begin
  if Pos2=0 then
0:begin CopyTFString:=0; exit end; {Mark***}
  cf:=CFile; if not TF^.IsWork then md:=NewLMode(WrMode);
  CFile:=FD2; if not TF2^.IsWork then md2:=NewLMode(RdMode);
  RdWrCache(true,TF2^.Handle,TF2^.NotCached,Pos2,2,l);
  if l<=MPageSize-2 then begin { short text }
    if l=0 then goto 0;{Mark***}
    RdWrCache(true,TF2^.Handle,TF2^.NotCached,Pos2+2,l,X); CFile:=cf;
    rest:=MPageSize-TF^.FreePart mod MPageSize;
    if l+2<=rest then pos:=TF^.FreePart
    else begin pos:=TF^.NewPage(false); TF^.FreePart:=pos; rest:=MPageSize end;
    if l+4>=rest then TF^.FreePart:=TF^.NewPage(false)
    else begin inc(TF^.FreePart,l+2); rest:=l+4-rest;
      RdWrCache(false,TF^.Handle,TF^.NotCached,TF^.FreePart,2,rest) end;
    RdWrCache(false,TF^.Handle,TF^.NotCached,pos,2,l);
    RdWrCache(false,TF^.Handle,TF^.NotCached,pos+2,l,X); CopyTFString:=pos; goto 4 end;
  if (Pos2 mod MPageSize)<>0 then goto 2;
  RdWrCache(true,TF2^.Handle,TF2^.NotCached,Pos2,MPageSize,X); frst:=true;
1:if l>MaxLStrLen+1 then begin
2:  TF2^.Err(889,false); CopyTFString:=0; goto 4 end;
  isLongTxt:=(l=MaxLStrLen+1); if isLongTxt then dec(l); inc(l,2);
3:CFile:=cf;
  if frst then begin pos:=TF^.NewPage(false); CopyTFString:=pos; frst:=false end;
  if (l>MPageSize) or isLongTxt then begin
    Pos2:=LongintPtr(@X[MPageSize-4])^;
    nxtPos:=TF^.NewPage(false); LongintPtr(@X[MPageSize-4])^:=nxtPos;
    RdWrCache(false,TF^.Handle,TF^.NotCached,pos,MPageSize,X); pos:=nxtpos;
    CFile:=FD2;
    if (Pos2<MPageSize) or (Pos2+MPageSize>TF2^.MLen) or (Pos2 mod MPageSize<>0) then begin
      TF2^.Err(888,false); CopyTFString:=0; goto 4 end;
    RdWrCache(true,TF2^.Handle,TF2^.NotCached,Pos2,MPageSize,X);
    if (l<=MPageSize) then begin l:=ll; goto 1 end;
    dec(l,MPageSize-4); goto 3 end;
  RdWrCache(false,TF^.Handle,TF^.NotCached,pos,MPageSize,X);
4:CFile:=FD2; if not TF2^.IsWork then OldLMode(md2);
  CFile:=cf; if not TF^.IsWork then OldLMode(md);
end;

end.