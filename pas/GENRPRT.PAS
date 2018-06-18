{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit genrprt;

interface

uses dos,
     base,drivers,access,wwmenu,wwmix,
     rdrun,compile,
     rdrprt,runrprt;

function SelForAutoRprt(RO:RprtOptPtr):boolean;
procedure RunAutoReport(RO:RprtOptPtr);
function  SelGenRprt(const RprtName:string):LongStrPtr;

implementation

uses runfrml;

type
  PFldDPtr=^PFldD;
  PFldD=record
    Chain:PFldDPtr; FldD:FieldDPtr;
    ColTxt,ColItem:integer; IsCtrl,IsSum,NxtLine:boolean; Level:byte end;

var PFldDs: PFldDPtr;
    KpLetter: boolean;
    MaxCol,MaxColOld,MaxColUsed,NLines,NLevels: integer;
    Mode: AutoRprtMode;
    Txt: LongStrPtr;

procedure SubstChar(S:string;C1,C2:char); assembler;
asm  les di,S; xor ch,ch; mov cl,es:[di]; jcxz @3; inc di;
     mov al,C1; mov ah,C2; cld;
@1:  cmp al,es:[di]; jne @2; mov es:[di],ah;
@2:  inc di; loop @1;
@3:  end;

procedure Design(RO:RprtOptPtr);
var
  L,L2,LTxt,LItem,Col:integer; D,D1:PFldDPtr; F:FieldDPtr;
  WasTT,LastTT,First,frstOnLine:boolean;
label 1;
begin
  MaxCol:=RO^.Width; MaxColOld:=MaxCol; First:=true;
  case RO^.Style of
   'C':KpLetter:=true;
   '?':begin KpLetter:=true; MaxCol:=trunc(MaxCol/0.6) end end;
1:NLines:=1; Col:=1; WasTT:=false; LastTT:=false; D:=PFldDs; frstOnLine:=true;
  while D<>nil do begin
    F:=D^.FldD; LTxt:=Length(F^.Name); LItem:=F^.L; L:=maxi(LTxt,LItem);
    if D^.IsSum then L2:=2 else L2:=0;
    D^.NxtLine:=false;
    if LastTT or (F^.Typ='T') or not frstOnLine and (Col+L2+L>MaxCol+1) then begin
      D^.NxtLine:=True; inc(NLines); Col:=1; D1:=D end;
    frstOnLine:=false;
    Col:=Col+L2;
    D^.ColItem:=Col+(L-LItem+1)div 2;
    if (F^.Typ in ['A','N']) and (F^.M=LeftJust) then D^.ColTxt:=Col
    else D^.ColTxt:=Col+L-LTxt;
    if F^.Typ='T' then begin
      D^.ColItem:=1; D^.ColTxt:=1; WasTT:=true; LastTT:=true end
    else LastTT:=false; inc(Col,L+1); D:=D^.Chain end;
  if NLines>1 then begin
    if First and (RO^.Style='?') then begin
      KpLetter:=false; MaxCol:=RO^.Width; First:=false; goto 1 end;
    MaxColUsed:=MaxCol; L:=MaxCol+1-Col; if L>0 then D:=D1;
  if not WasTT then while D<>nil do begin
    D^.ColTxt:=D^.ColTxt+L; D^.ColItem:=D^.ColItem+L; D:=D^.Chain end end
  else begin MaxColUsed:=Col;
    if (MaxColUsed<=RO^.Width) and (RO^.Style='?') then begin
      MaxCol:=RO^.Width;KpLetter:=false end end;
end;


procedure WrChar(C:char);
var p:^char;
begin
  p:=GetStore(1); p^:=C; inc(Txt^.LL)
end;
procedure WrBlks(N:integer);
var p:pointer;
begin
  if N<=0 then exit;
  p:=GetStore(N); fillchar(p^,N,' '); inc(Txt^.LL,N);
end;
procedure WrStr(const S:string);
var p:pointer;
begin
  p:=GetStore(length(S)); move(S[1],p^,length(S)); inc(Txt^.LL,length(S));
end;
procedure WrLevel(Level:integer);
var first:boolean; d:PFldDPtr; f:FieldDPtr; s:string[50]; col,i,l,n,m:integer;
    b:boolean;             x:string;
begin
  b:=(Level=0) and (Mode=_AErrRecs);
  if b then WrStr('(warning) begin noErrRecs+=1 end,');
  first:=true; d:=PFldDs; while d<>nil do begin
    if (Level=0) or d^.IsSum  or d^.IsCtrl and (d^.Level>=Level) then begin
      if not first then WrChar(',');
      f:=d^.FldD; s:=f^.Name; if (Level<>0) and d^.IsSum then s:='sum('+s+')';
      if f^.Typ='D' then begin
        WrStr('strdate('); WrStr(s); WrStr(',''');
        x:=FieldDMask(f)^; SubstChar(x,'''','"'); WrStr(x); WrStr(''')') end
      else WrStr(s);
      first:=false end;
    d:=d^.Chain end;
  if b then begin if not first then WrChar(',');
    WrStr('errortext+cond(^error:'' ??'')') end;
  WrStr(';'^m^j); col:=1; if CFile^.Typ='0'{RDB} then WrChar(^q);
  d:=PFldDs; while d<>nil do begin
    if (CFile^.Typ='0') and (d^.Chain=nil) then WrChar(^q);
    if d^.NxtLine then begin WrStr(^m^j); col:=1 end;
    f:=d^.FldD; l:=f^.L; n:=d^.ColItem-col; col:=d^.ColItem+l;
    if (Level=0) or d^.IsSum or d^.IsCtrl and (d^.Level>=Level) then begin
      if (Level<>0) and d^.IsSum then begin dec(n,2); inc(l,2) end;
      WrBlks(n); if f^.Typ in ['F','R'] then begin
        m:=f^.M; if m<>0 then begin
          for i:=1 to l-m-1 do WrChar('_'); l:=m;
          if f^.Flg and f_Comma<>0 then WrChar(',') else WrChar('.') end end;
      for i:=1 to l do WrChar('_') end
    else WrBlks(n+l);
    d:=d^.Chain end;
  if Level>0 then begin
    WrBlks(MaxColUsed-col+1); for I:=1 to Level do WrChar('*') end;
  if b then begin
    WrStr(^m^j^w); WrBlks(5); WrStr('_'^w) end;
  if (Mode<>_AErrRecs) and (NLines>1) then WrStr(^m^j);
end;

function GenAutoRprt(RO:RprtOptPtr;WithNRecs:boolean):LongStrPtr;
var d:PFldDPtr; f:FieldDPtr; fl,fl1:FieldList; kf:KeyFldDPtr;
    i,l,col:integer; p:Pchar; first,point:boolean; s:string;
begin
  CFile:=RO^.FDL.FD; Mode:=RO^.Mode;
  NLevels:=ListLength(RO^.Ctrl); PFldDs:=nil;
  fl:=RO^.Flds; while fl<>nil do begin
    d:=GetZStore(sizeof(PFldD)); f:=fl^.FldD; d^.FldD:=f;
    d^.IsSum:=FieldInList(f,RO^.Sum);
    fl1:=RO^.Ctrl; i:=NLevels; while fl1<>nil do begin
      if fl1^.FldD=f then begin d^.IsCtrl:=true; d^.Level:=i end;
      dec(i); fl1:=fl1^.Chain end;
    if (Mode=_ATotal) and not d^.IsSum and not d^.IsCtrl then ReleaseStore(d)
    else ChainLast(PFldDs,d);
    fl:=fl^.Chain end;
  Design(RO);

  Txt:=GetZStore(2);

  if (Mode=_AErrRecs) then WrStr('var noErrRecs:real;'^m^j);
  WrStr('#I1_'); WrStr(CFile^.Name);
  if RO^.SK<>nil then WrChar('!');
  WrBlks(2);
  first:=true; fl:=RO^.Ctrl; kf:=RO^.SK; while fl<>nil do begin
    if not first then WrChar(','); f:=fl^.FldD;
    if (kf<>nil) and (f=kf^.FldD) then begin
      if kf^.Descend then WrChar('>'); if kf^.CompLex then WrChar('~');
      kf:=kf^.Chain end
    else if f^.Typ='A' then WrChar('~');
    WrStr(f^.Name);
    fl:=fl^.Chain; first:=false end;
  if kf<>nil then begin if not first then WrChar(';'); first:=true;
    while kf<>nil do begin if not first then WrChar(',');
      if kf^.Descend then WrChar('>'); if kf^.CompLex then WrChar('~');
      WrStr(kf^.FldD^.Name);
      kf:=kf^.Chain; first:=false end end;

  if (Mode=_ATotal) and (NLevels=0) then WrStr(^m^j'#RH') else WrStr(^m^j'#PH ');
  if RO^.HeadTxt=nil then begin
    WrStr('today,page;'^m^j); WrBlks(19);
    WrChar(^q); s:=CFile^.Name; WrBlks(8-length(s));
    SubstChar(s,'_','-'); WrStr(s); WrChar(^q);
    WrBlks(14); WrStr('__.__.____');
    RdMsg(17); WrBlks(12-length(MsgLine)); WrStr(MsgLine); WrStr('___') end
  else begin
    l:=RO^.HeadTxt^.LL; p:=Pchar(@RO^.HeadTxt^.A); i:=0; first:=true;
    while i<l do begin
      if p[i]='_' then begin point:=false;
        while (i<=l) and (p[i] in ['_','.']) do begin
          if p[i]='.' then point:=true; inc(i) end;
        if not first then WrChar(','); first:=false;
        if point then WrStr('today') else WrStr('page') end;
      inc(i) end;
    WrStr(';'^m^j);
    for i:=0 to l-1 do WrChar(p[i]) end;
  if Mode=_AErrRecs then begin
    RdMsg(18); WrStr(^m^j^w); WrBlks((38-length(MsgLine))div 2);
    WrStr(MsgLine); WrChar(^w) end;
  if RO^.CondTxt<>nil then begin
    WrStr(^m^j^w); s:=RO^.CondTxt^;
    SubstChar(s,'{','%'); SubstChar(s,'}','%'); SubstChar(s,'_','-');
    SubstChar(s,'@','*'); SubstChar(s,'#','='); SubstChar(s,'\','|');
    if length(s)>MaxCol then s[0]:=char(MaxCol);
    WrBlks((MaxColOld-length(s))div 2); WrStr(s); WrChar(^w) end;
  WrStr(^m^j);
  if KpLetter then WrChar(^e);
  d:=PFldDs; col:=1; while d<>nil do begin
    if d^.NxtLine then begin WrStr(^m^j); col:=1 end;
    WrBlks(d^.ColTxt-col); s:=d^.FldD^.Name; SubstChar(s,'_','-'); WrStr(s);
    col:=d^.ColTxt+length(d^.FldD^.Name); d:=d^.Chain end;

  if KpLetter then WrStr(^m^j'#PF;'^m^j^e);

  WrStr(^m^j'#DH .notsolo;'^m^j);
  if Mode<>_ATotal then begin WrStr(^m^j'#DE '); WrLevel(0) end;
  for i:=1 to NLevels do begin
    WrStr(^m^j'#CF_'); d:=PFldDs; while d<>nil do begin
      if d^.IsCtrl and (d^.Level=i) then WrStr(d^.FldD^.Name); d:=d^.Chain end;
    WrChar(' '); WrLevel(i) end;
  if (RO^.Ctrl<>nil) or (RO^.Sum<>nil) then begin
    WrStr(^m^j'#RF (sum(1)>0) '); WrLevel(NLevels+1) end;
  if WithNRecs then begin
    WrStr(^m^j'#RF ');if Mode=_AErrRecs then WrStr('noErrRecs,');
    WrStr('sum(1);'^m^j^m^j);
    if Mode=_AErrRecs then begin
     RdMsg(18); WrStr(MsgLine); WrStr(':_____'^m^j) end;
    RdMsg(20); WrStr(MsgLine); WrStr('_______') end;
  GenAutoRprt:=Txt;
  { for i:=1 to Txt^.LL do write(Txt^.A[i]); writeln; wait; }
end;

procedure RunAutoReport(RO:RprtOptPtr);
var p,p1:pointer; txt:LongStrPtr;
begin
  MarkStore(p); p1:=RO^.FDL.FD^.RecPtr;
  txt:=GenAutoRprt(RO,true); SetInpLongStr(txt,false);
  ReadReport(RO); RunReport(RO);
  RO^.FDL.FD^.RecPtr:=p1; ReleaseStore(p);
end;

function SelForAutoRprt(RO:RprtOptPtr):boolean;
var FL:FieldList; N:word;
begin
  SelForAutoRprt:=false;
  if (RO^.SK=nil) and not PromptSortKeys(RO^.Flds,RO^.SK) then exit;
  N:=Menu(4,1); if N=0 then exit;
  RO^.Mode:=autorprtmode(N-1); CFile:=RO^.FDL.FD;
  if RO^.Mode in [_ARprt,_ATotal] then begin
    FL:=RO^.Flds; while FL<>nil do begin
      if FL^.FldD^.Typ<>'T' then PutSelect(FL^.FldD^.Name); FL:=FL^.Chain end;
    if not SelFieldList(37,false,RO^.Ctrl) then exit;
    FL:=RO^.Flds; while FL<>nil do begin
      if FL^.FldD^.FrmlTyp='R' then PutSelect(FL^.FldD^.Name);
      FL:=FL^.Chain end;
    if not SelFieldList(38,true,RO^.Sum) then exit end;
  if spec.AutoRprtPrint then begin RO^.Path:=GetStore(5);RO^.Path^:='LPT1' end;
  SelForAutoRprt:=true;
end;

function SelGenRprt(const RprtName:string):LongStrPtr;
var r:RdbDPtr; fd:FileDPtr; f:FieldDPtr; ro:RprtOptPtr; s:string; i:integer;
    fl:FieldList;
begin
  SelGenRprt:=nil;
  r:=CRdb; while r<>nil do begin
    fd:=r^.FD^.Chain; while fd<>nil do begin
      s:=fd^.name; if r<>CRdb then s:=r^.FD^.Name+'.'+s; PutSelect(s);
      fd:=fd^.Chain end;
    r:=r^.ChainBack end;
  ss.Abcd:=true; SelectStr(0,0,19,'"'+RprtName+'"'); if KbdChar=_ESC_ then exit;
  s:=GetSelect; i:=pos('.',s); r:=CRdb; if i<>0 then begin
    repeat r:=r^.ChainBack until r^.fd^.Name=copy(s,1,i-1); s:=copy(s,i+1,255) end;
  fd:=r^.FD; repeat fd:=fd^.chain until fd^.Name=s;
  ro:=GetRprtOpt; ro^.FDL.FD:=fd;
  f:=fd^.FldD; while f<>nil do begin
    s:=f^.Name; if f^.Flg and f_Stored=0 then s:=SelMark+s; PutSelect(s);
    f:=f^.Chain end;
  CFile:=fd; SelFieldList(36,true,ro^.Flds);
  if ro^.Flds=nil then exit; ro^.Mode:=_ARprt;
  fl:=RO^.Flds; while fl<>nil do begin
    PutSelect(fl^.FldD^.Name); fl:=fl^.Chain end;
  if not SelFieldList(37,false,RO^.Ctrl) then exit;
  fl:=RO^.Flds; while fl<>nil do begin
    if fl^.FldD^.FrmlTyp='R' then PutSelect(fl^.FldD^.Name);
    fl:=fl^.Chain end;
  if not SelFieldList(38,false,RO^.Sum) then exit;
  SelGenRprt:=GenAutoRprt(ro,false);
end;

end.