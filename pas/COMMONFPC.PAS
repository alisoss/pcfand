{ COMMON.PAS for FPC: its assembler routines in Pascal }
procedure MyMove(var A1,A2; N:word);
begin Move(A1,A2,N); end;
function IsLetter(C:char):boolean;
begin IsLetter:=(C='_') or (C in ['A'..'Z']) or (C in ['a'..'z']) or (C>=#$80); end;
function IsDigit(C:char):boolean;
begin IsDigit:=C in ['0'..'9']; end;
procedure ChainLast(var Frst; New:pointer);
type PPtr=^pointer;
var p,nxt:PPtr; n:longint;
begin
  p:=PPtr(@Frst); n:=0;
  while p^<>nil do begin
    nxt := PPtr(p^);
    if (PtrUInt(nxt) < $100000) or (PtrUInt(nxt) > $FFFFFFFFFFFF) then break;
    p:=nxt;
    inc(n); if n > 1000000 then break;
  end;
  p^:=New;
  PPtr(New)^:=nil;
end;
function LastInChain(var Frst):pointer;
type PPtr=^pointer;
var p:pointer;
begin
  p:=pointer((@Frst)^);
  if p=nil then begin LastInChain:=@Frst; exit end;
  while PPtr(p)^<>nil do p:=PPtr(p)^;
  LastInChain:=p;
end;
function ListLength(P:pointer):word;
type PPtr=^pointer;
var n:word;
begin
  n:=0;
  while P<>nil do begin inc(n); P:=PPtr(P)^; end;
  ListLength:=n;
end;
function ListAt(P:pointer;I:word):pointer;
type PPtr=^pointer;
begin
  while (I>0) and (P<>nil) do begin P:=PPtr(P)^; dec(I); end;
  ListAt:=P;
end;
{ in place, as BP7's assembler version (which never copied S) }
procedure ReplaceChar(var S:string;C1,C2:char);
var i:integer;
begin for i:=1 to length(S) do if S[i]=C1 then S[i]:=C2; end;
function SEquUpcase(S1,S2:string):boolean;
var i:integer;
begin
  SEquUpcase:=false;
  if length(S1)<>length(S2) then exit;
  for i:=1 to length(S1) do
    if UpcCharTab[S1[i]]<>UpcCharTab[S2[i]] then exit;
  SEquUpcase:=true;
end;
function StrPas(Src:PChar):string;
var n:word; s:string;
begin
  n:=0; while (n<255) and (Src[n]<>#0) do begin s[n+1]:=Src[n]; inc(n) end;
  s[0]:=char(n); StrPas:=s;
end;
procedure StrLPCopy(Dest:PChar;s:string;MaxL:word);
var n:word;
begin FillChar(Dest^,MaxL,#0); n:=length(s); if n>=MaxL then n:=MaxL-1; Move(s[1],Dest^,n) end;
function SLeadEqu(S1,S2:string):word;
var i,len:word;
begin
  if length(S1)<length(S2) then len:=length(S1) else len:=length(S2);
  i:=0;
  while (i<len) and (S1[i+1]=S2[i+1]) do inc(i);
  SLeadEqu:=i;
end;
function EquMask1:word;
begin EquMask1:=0; end;
function EqualsMask(p:pointer;l:word;Mask:string):boolean;
var mi,pi:word;
  { BP7's EquMask1: the character after a '*' is matched as it is, even a
    '?' or '*' }
  function Match(mi,pi:word):boolean;
  var c:char; k:word;
  begin
    Match:=false;
    while mi<=length(Mask) do begin
      c:=Mask[mi]; inc(mi);
      if pi>l then begin Match:=(c='*') and (mi>length(Mask)); exit end;
      if c='*' then begin
        if mi>length(Mask) then begin Match:=true; exit end;
        c:=CharOrdTab[Mask[mi]]; inc(mi);
        while pi<=l do begin
          k:=pi; inc(pi);
          if CharOrdTab[chr(PByteArray(p)^[k-1])]=c then
            if Match(mi,pi) then begin Match:=true; exit end;
        end;
        exit;
      end;
      if c<>'?' then
        if CharOrdTab[c]<>CharOrdTab[chr(PByteArray(p)^[pi-1])] then exit;
      inc(pi);
    end;
    Match:=(pi>l);
  end;
begin EqualsMask:=Match(1,1) end;
function EquLongStr(S1,S2:LongStrPtr):boolean;
begin
  EquLongStr:=false;
  if S1^.LL<>S2^.LL then exit;
  if S1^.LL=0 then begin EquLongStr:=true; exit end;
  EquLongStr:=(CompareByte(S1^.A[1],S2^.A[1],S1^.LL)=0);
end;
function EquArea(P1,P2:pointer;L:word):boolean;
begin EquArea:=(CompareByte(P1^,P2^,L)=0); end;
function MinI(X, Y: Integer): Integer;
begin if X <= Y then MinI:= X else MinI:= Y end;
function MaxI(X, Y: Integer): Integer;
begin if X >= Y then MaxI:= X else MaxI:= Y end;
function MinW(X, Y: Word): Word;
begin if X <= Y then MinW:= X else MinW:= Y end;
function MaxW(X, Y: Word): Word;
begin if X >= Y then MaxW:= X else MaxW:= Y end;
function MinL(X, Y: longint): longint;
begin if X < Y then MinL:= X else MinL:= Y end;
function MaxL(X, Y: longint): longint;
begin if X < Y then MaxL:= Y else MaxL:= X end;
function SwapLong(N:longint):longint;
type TBA=array[0..3] of byte;
var r:longint;
begin
  TBA(r)[0]:=TBA(N)[3]; TBA(r)[1]:=TBA(N)[2];
  TBA(r)[2]:=TBA(N)[1]; TBA(r)[3]:=TBA(N)[0];
  SwapLong:=r;
end;
procedure ExChange(X,Y:pointer; L:word);
var i:word; t:byte;
begin
  if L=0 then exit;
  for i:=0 to L-1 do begin
    t:=PByteArray(X)^[i]; PByteArray(X)^[i]:=PByteArray(Y)^[i]; PByteArray(Y)^[i]:=t;
  end;
end;
function OverlapByteStr(p1,p2:pointer):boolean;
var i,j,l1,l2:byte;
begin
  OverlapByteStr:=false;
  l1:=PByteArray(p1)^[0]; l2:=PByteArray(p2)^[0];
  for i:=1 to l1 do
    for j:=1 to l2 do
      if PByteArray(p1)^[i]=PByteArray(p2)^[j] then begin OverlapByteStr:=true; exit end;
end;
function CountDLines(var Buf;L:word;C:char):word;
var i,n:word;
begin
  n:=1; if L=0 then begin CountDLines:=0; exit end;
  for i:=0 to L-1 do if PByteArray(@Buf)^[i]=ord(C) then inc(n);
  CountDLines:=n;
end;
function GetDLine(var Buf;L:word;C:char;I:word):string;
var j,start,len:word; tmp:string;
begin
  tmp:='';
  j:=0; dec(I);
  while (I>0) and (j<L) do begin
    if PByteArray(@Buf)^[j]=ord(C) then dec(I);
    inc(j);
  end;
  if (C=#13) and (j<L) and (PByteArray(@Buf)^[j]=10) then inc(j);
  start:=j;
  while (j<L) and (PByteArray(@Buf)^[j]<>ord(C)) do inc(j);
  len:=j-start; if len>255 then len:=255;
  tmp[0]:=chr(len);
  if len>0 then Move(PByteArray(@Buf)^[start],tmp[1],len);
  GetDLine:=tmp;
end;
function FindCtrlM(s:LongStrPtr;i,n:word):word;
var l:word;
begin l:=s^.LL; while (i<=l) do begin if s^.A[i]=^m then
    if n>1 then dec(n) else begin FindCtrlM:=i; exit end; inc(i) end;
  FindCtrlM:=l+1; exit;
end;
function SkipCtrlMJ(s:LongStrPtr;i:word):word;
var l:word;
begin l:=s^.LL; if i<=l then begin
    inc(i); if (i<=l) and (s^.A[i]=^j) then inc(i) end;
  SkipCtrlMJ:=i;
end;
procedure AddBackSlash(var S:string);
begin
  if (length(S)>0) and (S[length(S)]<>DirectorySeparator) then begin
    inc(S[0]); S[length(S)]:=DirectorySeparator end;
end;
procedure DelBackSlash(var S:string);
begin if (length(S)>3) and (S[length(S)]=DirectorySeparator) then dec(S[0]) end;
function MouseInRect(X,Y,XSize,YSize:word):boolean;
begin
  MouseInRect:=(Event.Where.X>=X) and (Event.Where.X<X+XSize) and
               (Event.Where.Y>=Y) and (Event.Where.Y<Y+YSize);
end;
function MouseInRectProc(X,Y,XSize,YSize:word):boolean;
begin
  if not IsGraphMode then
    MouseInRectProc:=(Event.Where.X>=X-1) and (Event.Where.X<X-1+XSize) and
                     (Event.Where.Y>=Y-1) and (Event.Where.Y<Y-1+YSize)
  else
    MouseInRectProc:=(Event.WhereG.X>=X) and (Event.WhereG.X<X+XSize) and
                     (Event.WhereG.Y>=Y) and (Event.WhereG.Y<Y+YSize);
end;
function HexStrToLong(S:string):longint;
var i:word; r:longint; c:byte;
begin
  r:=0;
  for i:=1 to length(S) do begin
    if (S[i]>='a') and (S[i]<='f') then c:=ord(S[i])-ord('a')+10
    else if (S[i]>='A') and (S[i]<='F') then c:=ord(S[i])-ord('A')+10
    else if (S[i]>='0') and (S[i]<='9') then c:=ord(S[i])-ord('0')
    else begin HexStrToLong:=0; exit end;
    r:=r shl 4 + c;
  end;
  HexStrToLong:=r;
end;
const NoDayInMonth:array[1..12]of word=(31,28,31,30,31,30,31,31,30,31,30,31);
function OlympYear(y:word):boolean;
  begin OlympYear:=(y mod 4=0) and ((y mod 100<>0) or (y mod 400=0)) end;
function OlympYears(y:word):word;
  begin if y<3 then OlympYears:=0
               else begin dec(y);OlympYears:=y div 4-y div 100+y div 400
  end               end;
function RDate (Y,M,D,hh,mm,ss,tt:word) :float;
  var i:word; l,n:longint; r:float;
  begin if (D>NoDayInMonth[M]) and ((M<>2) or (D<>29) or not(OlympYear(Y)))
        then begin RDate:=0;exit end;
        if Y+M+D=0 then l:=0
        else begin
             l:=longint(Y-1)*365+OlympYears(Y)+D;
             for i:=1 to M-1 do l:=l+NoDayInMonth[i];
             if (M>2) and OlympYear(Y) then inc(l) end;
        n:=tt+100*ss+6000*longint(mm); r:=(n+360000.0*hh)/(8640000.0);
        RDate:=l+r;
  end;
procedure SplitDate(R:float;var d,m,y:word);
  var l:longint; i,j:word;
  label 1;
  begin l:=trunc(R); if l=0 then begin y:=1;m:=1;d:=1 end
        else begin y:=l div 365; inc(y); l:=l mod 365;
              while l<=OlympYears(y) do begin dec(y);l:=l+365 end;
              l:=l-OlympYears(y);
              for j:=1 to 12 do begin
                  i:=NoDayInMonth[j];
                  if (j=2) and OlympYear(y) then inc(i);
                  if i>=l then goto 1; l:=l-i  end;
            1:m:=j;d:=l end;
  end;
function AddMonth(R,RM:float):float;
  var d,m,y:word; l:longint; RTime:float;
  begin SplitDate(R,d,m,y); l:=longint(y)*12+m-1+trunc(RM); RTime:=frac(R);
      y:=l div 12;m:=(l mod 12)+1;
      if d>NoDayInMonth[m] then
        begin d:=NoDayInMonth[m]; if (m=2) and Olympyear(y) then d:=29 end;
      AddMonth:=RDate(y,m,d,0,0,0,0)+RTime end;
function DifMonth(R1,R2:float):float;
var d1,m1,y1,d2,m2,y2:word;
begin
  SplitDate(R1,d1,m1,y1); SplitDate(R2,d2,m2,y2);
  DifMonth:=(integer(y2)-integer(y1))*12+integer(m2)-integer(m1);
end;
procedure EncodeMask(var Mask:string;var Min,Max:word);
  const Code:array[0..6]of char='YMDhmst';
  var i,j:word;
  begin Min:=9;Max:=0;for i:=1 to length(Mask) do for j:=0 to 6 do
        if Mask[i]=Code[j] then
        begin Mask[i]:=char(j);if Min>j then Min:=j;if Max<j then Max:=j end end;
procedure AnalDateMask(const Mask:string;var I,IDate,N:word);
  begin N:=0;
     if ord(Mask[I])<=6 then begin IDate:=ord(Mask[I]);
     repeat inc(I);inc(N) until (I>length(Mask)) or (ord(Mask[I])<>IDate) end;
  end;
function ExCode(N:word;const Mask:string):boolean;
  var i:word;
  begin ExCode:=true; for i:=1 to length(Mask) do if ord(Mask[i])=N then exit;
        ExCode:=false end;
function ValDate(const Txt:string;Mask:string):float;
var z:record Y,M,D,hh,mm,ss,tt:longint end;
    Date:array[0..6]of longint absolute z;
    i,j,k,min,max,iDate,n,Ylength,Year,y,Month,Day:word;
    s:string[4]; WasYMD,WasMinus:boolean;  R:float; n1:longint;
label 1,2,3;
begin
  ValDate:=0;Ylength:=0; z.Y:=-1;z.M:=-1;z.D:=-1; for i:=3 to 6 do Date[i]:=0;
  WasYMD:=false;WasMinus:=false; EncodeMask(Mask,min,max);
  i:=1; j:=1;
1:if j>length(Txt) then goto 2 else if i>length(Mask) then exit;
  AnalDateMask(Mask,i,iDate,n);
  if n=0 then begin if Mask[i]<>Txt[j] then exit;inc(i);inc(j) end
  else begin
    s:=''; if iDate<3 then WasYMD:=true;
    while (Txt[j]=' ') and (n>1) do begin inc(j);dec(n) end;
    if (Txt[j]='-') and (n>1) and (iDate=min) and (iDate>2) then begin
      WasMinus:=true;inc(j);dec(n) end;
    if not (Txt[j] in ['0'..'9']) then exit;
    while (j<=length(Txt)) and (Txt[j] in ['0'..'9']) and (n>0) do begin
      s:=s+Txt[j];inc(j);dec(n) end;
    val(s,Date[iDate],k); if iDate=0 then Ylength:=length(s); end;
  goto 1;
2:if (min=2) and (max>=3) then begin
    if z.D<0 then z.D:=0;
    R:=z.D+(z.tt+100*z.ss+6000*longint(z.mm)+360000.0*z.hh)/8640000.0;
    goto 3 end;
  if WasYMD then begin
    SplitDate(Today,Day,Month,Year);
    if z.D=-1 then z.D:=1 else
      if (z.D=0) or (z.D>31) then exit else if z.M=-1 then z.M:=Month;
    if z.M=-1 then z.M:=1 else if (z.M=0) or (z.M>12) then exit;
    if Ylength=0 then z.Y:=Year else if z.Y>9999 then exit else
      if Ylength<=2 then
        if Spec.OffDefaultYear=0 then z.Y:=(Year div 100)*100+z.Y
        else begin y:=(Year+Spec.OffDefaultYear) mod 100;
    if z.Y<y then z.Y:=z.Y+2000 else z.Y:=z.Y+1900 end end
  else begin z.Y:=0; z.M:=0; z.D:=0 end;
  if (min<3) and (z.hh>23) then exit; if (min<4) and (z.mm>59) then exit;
  if (min<5) and (z.ss>59) then exit;
  R:=RDate(z.Y,z.M,z.D,z.hh,z.mm,z.ss,z.tt);
3:if not WasYMD and (R=0.0) then R:=1E-11;
  if WasMinus then ValDate:=-R else ValDate:=R;
end;
function StrDate(R:float;Mask:string):string;
  var d:record Y,M,D:word end; Date:array[0..2]of word absolute d;
      t:record hh,mm,ss,tt:longint end; Time:array[3..6]of longint absolute t;
      s,x:ScreenStr; i,iDate,n,m,min,max:word;
      f,l:longint; First,WasMinus:boolean; c:char;
  const MultX:array[3..6] of real=(24,1440,86400,8640000);
        DivX:array[3..11] of longint=(1,60,3600,360000,1,60,6000,1,100);
  label 1;
  begin s:=''; EncodeMask(Mask,min,max); WasMinus:=false;
        if (R=0) or (R<0) and (min<3) then begin
          for i:=1 to length(Mask) do
            if ord(Mask[i])<=6 then s:=s+' ' else s:=s+Mask[i]; goto 1 end
        else if R<0 then begin WasMinus:=true;R:=-R end;
        if min<3 then begin
          if (min=2) and (max>=3) then d.D:=trunc(R) else SplitDate(R,d.D,d.M,d.Y);
          R:=frac(R) end;
        if max>=3 then
        begin l:=round(R*MultX[max]);
              if ExCode(3,Mask) then
                 begin f:=DivX[max]; t.hh:=l div f; l:=l mod f end;
              if ExCode(4,Mask) then
                 begin f:=DivX[max+3]; t.mm:=l div f; l:=l mod f end;
              if ExCode(5,Mask) then
                 begin f:=DivX[max+5]; t.ss:=l div f; l:=l mod f end;
              t.tt:=l;
        end;
        i:=1; First:=true;
        while i<=length(Mask) do
        begin AnalDateMask(Mask,i,iDate,n);
         if n=0 then begin s:=s+Mask[i];inc(i) end
         else begin
            if iDate<3 then str(Date[iDate],x)
            else begin str(Time[iDate],x);
               if (iDate=min) and WasMinus then x:='-'+x end;
            if First and (iDate>2) and (iDate=min) then c:=' ' else c:='0';
            First:=false; while length(x)<n do x:=c+x;
            if iDate<3 then x:=copy(x,length(x)-n+1,n);
            s:=s+x end end;
      1:StrDate:=s;
  end;
function Today :float;
  var Year,Month,Day,WeekDay:word;
  begin if userToday<>0 then Today:=userToday else begin
   GetDate(Year,Month,Day,WeekDay); Today:=RDate(Year,Month,Day,0,0,0,0) end end;
function CurrTime :float;
  var Hour,Minute,Second,Sec100:word;
  begin GetTime(Hour,Minute,Second,Sec100);
        CurrTime:=RDate(1,1,0,Hour,Minute,Second,Sec100) end;
function LenStyleStr(s:string):word;
  var l,i:word;
  begin l:=length(s); for i:=1 to length(s) do
        if s[i] in [^s,^w,^q,^d,^b,^e,^a] then dec(l); LenStyleStr:=l end;
function LogToAbsLenStyleStr(s:string;l:word):word;
  var i:word;
  begin i:=1; while (i<=length(s)) and (l>0) do
        begin if not(s[i] in [^s,^w,^q,^d,^b,^e,^a]) then dec(l); inc(i) end;
        LogToAbsLenStyleStr:=i-1 end;
var CStyle:string[10]; CColor:string[11];
function SetStyleAttr(c:char;var a:byte):boolean;
  begin SetStyleAttr:=true; with colors do if c=^s then a:=tUnderline
        else if c=^w then a:=tItalic else if c=^q then a:=tDWidth else
        if c=^d then a:=tDStrike else if c=^b then a:=tEmphasized else
        if c=^e then a:=tCompressed else if c=^a then a:=tElite
        else SetStyleAttr:=false end;
procedure WrStyleChar(c:char);
  var a:byte; b:boolean; i:word;
  begin if SetStyleAttr(c,a) then
             begin i:=pos(c,CStyle);
                   if i<>0 then
                      begin delete(CStyle,i,1);delete(CColor,i,1) end
                   else begin CStyle:=c+CStyle;CColor:=char(a)+CColor end;
                   TextAttr:=ord(CColor[1]) end
        else if c=^m then write(^m^j)
        else if c<>^j then write(c) end;
procedure WrStyleStr(s:string; Attr:word);
  var i:word;
  begin TextAttr:=Attr; CStyle:=''; CColor:=char(Attr);
        for i:=1 to length(s) do WrStyleChar(s[i]);TextAttr:=Attr end;
procedure WrLongStyleStr(S:LongStrPtr; Attr:word);
  var i:word;
  begin TextAttr:=Attr; CStyle:=''; CColor:=char(Attr);
        for i:=1 to S^.LL do WrStyleChar(S^.A[i]); TextAttr:=Attr end;
procedure RectToPixel(c1,r1,c2,r2:word; var x1,y1,x2,y2:word);
begin
end;
procedure SetMsgPar(const s:string);
  begin MsgPar[1]:=s end;
procedure Set2MsgPar(const s1,s2:string);
  begin MsgPar[1]:=s1; MsgPar[2]:=s2 end;
procedure Set3MsgPar(const s1,s2,s3:string);
  begin Set2MsgPar(s1,s2); MsgPar[3]:=s3 end;
procedure Set4MsgPar(const s1,s2,s3,s4:string);
  begin Set3MsgPar(s1,s2,s3); MsgPar[4]:=s4 end;
procedure WriteMsg(N:word);
  begin RdMsg(N);write(MsgLine) end;
procedure ClearLL(attr:byte);
  begin ScrClr(0,TxtRows-1,TxtCols,1,' ',colors.uNorm) end;
{$S-}procedure wait; begin if not FandBatch then readkey end;{$S+}
{$ifndef FandRunV}
const HexStr:array[0..15]of char='0123456789ABCDEF';
function HexB(b:byte):string2;
  begin HexB:=HexStr[b shr 4] + HexStr[b and $0F] end;
function HexW(i:word):string4;
  begin HexW:=HexB(hi(i)) + HexB(lo(i)) end;
function HexD(i:LongInt):string8;
  var a:array[1..4]of byte absolute i;
  begin HexD:=HexB(a[4])+HexB(a[3])+HexB(a[2])+HexB(a[1]) end;
function HexPtr(p:pointer):string9;
  begin HexPtr:=HexD(PtrUInt(p)) end;
procedure DispH(var ad;NoBytes:integer);
  var i:integer;
  begin for i:=0 to NoBytes-1 do write(HexB(PByteArray(@ad)^[i]),' ') end;
{$endif}
