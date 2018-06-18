procedure MyMove(var A1,A2; N:word); assembler;
asm  mov dx,ds; lds si,A1; les di,A2; mov cx,N; cld; rep movsb; mov ds,dx;
end;
function IsLetter(C:char):boolean; assembler;
asm  mov al,C; cmp al,'_'; je @1; cmp al,'A'; jb @2; cmp al,'Z'; jbe @1;
     cmp al,'a'; jb @2; cmp al,'z'; jbe @1; cmp al,80H; jb @2;
@1:  mov ax,1; jmp @3;
@2:  xor ax,ax;
@3:  end;
function IsDigit(C:char):boolean; assembler;
asm  mov al,C; cmp al,'0'; jb @1; cmp al,'9'; ja @1; mov ax,1; jmp @2;
@1:  xor ax,ax;
@2:  end;
procedure ChainLast(var Frst; New:pointer); assembler;
asm  push ds; lds si,Frst;
@1:  cmp [si+2].word,0; je @2; lds si,[si]; jmp @1;
@2:  les di,New; mov [si],di; mov ax,es; mov [si+2],ax;
     mov es:[di].word,0; mov es:[di+2].word,0; pop ds;
end;
function LastInChain(var Frst):pointer; assembler;
asm  les di,Frst;
@1:  cmp es:[di+2].word,0; je @2; les di,es:[di]; jmp @1;
@2:  mov dx,es; mov ax,di;
end;
function ListLength(P:pointer):word; assembler;
asm  xor ax,ax; les di,P;
@1:  mov dx,es; or dx,di; jz @2; inc ax; les di,DWORD PTR es:[di]; jmp @1;
@2:  end;
function ListAt(P:pointer;I:word):pointer; assembler; {zero based index}
asm  mov cx,I; les di,P; jcxz @2;
@1:  mov dx,es; or dx,si; jz @2; les di,DWORD PTR es:[di]; loop @1;
@2:  mov dx,es end;
procedure ReplaceChar(S:string;C1,C2:char); assembler;
asm  mov al,C1; mov dl,C2; les di,S; xor ch,ch; mov cl,es:[di]; inc di;
@1:  jcxz @2; repne scasb; jne @2; mov es:[di-1],dl; jmp @1;
@2:  end;
function SEquUpcase(S1,S2:string):boolean; assembler;
asm  push ds; cld; lds si,S1; mov dx,ds; les di,S2;
     xor ch,ch; mov cl,es:[di]; inc di; lodsb;
     cmp al,cl; jne @false; jcxz @true; xor bh,bh;
@1:  lodsb; mov bl,es:[di]; inc di; pop ds; push ds;
     mov ah,UpcCharTab[bx].byte;
     mov bl,al; cmp ah,UpcCharTab[bx].byte; jne @false;
     mov ds,dx; loop @1;
@true:  mov ax,1; jmp @2;
@false: xor ax,ax;
@2:  pop ds end;

function StrPas(Src:PChar):string;
var n:word; s:string;
begin
  n:=0; while (n<255) and (Src[n]<>#0) do begin s[n+1]:=Src[n]; inc(n) end;
  s[0]:=char(n); StrPas:=s;
end;
procedure StrLPCopy(Dest:PChar;s:string;MaxL:word);
var n:word;
begin
  FillChar(Dest^,MaxL,#0); n:=length(s); if n>=MaxL then n:=MaxL-1;
  Move(s[1],Dest^,n);
end;
function SLeadEqu(S1,S2:string):word; assembler;
asm push ds; cld; lds si,S1; les di,S2;
    mov cl,[si]; cmp cl,es:[di]; jna @1; mov cl,es:[di];  {min length}
 @1:inc si; inc di; mov al,cl; xor ch,ch; repe cmpsb; je @2; inc cx;
 @2:xor ah,ah; sub ax,cx; pop ds;
end;
function EquMask1:word; assembler;
asm
@1:  jcxz @fin; mov bl,ss:[si].byte; inc si; dec cx;
     cmp dx,0; jne @11; cmp bl,'*'; jne @no; jcxz @yes; jmp @no;
@11: cmp bl,'*'; jne @8; jcxz @yes;
     mov bl,ss:[si].byte; mov al,CharOrdTab[bx].byte; inc si; dec cx;
@2:  mov bl,es:[di].byte; cmp al,CharOrdTab[bx].byte; je @3;
     inc di; dec dx; jnz @2; jmp @no;
@3:  inc di; dec dx; push ax; push di; push dx; push si; push cx;
     call EquMask1; pop cx; pop si; pop dx; pop di; cmp ax,1; pop ax;
     je @yes; cmp dx,0; jne @2; jmp @no;
@8:  cmp bl,'?'; jne @9; inc di; dec dx; jmp @1;
@9:  mov al,CharOrdTab[bx].byte; mov bl,es:[di].byte;
     cmp al,CharOrdTab[bx].byte; jne @no;
     inc di; dec dx; jmp @1;
@fin:cmp dx,0; je @yes;
@no: xor ax,ax; jmp @6;
@yes:mov ax,1;
@6:  end;
function EqualsMask(p:pointer;l:word;Mask:string):boolean;
begin {PASCAL copies Mask to local stack}
asm  les di,p; mov dx,l; lea si,Mask; xor ch,ch; mov cl,ss:[si].byte; inc si;
     xor bh,bh; call EquMask1; mov @result,al;
end end;
function EquLongStr(S1,S2:LongStrPtr):boolean; assembler;
asm  push ds; cld; lds si,S1; lodsw; les di,S2; mov cx,es:[di]; inc(di); inc(di);
     cmp ax,cx; jne @1; rep cmpsb; jne @1; mov ax,1; jmp @2;
@1:  xor ax,ax;
@2:  pop ds;
end;
function EquArea(P1,P2:pointer;L:word):boolean; assembler;
asm  push ds; cld; lds si,P1; les di,P2; mov cx,L;
     rep cmpsb; jne @1; mov ax,1; jmp @2;
@1:  xor ax,ax;
@2:  pop ds;
end;
function MinI(X, Y: Integer): Integer; assembler;
asm
   mov ax,X; cmp ax,Y; jle @@1; mov ax,Y; @@1:
end;
function MaxI(X, Y: Integer): Integer; assembler;
asm
   mov ax,X; cmp ax,Y; jge @@1; mov ax,Y; @@1:
end;
function MinW(X, Y: Word): Word; assembler;
asm
   mov ax,X; cmp ax,Y; jna @@1; mov ax,Y; @@1:
end;
function MaxW(X, Y: Word): Word; assembler;
asm
   mov ax,X; cmp ax,Y; jnb @@1; mov ax,Y; @@1:
end;
function MinL(X, Y: longint): longint;
begin
  if X < Y then MinL:= X else MinL:= Y;
end;
function MaxL(X, Y: longint): longint;
begin
  if X < Y then MaxL:= Y else MaxL:= X;
end;
function SwapLong(N:longint):longint; assembler;
asm mov dh,N.byte; mov dl,N[1].byte; mov ah,N[2].byte; mov al,N[3].byte end;
procedure ExChange(X,Y:pointer; L:word); assembler;
asm push ds; cld; lds si,X; les di,Y; mov cx,L; jcxz @2;
@1: mov al,[si]; mov ah,es:[di]; stosb; mov [si],ah; inc si; loop @1;
@2: pop ds end;
function OverlapByteStr(p1,p2:pointer):boolean; assembler;
asm push ds; cld; lds si,p1; lodsb; mov dl,al; mov ch,0;
@1: cmp dl,0; mov ax,0; je @2; dec dl; 
    les di,p2; mov cl,es:[di]; jcxz @2; inc di; lodsb;
    repne scasb; jne @1; mov ax,1
@2: pop ds end;
function CountDLines(var Buf;L:word;C:char):word; assembler;
asm  les di,Buf; mov cx,L; xor dx,dx; jcxz @2; mov al,C; cld;
@1:  inc dx; jcxz @2; repne scasb; je @1;
@2:  mov ax,dx;
end;
function GetDLine(var Buf;L:word;C:char;I:word):string; assembler;
asm  push ds; cld; les di,Buf; mov al,C; mov cx,L; jcxz @5; mov dx,I;
@1:  dec dx; cmp dx,0; je @2; repne scasb; jcxz @5; jmp @1;
@2:  cmp al,0dH; jne @3; cmp es:[di].byte,0aH; jne @3; inc di; dec cx;
@3:  mov si,di; jcxz @5; repne scasb; je @4; inc di;
@4:  mov cx,di; sub cx,si; dec cx; push es; pop ds;
@5:  les di,@result; mov es:[di],cl; inc di; rep movsb; pop ds;
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

procedure AddBackSlash(S:string); assembler;
asm  les di,S; mov bl,es:[di]; cmp bl,0; je @1; xor bh,bh;
     cmp es:[di+bx].byte,'\'; je @1;
     mov es:[di+bx+1].byte,'\'; inc bl; mov es:[di],bl;
@1:  end;
procedure DelBackSlash(S:string); assembler;
asm  les di,S; mov bl,es:[di]; cmp bl,3; jna @1;
     xor bh,bh; cmp es:[di+bx].byte,'\'; jne @1; dec bl; mov es:[di],bl;
@1:  end;

function MouseInRect(X,Y,XSize,YSize:word):boolean; assembler;
asm  mov ax,0; mov dx,Event.Where.X;
     mov cx,X; cmp dx,cx; jb @1; add cx,XSize; cmp dx,cx; jae @1;
     mov dx,Event.Where.Y;
     mov cx,Y; cmp dx,cx; jb @1; add cx,YSize; cmp dx,cx; jae @1; mov ax,1;
@1:  end;

function MouseInRectProc(X,Y,XSize,YSize:word):boolean; assembler;
asm  mov ax,0; cmp IsGraphMode,0; jne @1;
     mov dx,Event.Where.X; mov cx,X; dec cx;          {textovy rezim}
     cmp dx,cx; jb @2;
     add cx,XSize;cmp dx,cx; jae @2;
     mov dx,Event.Where.Y;mov cx,Y; dec cx;
     cmp dx,cx; jb @2;
     add cx,YSize; cmp dx,cx; jae @2;
     mov ax,1;jmp @2;
@1:  mov dx,Event.WhereG.X; mov cx,X;                 {graficky rezim}
     cmp dx,cx; jb @2;
     add cx,XSize;cmp dx,cx; jae @2;
     mov dx,Event.WhereG.Y; mov cx,Y;
     cmp dx,cx; jb @2;
     add cx,YSize; cmp dx,cx; jae @2; mov ax,1;
@2:  end;


function HexStrToLong(S:string):longint; assembler;
asm  push ds; cld; lds si,S; lodsb; xor ah,ah; mov cx,ax; jcxz @5;
     xor dx,dx; xor bx,bx;
@1:  lodsb; cmp al,'a'; jb @2; cmp al,'f'; ja @5; sub al,57H; jmp @4;
@2:  cmp al,'A'; jb @3; cmp al,'F'; ja @5; sub al,37H; jmp @4;
@3:  cmp al,'0'; jb @5; cmp al,'9'; ja @5; sub al,30H;
@4:  shl bx,1; rcl dx,1; shl bx,1; rcl dx,1; shl bx,1; rcl dx,1; shl bx,1; rcl dx,1;
     add bx,ax; loop @1; mov ax,bx; jmp @6;
@5:  xor dx,dx; xor ax,ax;
@6:  pop ds;
end;


               { TIME - DATE }


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
  if n=0 then begin if Mask[i]<>Txt[j] then exit;inc(i);inc(j) end { delimiter }
  else begin                                                       { YMDhmst }
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
    {if (max<3) and (z.D=-1) and (z.M=-1) and (z.Y=-1) then exit;}
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


                      { DISPLAY }


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
var mx,my:word;dif:byte;
begin {$ifdef FandGraph}
  if IsGraphMode then begin
	if GraphMode=HercMonoHi then dif:=2 else dif:=0;
	mx:=GetMaxX; if GraphMode=VGAHi {GraphDriver=VGA} then my:=GetMaxY-5 else my:=GetMaxY;
	x1:=(c1*mx)div TxtCols; y1:=(r1*(my+dif))div TxtRows;
	x2:=((c2+1)*mx)div TxtCols; y2:=((r2+1)*(my+dif))div TxtRows end;
	{$endif}
end;



                         { MESSAGES }



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





                 {  DEBUGGING  }

{$S-}procedure wait; begin readkey end;{$S+}

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
  var a:array[1..2]of word absolute p;
  begin HexPtr:=HexW(a[2])+':'+HexW(a[1]) end;
procedure DispH(var ad;NoBytes:integer);
  var i:integer; p:^byte;
  begin p:=addr(ad);
        for i:=1 to NoBytes do
        begin write({TrF,}HexB(p^),' '); p:=Ptr(seg(p^),ofs(p^)+1) end;
  end;

{$endif}
