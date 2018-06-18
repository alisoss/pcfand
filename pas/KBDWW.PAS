
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




type
  WParamPtr=^WParam;
  WParam=record
    Min,Max,Attr:word;
    Cursor:longint;
    GrRoot:longint;
  end;
  WGrBufPtr=^WGrBuf;
  WGrBuf=record
    LL:word;
    ChainPos:longint;
    X,Y:word;
    A:byte;
  end;
const MaxGrBufSz=$7fff-4;

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

function PushWParam(C1,R1,C2,R2:word;WW:boolean):pointer;
var wp:WParamPtr;
begin
  wp:=GetZStore(sizeof(WParam));
  wp^.Min:=word(WindMin); wp^.Max:=word(WindMax); wp^.Attr:=TextAttr;
  wp^.Cursor:=CrsGet; if WW then window(C1,R1,C2,R2); PushWParam:=wp;
end;

procedure PopWParam(p:pointer);
var wp:WParamPtr absolute p;
begin word(WindMin):=wp^.Min; word(WindMax):=wp^.Max; TextAttr:=wp^.Attr;
      CrsSet(wp^.Cursor) end;

function PushScr(C1,R1,C2,R2:word):pointer;
var p:pointer;
begin
  dec(C1); dec(R1); p:=GetStore(4); PushScr:=p;
  asm  les di,p; cld; mov ax,C1; stosw; mov ax,R1; stosw end;
  ScrPush(C1,R1,C2-C1,R2-R1);
end;

function PushW1(C1,R1,C2,R2:word;PushPixel,WW:boolean):longint;
var s:LongStrPtr; w:WParamPtr;
    x1,y1,x2,y2,i,n,sz:word; pos:longint; buf:WGrBufPtr; vp:ViewPortType;
label 1;
begin
  pos:=0;                            {$ifdef FandGraph}
  if IsGraphMode and PushPixel then begin
    HideMaus; GetViewSettings(vp); SetViewPort(0,0,GetMaxX,GetMaxY,true);
    RectToPixel(C1-1,R1-1,C2-1,R2-1,x1,y1,x2,y2);
    buf:=GetStore($7fff+2); i:=y1;
    repeat n:=y2-i+1;
1:    sz:=ImageSize(x1,i,x2,i+n-1);
      if (sz=0) or (sz>$7fff-8) then begin n:=n shr 1; goto 1 end;
      with buf^ do begin LL:=sz+8; ChainPos:=pos; X:=x1; Y:=i end;
      GetImage(x1,i,x2,i+n-1,buf^.A);
      pos:=StoreInTWork(LongStrPtr(buf));
      inc(i,n);
    until i>y2;
    ReleaseStore(buf); SetViewPort(vp.X1,vp.Y1,vp.X2,vp.Y2,vp.Clip);
    ShowMaus end;                    {$endif}
  s:=GetStore(2); w:=PushWParam(C1,R1,C2,R2,WW); w^.GrRoot:=pos;
  PushScr(C1,R1,C2,R2);
  s^.LL:=AbsAdr(HeapPtr)-AbsAdr(s)-2; PushW1:=StoreInTWork(s);
  ReleaseStore(s);
end;

function PushW(C1,R1,C2,R2:word):longint;
begin PushW:=PushW1(C1,R1,C2,R2,false,true) end;

procedure PopScr(p:pointer);
var C1,R1:word;
begin
asm  push ds; lds si,p; cld; lodsw; mov C1,ax; lodsw; mov R1,ax;
     mov p.word,si; pop ds end;
  ScrPop(C1,R1,p);
end;

procedure PopW2(pos:longint;draw:boolean);
var s:LongStrPtr; w:WParamPtr; wofs:word absolute w;
    buf:WGrBufPtr; b:boolean; vp:ViewPortType;
begin
  s:=ReadDelInTWork(pos);
  w:=WParamPtr(@s^.A); PopWParam(w); pos:=w^.GrRoot;
  if draw then begin inc(wofs,sizeof(WParam)); b:=IsGraphMode;
    if pos<>0 then IsGraphMode:=false;{don't actually draw content of text buf}
    PopScr(w); IsGraphMode:=b end;
  ReleaseStore(s);                              {$ifdef FandGraph}
  if IsGraphMode and (pos<>0) then begin
    HideMaus; GetViewSettings(vp); SetViewPort(0,0,GetMaxX,GetMaxY,true);
    while pos<>0 do begin
      buf:=WGrBufPtr(ReadDelInTWork(pos));
      if draw then PutImage(buf^.X,buf^.Y,buf^.A,0);
      pos:=buf^.ChainPos; ReleaseStore(buf) end;
    SetViewPort(vp.X1,vp.Y1,vp.X2,vp.Y2,vp.Clip); ShowMaus end; {$endif}
end;
procedure PopW(pos:longint);
  begin PopW2(pos,true) end;


procedure WriteWFrame(WFlags:byte;const top,bottom:string);
procedure WrHd(const Hd:string;Row,MaxCols:word);
var s:ScreenStr;
begin
  if Hd='' then exit; s:=' '+Hd+' ';
  if length(s)>MaxCols then s[0]:=char(MaxCols);
  gotoxy((MaxCols-length(s)) div 2+2,Row); write(s);
end;
var i,cols,rows,n:word;
begin
  if (WFlags and WhasFrame)=0 then exit;
  n:=0; if (WFlags and WDoubleFrame)<>0 then n:=9;
  cols:=WindMax.X-WindMin.X+1; rows:=WindMax.Y-WindMin.Y+1;
  ScrWrFrameLn(WindMin.X,WindMin.Y,N,cols,TextAttr);
  for i:=1 to Rows-2 do
    if (WFlags and WNoClrScr)=0 then
      ScrWrFrameLn(WindMin.X,WindMin.Y+i,N+6,cols,TextAttr)
    else begin
      ScrWrChar(WindMin.X,WindMin.Y+i,FrameChars[N+6],TextAttr);
      ScrWrChar(WindMin.X+cols-1,WindMin.Y+i,FrameChars[N+8],TextAttr) end;
  ScrWrFrameLn(WindMin.X,WindMax.Y,N+3,cols,TextAttr);
  WrHd(top,1,cols-2);WrHd(bottom,Rows,cols-2);
end;
procedure CenterWw(var C1,R1,C2,R2:byte; WFlags:byte);
var Cols,Rows,M:integer;
begin
  M:=0; if (WFlags and WHasFrame)<>0 then M:=2;
  Cols:=C2+M; if C1<>0 then Cols:=C2-C1+1; Cols:=maxi(M+1,mini(Cols,TxtCols));
  if C1=0 then C1:=(TxtCols-Cols) div 2 +1 else C1:=mini(C1,TxtCols-Cols+1);
  C2:=C1+Cols-1;
  Rows:=R2+M; if R1<>0 then Rows:=R2-R1+1; Rows:=maxi(M+1,mini(Rows,TxtRows));
  if R1=0 then R1:=(TxtRows-Rows) div 2 +1 else R1:=mini(R1,TxtRows-Rows+1);
  R2:=R1+Rows-1;
end;
function PushWFramed(C1,R1,C2,R2:byte; Attr:word; const top,bottom:string;
  WFlags:byte):longint;
var i,x,y:word;
begin
  CenterWw(C1,R1,C2,R2,WFlags);
  x:=0; y:=0; if (WFlags and WShadow)<>0 then begin
    x:=minw(2,TxtCols-C2); y:=minw(1,TxtRows-R2) end;
  PushWFramed:=PushW1(C1,R1,C2+x,R2+y,(WFlags and WPushPixel)<>0,true); CrsHide;
  if y=1 then ScrColor(C1+1,R2,C2-C1+x-1,colors.ShadowAttr);
  if x>0 then for i:=R1 to R2 do ScrColor(C2,i,x,colors.ShadowAttr);
  window(C1,R1,C2,R2); TextAttr:=Attr;
  if (WFlags and WHasFrame)<>0 then begin
    WriteWFrame(WFlags,top,bottom); window(C1+1,R1+1,C2-1,R2-1) end;
end;


                         { MESSAGES }


var MsgPar:array[1..4]of ScreenStr;

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

procedure WrLLMsg(N:word);
begin RdMsg(N); WrLLMsgTxt end;

function PushWrLLMsg(N:word;WithESC:boolean):longint;
var l:word;
begin
  PushWrLLMsg:=PushW(1,TxtRows,TxtCols,TxtRows);
  TextAttr:=colors.zNorm; ClrEol;
  TextAttr:=colors.zNorm or $80; write('  ');
  TextAttr:=colors.zNorm; if WithESC then write('(ESC) ');
  RdMsg(N); l:=TxtCols-whereX; if length(MsgLine)>l then MsgLine[0]:=char(l);
  write(MsgLine);
end;

procedure WrLLMsgTxt;
var p:pointer; i,j,w:word; On:boolean;
    Buf:array[0..MaxTxtCols] of word;
begin
  p:=PushWParam(1,TxtRows,TxtCols,TxtRows,true); WordRec(w).Hi:=colors.lNorm;
  On:=false; i:=1; j:=0;
  while (i<=length(MsgLine)) and (j<TxtCols) do begin
    if MsgLine[i]=^w then
      if On then begin WordRec(w).Hi:=colors.lNorm; On:=false end
      else begin WordRec(w).Hi:=colors.lFirst; On:=true end
    else begin WordRec(w).Lo:=ord(MsgLine[i]); Buf[j]:=w; inc(j) end;
    inc(i) end;
  WordRec(w).Lo:=ord(' '); while j<TxtCols do begin Buf[j]:=w; inc(j) end;
  ScrWrBuf(0,TxtRows-1,Buf,TxtCols);
  PopWParam(p);ReleaseStore(p);
end;

{$S+}procedure WrLLF10MsgLine;
var Buf:array[1..MaxTxtCols] of word; col,row,len:word;
label 1,2,3;
begin
  row:=TxtRows-1; ScrRdBuf(0,row,Buf,TxtCols);
  Beep; ScrClr(0,row,TxtCols,1,' ',colors.zNorm);
  if F10SpecKey=$ffff then
    ScrWrStr(0,row,'...!',colors.zNorm or $80)
  else ScrWrStr(0,row,'F10!',colors.zNorm or $80);
  col:=length(MsgLine)+5; len:=0;
  if (F10SpecKey=$fffe) or (F10SpecKey=_F1_) then begin
    MsgLine:=MsgLine+' '^p'F1'; len:=2 end;
  if (F10SpecKey=$fffe) or (F10SpecKey=_ShiftF7_) then begin
    MsgLine:=MsgLine+' '^p'ShiftF7'; len:=len+7 end;
  if length(MsgLine)>TxtCols-5 then begin
    MsgLine[0]:=char(TxtCols-5); len:=0 end;
  ScrWrStr(5,row,MsgLine,colors.zNorm);
1:GetEvent; with Event do case What of
   evMouseDown:
     if MouseInRect(0,row,3,1) then begin KbdChar:=_F10_; goto 2 end
     else if (len>0) and MouseInRect(col,row,len,1) then begin
       KbdChar:=F10SpecKey; goto 2 end;
   evKeyDown:
     if (KeyCode=_F10_) or (KeyCode=F10SpecKey) or (F10SpecKey=$ffff)
     or (F10SpecKey=$fffe) and ((KeyCode=_ShiftF7_) or (KeyCode=_F1_)) then
       begin KbdChar:=KeyCode;
2:     ClrEvent; goto 3 end;
  end; ClrEvent; goto 1;
3:F10SpecKey:=0;
  ScrWrBuf(0,row,Buf,TxtCols);
end; {$S-}

{$S-}procedure WrLLF10Msg(N:word);
  begin RdMsg(N); WrLLF10MsgLine end;{$S+}

function PromptYN(NMsg:word):boolean;
  var w:longint; col,row:word; cc:char;
  label 1;
  begin w:=PushW(1,TxtRows,TxtCols,TxtRows);
        TextAttr:=colors.pTxt;ClrEol;WriteMsg(NMsg);
        col:=whereX;row:=whereY;
        TextAttr:=colors.pNorm;
        write(' ');gotoxy(col,row); CrsShow;
      1:cc:=upcase(char(ReadKbd));
        if (KbdChar<>F10SpecKey) and
           (cc<>AbbrYes) and (cc<>AbbrNo) then goto 1;
        F10SpecKey:=0;PopW(w);PromptYN:=cc=AbbrYes;
   end;

var RunErrNr:word;
procedure RunError(N:word);
  begin RunErrNr:=N; ClearKbdBuf; WrLLF10Msg(RunErrNr); GoExit end;