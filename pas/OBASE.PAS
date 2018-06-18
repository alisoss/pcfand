{$I switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}
unit obase;

interface

uses dos;

function IsPrintCtrl(c:char):boolean;
procedure PrintChar(c:char);
procedure PrintStr(s:string);
function ResetPrinter(PgeLength,LeftMargin:word;Adj,Frst:boolean):boolean;
procedure ClosePrinter(LeftMargin:word);
procedure Seek0Txt(var F:text);
function OpenTxt(var F:TextRec):integer;
function ResetTxt(var F:text):boolean;
function RewriteTxt(var F:text;PrintCtrl:boolean):boolean;
procedure SetPrintTxtPath;
function  OpenLPTHandle:word;

implementation

uses base,obaseww,drivers,wwmenu;


procedure Ovr; far;
assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push cs; push ax;
end;


var SFlag,QFlag,WFlag,BFlag,DFlag,EFlag,AFlag,XFlag,VFlag,TFlag:boolean;
    CPState,CPCount:word;

procedure ResetCtrlFlags;
begin
  SFlag:=false;BFlag:=false;QFlag:=false;WFlag:=false;DFlag:=false;
  EFlag:=false;AFlag:=false;XFlag:=false;VFlag:=false;TFlag:=false;
  CPState:=0;
end;

function IsPrintCtrl(C:char):boolean;
begin
  IsPrintCtrl:= (C in [^s,^q,^w,^b,^d,^e,^a,^x,^v,^t])
end;

procedure PrintByte(B:byte);
const timeout=$01;errorio=$08;
      selected=$10;outofpaper=$20;acknowledge=$40;notbusy=$80;
var r:registers;  N,LptNr:word;
label 2,3;
begin
{  write(' ',hexb(b));wait;exit; }
  with printer[prCurr] do begin
   if ToHandle then begin WriteH(Handle,1,B); exit end; LptNr:=Lpti-1 end;
  if ESCPressed and PromptYN(22) then begin
    r.AH:=0;r.AL:=$18{cancel buffer};r.DX:=LptNr;Intr($17,r);
    r.AH:=0;r.AL:=ord(^l){form feed};r.DX:=LptNr;Intr($17,r);
    WasLPTCancel:=true;GoExit end;
2:r.AH:=0;r.AL:=B;r.DX:=LptNr;Intr($17,r);
  if (r.AH and errorio)<>0 then
    if (r.AH and outofpaper)<>0 then begin N:=11;{out of paper} goto 3 end
    else begin N:=12;{turn printer on} goto 3 end
  else if (r.AH and timeout)<>0 then begin
    N:=13; {printer offline}
3:  F10SpecKey:=_ESC_;WrLLF10Msg(N);
    if KbdChar=_ESC_ then GoExit else goto 2 end;
end;
procedure PrintByteStr(S:string);
  var i:word;
  begin for i:=1 to Length(S) do PrintByte(ord(S[i])) end;


function CtrlToESC(C:char):string;
begin
  case C of
   ^h,^l:CtrlToESC:=c;
   ^m:CtrlToESC:=^m^j;  { weak new-line, ^j of hard new-line ignored }
   ^s:if SFlag then begin CtrlToESC:=PrTab(prUl2);SFlag:=False end
         else begin SFlag:=True;CtrlToESC:=PrTab(prUl1) end;
   ^q:if QFlag then begin CtrlToESC:=PrTab(prBr2);QFlag:=False end
      else begin QFlag:=True;CtrlToESC:=PrTab(prBr1) end;
   ^w:if WFlag then begin CtrlToESC:=PrTab(prKv2);WFlag:=False end
      else begin WFlag:=True;CtrlToESC:=PrTab(prKv1) end;
   ^b:if BFlag then begin CtrlToESC:=PrTab(prBd2);BFlag:=False end
      else begin BFlag:=True;CtrlToESC:=PrTab(prBd1) end;
   ^d:if DFlag then begin CtrlToESC:=PrTab(prDb2);DFlag:=False end
      else begin DFlag:=True;CtrlToESC:=PrTab(prDb1) end;
   ^e:if EFlag then begin CtrlToESC:=PrTab(prKp2);EFlag:=False end
      else begin EFlag:=True;CtrlToESC:=PrTab(prKp1) end;
   ^a:if AFlag then begin CtrlToESC:=PrTab(prEl2);AFlag:=false end
      else begin AFlag:=true;CtrlToESC:=PrTab(prEl1) end;
   ^x:if XFlag then begin CtrlToESC:=PrTab(prUs12);XFlag:=false end
      else begin XFlag:=true;CtrlToESC:=PrTab(prUs11) end;
   ^v:if VFlag then begin CtrlToESC:=PrTab(prUs22);VFlag:=false end
      else begin VFlag:=true;CtrlToESC:=PrTab(prUs21) end;
   ^t:if TFlag then begin CtrlToESC:=PrTab(prUs32);TFlag:=false end
      else begin TFlag:=true;CtrlToESC:=PrTab(prUs31) end;
   else CtrlToESC:='' end;
end;
function CPTest(c:char):word; { 0=binary;1=normal;2=control;3=bypass }
begin
  case CPState of  {0=normal; 1=lo count; 2=hi count; 3=binary }
    0: if c<' ' then
         if c=^P then begin
            CPState:=1; CPTest:=3 end
         else CPTest:=2
       else CPTest:=1;
    1: begin CPState:=2; CPCount:=ord(c); CPTest:=3 end;
    2: begin CPCount:=CPCount + (word(c) shl 8);
       if CPCount=0 then CPState:=0 else CPState:=3; CPTest:=3 end;
    3: begin dec(CPCount); CPTest:=0; if CPCount=0 then CPState:=0 end end;
end;
procedure TranslateCodePage(var c:char);
begin
  if c>=#$80 then case printer[prCurr].Kod of
    'K':ConvKamenLatin(c,1,true);
    'k':ConvToNoDiakr(c,1,foKamen);
    'L':ConvKamenLatin(c,1,false);
    'l':ConvToNoDiakr(c,1,foLatin2) end;
end;
procedure PrintChar(c:char);
begin
  case CPTest(c) of
    0:PrintByte(ord(c));
    1:begin TranslateCodePage(c); PrintByte(ord(c)) end;
    2:PrintByteStr(CtrlToESC(c)) end;
end;
procedure PrintStr(s:string);
var i:word;
begin
  for i:=1 to length(s) do PrintChar(s[i]);
end;
function OpenLPTHandle:word;
var nr:string;
begin
 str(printer[prCurr].Lpti,nr); CPath:='LPT'+nr;
 OpenLPTHandle:=OpenH(_isoverwritefile,Exclusive);
end;
function ResetPrinter(PgeLength,LeftMargin:word;Adj,Frst:boolean):boolean;
var er:ExitRecord; s:string[5];
label 1;
begin
  ResetPrinter:=false; NewExit(Ovr,er); goto 1;
  if Adj then
    if spec.ChoosePrMsg then begin
      if not printer[prCurr].ToHandle then PrintStr(^h^h) end
    else if not printer[prCurr].ToHandle then begin
      PrintStr(^h^h); F10SpecKey:=_ESC_; WrLLF10Msg(10);{adjust printer}
      if KbdChar=_ESC_ then goto 1 end;
  with printer[prCurr] do if ToHandle then Handle:=OpenLPTHandle;
  if Frst then begin
    PrintByteStr(PrTab(prReset)); if length(PrTab(prPageSizeNN))>0 then begin
      PrintByteStr(PrTab(prPageSizeNN));
      if printer[prCurr].Typ='L' then begin
        str(maxw(8,minw(PgeLength,128)),s); PrintByteStr(s) end
      else PrintByte(PgeLength);
      PrintByteStr(PrTab(prPageSizeTrail)) end;
    if (LeftMargin>0) and (length(PrTab(prLMarg))>0) then begin
      PrintByteStr(PrTab(prLMarg));
      if printer[prCurr].Typ='L' then begin
        str(LeftMargin,s); PrintByteStr(s) end
      else PrintByte(LeftMargin);
      PrintByteStr(PrTab(prLMargTrail)) end end;
  ResetCtrlFlags; WasLPTCancel:=false; ResetPrinter:=true;
1:RestoreExit(er);
end;
procedure ClosePrinter(LeftMargin:word);
var nr:string[3]; h:word;
begin
  if LeftMargin>0 then begin
    PrintByteStr(PrTab(prLMarg));
    if printer[prCurr].Typ='L' then PrintByteStr('0') else PrintByte(0);
    PrintByteStr(PrTab(prLMargTrail)) end;
  if PrTab(prClose)<>'ff' then PrintByteStr(PrTab(prClose)); {Mark***}
  with printer[prCurr] do
  if ToHandle then CloseH(Handle) else
  if OpCls then CloseH(OpenLPTHandle);
end;

var PrintCtrlFlag:boolean;
procedure TestTxtHError(var F:TextRec);
var s:string;
begin
  if HandleError<>0 then begin
    SetMsgPar(StrPas(F.Name)); WrLLF10Msg(700+HandleError); GoExit end;
end;
function InputTxt(var F:TextRec):integer; far;
    begin with F do begin
          BufEnd:=ReadH(Handle,BufSize,BufPtr^); BufPos:=0 end;
          TestTxtHError(F); InputTxt:=0 end;
function OutputTxt(var F:TextRec):integer; far;
var p:pointer; n:word; s:string;
    x1:CharPtr;x1ofs:word absolute x1; x2:CharPtr;x2ofs:word absolute x2;
begin
  if PrintCtrlFlag then begin
    p:=GetStore(1024); x1:=pointer(F.BufPtr); x2:=p; n:=0;
    while F.BufPos>0 do begin
      case CPTest(x1^) of
        0: begin x2^:=x1^; inc(n); inc(x2ofs) end;
        1:begin x2^:=x1^; inc(n); TranslateCodePage(x2^); inc(x2ofs) end;
        2: begin s:=CtrlToESC(x1^); move(s[1],x2^,length(s));
           inc(n,length(s)); inc(x2ofs,length(s)) end end;
      inc(x1ofs); dec(F.BufPos) end;
    WriteH(F.Handle,n,p^); ReleaseStore(p) end
  else WriteH(F.Handle,F.BufPos,F.BufPtr^);
  F.BufPos:=0; TestTxtHError(F); OutputTxt:=0;
end;
function OutputLPT1(var F:TextRec):integer; far;
var c:CharPtr; COfs:word absolute c;
begin
  with F do begin
    c:=CharPtr(BufPtr); while BufPos>0 do begin
      if not WasLPTCancel then PrintChar(c^); dec(BufPos); inc(COfs) end end;
  OutputLPT1:=0;
end;
function FlushTxt(var F:TextRec):integer; far;
    begin FlushTxt:=0 end;
function CloseTxt(var F:TextRec):integer; far;
    begin CloseH(F.Handle);TestTxtHError(F);CloseTxt:=0 end;
function CloseLPT1(var F:TextRec):integer; far;
    begin if not WasLPTCancel then PrintChar(^l);CloseLPT1:=0 end;
function OpenTxt(var F:TextRec):integer;
    begin with F do begin
         if Mode=fmInOut {append} then SeekH(Handle,FileSizeH(Handle));
         if Mode=fmInput then InOutFunc:=@InputTxt else InOutFunc:=@OutputTxt;
         FlushFunc:=@FlushTxt; CloseFunc:=@CloseTxt end;
         if PrintCtrlFlag then ResetCtrlFlags;
         OpenTxt:=0 end;
function OpenLPT1(var F:TextRec):integer; far;
    begin with F do begin
         InOutFunc:=@OutputLPT1;CloseFunc:=@CloseLPT1 end; OpenLPT1:=0 end;
procedure Seek0Txt(var F:text);
   begin with TextRec(F) do begin
         SeekH(Handle,0); bufend:=bufsize; bufpos:=bufend end end;
function ResetTxt(var F:text):boolean;
  begin Assign(F,CPath);with TextRec(F) do begin
        OpenFunc:=@OpenTxt; Handle:=0; { for error detection in OpenH }
        Handle:=OpenH(_isoldfile,RdOnly) end;
        if HandleError<>0 then begin ResetTxt:=false;exit end;
        Reset(F); ResetTxt:=true end;
function RewriteTxt(var F:text;PrintCtrl:boolean):boolean;
  begin Assign(F,CPath);
        if CPath='LPT1' then TextRec(F).OpenFunc:=@OpenLPT1
        else begin PrintCtrlFlag:=PrintCtrl; TextRec(F).OpenFunc:=@OpenTxt;
           TextRec(F).Handle:=OpenH(_isoverwritefile,Exclusive);
           if HandleError<>0 then begin RewriteTxt:=false;exit end end;
        Rewrite(F); RewriteTxt:=true end;
procedure SetPrintTxtPath;
  begin CPath:=WrkDir+'PRINTER.TXT';CVol:='' end;



end.