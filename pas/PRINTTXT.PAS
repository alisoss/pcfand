{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L-}

unit printtxt;

interface


procedure PrintArray(P:pointer;N:word;CtrlL:boolean);
procedure PrintTxtFile(BegPos:longint);
procedure PrintFandWork;

implementation

uses dos,base,drivers,obase,obaseww,access,oaccess,wwmenu,runfrml,rdrun;

procedure Ovr; far;
assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push cs; push ax;
end;

var pBlk:CharArrPtr; iBlk,nBlk,Po:word;  charrd:longint;
    printBlk,outpsw:boolean;
function replaceNo(s:string;sNew:string):string;
var i:integer;
begin
 i:=pos('#',s); if i>0 then begin delete(s,i,1); insert(sNew,s,i) end;
 replaceNo:=s;
end;
procedure ExecMgrPgm;
var pgmNm,param:string; x,y:byte; wmin,wmax:word; w,crs:longint;
begin
  pgmNm:=prTab(prMgrProg); if pgmNm='' then exit;
  param:=replaceNo(prTab(prMgrParam),CPath);
  wmin:=word(WindMin); wmax:=word(WindMax); crs:=CrsGet;
  w:=PushW(1,1,TxtCols,1);
  word(WindMin):=wmin; word(WindMax):=wmax; CrsSet(crs);
  OSShell(pgmNm,param,true,true,false,false);
  asm mov ah,3; mov bh,0; push bp; int 10H; pop bp; mov x,dl; mov y,dh end;
  PopW(w); gotoxy(x-WindMin.X+1,y-WindMin.Y+1);
end;
var prFileNr:word;
function OpenMgrOutput:word;
var s:string; h:word;
begin prFileNr:=(prFileNr+1)mod 100; str(prFileNr,s);
  CPath:=replaceNo(prTab(prMgrFileNm),s); CVol:='';
  if length(CPath)=0 then h:=$ff else begin
  h:=OpenH(_isoverwritefile,Exclusive); if HandleError<>0 then
     begin SetMsgPar(CPath);WrLLF10Msg(700+HandleError); h:=$ff end end;
  OpenMgrOutput:=h;
end;
procedure CopyToMgr;
var h1,h2,n:word; buf:pointer; lbuf,sz:longint; s:string; cf,cr:pointer;
begin
  h2:=OpenMgrOutput; if h2=$ff then exit; cf:=CFile; cr:=CRecPtr;
  if printblk then WriteH(h2,nBlk,pBlk^)
  else begin with TextRec(Rprt) do h1:=Handle; SeekH(h1,0);
   lbuf:=1000; buf:=GetStore(lbuf); sz:=FileSizeH(h1);
   while sz>0 do begin
    if sz>lbuf then n:=lbuf else n:=sz; dec(sz,n);
    ReadH(h1,n,buf^); WriteH(h2,n,buf^) end;
   ReleaseStore(buf); CloseH(h1); if h1=WorkHandle then WorkHandle:=$ff;
   with TextRec(Rprt) do Handle:=$ff end;
  CloseH(h2); ExecMgrPgm; CFile:=cf; CRecPtr:=cr;
end;


procedure PrintTxtFBlk(BegPos:longint;CtrlL:boolean);
procedure PrintChar(c:char);
  begin if outpsw then obase.PrintChar(c) end;
procedure PrintStr(s:string);
var i:word;
begin
  for i:=1 to length(s) do PrintChar(s[i]);
end;
  var Ln:string;
procedure NewLine;
  begin PrintStr(^m^j);inc(RprtLine) end;
procedure PrintHeFo(T:string);
  var i:word; m,s:string; point:boolean;
  begin i:=1;while i<=length(T) do
        if T[i]='_' then
           begin m:=''; point:=false;
              while (i<=length(T)) and (T[i] in ['_','.',':']) do
                begin if T[i]<>'_' then point:=true;m:=m+T[i];inc(i) end;
              if point then
                 if m='__.__.__' then PrintStr(StrDate(today,'DD.MM.YY')) else
                 if m='__.__.____' then PrintStr(StrDate(today,'DD.MM.YYYY'))
                 else if m='__:__' then PrintStr(StrDate(currtime,'hh:mm'))
                 else PrintStr(m)
              else begin str(RprtPage:length(m),m);PrintStr(m) end end
        else begin PrintChar(T[i]);inc(i) end;
        NewLine;

  end;
procedure GetNum(var NN:word);
  var i,n:word;
  begin val(leadchar(' ',trailchar(' ',copy(Ln,4,255))),n,i);
        if i=0 then NN:=n end;
function EofInp:boolean;
  begin if printBlk then EofInp:=iBlk>nBlk else EofInp:=Eof(Rprt) end;
procedure RdLnInp;
var c:char; F:TextRec absolute Rprt;
label 1;
begin
  Ln[1]:=' '; if printBlk then begin
    Ln:=''; while iBlk<=nBlk do begin
      c:=pBlk^[iBlk]; inc(iBlk);
      if c=^m then begin
        inc(charrd); if pBlk^[iBlk]=^j then begin inc(iBlk); inc(charrd) end;
        goto 1 end;
      Ln:=Ln+c end;
1:    inc(charrd,length(Ln)); if charrd>BegPos then outpsw:=true end
  else begin
    readln(Rprt,Ln);
    if not outpsw then with F do
      if PosH(Handle)-(BufEnd-BufPos)>=BegPos then outpsw:=true end;
end;
procedure ResetInp;
  begin if printBlk then iBlk:=1 else Seek0Txt(Rprt) end;
var Ti,Times,Cp,Pl,MaxLine:word;
    FrstRun,AutoFF,FFOpt,NMOpt,He,Fo,adj:boolean;
    FoTxt,HeTxt:string; s:string3; er:ExitRecord;
label 1,3;
begin NewExit(Ovr,er);goto 3;  RunMsgOn('P',0);
    FrstRun:=true; outpsw:=false; charrd:=0;
    repeat AutoFF:=false;FFOpt:=false;NMOpt:=false;He:=false;Fo:=false;
      Po:=0;Ti:=1;Cp:=spec.CpLines;Pl:=spec.AutoRprtLimit+Cp;
      ResetInp;
      while not EofInp do
         begin RdLnInp;s:=copy(Ln,1,3);
               if SEquUpcase(s,'.cp') then begin AutoFF:=true;GetNum(Cp) end
               else if SEquUpcase(s,'.pl') then GetNum(Pl)
               else if SEquUpcase(s,'.po') then GetNum(Po)
               else if SEquUpcase(s,'.ti') then GetNum(Ti)
               else if SEquUpcase(s,'.he') then
                    begin He:=true;AutoFF:=true;HeTxt:=copy(Ln,4,255) end
               else if SEquUpcase(s,'.fo') then
                    begin Fo:=true;AutoFF:=true;FoTxt:=copy(Ln,4,255) end
               else if SEquUpcase(s,'.ff') then FFOpt:=true
               else if SEQuUpcase(s,'.nm') then NMOpt:=true
               else goto 1 end;
      goto 3;
    1:adj:=FrstRun and not FFOpt and not NMOpt;
      if adj and spec.ChoosePrMsg then if not PrinterMenu(62) then goto 3;
      if printer[prCurr].ToMgr then begin CopyToMgr; goto 3 end;
      if not ResetPrinter(Pl,Po,adj,FrstRun) then goto 3;
      RprtPage:=1;RprtLine:=1;
      if FrstRun then begin FrstRun:=false; Times:=Ti end;
      if Fo then inc(Cp,2); MaxLine:=Pl-Cp;
      if He then begin PrintHeFo(HeTxt);NewLine end;
      PrintStr(Ln);
      NewLine;
      while not EofInp do
        begin RdLnInp;
              if AutoFF and ((RprtLine>MaxLine) or (Ln[1]=^L)) then
                 begin if Fo then
                       begin while RprtLine<=MaxLine do NewLine; NewLine;
                             PrintHeFo(FoTxt) end;
                       PrintChar(^l);inc(RprtPage);RprtLine:=1;
                       if He then begin PrintHeFo(HeTxt);NewLine end
                 end
              else if Ln[1]=^L then PrintChar(^l);
              if Ln[1]=^L then Ln:=copy(Ln,2,255);
              PrintStr(Ln);NewLine;
              end;
      if Fo then
      begin while RprtLine<=MaxLine do NewLine; NewLine;
            PrintHeFo(FoTxt) end;
      if not FFOpt and CtrlL then
         if PrTab(prClose)<>'ff' then PrintChar(^l); {  Mark*** }
      dec(Times);
    until Times=0;
    ClosePrinter(Po);
  3:RestoreExit(er); RunMsgOff;
  end;
procedure PrintTxtFile(BegPos:longint);
  begin TestMountVol(CPath[1]); if not ResetTxt(Rprt) then
           begin SetMsgPar(CPath);WrLLF10Msg(700+HandleError);exit end;
        printBlk:=false;PrintTxtFBlk(BegPos,true);
        with TextRec(Rprt) do if Handle=$ff then exit; Close(Rprt) end;
procedure PrintArray(P:pointer;N:word;CtrlL:boolean);
begin
  printBlk:=true; pBlk:=CharArrPtr(P); nBlk:=N; PrintTxtFBlk(0,CtrlL);
end;
procedure PrintFandWork;
begin
  CloseH(WorkHandle); Assign(Rprt,FandWorkName); with TextRec(Rprt) do begin
    OpenFunc:=@OpenTxt; OpenWorkH; Handle:=WorkHandle end;
  Reset(Rprt); printBlk:=false; PrintTxtFBlk(0,true);
  if WorkHandle=$ff then OpenWorkH;
end;


end.