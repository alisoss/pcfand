{*****************************************************************************}

procedure Edit;

type ArrLine=array[1..SuccLineSize] of char;
     ArrPtr=^ArrLine;
     Character= record ch:char;color:byte end;
var           {line descriptor  LineI,Posi,BPos}
     Arr:ArrLine;
     NextI:word;
     LineL,ScrL:integer;
     RScrL:longint;
     UpdatedL,CtrlL,HardL:boolean;
              {screen descriptor  ScrI}
     BCol,Colu,Row:word;
     ChangeScr:boolean;
     ColScr:ColorOrd;

     IsWrScreen:boolean;

var FirstR,FirstC,LastR,LastC,MinC,MinR,MaxC,MaxR:word;
    MargLL:array[1..4] of word;
    PageS,LineS:word;

    Scroll,FirstScroll,HelpScroll:boolean;
    PredScLn:longint;PredScPos:word;        {pozice pred Scroll}
    FrameDir:byte;

var WordL:word;       {Mode=HelpM & ctrl-word is on screen}
    Konec:boolean;

                     { GLOBAL EDIT ROUTINES }

procedure DekodLine;forward;

{ **************************** segmenty ******************* }

procedure DelEndT;
  begin if LenT>0 then begin ReleaseStore(@T^[LenT]);dec(LenT) end end;
procedure WrEndT;
  var p:pointer;
  begin p:=GetStore(1);inc(LenT);T^[LenT]:=_CR end;

procedure MoveIdx(dir:integer);
  var mi,ml:word;
  begin mi:=-dir*Part.MovI;ml:=-dir*Part.MovL;
    inc(ScrI,mi);inc(LineI,mi);                {****GLOBAL***}
    inc(NextI,mi);inc(LineL,ml);inc(ScrL,ml);  {****Edit***}
  end;

procedure TestUpdFile; begin DelEndT;if Part.UpdP then UpdateFile  end;
procedure SetUpdat;
  begin UpdatT:=true;
    if TypeT=FileT then
      begin if Part.PosP<$400 then UpdPHead:=true;Part.UpdP:=true end end;
procedure PredPart;
  begin TestUpdFile;ChangePart:=RdPredPart;MoveIdx(-1);WrEndT end;
procedure NextPart;
  begin TestUpdFile;ChangePart:=RdNextPart;MoveIdx(1);WrEndT end;
procedure NextPartDek;
  begin NextPart;DekodLine end;
procedure SetPart(Idx:longint);
  begin
    if (Idx>Part.PosP)and(Idx<Part.PosP+LenT)or(TypeT<>FileT) then exit;
    TestUpdFile;ReleaseStore(@T^);RdFirstPart;
    while (Idx>Part.PosP+Part.LenP)and not AllRd do ChangePart:=RdNextPart;
    WrEndT;
  end;

{procedure SetPredPartLn(Ln:integer);
  begin while (Ln<=Part.LineP)and(Part.PosP>0) do PredPart end;}
procedure SetPartLine(Ln:longint);
  begin
    while (Ln<=Part.LineP)and(Part.PosP>0) do PredPart;
    while (Ln-Part.LineP>$7FFF)and not AllRd do NextPart end;

function LineAbs(Ln:integer):longint;
  begin LineAbs:=Part.LineP+Ln end;
function LineInBlock(Ln:integer):boolean;
  begin if (LineAbs(Ln)>BegBLn)and(LineAbs(Ln)<EndBLn) then
          LineInBlock:=true else LineInBlock:=false end;
function LineBndBlock(Ln:integer):boolean;
  begin if (LineAbs(Ln)=BegBLn)or(LineAbs(Ln)=EndBLn) then
          LineBndBlock:=true else LineBndBlock:=false end;

{ ************************** strankovani ve Scroll ************ }

function NewRL(Line:integer):longint;
{  var nl:longint;
  begin nl:=LineAbs(Line);
    if nl<=PHNum then NewRL:=1
    else dec(nl,PHNum);NewRL:=nl+((nl-1) div PPageS) end;}
  begin NewRL:=LineAbs(Line) end;
function NewL(RLine:longint):integer;
{  begin NewL:=RLine-(RLine div(PPageS+1))+PHNum-Part.LineP end;}
  begin NewL:=RLine-Part.LineP end;
function ModPage(RLine:longint):boolean;
{  begin ModPage:=(RLine mod(PPageS+1))=0 end;}
  begin ModPage:=false end;

procedure TestLenText(F:word;LL:longint);
 procedure SmallerPart(Ind,FreeSize:word);
    var i,il,l:word;lon:longint;
    begin NullChangePart;
      if ((StoreAvail>FreeSize)and(MaxLenT-LenT>FreeSize)) then exit;
      TestUpdFile;WrEndT;
      lon:=MinL(LenT+StoreAvail,MaxLenT);
      dec(lon,FreeSize);if lon<=0 then exit;
      dec(lon,lon shr 3);
      i:=1;il:=0;l:=0;
      while i<Ind  do
        begin
          if T^[i]=_CR then
            begin inc(l);il:=i;if T^[il+1]=_LF then inc(il) end;
          if LenT-il<lon then i:=Ind;inc(i);
        end;
      if il>0 then
        begin with Part do begin inc(PosP,il);inc(LineP,l);MovI:=il;MovL:=l;
                             SetColorOrd(ColorP,1,MovI+1) end;
          dec(LenT,il);move(T^[il+1],T^,LenT);T^[LenT]:=_CR;
          ReleaseStore(@T^[LenT+1]);ChangePart:=true;
          MoveIdx(1);
        end;
      dec(Ind,il);if LenT<lon then exit;
      i:=LenT;il:=LenT;
      while i>Ind do
        begin
          if T^[i]=_CR then
            begin il:=i;if T^[il+1]=_LF then inc(il) end;
          dec(i);
          if il<lon then i:=Ind;
        end;
      if il<LenT then
        begin if il<LenT-1 then AllRd:=false;
          Part.LenP:=il;LenT:=il+1;T^[LenT]:=_CR;ReleaseStore(@T^[LenT+1]);
        end;
    end;

var L:word absolute LL; size:longint;
begin
  size:=LL-F;
  if F<LL then
    begin if TypeT=FileT then
      begin SmallerPart(F,size);dec(F,Part.MovI);dec(L,Part.MovI) end;
      if (StoreAvail<=size)or(MaxLenT<=LenT+size) then RunError(404)
{text prilis dlouhy, nestaci pamet}
      else GetStore(Size);
    end;
  if LenT>=F then move(T^[F],T^[L],succ(LenT-F));
  if F>=LL then ReleaseStore(@T^[LenT+size+1]);
  inc(LenT,size); SetUpdat;
end;

procedure DekodLine;
var LP,LL:word;
begin LL:=1;LP:=FindChar(LL,_CR,LineI,LenT)-LineI;HardL:=true;
      fillchar(Arr,LineSize,32);
      NextI:=LineI+LP+1;
      if (NextI<LenT)and(T^[NextI]=_LF) then inc(NextI) else HardL:=false;
      if LP>LineSize then
        begin LP:=LineSize;
          if Mode=TextM then
            if PromptYN(402) then
              begin LL:=LineI+LineSize; NullChangePart;
                TestLenText(LL,longint(LL)+1);dec(LL,Part.MovI);
                T^[LL]:=_CR;NextI:=LineI+LP+1 end
            else Mode:=ViewM;
{          if Mode=TextM then
            begin WrLLF10Msg(402);LL:=LineI+LineSize;
              NullChangePart;TestLenText(LL,longint(LL)+1);dec(LL,Part.MovI);
              T^[LL]:=_CR;NextI:=LineI+LP+1;
            end;                            }
        end;
      if LP>0 then move(T^[LineI],Arr,LP);
      UpdatedL:=false;
end;

function ShortName(Name:pathstr):pathstr;
var J:word;
    s:pathstr;
begin J:=length(Name);
      while not(Name[J]in['\',':'])and(J>0) do dec(J);
      s:=copy(Name,succ(J),length(Name)-J);
      if Name[2]=':' then s:=copy(Name,1,2)+s;ShortName:=s;
end;

function CountChar(C:char;First,Last:word):word;
  var I,j,n:word;
  begin
    j:=1;I:=FindChar(j,C,First,LenT);n:=0;
    while I<Last do begin inc(n);I:=FindChar(j,C,I+1,LenT) end;
    CountChar:=n;
  end;

function SetLine(Ind:word):word;
  begin SetLine:=CountChar(_CR,1,Ind)+1 end;

function SetCurrI(Ind:word ):word ;
begin SetCurrI:=1;dec(Ind);
      while Ind>0 do begin if T^[Ind]=_CR then
                             begin inc(Ind);if T^[Ind]=_LF then inc(Ind);
                               SetCurrI:=Ind;exit end;
                           dec(Ind) end;
end;

procedure SetDekCurrI(Ind:word);
begin LineI:=SetCurrI(Ind);DekodLine end;

procedure SetDekLnCurrI(Ind:word);
begin SetDekCurrI(Ind);LineL:=SetLine(LineI) end;

function FindLine(var Num:integer):word;
var I,J:word;
label 1;
begin
1:if Num<=0 then if Part.PosP=0 then Num:=1 else begin PredPart;goto 1 end;
  if Num=1 then FindLine:=1
  else
    begin J:=pred(Num);I:=FindChar(J,_CR,1,LenT)+1;
      if T^[I]=_LF then inc(I);
      if I>LenT then
        if AllRd then
          begin Num:=SetLine(LenT);FindLine:=SetCurrI(LenT) end else
          begin NextPart;if Num<>LineL then dec(Num,Part.MovL);goto 1 end
      else FindLine:=I;
    end;
end;

procedure DekFindLine(Num:longint);
begin SetPartLine(Num);LineL:=Num-Part.LineP;LineI:=FindLine(LineL);
  DekodLine end;

procedure PosDekFindLine(Num:longint;Pos:word;ChScr:boolean);
  begin Posi:=Pos;DekFindLine(Num);ChangeScr:=ChangeScr or ChScr end;

{procedure FindDekLine(Num:integer);
begin LineL:=Num;LineI:=FindLine(LineL);DekodLine end;

procedure FindPartDekLine(Num:integer;Pos:word);
  begin Posi:=Pos;SetPredPartLn(Num);FindDekLine(Num-Part.LineP) end;}

function SetInd(Ind:word;Pos:word):word;    { line,pozice --> index}
var P:word;
begin P:=pred(Ind);
      if Ind<LenT then while (Ind-P<Pos)and(T^[Ind]<>_CR) do inc(Ind);
      SetInd:=Ind
end;

function Position(c:word):word;       {PosToCol}
  var p,cc:word;
  begin cc:=1;p:=1;
    while cc<=c do begin if Arr[p]>=' 'then inc(cc);inc(p) end;
    Position:=p-1;
  end;
function Column(p:word):word;         {ColToPos}
  var c,pp:word;
  begin if p=0 then begin Column:=0;exit end;
    pp:=1;c:=1;
    while pp<=p do begin if Arr[pp]>=' 'then inc(c);inc(pp) end;
    if Arr[p]>=' ' then dec(c);Column:=c;
  end;

procedure SetScreen(Ind,ScrXY,Pos:word);
var X:word;
    rl:longint;
begin SetDekLnCurrI(Ind);Posi:=MinI(LineSize,MaxI(MaxW(1,Pos),Ind-LineI+1));
      if ScrXY>0 then
        begin ScrL:=LineL-(ScrXY shr 8)+1;
          Posi:=MaxW(Posi,ScrXY and $00FF);BPos:=Posi-(ScrXY and $00FF);
          ChangeScr:=true end;
      Colu:=Column(Posi);BCol:=Column(BPos);
      if Scroll then
        begin
          RScrL:=NewRL(ScrL);
          LineL:=MaxI(PHNum+1,LineAbs(LineL))-Part.LineP;rl:=NewRL(LineL);
          if (rl>=RScrL+PageS)or(rl<RScrL) then
            begin if rl>10 then RScrL:=rl-10 else RScrL:=1;
              ChangeScr:=true;ScrL:=NewL(RScrL) end;
          LineL:=ScrL;DekFindLine(LineAbs(LineL));
        end else
        if (LineL>=ScrL+PageS)or(LineL<ScrL) then
          begin if LineL>10 then ScrL:=LineL-10 else ScrL:=1;
            ChangeScr:=true; end;
end;

function LastPosLine:word;
var LP:word;
begin LP:=LineSize;while (LP>0)and(Arr[LP]=' ') do dec(LP);
  LastPosLine:=LP;
end;

procedure KodLine;
var LP:word;
begin LP:=LastPosLine+1;
      if HardL then inc(LP);TestLenText(NextI,longint(LineI)+LP);
      move(Arr,T^[LineI],LP);NextI:=LineI+LP;LP:=NextI-1;
      if HardL then dec(LP);T^[LP]:=_CR;if HardL then T^[LP+1]:=_LF;
{      if NextI>LenT then LenT:=LP else}
      UpdatedL:=false;
end;

procedure TestKod;
begin if UpdatedL then KodLine end;

procedure NextLine(WrScr:boolean);
  procedure MyWriteln;
    begin textattr:=TxtColor;writeln end;
  var b:boolean;
  begin TestKod;
    if (NextI>=LenT)and not AllRd then NextPartDek;
    if NextI<=LenT then
      begin LineI:=NextI;DekodLine;inc(LineL);
        if Scroll then
          begin if PageS>1 then MyWriteln;inc(ScrL);ChangeScr:=true;
            inc(RScrL);
            if ModPage(RScrL) then
              begin if PageS>1 then MyWriteln;{inc(RLineL);}inc(RScrL) end;
          end else
        if WrScr and(LineL=ScrL+PageS) then
          begin if PageS>1 then MyWriteln;inc(ScrL);ChangeScr:=true end;
      end;
  end;

{*********************** HELP **************************************}

function WordExist:boolean;
  begin WordExist:=(WordL>=ScrL)and(WordL<ScrL+PageS) end;

function WordNo(I:word):word;
  begin WordNo:=(CountChar(^s,1,MinW(LenT,I-1))+1)div 2 end;

function WordNo2:word;
  begin if WordExist then WordNo2:=WordNo(SetInd(LineI,Posi))
        else WordNo2:=WordNo(ScrI);
  end;

procedure ClrWord;
  var k,m:word;
  begin m:=1;k:=1;k:=FindChar(m,^q,k,LenT);
    while k<LenT do begin T^[k]:=^s;m:=1;k:=FindChar(m,^q,k,LenT) end;
  end;

function WordFind(i:word;var WB,WE,LI:word):boolean;
  var k:word;
  begin WordFind:=false;if i=0 then exit;
    i:=i*2-1;
    k:=FindChar(i,^s,1,LenT);
    if k>=LenT then exit;WB:=k;inc(k);
    while T^[k]<>^s do inc(k);if k>=LenT then exit;
    WE:=k;LI:=SetLine(WB);WordFind:=true;
  end;

procedure SetWord(WB,WE:word);
  begin T^[WB]:=^q;T^[WE]:=^q;SetDekLnCurrI(WB);
    WordL:=LineL;Posi:=WB-LineI+1;Colu:=Column(Posi) end;

procedure CursorWord;
  var O:set of char;
      pp:word;
  begin
    LexWord:=''; pp:=Posi;
    if Mode=HelpM then O:=[^q]
    else begin O:=Oddel;if Arr[pp] in O then dec(pp) end;
    while (pp>0)and not (Arr[pp] in O) do dec(pp);inc(pp);
    while (pp<=LastPosLine)and not(Arr[pp] in O) do
      begin LexWord:=LexWord+Arr[pp];inc(pp) end;
  end;

{procedure RdPrintHead;
  var ln:string;
  procedure GetNum(var NN:word);
    var i,n:word;
    begin val(leadchar(' ',trailchar(' ',copy(ln,4,255))),n,i);
          if i=0 then NN:=n end;
  var I1,I2:integer;
      s:String3;
      hefo,cp,pl:word;
  begin I1:=LineAbs(ScrL);I2:=LineAbs(LineL);
    SetPart(1);SetDekLnCurrI(1);ScrL:=1;
    PHNum:=0;cp:=3;pl:=72;hefo:=0;s:=' ';
    while s<>'' do
      begin ln[0]:=chr(LastPosLine);move(Arr,ln[1],length(ln));
        s:=copy(ln,1,3);
        if SEquUpcase(s,'.cp') then begin inc(PHNum);GetNum(cp) end else
        if SEquUpcase(s,'.pl') then begin inc(PHNum);GetNum(pl) end else
        if SEquUpcase(s,'.po')or SEquUpcase(s,'.ti')or SEquUpcase(s,'.ff')
           then inc(PHNum) else
        if SEquUpcase(s,'.he')or SEquUpcase(s,'.fo') then
          begin inc(PHNum);inc(hefo,2) end else s:='';
        NextLine(false);
      end;
    if PHNum=0 then PPageS:=$7FFF else PPageS:=pl-cp-hefo;
    ScrL:=MaxI(I1,PHNum+1);LineL:=ScrL;LineI:=FindLine(LineL);
    RScrL:=NewRL(ScrL);RLineL:=RScrL;
    if I1<>ScrL then ChangeScr:=true;DekodLine;
  end;  }


{$I edscreen.pas}
{$I edevent.pas}

           { BEGIN OF Edit }

var LockPtr:^byte;

var Regs:registers;
    i1,i2,i3:word;

begin
  InitScr;IsWrScreen:=false;
  WrEndT;IndT:=MinW(MaxW(1,IndT),LenT);
  BegBLn:=1;EndBLn:=1;BegBPos:=1;EndBPos:=1;
  ScrL:=1;ScrI:=1;RscrL:=1;PredScLn:=1;PredScPos:=1;UpdPHead:=false;
  if TypeT<>FileT then with Part do
    begin AllRd:=true;AbsLenT:=LenT-1;LineP:=0;PosP:=0;LenP:=AbsLenT;
      ColorP:='';UpdP:=false;NullChangePart;SimplePrintHead end;
  LockPtr:=ptr(0,$417);FirstScroll:=Mode=ViewM;
  Scroll:=(((LockPtr^ and $10)<>0)or FirstScroll)and(Mode<>HelpM);
  if Scroll then begin ScrL:=NewL(RScrL);ChangeScr:=true end;
  HelpScroll:=Scroll or(Mode=HelpM);if HelpScroll then CrsHide else CrsNorm;
  BCol:=0;BPos:=0;SetScreen(IndT,ScrT,Posi);Konec:=false;
  if Mode=HelpM then
    begin WordL:=0;ScrI:=SetInd(LineI,Posi);
      if WordFind(WordNo2+1,i1,i2,i3) then SetWord(i1,i2);
      if not WordExist then SetDekLnCurrI(IndT);ScrI:=1 end;
  fillchar(MargLL,sizeof(MargLL),0);ColScr:=Part.ColorP;
  WrStatusLine;
  textattr:=TxtColor;clrscr;Background;FirstEvent:=false;
{!!!!!!!!!!!!!!}
  if ErrMsg<>'' then begin
    SetMsgPar(ErrMsg); F10SpecKey:=$ffff; WrLLF10Msg(110);
    ClearKbdBuf; AddToKbdBuf(KbdChar) end;
  fillchar(MargLL,sizeof(MargLL),0);
  WrLLMargMsg(LastS,LastNr);
  repeat if TypeT=FileT then NullChangePart;HandleEvent;
    if not(Konec or IsWrScreen) then Background;
  until Konec;
  if Scroll and(Mode<>HelpM) then
    begin Posi:=BPos+1;LineL:=ScrL;LineI:=ScrI end;
  IndT:=SetInd(LineI,Posi);ScrT:=((LineL-ScrL+1)shl 8)+Posi-BPos;
  if Mode<>HelpM then TxtXY:=ScrT+longint(Posi)shl 16;
  CursorWord;if Mode=HelpM then ClrWord;
  CrsHide;window(MinC,MinR,MaxC,MaxR);
  TestUpdFile;
end;

