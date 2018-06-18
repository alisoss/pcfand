{procedure NextLine in EDEDIT - use in MyTestEvent}

procedure MyInsLine;
  begin textattr:=TxtColor;insline end;

procedure MyDelLine;
  begin textattr:=TxtColor;delline end;

procedure PredLine;
var mi,ml:word;
begin TestKod;
      if (LineL=1)and(Part.PosP>0) then PredPart;
      if LineL>1 then
        begin if T^[LineI-1]=_LF then SetDekCurrI(LineI-2)
              else SetDekCurrI(LineI-1);dec(LineL);
          if LineL<ScrL then
            begin gotoxy(1,1);MyInsLine;dec(ScrL);ChangeScr:=true;
              if Scroll then
                begin {dec(RLineL);}dec(RScrL);
                  {if ModPage(RLineL) then}
                  if ModPage(RScrL) then
                    begin gotoxy(1,1);MyInsLine;{dec(RLineL);}dec(RScrL) end;
                end;
            end;
        end
end;

procedure RollNext;
  begin if (NextI>=LenT)and not AllRd then NextPartDek;
    if NextI<=LenT then
          begin gotoxy(1,1);MyDelLine;inc(ScrL);ChangeScr:=true;
            if LineL<ScrL then
              begin TestKod;inc(LineL);LineI:=NextI;DekodLine end;
          end;
  end;

procedure RollPred;
  begin if (ScrL=1)and(Part.PosP>0) then PredPart;
    if ScrL>1 then
      begin gotoxy(1,1);MyInsLine;dec(ScrL);ChangeScr:=true;
        if LineL=ScrL+PageS then
          begin TestKod;dec(LineL);
            if T^[LineI-1]=_LF then SetDekCurrI(LineI-2)
            else SetDekCurrI(LineI-1) end;
      end;
  end;

procedure Frame;
  const FrameString:string[15]=
        #80#72#179#77#218#192#195#75#191#217#180#196#194#193#197;
        {       ³      Ú   À   Ã      ¿   Ù   ´   Ä   Â   Á   Å }
        FS1:string[15]=
        #80#72#186#77#214#211#199#75#183#189#182#196#210#208#215;
        {       º      Ö   Ó   Ç      ·   ½   ¶   Ä   Ò   Ð   × }
        FS2:string[15]=
        #80#72#179#77#213#212#198#75#184#190#181#205#209#207#216;
        {       ³      Õ   Ô   Æ      ¸   ¾   µ   Í   Ñ   Ï   Ø }
        FS3:string[15]=
        #80#72#186#77#201#200#204#75#187#188#185#205#203#202#206;
        {       º      É   È   Ì      »   ¼   ¹   Í   Ë   Ê   Î }
  var oldzn:char;dir,odir,zn1,zn2,b:byte;
  procedure direction(x:byte);var y:byte;
    begin y:=$10;if x>2 then y:=y shl 1;if x=0 then y:=0;
      if Mode=DouFM then zn2:=zn2 or y else zn2:=zn2 and not y end;
  begin
    UpdStatLine(LineL,Posi);CrsBig;odir:=0;
    ClrEvent;
    while true do with Event do
      begin
        if not MyGetEvent or
           ((What=evKeyDown)and(KeyCode=_ESC_))or(What<>evKeyDown)
           then begin ClrEvent;CrsNorm;Mode:=TextM;exit end;
        case Event.KeyCode of
                  _frmsin_:Mode:=SinFM;
                  _frmdoub_:Mode:=DouFM;
                  _dfrm_:Mode:=DelFM;
                  _nfrm_:Mode:=NotFM;
                  _left_,_right_,_up_,_down_:
        if not Scroll then begin
          FrameString[0]:=chr(63);
          zn1:=pos(Arr[Posi],FrameString);zn2:=zn1 and $30; zn1:=zn1 and $0F;
          dir:=pos(chr(hi(KeyCode)),FrameString);if (dir+odir) in[2,4,8,16] then odir:=0;
          if zn1 in[1,2,4,8] then zn1:=0;oldzn:=Arr[Posi];Arr[Posi]:=' ';
          if Mode=DelFM then b:=zn1 and not(odir or dir)
          else b:=zn1 or(odir xor dir);
          if b in[1,2,4,8] then b:=0;
          if (Mode=DelFM)and(zn1<>0)and(b=0) then oldzn:=' ';
          direction(dir);direction(odir);
          if Mode=NotFM then b:=0;
          if (b<>0)and((Event.KeyCode=_left_)or(Event.KeyCode=_right_)or
             (Event.KeyCode=_up_)or(Event.KeyCode=_down_)) then
            Arr[Posi]:=FrameString[zn2+b] else Arr[Posi]:=oldzn;
          if (dir=1)or(dir=4) then odir:=dir*2 else odir:=dir div 2;
          if Mode=NotFM then odir:=0 else UpdatedL:=true;
          case Event.KeyCode of _left_:if Posi>1 then dec(Posi);
                    _right_:if Posi<LineSize then inc(Posi);
                    _up_:PredLine;
                    _down_:NextLine(true);
                    end;
        end;
        end;
        ClrEvent;
        UpdStatLine(LineL,Posi);{if not MyTestEvent then }Background;
      end;
  end;

procedure CleanFrameM;
  begin if Mode in[SinFM,DouFM,DelFM,NotFM] then with Event do
    if not MyGetEvent or
       ((What=evKeyDown)and(KeyCode=_ESC_))or(What<>evKeyDown)
       then begin ClrEvent;CrsNorm;Mode:=TextM;UpdStatLine(LineL,Posi);exit end;
  end;

procedure FrameStep(var odir:byte;EvKeyC:word);
  const FrameString:string[15]=
        #80#72#179#77#218#192#195#75#191#217#180#196#194#193#197;
        {       ³      Ú   À   Ã      ¿   Ù   ´   Ä   Â   Á   Å }
        FS1:string[15]=
        #80#72#186#77#214#211#199#75#183#189#182#196#210#208#215;
        {       º      Ö   Ó   Ç      ·   ½   ¶   Ä   Ò   Ð   × }
        FS2:string[15]=
        #80#72#179#77#213#212#198#75#184#190#181#205#209#207#216;
        {       ³      Õ   Ô   Æ      ¸   ¾   µ   Í   Ñ   Ï   Ø }
        FS3:string[15]=
        #80#72#186#77#201#200#204#75#187#188#185#205#203#202#206;
        {       º      É   È   Ì      »   ¼   ¹   Í   Ë   Ê   Î }
  var oldzn:char;dir,zn1,zn2,b:byte;
  procedure direction(x:byte);var y:byte;
    begin y:=$10;if x>2 then y:=y shl 1;if x=0 then y:=0;
      if Mode=DouFM then zn2:=zn2 or y else zn2:=zn2 and not y end;
  begin
    case EvKeyC of
              _frmsin_:Mode:=SinFM;
              _frmdoub_:Mode:=DouFM;
              _dfrm_:Mode:=DelFM;
              _nfrm_:Mode:=NotFM;
              _left_,_right_,_up_,_down_:
    begin
      FrameString[0]:=chr(63);
      zn1:=pos(Arr[Posi],FrameString);zn2:=zn1 and $30; zn1:=zn1 and $0F;
      dir:=pos(chr(hi(EvKeyC)),FrameString);
      if (dir+odir) in[2,4,8,16] then odir:=0;
      if zn1 in[1,2,4,8] then zn1:=0;oldzn:=Arr[Posi];Arr[Posi]:=' ';
      if Mode=DelFM then b:=zn1 and not(odir or dir)
      else b:=zn1 or(odir xor dir);
      if b in[1,2,4,8] then b:=0;
      if (Mode=DelFM)and(zn1<>0)and(b=0) then oldzn:=' ';
      direction(dir);direction(odir);
      if Mode=NotFM then b:=0;
      if (b<>0)and((Event.KeyCode=_left_)or(Event.KeyCode=_right_)or
         (Event.KeyCode=_up_)or(Event.KeyCode=_down_)) then
        Arr[Posi]:=FrameString[zn2+b] else Arr[Posi]:=oldzn;
      if (dir=1)or(dir=4) then odir:=dir*2 else odir:=dir div 2;
      if Mode=NotFM then odir:=0 else UpdatedL:=true;
      case Event.KeyCode of _left_:if Posi>1 then dec(Posi);
                _right_:if Posi<LineSize then inc(Posi);
                _up_:PredLine;
                _down_:NextLine(true);
                end;
    end;end;
    UpdStatLine(LineL,Posi);
  end;

function TestLastPos(F,T:word):boolean;
  procedure MoveB(var B:word);
    begin if F<=T then begin if B>F then inc(B,T-F) end
          else if B>=F then dec(B,F-T)
               else if B>T then B:=T ;B:=MinW(B,LastPosLine+1); end;
  var LP:word;
  begin LP:=LastPosLine;if F>LP then F:=LP+1;
        if LP+T-F<=LineSize then
          begin if LP>=F then move(Arr[F],Arr[T],succ(LP-F));
            if TypeB=TextBlock then
              begin if LineAbs(LineL)=BegBLn then MoveB(BegBPos);
                    if LineAbs(LineL)=EndBLn then MoveB(EndBPos) end;
            if F>T then
              begin if T<=LP then fillchar(Arr[LP+1+T-F],F-T,32) end;
            UpdatedL:=true;TestLastPos:=true end
        else TestLastPos:=false;
  end;

procedure DelChar;
var LP:word;
begin if TestLastPos(succ(Posi),Posi) then end;

procedure FillBlank;
  var I:word;
  begin KodLine;I:=LastPosLine;
    if Posi>I+1 then begin TestLenText(LineI+I,longint(LineI)+Posi-1);
            fillchar(T^[LineI+I],Posi-I-1,32);inc(NextI,Posi-I-1) end;
  end;

procedure DeleteL;
var B:boolean;
begin FillBlank;
      if LineAbs(LineL)+1<=BegBLn then
        begin dec(BegBLn);
          if (LineAbs(LineL)=BegBLn)and(TypeB=TextBlock) then
            inc(BegBPos,LastPosLine) end;
      if LineAbs(LineL)+1<=EndBLn then
        begin dec(EndBLn);
          if (LineAbs(LineL)=EndBLn)and(TypeB=TextBlock) then
            inc(EndBPos,LastPosLine) end;
      if (NextI>=LenT)and not AllRd then NextPartDek;
      if NextI<=LenT then
        if T^[NextI-1]=_LF then TestLenText(NextI,NextI-2)
        else TestLenText(NextI,NextI-1);
      DekodLine;
end;

procedure NewLine(Mode:char);
var LP:word;
begin KodLine;LP:=LineI+MinI(LastPosLine,Posi-1);
  NullChangePart;TestLenText(LP,longint(LP)+2);dec(LP,Part.MovI);
  if LineAbs(LineL)<=BegBLn then
    if LineAbs(LineL)<BegBLn then inc(BegBLn)
    else if (BegBPos>Posi)and(TypeB=TextBlock) then
           begin inc(BegBLn);dec(BegBPos,Posi-1) end;
  if LineAbs(LineL)<=EndBLn then
    if LineAbs(LineL)<EndBLn then inc(EndBLn)
    else if (EndBPos>Posi)and(TypeB=TextBlock) then
           begin inc(EndBLn);dec(EndBPos,Posi-1) end;
  T^[LP]:=_CR;T^[succ(LP)]:=_LF;
  if Mode='m' then begin inc(LineL);LineI:=LP+2 end;
  DekodLine;
end;

function SetPredI:word;
begin if (LineL=1)and(Part.PosP>0) then PredPart;
  if LineI<=1 then SetPredI:=LineI else
  if T^[LineI-1]=_LF then SetPredI:=SetCurrI(LineI-2)
  else SetPredI:=SetCurrI(LineI-1);
end;

procedure WrChar(Ch:char);
begin if Insert then
        begin if TestLastPos(Posi,succ(Posi)) then
                begin Arr[Posi]:=Ch;if Posi<LineSize then inc(Posi) end end
      else begin Arr[Posi]:=Ch;UpdatedL:=true;
             if Posi<LineSize then inc(Posi) end;
end;

procedure Format(First,Last:longint;Posit:word;Rep:boolean);
  var fst,lst,ii1:word;
      ii:integer;
      llst:longint;
      A:array[1..260] of char;
      bool:boolean;
      rp,nb,nw,n:word;
      RelPos:word;
  begin
    SetPart(First);fst:=First-Part.PosP;llst:=Last-Part.PosP;
    if llst>LenT then lst:=LenT else lst:=llst;
    repeat
      if LenT>$400 then ii1:=LenT-$400 else ii1:=0;
      if (fst>=ii1)and not AllRd then
        begin NextPartDek;
          dec(fst,Part.MovI);dec(lst,Part.MovI);dec(llst,Part.MovI);
          if llst>LenT then lst:=LenT else lst:=llst end;
      I:=fst;ii1:=I;
      if (I<2)or(T^[I-1]=_LF) then
        begin while T^[ii1]=' ' do inc(ii1);Posit:=MaxW(Posit,ii1-I+1) end;
      ii1:=I;RelPos:=1;
      if Posit>1 then
        begin move(T^[I],A,Posit);
          for ii:=1 to Posit-1 do
             begin if pos(T^[I],CtrlKey)=0  then inc(RelPos);
               if T^[I]=_CR then A[ii]:=' ' else inc(I); end;
          if (T^[I]=' ')and(A[Posit-1]<>' ') then
            begin inc(Posit);inc(RelPos) end;
        end;
      while I<lst do
        begin bool:=true;nw:=0;nb:=0;
          if RelPos<LeftMarg then
            if (Posit=1)or(A[Posit-1]=' ')  then
              begin ii:=LeftMarg-RelPos;fillchar(A[Posit],ii,32);
                inc(Posit,ii);RelPos:=LeftMarg end
            else while RelPos<LeftMarg do
                   begin inc(Posit);if pos(T^[I],CtrlKey)=0 then inc(RelPos);
                     if T^[I]<>_CR then inc(I);
                     if T^[I]=_CR then A[Posit]:=' ' else A[Posit]:=T^[I];
                   end;
          while (RelPos<=RightMarg)and(I<lst) do
            begin
              if (T^[I]=_CR)or(T^[I]=' ') then
                begin while ((T^[I]=_CR)or(T^[I]=' '))and(I<lst) do
                        if T^[I+1]=_LF then lst:=I
                        else begin T^[I]:=' ';inc(I) end;
                  if not bool then begin inc(nw);if I<lst then dec(I) end end;
              if I<lst then
                begin bool:=false;A[Posit]:=T^[I];
                  if pos(A[Posit],CtrlKey)=0 then inc(RelPos);
                  inc(I);inc(Posit) end
            end;
          if (I<lst)and(T^[I]<>' ')and(T^[I]<>_CR) then
            begin ii:=Posit-1;if pos(A[ii],CtrlKey)<>0 then dec(ii);
              rp:=RelPos;dec(RelPos);
              while (A[ii]<>' ')and(ii>LeftMarg) do
                begin if pos(A[ii],CtrlKey)=0 then dec(RelPos);dec(ii) end;
              if RelPos>LeftMarg then
                begin nb:=rp-RelPos;dec(I,Posit-ii);Posit:=ii end
              else
                begin while (T^[I]<>' ')and(T^[I]<>_CR)and(Posit<LineSize) do
                       begin A[Posit]:=T^[I];inc(I);inc(Posit) end;
                  while ((T^[I]=_CR)or(T^[I]=' '))and(I<lst) do
                    if T^[I+1]=_LF then lst:=I
                    else begin T^[I]:=' ';inc(I) end;
               end;
            end;
          if Just then
            begin ii:=LeftMarg;
              while (nb>0)and(nw>1) do
                begin while A[ii]=' ' do inc(ii);while A[ii]<>' ' do inc(ii);
                  dec(nw);n:=nb div nw;
                  if (nw mod nb<>0)and odd(nw)and(nb>n) then inc(n);
                  if Posit-ii>0 then move(A[ii],A[ii+n],Posit-ii+1);
                  fillchar(A[ii],n,32);inc(Posit,n);
                  dec(nb,n);
                end end;
          ii:=1;while A[ii]=' ' do inc(ii);if ii>=Posit then Posit:=1;
          if I<lst then A[Posit]:=_CR else dec(Posit);
          TestLenText(I,longint(ii1)+Posit);
          if Posit>0 then move(A,T^[ii1],Posit);
          ii:=ii1+Posit-I;I:=ii1+Posit;inc(lst,ii);inc(llst,ii);
          Posit:=1;RelPos:=1;ii1:=I;
        end;
      if Rep then
        begin while (T^[I]=_CR)or(T^[I]=_LF) do inc(I);fst:=I;Rep:=I<llst;
          if llst>LenT then lst:=LenT else lst:=llst end;
    until not Rep;
    BegBLn:=1;BegBPos:=1;EndBLn:=1;EndBPos:=1;TypeB:=TextBlock;
  end;

procedure Calculate;
  var Z:FrmlPtr; Txt:string; er:ExitRecord; I:word; Msg:ScreenStr;
      p:pointer; FTyp:char; R:float; Del:boolean;
  label 0,1,2,3,4;
  begin
    MarkStore(p); NewExit(Ovr,er);goto 2; ResetCompilePars;
    RdFldNameFrml:=RdFldNameFrmlT;
    0:Txt:=CalcTxt; Del:=true; I:=1;
    1:TxtEdCtrlUBrk:=true; TxtEdCtrlF4Brk:=true;
    PromptLL(114,Txt,I,Del); if KbdChar=_U_ then goto 0;
    if (KbdChar=_ESC_) or (length(Txt)=0) then goto 3;
    CalcTxt:=Txt;
    if (KbdChar=_CtrlF4_)and(Mode=TextM)and not Scroll then
      begin if length(Txt)>LineSize-LastPosLine then
              begin I:=LineSize-LastPosLine;WrLLF10Msg(419);goto 1 end;
        if Posi<=LastPosLine then TestLastPos(Posi,Posi+length(Txt));
        move(Txt[1],Arr[Posi],length(Txt));UpdatedL:=true;goto 3 end;
    SetInpStr(Txt); RdLex; Z:=RdFrml(FTyp); if Lexem<>^z then Error(21);
    case FTyp of
      'R':begin R:=RunReal(Z);str(R:30:10,Txt);
            Txt:=leadchar(' ',trailchar('0',Txt));
            if Txt[length(Txt)]='.' then dec(Txt[0]) end;
      'S':Txt:=RunShortStr(Z);   { wie RdMode fuer T ??}
      'B':if RunBool(Z) then Txt:=AbbrYes else Txt:=AbbrNo
    end;
    I:=1; goto 1;
    2:Msg:=MsgLine; I:=CurrPos; SetMsgPar(Msg); WrLLF10Msg(110);
    IsCompileErr:=false; ReleaseStore(p); Del:=false; goto 1;
    3:ReleaseStore(p); RestoreExit(er);
  end;

{ ************************* BLOCK *******************************}

function BlockExist:boolean;
begin if TypeB=TextBlock then
        BlockExist:=(BegBLn<EndBLn)or(BegBLn=EndBLn)and(BegBPos<EndBPos)
      else BlockExist:=(BegBLn<=EndBLn)and(BegBPos<EndBPos)   end;

procedure SetBlockBound(var BBPos,EBPos:longint);
  var i:integer;
  begin SetPartLine(EndBLn);i:=EndBLn-Part.LineP;
    EBPos:=SetInd(FindLine(i),EndBPos)+Part.PosP;
    SetPartLine(BegBLn);i:=BegBLn-Part.LineP;
    BBPos:=SetInd(FindLine(i),BegBPos)+Part.PosP;
  end;

function BlockHandle(Oper:char):boolean;
  var i,I1,I2,Ps:word;
      LL1,LL2,Ln:longint;
      a:ArrLine;
      co:colorOrd;isPrintFile:boolean;p:CharArrPtr;
      tb:boolean;c:char;
  procedure ResetPrint(LenPrint:longint);
    begin co:=Part.ColorP;SetColorOrd(co,1,I1);isPrintFile:=false;
      fs:=length(co);inc(LenPrint,fs);
      if Oper='p' then inc(LenPrint);
      if (StoreAvail>LenPrint)and(LenPrint<$FFF0) then
        begin p:=GetStore2(LenPrint);move(co[1],p^,length(co)) end else
        begin isPrintFile:=true;W1:=WorkHandle;SeekH(W1,0);
          WriteH(W1,length(co),co[1]);HMsgExit(CPath);
        end end;
  procedure LowCase(var c:char);
    var i:char;
    begin if (c>='A')and(c<='Z') then begin c:=chr(ord(c)+$20);exit end;
      for i:=#128 to #255 do
        if (UpcCharTab[i]=c)and(i<>c) then begin c:=i; exit end;
    end;

  begin TestKod;Ln:=LineAbs(LineL);Ps:=Posi;
    if Oper='p' then begin tb:=TypeB;TypeB:=TextBlock end else
    if not BlockExist then begin BlockHandle:=false;exit end;
    CrsHide;BlockHandle:=true;
    if TypeB=TextBlock then
      begin
        if Oper='p' then
          begin LL2:=AbsLenT-Part.LenP+LenT;LL1:=Part.PosP+SetInd(LineI,Posi) end
        else SetBlockBound(LL1,LL2);      I1:=LL1-Part.PosP;
        if upcase( Oper)='P' then ResetPrint(LL2-LL1);
        repeat if LL2>Part.PosP+LenT then I2:=LenT
               else I2:=LL2-Part.PosP;
          case Oper of
          'Y': begin TestLenText(I2,I1);dec(LL2,I2-I1) end;
          'U': begin for i:=I1 to I2-1 do T^[i]:=UpcCharTab[T^[i]];
                 inc(LL1,I2-I1) end;
          'L': begin for i:=I1 to I2-1 do LowCase(T^[i]);inc(LL1,I2-I1) end;
          'p','P': begin
                 if isPrintFile then
                   begin WriteH(W1,I2-I1,T^[I1]);HMsgExit(CPath) end else
                   move(T^[I1],p^[fs+1],I2-I1);
                 inc(fs,I2-I1);inc(LL1,I2-I1) end;
          'W': begin SeekH(W1,fs); WriteH(W1,I2-I1,T^[I1]); HMsgExit(CPath);
                 inc(fs,I2-I1);inc(LL1,I2-I1) end;
          end;
          if Oper in['U','L','Y'] then SetUpdat;
          if (Oper='p')and AllRd then LL1:=LL2;
          if not AllRd and(LL1<LL2) then
            begin I1:=LenT;NextPart;dec(I1,Part.MovI);end;
        until LL1=LL2;
      end else              {ColBlock}
      begin PosDekFindLine(BegBLn,BegBPos,false);
        I1:=EndBPos-BegBPos;LL1:=(EndBLn-BegBLn+1)*(I1+2);LL2:=0;
        if Oper='P' then ResetPrint(LL1);
        repeat
          case Oper of
          'Y': TestLastPos(EndBPos,BegBPos);
          'U': begin for i:=BegBPos to EndBPos-1 do
                 Arr[i]:=UpcCharTab[Arr[i]];UpdatedL:=true end;
          'L': begin for i:=BegBPos to EndBPos-1 do LowCase(Arr[i]);
                 UpdatedL:=true end;
          'W','P':
            begin move(Arr[BegBPos],a,I1);a[I1+1]:=_CR;a[I1+2]:=_LF;
              if (Oper='P')and not isPrintFile then move(a,p^[fs+1],I1+2)
              else begin WriteH(W1,I1+2,a);HMsgExit(CPath) end;
              inc(fs,I1+2);
            end;
          end;
          inc(LL2,I1+2);NextLine(false);
        until LL2=LL1
      end;
    if upcase(Oper)='P' then
      if isPrintFile then
        begin WriteH(W1,0,T^);{truncH}PrintFandWork end else
        begin PrintArray(p,fs,false); ReleaseStore2(p) end;
    if Oper='p' then TypeB:=tb;
    if Oper='Y' then PosDekFindLine(BegBLn,BegBPos,true)
    else begin if Oper='p' then SetPart(1);PosDekFindLine(Ln,Ps,true) end;
    if not Scroll then CrsShow;
  end;

procedure DelStorClpBd;
  begin TWork.Delete(ClpBdPos); ClpBdPos:=TWork.Store(sp);
    ReleaseStore2(P1) end;
procedure MarkRdClpBd;
  begin MarkStore2(P1);sp:=TWork.Read(2,ClpBdPos);end;

function BlockGrasp(Oper:char):boolean;
  procedure MovePart(Ind:word);
    begin if TypeT<>FileT then exit;TestUpdFile;WrEndT;
      with Part do
        begin MovI:=SetCurrI(Ind)-1;MovL:=SetLine(MovI)-1;
          inc(LineP,MovL);inc(PosP,MovI);dec(LenP,MovI);
          SetColorOrd(ColorP,1,MovI+1);
          TestLenText(MovI+1,1);ChangePart:=true;
        end;
    end;
  var L,L1,L2,ln:longint;
      I1:word;
  begin BlockGrasp:=false;
    if not BlockExist then exit;L:=Part.PosP+LineI+Posi-1;
    ln:=LineAbs(LineL);if Oper='G' then TestKod;
    SetBlockBound(L1,L2);if (L>L1)and(L<L2)and(Oper<>'G') then exit;
    L:=L2-L1; if L>$7FFF then begin WrLLF10Msg(418);exit end;
    if L2>Part.PosP+LenT then MovePart(L1-Part.PosP);
    I1:=L1-Part.PosP;
    MarkStore2(P1);sp:=GetStore2(L+2);sp^.LL:=L; move(T^[I1],sp^.A,L);
    if Oper='M' then
      begin TestLenText(I1+L,I1);
     {   if L1>Part.PosP+I1 then dec(L1,L);}
        if EndBLn<=ln then
          begin if (EndBLn=ln)and(Posi>=EndBPos) then
                  Posi:=BegBPos+Posi-EndBPos;
            dec(ln,EndBLn-BegBLn);
          end;

      end;
    if Oper='G' then DelStorClpBd;PosDekFindLine(ln,Posi,false);
    BlockGrasp:=true;
  end;
procedure BlockDrop(Oper:char);
  var I,I2:word;
  begin if Oper='D' then MarkRdClpBd;if sp^.LL=0 then exit;
                                     { hlidani sp^.LL a StoreAvail,MaxLenT
                                       dela TestLenText,prip.SmallerPart}
    if Oper='D' then FillBlank; I:=LineI+Posi-1; I2:=sp^.LL;
    BegBLn:=LineAbs(LineL);BegBPos:=Posi;
    NullChangePart;TestLenText(I,longint(I)+I2);
    if ChangePart then dec(I,Part.MovI);
    move(sp^.A,T^[I],I2); ReleaseStore2(P1);
    SetDekLnCurrI(I+I2);EndBLn:=Part.LineP+LineL;EndBPos:=succ(I+I2-LineI);
    PosDekFindLine(BegBLn,BegBPos,true){;ChangeScr:=true}
  end;

function BlockCGrasp(Oper:char):boolean;
  var i,I2:word;
      l1,L:longint;
      a:ArrLine;
  begin BlockCGrasp:=false;if not BlockExist then exit;TestKod;

    L:=LineAbs(LineL);
    if (L in[BegBLn..EndBLn])and(Posi in[BegBPos+1..EndBPos-1])and(Oper<>'G')
      then exit;
    l1:=(EndBLn-BegBLn+1)*(EndBPos-BegBPos+2);
    if l1>$7FFF then begin WrLLF10Msg(418);exit end;
    MarkStore2(P1);sp:=GetStore2(l1+2);sp^.LL:=l1;
    PosDekFindLine(BegBLn,Posi,false);I2:=0;i:=EndBPos-BegBPos;
    repeat move(Arr[BegBPos],a,i);a[i+1]:=_CR;a[i+2]:=_LF;
      if Oper='M' then TestLastPos(EndBPos,BegBPos);
      move(a,sp^.A[I2+1],i+2);inc(I2,i+2);TestKod;NextLine(false);
    until I2=sp^.LL;
    if (Oper='M')and(L in[BegBLn..EndBLn])and(Posi>EndBPos) then
      dec(Posi,EndBPos-BegBPos);
    if Oper='G' then DelStorClpBd;PosDekFindLine(L,Posi,false);
    BlockCGrasp:=true;
  end;

procedure BlockCDrop(Oper:char);
  var i,I1,I3,ww:word;
  procedure InsertLine;
    begin i:=MinW(I1-I3,LineSize-LastPosLine);
      if i>0 then begin TestLastPos(ww,ww+i);move(sp^.A[I3],Arr[ww],i) end;
      TestKod end;
  begin if Oper='D' then MarkRdClpBd;if sp^.LL=0 then exit;
                                     { hlidani sp^.LL a StoreAvail,MaxLenT
                                       dela NextLine- prechazi mezi segmenty}
    if Oper<>'R' then
      begin EndBPos:=Posi;BegBPos:=Posi;BegBLn:=LineL+Part.LineP end;
    ww:=BegBPos;I1:=1;I3:=1;
    repeat if sp^.A[I1]=_CR then
      begin InsertLine;
        ww:=BegBPos;EndBPos:=MaxW(ww+i,EndBPos);
        if (NextI>LenT)and((TypeT<>FileT)or AllRd) then
          begin TestLenText(LenT,longint(LenT)+2);
            T^[LenT-2]:=_CR;T^[LenT-1]:=_LF;NextI:=LenT end;
        NextLine(false);
      end;
      if sp^.A[I1]in[_CR,_LF,^z] then I3:=I1+1; inc(I1);
    until I1>sp^.LL;
    if I3<I1 then InsertLine;
    if Oper<>'R' then
      begin EndBLn:=Part.LineP+LineL-1;ReleaseStore2(P1);
        PosDekFindLine(BegBLn,BegBPos,true) end;
  end;

procedure BlockCopyMove(Oper:char);
  var b:boolean;
  begin if not BlockExist then exit;FillBlank;
    if TypeB=TextBlock then
      begin if BlockGrasp(Oper) then BlockDrop(Oper) end else
            if BlockCGrasp(Oper) then BlockCDrop(Oper);
  end;

function ColBlockExist:boolean;
  var b:boolean;
  begin if (TypeB=ColBlock)and(BegBPos=EndBPos)and(BegBLn<EndBLn) then
          ColBlockExist:=true else ColBlockExist:=BlockExist;
  end;
procedure BlockLRShift(I1:word);
  var L2:longint;
  procedure NewBlock;
    begin if I1<>Posi then
            begin BegBLn:=L2;EndBLn:=L2;
              BegBPos:=MinW(I1,Posi);EndBPos:=MaxW(I1,Posi) end;
    end;

  begin
    if not Scroll and(Mode<>HelpM)and((KbdFlgs and $03)<>0) then   {Shift}
      begin L2:=LineAbs(LineL);
        if not ColBlockExist then NewBlock else
        case TypeB of
        TextBlock:
          if (BegBLn=EndBLn)and(L2=BegBLn)and(EndBPos=BegBPos)and(I1=BegBPos)
            then if I1>Posi then BegBPos:=Posi else EndBPos:=Posi else
          if (L2=BegBLn)and(I1=BegBPos) then BegBPos:=Posi else
          if (L2=EndBLn)and(I1=EndBPos) then EndBPos:=Posi else NewBlock;
        ColBlock:
          if (L2>=BegBLn)and(L2<=EndBLn) then
            if (EndBPos=BegBPos)and(I1=BegBPos) then
              if I1>Posi then BegBPos:=Posi else EndBPos:=Posi else
            if I1=BegBPos then BegBPos:=Posi else
            if I1=EndBPos then EndBPos:=Posi else NewBlock
          else NewBlock;
        end;
      end;
  end;

procedure BlockUDShift(L1:longint);
  var L2:longint;
  procedure NewBlock;
    begin if L1<>L2 then begin BegBPos:=Posi;EndBPos:=Posi;
                           BegBLn:=MinL(L1,L2);EndBLn:=MaxL(L1,L2) end;
    end;
  begin
    if not Scroll and(Mode<>HelpM)and((KbdFlgs and $03)<>0) then   {Shift}
      begin L2:=LineAbs(LineL);
        if not ColBlockExist then NewBlock else
        case TypeB of
        TextBlock:
          if (BegBLn=EndBLn)and(L1=BegBLn) then
            if (Posi>=BegBPos)and(Posi<=EndBPos) then
              if L1<L2 then begin EndBLn:=L2;EndBPos:=Posi end
              else begin BegBLn:=L2;BegBPos:=Posi end else NewBlock else
          if (L1=BegBLn)and(BegBPos=Posi) then BegBLn:=L2 else
          if (L1=EndBLn)and(EndBPos=Posi) then EndBLn:=L2 else NewBlock;
        ColBlock:
          if (Posi>=BegBPos)and(Posi<=EndBPos) then
            if (BegBLn=EndBLn)and(L1=BegBLn) then
              if L1<L2 then EndBLn:=L2 else BegBLn:=L2 else
            if L1=BegBLn then BegBLn:=L2 else
            if L1=EndBLn then EndBLn:=L2 else NewBlock
          else NewBlock;
        end;
      end;
  end;

function MyPromptLL(n:word;var s:string):boolean;
  begin PromptLL(n,s,1,true); MyPromptLL:=KbdChar=_ESC_ end;

procedure FindReplaceString(First,Last:longint);
var fst,lst:word;
  procedure ChangeP;
    begin
      if ChangePart then
        begin if fst<=Part.MovI then fst:=1 else dec(fst,Part.MovI);
{          if Last>Part.PosP+LenT then lst:=LenT-1 else lst:=Last-Part.PosP;}
          NullChangePart;
    end end;
  procedure ReplaceString(var J:word);
    var r,f:integer;
    begin r:=length(ReplaceStr);f:=length(FindStr);
      TestLenText(J,longint(J)+r-f);ChangeP;
      if TestLastPos(Posi,Posi+r-f) then;
      if ReplaceStr<>'' then move(ReplaceStr[1],T^[J-f],r);
      inc(J,r-f);SetScreen(J,0,0);inc(lst,r-f);inc(Last,r-f);
    end;
  function MyVerifyLL(n:word;s:string):char;
    var w,t:longint; c1,c2,r1,r2,r:word; cc:char;
    begin c2:=wherex+FirstC-1;r2:=wherey+FirstR;
      w:=PushW(1,1,TxtCols,TxtRows);gotoxy(1,TxtRows);
      TextAttr:=colors.pTxt;ClrEol;SetMsgPar(s);WriteMsg(n);
      c1:=whereX;r1:=whereY;
      TextAttr:=colors.pNorm;
      write(' '); CrsNorm; t:=Timer+15; r:=r1;
      repeat while not KbdPressed do
        if Timer>=t then begin
          t:=Timer+15; if r=r1 then begin gotoxy(c2,r2); r:=r2 end
          else begin gotoxy(c1,r1); r:=r1 end end;
        cc:=upcase(char(ReadKbd));
      until cc in [AbbrYes,AbbrNo,_ESC];
      PopW(w);MyVerifyLL:=cc;
    end;
  label 1,2;
  var c:char;
  begin if First>=Last then
          begin if (TypeT=MemoT)and TestOptStr('e') then
                  begin SrchT:=true;Konec:=true end;exit end;
    FirstEvent:=false;
    SetPart(First);fst:=First-Part.PosP;NullChangePart;
    1:if Last>Part.PosP+LenT then lst:=LenT-1 else lst:=Last-Part.PosP;
    ChangeP;            { Background muze volat NextPart }
    if FindString(fst,lst) then
      begin SetScreen(fst,0,0);
        if Replace then
          begin
            if TestOptStr('n') then
              begin ReplaceString(fst);UpdStatLine(LineL,Posi);{BackGround} end
            else begin FirstEvent:=true;BackGround;FirstEvent:=false;
                   c:=MyVerifyLL(408,'');
                   if c=AbbrYes then ReplaceString(fst)
                   else if c=_ESC then exit;
                 end;
            if TestOptStr('g') or TestOptStr('e') or TestOptStr('l')
              then goto 1;
          end;
      end
    else                        { not FindString }
      if not AllRd and(Last>Part.PosP+LenT) then
        begin NextPart;goto 1 end else
      if TestOptStr('e')and(TypeT=MemoT) then
        begin SrchT:=true;Konec:=true end else SetScreen(lst,0,0);
{    BackGround;}
  end;

procedure HelpLU(dir:char);
  var I,I1,I2,h1,h2:word;
  begin ClrWord;h1:=WordNo2;
    if dir='U' then
      begin DekFindLine(LineL-1);Posi:=Position(Colu);
        h2:=MinW(h1,WordNo2+1) end
    else h2:=h1;
    if WordFind(h2,I1,I2,I)and(I>=ScrL-1) then SetWord(I1,I2)
    else
      begin if WordFind(h1+1,I1,I2,I)and(I>=ScrL) then SetWord(I1,I2)
            else begin I1:=SetInd(LineI,Posi);WordL:=0 end;
        I:=ScrL-1 end;
    if I<=ScrL-1 then
      begin DekFindLine(ScrL);RollPred end;
    if WordExist then SetDekLnCurrI(I1);
  end;

procedure HelpRD(dir:char);
  var I,I1,I2,h1,h2:word;
  begin ClrWord;h1:=WordNo2;if WordExist then inc(h1);
    if dir='D' then
      begin NextLine(false);Posi:=Position(Colu);
        while (Posi>0)and(Arr[Posi]<>^s) do dec(Posi);inc(Posi);
        h2:=MaxW(h1+1,WordNo2+1) end
    else h2:=h1+1;
    if WordFind(h2,I1,I2,I)and(I<=ScrL+PageS) then SetWord(I1,I2)
    else
      begin if WordNo2>h1 then inc(h1);
        if WordFind(h1,I1,I2,I)and(I<=ScrL+PageS) then SetWord(I1,I2)
        else begin I1:=SetInd(LineI,Posi);WordL:=0 end;
        I:=ScrL+PageS end;
    if I>=ScrL+PageS then
      begin DekFindLine(ScrL+PageS-1);RollNext end;
    if WordExist then SetDekLnCurrI(I1);
end;



