procedure WrStatusLine;
  var Blanks:string;
      Len:byte absolute Blanks;
      s:string;
      i:integer;
  begin
    if Mode<>HelpM then
      begin fillchar(Blanks[1],TxtCols,32);Len:=TxtCols;
        if HeadS<>nil then
          begin move(HeadS^[1],Blanks[1],length(HeadS^));
            i:=pos('_',Blanks);
            if i=0 then begin move(Blanks[1],Blanks[TStatL+3],252-TStatL);
                          fillchar(Blanks[1],TStatL+2,32) end
            else while (i<=Len)and(Blanks[i]='_') do
                   begin Blanks[i]:=' ';inc(i) end;
          end
        else
          begin s:=ShortName(NameT);
            i:=TStatL+3; {free}
            if length(s)+i>=TxtCols then i:=TxtCols-length(s)-2;
            move(s[1],Blanks[i],length(s)) end;
        ScrWrStr(0,0,Blanks,SysLColor);
      end;
  end;

procedure WriteMargins;
var LastL:array[0..200]of word;
begin
  if (Mode<>HelpM)and(Mode<>ViewM)and Wrap then
          begin ScrRdBuf(FirstC-1,TxtRows-1,LastL[1],LineS);
            LastL[MargLL[1]]:=MargLL[2];LastL[MargLL[3]]:=MargLL[4];
            MargLL[1]:=MaxI(0,LeftMarg-BPos);if MargLL[1]>0 then
              begin MargLL[2]:=LastL[Margll[1]];
                LastL[MargLL[1]]:=(LastL[LineS]and $FF00)+$10 end;
            MargLL[3]:=MaxI(0,RightMarg-BPos);if MargLL[3]>0 then
              begin MargLL[4]:=LastL[MargLL[3]];
                LastL[MargLL[3]]:=(LastL[LineS]and $FF00)+$11 end;
            ScrWrBuf(FirstC-1,TxtRows-1,LastL[1],LineS);
          end;
end;

procedure WrLLMargMsg(s:StringPtr;n:word);
  begin
    if s<>nil then begin MsgLine:=s^;WrLLMsgTxt end else
    if n<>0 then WrLLMsg(n) else
    if LastS<>nil then begin MsgLine:=LastS^;WrLLMsgTxt end else
    WrLLMsg(LastNr);         if Mode=TextM then WriteMargins end;

procedure InitScr;
begin
  FirstR:=WindMin.Y+1; FirstC:=WindMin.X+1;
  LastR:=WindMax.Y+1; LastC:=WindMax.X+1;
  if (FirstR=1)and(Mode<>HelpM) then inc(FirstR);
  if LastR=TxtRows then dec(LastR);
  MinC:=FirstC;MinR:=FirstR;MaxC:=LastC;MaxR:=LastR;
  window(FirstC,FirstR,LastC,LastR);
  dec(FirstR);
  if (Mode<>HelpM)and(Mode<>ViewM)and Wrap then dec(LastC);
  PageS:=LastR-FirstR;LineS:=succ(LastC-FirstC);
end;

procedure UpdStatLine(Row,Col:integer);
  var StatLine:string[35];
      st:string[10];len:byte absolute st;
      i:integer;
      lRow:longint;
  begin if not HelpScroll then
    begin lRow:=Row+Part.LineP;
      StatLine:='     1:                             ';
      str(lRow:5,st);move(st[1],StatLine[2],5);
      str(Col,st);while len<4 do st:=st+' ';move(st[1],StatLine[8],4);
      case Mode of
        TextM:begin if Insert then move(InsMsg[1],StatLine[11],5)
                    else move(nInsMsg[1],StatLine[11],5);
                if Indent then move(IndMsg[1],StatLine[16],5);
                if Wrap then move(WrapMsg[1],StatLine[21],5);
                if Just then move(JustMsg[1],StatLine[26],5);
                if TypeB=ColBlock then move(BlockMsg[1],StatLine[31],5);
                {WriteMargins;}
              end;
        ViewM:move(ViewMsg[1],StatLine[11],length(ViewMsg));
        SinFM:StatLine[13]:='-';
        DouFM:StatLine[13]:='=';
        DelFM:StatLine[13]:='/';
      end;
      i:=1;
      if HeadS<>nil then
        begin i:=MaxW(1,pos('_',HeadS^));
          if i>TxtCols-TStatL then i:=MaxI(integer(TxtCols)-TStatL,1) end;
       ScrWrStr(i-1,0,StatLine,SysLColor);
    end;
  end;

procedure EditWrline (P:ArrPtr;Row:integer);
  var
    BuffLine:array[1..255] of word;
    Line:word;
    I,LP,B,E : integer;
    Newvalue : integer;
    Nv : array [1..2] of byte absolute Newvalue;
    IsCtrl:boolean;
  begin
    Line:=pred(ScrL+Row);
    if LineInBlock(Line)and(TypeB=TextBlock) then
      Nv[2]:=BlockColor else Nv[2]:=TxtColor;
    I:=1;
    while (P^[I]<>_CR)and(I<=LineSize) do
      begin Nv[1]:=ord(P^[I]);BuffLine[I]:=Newvalue;
        if Nv[1]<32 then IsCtrl:=true;inc(I) end;
    LP:=I-1;Nv[1]:=32;
    for I:=LP+1 to BPos+LineS do BuffLine[I]:=Newvalue;
    if BegBLn<=EndBLn then
      if LineBndBlock(Line)or((TypeB=ColBlock)and LineInBlock(Line)) then
        begin if (BegBLn=LineAbs(Line))or(TypeB=ColBlock) then
                B:=MinI(BegBPos,LineS+BPos+1) else B:=1;
              if (EndBLn=LineAbs(Line))or(TypeB=ColBlock) then
                E:=MinI(EndBPos,LineS+BPos+1) else E:=LineS+BPos+1;
              for I:=B to pred(E) do
                BuffLine[I]:=(BuffLine[I]and $00FF)+BlockColor shl 8;
        end;
    if IsCtrl then
      for I:=succ(BPos) to LP do
        if ord(P^[I])<32 then
          BuffLine[I]:=((ord(P^[I])+64)and $00FF)+
                       ColKey[pos(char(P^[I]),CtrlKey)] shl 8;

    ScrWrBuf(WindMin.X,WindMin.Y+Row-1,BuffLine[BPos+1],LineS);
end;

var InsPage:boolean;

procedure ScrollWrline (P:ArrPtr;Row:integer;var CO:ColorOrd);
  var len:byte absolute CO;
  function Color:byte;
    begin if CO='' then Color:=TxtColor
          else Color:=ColKey[pos(CO[len],CtrlKey)] end;
  const GrafCtrl:string[15]=#3#6#9#11#15#16#18#21#22#24#25#26#29#30#31;
{  function PColumn(w:word):word;
    var c,ww:word;
    begin if w=0 then begin PColumn:=0;exit end;
      ww:=1;c:=1;
      while ww<=w do begin if P^[ww]>=' 'then inc(c);inc(ww) end;
      if P^[w]>=' ' then dec(c);PColumn:=c;
    end;   }
  var
    BuffLine:array[1..255] of word;
    I,J,LP,pp: integer;
    Newvalue : integer;
    Nv : array [1..2] of byte absolute Newvalue;
    IsCtrl:boolean;cc:char;
    Col:byte;
  begin Col:=Color;Nv[2]:=Col;I:=1;J:=1;cc:=P^[I];
    while (cc<>_CR)and(I<=LineSize)and not InsPage do
      begin if (cc>=#32)or(pos(cc,GrafCtrl)>0) then
              begin Nv[1]:=ord(cc);BuffLine[J]:=Newvalue;inc(J) end else
            if pos(cc,CtrlKey)>0 then IsCtrl:=true else
            if Scroll and(cc=^l) then begin InsPage:=InsPg;dec(I) end;
        inc(I);cc:=P^[I]  end;
    LP:=I-1;Nv[1]:=32;
    while J<=BCol+LineS do begin BuffLine[J]:=Newvalue;inc(J) end;
    if IsCtrl then
      begin I:=1;J:=1;
        while I<=LP do
          begin cc:=P^[I];
            if (cc>=#32)or(pos(cc,GrafCtrl)>0) then
              begin BuffLine[J]:=(BuffLine[J] and $00FF)+Col shl 8;
                inc(J) end
            else if pos(cc,CtrlKey)>0 then
                   begin pp:=pos(cc,CO);
                     if pp>0 then CO:=copy(CO,1,pp-1)+copy(CO,pp+1,len-pp)
                     else CO:=CO+cc;
                     Col:=Color
                   end
                 else if cc=^l then BuffLine[J]:= 219+Col shl 8;
            inc(I) end;
        while J<=BCol+LineS do
          begin BuffLine[J]:=(BuffLine[J] and $00FF)+Col shl 8;inc(J) end;
      end;
    ScrWrBuf(WindMin.X,WindMin.Y+Row-1,BuffLine[BCol+1],LineS);
end;

function MyTestEvent:boolean;
  begin
    if FirstEvent then begin MyTestEvent:=false; exit end;
    MyTestEvent:=TestEvent;
  end;

procedure UpdScreen;
  var r,rr:integer;w:word;
      Ind:word;
      co1,co2:ColorOrd;
      oldSI:word;
      PgStr:string;
  label 1;
  procedure WrEndL(Hard:boolean;Row:integer);
    var w:word;
    begin if (Mode<>HelpM)and(Mode<>ViewM)and Wrap then
      begin if Hard then w:=$11+TxtColor shl 8 else w:=32+TxtColor shl 8;
        ScrWrBuf(WindMin.X+LineS,WindMin.Y+Row-1,w,1) end;
    end;
  begin oldSI:=ScrI;InsPage:=false;
    if ChangeScr then
      begin if ChangePart then DekodLine;ChangeScr:=false;
        if Scroll then ScrI:=LineI else ScrI:=FindLine(ScrL);
        if HelpScroll  then
{          if ChangePart then}
            begin ColScr:=Part.ColorP;SetColorOrd(ColScr,1,ScrI) end
{          else if oldSI<=ScrI then SetColorOrd(ColScr,oldSI,ScrI)
               else SetColorOrd(ColScr,ScrI,oldSI);}
      end;
    if Scroll then                    {tisk aktualniho radku}
      begin fillchar(PgStr,256,ord(CharPg));PgStr[0]:=chr(255);
        co1:=ColScr;r:=1;while Arr[r]=^l do inc(r);
        ScrollWrLine(addr(Arr[r]),1,co1) end else
    if Mode=HelpM then
      begin co1:=Part.ColorP;SetColorOrd(co1,1,LineI);
          ScrollWrLine(addr(Arr),LineL-ScrL+1,co1) end
    else EditWrline(addr(Arr),LineL-ScrL+1);
    WrEndL(HardL,LineL-ScrL+1);
    if MyTestEvent then exit;
    Ind:=ScrI;r:=1;rr:=0;w:=1;InsPage:=false;co2:=ColScr;
    if Scroll then while T^[Ind]=^l do inc(Ind);
    repeat if MyTestEvent then exit;                   {tisk celeho okna}
      if (Ind>=LenT)and not AllRd then
        begin NextPartDek;dec(Ind,Part.MovI) end;

      if Scroll and(Ind<LenT) then
        if (InsPg and(ModPage(r-rr+RScrL-1)))or InsPage then
          begin EditWrline(addr(PgStr[1]),r);WrEndL(false,r);
            if InsPage then inc(rr);InsPage:=false;goto 1 end;
      if not Scroll and(Ind=LineI) then begin Ind:=NextI;co2:=co1;goto 1 end;
      if Ind<LenT then
        begin
          if HelpScroll then ScrollWrLine(addr(T^[Ind]),r,co2)
          else  EditWrLine(addr(T^[Ind]),r);
          if InsPage then Ind:=FindChar(w,^l,Ind,LenT)+1
          else Ind:=FindChar(w,_CR,Ind,LenT)+1;
          WrEndL((Ind<LenT)and(T^[Ind]=_LF),r);if T^[Ind]=_LF then inc(Ind)
        end
      else begin EditWrline(addr(T^[LenT]),r);WrEndL(false,r) end;

      1:inc(r);
      if Scroll and(T^[Ind]=^l) then begin InsPage:=InsPg;inc(Ind) end;
    until r>PageS;
  end;

procedure Background;
  var p:word;
  begin
    UpdStatLine(LineL,Posi);if MyTestEvent then exit;
    if HelpScroll then
      begin p:=Posi;
        if Mode=HelpM then if WordL=LineL then while Arr[p+1]<>^q do inc(p);
{        if Column(p)-Column(BPos)>LineS then BPos:=Position(Column(p)-LineS);
        if Column(Posi)<=Column(BPos) then BPos:=Position(Column(Posi)-1);}
        if Column(p)-BCol>LineS then
          begin BCol:=Column(p)-LineS;BPos:=Position(BCol) end;
        if Column(Posi)<=BCol then
          begin BCol:=Column(Posi)-1;BPos:=Position(BCol) end;
      end else
      begin if Posi>LineS then if Posi>BPos+LineS then BPos:=Posi-LineS;
        if Posi<=BPos then BPos:=pred(Posi) end;
    if LineL<ScrL then begin ScrL:=LineL;ChangeScr:=true end;
    if LineL>=ScrL+PageS then
      begin ScrL:=succ(LineL-PageS);ChangeScr:=true end;
    UpdScreen;                           {tisk obrazovky}
    WriteMargins;gotoxy (Posi-BPos,succ(LineL-ScrL));
    IsWrScreen:=true;
  end;

