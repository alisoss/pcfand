{procedure waitln;
  var cc:word;begin repeat cc:=readkey until cc=13 end;}


procedure HandleEvent;
label 1,2,Nic,Opet,OznB;
var I,I1,I2,I3,W1,W2,ww:word;
    L1,L2,fs:longint;
    ss:string;
    j:integer;
    LastL:array[0..160] of word;
    sp:LongStrPtr;
    P1:pointer;
    bb:boolean;

{$I edevinpt.pas}
{$I edevproc.pas}

var er:ExitRecord;
    X:EdExitDPtr;
begin
  IsWrScreen:=false;
  if not MyGetEvent then begin ClrEvent;IsWrScreen:=false;exit end;
  if not Scroll then CleanFrameM;
  NewExit(Ovr,er);goto Opet;         with Event do
  if What=evKeyDown then begin EdOK:=false; ww:=KeyCode; ClrEvent;
  X:=ExitD;                         { Exit-procedure}
  while X<>nil do
    begin if TestExitKey(ww,X) then
            begin  {//nastavuje i EdBreak}
               TestKod;IndT:=SetInd(LineI,Posi);
              ScrT:=((LineL-ScrL+1)shl 8)+Posi-BPos;
              LastTxtPos:=IndT+Part.PosP;TxtXY:=ScrT+longint(Posi)shl 16;
              if X^.Typ='Q' then
                begin KbdChar:=ww;Konec:=true;EditT:=false;goto Nic end;
              case TypeT of
                FileT: begin TestUpdFile;ReleaseStore(T);CloseH(TxtFH) end;
                LocalT,MemoT:
                  begin DelEndT;GetStore(2);move(T^,T^[3],LenT);
                    sp:=pointer(T);sp^.LL:=LenT;
                    if TypeT=LocalT then
                       begin TWork.Delete(LocalPPtr^);
                         LocalPPtr^:=StoreInTWork(sp) end
                    else if UpdatT then
                           begin UpdateEdTFld(sp);UpdatT:=false;
                            end;
                    ReleaseStore(sp);
                  end;
                end;
              L2:=SavePar;CrsHide;
              RestoreExit(er);
              if TypeT=MemoT then StartExit(X,false)
              else CallProcedure(X^.Proc);
              NewExit(Ovr,er);goto Opet;
              if not Scroll then CrsShow;RestorePar(L2);
              case TypeT of
                FileT:begin fs:=Part.PosP+IndT;
                        OpenTxtFH(Mode);RdFirstPart;SimplePrintHead;
                        while (fs>Part.PosP+Part.LenP)and not AllRd do RdNextPart;
                        IndT:=fs-Part.PosP end;
                LocalT,MemoT:
                  begin
                    if TypeT=LocalT then sp:=TWork.Read(1,LocalPPtr^)
                    else begin CRecPtr:=EditDRoot^.NewRecPtr;
                           sp:=_longs(CFld^.FldD);end;
                    LenT:=sp^.LL;T:=pointer(sp);move(T^[3],T^[1],LenT);
                  end;
              end;
              WrEndT;
              IndT:=MinW(IndT,LenT);
              if TypeT<>FileT then with Part do
                begin AbsLenT:=LenT-1;LenP:=AbsLenT;SimplePrintHead end;
              SetScreen(IndT,ScrT,Posi);
              if not Scroll then CrsShow;if not EdOK then goto Nic;
            end;
      X:=X^.Chain end;

  if (Mode in [SinFM,DouFM,DelFM,NotFM])and not Scroll then
    FrameStep(FrameDir,ww)  else
  case ww of
    _left_ :if Mode=HelpM then HelpLU('L') else
              if Scroll then
                if BCol>0 then begin Colu:=BCol;Posi:=Position(Colu) end else
{                begin Posi:=MaxI(1,Position(Column(BPos+1)-1));
                  Colu:=Column(Posi) end}
              else begin I1:=Posi;if Posi>1 then dec(Posi);
                     BlockLRShift(I1) end;
    _right_:if Mode=HelpM then HelpRD('R') else
              if Scroll then
                begin Posi:=MinI(LineSize,Position(BCol+LineS+1));
                  Colu:=Column(Posi) end
{                begin Posi:=MinI(LineSize,Position(Column(BPos)+LineS+1));
                  Colu:=Column(Posi) end}
              else begin I1:=Posi;if Posi<LineSize then inc(Posi);
                     BlockLRShift(I1) end;
    _up_   :if Mode=HelpM then HelpLU('U') else
              begin if Scroll then if RScrL=1 then goto Nic;
                L1:=LineAbs(LineL);PredLine;BlockUDShift(L1);
                if Scroll then Posi:=Position(Colu) end;
    _down_ :if Mode=HelpM then HelpRD('D') else
              begin L1:=LineAbs(LineL);NextLine(true);BlockUDShift(L1);
              if Scroll then Posi:=Position(Colu) end;
    _pgup_ :begin if Mode<>HelpM then TestKod
                  else begin ClrWord; LineL:=ScrL end;
              L1:=LineAbs(LineL);
              if Scroll then
                begin RScrL:=MaxL(1,RScrL-PageS);
                  if ModPage(RScrL) then inc(RScrL);
                  ScrL:=NewL(RScrL);LineL:=ScrL;
                  DekFindLine(LineAbs(LineL));Posi:=Position(Colu);
                  j:=CountChar(^l,LineI,ScrI);
                  if (j>0)and InsPg then begin DekFindLine(LineAbs(LineL+j));
                                           ScrL:=LineL;RScrL:=NewRL(ScrL) end;
                end else
                begin dec(ScrL,PageS);DekFindLine(LineAbs(LineL-PageS)) end;
              ChangeScr:=true;
              if Mode=HelpM then
                begin ScrI:=FindLine(ScrL);
                  Posi:=Position(Colu);
                  if WordFind(WordNo2+1,I1,I2,WordL)and WordExist then
                    SetWord(I1,I2)                  else WordL:=0 end
              else BlockUDShift(L1)
            end;
    _pgdn_ :begin if Mode<>HelpM then TestKod
                  else begin ClrWord;LineL:=ScrL end;
              L1:=LineAbs(LineL);
              if Scroll then
                begin inc(RScrL,PageS);if ModPage(RScrL) then dec(RScrL);
                  DekFindLine(LineAbs(NewL(RScrL)));Posi:=Position(Colu);
                  j:=CountChar(^l,ScrI,LineI);
                  if (j>0)and InsPg then DekFindLine(LineAbs(LineL-j));
                  ScrL:=LineL;RScrL:=NewRL(ScrL);
                  end else
                begin DekFindLine(LineAbs(LineL)+PageS);
                  if LineL>=ScrL+PageS then inc(ScrL,PageS) end;
              ChangeScr:=true;
              if Mode=HelpM then
                begin ScrI:=FindLine(ScrL);
                  Posi:=Position(Colu);W1:=WordNo2;I3:=WordL;
                  if WordFind(W1+1,I1,I2,WordL)and WordExist then
                    SetWord(I1,I2) else
                  if WordFind(W1,I1,I2,WordL)and WordExist then
                    SetWord(I1,I2) else WordL:=0 end
              else BlockUDShift(L1)
            end;

    _ctrlleft_
       :begin
          repeat dec(Posi);
            if Posi=0 then
              begin I:=LineI;PredLine;
                if (I>1)or ChangePart then Posi:=LastPosLine; goto 1 end;
          until not(Arr[Posi] in Oddel);
          while not(Arr[Posi] in Oddel) do
            begin dec(Posi);if Posi=0 then goto 1 end;
          1:inc(Posi);
        end;
    _ctrlright_
       :begin
          while not(Arr[Posi] in Oddel) do
            begin inc(Posi);if Posi>LastPosLine then goto 2 end;
          while Arr[Posi] in Oddel do
            begin inc(Posi);I:=LastPosLine;
              if Posi>I then
                if (NextI<=LenT)and((I=0)or(Posi>I+1)) then
                  begin NextLine(true);Posi:=1 end
                else begin Posi:=I+1;goto 2 end;
            end;
      2:end;

    _Z_:RollNext;
    _W_:RollPred;

    _home_:begin I1:=Posi;Posi:=1;if Wrap then Posi:=MaxI(LeftMarg,1);
             BlockLRShift(I1) end;
    _end_ :begin I1:=Posi;Posi:=LastPosLine;if Posi<LineSize then inc(Posi);
             BlockLRShift(I1) end;
    _QE_:begin TestKod;LineL:=ScrL;LineI:=ScrI;DekodLine end;
    _QX_:begin TestKod;DekFindLine(LineAbs(ScrL+PageS-1)) end;

    _ctrlpgup_:begin TestKod;SetPart(1);SetScreen(1,0,0) end;
    _ctrlpgdn_:begin TestKod;SetPart(AbsLenT-Part.LenP+LenT);
                 SetScreen(LenT,0,0) end;
    _ctrlF3_:begin ss:='';TestKod;
           repeat if MyPromptLL(420,ss) then goto Nic;val(ss,L1,I)
           until (L1>0);
           DekFindLine(L1) end;


    $0020..$00FF:                              { *********CHAR********** }
         begin WrChar(chr(lo(ww)));
           if Wrap then if Posi>RightMarg+1 then
             begin W1:=ord(Arr[Posi]);Arr[Posi]:=char($FF);KodLine;
               I1:=LeftMarg;while Arr[I1]=' ' do inc(I1);
               if I1>RightMarg then I1:=RightMarg;
               L1:=Part.PosP+LineI;
               Format(L1,AbsLenT+LenT-Part.LenP,I1,false);
               SetPart(L1);I:=1;I:=FindChar(I,#$FF,1,LenT);T^[I]:=char(W1);
               SetDekLnCurrI(I);Posi:=I-LineI+1 end;
           end;
    _M_:if Mode=HelpM then begin Konec:=WordExist; KbdChar:=ww end else
        begin if (NextI>=LenT)and not AllRd then NextPartDek;
          if (NextI>LenT)or Insert then
            begin NewLine('m');Posi:=1;clreol;
              if LineL-ScrL=PageS then
                begin gotoxy(1,1);MyDelLine;inc(ScrL);ChangeScr:=true end
              else begin gotoxy(1,succ(LineL-ScrL));MyInsLine end;
              if Indent then
                begin I1:=SetPredI;I:=I1;
                  while (T^[I]=' ')and(T^[I]<>_CR) do inc(I);
                  if T^[I]<>_CR then Posi:=I-I1+1;
                end
              else if Wrap then Posi:=LeftMarg;
              if TestLastPos(1,Posi) then fillchar(Arr[1],Posi-1,32);
            end
          else if NextI<=LenT then begin NextLine(true);Posi:=1 end;
        end;
    _N_:begin NewLine('n');clreol;gotoxy(1,LineL-ScrL+2);MyInsLine end;

    _ins_:Insert:=not Insert;

    _del_,_G_:if Posi<=LastPosLine then DelChar else DeleteL;
    _H_:if Posi>1 then begin dec(Posi);DelChar end
        else begin if (LineL=1)and(Part.PosP>0) then PredPart;
               if LineI>1 then
                 begin TestKod;dec(LineL);
                   if T^[LineI-1]=_LF then SetDekCurrI(LineI-2)
                   else SetDekCurrI(LineI-1);
                   Posi:=MinW(255,succ(LastPosLine));DeleteL;
                   if LineL<ScrL then begin dec(ScrL);ChangeScr:=true end;
                 end;
             end;
    _Y_:begin if (NextI>=LenT)and not AllRd then NextPartDek;
          NextI:=MinW(NextI,LenT);TestLenText(NextI,LineI);
          if BegBLn>LineAbs(LineL) then dec(BegBLn) else
          if BegBLn=LineAbs(LineL) then if TypeB=TextBlock then BegBPos:=1;
{          if BegBLn>=LineAbs(LineL) then
            if (BegBLn=LineAbs(LineL))and(TypeB=TextBlock) then
              BegBPos:=1 else dec(BegBLn);}
          if EndBLn>=LineAbs(LineL) then
            if (EndBLn=LineAbs(LineL))and(TypeB=TextBlock) then
              EndBPos:=1 else dec(EndBLn);
          MyDelLine;DekodLine;Posi:=1;
        end;
    _T_:if Posi>LastPosLine then DeleteL
        else
          begin I:=Posi;
            if Arr[Posi] in Oddel then DelChar
            else while (I<=LastPosLine)and not(Arr[I] in Oddel) do inc(I);
            while (I<=LastPosLine)and(Arr[I]=' ') do inc(I);
            if (I>Posi) and TestLastPos(I,Posi) then
          end;
    _QI_:Indent:=not Indent;
    _QL_:if UpdatedL then DekodLine;
    _QY_:if TestLastPos(LastPosLine+1,Posi) then clreol;

    _QF_,_QA_
        :begin Replace:=false;
           if MyPromptLL(405,FindStr) then goto Nic;
           if ww=_QA_ then
             begin if MyPromptLL(407,ReplaceStr) then goto Nic;
               Replace:=true end;
           ss:=OptionStr;if MyPromptLL(406,ss) then goto Nic;OptionStr:=ss;
           TestKod;
           if TestOptStr('l')and(not BlockExist or(TypeB=ColBlock)) then
             goto Nic;
           if TestOptStr('l') then SetBlockBound(L1,L2)
           else begin L2:=AbsLenT-Part.LenP+LenT;
                  if TestOptStr('g') or TestOptStr('e') then L1:=1
                  else L1:=Part.PosP+SetInd(LineI,Posi) end;
           FindReplaceString(L1,L2);if ww=_QA_ then DekodLine;
           if not Konec then begin FirstEvent:=false;Background end;
         end;
    _L_:if FindStr<>'' then
          begin TestKod;
            if TestOptStr('l')and(not BlockExist or(TypeB=ColBlock)) then
               goto Nic;
            fs:=1;L1:=Part.PosP+SetInd(LineI,Posi);
            if TestOptStr('l') then SetBlockBound(fs,L2)
            else L2:=AbsLenT-Part.LenP+LenT;
            if L1<fs then L1:=fs;{if L1>=L2 then goto Nic;}
            FindReplaceString(L1,L2);
            if not Konec then begin FirstEvent:=false;Background end;
          end;

    _I_:begin I1:=SetPredI+Posi;if I1>=LineI-1 then goto Nic;
          I:=I1;while (T^[I]<>' ')and(T^[I]<>_CR) do inc(I);
          while T^[I]=' ' do inc(I);I2:=I-I1+1;
          if TestLastPos(Posi,Posi+I2) then
            fillchar(Arr[Posi],I2,32);inc(Posi,I2);
        end;
    _J_:begin I1:=SetPredI+Posi-2;if (I1>=LineI-1)or(I1=0) then goto Nic;
          I:=I1;while T^[I]=' ' do inc(I);
          while (T^[I]<>' ')and(T^[I]<>_CR)  do inc(I);
          if I=I1 then goto Nic;I2:=I-I1-1;I:=Posi;
          dec(Posi);while (Posi>0)and(Arr[Posi]<>' ')do dec(Posi);inc(Posi);
          if TestLastPos(Posi,Posi+I2) then
            fillchar(Arr[Posi],I2,32);Posi:=I+I2+1;
         end;

    _QB_:begin TestKod;
           PosDekFindLine(BegBLn,MinW(LastPosLine+1,BegBPos),false) end;
    _QK_:begin TestKod;
           PosDekFindLine(EndBLn,MinW(LastPosLine+1,EndBPos),false) end;

    _KB_,_F7_,_KH_:
         begin BegBLn:=LineAbs(LineL);
           if TypeB=TextBlock then BegBPos:=MinI(LastPosLine+1,Posi)
           else BegBPos:=Posi;
           if ww=_KH_ then goto OznB end;
    _KK_,_F8_:
         OznB:
         begin EndBLn:=LineAbs(LineL);
           if TypeB=TextBlock then EndBPos:=MinI(LastPosLine+1,Posi)
           else EndBPos:=Posi end;
    _KN_:if TypeB=TextBlock then TypeB:=ColBlock
         else TypeB:=TextBlock;
    _KY_:if BlockHandle('Y') then begin EndBLn:=BegBLn;EndBPos:=BegBPos end;
    _KC_:BlockCopyMove('C');
    _KV_:BlockCopyMove('M');
    _KU_:BlockHandle('U');
    _KL_:BlockHandle('L');
    _CtrlF7_:if TypeB=TextBlock then BlockGrasp('G') else BlockCGrasp('G');
(*    _KW_:if BlockExist  then
           begin CPath:=SelectDiskFile('.TXT',401,false);if CPath='' then goto Nic;
             CVol:=''; W1:=OpenH(_isnewfile,Exclusive);
             if HandleError=80 then
               begin SetMsgPar(CPath);
                 if PromptYN(780) then W1:=OpenH(_isoverwritefile,Exclusive)
                 else goto Nic end;
             if HandleError<>0 then begin MyWrLLMsg(CPath);goto Nic end;
             fs:=0;{L1:=LineAbs(LineL);I:=Posi;}
             if BlockHandle('W') then
               begin WriteH(W1,0,T^);{truncH} CloseH(W1); HMsgExit(CPath) end;
{             PosDekFindLine(L1,I,true);}
           end;
*)
    _KW_:begin I1:=BegBLn;I2:=BegBPos;I3:=EndBLn;I:=EndBPos;bb:=TypeB;
           if not BlockExist then
             begin BegBLn:=1;EndBLn:=$7FFF;BegBPos:=1;EndBPos:=$FF;
               TypeB:=TextBlock end;
           CPath:=SelectDiskFile('.TXT',401,false);if CPath='' then goto Nic;
           CVol:=''; W1:=OpenH(_isnewfile,Exclusive);
           if HandleError=80 then
             begin SetMsgPar(CPath);
               if PromptYN(780) then W1:=OpenH(_isoverwritefile,Exclusive)
               else goto Nic end;
           if HandleError<>0 then begin MyWrLLMsg(CPath);goto Nic end;
           fs:=0;{L1:=LineAbs(LineL);I:=Posi;}
           if BlockHandle('W') then
             begin WriteH(W1,0,T^);{truncH} CloseH(W1); HMsgExit(CPath) end;
{           PosDekFindLine(L1,I,true);}
           BegBLn:=I1;BegBPos:=I2;EndBLn:=I3;EndBPos:=I;TypeB:=bb;
         end;
    _ShiftF7_:if TypeB=TextBlock then BlockDrop('D') else BlockCDrop('D');
    _KR_:begin
           CPath:=SelectDiskFile('.TXT',400,false);if CPath='' then goto Nic;
           CVol:=''; W1:=OpenH(_isoldfile,RdOnly);
           if HandleError<>0 then begin MyWrLLMsg(CPath);goto Nic end;
           BegBLn:=Part.LineP+LineL;BegBPos:=Posi;
           L1:=Part.PosP+LineI+Posi-1;
           FillBlank;fs:=FileSizeH(W1);L2:=0;NullChangePart;
           case TypeB of
           TextBlock:
             begin
               repeat I2:=$1000;if fs-L2<longint(I2) then I2:=fs-L2;
                 if (TypeT<>FileT)and
                    ((I2>=MaxLenT-LenT)or(I2>=StoreAvail))
                 then begin if I2>=StoreAvail then I2:=StoreAvail;
                        I2:=MinW(I2,MaxLenT-LenT)-2;fs:=L2+I2;
                        WrLLF10Msg(404) end;
                 I1:=L1+L2-Part.PosP;
                 TestLenText(I1,longint(I1)+I2);
                 if ChangePart then dec(I1,Part.MovI);
                 SeekH(W1,L2);ReadH(W1,I2,T^[I1]);HMsgExit('');
                 inc(L2,I2);
               until L2=fs;
               I:=L1+L2-Part.PosP;
               if T^[I-1]=^z then begin TestLenText(I,I-1);dec(I) end;
               SetDekLnCurrI(I);EndBLn:=Part.LineP+LineL;EndBPos:=succ(I-LineI);
             end;
           ColBlock:
             begin EndBPos:=Posi;I2:=$1000;
               MarkStore2(P1);sp:=GetStore2(I2+2);{ww:=BegBPos;}
               repeat if fs-L2<longint(I2) then I2:=fs-L2;
                 SeekH(W1,L2);ReadH(W1,I2,sp^.A);HMsgExit('');
                 inc(L2,I2);sp^.LL:=I2;BlockCDrop('R');
               until L2=fs;
               EndBLn:=Part.LineP+LineL-1;ReleaseStore2(P1);
             end;
           end;
           CloseH(W1);HMsgExit('');
           SetPartLine(BegBLn);SetDekLnCurrI(L1-Part.PosP);UpdatedL:=true;
         end;
    _KP_:begin if BlockHandle('P') then else
             begin I1:=BegBLn;I2:=BegBPos;I3:=EndBLn;I:=EndBPos;bb:=TypeB;
               BegBLn:=1;EndBLn:=$7FFF;BegBPos:=1;EndBPos:=$FF;
               TypeB:=TextBlock;
               if BlockHandle('P') then;
               BegBLn:=I1;BegBPos:=I2;EndBLn:=I3;EndBPos:=I;TypeB:=bb;
         end end;
    _KF_:if BlockExist and(TypeB=TextBlock) then
           begin TestKod;CrsHide;
             SetPartLine(EndBLn);I2:=EndBLn-Part.LineP;
             L1:=SetInd(FindLine(integer(I2)),EndBPos)+Part.PosP;
             L2:=BegBLn;Posi:=BegBPos;SetPartLine(L2);I2:=BegBLn-Part.LineP;
             Format(FindLine(integer(I2))+Part.PosP,L1,BegBPos,true);
             DekFindLine(L2);if not Scroll then CrsShow;
           end;

    _OJ_:Just:=not Just;
    _OW_:begin Wrap:=not Wrap;
           if Wrap then begin dec(LineS);dec(LastC) end
           else begin inc(LastC);inc(LineS);
                  ScrRdBuf(FirstC-1,TxtRows-1,LastL[1],LineS);
                  LastL[MargLL[1]]:=MargLL[2];LastL[MargLL[3]]:=MargLL[4];
                  ScrWrBuf(FirstC-1,TxtRows-1,LastL[1],LineS) end;
         end;
    _OL_:begin       {LeftMarg}
           repeat str(Posi,ss);if MyPromptLL(410,ss) then goto Nic;
             val(ss,I1,I) until (I1<RightMarg)and(I1>0);
           LeftMarg:=I1 end;
    _OR_:begin       {RightMarg}
           repeat str(Posi,ss);if MyPromptLL(409,ss) then goto Nic;
             val(ss,I1,I);{ inc(I1)} until (I1<=255) and (LeftMarg<I1);
           RightMarg:=I1 end;
    _OC_:begin I1:=1;while (I1<LastPosLine)and(Arr[I1]=' ') do inc(I1);
           I2:=LastPosLine;while (I2>1) and(Arr[I2]=' ') do dec(I2);
           j:=(LeftMarg+(RightMarg-LeftMarg)div 2)-integer(I1+(I2-I1)div 2);
           if (I2<I1)or(j=0) then goto Nic;
           if j>0 then
             begin if TestLastPos(1,j+1) then fillchar(Arr[1],j,32) end
           else begin j:=MinI(-j,I1-1);TestLastPos(j+1,1) end;
           Posi:=MinW(LineSize,LastPosLine+1);
         end;

    _B_:begin TestKod;L1:=Part.PosP+LineI;
          Format(L1,AbsLenT+LenT-Part.LenP,MinI(LeftMarg,Posi),false);
          SetPart(L1);I2:=L1-Part.PosP;SetDekLnCurrI(I2);Posi:=1 end;

{    _framesingle_:begin Mode:=SinFM;Frame end;
    _framedouble_:begin Mode:=DouFM;Frame end;
    _delframe_:begin Mode:=DelFM;Frame end;}
    _framesingle_:begin Mode:=SinFM;CrsBig;FrameDir:=0 end;
    _framedouble_:begin Mode:=DouFM;CrsBig;FrameDir:=0 end;
    _delframe_:begin Mode:=DelFM;CrsBig;FrameDir:=0 end;

    _F4_:begin W1:=ord(ToggleCS(Arr[Posi]));
               UpdatedL:= chr(W1)<>Arr[Posi];Arr[Posi]:=chr(W1) end;

    $1000..$101F: begin WrChar(chr(lo(ww)));        { ***CTRL-klavesy*** }
                    if ww=$100D then begin TestKod;DekodLine;dec(Posi) end end;
    _CtrlF5_:
        Calculate;
    _AltF8_: begin L2:=SavePar;
               W1:=Menu(45,ord(spec.KbdTyp)+1);
               if W1<>0 then spec.KbdTyp:=TKbdConv(W1-1);
               RestorePar(L2);
             end;
    _CtrlF6_:if (TypeT=FileT)or(TypeT=LocalT) then BlockHandle('p');

    $1000: Opet:begin
                  if (Mode<>HelpM)and(Mode<>ViewM)and Wrap then
                    window(FirstC,FirstR+1,LastC+1,LastR)
                  else window(FirstC,FirstR+1,LastC,LastR);

                  if not Scroll then CrsShow;SetScreen(LineI,0,0) end;
                                               { ***ERROR TESTLENTEXT*** }
    _U_:if TypeT<>FileT then
        if PromptYN(108) then
          begin IndT:=1;KbdChar:=_U_;Konec:=true;EdBreak:=$FFFF end;
    else                                       { ********BREAKS******* }
      if ((lo(ww)=$00)and(pos(chr(hi(ww)),Breaks)<>0))or
         ((ww=_AltEqual_)and(TypeT<>FileT)) then
        begin TestKod;KbdChar:=ww;Konec:=true;EdBreak:=$FFFF end
      else if ww=_ESC_  then
         begin TestKod;KbdChar:=ww;Konec:=true;EdBreak:=0 end
  end;end
    else if (What=evMouseDown)and((Mode=HelpM)or(Mode=TextM)) then
           begin
             if Mode=TextM then TestKod;
             if not((Where.y in[FirstR..LastR-1])and
                    (Where.x in[FirstC-1..LastC-1])) then
               begin ClrEvent;goto Nic end;
             I3:=LineI;j:=Posi;W1:=Where.y-WindMin.y+ScrL;
             if Mode=HelpM then W2:=WordNo2+1;
             DekFindLine(LineAbs(W1));
             Posi:=Where.x-WindMin.x+1;if Mode<>TextM then Posi:=Position(Posi);
             inc(Posi,BPos);
             I:=SetInd(LineI,Posi);
             if I<LenT then
               begin if Mode=HelpM then
                 begin ClrWord; WordFind(WordNo(I+1),I1,I2,W1);
                   if (I1<=I)and(I2>=I) then begin
                     SetWord(I1,I2); KbdChar:=_M_; Konec:=true end
                   else if WordExist then
                          begin WordFind(W2,I1,I2,W1);SetWord(I1,I2) end
                        else SetDekLnCurrI(I3);
               end end
             else begin SetDekLnCurrI(I3);Posi:=j end;
             ClrEvent;
           end
         else ClrEvent;
  Nic:{ClrEvent;}RestoreExit(er);IsWrScreen:=false;
end;

