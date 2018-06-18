{$I switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit Editor;

interface

uses dos,
     base,access,wwmenu,                 {INITPGM ???}
     rdrun;                              {TxExitDPtr,CalcTxt,...}

type MsgStr=record Head,Last,CtrlLast,AltLast,ShiftLast:StringPtr end;
     MsgStrPtr=^MsgStr;

function EditText(pMode:char;pTxtType:char;pName:pathstr;pErrMsg:string;
                  pTxtPtr:CharArrPtr;pMaxLen:word;
                  var pLen{*},pInd{*}:word;var pScr{*}:longint;
                  pBreaks:string;pExD:EdExitDPtr;
                  var pSrch{*},pUpdat{*}:boolean;
                  pLastNr,pCtrlLastNr:word;pMsgS:MsgStrPtr):boolean;
procedure SimpleEditText(pMode:char;pErrMsg,pName:string;
                  TxtPtr:CharArrPtr;MaxLen:word;var Len{*},Ind{*}:word;
                  var Updat{*}:boolean);
function FindText(const Pstr:string;Popt:ScreenStr;PTxtPtr:CharArrPtr;PLen:word):word;
procedure Help(R:RdbDPtr;Name:string;InCWw:boolean);
procedure InitTxtEditor;
procedure EditTxtFile(LP:LongintPtr;Mode:char;ErrMsg:ScreenStr;ExD:EdExitDPtr;
                      TxtPos:longint;Txtxy:longint;
                      V:WRectPtr;Atr:word;const Hd:string;
                      WFlags:byte;MsgS:MsgStrPtr);
procedure ViewPrinterTxt;
procedure SetEditTxt(PD:InstrPtr);
procedure GetEditTxt(var pInsert,pIndent,pWrap,pJust,pColBlk:boolean;
                     var pLeftMarg,pRightMarg:integer);
procedure ClearHelpStkForCRdb;

const TextM='T';ViewM='V';HelpM='H';
      SinFM='S';DouFM='D';DelFM='F';NotFM='N';
      FileT='F';LocalT='V';MemoT='M';
var Insert,Indent,Wrap,Just:boolean;


implementation

{procedure waitln;
var c:char;begin repeat c:=crt.readkey until c=^m end;}

uses drivers,obase,obaseww,runfrml,oaccess,wwmix,           {EDEVENT - SelectDiskFile ... ^k^w,^k^r,...}
     compile,                    { - Error,RunError,... }
     runproc,                    {EDEVENT - CallProcedure ... ExitD}
     runedi,                     {EDEVENT - StartExit, E^ ...ExitD}
     printtxt;                   {EDEVENT - PrintArray ... ^k^p}

procedure Ovr; far;
assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push cs; push ax;
end;

const InterfL=4;{sizeof(Insert+Indent+Wrap+Just)}

const LineSize=255; SuccLineSize=256; TextStore=$1000;
      TStatL=35;{=10(Row:Col)+length(InsMsg+IndMsg+WrapMsg+JustMsg+BlockMsg)}

const Oddel:set of char=[#1..#47,#58..#64,#91..#94,#96,#123..#127];

const _QY_=$1119;_QL_=$110C;_QK_=$110B;_QB_=$1102;_QI_=$1109;_QF_=$1106;
      _QE_=$1105;_QX_=$1118;_QA_=$1101;
      _framesingle_=$112D;_framedouble_=$113D;_delframe_=$112F;
      _frmsin_=$2D;_frmdoub_=$3D;_dfrm_=$2F;_nfrm_=$20;
      _KB_=$0B02;_KK_=$0B0B;_KH_=$0B08;
      _KS_=$0B13;_KY_=$0B19;_KC_=$0B03;_KV_=$0B16;_KW_=$0B17;
      _KR_=$0B12;_KP_=$0B10;_KN_=$0B0E;
      _KU_=$0B15;_KL_=$0B0C;
      _OW_=$0F17;_OL_=$0F0C;_OR_=$0F12;_OJ_=$0F0A;_OC_=$0F03;
      _KF_=$0B06;

const CountC=7;
      CtrlKey:string[CountC]=#19#23#17#4#2#5#1;
          { ^s - underline, ^w - italic, ^q - expanded,
            ^d - double, ^b - bold, ^e - compressed, ^a - ELITE }
type ColorOrd=string[CountC];
const ColBlock=true;TextBlock=false;

    {**********global param begin for SavePar}
var Mode:char;
    TypeT:char;
    NameT:pathstr;
    ErrMsg:string;
    MaxLenT,LenT,IndT,ScrT:word;
    Breaks:string;
    ExitD:EdExitDPtr;
    SrchT,UpdatT:boolean;
    LastNr,CtrlLastNr:word;
    LeftMarg,RightMarg:integer;
    TypeB:boolean;
    LastS,CtrlLastS,ShiftLastS,AltLastS,HeadS:StringPtr;
    LocalPPtr:LongintPtr;
    EditT:boolean;

var ColKey:array[0..CountC] of byte;
    TxtColor,BlockColor,SysLColor:byte;

    InsMsg,nInsMsg,IndMsg,WrapMsg,JustMsg,BlockMsg:string[5];
    ViewMsg:string[20];

    CharPg:char;InsPg:boolean;

    BegBLn,EndBLn:longint;BegBPos,EndBPos:word;
    ScrI,LineI,Posi,BPos:word;        {screen status}

    FindStr,ReplaceStr:string;Replace:boolean;
    OptionStr:ScreenStr;

    FirstEvent:boolean;

    PHNum,PPageS:word;   {strankovani ve Scroll}

type PartDescr=record PosP,LineP:longint;
                      LenP,MovI,MovL:word;
                      UpdP:boolean;
                      ColorP:ColorOrd end;
var  Part:PartDescr;
     TxtFH:word;
     TxtPath:pathstr;TxtVol:volstr;
     AllRd:boolean;
     AbsLenT:longint;
     ChangePart,UpdPHead:boolean;

var T:CharArrPtr;
    {************ end for SavePar}

function SavePar:longint;
  var sp:LongStrPtr;len:word;
  begin len:=InterfL+ofs(T)-ofs(Mode)+4;
    sp:=GetStore(len+2);sp^.LL:=len;
    move(Insert,sp^.A,InterfL);move(Mode,sp^.A[InterfL+1],len-InterfL);
    SavePar:=StoreInTWork(sp);ReleaseStore(sp);
  end;
procedure RestorePar(l:longint);
  var sp:LongStrPtr;
  begin sp:=ReadDelInTWork(l);
    move(sp^.A,Insert,InterfL);move(sp^.A[InterfL+1],Mode,sp^.LL-InterfL);
    ReleaseStore(sp);
  end;

{$I edglobal.pas}
{$I edtextf.pas}
{$I ededit.pas}

procedure SetEditTxt(PD:InstrPtr);
begin
  if PD^.Insert<>nil then Insert:=not RunBool(PD^.Insert);
  if PD^.Indent<>nil then Indent:=RunBool(PD^.Indent);
  if PD^.Wrap<>nil then Wrap:=RunBool(PD^.Wrap);
  if PD^.Just<>nil then Just:=RunBool(PD^.Just);
  if PD^.ColBlk<>nil then TypeB:=RunBool(PD^.ColBlk);
  if PD^.Left<>nil then LeftMarg:=maxi(1,RunInt(PD^.Left));
  if PD^.Right<>nil then RightMarg:=maxi(LeftMarg,mini(255,RunInt(PD^.Right)));
end;

procedure GetEditTxt(var pInsert,pIndent,pWrap,pJust,pColBlk:boolean;
                     var pLeftMarg,pRightMarg:integer);
begin
  pInsert:=Insert;pIndent:=Indent;pWrap:=Wrap;pJust:=Just;pColBlk:=TypeB;
  pLeftMarg:=LeftMarg;pRightMarg:=RightMarg;
end;

function EditText(pMode:char;pTxtType:char;pName:pathstr;pErrMsg:string;
                  pTxtPtr:CharArrPtr;pMaxLen:word;
                  var pLen{*},pInd{*}:word;var pScr{*}:longint;
                  pBreaks:string;pExD:EdExitDPtr;
                  var pSrch{*},pUpdat{*}:boolean;
                  pLastNr,pCtrlLastNr:word;pMsgS:MsgStrPtr):boolean;
  var oldEdOK:boolean;
  begin
    oldEdOK:=EdOK;EditT:=true;
    Mode:=pMode;TypeT:=pTxtType;NameT:=pName;ErrMsg:=pErrMsg;
    T:=pTxtPtr;MaxLenT:=pMaxLen;
    LenT:=pLen;IndT:=pInd;ScrT:=pScr and $FFFF;Posi:=pScr shr 16;
    Breaks:=pBreaks;ExitD:=pExD;
    SrchT:=pSrch;UpdatT:=pUpdat;
    LastNr:=pLastNr;CtrlLastNr:=pCtrlLastNr;
    if pMsgS<>nil then
      begin LastS:=pMsgS^.Last;CtrlLastS:=pMsgS^.CtrlLast;
        ShiftLastS:=pMsgS^.ShiftLast;AltLastS:=pMsgS^.AltLast;
        HeadS:=pMsgS^.Head end else
      begin LastS:=nil;CtrlLastS:=nil;ShiftLastS:=nil;AltLastS:=nil;
        HeadS:=nil end;
    if Mode<>HelpM then TxtColor:=textattr;
    FirstEvent:=not SrchT;
    if SrchT then
      begin SrchT:=false;KBDBuffer:=^l+KBDBuffer;KbdChar:=_L_;IndT:=0; end;

    Edit;
    if Mode<>HelpM then textattr:=TxtColor;
    pUpdat:=UpdatT;pSrch:=SrchT;pLen:=LenT;
    pInd:=IndT;pScr:=ScrT+longint(Posi) shl 16;
    EdOK:=oldEdOK;EditText:=EditT;
  end;

procedure SimpleEditText(pMode:char;pErrMsg,pName:string;TxtPtr:CharArrPtr;MaxLen:word;var Len{*},Ind{*}:word;
                  var Updat{*}:boolean);
  var Srch:boolean;Scr:longint;
  begin Srch:=false;Scr:=0;
    EditText(pMode,LocalT,pName,pErrMsg,TxtPtr,MaxLen,Len,Ind,Scr,'',nil,Srch,Updat,0,0,nil);
  end;

function FindText(const Pstr:string;Popt:ScreenStr;PTxtPtr:CharArrPtr;PLen:word):word;
  var I:word;f,o:string;r:boolean;tt:CharArrPtr;
  begin tt:=T;T:=PTxtPtr;f:=FindStr;o:=OptionStr;r:=Replace;
    FindStr:=Pstr;OptionStr:=Popt;Replace:=false;
    I:=1;if FindString(I,PLen+1) then FindText:=I else FindText:=0;
    FindStr:=f;OptionStr:=o;Replace:=r;T:=tt;
  end;

             {********TEXT-FILE**********}

(*
procedure EditTxtFile(LS:LongStrPtr;Mode:char;ExD:EdExitDPtr;
                      TxtPos:longint;Txtxy:longint;
                      V:WRectPtr;Atr:word;const Hd:string;
                      WFlags:byte;MsgS:MsgStrPtr);

var Srch,Upd:boolean; Size,L:longint;
    w1,w2,w3:longint; er:ExitRecord; p:pointer;
    Loc:boolean;
    Ind,oldInd:word;
    oldTxtxy:longint;
label 1,2,4;
begin
  if Atr=0 then Atr:=colors.tNorm; w2:=0; w3:=0; MarkStore(p);
  if V<>nil then begin
    w1:=PushW1(1,1,TxtCols,1,(WFlags and WPushPixel)<>0,false);
    w2:=PushW1(1,TxtRows,TxtCols,TxtRows,(WFlags and WPushPixel)<>0,false);
    w3:=PushWFramed(v^.C1,v^.R1,v^.C2,v^.R2,Atr,Hd,'',WFlags) end
  else begin w1:=PushW(1,1,TxtCols,TxtRows); TextAttr:=Atr end;
  NewExit(Ovr,er);goto 4;
  Loc:=LS<>nil;
  if not Loc then
    begin MaxLenT:=$FFF0;Part.UpdP:=false;
      TxtPath:=CPath;TxtVol:=CVol;OpenTxtFh(Mode);RdFirstPart;SimplePrintHead;
      while (TxtPos>Part.PosP+Part.LenP)and not AllRd do RdNextPart;
      Ind:=TxtPos-Part.PosP end
  else begin Ind:=TxtPos;L:=StoreInTWork(LS) end;
  oldInd:=Ind;oldTxtxy:=Txtxy;

  1:Srch:=false;Upd:=false;if not Loc then
    EditText(Mode,FileT,TxtPath,'',T,$FFF0,LenT,Ind,Txtxy,
             _F1+_F6+_F9+_CtrlF6+_AltF10,ExD,
             Srch,Upd,126,143,MsgS) else
    EditText(Mode,LocalT,'','',@LS^.A,MaxLStrLen,LS^.LL,Ind,Txtxy,
             _F1+_F6+_CtrlF6,ExD,
             Srch,Upd,126,143,MsgS);
  TxtPos:=Ind+Part.PosP;
  if Upd then EdUpdated:=true;
  if (KbdChar=_AltEqual_)or(KbdChar=_U_) then
    begin ReleaseStore(LS);LS:=TWork.Read(1,L);
      if KbdChar=_AltEqual_ then begin KbdChar:=_ESC_;goto 4 end
      else begin Ind:=oldInd;Txtxy:=oldTxtxy;goto 1 end;
    end;
  if not Loc then ReleaseStore(T);
  if EdBreak=$FFFF then case KbdChar of
    _F9_    : begin RdPart; goto 1 end;
    _AltF10_: begin Help(nil,'',false); goto 2 end;
    _F1_    : begin RdMsg(6); Help(RdbDPtr(HelpFD),MsgLine,false);
2:            if not Loc then RdPart; goto 1 end end;
  if not Loc then  begin Size:=FileSizeH(TxtFh); CloseH(TxtFh) end;
  if (EdBreak=$FFFF)and((KbdChar=_F6_)or(KbdChar=_CtrlF6_)) then
    if Loc then
      begin if KbdChar=_F6_ then PrintArray(@T^,LenT)
            else PrintArray(@T^[TxtPos],LenT-TxtPos+1);goto 1 end else
      begin CPath:=TxtPath; CVol:=TxtVol;
        if KbdChar=_F6_ then PrintTxtFile(0) else PrintTxtFile(TxtPos);
        OpenTxtFh(Mode); RdPart; goto 1 end;
  if not Loc and(Size<1) then DeleteFile(TxtPath);
  if Loc and(KbdChar=_Esc_) then LS^.LL:=LenT;
4:if Loc then begin p:=ReadDelInTWork(l);ReleaseStore(p) end;
  if w3<>0 then PopW2(w3,(WFlags and WNoPop)=0); if w2<>0 then PopW(w2);
  PopW(w1); ReleaseStore(p); LastTxtPos:=Ind+Part.PosP; RestoreExit(er);
end;
*)

procedure EditTxtFile(LP:LongintPtr;Mode:char;ErrMsg:ScreenStr;ExD:EdExitDPtr;
                      TxtPos:longint;Txtxy:longint;
                      V:WRectPtr;Atr:word;const Hd:string;
                      WFlags:byte;MsgS:MsgStrPtr);

var Srch,Upd:boolean; Size,L:longint;
    w1,w2,w3:longint; er:ExitRecord;
    Loc:boolean;
    Ind,oldInd:word;
    oldTxtxy:longint;
    LS:LongStrPtr;                               compErrTxt:string;
label 1,2,4;
begin
  if Atr=0 then Atr:=colors.tNorm; w2:=0; w3:=0;
  if V<>nil then begin
    w1:=PushW1(1,1,TxtCols,1,(WFlags and WPushPixel)<>0,false);
    w2:=PushW1(1,TxtRows,TxtCols,TxtRows,(WFlags and WPushPixel)<>0,false);
    w3:=PushWFramed(v^.C1,v^.R1,v^.C2,v^.R2,Atr,Hd,'',WFlags) end
  else begin w1:=PushW(1,1,TxtCols,TxtRows); TextAttr:=Atr end;
  NewExit(Ovr,er);goto 4;
  Loc:=LP<>nil;LocalPPtr:=LP;
  if not Loc then
    begin MaxLenT:=$FFF0;LenT:=0;Part.UpdP:=false;
      TxtPath:=CPath;TxtVol:=CVol;OpenTxtFh(Mode);RdFirstPart;SimplePrintHead;
      while (TxtPos>Part.PosP+Part.LenP)and not AllRd do RdNextPart;
      Ind:=TxtPos-Part.PosP end
  else begin LS:=TWork.Read(1,LP^);Ind:=TxtPos;L:=StoreInTWork(LS) end;
  oldInd:=Ind;oldTxtxy:=Txtxy;

  1:Srch:=false;Upd:=false;if not Loc then
    EditText(Mode,FileT,TxtPath,ErrMsg,T,$FFF0,LenT,Ind,Txtxy,
             _F1+_F6+_F9+_AltF10,ExD,
             Srch,Upd,126,143,MsgS) else
    EditText(Mode,LocalT,'',ErrMsg,@LS^.A,MaxLStrLen,LS^.LL,Ind,Txtxy,
             _F1+_F6,ExD,
             Srch,Upd,126,143,MsgS);
  TxtPos:=Ind+Part.PosP;
  if Upd then EdUpdated:=true;
  if (KbdChar=_AltEqual_)or(KbdChar=_U_) then
    begin ReleaseStore(LS);LS:=TWork.Read(1,L);
      if KbdChar=_AltEqual_ then begin KbdChar:=_ESC_;goto 4 end
      else begin Ind:=oldInd;Txtxy:=oldTxtxy;goto 1 end;
    end;
  if not Loc then ReleaseStore(T);
  if EdBreak=$FFFF then case KbdChar of
    _F9_    : begin if Loc then
                      begin TWork.Delete(LP^);LP^:=StoreInTWork(LS) end
                    else RdPart;
                goto 1 end;
    _AltF10_: begin Help(nil,'',false); goto 2 end;
    _F1_    : begin RdMsg(6); Help(RdbDPtr(HelpFD),MsgLine,false);
2:            if not Loc then RdPart; goto 1 end end;
  if not Loc then  begin Size:=FileSizeH(TxtFh); CloseH(TxtFh) end;
  if (EdBreak=$FFFF)and(KbdChar=_F6_) then
    if Loc then begin PrintArray(@T^,LenT,false); goto 1 end
    else begin CPath:=TxtPath; CVol:=TxtVol; PrintTxtFile(0);
       OpenTxtFh(Mode); RdPart; goto 1 end;
  if not Loc and(Size<1) then DeleteFile(TxtPath);
  if Loc and(KbdChar=_Esc_) then LS^.LL:=LenT;
4:if IsCompileErr then begin
    IsCompileErr:=false; compErrTxt:=MsgLine;
    SetMsgPar(compErrTxt); WrLLF10Msg(110) end;
  if Loc then
    begin TWork.Delete(L);
      TWork.Delete(LP^);LP^:=StoreInTWork(LS);ReleaseStore(LS) end;
  if w3<>0 then PopW2(w3,(WFlags and WNoPop)=0); if w2<>0 then PopW(w2);
  PopW(w1); LastTxtPos:=Ind+Part.PosP; RestoreExit(er);
end;

procedure ViewPrinterTxt;
const v:WRect=(C1:1;R1:2;C2:80;R2:24);
begin
  if not PrintView then exit; SetPrintTxtPath;
  v.C2:=TxtCols; v.R2:=TxtRows-1;
  EditTxtFile(nil,'T','',nil,1,0,@v,0,'',WPushPixel,nil);
end;


       {**********HELP*********}


const
  maxStk=15; iStk:word=0;
var
  Stk:array[1..maxStk] of record Rdb:RdbDPtr;FD:FileDPtr;iR,iT:word end;

procedure ClearHelpStkForCRdb;
var i:word;
begin
  i:=1; while i<=iStk do
  if Stk[i].Rdb=CRdb then begin move(Stk[i+1],Stk[i],(iStk-i)*12); dec(iStk) end
  else inc(i);
end;


procedure Help(R:RdbDPtr;Name:string;InCWw:boolean);

procedure ViewHelpText(S:LongStrPtr;var TxtPos:word);
var L:longint;
    Srch,Upd:boolean;
    Scr:longint;
label 1;
begin
  L:=SavePar;
  TxtColor:=colors.hNorm; fillchar(ColKey,8,colors.tCtrl);
  ColKey[5]:=colors.hSpec; ColKey[3]:=colors.hHili; ColKey[1]:=colors.hMenu;
  Srch:=false;Upd:=false;Scr:=0;
1:EditText(HelpM,MemoT,'','',@S^.A,$FFF0,S^.LL,TxtPos,Scr,
           _F1+_F10+_F6+_CtrlHome+_CtrlEnd,nil,Srch,Upd,142,145,nil);
  if KbdChar=_F6_ then begin PrintArray(@S^.A,S^.LL,true); goto 1 end;
  RestorePar(L);
end;
var
  p:pointer; er:ExitRecord; fd:FileDPtr;
  c1,c2,r1,r2:word; w,w2:longint; i,l,l2:word; iRec,oldIRec:word;
  s,s2:LongStrPtr; os:word absolute s; os2:word absolute s2;
  delta:integer; frst,byName,backw:boolean; cf,cf2:FileDPtr;
label 1,2,3,4;
begin
  if R=nil then begin
    if iStk=0 then exit; R:=Stk[iStk].Rdb; backw:=true end
  else begin if Name='' then exit; backw:=false end;
  if R=RdbDPtr(HelpFD) then begin
    fd:=HelpFD; if HelpFD^.Handle=$FF then begin WrLLF10Msg(57);exit end end
  else begin fd:=R^.HelpFD; if fd=nil then exit end;
  MarkStore(p); cf:=CFile; w:=0; w2:=0;
  NewExit(Ovr,er); goto 4;
  if InCWw then begin
    c1:=WindMin.X+1; c2:=WindMax.X+1; r1:=WindMin.Y+1; r2:=WindMax.Y+1;
    if (c1=1) and (c2=TxtCols) and (r1=2) and (r2=TxtRows) then r1:=1;
    w:=PushW(1,TxtRows,TxtCols,TxtRows); w2:=PushW1(c1,r1,c2,r2,true,true) end
  else w:=PushW1(1,1,TxtCols,TxtRows,true,true);
  i:=1; frst:=true; delta:=0; if backw then begin byName:=false; goto 3 end;
1:byName:=true;
2:s:=GetHlpText(R,Name,byName,iRec); cf2:=CFile;
  if s=nil then
    if frst and (R=RdbDPtr(HelpFD)) and (KbdChar=_CtrlF1_) then begin
      KbdChar:=0; Name:='Ctrl-F1 error'; goto 1 end
    else begin Set2MsgPar(Name,fd^.Name); WrLLF10Msg(146) end
  else begin
    frst:=false; byName:=false; s2:=s;
    if (s^.LL>0) and (s^.A[1]='{') then begin   { view after 1. line }
      l:=FindCtrlM(s,1,1); l:=SkipCtrlMJ(s,l)-1; inc(os2,l); s2^.LL:=s^.LL-l end;
    if s2^.LL=0 then if delta=0 then goto 4 else if iRec<>oldIRec then begin
      oldIRec:=iRec; inc(iRec,delta); goto 2 end;
    ViewHelpText(s2,i);
    if iStk<maxStk then inc(iStk) else move(Stk[2],Stk,sizeof(Stk)-4);
    with Stk[iStk] do begin Rdb:=R; FD:=cf2; iR:=iRec; iT:=i end;
    oldIRec:=iRec; i:=1; delta:=0; ReleaseStore(s); CFile:=cf2;
    case KbdChar of
     _ESC_:;
     _F10_:begin dec(iStk);
3:         if iStk>0 then begin
             with Stk[iStk] do begin
                R:=Rdb; CFile:=FD; iRec:=iR; i:=iT end;
             dec(iStk); goto 2 end end;
     _CtrlHome_:begin dec(iRec); delta:=-1; goto 2 end;
     _CtrlEnd_:begin inc(iRec); delta:=1; goto 2 end;
     else
       if KbdChar=_F1_ then Name:='root' else Name:=LexWord; goto 1;
    end end;
4:RestoreExit(er);
  if w2<>0 then PopW(w2); if w<>0 then PopW(w);
  ReleaseStore(p); CFile:=cf;
end;


procedure InitTxtEditor;
{var ppp:pointer;l:longint;}
begin
  FindStr[0]:=#0;ReplaceStr[0]:=#0;OptionStr[0]:=#0;Replace:=false;
  TxtColor:=colors.tNorm;BlockColor:=colors.tBlock;
  SysLColor:=colors.fNorm;
  ColKey[0]:=colors.tCtrl;move(colors.tUnderline,ColKey[1],7);
  RdMsg(411);InsMsg:=MsgLine;RdMsg(412);nInsMsg:=MsgLine;
  RdMsg(413);IndMsg:=MsgLine;RdMsg(414);WrapMsg:=MsgLine;
  RdMsg(415);JustMsg:=MsgLine;RdMsg(417);BlockMsg:=MsgLine;
  RdMsg(416);ViewMsg:=MsgLine;
  Insert:=true;Indent:=true;Wrap:=false;Just:=false;TypeB:=false;
  LeftMarg:=1;RightMarg:=78;
  CharPg:={char(250)} spec.TxtCharPg; InsPg:={true} spec.TxtInsPg ;
{ppp:=nil;l:=0;
while true do
begin ppp:=GetStore2(MaxLStrLen);inc(l,MaxLStrLen);
writeln('O.K ',l,' ',seg(ppp),' ',ofs(ppp));wait end;  }
end;


end.