
procedure UpwEdit(LkD:LinkDPtr);forward;

function TestAccRight(S:StringList):boolean;
begin
  if UserCode=0 then begin TestAccRight:=true; exit end;
  TestAccRight:=OverlapByteStr(ptr(seg(S^),ofs(S^)+5+length(S^.S)),@AccRight);
end;
function ForNavigate(FD:FileDPtr):boolean;
var S:StringList;
begin
  ForNavigate:=true; if UserCode=0 then exit; S:=FD^.ViewNames;
  while S<>nil do begin
    if TestAccRight(S) then exit; S:=S^.Chain end;
  ForNavigate:=false;
end;


procedure InsertRecProc(RP:pointer);
begin
  GotoRecFld(CRec,E^.FirstFld); IsNewRec:=true; LockRec(false);
  if RP<>nil then Move(RP^,CRecPtr^,CFile^.RecLen) else ZeroAllFlds;
  DuplOwnerKey; SetWasUpdated;
  IVOff; MoveDispl(E^.NRecs-1,E^.NRecs,E^.NRecs-IRec);
  FirstEmptyFld:=CFld; DisplRec(IRec); IVOn; NewDisplLL:=true;
  NewRecExit end;

procedure AppendRecord(RP:pointer);
 var Max:word;
 begin IVoff; IsNewRec:=true; Max:=E^.NRecs;
       CFld:=E^.FirstFld;FirstEmptyFld:=CFld;
       if IRec<Max then
          begin inc(IRec);MoveDispl(Max-1,Max,Max-IRec);DisplRec(IRec);IVon end
       else if Max=1 then begin inc(BaseRec);DisplWwRecsOrPage end
       else begin inc(BaseRec,Max-1);IRec:=2; DisplAllWwRecs end;
       if RP<>nil then Move(RP^,CRecPtr^,CFile^.RecLen) else ZeroAllFlds;
       DuplOwnerKey; DisplRecNr(CRec); SetWasUpdated; LockRec(false);
       NewRecExit;
  end;

function GotoXRec(PX:XStringPtr;var N:longint):boolean;
var md:LockMode; k:KeyDPtr;
begin
  md:=NewLMode(RdMode); k:=VK; if Subset then k:=WK;
  if SubSet or HasIndex then begin
    GotoXRec:=k^.SearchIntvl(PX^,false,n); n:=k^.PathToNr end
  else GotoXRec:=SearchKey(PX^,k,n);
  RdRec(CRec); GotoRecFld(n,CFld); OldLMode(md);
end;

function PromptAndSearch(Create:boolean):boolean;
procedure CreateOrErr(RP:pointer;N:longint);
begin
  if Create then if N>CNRecs then AppendRecord(RP) else InsertRecProc(RP)
  else if not NoSrchMsg then WrLLF10Msg(118);
end;
function FindEFld(F:FieldDPtr):EFldDPtr;
var D:EFldDPtr;
label 1;
begin
  D:=E^.FirstFld; while D<>nil do begin
    if D^.FldD=F then goto 1; D:=D^.Chain end;
1:FindEFld:=D;
end;
procedure PromptSearch;
var F,F2:FieldDPtr; FD,FD2:FileDPtr; RP,RP2:pointer; KF,KF2:KeyFldDPtr;
    n:longint; s:string; r:float; b,li,found:boolean; md:LockMode;
    x,xOld:XString; K:KeyDPtr; w:longint; Col,LWw,pos:word; D:EFldDPtr;
label 1,2,3;
begin
  FD:=CFile; K:=VK; if SubSet then K:=WK; KF:=K^.KFlds;
  RP:=GetRecSpace; CRecPtr:=RP; ZeroAllFlds; x.Clear;
  li:=F3LeadIn and not IsNewRec;
  w:=PushW1(1,TxtRows,TxtCols,TxtRows,true,false);
  if KF=nil then goto 1;
  if HasIndex and E^.DownSet and (VK=E^.DownKey) then begin
    FD2:=E^.DownLD^.ToFD; RP2:=E^.DownRecPtr; KF2:=E^.DownLD^.ToKey^.KFlds;
    CFile:=FD2; CRecPtr:=RP2;
    while KF2<>nil do begin
      CFile:=FD2; CRecPtr:=RP2; F:=KF^.FldD; F2:=KF2^.FldD;
      case F^.FrmlTyp of
        'S':begin s:=_ShortS(F2); x.StoreStr(s,KF);
              CFile:=FD; CRecPtr:=RP; s_(F,s) end;
        'R':begin r:=_r(F2); x.StoreReal(r,KF);
              CFile:=FD; CRecPtr:=RP; r_(F,r) end;
        'B':begin b:=_b(F2); x.StoreBool(b,KF);
              CFile:=FD; CRecPtr:=RP; b_(F,b) end;
      end;
      KF2:=KF2^.Chain; KF:=KF^.Chain end end;
  if KF=nil then begin
1:  PromptAndSearch:=true; CRecPtr:=E^.NewRecPtr; goto 3 end;
  while KF<>nil do begin
    F:=KF^.FldD; if li then begin
      D:=FindEFld(F); if D<>nil then GotoRecFld(CRec,D) end;
    gotoxy(1,TxtRows); TextAttr:=colors.pTxt; ClrEol; write(F^.Name,':');
    s:=''; pos:=1; Col:=WhereX;
    if Col+F^.L>TxtCols then LWw:=TxtCols-Col else LWw:=F^.L;
2:  TextAttr:=colors.pNorm; gotoxy(Col,TxtRows);
    pos:=FieldEdit(F,nil,LWw,pos,s,r,false,true,li,E^.WatchDelay);
    xOld:=x;
    if (KbdChar=_ESC_) or (Event.What=evKeyDown) then begin
      CRecPtr:=E^.NewRecPtr; goto 3 end;
    case F^.FrmlTyp of
      'S':begin x.StoreStr(s,KF); s_(F,s) end;
      'R':begin x.StoreReal(r,KF); r_(F,r) end;
      'B':begin b:=s[1]=AbbrYes; x.StoreBool(b,KF); b_(F,b) end end;
    if li then begin
      CRecPtr:=E^.NewRecPtr; found:=GotoXRec(@x,n);
      if (pos=0) and (F^.FrmlTyp='S') then begin
        x:=xOld; x.StoreStr(_ShortS(F),KF) end;
      CRecPtr:=RP; if pos<>0 then begin x:=xOld; goto 2 end end;
    KF:=KF^.Chain end;
  CRecPtr:=E^.NewRecPtr;
  if li then begin if not found then CreateOrErr(RP,n) end
  else if IsNewRec then Move(RP^,CRecPtr^,CFile^.RecLen)
  else if not GotoXRec(@x,n) then CreateOrErr(RP,n);
  PromptAndSearch:=true;
3:PopW(w); ReleaseStore(RP);
end;
begin
  PromptAndSearch:=false; if VK=nil then begin WrLLF10Msg(111); exit end;
  PromptSearch; GotoRecFld(CRec,E^.FirstFld);
end;

procedure PromptGotoRecNr;
var I:word; Txt:string; N:longint; Del:boolean;
begin
  I:=1; Txt:=''; Del:=true;
  repeat PromptLL(122,Txt,I,Del); if KbdChar=_ESC_ then exit; val(Txt,N,I); Del:=false;
  until I=0;
  GotoRecFld(N,CFld);
end;

function CompChk(D:EFldDPtr;Typ:char):ChkDPtr;
var C:ChkDPtr; w,f:boolean;
begin
  w:=WarnSwitch and (Typ in ['W','?']); f:=(Typ in ['F','?']);
  C:=D^.Chk; CompChk:=nil; while C<>nil do begin
    if (w and C^.Warning or f and not C^.Warning) and not RunBool(C^.Bool) then begin
      CompChk:=C; exit end;
    C:=C^.Chain end;
end;
procedure DisplChkErr(C:ChkDPtr);
procedure FindExistTest(Z:FrmlPtr;var LD:LinkDPtr);
  begin LD:=nil;if Z=nil then exit;
      case Z^.Op of
       _field:if Z^.Field^.Flg and f_Stored=0 then FindExistTest(Z^.Field^.Frml,LD);
       _access:if Z^.P1=nil then LD:=Z^.LD; {file.exist}
       #$60..#$af{1-ary}:FindExistTest(Z^.P1,LD);
       #$b0..#$ef{2-ary}:begin FindExistTest(Z^.P1,LD);
               if LD=nil then FindExistTest(Z^.P2,LD) end;
       #$f0..#$ff{3-ary}:begin FindExistTest(Z^.P1,LD);
               if LD=nil then begin FindExistTest(Z^.P2,LD);
               if LD=nil then FindExistTest(Z^.P3,LD) end end end;
  end;
var LD:LinkDPtr; cf:FileDPtr; cr:pointer; b:boolean; n:longint;
label 1;
begin
  FindExistTest(C^.Bool,LD);
  if not C^.Warning and (LD<>nil) and ForNavigate(LD^.ToFD)
     and CFld^.Ed(IsNewRec) then begin
    cf:=CFile; cr:=CRecPtr; b:=LinkUpw(LD,n,false); ReleaseStore(CRecPtr);
    CFile:=cf; CRecPtr:=cr;
    if not b then
      if NoShiftF7Msg then goto 1 else  F10SpecKey:=_ShiftF7_ end;
  if C^.HelpName<>nil then
    if F10SpecKey=_ShiftF7_ then F10SpecKey:=$fffe else F10SpecKey:=_F1_;
  SetMsgPar(RunShortStr(C^.TxtZ)); WrLLF10Msg(110);
  if KbdChar=_F1_ then Help(CFile^.ChptPos.R,C^.HelpName^,false)
  else if KbdChar=_ShiftF7_ then
1:    UpwEdit(LD);
end;



procedure CheckFromHere;
label 1;
var N:longint; D:EFldDPtr; C:ChkDPtr; md:LockMode;
begin
  D:=CFld; N:=CRec; md:=NewLMode(RdMode);
1:if not DeletedFlag then while D<>nil do begin
    C:=CompChk(D,'?'); if C<>nil then begin
      if BaseRec+E^.NRecs-1<N then BaseRec:=N;
      IRec:=N-BaseRec+1; CFld:=D; DisplWwRecsOrPage; OldLMode(md);
      DisplChkErr(C); exit end;
    D:=D^.Chain end;
  if N<CNRecs then begin
    inc(N); DisplRecNr(N); RdRec(N); D:=E^.FirstFld; goto 1 end;
  RdRec(CRec); DisplRecNr(CRec); OldLMode(md); WrLLF10Msg(120);
end;

procedure Sorting;
var SKRoot:KeyFldDPtr; p:pointer; er:ExitRecord; md:LockMode;
label 1,2;
begin
  SaveFiles; MarkStore(p);
  if not PromptSortKeys(E^.Flds,SKRoot) or (SKRoot=nil) then goto 2;
  if not TryLMode(ExclMode,md,1) then goto 2;
  NewExit(Ovr,er); goto 1;
  SortAndSubst(SKRoot); E^.EdUpdated:=true;
1:RestoreExit(er); CFile:=E^.FD; OldLMode(md);
2:ReleaseStore(p); CRecPtr:=E^.NewRecPtr; DisplAllWwRecs;
end;

procedure AutoReport;
var p:pointer; RO:RprtOptPtr; UM:FileUseMode;
begin
  MarkStore(p); RO:=GetRprtOpt; RO^.FDL.FD:=CFile; RO^.Flds:=E^.Flds;
  if Select then begin RO^.FDL.Cond:=E^.Bool; RO^.CondTxt:=E^.BoolTxt end;
  if SubSet then RO^.FDL.ViewKey:=WK else if HasIndex then RO^.FDL.ViewKey:=VK;
  PrintView:=false; if SelForAutoRprt(RO) then begin
    SpecFDNameAllowed:=IsCurrChpt;
    RunAutoReport(RO);
    SpecFDNameAllowed:=false end;
  ReleaseStore(p); ViewPrinterTxt; CRecPtr:=E^.NewRecPtr;
end;

procedure AutoGraph;
var Bool:FrmlPtr; K:KeyDPtr;
begin {$ifdef FandGraph}
  Bool:=nil; if Select then Bool:=E^.Bool;
  K:=nil; if SubSet then K:=WK else if HasIndex then K:=VK;
  RunAutoGraph(E^.Flds,K,Bool);{$endif}
  CFile:=E^.FD; CRecPtr:=E^.NewRecPtr;
end;

function IsDependItem:boolean;
  var Dp:DepDPtr;
  begin IsDependItem:=false; if not IsNewRec and (E^.NEdSet=0) then exit;
      Dp:=CFld^.Dep;while Dp<>nil do
      begin if RunBool(Dp^.Bool) then begin IsDependItem:=true;exit end;
         Dp:=Dp^.Chain end;
  end;
procedure SetDependItem;
  var Dp:DepDPtr;
  begin Dp:=CFld^.Dep;while Dp<>nil do
      begin if RunBool(Dp^.Bool) then begin AssignFld(CFld^.FldD,Dp^.Frml);exit end;
         Dp:=Dp^.Chain end;
  end;

procedure SwitchToAppend;
  begin GotoRecFld(CNRecs,CFld); Append:=true;AppendRecord(nil);
        NewDisplLL:=true end;


function CtrlMProc(Mode:word):boolean;
function FldInModeF3Key(F:FieldDPtr):boolean;
var KF:KeyFldDPtr;
begin
  FldInModeF3Key:=false; if F^.Flg and f_Stored=0 then exit;
  KF:=VK^.KFlds; while KF<>nil do begin
    if KF^.FldD=F then begin FldInModeF3Key:=true;exit end; KF:=KF^.Chain end;
end;
function IsSkipFld(D:EFldDPtr):boolean;
begin with D^ do begin
  IsSkipFld:=not Tab and ((E^.NTabsSet>0) or (FldD^.Flg and f_Stored=0)
    or OnlySearch and FldInModeF3Key(FldD));
end end;
function ExNotSkipFld:boolean;
var D:EFldDPtr;
begin
  ExNotSkipFld:=false; if E^.NFlds=1 then exit;
  D:=E^.FirstFld; while D<>nil do begin
    if (D<>CFld) and not IsSkipFld(D) then begin ExNotSkipFld:=true; exit end;
    D:=D^.Chain end;
end;
function CheckForExit(var Quit:boolean):boolean;
var X:EdExitDPtr; b:boolean;
begin
  CheckForExit:=false; X:=E^.ExD;
  while X<>nil do begin
    b:=FieldInList(CFld^.FldD,X^.Flds); if X^.NegFlds then b:=not b;
    if b then if X^.Typ='Q' then Quit:=true else begin
      EdBreak:=12; LastTxtPos:=-1; if not StartExit(X,true) then exit end;
    X:=X^.Chain end;
  CheckForExit:=true;
end;

var OldCRec,i:longint; OldCFld:EFldDPtr; b:boolean;
    C:ChkDPtr; X:EdExitDPtr; Brk,NR:word; KL:KeyList;
    displ,skip,Quit,WasNewRec:boolean; md:LockMode; Typ:char;
label 1,2,3,4;
begin
  OldCRec:=CRec; OldCFld:=CFld; CtrlMProc:=true; NR:=0;
  if Mode=0 {only bypass unrelevant fields} then goto 2;
1:if IsFirstEmptyFld then FirstEmptyFld:=FirstEmptyFld^.Chain;
  Quit:=false; if not CheckForExit(Quit) then exit;
  TextAttr:=E^.dHili; DisplFld(CFld,IRec);
  if ChkSwitch then begin
    if Mode in [1,3] then typ:='?' else typ:='F';
    C:=CompChk(CFld,typ); if C<>nil then begin
      DisplChkErr(C); if not C^.Warning then exit end end;
  if WasUpdated and not EdRecVar and HasIndex then begin
    KL:=CFld^.KL; while KL<>nil do begin
      md:=NewLMode(RdMode); b:=TestDuplKey(KL^.Key); OldLMode(md);
      if b then begin DuplKeyMsg(KL^.Key); exit end; KL:=KL^.Chain end end;
  if Quit and not IsNewRec and (Mode in [1,3]) then begin
    EdBreak:=12; CtrlMProc:=false; exit end;
  if CFld^.Chain<>nil then begin
    GotoRecFld(CRec,CFld^.Chain); if Mode in [1,3] then Mode:=0 end
  else begin
    WasNewRec:=IsNewRec; Mode:=0; inc(NR);
    if not WriteCRec(true,Displ) then exit;
    if Displ then DisplAllWwRecs else SetRecAttr(IRec);
    if Only1Record then
      if NoESCPrompt then begin EdBreak:=0; CtrlMProc:=false; exit end
      else begin Append:=false; goto 3 end;
    if OnlySearch then begin Append:=false; goto 3 end;
    if Append then AppendRecord(nil)
    else begin
      if WasNewRec then NewDisplLL:=true;
      if CRec<CNRecs then
        if Select then begin
          for i:=CRec+1 to CNRecs do begin
            if KeyPressed and (ReadKey<>_M_) and PromptYN(23) then goto 4;
            RdRec(I); DisplRecNr(i); if not DeletedFlag and RunBool(E^.Bool) then begin
              RdRec(CRec); GotoRecFld(I,E^.FirstFld); goto 2 end end;
4:        RdRec(CRec); DisplRecNr(CRec);
          GotoRecFld(OldCRec,OldCFld); beep; beep; exit end
        else GotoRecFld(CRec+1,E^.FirstFld)
      else 3: begin
        GotoRecFld(CRec,OldCFld); beep; beep; exit end end;
    end;
2:skip:=false; displ:=false;
  if IsFirstEmptyFld then begin
    if (CFld^.Impl<>nil) and LockRec(true) then begin
      AssignFld(CFld^.FldD,CFld^.Impl); displ:=true end;
    if CFld^.Dupl and (CRec>1) and LockRec(true) then begin
        DuplFromPrevRec; displ:=true; skip:=true end end;
  if IsDependItem and LockRec(true) then begin
    SetDependItem; displ:=true; skip:=true end;
  if IsSkipFld(CFld) then skip:=true; if CFld^.Tab then skip:=false;
  if displ then begin TextAttr:=E^.dHiLi; DisplFld(CFld,IRec) end;
  if Mode=2 {bypass all remaining fields of the record } then goto 1;
  if skip and ExNotSkipFld and (NR<=1) then goto 1;
end;

function GoPrevNextRec(Delta:integer; Displ:boolean):boolean;
var i,D,OldBaseRec:longint; md:LockMode; w,Max:word;
label 0,1,2,3,4;
begin
  GoPrevNextRec:=false; if EdRecVar then exit; md:=NewLMode(RdMode); i:=CRec;
  if Displ then IVoff;
0:inc(i,Delta); if (i>0) and (i<=CNRecs) then begin
    RdRec(i); if Displ then DisplRecNr(i);
    if not Select or not DeletedFlag and RunBool(E^.Bool) then goto 2;
    if KeyPressed then begin w:=ReadKey;
      if ((Delta>0) and (w<>_Down_) and (w<>_CtrlEnd_) and (w<>_PgDn_)
       or (Delta<0) and (w<>_Up_) and (w<>_CtrlHome_) and (w<>_PgUp_))
       and PromptYN(23) then goto 1 end;
    goto 0 end;
  if Select then WrLLF10Msg(16);
1:RdRec(CRec);
  if Displ then begin DisplRecNr(CRec); IVon end; goto 4;
2:GoPrevNextRec:=true; OldBaseRec:=BaseRec; SetNewCRec(i,false);
  if Displ then begin
    Max:=E^.NRecs; D:=BaseRec-OldBaseRec;
    if abs(D)>=Max then begin DisplWwRecsOrPage;goto 3 end;
    if D>0 then begin
      MoveDispl(D+1,1,Max-D); for i:=Max-D+1 to Max do DisplRec(i) end
    else if D<0 then begin
      D:=-D; MoveDispl(Max-D,Max,Max-D); for i:=1 to D do DisplRec(i) end end;
3:if Displ then IVon;
4:OldLMode(md);
end;

function GetChpt(const Heslo:string;var NN:longint):boolean;
var j:longint; s:string[12]; i:integer;
label 1;
begin
  GetChpt:=true; for j:=1 to CFile^.NRecs do begin
    ReadRec(j); if IsCurrChpt then begin
      s:=TrailChar(' ',_shorts(ChptName)); i:=pos('.',s);
      if i>0 then delete(s,i,255); if SEquUpcase(Heslo,s) then goto 1 end
    else begin
      s:=TrailChar(' ',_shorts(CFile^.FldD));
      ConvToNoDiakr(s[1],length(s),Fonts.VFont);
      if EqualsMask(@Heslo[1],length(Heslo),s) then
1:      begin NN:=j; exit end end end;
  RdRec(CRec); GetChpt:=false;
end;

procedure SetCRec(I:longint);
begin
  if I>BaseRec+E^.NRecs-1 then BaseRec:=I-E^.NRecs+1
  else if I<BaseRec then BaseRec:=I;
  IRec:=I-BaseRec+1; RdRec(CRec);
end;

procedure UpdateEdTFld(S:LongStrPtr);
var md:LockMode;
begin
  CRecPtr:=E^.NewRecPtr; if not EdRecVar then md:=NewLMode(WrMode);
  SetWasUpdated; DelDifTFld(E^.NewRecPtr,E^.OldRecPtr,CFld^.FldD);
  longs_(CFld^.FldD,S); if not EdRecVar then OldLMode(md);
end;
procedure UpdateTxtPos(TxtPos:word);
var md:LockMode;
begin if IsCurrChpt then begin
  md:=NewLMode(WrMode); SetWasUpdated; r_(ChptTxtPos,integer(TxtPos)); OldLMode(md);
end end;

function EditFreeTxt(F:FieldDPtr;ErrMsg:ScreenStr;Ed:boolean;var Brk:word):boolean;
const BreakKeys:string[6]={$ifndef FandRunV}_CtrlF1+{$endif}
              _F1+_CtrlHome+_CtrlEnd+_F9+_AltF10;
      BreakKeys1:string[8]=_CtrlF1+_F1+_CtrlHome+_CtrlEnd+_F9+_AltF10+_ShiftF1+_F10;
      BreakKeys2:string[14]=_F1+_CtrlHome+_CtrlEnd+_F9+_F10+_AltF10+
              _CtrlF1+_AltF1+_ShiftF1+_AltF2+_AltF3+_CtrlF8+_CtrlF9+_AltF9;
      maxStk=10;
var Breaks:string[14]; Srch,Upd,WasUpd,Displ,quit:boolean; HdTxt:string[22];
    TxtMsgS:MsgStr; PTxtMsgS:MsgStrPtr;
    TxtXY:longint;
    R1,OldTxtPos,TxtPos,CtrlMsgNr,C,LastLen:word; S:LongStrPtr;
    Kind:char; md:LockMode; p:pointer; i,w:longint;   X:EdExitDPtr;
    iStk:word;  Stk:array[1..maxStk] of record N,I:longint end;
    heslo:string[80];
label 1,2,3,31,4,5,6;
begin
  MarkStore(p); Srch:=false; Brk:=0; TxtPos:=1; iStk:=0; TxtXY:=0;
  EditFreeTxt:=true;
  w:=0; if E^.Head^='' then w:=PushW(1,1,TxtCols,1);
  if E^.TTExit then
     with TxtMsgS do begin Head:=nil;Last:=E^.Last;CtrlLast:=E^.CtrlLast;
                       AltLast:=E^.AltLast;ShiftLast:=E^.ShiftLast;
                       PTxtMsgS:=@TxtMsgS end
  else PTxtMsgS:=nil;
1:HdTxt:='    ';  WasUpd:=false;
  if CRec>1 then HdTxt[3]:=^x; if CRec<CNRecs then HdTxt[4]:=^y;
  if IsCurrChpt then begin
      HdTxt:=_shorts(ChptTyp)+':'+_shorts(ChptName)+HdTxt;
      TxtPos:=trunc(_r(ChptTxtPos)); Breaks:=BreakKeys2; CtrlMsgNr:=131 end
  else begin CtrlMsgNr:=151;
    if CFile=CRdb^.HelpFD then Breaks:=BreakKeys1 else Breaks:=BreakKeys end;
  R1:=E^.FrstRow; if (R1=3) and WithBoolDispl then R1:=2;
  window(E^.FrstCol,R1,E^.LastCol,E^.LastRow); TextAttr:=colors.tNorm;
  Kind:='V'; OldTxtPos:=TxtPos;
  if ed then LockRec(false);
  if F^.Flg and f_Stored<>0 then begin S:=_longs(F);if ed then Kind:='T' end
  else S:=RunLongStr(F^.Frml);
2:X:=nil; if TTExit then X:=E^.ExD;Upd:=false;
  EditFreeTxt:=
  EditText(Kind,MemoT,HdTxt,ErrMsg,@S^.A,MaxLStrLen,S^.LL,TxtPos,TxtXY,Breaks,X,
             Srch,Upd,141,CtrlMsgNr,PTxtMsgS);
  ErrMsg:=''; heslo:=LexWord; LastLen:=S^.LL;
  if EdBreak=$ffff then C:=KbdChar else C:=0;
  if C=_AltEqual_ then C:=_ESC_ else WasUpd:=WasUpd or Upd;
  case C of
    _AltF3_:begin EditHelpOrCat(C,0,''); goto 2 end;
    _U_: begin ReleaseStore(S); TxtXY:=0; goto 1 end;
  end;
  window(1,1,TxtCols,TxtRows);
  if WasUpd then UpdateEdTFld(S);
  if (OldTxtPos<>TxtPos) and not Srch then UpdateTxtPos(TxtPos);
  ReleaseStore(S);
  if ed and not WasUpdated then UnlockRec(E);
  if Srch then if WriteCRec(false,Displ) then goto 31;
  case C of
    _F9_    :begin if WriteCRec(false,Displ) then begin
               SaveFiles; UpdCount:=0 end; goto 4 end;
    _F1_    :begin RdMsg(6); heslo:=MsgLine; goto 3 end;
    _CtrlF1_:goto 3;
    _ShiftF1_:if IsCurrChpt or (CFile=CRdb^.HelpFD) then begin
             if (iStk<maxStk) and WriteCRec(false,Displ) and GetChpt(heslo,i) then begin
               Append:=false; inc(iStk); with Stk[iStk] do begin N:=CRec; I:=TxtPos end;
               SetCRec(i) end;
             TxtXY:=0; goto 4 end;
    _F10_   :begin if (iStk>0) and WriteCRec(false,Displ) then begin
               Append:=false; with Stk[iStk] do begin SetCRec(N); TxtPos:=I end;
               dec(iStk) end;
             TxtXY:=0; goto 4 end;
    _AltF10_:begin Help(nil,'',false); goto 4 end;
    _AltF1_ :begin heslo:=_shorts(ChptTyp);
3:                 Help(RdbDPtr(HelpFD),heslo,false); goto 4 end end;
  if (C>$FF) and WriteCRec(false,Displ) then begin Append:=false;
    if C=_CtrlHome_ then begin GoPrevNextRec(-1,false); TxtXY:=0; goto 4 end;
    if C=_CtrlEnd_ then begin
31:   if not GoPrevNextRec(+1,false) and Srch then begin
        UpdateTxtPos(LastLen); Srch:=false end;
      TxtXY:=0;
4:    if not ed or LockRec(false) then goto 1 else goto 5 end;
    WrEStatus; Brk:=1; KbdChar:=C; goto 6 end;
5:ReleaseStore(p);
  DisplEditWw;
6:if w<>0 then PopW(w);
end;

function EditItemProc(del,ed:boolean;var Brk:word):boolean;
var F:FieldDPtr; Txt:string; R:float; b:boolean; C:ChkDPtr; wd:word;
begin
  F:=CFld^.FldD; EditItemProc:=true;
  if F^.Typ='T' then begin if not EditFreeTxt(F,'',ed,Brk) then begin
    EditItemProc:=false; exit end end
  else begin TextAttr:=E^.dHiLi;
    DecodeField(F,CFld^.FldD^.L,Txt); GotoXY(CFld^.Col,FldRow(CFld,IRec));
    wd:=0; if CFile^.NotCached then wd:=E^.WatchDelay;
    FieldEdit(F,CFld^.Impl,CFld^.L,1,Txt,R,del,ed,false,wd);
    if (KbdChar=_ESC_) or not ed then begin
      DisplFld(CFld,IRec); if ed and not WasUpdated then UnLockRec(E); exit end;
    SetWasUpdated;
    case F^.FrmlTyp of
     'B':b_(F,upcase(Txt[1])=AbbrYes);
     'S':s_(F,Txt);
     'R':r_(F,R);  end end;
  if Brk=0 then EditItemProc:=CtrlMProc(1);
end;
procedure SetSwitchProc;
var B:boolean; N,iMsg:word;
label 1;
begin
  iMsg:=104; if EdRecVar then goto 1; iMsg:=101;
  if MustCheck then if MustAdd then goto 1 else begin iMsg:=102; goto 1 end;
  iMsg:=103; if MustAdd then goto 1; iMsg:=100;
1:N:=Menu(iMsg,1); if N=0 then exit;
  case iMsg of
    101: if N=4 then N:=6;
    102: if N=5 then N:=6;
    103: if N>=4 then inc(N);
    104: inc(N,2) end;
  case N of
   1:begin if Select then Select:=False else if E^.Bool<>nil then Select:=True;
           DisplBool; NewDisplLL:=true; SetNewWwRecAttr end;
   2:if CFld^.FldD^.Flg and f_Stored<>0 then begin
           B:=CFld^.Dupl;CFld^.Dupl:=not B;DisplTabDupl;
           if B then dec(E^.NDuplSet) else inc(E^.NDuplSet) end;
   3:begin B:=CFld^.Tab;CFld^.Tab:=not B;DisplTabDupl;
           if B then dec(E^.NTabsSet) else inc(E^.NTabsSet) end;
   4:begin AddSwitch:=not AddSwitch; NewDisplLL:=true end;
   5:if not MustCheck then begin
       ChkSwitch:=not ChkSwitch; NewDisplLL:=true end;
   6:begin WarnSwitch:=not WarnSwitch; NewDisplLL:=true end;
  end;
end;
procedure PromptSelect;
var Txt:string;
begin
  if Select then Txt:=E^.BoolTxt^ else Txt:='';
  if IsCurrChpt then ReleaseFDLDAfterChpt;
  ReleaseStore(E^.AfterE);
  PromptFilter(Txt,E^.Bool,E^.BoolTxt);
  if E^.Bool=nil then Select:=false else Select:=true;
  DisplBool; SetNewWwRecAttr; NewDisplLL:=true;
end;

procedure SwitchRecs(Delta:integer);
var
  md:LockMode; n1,n2:longint; p1,p2:pointer; x1,x2:XString; k:KeyDPtr;
label 1;
begin
{$ifdef FandSQL} if CFile^.IsSQLFile then exit; {$endif}
  if NoCreate and NoDelete or WasWK then exit;
  if not TryLMode(WrMode,md,1) then exit;
  p1:=GetRecSpace; p2:=GetRecSpace;
  CRecPtr:=p1; n1:=AbsRecNr(CRec); ReadRec(n1);
  if HasIndex then x1.PackKF(VK^.KFlds);
  CRecPtr:=p2; n2:=AbsRecNr(CRec+Delta); ReadRec(n2);
  if HasIndex then begin x2.PackKF(VK^.KFlds); if x1.S<>x2.S then goto 1 end;
  WriteRec(n1); CRecPtr:=p1; WriteRec(n2);
  if HasIndex then begin
    k:=CFile^.Keys; while k<>nil do begin
      if k<>VK then begin
        CRecPtr:=p1; k^.Delete(n1); CRecPtr:=p2; k^.Delete(n2);
        CRecPtr:=p1; k^.Insert(n2,true); CRecPtr:=p2; k^.Insert(n1,true) end;
      k:=k^.Chain end end;
  SetNewCRec(CRec+Delta,true); DisplAllWwRecs; DisplRecNr(CRec); E^.EdUpdated:=true;
  if IsCurrChpt then SetCompileAll;
1:OldLMode(md); ReleaseStore(p1); CRecPtr:=E^.NewRecPtr;
end;
