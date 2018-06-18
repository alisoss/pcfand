function SelFldsForEO(EO:EditOptPtr;LD:LinkDPtr):boolean;
function FinArgs(F:FieldDPtr):boolean;
var KF:KeyFldDPtr;
begin
  FinArgs:=true; KF:=LD^.Args; while KF<>nil do begin
    if KF^.FldD=F then exit; KF:=KF^.Chain end;
  FinArgs:=false;
end;
var F:FieldDPtr; FL,FL1:FieldList; s:string; p:pointer;
begin
  SelFldsForEO:=true; if EO^.Flds=nil then exit; FL:=EO^.Flds;
  if not EO^.UserSelFlds then begin
    if LD<>nil then begin
      FL1:=FieldList(@EO^.Flds); while FL<>nil do begin
        if not FinArgs(FL^.FldD) then begin FL1^.Chain:=FL; FL1:=FL end;
        FL:=FL^.Chain end;
      FL1^.Chain:=nil end;
    exit end;
  MarkStore(p); while FL<>nil do begin
    F:=FL^.FldD; if (LD=nil) or not FinArgs(F) then begin
      s:=F^.Name;if F^.Flg and f_Stored=0 then s:=SelMark+s; PutSelect(s) end;
    FL:=FL^.Chain end;
  if EO^.Flds=nil then WrLLF10Msg(156) else SelFieldList(36,true,EO^.Flds);
  if EO^.Flds=nil then begin ReleaseStore(p); SelFldsForEO:=false end;
end;

function GetFileViewName(FD:FileDPtr;var SL:StringList):string;
begin
  if SL=nil then begin GetFileViewName:=FD^.Name;exit end;
  while not TestAccRight(SL) do SL:=SL^.Chain;
  GetFileViewName:=^A+SL^.S;
  repeat SL:=SL^.Chain until (SL=nil) or TestAccRight(SL);
end;

function EquFileViewName(FD:FileDPtr;S:ScreenStr;var EO:EditOptPtr):boolean;
var SL:StringList; cf:FileDPtr;
label 1;
begin
  EquFileViewName:=true; cf:=CFile; CFile:=FD;
  if S[1]=^A then begin
    S:=copy(S,2,255); SL:=CFile^.ViewNames;
    while SL<>nil do begin
      if SL^.S=S then begin EO:=GetEditOpt; RdUserView(S,EO); goto 1 end;
      SL:=SL^.Chain end end
  else if S=CFile^.Name then begin
    EO:=GetEditOpt;EO^.Flds:=AllFldsList(CFile,false);exit end;
  EquFileViewName:=false;
1:CFile:=cf;
end;

function EquRoleName(S:ScreenStr;LD:LinkDPtr):boolean;
begin
  if S='' then EquRoleName:=LD^.ToFD^.Name=LD^.RoleName
  else EquRoleName:=S=LD^.RoleName;
end;

procedure GetSel2S(var s,s2:ScreenStr;C:char;wh:word);
var i:word; s1:ScreenStr;
begin
  s:=GetSelect;s2:=''; i:=pos(C,s); if i>0 then
  if wh=1 then begin
    s1:=copy(s,i+1,255); s2:=copy(s,1,i-1);s:=s1 end
  else begin s2:=copy(s,i+1,255);s:=copy(s,1,i-1) end;
end;

procedure UpwEdit(LkD:LinkDPtr);
procedure SetPointTo(LD:LinkDPtr;var s1,s2:ScreenStr);
var KF:KeyFldDPtr;
begin
  KF:=LD^.Args; while KF<>nil do begin
    if KF^.FldD=CFld^.FldD then begin s2:=s1; ss.PointTo:=StringPtr(@s2) end;
    KF:=KF^.Chain end;
end;
var p:pointer; s,s1,s2:ScreenStr; x:XString; px:XStringPtr;
    F:FieldDPtr; KF:KeyFldDPtr; K:KeyDPtr; EO:EditOptPtr;
    Brk:word; ToFD:FileDPtr; SL,SL1:StringList; LD:LinkDPtr;
    w:longint; b:boolean;
label 1;
begin
  MarkStore(p); w:=PushW1(1,1,TxtCols,TxtRows,true,true); CFile^.IRec:=AbsRecNr(CRec);
  WrEStatus;
  if LkD=nil then begin
    LD:=LinkDRoot; while LD<>nil do begin ToFD:=LD^.ToFD;
      if (LD^.FromFD=CFile) and ForNavigate(ToFD) then begin
        s:=''; if ToFD^.Name<>LD^.RoleName then s:='.'+LD^.RoleName;
        SL:=ToFD^.ViewNames;
        repeat s1:=GetFileViewName(ToFD,SL)+s;
          PutSelect(s1);
          SetPointTo(LD,s1,s2) until SL=nil end;
      LD:=LD^.Chain end;
    ss.Abcd:=true; SelectStr(0,0,35,''); if KbdChar=_ESC_ then goto 1;
    GetSel2S(s1,s2,'.',2);LD:=LinkDRoot;
    while not ((LD^.FromFD=CFile) and EquRoleName(s2,LD)
          and EquFileViewName(LD^.ToFD,s1,EO)) do
          LD:=LD^.Chain end
  else begin
    LD:=LkD; EO:=GetEditOpt; EO^.UserSelFlds:= false; CFile:=LD^.ToFD;
    SL:=CFile^.ViewNames; SL1:= nil;  while SL<>nil do begin
      if TestAccRight(SL) then SL1:=SL; SL:=SL^.Chain end;
    if SL1=nil then EO^.Flds:=AllFldsList(CFile,false)
    else RdUserView(SL1^.S,EO);
    EO^.SetOnlyView:=true end;
  CFile:=E^.FD; x.PackKF(LD^.Args); px:=@x;
  K:=LD^.ToKey; CFile:=LD^.ToFD;
  if EO^.ViewKey=nil then EO^.ViewKey:=K
  else if AbsAdr(EO^.ViewKey)<>AbsAdr(K) then px:=nil;
  if SelFldsForEO(EO,nil) then begin
    NewEditD(CFile,EO);
    E^.ShiftF7LD:=LkD; if OpenEditWw then RunEdit(px,Brk);
    SaveFiles; PopEdit end;
1:PopW(w); ReleaseStore(p); RdEStatus; DisplEditWw;
end;

procedure ImbeddEdit;
var p:pointer; s,s1,s2:ScreenStr; Brk:word; SL:StringList;
    EO:EditOptPtr; FD:FileDPtr; R:RdbDPtr; w:longint;
label 1;
begin
  MarkStore(p); w:=PushW1(1,1,TxtCols,TxtRows,true,true); CFile^.IRec:=AbsRecNr(CRec);
  WrEStatus; R:=CRdb;
  while R<>nil do begin
    FD:=R^.FD^.Chain; while FD<>nil do begin
      if ForNavigate(FD) then begin
        SL:=FD^.ViewNames;
        repeat s:=GetFileViewName(FD,SL); if R<>CRdb then s:=R^.FD^.Name+'.'+s;
          PutSelect(s) until SL=nil end;
      FD:=FD^.Chain end;
    R:=R^.ChainBack end;
  ss.Abcd:=true; SelectStr(0,0,35,''); if KbdChar=_ESC_ then goto 1;
  GetSel2S(s1,s2,'.',1);
  R:=CRdb; if s2<>'' then repeat R:=R^.ChainBack until R^.FD^.Name=s2;
  CFile:=R^.FD; while not EquFileViewName(CFile,s1,EO) do CFile:=CFile^.Chain;
  if SelFldsForEO(EO,nil) then begin
    NewEditD(CFile,EO); if OpenEditWw then RunEdit(nil,Brk);
    SaveFiles; PopEdit end;
1:PopW(w); ReleaseStore(p); RdEStatus; DisplEditWw;
end;

procedure DownEdit;
var
  LD:LinkDPtr; FD:FileDPtr; SL:StringList; K:KeyDPtr;
  EO:EditOptPtr; Brk,i:word; p:pointer; s,s1,s2:ScreenStr; w:longint;
label 1;
begin
  MarkStore(p); w:=PushW1(1,1,TxtCols,TxtRows,true,true); CFile^.IRec:=AbsRecNr(CRec);
  WrEStatus;
  LD:=LinkDRoot; while LD<>nil do begin FD:=LD^.FromFD;
    if (LD^.ToFD=CFile) and ForNavigate(FD) and (LD^.IndexRoot<>0)
      {own key with equal beginning} then begin
      SL:=FD^.ViewNames; K:=GetFromKey(LD);
      repeat s:=GetFileViewName(FD,SL);
        if K^.Alias^<>'' then s:=s+'/'+K^.Alias^; PutSelect(s);
      until SL=nil end;
    LD:=LD^.Chain end;
  ss.Abcd:=true; SelectStr(0,0,35,''); if KbdChar=_ESC_ then goto 1;
  GetSel2S(s1,s2,'/',2);
  LD:=LinkDRoot;
  while (LD^.ToFD<>E^.FD) or (LD^.IndexRoot=0) or (s2<>GetFromKey(LD)^.Alias^)
     or not EquFileViewName(LD^.FromFD,s1,EO) do LD:=LD^.Chain;
  CFile:=LD^.FromFD;
  if SelFldsForEO(EO,LD) then begin
    EO^.DownLD:=LD; EO^.DownRecPtr:=CRecPtr;
    NewEditD(CFile,EO);
    if OpenEditWw then RunEdit(nil,Brk); SaveFiles; PopEdit end;
1:PopW(w); ReleaseStore(p); RdEStatus; DisplEditWw;
end;

procedure ShiftF7Proc;
var F: FieldDPtr; KF: KeyFldDPtr; LD,LD1: LinkDPtr;
begin
  { find last (first decl.) foreign key link with CFld as an argument }
  F:= CFld^.FldD; LD:= LinkDRoot; LD1:=nil;
  while LD<>nil do begin
    KF:= LD^.Args; while KF<>nil do begin
      if (KF^.FldD=F) and ForNavigate(LD^.ToFD) then LD1:=LD;
      KF:=KF^.Chain end;
    LD:=LD^.Chain end;
  if LD1<>nil then UpwEdit(LD1);
end;

function ShiftF7Duplicate:boolean;
var ee:EditDPtr; kf,kf2:KeyFldDPtr;
begin
  ShiftF7Duplicate:=false; ee:=E^.PrevE;
  with ee^ do begin CFile:=FD; CRecPtr:=NewRecPtr;
    if not ELockRec(ee,CFile^.IRec,IsNewRec,SubSet) then exit;
    if not WasUpdated then begin
      Move(CRecPtr^,OldRecPtr^,CFile^.RecLen); WasUpdated:=true end;
    kf:=E^.ShiftF7LD^.Args; kf2:= E^.ShiftF7LD^.ToKey^.KFlds;
    while kf<>nil do begin
      DuplFld(E^.FD,CFile,E^.NewRecPtr,CRecPtr,OldRecPtr,kf2^.FldD,kf^.FldD);
      kf:=kf^.Chain; kf2:=kf2^.Chain end;
    SetUpdFlag end;
  CFile:=E^.FD; CRecPtr:=E^.NewRecPtr; ShiftF7Duplicate:=true;
  KbdBuffer:=^m+KbdBuffer;
end;

function DuplToPrevEdit:boolean;
var ee:EditDPtr; f1,f2:FieldDPtr; md:LockMode;
begin
  DuplToPrevEdit:=false; ee:=E^.PrevE; if ee=nil then exit;
  f1:=CFld^.FldD; with ee^ do begin
    f2:=CFld^.FldD;
    if (f2^.Flg and f_Stored=0) or (f1^.Typ<>f2^.Typ) or (f1^.L<>f2^.L)
       or (f1^.M<>f2^.M) or not CFld^.Ed(IsNewRec) then begin
      WrLLF10Msg(140); exit end;
    CFile:=FD; CRecPtr:=NewRecPtr;
    if not ELockRec(ee,CFile^.IRec,IsNewRec,SubSet) then exit;          {}
    if not WasUpdated then begin                                        {}
      Move(CRecPtr^,OldRecPtr^,CFile^.RecLen); WasUpdated:=true end;    {}
    DuplFld(E^.FD,CFile,E^.NewRecPtr,CRecPtr,OldRecPtr,f1,f2); SetUpdFlag end;
  CFile:=E^.FD; CRecPtr:=E^.NewRecPtr; DuplToPrevEdit:=true;
  KbdBuffer:=^m+KbdBuffer;
end;

procedure Calculate;
var
  Z:FrmlPtr; Txt:string; er:ExitRecord; I:word; Msg:ScreenStr;
  p:pointer; FTyp:char; R:float; F:FieldDPtr; Del:boolean;
label 0,1,2,3,4,5;
begin
  MarkStore(p); NewExit(Ovr,er);goto 2; ResetCompilePars;
0:Txt:=CalcTxt;
4:I:=1;Del:=true;
1:TxtEdCtrlUBrk:=true;TxtEdCtrlF4Brk:=true;
  PromptLL(114,Txt,I,Del); if KbdChar=_U_ then goto 0;
  if (KbdChar=_ESC_) or (length(Txt)=0) then goto 3;
  CalcTxt:=Txt;
  SetInpStr(Txt); RdLex; Z:=RdFrml(FTyp); if Lexem<>^z then Error(21);
  if KbdChar=_CtrlF4_ then begin
    F:=CFld^.FldD;
    if CFld^.Ed(IsNewRec) and (F^.FrmlTyp=FTyp) then begin
      if LockRec(true) then begin
        if (F^.Typ='F') and (F^.Flg and f_Comma<>0) then begin
         if (Z^.Op=_const) then r:=Z^.R else
         if (Z^.Op=_unminus) and (Z^.P1^.Op=_const) then r:=-Z^.P1^.R
         else goto 5;
         SetWasUpdated; r_(F,r*Power10[F^.M]) end
        else 5:AssignFld(F,Z);
        DisplFld(CFld,IRec); IVOn; goto 3 end end
    else WrLLF10Msg(140) end;
  case FTyp of
   'R':begin R:=RunReal(Z);str(R:30:10,Txt);
       Txt:=leadchar(' ',trailchar('0',Txt));
       if Txt[length(Txt)]='.' then dec(Txt[0]) end;
   'S':Txt:=RunShortStr(Z);   { wie RdMode fuer T ??}
   'B':if RunBool(Z) then Txt:=AbbrYes else Txt:=AbbrNo end;
  goto 4;
2:Msg:=MsgLine; I:=CurrPos; SetMsgPar(Msg); WrLLF10Msg(110);
  IsCompileErr:=false; Del:=false; CFile:=E^.FD; ReleaseStore(p); goto 1;
3:ReleaseStore(p); RestoreExit(er);
end;

procedure DelNewRec;
var md:LockMode;
begin
  DelAllDifTFlds(CRecPtr,nil); if CNRecs=1 then exit;
  IsNewRec:=false; Append:=false; WasUpdated:=false; CFld:=E^.FirstFld;
  if CRec>CNRecs then if IRec>1 then dec(IRec) else dec(BaseRec);
  RdRec(CRec); NewDisplLL:=true; DisplWwRecsOrPage;
end;

function FrstFldOnPage(Page:word):EFldDPtr;
  var D:EFldDPtr;
  begin D:=E^.FirstFld;while D^.Page<Page do D:=D^.Chain;FrstFldOnPage:=D end;

procedure F6Proc;
var iMsg:word;
begin
  iMsg:=105;
  if Subset or HasIndex or NoCreate or NoDelete
{$ifdef FandSQL} or CFile^.IsSQLFile {$endif} then iMsg:=106;
  case Menu(iMsg,1) of
   1:AutoReport; 2:CheckFromHere; 3:PromptSelect; 4:Autograph; 5:Sorting end;
end;

function GetEdRecNo:longint;
begin
  if IsNewRec then GetEdRecNo:=0 else
  if E^.IsLocked then GetEdRecNo:=E^.LockedRec else GetEdRecNo:=AbsRecNr(CRec);
end;

procedure SetEdRecNoEtc(RNr:longint);
var x:XString absolute EdRecKey; cr:pointer; k:KeyDPtr;
begin
  EdField:=CFld^.FldD^.Name; EdIRec:=IRec; EdRecKey:=''; EdKey:='';
  EdRecNo:=RNr; if RNr=0 then EdRecNo:=GetEdRecNo; if VK=nil then exit;
  if not WasWK and (VK^.Alias<>nil) then begin
    EdKey:=VK^.Alias^; if EdKey='' then EdKey:='@' end;
  if not IsNewRec then begin
    cr:=CRecPtr; if WasUpdated then CRecPtr:=E^.OldRecPtr;
    k:=VK; if Subset then k:=WK;
    x.PackKF(k^.KFlds); CRecPtr:=cr end;
end;

function StartExit(X:EdExitDPtr;Displ:boolean):boolean;

function StartProc(ExitProc:InstrPtr;Displ:boolean):boolean;
var upd:boolean; b,b2,lkd:boolean; p:Pchar; f:FieldDptr; d:word; md:LockMode; {t:real;}
begin
  StartProc:=false; CFile^.WasWrRec:=false; if HasTF then begin
    p:=GetRecSpace; move(CRecPtr^,p^,CFile^.RecLen) end;
  SetEdRecNoEtc(0);
  lkd:=E^.IsLocked; if not lkd and not LockRec(false) then exit;
  b:=WasUpdated; EdUpdated:=b; b2:=HasUpdFlag; SetWasUpdated; ClearUpdFlag;
  with ExitProc^.TArg[ExitProc^.N] do begin FD:=CFile; RecPtr:=CRecPtr end;
  md:=CFile^.LMode; WrEStatus;                            {t:=currtime;}
  CallProcedure(ExitProc);
  RdEStatus; NewLMode(md); upd:=CFile^.WasWrRec;      {writeln(strdate(currtime-t,'mm:ss.ttt'));wait;}
  if HasUpdFlag then begin b:=true; upd:=true end;
  WasUpdated:=b; if b2 then SetUpdFlag;
  if not WasUpdated and not lkd then UnlockRec(E);
  if Displ and upd then DisplAllWwRecs;
  if Displ then NewDisplLL:=true; StartProc:=true;
  if HasTF then begin
    f:=CFile^.FldD; while f<>nil do begin
      if (f^.Typ='T') and (f^.Flg and f_Stored<>0) and
         (LongintPtr(p+f^.Displ)^=LongintPtr(Pchar(E^.OldRecPtr)+f^.Displ)^) then
         NoDelTFlds:=true;
      f:=f^.Chain end;
    ReleaseStore(p) end;
end;

procedure StartRprt(RO:RprtOptPtr);
var displ:boolean; k:WKeyDPtr; kf:KeyFldDPtr;
begin
  if IsNewRec or EdRecVar or (EdBreak=16) or not WriteCRec(true,displ) then exit;
  if displ then DisplAllWwRecs; kf:=nil; if VK<>nil then kf:=VK^.KFlds;
  k:=GetZStore(sizeof(k^)); k^.OneRecIdx(kf,AbsRecNr(CRec));
  RO^.FDL.FD:=CFile; RO^.FDL.ViewKey:=k;
  ReportProc(RO,false);
  CFile:=E^.FD; CRecPtr:=E^.NewRecPtr;
end;
                                               { StartExit - body }
begin
  StartExit:=true; case X^.Typ of
    'P':StartExit:=StartProc(X^.Proc,Displ);
    'R':StartRprt(X^.RO) end;
end;

function ExitKeyProc:word;
var X:EdExitDPtr; w,c:word; ok:boolean;
begin
  w:=0; c:=KbdChar; X:=E^.ExD; while X<>nil do begin
    if TestExitKey(c,X) then begin LastTxtPos:=-1;
      if X^.Typ='Q' then w:=1 else begin
        ok:=EdOk; EdOK:=false; StartExit(X,true);
        if EdOK then w:=3 else w:=2; EdOk:=ok end end;
    X:=X^.Chain end;
  if ((w=0)or(w=3)) and (c=_ShiftF7_) and CFld^.Ed(IsNewRec) then begin
    ShiftF7Proc; w:=2 end;
  KbdChar:=c; ExitKeyProc:=w;
end;

procedure FieldHelp;
begin
  Help(CFile^.ChptPos.R,CFile^.Name+'.'+CFld^.FldD^.Name,false);
end;

{$S-}
procedure DisplLASwitches;
begin
  if not ChkSwitch then ScrWrStr(0,TxtRows-1,'#L',colors.lSwitch);
  if not WarnSwitch then ScrWrStr(2,TxtRows-1,'?',colors.lSwitch);
  if not EdRecVar and not AddSwitch then ScrWrStr(3,TxtRows-1,'#A',colors.lSwitch);
  if not WithBoolDispl and Select then ScrWrStr(5,TxtRows-1,'  ',colors.lSwitch);
end;
procedure DisplLL;
var n:word;
begin
  if E^.Last<>nil then begin
    MsgLine:=E^.Last^; if length(MsgLine)>0 then begin
      WrLLMsgTxt; DisplLASwitches end; exit end;
  if E^.ShiftF7LD<>nil then n:=144
  else if NoCreate or Only1Record then
    if IsNewRec then n:=129 else if EdRecVar then n:=130 else n:=128
  else if IsNewRec then n:=123 else n:=124;
  if not F1Mode or Mode24 then begin WrLLMsg(n); DisplLASwitches end;
end;
procedure DisplCtrlAltLL(Flags:word);
begin
  if Flags and $04<>0 then begin        { Ctrl }
    if E^.CtrlLast<>nil then begin MsgLine:=E^.CtrlLast^; WrLLMsgTxt end
    else if IsCurrChpt then WrLLMsg(125)
    else if EdRecVar then WrLLMsg(154) else WrLLMsg(127) end
  else if Flags and $03<>0 then         { Shift }
    if E^.ShiftLast<>nil then begin MsgLine:=E^.ShiftLast^; WrLLMsgTxt end
    else DisplLL
  else if Flags and $08<>0 then          { Alt }
    if E^.AltLast<>nil then begin MsgLine:=E^.AltLast^; WrLLMsgTxt end
    else DisplLL;
end;
{$S+}
procedure DisplLLHlp;
begin if CRdb^.HelpFD<>nil then begin
  DisplLLHelp(CFile^.ChptPos.R,CFile^.Name+'.'+CFld^.FldD^.Name,Mode24) end;
end;

procedure CtrlReadKbd;
var TimeBeg:longint; D:word; flgs:byte;
label 1,11,2;
begin
  flgs:=0;
  TestEvent; if Event.What in [evKeyDown,evMouseDown] then goto 2;
  ClrEvent; if NewDisplLL then begin DisplLL; NewDisplLL:=false end;
  TimeBeg:=Timer; D:=0;
  if CFile^.NotCached then begin
    if not E^.EdRecVar and ((spec.ScreenDelay=0) or (E^.RefreshDelay<spec.ScreenDelay)) then
      D:=E^.RefreshDelay;
    if E^.WatchDelay<>0 then
    if D=0 then D:=E^.WatchDelay else D:=minw(D,E^.WatchDelay) end;
  if F1Mode and Mode24 then DisplLLHlp;
1:if LLKeyFlags<>0 then begin flgs:=LLKeyFlags; goto 11 end
  else if KbdFlgs and $0f<>0 then begin flgs:=KbdFlgs;
11:  DisplCtrlAltLL(flgs) end
  else begin DisplLL; flgs:=0; if F1Mode and not Mode24 then DisplLLHlp end;
  if D>0 then
    if Timer>=TimeBeg+D then goto 2 else WaitEvent(TimeBeg+D-Timer)
  else WaitEvent(0);
  if not (Event.What in [evKeyDown,evMouseDown]) then begin
    ClrEvent; goto 1 end;
2:if flgs<>0 then begin LLKeyFlags:=0; DisplLL; AddCtrlAltShift(flgs) end;
end;
procedure MouseProc;
var i:word; n:longint; D:EFldDPtr; Displ:boolean;
label 1;
begin
  for i:=1 to E^.NRecs do begin
    n:=BaseRec+i-1; if n>CNRecs then goto 1;
    D:=E^.FirstFld; while D<>nil do begin
      if IsNewRec and (i=IRec) and (D=FirstEmptyFld) then goto 1;
      if (D^.Page=CPage) and MouseInRect(D^.Col-1,FldRow(D,i)-1,D^.L,1) then begin
        if (i<>IRec) and (IsNewRec or not WriteCRec(true,Displ)) then goto 1;
        GotoRecFld(n,D); if (Event.Buttons and mbDoubleClick)<>0 then begin
          if MouseEnter then Event.KeyCode:=_M_ else Event.KeyCode:=_INS_;
          Event.What:=evKeyDown; exit end
        else ClrEvent;
        exit end;
      D:=D^.Chain end end;
1:ClrEvent;
end;
procedure ToggleSelectRec;
var x:XString; k:WKeyDPtr; n:longint; md:LockMode;
begin
  k:=E^.SelKey; n:=AbsRecNr(CRec);
  if k^.RecNrToPath(x,n) then begin dec(k^.NR); k^.DeleteOnPath end
  else begin inc(k^.NR); k^.Insert(n,false) end;
  SetRecAttr(IRec); IVOn;
end;
procedure ToggleSelectAll;
var k:WKeyDPtr;
begin
  k:=E^.SelKey; if k=nil then exit; if k^.NR>0 then k^.Release else
    if Subset then CopyIndex(k,WK) else CopyIndex(k,VK);
  DisplAllWwRecs;
end;

procedure GoStartFld(SFld:EFldDPtr);
begin
 while (CFld<>SFld) and (CFld^.Chain<>nil) do begin
   if IsFirstEmptyFld then begin
     if (CFld^.Impl<>nil) and LockRec(true) then AssignFld(CFld^.FldD,CFld^.Impl);
     FirstEmptyFld:=FirstEmptyFld^.Chain; DisplFld(CFld,IRec) end;
   GotoRecFld(CRec,CFld^.Chain) end;
end;

procedure RunEdit(PX:XStringPtr; var Brk:word);
label 0,1,2,3,4,5,6,7,71,8,81,9,11,12,13,14,fin;
var i,LongBeep,w:word; Displ,b:boolean; X:EdExitDPtr;
    OldTimeW,OldTimeR,n:longint; EdBr:byte;
    Timer:longint absolute 0:$46C;
begin
  Brk:=0; DisplLL;
  if OnlySearch then goto 2;
  if not IsNewRec and (PX<>nil) then GotoXRec(PX,n);
  if Select and not RunBool(E^.Bool) then GoPrevNextRec(+1,true);
  if E^.StartFld<>nil then begin GoStartFld(E^.StartFld); goto 1 end;
0:if not CtrlMProc(0) then goto 7;
1:LongBeep:=0;
8:OldTimeW:=Timer;
81:OldTimeR:=Timer; CtrlReadKbd;
  if CFile^.NotCached then begin
    if not EdRecVar and (E^.RefreshDelay>0) and (OldTimeR+E^.RefreshDelay<Timer) then
      DisplAllWwRecs;
    if Event.What=0 then
    if (E^.WatchDelay>0) and (OldTimeW+E^.WatchDelay<Timer) then
      if LongBeep<3 then begin
        for i:=1 to 4 do beep; inc(LongBeep); goto 8 end
      else begin UndoRecord; EdBreak:=11; goto 7 end
    else goto 81 end;
  case Event.What of
   evMouseDown:
     if F1Mode and (CRdb^.HelpFD<>nil) and
        (Mode24 and (Event.Where.Y=TxtRows-2) or
         not Mode24 and (Event.Where.Y=TxtRows-1)) then begin
       ClrEvent; FieldHelp end
     else MouseProc;
   evKeyDown: begin
     KbdChar:=Event.KeyCode; ClrEvent;
     case ExitKeyProc of 1:{quit}goto 7; 2:{exit} goto 1 end;
   case KbdChar of
    _F1_ :begin RdMsg(7);Help(RdbDPtr(HelpFD),MsgLine,false) end;
    _CtrlF1_:FieldHelp;
    _AltF10_:Help(nil,'',false);
    _ESC_:
       begin if OnlySearch then begin
                if IsNewRec then begin if CNRecs>1 then DelNewRec else goto 9 end
                else if not WriteCRec(true,Displ) then goto 1;
              2:if PromptAndSearch(not NoCreate) then goto 0 end;
           9:EdBreak:=0;
           7:if IsNewRec and not EquOldNewRec then
               if not Prompt158 or PromptYN(158) then goto fin
               else goto 1;
             EdBr:=EdBreak; n:=GetEdRecNo;
             if (IsNewRec or WriteCRec(true,Displ)) and
                ((EdBreak=11) or NoESCPrompt or
                not Spec.EscVerify and not MustESCPrompt
                or PromptYN(137)) then begin
               EdBreak:=EdBr; SetEdRecNoEtc(n); goto 71;
           fin:SetEdRecNoEtc(0);
           71: if IsNewRec and not EdRecVar then DelNewRec;
               IVoff; EdUpdated:=E^.EdUpdated;
               if not EdRecVar then ClearRecSpace(E^.NewRecPtr);
               if Subset and not WasWK then WK^.Close;
               if not EdRecVar then begin    {$ifdef FandSQL}
                 if CFile^.IsSQLFile then Strm1^.EndKeyAcc(WK); {$endif}
                 OldLMode(E^.OldMd) end;
               exit end;
       end;
    _AltEqual_:begin UndoRecord; EdBreak:=0; goto fin end;
    _U_:if PromptYN(108) then UndoRecord;
    $1C{^\}:if not CtrlMProc(2) then goto 7;
    _F2_:if not EdRecVar then
         if IsNewRec then begin
           if (CNRecs>1) and (not Prompt158 or EquOldNewRec or PromptYN(158))
           then DelNewRec end
         else if not NoCreate and not Only1Record and WriteCRec(true,Displ) then
            begin if Displ then DisplAllWwRecs; SwitchToAppend;goto 0 end;
    _up_  : if LUpRDown then goto 11 else goto 13;
    _down_: if LUpRDown then goto 12 else goto 13;
    _left_,_S_:
11:     if CFld^.ChainBack<>nil then GotoRecFld(CRec,CFld^.ChainBack);
    _right_,_D_:
12:     if (CFld^.Chain<>nil) and not IsFirstEmptyFld then GotoRecFld(CRec,CFld^.Chain);
    _home_:
       3:GotoRecFld(CRec,E^.FirstFld);
    _end_:
       4:if IsNewRec and (FirstEmptyFld<>nil) then
            GotoRecFld(CRec,FirstEmptyFld)
         else GotoRecFld(CRec,E^.LastFld);
    _M_:
       if SelMode and (E^.SelKey<>nil) and not IsNewRec then begin
         if WriteCRec(true,Displ) then begin
           if (E^.SelKey<>nil) and (E^.SelKey^.NRecs=0) then ToggleSelectRec;
           EdBreak:=12; goto fin end end else
       if (E^.ShiftF7LD<>nil) and not IsNewRec then begin
         if ShiftF7Duplicate then goto 9 end else
       if not CtrlMProc(3) then goto 7;
    $20..$FE:
       if CFld^.Ed(IsNewRec) and ((CFld^.FldD^.Typ<>'T') or (_t(CFld^.FldD)=0)) and
          LockRec(true) then begin
         KbdBuffer:=char(KbdChar)+KbdBuffer;
         if not EditItemProc(true,true,Brk) then goto 7;if Brk<>0 then goto fin end;
    _ins_: begin b:=false;if CFld^.Ed(IsNewRec) and LockRec(true) then b:=true;
       if not EditItemProc(false,b,Brk) then goto 7;if Brk<>0 then goto fin end;
    _F4_:
       if (CRec>1) and (IsFirstEmptyFld or PromptYN(121)) and LockRec(true) then
       begin DuplFromPrevRec;if not CtrlMProc(1) then goto 7 end;
    _F5_:
       SetSwitchProc;
    _F7_:
       UpwEdit(nil);
    _CtrlF5_:Calculate;
    else
13:  if not IsNewRec then begin w:=KbdChar;
     if w=_Y_ then begin
       if not NoDelete then if DeleteRecProc then begin
         ClearKeyBuf; b:=true;
14:      if ((CNRecs=0) or (CNRecs=1) and IsNewRec) and NoCreate then begin
           WrLLF10Msg(112); EdBreak:=13; goto fin end;
         if b and not CtrlMProc(0) then goto 7 end end
     else if WriteCRec(true,Displ) then begin
       if Displ then DisplAllWwRecs; KbdChar:=w;       {only in edit mode}
       case w of
      _F9_: begin SaveFiles; UpdCount:=0 end;
      _N_:
         if not NoCreate and not Only1Record then
            begin InsertRecProc(nil);goto 0 end;
      _up_,_E_:
         if E^.NRecs>1 then GoPrevNextRec(-1,true);
      _CtrlHome_:
         GoPrevNextRec(-1,true);
      _down_,_X_:
         if E^.NRecs>1 then GoPrevNextRec(+1,true);
      _CtrlEnd_:
         GoPrevNextRec(+1,true);
      _PgUp_,_R_:
         if E^.NPages=1 then
            if E^.NRecs=1 then GoPrevNextRec(-1,true) else GotoRecFld(CRec-E^.NRecs,CFld)
         else if CPage>1 then GotoRecFld(CRec,FrstFldOnPage(CPage-1));
      _PgDn_,_C_:
         if E^.NPages=1 then
            if E^.NRecs=1 then GoPrevNextRec(+1,true) else GotoRecFld(CRec+E^.NRecs,CFld)
         else if CPage<E^.NPages then GotoRecFld(CRec,FrstFldOnPage(CPage+1));
      _Q_:
          case readkbd of _S_:goto 3; _D_:goto 4; _R_:goto 5; _C_:goto 6 end;
      _CtrlPgUp_:
          5:GotoRecFld(1,E^.FirstFld);
      _CtrlPgDn_:
          6:GotoRecFld(CNRecs,E^.LastFld);
      _CtrlLeft_:
          if CRec>1 then SwitchRecs(-1);
      _CtrlRight_:
          if CRec<CNRecs then SwitchRecs(+1);
      _F3_: if not EdRecVar then
          if CFile=CRdb^.HelpFD then begin
            if PromptHelpName(i) then begin GotoRecFld(i,CFld); goto 1 end end
          else begin PromptAndSearch(false); goto 0 end;
      _CtrlF2_:
          begin if not EdRecVar then RefreshSubset; b:=false; goto 14 end;
      _AltF2_,_AltF3_:if IsCurrChpt then
          if w=_AltF3_ then begin
            ForAllFDs(ClosePassiveFD); EditHelpOrCat(w,0,'') end
          else begin Brk:=2; goto fin end
          else if IsTestRun and (CFile<>CatFD) and (w=_AltF2_) then
            EditHelpOrCat(w,1,CFile^.Name+'.'+CFld^.FldD^.Name);
      _CtrlF3_:
          if not EdRecVar then PromptGotoRecNr;
      _F6_:if not EdRecVar then F6Proc;
      _CtrlF4_:if DuplToPrevEdit then begin EdBreak:=14;goto fin end;
      _CtrlF7_:DownEdit;
      _F8_:    if E^.SelKey<>nil then begin
                  ToggleSelectRec; GoPrevNextRec(+1,true) end;
      _ShiftF8_:  ToggleSelectAll;
      _CtrlF8_.._CtrlF10_,_AltF9_:
          if IsCurrChpt then begin Brk:=2; goto fin end;
      _AltF7_:ImbeddEdit;
     end end end end end;
     else ClrEvent;
  end;
  goto 1;
end;
          {called from Proc and Projmgr }
procedure EditDataFile(FD:FileDPtr;EO:EditOptPtr);
var p:pointer;w1,w2,w3:longint; Brk,r1,r2:word; pix:boolean; er:ExitRecord;
label 1,2;
begin
  MarkStore(p); if EO^.SyntxChk then begin
    IsCompileErr:=false; NewExit(Ovr,er); goto 1; NewEditD(FD,EO);
1:  RestoreExit(er); if IsCompileErr then begin
      EdRecKey:=MsgLine; LastExitCode:=CurrPos+1; IsCompileErr:=false end
    else LastExitCode:=0; goto 2 end;
  NewEditD(FD,EO);
  w2:=0; w3:=0; pix:=(E^.WFlags and WPushPixel)<>0;
  if E^.WwPart then with E^ do begin
    if E^.WithBoolDispl then r2:=2 else r2:=1;
    r1:=TxtRows; if E^.Mode24 then dec(r1);
    w1:=PushW1(1,1,TxtCols,r2,pix,true);
    w2:=PushW1(1,r1,TxtCols,TxtRows,pix,true);
    if (E^.WFlags and WNoPop)=0 then w3:=PushW1(V.C1,V.R1,V.C2+ShdwX,V.R2+ShdwY,pix,true) end
  else w1:=PushW1(1,1,TxtCols,TxtRows,pix,true);
  if OpenEditWw then begin
    if OnlyAppend and not Append then SwitchToAppend;
    RunEdit(nil,Brk) end;
  if w3<>0 then PopW(w3); if w2<>0 then PopW(w2); PopW(w1);
2:PopEdit; ReleaseStore(p);
end;