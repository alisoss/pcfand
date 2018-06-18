{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+,D+}

unit rdproc;

interface

uses dos,
     drivers,base,access,rdrun,compile;

function GetPInstr(Kind:PInstrCode;Size:word):InstrPtr;
procedure ReadProcHead;
function ReadProcBody:InstrPtr;
procedure ReadDeclChpt;
function RdViewOpt(EO:EditOptPtr): boolean;
function GetEvalFrml(X:FrmlPtr):FrmlPtr;


implementation

uses oaccess,rdfildcl,runfrml;

procedure Ovr; far;
assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push cs; push ax;
end;


var IsRdUserFunc:boolean;

procedure TestCatError(I:word;Nm:string;Old:boolean);
begin
  if I=0 then begin SetMsgPar(Nm); if Old then OldError(96) else Error(96) end;
end;
function IsRecVar(var LV:LocVarPtr):boolean;
begin
  IsRecVar:=false; if not FindLocVar(LVBD.Root,LV) or (LV^.FTyp<>'r') then exit;
  RdLex; IsRecVar:=true;
end;
function RdRecVar:LocVarPtr;
var LV:LocVarPtr;
begin
  if not IsRecVar(LV) then Error(141); RdRecVar:=LV;
end;
function RdIdxVar:LocVarPtr;
var lv:LocVarPtr;
begin
  if not FindLocVar(LVBD.Root,lv) or (lv^.FTyp<>'i') then Error(165);
  RdIdxVar:=lv; RdLex;
end;
function RdRecVarFldFrml(LV:LocVarPtr;var FTyp:char):FrmlPtr;
var Z:FrmlPtr; cf:FileDPtr; fa:boolean;
begin
  Accept('.');
  case LV^.FTyp of
   'r': begin Z:=GetOp(_recvarfld,12); cf:=CFile;
              CFile:=LV^.FD; Z^.File2:=CFile; Z^.LD:=LV^.RecPtr;
              fa:=FileVarsAllowed; FileVarsAllowed:=true;
              Z^.P1:=RdFldNameFrmlF(FTyp); FileVarsAllowed:=fa; CFile:=cf end;
   'i': begin Z:=GetOp(_indexnrecs,4); Z^.WKey:=WKeyDPtr(LV^.RecPtr);
              AcceptKeyWord('nrecs'); FTyp:='R' end;
   else OldError(177) end;
  RdRecVarFldFrml:=Z;
end;

function RdOwner(var LLD:LinkDPtr;var LLV:LocVarPtr{FrmlPtr}):char;{'r','i','F'}
var ld,ld1:LinkDPtr; lv:LocVarPtr; kf:KeyFldDPtr; fd:FileDPtr;
label 1,2,3;
begin
  if FindLocVar(LVBD.Root,lv) then begin
    if not(lv^.FTyp in ['i','r','f']) then Error(177);
    ld:=nil; ld1:=LinkDRoot; while ld1<>nil do begin
    if (ld1^.FromFD=CFile) and (ld1^.IndexRoot<>0) and (ld1^.ToFD=lv^.FD) then
      ld:=ld1; ld1:=ld1^.Chain end;
    if ld=nil then Error(116); RdLex;
    if lv^.FTyp='f' then goto 2 else goto 1 end;
  TestIdentif; ld:=LinkDRoot; while ld<>nil do begin
    if (ld^.FromFD=CFile) and EquUpCase(ld^.RoleName) then begin
      if (ld^.IndexRoot=0) then Error(116); RdLex; fd:=LD^.ToFD;
      if Lexem='(' then begin RdLex;
        if not FindLocVar(LVBD.Root,lv) or not (lv^.FTyp in ['i','r']) then Error(177);
        RdLex; Accept(')'); if lv^.FD<>fd then OldError(149);
1:      if lv^.FTyp='i' then begin kf:=WKeyDPtr(lv^.RecPtr)^.KFlds;
          if ld^.FromFD^.IsSQLFile or ld^.ToFD^.IsSQLFile then OldError(155);
          if (kf<>nil) and not EquKFlds(kf,ld^.ToKey^.KFlds) then OldError(181) end;
        LLV:=lv; RdOwner:=lv^.FTyp; goto 3 end
      else begin
2:      {$ifdef FandSQL} if ld^.ToFD^.typSQLFile then Error(155); {$endif}
        Accept('['); LLV:=LocVarPtr(RdRealFrml); Accept(']'); RdOwner:='F';
3:      LLD:=ld; exit end end;
    ld:=ld^.Chain end;
  Error(9);
end;

function RdFldNameFrmlP(var FTyp:char):FrmlPtr; far;
var FD:FileDPtr; Z:FrmlPtr; LV:LocVarPtr; Op:char; LD:LinkDPtr; F:FieldDPtr;
    linked:boolean; FName:ScreenStr; K:KeyDPtr;
label 0,1,2,3;
begin
  if IsForwPoint then
    if FindLocVar(LVBD.Root,LV) and (LV^.FTyp in ['i','r']) then begin
      RdLex; RdFldNameFrmlP:=RdRecVarFldFrml(LV,FTyp); exit end
    else begin
      FName:=LexWord; linked:=IsRoleName(FileVarsAllowed,FD,LD);
      if FD<>nil then FName:=FD^.Name; if not linked then RdLex;
      RdLex; FTyp:='R';
      if IsKeyWord('LASTUPDATE') then begin
        Op:=_lastupdate; if FD<>nil then goto 2; F:=nil; goto 1 end;
      if IsKeyWord('ARCHIVES') then begin F:=CatArchiv; goto 0 end;
      if IsKeyWord('PATH') then begin F:=CatPathName; goto 0 end;
      if IsKeyWord('VOLUME') then begin F:=CatVolume;
       0:FTyp:='S';
       1:Z:=GetOp(_catfield,6); Z^.CatFld:=F;
         Z^.CatIRec:=GetCatIRec(FName,true); RdFldNameFrmlP:=Z;
         TestCatError(Z^.CatIRec,FName,true); exit end;
      if FD<>nil then begin
        if IsKeyWord('GENERATION') then begin Op:=_generation; goto 2 end;
        if IsKeyWord('NRECSABS') then begin Op:=_nrecsabs; goto 2 end;
        if IsKeyWord('NRECS') then begin Op:=_nrecs;
         2:Z:=GetOp(Op,sizeof(FileDPtr));
           Z^.FD:=FD; RdFldNameFrmlP:=Z; exit end end;
      if linked then begin RdFldNameFrmlP:=RdFAccess(FD,LD,FTyp); exit end;
      if FileVarsAllowed then OldError(9) else OldError(63);
      end;
  if ForwChar='[' then begin
    Z:=GetOp(_accrecno,8);FD:=RdFileName;RdLex;Z^.RecFD:=FD;
    {$ifdef FandSQL} if FD^.typSQLFile then OldError(155); {$endif}
    Z^.P1:=RdRealFrml; Accept(']');Accept('.');
    F:=RdFldName(FD);Z^.RecFldD:=F;
    FTyp:=F^.FrmlTyp; RdFldNameFrmlP:=Z; exit end;
  if IsKeyWord('KEYPRESSED') then begin Op:=_keypressed; goto 3 end;
  if IsKeyWord('ESCPROMPT') then begin Op:=_escprompt; goto 3 end;
  if IsKeyWord('EDUPDATED') then begin Op:=_edupdated;
   3:RdFldNameFrmlP:=GetOp(Op,0); FTyp:='B'; exit end;
  if IsKeyWord('GETPATH') then begin
     RdFldNameFrmlP:=GetOp(_getpath,0); FTyp:='S'; exit end;
  if FindLocVar(LVBD.Root,LV) then begin
     if LV^.FTyp in ['r','f','i'] then Error(143);
     RdLex;RdFldNameFrmlP:=FrmlPtr(@LV^.Op);FTyp:=LV^.FTyp; exit end;
  if FileVarsAllowed then begin
     z:=TryRdFldFrml(CFile,FTyp); if z=nil then Error(8);
     RdFldNameFrmlP:=z; exit end;
  Error(8);
end;

function RdPath(NoFD:boolean;var Path:StringPtr; var CatIRec:word):FileDPtr;
var fd:FileDPtr;
begin
  CatIRec:=0;
  if Lexem=_quotedstr then begin Path:=RdStrConst; fd:=nil end else begin
    TestIdentif; fd:=FindFileD;
    if fd=nil then begin
      CatIRec:=GetCatIRec(LexWord,true); TestCatError(CatIRec,LexWord,false) end
    else if NoFD then Error(97);
    RdLex end;
  RdPath:=fd;
end;

function RdFunctionP(var FFTyp:char):FrmlPtr; far;
function RdViewKeyImpl(FD:FileDPtr):KeyDPtr;
var K:KeyDPtr; cf:FileDPtr;
begin
  K:=FD^.Keys; if K=nil then Error(24); if Lexem='/' then begin
    cf:=CFile; CFile:=FD; K:=RdViewKey; CFile:=cf end;
  RdViewKeyImpl:=K;
end;
procedure RdSelectStr(Z:FrmlPtr);
var PD:InstrPtr;
begin with z^ do begin
  Delim:=^m; P1:=RdRealFrml; Accept(','); P2:=RdRealFrml; Accept(',');
  P3:=RdStrFrml; while Lexem=',' do begin RdLex;
    if IsOpt('HEAD') then P4:=RdStrFrml else
    if IsOpt('FOOT') then P5:=RdStrFrml else
    if IsOpt('MODE') then P6:=RdStrFrml else
    if IsOpt('DELIM') then Delim:=RdQuotedChar
    else Error(157) end;
end end;
var Z:FrmlPtr; Typ,FTyp:char; cf,FD:FileDPtr; F:FieldDPtr;
    K:KeyDPtr; KF:KeyFldDPtr; b:boolean;
    N:word; Arg:array[1..30] of FrmlPtr; Op:char;
    LV:LocVarPtr; LD:LinkDPtr;
    p:pointer; pofs:word absolute p;
label 1,11,2,3,4;
begin
  if IsKeyWord('EVALB') then begin FTyp:='B'; goto 4 end
  else if IsKeyWord('EVALS') then begin FTyp:='S'; goto 4 end
  else if IsKeyWord('EVALR') then begin FTyp:='R';
4:  RdLex; Z:=GetOp(_eval,5); Z^.EvalTyp:=FTyp; Z^.P1:=RdStrFrml end
  else if FileVarsAllowed then Error(75)
  else if IsKeyWord('PROMPT') then begin
    RdLex; Z:=GetOp(_prompt,4); Z^.P1:=RdStrFrml; F:=RdFldDescr('',true);
    Z^.FldD:=F; FTyp:=F^.FrmlTyp; if F^.Typ='T' then OldError(65);
    if Lexem=_assign then begin
      RdLex; Z^.P2:=RdFrml(Typ); if Typ<>FTyp then OldError(12) end end
  else if IsKeyWord('KEYOF') then begin
    RdLex; FTyp:='S'; if not IsRecVar(LV) then begin Op:=_recno; goto 11 end;
    Z:=GetOp(_keyof,8); Z^.LV:=LV;
    Z^.PackKey:=RdViewKeyImpl(Z^.LV^.FD); FTyp:='S' end
  else if IsKeyWord('RECNO') then begin Op:=_recno; goto 1 end
  else if IsKeyWord('RECNOABS') then begin Op:=_recnoabs; goto 1 end
  else if IsKeyWord('RECNOLOG') then begin Op:=_recnolog;
1:  RdLex; FTyp:='R';
11: FD:=RdFileName; K:=RdViewKeyImpl(FD);
    if Op=_recno then begin KF:=K^.KFlds; N:=0; if KF=nil then OldError(176);
      while KF<>nil do begin Accept(','); inc(N);
        if N>30 then Error(123); Arg[N]:=RdFrml(Typ);
        if Typ<>KF^.FldD^.FrmlTyp then OldError(12); KF:=KF^.Chain end end
    else begin Accept(',');N:=1;Arg[1]:=RdRealFrml end;
    Z:=GetOp(Op,(N+2)*4); Z^.FD:=FD; Z^.Key:=K; Move(Arg,Z^.Arg,4*N);
    if FTyp='R' then goto 2 end
  else if IsKeyWord('LINK') then begin
    RdLex; Z:=GetOp(_link,5);
    if IsRecVar(LV) then begin
      Z^.LinkFromRec:=true; Z^.LinkLV:=LV; FD:=LV^.FD end
    else begin FD:=RdFileName; Accept('[');
      Z^.LinkRecFrml:=RdRealFrml; Accept(']') end;
    Accept(',');
    {$ifdef FandSQL} if FD^.typSQLFile then OldError(155); {$endif}
    cf:=CFile; CFile:=FD;
    if not IsRoleName(true,FD,LD) or (LD=nil) then Error(9);
    CFile:=cf; Z^.LinkLD:=LD; FTyp:='R'; goto 2 end
  else if IsKeyWord('ISDELETED') then begin
    RdLex; FTyp:='B';
    if IsRecVar(LV) then begin
      Z:=GetOp(_lvdeleted,4); Z^.LV:=LV end
    else begin Z:=GetOp(_isdeleted,4); FD:=RdFileName; Z^.RecFD:=FD;
      Accept(','); Z^.P1:=RdRealFrml;
2:   {$ifdef FandSQL} if FD^.typSQLFile then Error(155) {$endif} end end
  else if IsKeyWord('GETPATH') then begin
    RdLex; Z:=GetOp(_getpath,0); Z^.P1:=RdStrFrml; FTyp:='S' end
  else if IsKeyWord('GETTXT') then begin
    RdLex; Z:=GetOp(_gettxt,6); FTyp:='S';goto 3 end
  else if IsKeyWord('FILESIZE') then begin
    RdLex; Z:=GetOp(_filesize,14); FTyp:='R';
3:  RdPath(true,Z^.TxtPath,Z^.TxtCatIRec);
    if (Z^.Op=_gettxt) and (Lexem=',') then begin
      RdLex; Z^.P1:=RdRealFrml; if Lexem=',' then begin
        RdLex; Z^.P2:=RdRealFrml  end end end
  else if IsKeyWord('INTTSR') then begin
    RdLex; Z:=GetOp(_inttsr,5); Z^.P1:=RdRealFrml; Accept(',');
    Z^.P2:=RdRealFrml; Accept(','); Typ:='r';
    if IsRecVar(LV) then Z^.P3:=LV^.RecPtr else Z^.P3:=RdFrml(Typ);
    Z^.N31:=ord(Typ); FTyp:='R' end                           {$ifdef FandSQL}
  else if IsKeyWord('SQL') then begin
    RdLex; Z:=GetOp(_sqlfun,0); Z^.P1:=RdStrFrml; FTyp:='R'; end{$endif}
  else if IsKeyWord('SELECTSTR') then begin
    RdLex; Z:=GetOp(_selectstr,13); FTyp:='S'; RdSelectStr(Z) end
  else if IsKeyWord('PROMPTYN') then begin
    RdLex; Z:=GetOp(_promptyn,0); Z^.P1:=RdStrFrml; FTyp:='B' end
  else if IsKeyWord('MOUSEEVENT') then begin
    RdLex; Z:=GetOp(_mouseevent,2); Z^.W01:=RdInteger; FTyp:='B' end
  else if IsKeyWord('ISMOUSE') then begin
    RdLex; Z:=GetOp(_ismouse,4); Z^.W01:=RdInteger; Accept(',');
    Z^.W02:=RdInteger; FTyp:='B' end
  else if IsKeyWord('MOUSEIN') then begin
    RdLex; Z:=GetOp(_mousein,4); Z^.P1:=RdRealFrml; Accept(',');
    Z^.P2:=RdRealFrml; Accept(','); Z^.P3:=RdRealFrml; Accept(',');
    Z^.P4:=RdRealFrml; FTyp:='B' end
  else if IsKeyWord('PORTIN') then begin
    RdLex; Z:=GetOp(_portin,0); Z^.P1:=RdBool; Accept(',');
    Z^.P2:=RdRealFrml; FTyp:='R' end
  else Error(75);
  Accept(')'); RdFunctionP:=Z; FFTyp:=FTyp;
end;

function GetPInstr(Kind:PInstrCode;Size:word):InstrPtr;
var PD:InstrPtr;
begin
  PD:=GetZStore(Size+5); PD^.Kind:=Kind; GetPInstr:=PD;
end;

function RdPInstr:InstrPtr;forward;

procedure RdPInstrAndChain(var PD:InstrPtr);
var PD1,PD2:InstrPtr;
begin
  PD1:=RdPInstr; {may be a chain itself}
  PD2:=InstrPtr(@PD); while PD2^.Chain<>nil do PD2:=PD2^.Chain; PD2^.Chain:=PD1;
end;
procedure RdChoices(PD:InstrPtr);
var CD:ChoiceDPtr; N,SumL:word;
label 1;
begin
  AcceptKeyWord('OF'); N:=0; SumL:=0;
1:if IsKeyWord('ESCAPE') then begin
    Accept(':'); PD^.WasESCBranch:=true; PD^.ESCInstr:=RdPInstr end
  else begin
    CD:=GetZStore(sizeof(CD^)); ChainLast(PD^.Choices,CD); inc(N);
    if (PD^.Kind=_menubar) and (N>30) then Error(102);
    CD^.TxtFrml:=RdStrFrml; if Lexem=',' then begin RdLex;
      if Lexem<>',' then begin CD^.HelpName:=RdHelpName; PD^.HelpRdb:=CRdb end;
      if Lexem=',' then begin RdLex;
        if Lexem<>',' then begin CD^.Bool:=RdBool; if Lexem='!' then begin
          CD^.DisplEver:=true; RdLex end end;
      end end;
    Accept(':'); CD^.Instr:=RdPInstr end;
  if Lexem=';' then begin
    RdLex;if IsKeyWord('END') then exit;goto 1 end;
  AcceptKeyWord('END');
end;
procedure RdMenuAttr(PD:InstrPtr);
begin
  if Lexem<>';' then exit; RdLex; with PD^ do begin
    mAttr[0]:=RdAttr; Accept(','); mAttr[1]:=RdAttr; Accept(',');
    mAttr[2]:=RdAttr; if Lexem=',' then begin RdLex; mAttr[3]:=RdAttr end end;
end;
function RdMenuBox(Loop:boolean):InstrPtr;
var PD:InstrPtr; S:StringPtr;
begin
  PD:=GetPInstr(_menubox,48);
  RdMenuBox:=PD; PD^.Loop:=Loop;
  if Lexem='(' then begin
    RdLex; if Lexem<>';' then begin
      PD^.X:=RdRealFrml; Accept(','); PD^.Y:=RdRealFrml end;
    RdMenuAttr(PD); Accept(')') end;
  if Lexem='!' then begin RdLex; PD^.Shdw:=true end;
  if IsKeyWord('PULLDOWN') then PD^.PullDown:=true;
  if not TestKeyWord('OF') then PD^.HdLine:=RdStrFrml;
  RdChoices(PD);
end;
function RdMenuBar:InstrPtr;
var PD:InstrPtr;
begin
  PD:=GetPInstr(_menubar,48);
  RdMenuBar:=PD;
  if Lexem='(' then begin
    RdLex; if Lexem<>';' then begin
      PD^.Y:=RdRealFrml; if Lexem=',' then begin
        RdLex; PD^.X:=RdRealFrml; Accept(','); PD^.XSz:=RdRealFrml end end;
    RdMenuAttr(PD); Accept(')') end;
  RdChoices(PD);
end;
function RdIfThenElse:InstrPtr;
  var PD:InstrPtr;
  begin PD:=GetPInstr(_ifthenelseP,12);RdIfThenElse:=PD;
        PD^.Bool:=RdBool;AcceptKeyWord('THEN');PD^.Instr:=RdPInstr;
        if IsKeyWord('ELSE') then PD^.ElseInstr:=RdPInstr;
  end;
function RdWhileDo:InstrPtr;
  var PD:InstrPtr;
  begin PD:=GetPInstr(_whiledo,8);RdWhileDo:=PD;
        PD^.Bool:=RdBool;AcceptKeyWord('DO');PD^.Instr:=RdPInstr end;
function RdFor:InstrPtr;
var PD,PD1:InstrPtr; LV:LocVarPtr; Z:FrmlPtr;
begin
  if not FindLocVar(LVBD.Root,LV) or (LV^.FTyp<>'R') then Error(146);
  RdLex; PD:=GetPInstr(_asgnloc,9); RdFor:=PD; PD^.AssLV:=LV;
  Accept(_assign); PD^.Frml:=RdRealFrml; AcceptKeyWord('TO');
  PD^.Chain:=GetPInstr(_whiledo,8); PD:=PD^.Chain;
  Z:=GetOp(_compreal,2); Z^.P1:=FrmlPtr(@LV^.Op); Z^.N21:=ord(_le);
  Z^.N22:=5; Z^.P2:=RdRealFrml; PD^.Bool:=Z;
  AcceptKeyWord('DO'); PD1:=RdPInstr; PD^.Instr:=PD1;
  PD1:=GetPInstr(_asgnloc,9); ChainLast(PD^.Instr,PD1); PD1^.Add:=true;
  PD1^.AssLV:=LV; Z:=GetOp(_const,SizeOf(float)); Z^.R:=1; PD1^.Frml:=Z;
end;
function RdCase:InstrPtr;
var PD,PD1:InstrPtr; b,first:boolean;
label 1,2;
begin
  first:=true;
1:PD1:=GetPInstr(_ifthenelseP,12);
  if first then RdCase:=PD1 else PD^.ElseInstr:=PD1; PD:=PD1; first:=false;
  PD^.Bool:=RdBool; Accept(':'); PD^.Instr:=RdPInstr;
  b:=Lexem=';'; if b then RdLex;
  if not IsKeyWord('END') then
    if IsKeyWord('ELSE') then
      while not IsKeyWord('END') do begin
        RdPInstrAndChain(PD^.ElseInstr);
        if Lexem=';' then RdLex else goto 2 end
    else if b then goto 1
    else 2:AcceptKeyWord('END');
end;
function RdRepeatUntil:InstrPtr;
var PD:InstrPtr;
label 1;
begin
  PD:=GetPInstr(_repeatuntil,8); RdRepeatUntil:=PD;
  while not IsKeyWord('UNTIL') do begin
    RdPInstrAndChain(PD^.Instr); if Lexem=';' then RdLex else begin
      AcceptKeyWord('UNTIL'); goto 1 end end;
1:PD^.Bool:=RdBool;
end;
function RdForAll:InstrPtr;
var PD:InstrPtr; LVi,LVr:LocVarPtr; LD:LinkDPtr; Z:FrmlPtr;
label 1,2;
begin
  if not FindLocVar(LVBD.Root,LVi) then Error(122); RdLex;
  if LVi^.FTyp='r' then begin LVr:=LVi; LVi:=nil; CFile:=LVr^.FD end
  else begin  TestReal(LVi^.FTyp); AcceptKeyWord('IN');
    if FindLocVar(LVBD.Root,LVr) then begin
      if LVr^.FTyp='f' then begin CFile:=LVr^.FD; RdLex; goto 1 end;
      if LVr^.FTyp<>'r' then Error(141); CFile:=LVr^.FD; RdLex end
    else begin CFile:=RdFileName;
1:    LVr:=nil end;                         {$ifdef FandSQL}
    if CFile^.typSQLFile then OldError(155) {$endif} end;
  PD:=GetPInstr(_forall,41);
  PD^.CFD:=CFile; PD^.CVar:=LVi; PD^.CRecVar:=LVr;       {$ifdef FandSQL}
  if CFile^.typSQLFile and IsKeyWord('IN') then begin
    AcceptKeyWord('SQL'); Accept('('); PD^.CBool:=RdStrFrml; Accept(')');
    PD^.inSQL:=true; goto 2 end;                                 {$endif}
  if IsKeyWord('OWNER') then with PD^ do begin
    COwnerTyp:=RdOwner(CLD,CLV); CViewKey:=GetFromKey(CLD) end
  else CViewKey:=RdViewKey;
  if Lexem='(' then with PD^ do begin
    RdLex; CBool:=RdKeyInBool(CKIRoot,false,true,CSQLFilter);
    if (CKIRoot<>nil) and (CLV<>nil) then OldError(118); Accept(')') end;
  if Lexem='!' then begin RdLex; PD^.CWIdx:=true end;
  if Lexem='%' then begin RdLex; PD^.CProcent:=true end;
  PD^.CKey:=CViewKey;
2:AcceptKeyWord('DO'); PD^.CInstr:=RdPInstr; RdForAll:=PD;
end;
function RdBeginEnd:InstrPtr;
var PD:InstrPtr;
label 1;
begin
  PD:=nil; if not IsKeyWord('END') then begin
1:  RdPInstrAndChain(PD); if Lexem=';' then begin
      RdLex; if not IsKeyWord('END') then goto 1 end
    else AcceptKeyWord('END') end;
  RdBeginEnd:=PD;
end;

function RdProcArg(Caller:char):InstrPtr;
var PD:InstrPtr; Pos:RdbPos; TArg:array[1..31] of TypAndFrml;
    N,L:word; LV:LocVarPtr; z:FrmlPtr;
label 1;
begin
  if Caller<>'C' then RdChptName('P',Pos,Caller in ['P','E','T']); N:=0;
  if Caller<>'P' then begin if Lexem='(' then begin RdLex; goto 1 end end
  else if Lexem=',' then begin
    RdLex; Accept('(');
1:  inc(N); if N>30 then Error(123);
    with TArg[N] do begin
      fillchar(FTyp,sizeof(TypAndFrml),0);
      if (ForwChar<>'.') and
         FindLocVar(LVBD.Root,LV) and (LV^.FTyp in ['i','r']) then begin
        RdLex; FTyp:=LV^.FTyp; FD:=LV^.FD; RecPtr:=LV^.RecPtr end
      else if Lexem='@'then begin RdLex;
        if Lexem='[' then begin
          RdLex; Name:=StoreStr(LexWord); Accept(_identifier);
          Accept(','); z:=GetOp(_setmybp,0); z^.P1:=RdStrFrml; TxtFrml:=z;
          Accept(']') end
        else FD:=RdFileName; FTyp:='f' end
      else Frml:=RdFrml(FTyp) end;
    if Lexem=',' then begin RdLex; goto 1 end;
    Accept(')') end;
  if Caller='E' then begin inc(N); TArg[N].FTyp:='r' end;
  L:=N*sizeof(TypAndFrml); PD:=GetPInstr(_proc,sizeof(RdbPos)+2+L);
  PD^.Pos:=Pos; PD^.N:=N; Move(TArg,PD^.TArg,L);
  PD^.ExPar:=(Caller='E'); RdProcArg:=PD;
end;

procedure RdKeyCode(X:EdExitDPtr);
function NotCode(const Nm:string;CodeBase,BrkBase:word;E:EdExKeyDPtr):boolean;
var i,k:word;
begin
  NotCode:=true; if Lexem<>_identifier then exit;
  if not SEquUpcase(copy(LexWord,1,length(Nm)),Nm) then exit;
  val(copy(LexWord,length(Nm)+1,2),i,k);
  if (k<>0) or (i<=0) or (i>10) then exit; dec(i); RdLex;
  E^.KeyCode:=CodeBase+(i shl 8); E^.Break:=BrkBase+i; NotCode:=false;
end;
const NKeyNames=20;
const KeyNames:array[0..NKeyNames] of
   record Nm:string[9];Brk:byte;Code:word end=
  ((Nm:'HOME';Brk:51;Code:_Home_),(Nm:'UP';Brk:52;Code:_Up_),
   (Nm:'PGUP';Brk:53;Code:_PgUp_),(Nm:'LEFT';Brk:55;Code:_Left_),
   (Nm:'RIGHT';Brk:57;Code:_Right_),(Nm:'END';Brk:59;Code:_End_),
   (Nm:'DOWN';Brk:60;Code:_Down_),(Nm:'PGDN';Brk:61;Code:_PgDn_),
   (Nm:'INS';Brk:62;Code:_Ins_),
   (Nm:'CTRLLEFT';Brk:71;Code:_CtrlLeft_),(Nm:'CTRLRIGHT';Brk:72;Code:_CtrlRight_),
   (Nm:'CTRLEND';Brk:73;Code:_CtrlEnd_),(Nm:'CTRLPGDN';Brk:74;Code:_CtrlPgDn_),
   (Nm:'CTRLHOME';Brk:75;Code:_CtrlHome_),(Nm:'CTRLPGUP';Brk:76;Code:_CtrlPgUp_),
   (Nm:'TAB';Brk:77;Code:_Tab_),(Nm:'SHIFTTAB';Brk:78;Code:_ShiftTab_),
   (Nm:'CTRLN';Brk:79;Code:_N_),(Nm:'CTRLY';Brk:80;Code:_Y_),
   (Nm:'ESC';Brk:81;Code:_ESC_),(Nm:'CTRLP';Brk:82;Code:_P_));
var i:word; E:EdExKeyDPtr;
begin
  E:=GetStore(Sizeof(EdExKeyD)); E^.Chain:=X^.Keys; X^.Keys:=E;
  if NotCode('F',_F1_,21,E) and NotCode('ShiftF',_ShiftF1_,1,E)
   and NotCode('CtrlF',_CtrlF1_,31,E) and NotCode('AltF',_AltF1_,41,E) then begin
   for i:=0 to NKeyNames do with KeyNames[i] do if EquUpcase(Nm) then begin
      E^.KeyCode:=Code; E^.Break:=Brk; RdLex; exit end;
   Error(129) end;
end;
function RdHeadLast(var AA):boolean;
var A:record Head,Last,CtrlLast,AltLast,ShiftLast:FrmlPtr end absolute AA;
begin RdHeadLast:=true;
  if IsOpt('HEAD') then A.Head:=RdStrFrml
  else if IsOpt('LAST') then A.Last:=RdStrFrml
  else if IsOpt('CTRL') then A.CtrlLast:=RdStrFrml
  else if IsOpt('ALT') then A.AltLast:=RdStrFrml
  else if IsOpt('SHIFT') then A.ShiftLast:=RdStrFrml else RdHeadLast:=false;
end;

function RdViewOpt(EO:EditOptPtr): boolean;
procedure RdKeyList(X:EdExitDPtr);  {$V-}
label 1;
begin with X^ do begin
1:if (Lexem='(') or (Lexem='^') then RdNegFldList(NegFlds,Flds)
  else if IsKeyWord('RECORD') then AtWrRec:=true
  else if IsKeyWord('NEWREC') then AtNewRec:=true
  else RdKeyCode(X);
  if Lexem=',' then begin RdLex; goto 1 end;
  Accept(':');  {$V+}
end end;
var l:word; X:EdExitDPtr; RO:RprtOptPtr; FD:FileDPtr;
    Flgs:array[1..23] of boolean;
label 1;
begin with EO^ do begin
  RdLex; RdViewOpt:=true; CViewKey:=ViewKey;
  if IsOpt('TAB') then RdNegFldList(NegTab,Tab)
  else if IsOpt('DUPL') then RdNegFldList(NegDupl,Dupl)
  else if IsOpt('NOED') then RdNegFldList(NegNoEd,NoEd)
  else if IsOpt('MODE') then begin SkipBlank(false);
    if (Lexem=_quotedstr) and (ForwChar in [',',')']) then begin
      EditModeToFlags(LexWord,Flgs,true);
      Mode:=GetOp(_const,length(LexWord)+1); Mode^.S:=LexWord; RdLex end
    else Mode:=RdStrFrml end
  else if RdHeadLast(Head) then exit
  else if IsOpt('WATCH') then WatchDelayZ:=RdRealFrml
  else if IsOpt('WW') then begin
    Accept('('); WFlags:=0; if Lexem='(' then begin RdLex; WFlags:=WNoPop end;
    RdW(W); RdFrame(Top,WFlags); if Lexem=',' then begin
      RdLex; ZAttr:=RdAttr; Accept(','); ZdNorm:=RdAttr; Accept(',');
      ZdHiLi:=RdAttr; if Lexem=',' then begin
        RdLex; ZdSubSet:=RdAttr; if Lexem=',' then begin
          RdLex; ZdDel:=RdAttr; if Lexem=',' then begin
            RdLex; ZdTab:=RdAttr; if Lexem=',' then begin
              RdLex; ZdSelect:=RdAttr end end end end end;
    Accept(')'); if (WFlags and WNoPop)<>0 then Accept(')') end
  else if IsOpt('EXIT') then begin
    Accept('(');
  1:X:=GetZStore(sizeof(X^)); ChainLast(ExD,X);
    RdKeyList(X);
    if IsKeyWord('QUIT') then X^.Typ:='Q'
    else if IsKeyWord('REPORT') then begin
      if X^.AtWrRec or (LVRecPtr<>nil) then OldError(144);
      Accept('('); X^.Typ:='R'; RO:=GetRprtOpt; RdChptName('R',RO^.RprtPos,true);
      while Lexem=',' do begin
        RdLex;
        if IsOpt('ASSIGN') then RdPath(true,RO^.Path,RO^.CatIRec)
        else if IsKeyWord('EDIT') then RO^.Edit:=true
        else Error(130) end;
      X^.RO:=RO;Accept(')') end
    else if not (Lexem in [',',')']) then begin
      X^.Typ:='P'; X^.Proc:=RdProcArg('E')
      end;
    if Lexem=',' then begin RdLex; goto 1 end;
    Accept(')') end
  else if LVRecPtr<>nil then RdViewOpt:=false
  else if IsOpt('COND') then begin
    if Lexem='(' then begin
      RdLex; Cond:=RdKeyInBool(KIRoot,false,true,SQLFilter); Accept(')') end
    else Cond:=RdKeyInBool(KIRoot,false,true,SQLFilter) end
  else if IsOpt('JOURNAL') then begin
    Journal:=RdFileName; l:=Journal^.RecLen-13; if CFile^.Typ='X' then inc(l);
    if CFile^.RecLen<>l then OldError(111) end
  else if IsOpt('SAVEAFTER') then SaveAfterZ:=RdRealFrml
  else if IsOpt('REFRESH') then RefreshDelayZ:=RdRealFrml
  else RdViewOpt:=false;
end end;




function RdPInstr:InstrPtr;

function GetPD(Kind:PInstrCode;Size:word):InstrPtr;
var PD:InstrPtr;
begin
  PD:=GetPInstr(Kind,Size); RdLex; RdPInstr:=PD; GetPD:=PD;
end;

procedure RdProcCall;

function RdFlds:FieldList;
  var FLRoot,FL:FieldList;
  label 1;
  begin FLRoot:=nil;
      1:FL:=GetStore(sizeof(FL^));ChainLast(FLRoot,FL);FL^.FldD:=RdFldName(CFile);
        if Lexem=',' then begin RdLex;goto 1 end;
        RdFlds:=FLRoot end;
function RdSubFldList(InFL:FieldList;Opt:char):FieldList;
  var FLRoot,FL,FL1:FieldList; F:FieldDPtr;
  label 1,2;
  begin Accept('(');FLRoot:=nil;
      1:FL:=GetStore(sizeof(FL^));ChainLast(FLRoot,FL);
        if InFL=nil then F:=RdFldName(CFile)
        else begin TestIdentif;FL1:=InFL;while FL1<>nil do
           begin if EquUpcase(FL1^.FldD^.Name) then goto 2;FL1:=FL1^.Chain end;
           Error(43); 2:F:=FL1^.FldD;RdLex end;
        FL^.FldD:=F; if (Opt='S') and (F^.FrmlTyp<>'R') then OldError(20);
        if Lexem=',' then begin RdLex;goto 1 end;
        Accept(')');RdSubFldList:=FLRoot;
  end;

procedure RdSortCall;
var PD:InstrPtr; FD:FileDPtr;
begin
  PD:=GetPD(_sort,8); FD:=RdFileName; PD^.SortFD:=FD;
{$ifdef FandSQL} if FD^.typSQLFile then OldError(155);{$endif}  Accept(',');
  Accept('('); RdKFList(PD^.SK,PD^.SortFD); Accept(')');
end;

procedure RdEditCall;

procedure RdEditOpt(EO:EditOptPtr);
var lv:LocVarPtr;
begin with EO^ do
  if IsOpt('FIELD') then StartFieldZ:=RdStrFrml
  else if LVRecPtr<>nil then Error(125)
  else if IsOpt('OWNER') then begin
     if EO^.SQLFilter or (EO^.KIRoot<>nil) then OldError(179);
     OwnerTyp:=RdOwner(DownLD,DownLV) end
  else if IsOpt('RECKEY') then StartRecKeyZ:=RdStrFrml
  else if {$ifdef FandSQL} not CFile^.typSQLFile and {$endif}
      IsOpt('RECNO') then StartRecNoZ:=RdRealFrml
  else if IsOpt('IREC') then StartIRecZ:=RdRealFrml
  else if IsKeyWord('CHECK') then SyntxChk:=true
  else if IsOpt('SEL') then begin
      lv:=RdIdxVar; SelKey:=WKeyDPtr(lv^.RecPtr);
      if (ViewKey=nil) then OldError(108); if ViewKey=SelKey then OldError(184);
      if (ViewKey^.KFlds<>nil) and (SelKey^.KFlds<>nil) and
        not EquKFlds(SelKey^.KFlds,ViewKey^.KFlds) then OldError(178) end
  else Error(125);
end;
var PD:InstrPtr; EO:EditOptPtr; p:pointer; b:boolean; K:KeyDPtr;
    lv:LocVarPtr;
begin
  PD:=GetPD(_edit,8); EO:=GetEditOpt; PD^.EO:=EO;
  if IsRecVar(lv) then begin EO^.LVRecPtr:=lv^.RecPtr; CFile:=lv^.FD end
  else begin CFile:=RdFileName;
     K:=RdViewKey; if K=nil then K:=CFile^.Keys; EO^.ViewKey:=K end;
  PD^.EditFD:=CFile; Accept(',');
  if IsOpt('U') then begin
    TestIdentif; if CFile^.ViewNames=nil then Error(114);
    p:=SaveCompState;
    b:=RdUserView(LexWord,EO);
    RestoreCompState(p);
    if not b then Error(114); RdLex end
  else RdBegViewDcl(EO);
  while Lexem=',' do begin
    b:=RdViewOpt(EO);
    if not b then RdEditOpt(EO) end;
end;

procedure RdReportCall;
procedure RdRprtOpt(RO:RprtOptPtr;HasFrst:boolean);
var FD:FileDPtr; Low,N:word; br:boolean;
label 1,2;
begin with RO^ do
  if IsOpt('ASSIGN') then RdPath(true,Path,CatIRec)
  else if IsOpt('TIMES') then Times:=RdRealFrml
  else if IsOpt('MODE') then
    if IsKeyWord('ONLYSUM') then Mode:=_ATotal
    else if IsKeyWord('ERRCHECK') then Mode:=_AErrRecs else Error(49)
  else if IsKeyWord('COND') then begin
    if not HasFrst then goto 2;
    Low:=CurrPos; Accept(_equ); br:=false;
    if Lexem='(' then begin Low:=CurrPos; RdLex; br:=true;
      if Lexem='?' then begin RdLex; UserCondQuest:=true; goto 1 end end;
    FDL.Cond:=RdKeyInBool(FDL.KeyIn,true,true,FDL.SQLFilter);
    N:=OldErrPos-Low; CondTxt:=GetStore(N+1);
    Move(InpArrPtr^[Low],CondTxt^[1],N); CondTxt^[0]:=char(N);
1:  if br then Accept(')') end
  else if IsOpt('CTRL') then begin
    if not HasFrst then goto 2;
    Ctrl:=RdSubFldList(RO^.Flds,'C') end
  else if IsOpt('SUM') then begin
    if not HasFrst then goto 2;
    Sum:=RdSubFldList(RO^.Flds,'S') end
  else if IsOpt('WIDTH') then WidthFrml:=RdRealFrml
  else if IsOpt('STYLE') then
    if IsKeyWord('COMPRESSED') then Style:='C' else
    if IsKeyWord('NORMAL') then Style:='N' else Error(50)
  else if IsKeyWord('EDIT') then Edit:=true
  else if IsKeyWord('PRINTCTRL') then PrintCtrl:=true
  else if IsKeyWord('CHECK') then SyntxChk:=true
  else if IsOpt('SORT') then begin
    if not HasFrst then
2:     OldError(51);
    Accept('(');RdKFList(SK,CFile);Accept(')') end
  else if IsOpt('HEAD') then Head:=RdStrFrml
  else Error(45);
end;
var PD:InstrPtr; RO:RprtOptPtr; lv:LocVarPtr; FDL:RprtFDList; b:boolean;
    hasfrst:boolean;
label 1,2;
begin
  PD:=GetPD(_report,4); RO:=GetRprtOpt; PD^.RO:=RO; hasfrst:=false;
  if Lexem=',' then goto 2; hasfrst:=true;
  FDL:=@RO^.FDL; b:=false; if Lexem='(' then begin RdLex; b:=true end;
1:if IsRecVar(lv) then begin FDL^.LVRecPtr:=lv^.RecPtr; FDL^.FD:=lv^.FD end
  else begin CFile:=RdFileName; FDL^.FD:=CFile;
    CViewKey:=RdViewKey; FDL^.ViewKey:=CViewKey;
    if Lexem='(' then begin RdLex;
      FDL^.Cond:=RdKeyInBool(FDL^.KeyIn,true,true,FDL^.SQLFilter); Accept(')') end end;
  if b and (Lexem=',') then begin
    RdLex; FDL^.Chain:=GetZStore(sizeof(RprtFDListEl)); FDL:=FDL^.Chain; goto 1 end;
  if b then Accept(')');
  CFile:=RO^.FDL.FD; CViewKey:=RO^.FDL.ViewKey;
2:Accept(',');
  if Lexem='[' then begin
    RdLex; RO^.RprtPos.R:=RdbDPtr(RdStrFrml); RO^.RprtPos.IRec:=0;
    RO^.FromStr:=true; Accept(']') end
  else if not hasfrst or (Lexem=_identifier) then begin
    TestIdentif; if not FindChpt('R',LexWord,false,RO^.RprtPos) then Error(37);
    RdLex end
  else begin Accept('(');
    case Lexem of
     '?':begin RO^.Flds:=AllFldsList(CFile,false);RdLex;RO^.UserSelFlds:=true end;
     ')':RO^.Flds:=AllFldsList(CFile,true);
     else RO^.Flds:=RdFlds;if Lexem='?' then begin RdLex;RO^.UserSelFlds:=true end end;
    Accept(')') end;
  while Lexem=',' do begin
    RdLex; RdRprtOpt(RO,(hasfrst and (FDL^.LVRecPtr=nil))) end;
  if (RO^.Mode=_ALstg) and ((RO^.Ctrl<>nil) or (RO^.Sum<>nil)) then RO^.Mode:=_ARprt;
end;

procedure RdRDBCall;
var PD:InstrPtr; s:string[1];
begin
  PD:=GetPD(_call,12); s[0]:=#0; if Lexem='\' then begin s:=Lexem; RdLex end;
  TestIdentif; if length(Lexword)>8 then Error(2);
  PD^.RdbNm:=StoreStr(s+LexWord); RdLex;
  if Lexem=',' then begin
    RdLex; TestIdentif; if length(LexWord)>12 then Error(2);
    PD^.ProcNm:=StoreStr(LexWord); RdLex; PD^.ProcCall:=RdProcArg('C') end
  else PD^.ProcNm:=StoreStr('main');
end;

procedure RdExec;
var PD:InstrPtr; FD:FileDPtr;
begin
  PD:=GetPD(_exec,14); RdPath(true,PD^.ProgPath,PD^.ProgCatIRec);
  Accept(','); PD^.Param:=RdStrFrml;
  while Lexem=',' do begin
    RdLex; if IsKeyWord('NOCANCEL') then PD^.NoCancel:=true
    else if IsKeyWord('FREEMEM') then PD^.FreeMm:=true
    else if IsKeyWord('LOADFONT') then PD^.LdFont:=true
    else if IsKeyWord('TEXTMODE') then PD^.TextMd:=true
    else Error(101) end;
end;

procedure RdCopyFile;
function RdCOpt:CpOption;
var i:byte;
const OptArr:array[1..3] of string3=('FIX','VAR','TXT');
begin
  RdLex; TestIdentif;
  for i:=1 to 3 do
    if EquUpcase(OptArr[i]) then begin RdLex; RdCOpt:=CpOption(i); exit end;
  Error(53);
end;
function RdX(FD:FileDPtr):boolean;
begin
  RdX:=false; if (Lexem='.') and (FD<>nil) then begin RdLex;
    AcceptKeyWord('X'); if FD^.Typ<>'X' then OldError(108); RdX:=true end;
end;
function TestFixVar(Opt:CpOption;FD1,FD2:FileDPtr):boolean;
begin
  if (Opt<>cpNo) and (FD1<>nil) then OldError(139); TestFixVar:=false;
  if Opt in [cpFix,cpVar] then begin
    TestFixVar:=true; if FD2=nil then OldError(139) end;
end;
function RdList(var S:StringPtr):boolean;
begin
  RdList:=false; if Lexem<>'(' then exit;
  RdLex; S:=StringPtr(RdStrFrml); Accept(')'); RdList:=true;
end;
const ModeTxt:array[1..7]of string[2]=('KL','LK','KN','LN','LW','KW','WL');
var PD:InstrPtr; F:FieldDPtr; i:word; D:CopyDPtr; noapp:boolean;
label 1;
begin
  PD:=GetPD(_copyfile,4); noapp:=false;
  D:=GetZStore(sizeof(D^)); PD^.CD:=D; with D^ do begin
    FD1:=RdPath(false,Path1,CatIRec1); WithX1:=RdX(FD1);
    if Lexem='/' then if FD1<>nil then begin CFile:=FD1;ViewKey:=RdViewKey end
    else Opt1:=RdCOpt;
    Accept(',');
    FD2:=RdPath(false,Path2,CatIRec2); WithX2:=RdX(FD2);
    if Lexem='/' then if FD2<>nil then Error(139) else Opt2:=RdCOpt;
    if not TestFixVar(Opt1,FD1,FD2) and not TestFixVar(Opt2,FD2,FD1) then begin
      if (Opt1=cpTxt) and (FD2<>nil) then OldError(139);
      noapp:= (FD1=nil) xor (FD2=nil);
{$ifdef FandSQL} if noapp then
      if (FD1<>nil) and (FD1^.typSQLFile) or (FD2<>nil) and (FD2^.typSQLFile)
        then OldError(155) {$endif}  end;
    while Lexem=',' do begin
      RdLex;
      if IsOpt('HEAD') then begin
        HdFD:=RdFileName; Accept('.'); HdF:=RdFldName(HdFD);
        if (HdF^.FrmlTyp<>'S') or not HdFD^.IsParFile or
          (Opt1 in [cpFix,cpVar]) and (HdF^.Flg and f_Stored=0) then Error(52) end
      else if IsOpt('MODE') then begin
        TestLex(_quotedstr); for i:=1 to 7 do
          if SEquUpcase(LexWord,ModeTxt[i]) then begin Mode:=i; goto 1 end;
        Error(142);
1:      RdLex end
      else if IsKeyWord('NOCANCEL') then NoCancel:=true
      else if IsKeyWord('APPEND') then begin
        if noapp then OldError(139); Append:=true end
      else Error(52);
  end end;
end;

procedure RdPrintTxt;
var PD:InstrPtr;
begin
  PD:=GetPD(_printtxt,10); with PD^ do
  if FindLocVar(LVBD.Root,TxtLV) then begin RdLex;TestString(TxtLV^.FTyp) end
  else RdPath(true,TxtPath,TxtCatIRec);
end;

procedure RdEditTxt;
var PD:InstrPtr; pX:EdExitDPtr;
label 1,2;
begin PD:=GetPD(_edittxt,73); with PD^ do begin
  if FindLocVar(LVBD.Root,TxtLV) then begin RdLex;TestString(TxtLV^.FTyp) end
  else RdPath(true,TxtPath,TxtCatIRec);
  EdTxtMode:='T';
  while Lexem=',' do begin
    RdLex;
    if IsOpt('WW') then begin
      Accept('('); if Lexem='(' then begin RdLex; WFlags:=WNoPop end;
      RdW(Ww); RdFrame(Hd,WFlags);
      if Lexem=',' then begin RdLex; Atr:=RdAttr end;
      Accept(')'); if (WFlags and WNoPop)<>0 then Accept(')') end else
    if IsOpt('TXTPOS') then TxtPos:=RdRealFrml else
    if IsOpt('TXTXY') then TxtXY:=RdRealFrml else
    if IsOpt('ERRMSG') then ErrMsg:=RdStrFrml else
    if IsOpt('EXIT') then begin  Accept('(');
1:    pX:=GetZStore(sizeof(pX^)); ChainLast(ExD,pX);
2:    RdKeyCode(pX); if Lexem=',' then begin RdLex; goto 2 end;
      Accept(':');
      if IsKeyWord('QUIT') then pX^.Typ:='Q' else
      if not (Lexem in [',',')']) then begin
        pX^.Typ:='P'; pX^.Proc:=RdProcArg('T') end;
      if Lexem=',' then begin RdLex; goto 1 end;
      Accept(')') end else
    if RdHeadLast(Head) then
    else if IsKeyWord('NOEDIT') then EdTxtMode:='V'
    else Error(161) end;
end end;

procedure RdPutTxt;
var PD:InstrPtr;
begin
  PD:=GetPD(_puttxt,11); RdPath(true,PD^.TxtPath,PD^.TxtCatIRec);
  Accept(','); PD^.Txt:=RdStrFrml;
  if Lexem=',' then begin RdLex; AcceptKeyWord('APPEND'); PD^.App:=true end;
end;

procedure RdTurnCat;
var PD:InstrPtr; Frst,I:word; RN,FN:string;
begin
  PD:=GetPD(_turncat,12); TestIdentif; PD^.NextGenFD:=FindFileD;
  Frst:=GetCatIRec(LexWord,true); TestCatError(Frst,LexWord,true); RdLex;
  PD^.FrstCatIRec:=Frst; RN:=RdCatField(Frst,CatRdbName); FN:=RdCatField(Frst,CatFileName);
  I:=Frst+1;
  while (CatFD^.NRecs>=I) and SEquUpcase(RN,RdCatField(I,CatRdbName))
       and SEquUpcase(FN,RdCatField(I,CatFileName)) do inc(I);
  if I=Frst+1 then OldError(98); PD^.NCatIRecs:=I-Frst;
  Accept(','); PD^.TCFrml:=RdRealFrml;
end;

procedure RdWriteln(OpKind:byte);
var d:WrLnD; w:WrLnDPtr; pd:InstrPtr; z:FrmlPtr; n:word;
label 1;
begin
  RdLex; z:=nil; fillchar(d,sizeof(d),0); w:=@d;
1:with w^ do begin
    Frml:=RdFrml(Typ); if Typ='R' then begin
      Typ:='F'; if Lexem=':' then begin RdLex;
        if Lexem=_quotedstr then begin
          Typ:='D'; Mask:=StoreStr(LexWord); RdLex end
        else begin N:=RdInteger;
          if Lexem=':' then begin RdLex;
            if Lexem='-' then begin RdLex; M:=-RdInteger end
            else M:=RdInteger end end end end end;
  if Lexem=',' then begin RdLex;
    if (OpKind=2) and IsOpt('HELP') then z:=RdStrFrml
    else begin w:=GetZStore(sizeof(d)); ChainLast(d,w); goto 1 end end;
  n:=1+sizeof(d); if z<>nil then begin OpKind:=3; inc(n,8) end;
  pd:=GetPInstr(_writeln,n); with pd^ do begin
    LF:=OpKind; WD:=d; if OpKind=3 then begin mHlpRdb:=CRdb; mHlpFrml:=z end end;
  RdPInstr:=pd;
end;

procedure RdReleaseDrive;
var PD:InstrPtr;
begin
  PD:=GetPD(_releasedrive,4); PD^.Drive:=RdStrFrml;
end;

procedure RdIndexfile;
var PD:InstrPtr;
begin
  PD:=GetPD(_indexfile,5); PD^.IndexFD:=RdFileName;
  if PD^.IndexFD^.Typ<>'X' then OldError(108); if Lexem=',' then begin
    RdLex; AcceptKeyword('COMPRESS'); PD^.Compress:=true end;
end;
procedure RdGetIndex;
var PD:InstrPtr; lv,lv2:LocVarPtr; b:boolean; ld:LinkDPtr; k:KeyDPtr;
begin
  PD:=GetPD(_getindex,31); lv:=RdIdxVar; PD^.giLV:=lv; Accept(',');
  PD^.giMode:=' '; if Lexem in ['+','-'] then begin
    PD^.giMode:=Lexem; RdLex; Accept(','); PD^.giCond{RecNr}:=RdRealFrml;
    exit end;
  CFile:=RdFileName;
  if lv^.FD<>CFile then OldError(164); CViewKey:=RdViewKey; PD^.giKD:=CViewKey;
  while Lexem=',' do begin RdLex;
    if IsOpt('SORT') then begin
      if WKeyDPtr(lv^.RecPtr)^.KFlds<>nil then OldError(175);
      Accept('('); RdKFList(PD^.giKFlds,CFile); Accept(')') end
    else if IsOpt('COND') then begin
      Accept('('); PD^.giCond:=RdKeyInBool(PD^.giKIRoot,false,true,PD^.giSQLFilter);
      Accept(')') end
    else if IsOpt('OWNER') then begin
      PD^.giOwnerTyp:=RdOwner(PD^.giLD,PD^.giLV2); k:=GetFromKey(PD^.giLD);
      if CViewKey=nil then PD^.giKD:=k else if CViewKey<>k then OldError(178);
      end
    else Error(167);
    if (PD^.giOwnerTyp<>#0) and (PD^.giSQLFilter or (PD^.giKIRoot<>nil)) then
      Error(179);
    end;
end;

procedure RdGotoXY;
var PD:InstrPtr;
begin
  PD:=GetPD(_gotoxy,8); PD^.GoX:=RdRealFrml; Accept(','); PD^.GoY:=RdRealFrml;
end;

procedure RdClrWw;
var PD:InstrPtr;
begin
  PD:=GetPD(_clrww,24); with PD^ do begin
    RdW(W); if Lexem=',' then begin
      RdLex; if Lexem<>',' then Attr:=RdAttr;
      if Lexem=',' then begin RdLex; FillC:=RdStrFrml end end end;
end;

procedure RdMount;
var PD:InstrPtr; FD:FileDPtr; I:word;
begin
  PD:=GetPD(_mount,3); TestIdentif; FD:=FindFileD;
  if FD=nil then I:=GetCatIRec(LexWord,true) else I:=FD^.CatIRec;
  TestCatError(I,LexWord,false); RdLex; PD^.MountCatIRec:=I;
  if Lexem=',' then begin
    RdLex; AcceptKeyWord('NOCANCEL'); PD^.MountNoCancel:=true end;
end;

procedure RdDisplay;
var PD:InstrPtr; s:StringPtr;
begin
  PD:=GetPD(_display,sizeof(RdbPos));
  if (Lexem=_identifier) and FindChpt('H',LexWord,false,PD^.Pos) then RdLex
  else with PD^.Pos do begin R:=RdbDPtr(RdStrFrml); IRec:=0 end;
end;

procedure RdGraphP;
var PD:InstrPtr;
type FrmlArr = array[1..15] of FrmlPtr; FrmlArrPtr=^FrmlArr;
var i:word; FAr:FrmlArrPtr; VD:GraphVDPtr; WD:GraphWDPtr; RGBD:GraphRGBDPtr;
	Ww:WinGPtr;
const Nm1:array[1..11]of string[7]=
('TYPE','HEAD','HEADX','HEADY','HEADZ','FILL','DIRX','GRID','PRINT','PALETTE','ASSIGN');
const Nm2:array[1..6]of string[6]=
('WIDTH','RECNO','NRECS','MAX','MIN','GRPOLY');
label 1;
begin
  PD:=GetPD(_graph,4); PD^.GD:=GetZStore(sizeof(GraphD)); with PD^.GD^ do begin
  if IsOpt('GF') then GF:=RdStrFrml
  else begin FD:=RdFileName; CFile:=FD; CViewKey:=RdViewKey; ViewKey:=CViewKey;
	Accept(','); Accept('('); X:=RdFldName(FD); i:=0;
	repeat Accept(','); ZA[i]:=RdFldName(FD); inc(i);
	until (i>9) or (Lexem<>',');  Accept(')') end;
  while Lexem=',' do begin
	RdLex;
	for i:=1 to 11 do if IsOpt(Nm1[i]) then begin
	   FAr:=FrmlArrPtr(@T); FAr^[i]:=RdStrFrml; goto 1 end;
	for i:=1 to 6 do if IsOpt(Nm2[i]) then begin
	   FAr:=FrmlArrPtr(@S); FAr^[i]:=RdRealFrml; goto 1 end;
	if IsDigitOpt('HEADZ',i) then HZA[i]:=RdStrFrml
	else if IsKeyWord('INTERACT') then Interact:=true
	else if IsOpt('COND') then begin
	  if Lexem='(' then begin RdLex; Cond:=RdKeyInBool(KeyIn,false,true,SQLFilter); Accept(')') end
	  else Cond:=RdKeyInBool(KeyIn,false,true,SQLFilter) end
	else if IsOpt('TXT') then begin
	   VD:=GetZStore(sizeof(VD^)); ChainLast(V,VD); with VD^ do begin
	   Accept('('); XZ:=RdRealFrml;Accept(',');YZ:=RdRealFrml;Accept(',');
	   Velikost:=RdRealFrml;Accept(',');BarPis:=RdStrFrml;Accept(',');
	   Text:=RdStrFrml; Accept(')') end end
	else if IsOpt('TXTWIN') then begin
	  WD:=GetZStore(sizeof(WD^)); ChainLast(W,WD); with WD^ do begin
	  Accept('('); XZ:=RdRealFrml;Accept(',');YZ:=RdRealFrml;Accept(',');
	  XK:=RdRealFrml;Accept(',');YK:=RdRealFrml;Accept(',');
	  BarPoz:=RdStrFrml;Accept(',');BarPis:=RdStrFrml;Accept(',');
	  Text:=RdStrFrml; Accept(')') end end
	else if IsOpt('RGB') then begin
	   RGBD:=GetZStore(sizeof(RGBD^)); ChainLast(RGB,RGBD); with RGBD^ do begin
	   Accept('('); Barva:=RdStrFrml;Accept(',');R:=RdRealFrml;Accept(',');
	   G:=RdRealFrml;Accept(',');B:=RdRealFrml; Accept(')') end end
	else if IsOpt('WW') then begin
	   Ww:=GetZStore(Sizeof(Ww^));
	   Accept('('); if Lexem='(' then begin RdLex; Ww^.WFlags:=WNoPop end;
	   RdW(Ww^.W); RdFrame(Ww^.Top,Ww^.WFlags);
	   if Lexem=',' then begin
		 RdLex; Ww^.ColBack:=RdStrFrml; Accept(','); Ww^.ColFor:=RdStrFrml;
		 Accept(','); Ww^.ColFrame:=RdStrFrml end;
	   Accept(')'); if (Ww^.WFlags and WNoPop)<>0 then Accept(')') end
	else Error(44);
1:  end end;
end;

procedure RdMixRecAcc(Op:PInstrCode);
var PD:InstrPtr; cf:FileDPtr; Z:FrmlPtr; K:KeyDPtr; FTyp:char;
begin
  cf:=CFile;
  if (Op=_appendrec) or (Op=_recallrec) then begin
    PD:=GetPD(Op,9); CFile:=RdFileName; PD^.RecFD:=CFile;
    {$ifdef FandSQL} if CFile^.typSQLFile then OldError(155) {$endif};
    if Op=_recallrec then begin Accept(','); PD^.RecNr:=RdRealFrml end end
  else begin
    PD:=GetPD(Op,15);
    if Op=_deleterec then begin CFile:=RdFileName; PD^.RecFD:=CFile end
    else begin {_readrec,_writerec}
      if not IsRecVar(PD^.LV) then Error(141); CFile:=PD^.LV^.FD end;
    K:=RdViewKey; Accept(','); {$ifdef FandSQL}
    if CFile^.typSQLFile and (Lexem in [_equ,_le,_gt,_lt,_ge]) then begin
      PD^.CompOp:=Lexem; RdLex end; {$endif}
    Z:=RdFrml(FTyp); PD^.RecNr:=Z; case FTyp of
     'B':OldError(12);
     'S':begin PD^.ByKey:=true; if PD^.CompOp=#0 then PD^.CompOp:=_equ;
         if K=nil then K:=CFile^.Keys; PD^.Key:=K;
         if (K=nil) and (not CFile^.IsParFile or (Z^.Op<>_const)
            or (Z^.S[0]<>#0)) then OldError(24) end; {$ifdef FandSQL}
     else if PD^.CompOp<>#0 then OldError(19);
         if CFile^.typSQLFile and ((Op=_deleterec) or (Z^.Op<>_const)
         or (Z^.R<>0)) then Error(155);              {$endif}
    end end;
  if (Lexem=',') and (Op in [_writerec,_deleterec,_recallrec]) then begin
    RdLex; Accept('+'); PD^.AdUpd:=true end;
  CFile:=cf;
end;

procedure RdLinkRec;
var PD:InstrPtr; LV:LocVarPtr; LD:LinkDPtr;
begin
  PD:=GetPD(_linkrec,12); if not IsRecVar(PD^.RecLV1) then Error(141);
  Accept(','); CFile:=PD^.RecLV1^.FD;
  if IsRecVar(LV) then begin
    LD:=FindLD(LV^.FD^.Name); if LD=nil then OldError(154) end
  else begin
    TestIdentif; LD:=FindLD(LexWord); if LD=nil then Error(9); RdLex;
    Accept('('); LV:=RdRecVar; if LD^.ToFD<>LV^.FD then OldError(141);
    Accept(')') end;
  PD^.RecLV2:=LV; PD^.LinkLD:=LD;
end;

procedure RdBackup(MTyp:char;IsBackup:boolean);
var PD:InstrPtr; i:word; cr:pointer; cf:FileDPtr;
label 1,2;
begin
  if MTyp='M' then PD:=GetPD(_backupm,15) else PD:=GetPD(_backup,5);
  PD^.IsBackup:=IsBackup;
  TestIdentif; cf:=CFile; cr:=CRecPtr; CFile:=CatFD; CRecPtr:=GetRecSpace;
  for i:=1 to CatFD^.NRecs do begin ReadRec(i);
    if SEquUpcase(trailchar(' ',_shorts(CatRdbName)),'ARCHIVES') and
       SEquUpcase(trailchar(' ',_shorts(CatFileName)),LexWord) then begin
     RdLex; PD^.BrCatIRec:=i; ReleaseStore(CRecPtr);
     CFile:=cf; CRecPtr:=cr;
     goto 1 end end;
  Error(88);
1:if MTyp='M' then begin
    Accept(',');PD^.bmDir:=RdStrFrml;
    if IsBackup then begin Accept(','); PD^.bmMasks:=RdStrFrml end end;
  while Lexem=',' do begin
    RdLex;
    if MTyp='M' then begin
      if not IsBackup and IsKeyWord('OVERWRITE') then begin PD^.bmOverwr:=true; goto 2 end;
      if IsKeyWord('SUBDIR') then begin PD^.bmSubDir:=true; goto 2 end end;
    if IsKeyWord('NOCOMPRESS') then PD^.NoCompress:=true else
    begin AcceptKeyWord('NOCANCEL'); PD^.BrNoCancel:=true end;
2:  end;
end;

procedure RdSetEditTxt;
var PD:InstrPtr;
label 1;
begin
  PD:=GetPD(_setedittxt,7*4); with PD^ do begin
1:  if IsOpt('OVERWR') then Insert:=RdBool else
    if IsOpt('INDENT') then Indent:=RdBool else
    if IsOpt('WRAP') then Wrap:=RdBool else
    if IsOpt('ALIGN') then Just:=RdBool else
    if IsOpt('COLBLK') then ColBlk:=RdBool else
    if IsOpt('LEFT') then Left:=RdRealFrml else
    if IsOpt('RIGHT') then Right:=RdRealFrml else Error(160);
    if Lexem=',' then begin RdLex; goto 1 end end
end;          {$ifdef FandSQL}
procedure RdSqlRdWrTxt(Rd:boolean);
var pd:InstrPtr; k:KeyDPtr;
begin
  pd:=GetPD(_sqlrdwrtxt,23); with pd^ do begin
    IsRead:=Rd; RdPath(true,TxtPath,TxtCatIRec);
    Accept(','); CFile:=RdFileName; sqlFD:=CFile;
    k:=RdViewKey; if k=nil then k:=CFile^.Keys; sqlKey:=k; Accept(',');
    sqlFldD:=RdFldName(CFile); Accept(','); sqlXStr:=RdStrFrml;
    if not sqlFD^.typSQLFile or (sqlFldD^.Typ<>'T') then OldError(170) end;
end;          {$endif}  {$ifdef FandProlog}
procedure RdCallLProc;
var pd:InstrPtr;
begin
  pd:=GetPD(_lproc,sizeof(RdbPos)+4); RdChptName('L',pd^.lpPos,true);
  if Lexem=',' then begin
     RdLex; TestIdentif; pd^.lpName:=StoreStr(LexWord); RdLex end;
end;          {$endif}

                                     {RdProcCall - body}
var PD:InstrPtr;
label 1,2,3;
begin
  if IsKeyWord('EXEC') then RdExec
  else if IsKeyWord('COPYFILE') then RdCopyFile
  else if IsKeyWord('PROC') then begin RdLex; RdPInstr:=RdProcArg('P') end
  else if IsKeyWord('DISPLAY') then RdDisplay
  else if IsKeyWord('CALL') then RdRDBCall
  else if IsKeyWord('WRITELN') then RdWriteln(1)
  else if IsKeyWord('WRITE') then RdWriteln(0)
  else if IsKeyWord('HEADLINE') then begin PD:=GetPD(_headline,4);goto 1 end
  else if IsKeyWord('SETKEYBUF') then begin PD:=GetPD(_setkeybuf,4);goto 1 end
  else if IsKeyWord('HELP') then begin
    PD:=GetPD(_help,8); if CRdb^.HelpFD=nil then OldError(132);
    PD^.HelpRdb:=CRdb;
1:  PD^.Frml:=RdStrFrml  end
  else if IsKeyWord('MESSAGE') then RdWriteln(2)
  else if IsKeyWord('GOTOXY') then RdGotoXY
  else if IsKeyWord('MERGE') then begin
    PD:=GetPD(_merge,sizeof(RdbPos)); RdChptName('M',PD^.Pos,true) end
  else if IsKeyWord('SORT') then RdSortCall
  else if IsKeyWord('EDIT') then RdEditCall
  else if IsKeyWord('REPORT') then RdReportCall
  else if IsKeyWord('EDITTXT') then RdEditTxt
  else if IsKeyWord('PRINTTXT') then RdPrintTxt
  else if IsKeyWord('PUTTXT') then RdPutTxt
  else if IsKeyWord('TURNCAT') then RdTurnCat
  else if IsKeyWord('RELEASEDRIVE') then RdReleaseDrive
  else if IsKeyWord('SETPRINTER') then begin PD:=GetPD(_setprinter,4); goto 2 end
  else if IsKeyWord('INDEXFILE') then RdIndexfile
  else if IsKeyWord('GETINDEX') then RdGetIndex
  else if IsKeyWord('MOUNT') then RdMount
  else if IsKeyWord('CLRSCR') then RdClrWw
  else if IsKeyWord('APPENDREC') then RdMixRecAcc(_appendrec)
  else if IsKeyWord('DELETEREC') then RdMixRecAcc(_deleterec)
  else if IsKeyWord('RECALLREC') then RdMixRecAcc(_recallrec)
  else if IsKeyWord('READREC')   then RdMixRecAcc(_readrec)
  else if IsKeyWord('WRITEREC')  then RdMixRecAcc(_writerec)
  else if IsKeyWord('LINKREC') then RdLinkRec
  else if IsKeyWord('DELAY') then begin PD:=GetPD(_delay,4); goto 2 end
  else if IsKeyWord('SOUND') then begin PD:=GetPD(_sound,4);
2:  PD^.Frml:=RdRealFrml end
  {$ifdef FandProlog}
  else if IsKeyWord('LPROC') then RdCallLProc {$endif}
  {$ifdef FandGraph}
  else if IsKeyWord('GRAPH') then RdGraphP
  else if IsKeyWord('PUTPIXEL') then begin PD:=GetPD(_putpixel,3*4); goto 3 end
  else if IsKeyWord('LINE') then begin PD:=GetPD(_line,5*4); goto 3 end
  else if IsKeyWord('RECTANGLE') then begin PD:=GetPD(_rectangle,5*4); goto 3 end
  else if IsKeyWord('ELLIPSE') then begin PD:=GetPD(_ellipse,7*4); goto 3 end
  else if IsKeyWord('FLOODFILL') then begin PD:=GetPD(_floodfill,5*4); goto 3 end
  else if IsKeyWord('OUTTEXTXY') then begin PD:=GetPD(_outtextxy,11*4);
3:  PD^.Par1:=RdRealFrml; Accept(','); PD^.Par2:=RdRealFrml; Accept(',');
    if PD^.Kind=_outtextxy then begin PD^.Par3:=RdStrFrml; Accept(',');
      PD^.Par4:=RdRealFrml; Accept(','); PD^.Par5:=RdAttr;
      if Lexem=',' then begin RdLex; PD^.Par6:=RdRealFrml;
        if Lexem=',' then begin RdLex; PD^.Par7:=RdRealFrml;
          if Lexem=',' then begin RdLex; PD^.Par8:=RdRealFrml; Accept(',');
            PD^.Par9:=RdRealFrml; Accept(','); PD^.Par10:=RdRealFrml; Accept(',');
            PD^.Par11:=RdRealFrml end end end end
    else if PD^.Kind=_putpixel then PD^.Par3:=RdAttr else begin
      PD^.Par3:=RdRealFrml; Accept(',');
      if PD^.Kind=_floodfill then PD^.Par4:=RdAttr else PD^.Par4:=RdRealFrml;
      Accept(','); PD^.Par5:=RdAttr;
      if (PD^.Kind=_ellipse) and (Lexem=',') then begin
        RdLex; PD^.Par6:=RdRealFrml; Accept(','); PD^.Par7:=RdRealFrml end end
    end                           {$endif}
  else if IsKeyWord('CLOSE') then begin
    PD:=GetPD(_closefds,4); PD^.clFD:=RdFileName  end
  else if IsKeyWord('BACKUP') then RdBackup(' ',true)
  else if IsKeyWord('BACKUPM') then RdBackup('M',true)
  else if IsKeyWord('RESTORE') then RdBackup(' ',false)
  else if IsKeyWord('RESTOREM') then RdBackup('M',false)
  else if IsKeyWord('SETEDITTXT') then RdSetEditTxt
  else if IsKeyWord('SETMOUSE') then begin
    PD:=GetPD(_setmouse,12); PD^.MouseX:=RdRealFrml; Accept(',');
    PD^.MouseY:=RdRealFrml; Accept(','); PD^.Show:=RdBool end
  else if IsKeyWord('CHECKFILE') then begin
    PD:=GetPD(_checkfile,10); PD^.cfFD:=RdFileName;
    with PD^.cfFD^ do if (Typ in ['8','D']) {$ifdef FandSQL}or typSQLFile{$endif} then OldError(169);
    Accept(','); RdPath(true,PD^.cfPath,PD^.cfCatIRec) end   {$ifdef FandSQL}
  else if IsKeyWord('SQL') then begin PD:=GetPD(_sql,4); goto 1 end
  else if IsKeyWord('LOGIN') then begin
    PD:=GetPD(_login,8); with PD^ do begin
      liName:=RdStrFrml; Accept(','); liPassWord:=RdStrFrml end end
  else if IsKeyWord('SQLRDTXT') then RdSqlRdWrTxt(true)
  else if IsKeyWord('SQLWRTXT') then RdSqlRdWrTxt(false)     {$endif}
  else if IsKeyWord('PORTOUT') then begin
    PD:=GetPD(_portout,12); with PD^ do begin
      IsWord:=RdBool; Accept(','); Port:=RdRealFrml; Accept(','); PortWhat:=RdRealFrml end end
  else Error(34);
  Accept(')');
end;

function AdjustComma(Z1:FrmlPtr;F:FieldDPtr;Op:char):FrmlPtr;
var Z,Z2:FrmlPtr;
begin
  AdjustComma:=Z1; if F^.Typ<>'F' then exit; if F^.Flg and f_Comma=0 then exit;
  Z2:=GetOp(_const,sizeof(float)); Z2^.R:=Power10[F^.M];
  Z:=GetOp(Op,0); Z^.P1:=Z1; Z^.P2:=Z2; AdjustComma:=Z;
end;
function MakeImplAssign(FD1,FD2:FileDPtr):AssignDPtr;
var F1,F2:FieldDPtr; ARoot,A:AssignDPtr; Z:FrmlPtr; FTyp:char; S:string;
begin
  S:=LexWord; ARoot:=nil; F1:=FD1^.FldD; while F1<>nil do begin
  if F1^.Flg and f_Stored<>0 then begin
    LexWord:=F1^.Name; F2:=FindFldName(FD2);
    if F2<>nil then begin
      A:=GetZStore(SizeOf(A^)); ChainLast(ARoot,A);
      if (F2^.FrmlTyp<>F1^.FrmlTyp) or (F1^.FrmlTyp='R')
         and (F1^.Typ<>F2^.Typ) then begin A^.Kind:=_zero;A^.FldD:=F1 end
      else begin A^.Kind:=_output;A^.OFldD:=F1;
        Z:=MakeFldFrml(F2,FTyp); Z:=AdjustComma(Z,F2,_divide);
        A^.Frml:=FrmlContxt(AdjustComma(Z,F1,_times),FD2,nil) end end end;
  F1:=F1^.Chain end;
  LexWord:=S; MakeImplAssign:=ARoot;
end;
procedure RdAssign;
var FD:FileDPtr; F:FieldDPtr; LV,LV2:LocVarPtr; PV:char;
    PD:InstrPtr; FName:ScreenStr; FTyp:char;
label 0,1,2,3;
begin
  if ForwChar='.' then
    if FindLocVar(LVBD.Root,lv) and (lv^.FTyp in ['r','i']) then begin
      FTyp:=lv^.FTyp; RdLex; RdLex; if FTyp='i' then begin
        AcceptKeyWord('NRECS'); Accept(_assign);
        if (Lexem<>_number) or (LexWord<>'0') then Error(183); RdLex;
        PD:=GetPInstr(_asgnxnrecs,4); PD^.xnrIdx:=WKeyDPtr(lv^.RecPtr) end
      else begin PD:=GetPInstr(_asgnrecfld,13); PD^.AssLV:=LV;
        F:=RdFldName(LV^.FD); PD^.RecFldD:=F; if F^.Flg and f_Stored=0 then OldError(14);
        FTyp:=F^.FrmlTyp;
0:      RdAssignFrml(FTyp,PD^.Add,PD^.Frml) end end
    else begin FName:=LexWord; FD:=FindFileD;
      if IsActiveRdb(FD) then Error(121); RdLex; RdLex;
      if IsKeyWord('ARCHIVES') then begin F:=CatArchiv; goto 1 end;
      if IsKeyWord('PATH') then begin F:=CatPathName; goto 1 end;
      if IsKeyWord('VOLUME') then begin F:=CatVolume;
1:      PD:=GetPInstr(_asgncatfield,16); PD^.FD3:=FD;
        PD^.CatIRec:=GetCatIRec(FName,true); PD^.CatFld:=F;
        TestCatError(PD^.CatIRec,FName,true);
        Accept(_assign);PD^.Frml3:=RdStrFrml end
      else if FD=nil then OldError(9)
      else if IsKeyWord('NRECS') then begin
        if FD^.Typ='0' then OldError(127); PD:=GetPInstr(_asgnnrecs,9);
        PD^.FD:=FD; FTyp:='R'; goto 0 end
      else begin if not FD^.IsParFile then OldError(64);
        PD:=GetPInstr(_asgnpar,13); PD^.FD:=FD; F:=RdFldName(FD); PD^.FldD:=F;
        if F^.Flg and f_Stored=0 then OldError(14);
        FTyp:=F^.FrmlTyp; goto 0 end end
  else if ForwChar='[' then begin
    PD:=GetPInstr(_asgnfield,18); FD:=RdFileName; PD^.FD:=FD; RdLex;
    {$ifdef FandSQL} if FD^.typSQLFile then OldError(155); {$endif}
    PD^.RecFrml:=RdRealFrml; Accept(']'); Accept('.'); F:=RdFldName(FD);
    PD^.FldD:=F; if F^.Flg and f_Stored=0 then OldError(14);
    PD^.IndexArg:=(FD^.Typ='X') and IsKeyArg(F,FD);
    RdAssignFrml(F^.FrmlTyp,PD^.Add,PD^.Frml) end
  else if FindLocVar(LVBD.Root,lv) then begin
    RdLex; FTyp:=lv^.FTyp; case FTyp of
     'f','i': OldError(140);
     'r':     begin Accept(_assign); if not IsRecVar(LV2) then Error(141);
         PD:=GetPInstr(_asgnrecvar,12); PD^.RecLV1:=LV; PD^.RecLV2:=LV2;
         PD^.Ass:=MakeImplAssign(LV^.FD,LV2^.FD) end
     else PD:=GetPInstr(_asgnloc,9); PD^.AssLV:=LV; goto 0 end end
  else if IsKeyWord('USERNAME') then begin PD:=GetPInstr(_asgnusername,4); goto 2 end
  else if IsKeyWord('CLIPBD') then begin PD:=GetPInstr(_asgnclipbd,4); goto 2 end
  else if IsKeyWord('ACCRIGHT') then begin PD:=GetPInstr(_asgnaccright,4);
2:  Accept(_assign); PD^.Frml:=RdStrFrml end
  else if IsKeyWord('EDOK') then begin
    PD:=GetPInstr(_asgnedok,4);Accept(_assign);PD^.Frml:=RdBool end
  else if IsKeyWord('RANDSEED') then begin PD:=GetPInstr(_asgnrand,4); goto 3 end
  else if IsKeyWord('TODAY') then begin PD:=GetPInstr(_asgnusertoday,4); goto 3 end
  else if IsKeyWord('USERCODE') then begin PD:=GetPInstr(_asgnusercode,4);
3:  Accept(_assign);PD^.Frml:=RdRealFrml end
  else begin RdLex;
    if Lexem=_assign then OldError(8) else OldError(34) end;
  RdPInstr:=PD;
end;
function RdWith:InstrPtr;
var P,p2:InstrPtr; Op:PInstrCode; ld:LockDPtr; i:LockMode;
label 1,2,3;
begin
  if IsKeyWord('WINDOW') then begin
    P:=GetPInstr(_window,29); Accept('(');
    if Lexem='(' then begin RdLex;P^.WithWFlags:=WNoPop end;
    RdW(P^.W); RdFrame(P^.Top,P^.WithWFlags);
    if Lexem=',' then begin RdLex; P^.Attr:=RdAttr end;
    Accept(')'); if (P^.WithWFlags and WNoPop)<>0 then Accept(')');
    AcceptKeyWord('DO'); P^.WwInstr:=RdPInstr  end
  else if IsKeyWord('SHARED') then begin Op:=_withshared; goto 1 end
  else if IsKeyWord('LOCKED') then begin Op:=_withlocked;
1:  P:=GetPInstr(Op,9+sizeof(LockD)); ld:=@P^.WLD;
2:  ld^.FD:=RdFileName;
    if Op=_withlocked then begin
      Accept('['); ld^.Frml:=RdRealFrml; Accept(']') end
    else begin
      Accept('('); for i:=NoExclMode to ExclMode do
        if IsKeyWord(LockModeTxt[i]) then begin ld^.Md:=i; goto 3 end;
      Error(100);
3:    Accept(')') end;
    if Lexem=',' then begin
      RdLex; ld^.Chain:=GetZStore(sizeof(LockD)); ld:=ld^.Chain; goto 2 end;
    AcceptKeyWord('DO'); P^.WDoInstr:=RdPInstr;
    if IsKeyWord('ELSE') then begin
      P^.WasElse:=true; P^.WElseInstr:=RdPInstr end end
  else if IsKeyWord('GRAPHICS') then begin
    P:=GetPInstr(_withgraphics,4); AcceptKeyWord('DO'); P^.WDoInstr:=RdPInstr end
  else Error(131);
  RdWith:=P;
end;
function RdUserFuncAssign:InstrPtr;
var pd:InstrPtr; lv:LocVarPtr;
begin
  if not FindLocVar(LVBD.Root,lv) then Error(34);
  RdLex; pd:=GetPInstr(_asgnloc,9); PD^.AssLV:=lv;
  RdAssignFrml(lv^.FTyp,pd^.Add,pd^.Frml); RdUserFuncAssign:=pd;
end;
  begin if IsKeyWord('IF') then RdPInstr:=RdIfThenElse
        else if IsKeyWord('WHILE') then RdPInstr:=RdWhileDo
        else if IsKeyWord('REPEAT') then RdPInstr:=RdRepeatUntil
        else if IsKeyWord('CASE') then RdPInstr:=RdCase
        else if IsKeyWord('FOR') then RdPInstr:=RdFor
        else if IsKeyWord('BEGIN') then RdPInstr:=RdBeginEnd
        else if IsKeyWord('BREAK') then RdPInstr:=GetPInstr(_break,0)
        else if IsKeyWord('EXIT') then RdPInstr:=GetPInstr(_exit,0)
        else if IsKeyWord('CANCEL') then RdPInstr:=GetPInstr(_cancel,0)
        else if Lexem=';' then RdPInstr:=nil
        else if IsRdUserFunc then RdPInstr:=RdUserFuncAssign
        else if IsKeyWord('MENULOOP') then RdPInstr:=RdMenuBox(true)
        else if IsKeyWord('MENU') then RdPInstr:=RdMenuBox(false)
        else if IsKeyWord('MENUBAR') then RdPInstr:=RdMenuBar
        else if IsKeyWord('WITH') then RdPInstr:=RdWith
        else if IsKeyWord('SAVE') then RdPInstr:=GetPInstr(_save,0)
        else if IsKeyWord('CLREOL') then RdPInstr:=GetPInstr(_clreol,0)
        else if IsKeyWord('FORALL') then RdPInstr:=RdForAll
        else if IsKeyWord('CLEARKEYBUF') then RdPInstr:=GetPInstr(_clearkeybuf,0)
        else if IsKeyWord('WAIT') then RdPInstr:=GetPInstr(_wait,0)
        else if IsKeyWord('BEEP') then RdPInstr:=GetPInstr(_beep,0)
        else if IsKeyWord('NOSOUND') then RdPInstr:=GetPInstr(_nosound,0)
        {$ifndef FandRunV}
        else if IsKeyWord('MEMDIAG') then RdPInstr:=GetPInstr(_memdiag,0)
        {$endif}
        else if IsKeyWord('RESETCATALOG') then RdPInstr:=GetPInstr(_resetcat,0)
        else if IsKeyWord('RANDOMIZE') then RdPInstr:=GetPInstr(_randomize,0)
        else if Lexem=_identifier then begin
             SkipBlank(false);
             if ForwChar='(' then RdProcCall
             else if IsKeyWord('CLRSCR') then RdPInstr:=GetPInstr(_clrscr,0)
             else if IsKeyWord('GRAPH') then RdPInstr:=GetPInstr(_graph,4)
             else if IsKeyWord('CLOSE') then RdPInstr:=GetPInstr(_closefds,4)
             else RdAssign end
        else Error(34)
  end;

procedure ReadProcHead;
begin
  ResetCompilePars; RdFldNameFrml:=RdFldNameFrmlP; RdFunction:=RdFunctionP;
  FileVarsAllowed:=false; IdxLocVarAllowed:=true; IsRdUserFunc:=false;
  RdLex; ResetLVBD; if Lexem='(' then begin
    RdLex; RdLocDcl(LVBD,true,true,'P'); Accept(')') end;
  if IsKeyWord('VAR') then RdLocDcl(LVBD,false,true,'P');
end;

function ReadProcBody:InstrPtr;
begin
  AcceptKeyWord('BEGIN');ReadProcBody:=RdBeginEnd;Accept(';');
  if Lexem<>^z then Error(40)
end;

procedure ReadDeclChpt;
var fc:FuncDPtr; typ:char; n:word; lv:LocVarPtr;
label 1;
begin
  RdLex;
1:if IsKeyWord('FUNCTION') then begin
    TestIdentif; fc:=FuncDRoot; while fc<>CRdb^.OldFCRoot do begin
      if EquUpcase(fc^.Name) then Error(26); fc:=fc^.Chain end;
    fc:=GetStore(sizeof(FuncD)-1+length(LexWord));
    fc^.Chain:=FuncDRoot; FuncDRoot:=fc;
    Move(LexWord,fc^.Name,length(LexWord)+1);
    RdFldNameFrml:=RdFldNameFrmlP;RdFunction:=RdFunctionP;@ChainSumEl:=nil;
    FileVarsAllowed:=false; IsRdUserFunc:=true;
    RdLex; ResetLVBD;
    Accept('('); if Lexem<>')' then RdLocDcl(LVBD,true,false,'D'); Accept(')');
    Accept(':');
    if IsKeyWord('REAL') then begin typ:='R'; n:=sizeof(float) end
    else if IsKeyWord('STRING') then begin typ:='S'; n:=sizeof(longint) end
    else if IsKeyWord('BOOLEAN') then begin typ:='B'; n:=sizeof(boolean) end
    else Error(39);
    lv:=GetZStore(sizeof(lv^)-1+length(fc^.Name)); ChainLast(LVBD.Root,lv);
    Move(fc^.Name,lv^.Name,length(fc^.Name)+1);
    with lv^ do begin FTyp:=typ; Op:=_getlocvar; BPOfs:=LVBD.Size end;
    fc^.FTyp:=typ; inc(LVBD.Size,n);
    Accept(';');
    if IsKeyWord('VAR') then RdLocDcl(LVBD,false,false,'D'); fc^.LVB:=LVBD;
    AcceptKeyWord('BEGIN');fc^.Instr:=RdBeginEnd;Accept(';');
    end
  else if Lexem=^z then exit else Error(40);
  goto 1;
end;

function GetEvalFrml(X:FrmlPtr):FrmlPtr;
var p,cr:pointer; s:LongStrPtr; z,z1:FrmlPtr; fTyp:char; cpos:word;
    er:ExitRecord; cf:FileDPtr; oldLVBD:LocVarBlkD; oldbp:procStkPtr;
label 1,2;
begin
  oldLVBD:=LVBD; oldbp:=MyBP; SetMyBP(ProcMyBP);
  z:=nil; cf:=CFile; cr:=CRecPtr; s:=RunLongStr(X^.P1);
  if s^.LL=0 then begin LastExitCode:=0; goto 2 end;
  LastExitCode:=1; p:=SaveCompState; ResetCompilePars;
  RdFldNameFrml:=RdFldNameFrmlP; RdFunction:=RdFunctionP;
  if X^.EvalFD=nil then FileVarsAllowed:=false else begin
    CFile:=X^.EvalFD; FileVarsAllowed:=true end;
  NewExit(Ovr,er); goto 1;
  SetInpLongStr(s,false); RdLex; z:=RdFrml(fTyp);
  if (fTyp<>X^.EvalTyp) or (Lexem<>^z) then z:=nil else LastExitCode:=0;
1:cpos:=CurrPos; RestoreExit(er); RestoreCompState(p);
  if LastExitCode<>0 then begin LastTxtPos:=cpos;
    if X^.EvalTyp='B' then begin z:=GetOp(_const,1); z^.B:=false end end;
  if z<>nil then begin z1:=z; z:=GetOp(_setmybp,0); z^.P1:=z1 end;
2:GetEvalFrml:=z; CFile:=cf; CRecPtr:=cr; SetMyBP(oldbp);
  LVBD:=oldLVBD; {for cond before cycle called when PushProcStk is not ready}
end;

end.