{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit rdedit;

interface

uses access,oaccess,rdrun;

procedure PushEdit;
procedure RdFormOrDesign(F:FileDPtr;FL:FieldList;FormPos:RdbPos);
procedure NewEditD(ParFD:FileDPtr;EO:EditOptPtr);

implementation

uses dos,drivers,base,obaseww,compile,rdfildcl,runfrml;

var E: EditDPtr absolute EditDRoot;

procedure PushEdit;
var E1:EditDPtr;
begin
  E1:=GetZStore(sizeof(E^)); with E1^.V do begin
    C1:=1; R1:=2; C2:=TxtCols; R2:=TxtRows-1 end;
  E1^.PrevE:=E; E:=E1 end;


procedure SToSL(var SLRoot;s:string);
  var SL:StringList;
  begin SL:=GetStore(length(s)+5); SL^.S:=s; ChainLast(SLRoot,SL) end;

procedure StoreRT(Ln:word;SL:StringList;NFlds:word);
  var RT:ERecTxtDPtr;
  begin if NFlds=0 then Error(81);
        RT:=GetStore(sizeof(RT^));ChainLast(E^.RecTxt,RT);
        RT^.N:=Ln;RT^.SL:=SL end;

procedure RdEForm(ParFD:FileDPtr;FormPos:RdbPos);
function FindScanNr(N:word):EFldDPtr;
var D,D1:EFldDPtr; M:word;
begin
  D:=E^.FirstFld; M:=$ffff; D1:=nil; while D<>nil do begin
    if (D^.ScanNr>=N) and (D^.ScanNr<M) then begin M:=D^.ScanNr; D1:=D end;
    D:=D^.Chain end;
  FindScanNr:=D1;
end;
var D,D1,PrevD:EFldDPtr; F:FieldDPtr; FL:FieldList; SLRoot:StringList;
    s:string; NPages,Col,Ln,Max,M,N,NFlds,i:word; FD1:FileDPtr;
    comment:boolean; c:char; a:byte;
label 1,2,3,4,5;
begin
  SetInpTT(FormPos,true); N:=0; Max:=0;
1:s:='';
  while not(ForwChar in ['#',^z,^m,'{']) do begin   { read headlines }
    s:=s+ForwChar;ReadChar end;
  case ForwChar of
    ^z: Error(76); '#': goto 2; '{':begin SkipBlank(true); goto 1 end end;
  ReadChar; if ForwChar=^j then ReadChar;
  SToSL(E^.HdTxt,s);inc(E^.NHdTxt);
  if E^.NHdTxt+1>E^.Rows then Error(102); goto 1;
                                                        { read field list }
2:ReadChar; ReadChar; Lexem:=CurrChar; Accept('_'); FD1:=RdFileName;
  if ParFD=nil then CFile:=FD1 else CFile:=ParFD;
  E^.FD:=CFile;
3:inc(N); D:=GetZStore(sizeof(D^));
  if Lexem=_number then begin
    M:=RdInteger; if M=0 then OldError(115); Accept(':'); D^.ScanNr:=M end
  else D^.ScanNr:=N;
  D1:=FindScanNr(D^.ScanNr);  ChainLast(E^.FirstFld,D);
  if (D1<>nil) and (D^.ScanNr=D1^.ScanNr) then Error(77);
  F:=RdFldName(CFile); D^.FldD:=F;
  FL:=GetStore(sizeof(FL^)); FL^.FldD:=F; ChainLast(E^.Flds,FL);
  if Lexem=',' then begin RdLex; goto 3 end;
  TestLex(';'); SkipBlank(true);
                                                     { read record lines }
  D:=E^.FirstFld; NPages:=0;
4:inc(NPages); Ln:=0; NFlds:=0; SLRoot:=nil;
5:s:=''; inc(Ln); Col:=E^.FrstCol;
  while not (ForwChar in [^m,^z,'\','{']) do
    if ForwChar='_' then begin
      if D=nil then Error(30); inc(NFlds);
      D^.Col:=Col; D^.Ln:=Ln; D^.Page:=NPages; M:=0;
      while ForwChar='_' do begin
        s:=s+' '; inc(M); inc(Col); ReadChar end;
      F:=D^.FldD; D^.L:=F^.L; if F^.Typ='T' then D^.L:=1;
      if (F^.Typ='A') and (M<F^.L) then D^.L:=M
      else if M<>D^.L then begin
        str(D^.L:2,s); Set2MsgPar(s,F^.Name); Error(79) end;
      if Col>E^.LastCol then Error(102);
      D:=D^.Chain end
    else begin
      if not SetStyleAttr(ForwChar,a) then begin
        if Col>E^.LastCol then Error(102); inc(Col) end;
      s:=s+ForwChar; ReadChar end;
  SToSL(SLRoot,s); c:=ForwChar; if c='\' then ReadChar;
  SkipBlank(true);
  if ForwChar<>^z then
    if (c='\') or (E^.NHdTxt+Ln=E^.Rows) then begin
      StoreRT(Ln,SLRoot,NFlds); goto 4 end
    else goto 5;
  StoreRT(Ln,SLRoot,NFlds); E^.NPages:=NPages;

  if D<>nil then Error(30);
  D:=FindScanNr(1); D^.ChainBack:=nil;
  for i:=2 to N do begin
    PrevD:=D; D:=FindScanNr(D^.ScanNr+1); D^.ChainBack:=PrevD end;
  E^.LastFld:=D; PrevD:=nil;
  while D<>nil do begin D^.Chain:=PrevD; PrevD:=D; D:=D^.ChainBack end;
  E^.FirstFld:=PrevD;
end;

procedure AutoDesign(FL:FieldList);
var NPages,Col,Ln,L,i,m,FldLen,maxcol:word; s:string; SLRoot:StringList;
    D,PrevD:EFldDPtr; F:FieldDPtr;
begin
  D:=EfldDPtr(@E^.FirstFld); PrevD:=nil; NPages:=1; s:=''; Ln:=0; SLRoot:=nil;
  Col:=E^.FrstCol; maxcol:=E^.LastCol-E^.FrstCol;
  while FL<>nil do begin
    F:=FL^.FldD; FL:=FL^.Chain;
    D^.Chain:=GetZStore(SizeOf(D^)); D:=D^.Chain; D^.ChainBack:=PrevD;
    PrevD:=D;
    D^.FldD:=F; D^.L:=F^.L; if D^.L>maxcol then D^.L:=maxcol;
    if (E^.FD^.Typ='C') and (D^.L>44) then D^.L:=44; {catalog pathname}
    FldLen:=D^.L; if F^.Typ='T' then D^.L:=1;
    L:=Length(F^.Name); if FldLen>L then L:=FldLen;
    if Col+L>E^.LastCol then begin
      SToSL(SLRoot,s); SToSL(SLRoot,''); inc(Ln,2); if Ln+2>E^.Rows then begin
        StoreRT(Ln,SLRoot,1); inc(NPages); Ln:=0; SLRoot:=nil end;
      Col:=E^.FrstCol; s:='' end;
    m:=(L-length(F^.Name)+1)div 2; for i:=1 to m do s:=s+' '; s:=s+F^.Name;
    m:=L-length(F^.Name)-m; for i:=1 to m+1 do s:=s+' ';
    D^.Col:=Col+(L-FldLen+1)div 2; D^.Ln:=Ln+2; D^.Page:=NPages;
    inc(Col,L+1) end;
  SToSL(SLRoot,s); SToSL(SLRoot,''); inc(Ln,2); StoreRT(Ln,SLRoot,1);
  D^.Chain:=nil; E^.LastFld:=D; E^.NPages:=NPages;
  if NPages=1 then with E^.RecTxt^ do
    if N=2 then begin
      E^.HdTxt:=SL; SL:=SL^.Chain; E^.HdTxt^.Chain:=nil;
      E^.NHdTxt:=1; N:=1; D:=E^.FirstFld;
      while D<>nil do begin dec(D^.Ln);D:=D^.Chain end;
      if E^.Rows=1 then begin E^.NHdTxt:=0;E^.HdTxt:=nil end end
    else if N<E^.Rows then begin
      s:=''; for i:=E^.FrstCol to E^.LastCol do s:=s+'-';
      SToSL(SL,s);inc(N) end;
end;

procedure RdFormOrDesign(F:FileDPtr;FL:FieldList;FormPos:RdbPos);
begin with E^ do begin
  FrstCol:=V.C1; FrstRow:=V.R1; LastCol:=V.C2; LastRow:=V.R2;
  if (WFlags and WHasFrame)<>0 then begin
    inc(FrstCol); dec(LastCol); inc(FrstRow); dec(LastRow) end;
  Rows:=LastRow-FrstRow+1;
  if FL=nil then begin
    ResetCompilePars; RdEForm(F,FormPos); IsUserForm:=true end
  else begin FD:=F; Flds:=FL; AutoDesign(FL) end;
end end;

procedure NewEditD(ParFD:FileDPtr;EO:EditOptPtr);

function FindEFld(F:FieldDPtr):EFldDPtr;
  var D:EFldDPtr;
  label 1;
  begin D:=E^.FirstFld;
      while D<>nil do begin if D^.FldD=F then goto 1; D:=D^.Chain end;
    1:FindEFld:=D;
  end;
procedure ZeroUsed;
  var D:EFldDPtr;
  begin D:=E^.FirstFld;while D<>nil do begin D^.Used:=False;D:=D^.Chain end end;
function LstUsedFld:EFldDPtr;
  var D:EFldDPtr;
  label 1;
  begin D:=E^.LastFld;while D<>nil do
        begin if D^.Used then goto 1;D:=D^.ChainBack end;
      1:LstUsedFld:=D;
  end;

procedure RdDepChkImpl;

procedure TestedFlagOff;
var F:FieldDPtr;
begin
  F:=CFile^.FldD; while F<>nil do
    begin F^.Typ:=char(ord(F^.Typ) and $7F); F:=F^.Chain end;
end;
procedure SetFrmlFlags(Z:FrmlPtr);
procedure SetFlag(F:FieldDPtr);
var D:EFldDPtr;
begin
  if (ord(F^.Typ) and $80)<>0 then exit; F^.Typ:=char(ord(F^.Typ) or $80);
  if F^.Flg and f_Stored<>0 then begin D:=FindEFld(F); if D<>nil then D^.Used:=true end
  else SetFrmlFlags(F^.Frml);
end;
  var Arg:KeyFldDPtr; fl:FrmlList;
  begin if Z=nil then exit;
      case Z^.Op of
       _field:SetFlag(Z^.Field);
       _access:if Z^.LD<>nil then
              begin Arg:=Z^.LD^.Args;while Arg<>nil do
                    begin SetFlag(Arg^.FldD);Arg:=Arg^.Chain end end;
       _userfunc:begin fl:=Z^.FrmlL; while fl<>nil do begin
              SetFrmlFlags(fl^.Frml); fl:=fl^.Chain end end;
       #$60..#$af{1-ary}:SetFrmlFlags(Z^.P1);
       #$b0..#$ef{2-ary}:begin SetFrmlFlags(Z^.P1);SetFrmlFlags(Z^.P2) end;
       #$f0..#$ff{3-ary}:begin SetFrmlFlags(Z^.P1);SetFrmlFlags(Z^.P2);
                               SetFrmlFlags(Z^.P3) end  end;
 end;

procedure RdDep;
  var Bool,Z:FrmlPtr; D:EFldDPtr; FTyp:char; Dp:DepDPtr;
  label 1,2;
  begin RdLex;
      1:Accept('(');Bool:=RdBool;Accept(')');
      2:D:=FindEFld(RdFldName(CFile));Accept(_assign);Z:=RdFrml(FTyp);
        if D<>nil then
        begin Dp:=GetStore(sizeof(Dp^));Dp^.Bool:=Bool;Dp^.Frml:=Z;
              ChainLast(D^.Dep,Dp) end;
        if Lexem=';' then
        begin RdLex;if not(Lexem in ['#',^z]) then
              begin if Lexem='(' then goto 1 else goto 2 end end;
  end;
procedure RdCheck;
  var Low:word; C:ChkDPtr; D:EFldDPtr;
  label 1;
  begin SkipBlank(false);Low:=CurrPos;RdLex;
    1:C:=RdChkD(Low);
      ZeroUsed; SetFrmlFlags(C^.Bool); TestedFlagOff;
      D:=LstUsedFld;
      if D<>nil then ChainLast(D^.Chk,C) else ReleaseStore(C);
      if Lexem=';' then
         begin SkipBlank(false);Low:=CurrPos;RdLex;
               if not(Lexem in ['#',^z]) then goto 1 end;
  end;

procedure RdImpl;
  var D:EFldDptr; Z:FrmlPtr; FTyp:char; F:FieldDPtr; ID:ImplDPtr;
  label 1;
  begin RdLex;
      1:F:=RdFldName(CFile);Accept(_assign);Z:=RdFrml(FTyp);
        D:=FindEFld(F);
        if D<>nil then D^.Impl:=Z
        else begin ID:=GetStore(sizeof(ID^));ID^.FldD:=F;ID^.Frml:=Z;
                   ChainLast(E^.Impl,ID) end;
        if Lexem=';' then
           begin RdLex;if not(Lexem in ['#',^z]) then goto 1 end;
  end;
procedure RdUDLI;
  begin RdLex;
        if (Lexem='#') and (ForwChar='U') then
            repeat RdLex until (Lexem in ['#',^z]);
        if (Lexem='#') and (ForwChar='D') then begin RdLex;RdDep end;
        if (Lexem='#') and (ForwChar='L') then begin RdLex;RdCheck end;
        if (Lexem='#') and (ForwChar='I') then begin RdLex;RdImpl end;
  end;
procedure RdAllUDLIs(FD:FileDPtr);
var r:RdbDPtr;
begin
  if FD^.OrigFD<>nil then RdAllUDLIs(FD^.OrigFD);
  if FD^.TxtPosUDLI<>0 then begin
    ResetCompilePars; SetInpTTxtPos(FD);
    r:=CRdb; CRdb:=FD^.ChptPos.R; RdUDLI; CRdb:=r end;
end;

  var s:string;
  label 1;
  begin CFile:=E^.FD;
        case CFile^.Typ of
         '0':begin RdMsg(53);s:=MsgLine;goto 1 end;
         'C':begin RdMsg(54);s:=MsgLine;
                 if spec.CPMDrive<>' ' then s:=s+','''+spec.CPMDrive+':''';
                 RdMsg(55);s:=s+MsgLine;
                 if spec.CPMDrive<>' ' then s:=s+','+spec.CPMDrive+':';
                 s:=s+'''';
               1:ResetCompilePars; SetInpStr(s);RdUDLI;exit end;
         else RdAllUDLIs(CFile);
  end    end;

function StandardHead:StringPtr;
var s:string; p:StringPtr;
const c:string[59]=
          '          ______                                 __.__.____';
begin
  if E^.ViewName<>nil then s:=E^.ViewName^ else if E^.EdRecVar then s:=''
  else begin s:=E^.FD^.Name;
    case E^.FD^.Typ of
     'X':begin p:=E^.VK^.Alias;
         if (p<>nil) and (p^<>'') then s:=s+'/'+E^.VK^.Alias^ end;
     '0':s:=s+'.RDB';
     '8':s:=s+'.DTA' end end;
  if length(s)>16 then s[0]:=#16;
  StandardHead:=StoreStr(copy(c,17,20-length(s))+s+c);
end;

function GetStr(Z:FrmlPtr):StringPtr;
var s:string;
begin
  if Z=nil then GetStr:=nil
  else begin
    s:=RunShortStr(Z); while LenStyleStr(s)>TxtCols do dec(s[0]);
    GetStr:=StoreStr(s) end;
end;
procedure NewChkKey;
var K:KeyDPtr; KF:KeyFldDPtr; D:EFldDPtr; KL:KeyList;
begin
  K:=CFile^.Keys; while K<>nil do begin
  if not K^.Duplic then begin ZeroUsed; KF:=K^.KFlds;
    while KF<>nil do begin
      D:=FindEFld(KF^.FldD); if D<>nil then D^.Used:=true; KF:=KF^.Chain end;
    D:=LstUsedFld; if D<>nil then begin
      KL:=GetStore(sizeof(KL^)); ChainLast(D^.KL,KL); KL^.Key:=K end end;
  K:=K^.Chain end;
end;



var D:EFldDPtr; FL:FieldList; p:pointer;                { NewEditD - body }
    i:word; s:string; F:FieldDPtr; b,b2,F2NoUpd:boolean;
begin
  PushEdit; MarkStore2(p); with E^ do begin
  Move(EO^.WFlags,WFlags,ofs(SelKey)-ofs(WFlags)+4);
  Attr:=RunWordImpl(EO^.ZAttr,colors.dTxt);
  dNorm:=RunWordImpl(EO^.ZdNorm,colors.dNorm);
  dHiLi:=RunWordImpl(EO^.ZdHiLi,colors.dHiLi);
  dSubSet:=RunWordImpl(EO^.ZdSubSet,colors.dSubSet);
  dDel:=RunWordImpl(EO^.ZdDel,colors.dDeleted);
  dTab:=RunWordImpl(EO^.ZdTab,Attr or $08);
  dSelect:=RunWordImpl(EO^.ZdSelect,colors.dSelect);
  Top:=StoreStr(RunShortStr(EO^.Top));
  if EO^.Mode<>nil then
    EditModeToFlags(RunShortStr(EO^.Mode),NoDelete,false);
  if spec.Prompt158 then Prompt158:=true;
  if EO^.SetOnlyView {UpwEdit} then begin
    EO^.Tab:=nil; OnlyTabs:=true; OnlySearch:=false end;
  if LVRecPtr<>nil then begin EdRecVar:=true; Only1Record:=true end;
  if Only1Record then OnlySearch:=false;
  if EO^.W.C1<>nil then begin
    RunWFrml(EO^.W,WFlags,V); WWPart:=true; if (WFlags and WShadow)<>0 then begin
      ShdwX:=minw(2,TxtCols-V.C2); ShdwY:=minw(1,TxtRows-V.R2) end end
  else begin
    if WithBoolDispl then V.R1:=3; if Mode24 then dec(V.R2) end;
  RdFormOrDesign(ParFD,EO^.Flds,EO^.FormPos);
  if NPages>1 then NRecs:=1
  else NRecs:=(Rows-NHdTxt) div RecTxt^.N;
  BaseRec:=1; IRec:=1;
  CFld:=FirstFld; FirstEmptyFld:=FirstFld;
  ChkSwitch:=true; WarnSwitch:=true;
  CFile:=FD; CRecPtr:=GetRecSpace; OldRecPtr:=CRecPtr;
  {$ifdef FandSQL}if CFile^.IsSQLFile then SetTWorkFlag;{$endif}
  if EdRecVar then begin
    NewRecPtr:=LVRecPtr; NoDelete:=true; NoCreate:=true;
    Journal:=nil; KIRoot:=nil end
  else begin
    CRecPtr:=GetRecSpace; NewRecPtr:=CRecPtr;
    {$ifdef FandSQL}if CFile^.IsSQLFile then SetTWorkFlag;{$endif}
    AddSwitch:=true; Cond:=RunEvalFrml(EO^.Cond);
    RefreshDelay:=RunWordImpl(EO^.RefreshDelayZ,spec.RefreshDelay)*18;
    SaveAfter:=RunWordImpl(EO^.SaveAfterZ,spec.UpdCount);
    if EO^.StartRecKeyZ<>nil then StartRecKey:=StoreStr(RunShortStr(EO^.StartRecKeyZ));
    StartRecNo:=RunInt(EO^.StartRecNoZ); StartIRec:=RunInt(EO^.StartIRecZ);
    VK:=EO^.ViewKey;
    if DownLD<>nil then begin
      DownSet:=true; DownKey:=GetFromKey(DownLD);
      if VK=nil then VK:=DownKey;
      case OwnerTyp of
        'r':DownRecPtr:=DownLV^.RecPtr;
        'F':begin OwnerRecNo:=RunInt(FrmlPtr(EO^.DownLV));
            CFile:=DownLD^.ToFD; DownRecPtr:=GetRecSpace; CFile:=FD end end end
    else if VK=nil then VK:=FD^.Keys;            {$ifdef FandSQL}
    if CFile^.IsSQLFile and (VK=nil) then begin
      SetMsgPar(CFile^.Name); RunError(652) end; {$endif}
    if SelKey<>nil then
      if SelKey^.KFlds=nil then SelKey^.KFlds:=VK^.KFlds else
      if not EquKFlds(SelKey^.KFlds,VK^.KFlds) then RunError(663);
    end;
  if EO^.StartFieldZ<>nil then begin
    s:=TrailChar(' ',RunShortStr(EO^.StartFieldZ)); D:=FirstFld;
    while D<>nil do begin
      if SEquUpcase(D^.FldD^.Name,s) then E^.StartFld:=D; D:=D^.Chain end end;
  WatchDelay:=RunInt(EO^.WatchDelayZ)*18;
  if EO^.Head=nil then Head:=StandardHead else Head:=GetStr(EO^.Head);
  Last:=GetStr(EO^.Last); AltLast:=GetStr(EO^.AltLast);
  CtrlLast:=GetStr(EO^.CtrlLast); ShiftLast:=GetStr(EO^.ShiftLast);
  F2NoUpd:=OnlyTabs and (EO^.Tab=nil) and not EO^.NegTab and OnlyAppend;
  D:=FirstFld;while D<>nil do begin
    inc(NFlds); F:=D^.FldD;
    b:=FieldInList(F,EO^.Tab); if EO^.NegTab then b:=not b;
    if b then begin D^.Tab:=true; inc(NTabsSet) end;
    b2:=FieldInList(F,EO^.NoEd); if EO^.NegNoEd then b2:= not b2;
    D^.EdU:=not(b2 or OnlyTabs and not b);
    D^.EdN:=F2NoUpd;
    if (F^.Flg and f_Stored<>0) and D^.EdU then inc(NEdSet);
    b:=FieldInList(F,EO^.Dupl); if EO^.NegDupl then b:=not b;
    if b and (F^.Flg and f_Stored<>0) then D^.Dupl:=true;
    if b or (F^.Flg and f_Stored<>0) then inc(E^.NDuplSet);
    D:=D^.Chain end;
  if OnlyTabs and (NTabsSet=0) then begin
    NoDelete:=true; if not OnlyAppend then NoCreate:=true end;
  RdDepChkImpl;
  NewChkKey;
  MarkStore(AfterE) end;
  ReleaseStore2(p);
end;

end.