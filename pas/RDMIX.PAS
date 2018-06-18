procedure RdLocDcl(var LVB:LocVarBlkD;IsParList,WithRecVar:boolean;CTyp:char);
function RdVarName:LocVarPtr;
var lv:LocVarPtr;
begin
  TestIdentif; lv:=LVB.Root; while lv<>nil do begin
    if EquUpcase(lv^.Name) then Error(26); lv:=lv^.Chain end;
  lv:=GetZStore(sizeof(lv^)-1+length(LexWord)); ChainLast(LVB.Root,lv);
  Move(LexWord,lv^.Name,length(LexWord)+1); RdLex; RdVarName:=lv;
  if IsParList then begin lv^.IsPar:=true; inc(LVB.NParam) end;
end;
var lv:LocVarPtr; Z:FrmlPtr; s:string; r:float; typ,lx,fc:char; sz,n:word;
    cf,fd:FileDPtr; cr,p:pointer; k:WKeyDPtr; rp:boolean; kf,kf1:KeyFldDPtr;
    FDTyp:char;
label 1,2,3;
begin
1:rp:=false; if IsParList and IsKeyWord('VAR') then begin
    if CTyp='D' then OldError(174); rp:=true end;
  lv:=RdVarName;
  if not IsParList then while Lexem=',' do begin RdLex; RdVarName end;
  Accept(':'); Z:=nil;
  if IsKeyWord('BOOLEAN') then begin
    if (Lexem=_equ) and not IsParList then begin
      RdLex; if IsKeyWord('TRUE') then begin
        Z:=GetOp(_const,sizeof(boolean));Z^.B:=true end
      else if not IsKeyWord('FALSE') then Error(42) end;
    typ:='B'; sz:=sizeof(boolean); goto 2 end
  else if IsKeyWord('REAL') then begin
    if (Lexem=_equ) and not IsParList then begin
      RdLex; r:=RdRealConst; if r<>0 then begin
        Z:=GetOp(_const,sizeof(float)); Z^.R:=r end end;
    typ:='R'; sz:=sizeof(float); goto 2 end
  else if IsKeyWord('STRING') then begin
    if (Lexem=_equ) and not IsParList then begin
      RdLex; s:=LexWord; Accept(_quotedstr); if s<>'' then begin
        Z:=GetOp(_const,length(s)+1); Z^.S:=s end end;
    typ:='S'; sz:=sizeof(longint);
2:  while lv<>nil do begin
      with lv^ do begin
        FTyp:=typ; Op:=_getlocvar; IsRetPar:=rp; Init:=Z; BPOfs:=LVB.Size end;
      inc(LVB.Size,sz); lv:=lv^.Chain end end
  else if rp then Error(168)
  else if WithRecVar then
  if TestKeyWord('FILE') then begin
    lv^.FTyp:='f'; LexWord:=lv^.Name; if length(LexWord)>8 then OldError(2);
    fd:=FindFileD; RdLex;
    if IsParList then begin
      if not WithRecVar then OldError(162);
      if fd=nil then OldError(163); lv^.FD:=fd end
    else begin
      if fd<>nil then OldError(26); FDTyp:='6';
      if Lexem='.' then begin RdLex; TestIdentif;
        if EquUpcase('X') then FDTyp:='X' else
        if EquUpcase('DBF') then FDTyp:='D' else Error(185); RdLex end;
      TestLex('[');
      p:=SaveCompState; RdFileD(lv^.Name,FDTyp,'$'); TestLex(']');
      lv^.FD:=CFile; n:=CurrPos; lx:=Lexem; fc:=ForwChar; RestoreCompState(p);
      CurrPos:=n; Lexem:=lx; ForwChar:=fc; RdLex end end
  else if IsKeyWord('INDEX') then begin typ:='i'; goto 3 end
  else if IsKeyWord('RECORD') then begin typ:='r';
3:    AcceptKeyWord('OF'); cf:=CFile; cr:=CRecPtr; CFile:=RdFileName;
      if typ='i' then begin
        if CFile^.Typ<>'X' then OldError(108); kf1:=nil;
        if Lexem='(' then begin RdLex; RdKFList(kf1,CFile); Accept(')') end end;
      while lv<>nil do begin
        lv^.FTyp:=typ; lv^.FD:=CFile;
        if typ='r' then lv^.RecPtr:=ptr(0,1) { for RdProc nil-tests + no Run}
           { frueher bei IsParList K:=nil; warum? }
        else begin
          k:=GetZStore(sizeof(k^)); k^.Duplic:=true; k^.InWork:=true;
          k^.KFlds:=kf1; kf:=kf1; while kf<>nil do begin
            inc(k^.IndexLen,kf^.FldD^.NBytes); kf:=kf^.Chain end;
          lv^.RecPtr:=k end;
        lv:=lv^.Chain end;
      CFile:=cf; CRecPtr:=cr end
    else Error(137)
  else Error(39);
  if IsParList then begin
    if Lexem=')' then exit else begin Accept(';');goto 1 end end;
  Accept(';');
  if (Lexem<>'#') and (Lexem<>'.') and not TestKeyWord('BEGIN') then
    goto 1;
end;

function FindLocVar(LVRoot:LocVarPtr;var LV:LocVarPtr):boolean;
begin
  FindLocVar:=false; if Lexem<>_identifier then exit;
  LV:=LVRoot; while LV<>nil do begin
    if EquUpcase(LV^.Name) then begin FindLocVar:=true; exit end;
    LV:=LV^.Chain end;
end;

function FindChpt(Typ:char;const Name:string;local:boolean;var RP:RdbPos):boolean;
var R:RdbDPtr; I:word;  CF:FileDPtr; CR:pointer;
label 1;
begin
  CF:=CFile;CR:=CRecPtr;CFile:=Chpt;CRecPtr:=GetRecSpace;R:=CRdb;
  FindChpt:=false;
  while R<>nil do begin CFile:=R^.FD;
    for I:=1 to CFile^.NRecs do begin
      ReadRec(I);
      if (_shorts(ChptTyp)=Typ) and
         SEquUpcase(TrailChar(' ',_shorts(ChptName)),Name) then begin
           RP.R:=R;RP.IRec:=I;FindChpt:=true;goto 1 end;
      end;
    if local then goto 1; R:=R^.ChainBack end;
1:ReleaseStore(CRecPtr);CFile:=CF;CRecPtr:=CR;
end;
procedure RdChptName(C:char;var Pos:RdbPos;TxtExpr:boolean);
begin
  if TxtExpr and (Lexem='[') then begin
    RdLex; Pos.R:=RdbDPtr(RdStrFrml); Pos.IRec:=0; Accept(']') end
  else begin
    TestLex(_identifier);if not FindChpt(C,LexWord,false,Pos) then Error(37);
    RdLex end;
end;


function AllFldsList(FD:FileDPtr;OnlyStored:boolean):FieldList;
  var FLRoot,FL:FieldList; F:FieldDPtr;
  begin F:=FD^.FldD; FLRoot:=nil;
        while F<>nil do begin if (F^.Flg and f_Stored<>0) or not OnlyStored then
                              begin FL:=GetStore(SizeOf(FL^));FL^.FldD:=F;
                                    ChainLast(FLRoot,FL) end;
                              F:=F^.Chain
                        end;
        AllFldsList:=FLRoot;
  end;

function GetEditOpt:EditOptPtr;
  var EO:EditOptPtr;
  begin EO:=GetZStore(sizeof(EO^)); GetEditOpt:=EO; EO^.UserSelFlds:=true end;
function GetRprtOpt:RprtOptPtr;
  var RO:RprtOptPtr;
  begin RO:=GetZStore(sizeof(RO^)); GetRprtOpt:=RO;
        RO^.Mode:=_ALstg; RO^.Style:='?'; RO^.Width:=spec.AutoRprtWidth end;


procedure CFileLikeFD(FD:FileDPtr;MsgNr:word);
var FD1:FileDPtr;
begin
  if not CFile^.IsJournal and ((CFile=FD) or (CFile^.OrigFD=FD)) then exit;
  Set2MsgPar(CFile^.Name,FD^.Name);RunError(MsgNr) end;

function RdHelpName:StringPtr;
var s:StringPtr;
begin
  if CRdb^.HelpFD=nil then Error(132);
  if Lexem<>_identifier then TestLex(_quotedstr);
  s:=StoreStr(LexWord); RdLex; RdHelpName:=s;
end;

function RdAttr:FrmlPtr;
var c:char; n:byte; z:FrmlPtr;
begin
  if Lexem='^' then begin RdLex; c:=char(ord(upcase(Rd1Char))-64);
    if not SetStyleAttr(c,n) then OldError(120);
    z:=GetOp(_const,SizeOf(float)); z^.R:=n; RdAttr:=z end
  else RdAttr:=RdRealFrml;
end;

procedure RdW(var W:WRectFrml);
begin
  W.C1:=RdRealFrml; Accept(','); W.R1:=RdRealFrml; Accept(',');
  W.C2:=RdRealFrml; Accept(','); W.R2:=RdRealFrml;
end;
procedure RdFrame(var Z:FrmlPtr;var WFlags:byte);
begin
  if Lexem<>',' then exit; RdLex;
  if Lexem='@' then begin WFlags:=WFlags or WNoClrScr; RdLex end;
  if Lexem='*' then begin WFlags:=WFlags or WPushPixel; RdLex end;
  if not (Lexem in [',',')','!']) then begin
    WFlags:=WFlags or WHasFrame; if Lexem=_equ then begin
      RdLex; WFlags:=WFlags or WDoubleFrame end;
    Z:=RdStrFrml end;
  if Lexem='!' then begin WFlags:=WFlags or WShadow; RdLex end;
end;

function PromptSortKeys(FL:FieldList;var SKRoot:KeyFldDPtr):boolean;
var SK:KeyFldDPtr;
label 1;
begin
  PromptSortKeys:=true; SKRoot:=nil; while FL<>nil do begin
    with FL^.FldD^ do if Typ<>'T' then PutSelect(Name); FL:=FL^.Chain end;
  if ss.empty then exit; ss.ascdesc:=true; ss.subset:=true;
  SelectStr(0,0,25,'');
  if KbdChar=_ESC_ then begin PromptSortKeys:=false; exit end;
1:LexWord:=GetSelect; if LexWord<>'' then begin
    SK:=GetZStore(sizeof(SK^)); ChainLast(SKRoot,SK);
    SK^.FldD:=FindFldName(CFile); if ss.Tag='>' then SK^.Descend:=true;
    if SK^.FldD^.Typ='A' then SK^.CompLex:=true; goto 1 end;
end;

procedure RdAssignFrml(FTyp:char;var Add:boolean;var Z:FrmlPtr);
var Typ:char;
begin
  if Lexem=_addass then begin RdLex; Add:=true end else Accept(_assign);
  Z:=RdFrml(Typ);
  if (FTyp<>Typ) or Add and (Typ<>'R') then OldError(12);
end;

function FldTypIdentity(F1,F2:FieldDPtr):boolean;
begin
 FldTypIdentity:=false; if F1^.Typ<>F2^.Typ then exit;
 if (F1^.Typ='F') and  (F1^.M<>F2^.M) then exit;
 if (F1^.Typ in ['N','A','F']) and (F1^.L<>F2^.L) then exit;
 FldTypIdentity:=true;
end;

procedure RdFldList(var FLRoot:FieldList);
  var F:FieldDPtr; FL:FieldList;
  label 1;
  begin 1:F:=RdFldName(CFile);FL:=GetStore(sizeof(FL^));FL^.FldD:=F;
        ChainLast(FLRoot,FL); if Lexem=',' then begin RdLex;goto 1 end end;
procedure RdNegFldList(var Neg:boolean;var FLRoot:FieldList);
begin
  if Lexem='^' then begin RdLex; Neg:=true end;
  Accept('('); if Lexem=')' then Neg:=true else RdFldList(FLRoot); Accept(')');
end;

procedure EditModeToFlags(const Mode:string;var Flgs;Err:boolean);
const FlgTxt:array[1..24] of string[2]=
   ('^Y','?Y','^N','F1','F2','F3','01',
    '!!','??','?E','?N','<=','R2','24','CO','LI',
    '->','^M','EX','WX','S7','#A','#L','SL');
   s:string[2]='xx';
var i,j:word; Flags:array[1..24] of boolean absolute Flgs;
label 1,2;
begin
  i:=1; while i<length(Mode) do begin
    s[1]:=upcase(Mode[i]); s[2]:=upcase(Mode[i+1]); inc(i,2);
    for j:=1 to 24 do if s=FlgTxt[j] then begin Flags[j]:=true; goto 1 end;
    goto 2;
1:  end;
  if i=length(Mode) then
2:  if Err then Error(92);
end;

function RdViewKey:KeyDPtr;
var k:KeyDPtr; lv:LocVarPtr; s:string; i:integer;
label 1;
begin
  RdViewKey:=nil; if Lexem<>'/' then exit; RdLex;
  k:=CFile^.Keys; if Lexem='@' then goto 1; TestIdentif;
  while k<>nil do begin
    if EquUpcase(k^.Alias^) then goto 1; k:=k^.Chain end;
  s:=LexWord; i:=pos('_',s); if i<>0 then s:=copy(s,i+1,255);
  s:=CFile^.Name+'_'+s; k:=CFile^.Keys; while k<>nil do begin
    if SEquUpcase(s,k^.Alias^) then goto 1; k:=k^.Chain end;
  if IdxLocVarAllowed and FindLocVar(LVBD.Root,lv) and (lv^.FTyp='i') then
    begin if lv^.FD<>CFile then Error(164); k:=KeyDPtr(lv^.RecPtr); goto 1 end;
  Error(109);
1:if CFile^.Typ<>'X' then {$ifdef FandSQL}
    if CFile^.typSQLFile then Error(24) else {$endif} Error(108);
  RdLex; RdViewKey:=k;
end;

var KeyArgFound:boolean; KeyArgFld:FieldDPtr;
procedure SrchZ(Z:FrmlPtr); forward;
procedure SrchF(F:FieldDPtr);
begin
  if F=KeyArgFld then begin KeyArgFound:=true; exit end;
  if F^.Flg and f_Stored=0 then SrchZ(F^.Frml);
end;
procedure SrchZ(Z:FrmlPtr);
var KF:KeyFldDPtr; fl:FrmlList;
begin
  if Z=nil then exit; case Z^.Op of
   _field:SrchF(Z^.Field);
   _access:if Z^.LD<>nil then begin
             KF:=Z^.LD^.Args; while KF<>nil do begin
               SrchF(KF^.FldD); KF:=KF^.Chain end end;
   _userfunc:begin fl:=Z^.FrmlL; while fl<>nil do begin
              SrchZ(fl^.Frml); fl:=fl^.Chain end end;
   #$60..#$af{1-ary}:SrchZ(Z^.P1);
   #$b0..#$ef{2-ary}:begin SrchZ(Z^.P1); SrchZ(Z^.P2) end;
   #$f0..#$ff{3-ary}:begin SrchZ(Z^.P1); SrchZ(Z^.P2); SrchZ(Z^.P3) end end;
end;
function IsKeyArg(F:FieldDPtr; FD:FileDPtr): boolean;
var k:KeyDPtr; kf:KeyFldDPtr;
begin
  KeyArgFound:=false; k:=FD^.Keys; while k<>nil do begin
    KeyArgFld:=F; kf:=k^.KFlds; while kf<>nil do begin
      SrchF(kf^.FldD); if KeyArgFound then begin IsKeyArg:=true; exit end;
      kf:=kf^.Chain end;
    k:=k^.Chain end;
  IsKeyArg:=false;
end;

function RdKF(FD:FileDPtr):KeyFldDPtr;
var KF:KeyFldDPtr; F:FieldDPtr;
begin
  KF:=GetZStore(sizeof(KeyFldD)); RdKF:=KF;
  if Lexem=_gt then begin RdLex; KF^.Descend:=true end;
  if Lexem='~' then begin RdLex; KF^.CompLex:=true end;
  F:=RdFldName(FD); KF^.FldD:=F; if F^.Typ='T' then OldError(84);
  if KF^.CompLex and (F^.Typ<>'A') then OldError(94);
end;
function RdKFList(var KFRoot:KeyFldDPtr;FD:FileDPtr):word;
var n:word; KF:KeyFldDPtr;
label 1;
begin
1:ChainLast(KFRoot,RdKF(FD)); if Lexem=',' then begin RdLex; goto 1 end;
  n:=0; KF:=KFRoot;   {looping over all fields, not only the last read}
  while KF<>nil do begin inc(n,KF^.FldD^.NBytes); KF:=KF^.Chain end;
  if n>255 then OldError(126); RdKFList:=n;
end;


procedure CompileRecLen;
var F:FieldDPtr; l,n:word;
begin with CFile^ do begin
  F:=FldD; l:=0; n:=0; if (Typ in ['X','D']) then l:=1;
  while F<>nil do begin
    case Typ of
     '8': if F^.Typ='D' then F^.NBytes:=2;
     'D': case F^.Typ of
          'F':F^.NBytes:=F^.L-1; 'D':F^.NBytes:=8; 'T':F^.NBytes:=10 end;
    end;
    if F^.Flg and f_Stored<>0 then begin F^.Displ:=l; inc(l,F^.NBytes); inc(n) end;
    F:=F^.Chain end;
  RecLen:=l;
  case Typ of
   '8':FrstDispl:=4; 'D':FrstDispl:=(n+1)*32+1; else FrstDispl:=6 end;
end end;

