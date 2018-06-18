function GetOp(Op:char;BytesAfter:integer):FrmlPtr;
var Z:FrmlPtr; l:word;
begin
  if Op<#$60 then l:=1 else if Op<#$b0 then l:=5 else if Op<#$f0 then l:=9 else l:=13;
  Z:=GetZStore(l+BytesAfter); Z^.Op:=Op; GetOp:=Z;
end;
function FindFldName(FD:FileDPtr):FieldDPtr;
  var F:FieldDPtr;
  label 1;
  begin F:=FD^.FldD;
      while F<>nil do
      begin if EquUpcase(F^.Name) then goto 1;
            F:=F^.Chain;
      end;
    1:FindFldName:=F;
  end;
function RdFldName(FD:FileDPtr):FieldDPtr;
  var F:FieldDPtr;
  begin TestIdentif; F:=FindFldName(FD);
        if F=nil then begin Set2MsgPar(LexWord,FD^.Name); Error(87) end;
        RdLex; RdFldName:=F end;
function FindFileD:FileDPtr;
var FD:FileDPtr; R:RdbDPtr; LV:LocVarPtr;
begin
  if FDLocVarAllowed and FindLocVar(LVBD.Root,LV) and (LV^.FTyp='f') then
    begin FindFileD:=LV^.FD; exit end;
  R:=CRdb; while R<>nil do begin
    FD:=R^.FD; while FD<>nil do begin
      if EquUpcase(FD^.Name) then begin FindFileD:=FD;exit end;
      FD:=FD^.Chain end;
    R:=R^.ChainBack end;
  if EquUpcase('CATALOG') then FindFileD:=CatFD else FindFileD:=nil;
end;
function RdFileName:FileDPtr;
  var FD:FileDPtr;
  begin if SpecFDNameAllowed and (Lexem='@') then
           begin LexWord:='@';Lexem:=_identifier end;
        TestIdentif; FD:=FindFileD;
        if (FD=nil) or (FD=CRdb^.FD) and not SpecFDNameAllowed then Error(9);
        RdLex;RdFileName:=FD end;
function FindLD(const RoleName:string):LinkDPtr;
var L:LinkDPtr;
begin
  FindLD:=nil; L:=LinkDRoot; while L<>nil do begin
    if (L^.FromFD=CFile) and SEquUpcase(L^.RoleName,RoleName) then begin
      FindLD:=L; exit end;
    L:=L^.Chain end;
end;
function IsRoleName(Both:boolean;var FD:FileDPtr;var LD:LinkDPtr):boolean;
  begin TestIdentif;
     FD:=FindFileD;IsRoleName:=true;
     if (FD<>nil) and FD^.IsParFile then begin RdLex;LD:=nil;exit end;
     if Both then
        begin LD:=FindLD(LexWord);
              if LD<>nil then begin RdLex;FD:=LD^.ToFD;exit end end;
     IsRoleName:=False;
  end;
function RdFAccess(FD:FileDPtr;LD:LinkDPtr;var FTyp:char):FrmlPtr;
var Z:FrmlPtr; cf:FileDPtr; fa:boolean;
begin
  TestIdentif;Z:=GetOp(_access,12);Z^.File2:=FD;Z^.LD:=LD;
  if (LD<>nil) and EquUpcase('EXIST') then begin RdLex;FTyp:='B' end
  else begin
    cf:=CFile; CFile:=FD; fa:=FileVarsAllowed; FileVarsAllowed:=true;
    Z^.P1:=RdFldNameFrmlF(FTyp); CFile:=cf; FileVarsAllowed:=fa end;
  RdFAccess:=Z;
end;
function FrmlContxt(Z:FrmlPtr;FD:FileDPtr;RP:pointer):FrmlPtr;
  var Z1:FrmlPtr;
  begin Z1:=GetOp(_newfile,8); Z1^.Frml:=Z;
        Z1^.NewFile:=FD; Z1^.NewRP:=RP; FrmlContxt:=Z1 end;
function MakeFldFrml(F:FieldDPtr;var FTyp:char):FrmlPtr;
  var Z:FrmlPtr;
  begin Z:=GetOp(_field,4);Z^.Field:=F;FTyp:=F^.FrmlTyp;MakeFldFrml:=Z end;

function TryRdFldFrml(FD:FileDPtr;var FTyp:char):FrmlPtr;
function FindOwnLD(FD:FileDPtr;const RoleName:string):LinkDPtr;
var ld:LinkDPtr;
label 1;
begin
  FindOwnLD:=nil; ld:=LinkDRoot; while ld<>nil do begin
    if (ld^.ToFD=FD) and EquUpcase(ld^.FromFD^.Name) and
       (ld^.IndexRoot<>0) and SEquUpcase(ld^.RoleName,RoleName) then goto 1;
    ld:=ld^.Chain end;
1:RdLex; FindOwnLD:=ld;
end;
var cf:FileDPtr; f:FieldDPtr; ld:LinkDPtr; z:FrmlPtr; roleNm:string[32];
    rff:function(var FTyp:char):FrmlPtr; typ:char;
label 1;
begin
  if IsKeyWord('OWNED') then begin
    rff:=RdFldNameFrml; RdFldNameFrml:=RdFldNameFrmlF;
    Accept('('); z:=GetOp(_owned,12);
    TestIdentif; SkipBlank(false);
    if ForwChar='(' then begin
      roleNm:=LexWord; RdLex; RdLex; ld:=FindOwnLD(FD,roleNm); Accept(')') end
    else ld:=FindOwnLD(FD,FD^.Name);
    if ld=nil then OldError(182); z^.ownLD:=ld; cf:=CFile; CFile:=ld^.FromFD;
    if Lexem='.' then begin
      RdLex; z^.ownSum:=RdFldNameFrmlF(FTyp); if FTyp<>'R' then OldError(20) end;
    if Lexem=':' then begin  RdLex;
      z^.ownBool:=RdFormula(typ); TestBool(typ) end;
    Accept(')'); CFile:=cf; FTyp:='R';
    RdFldNameFrml:=rff end
  else begin
    f:=FindFldName(FD);
    if f=nil then z:=nil else begin RdLex; z:=MakeFldFrml(f,FTyp) end end;
  TryRdFldFrml:=z;
end;

function RdFldNameFrmlF(var FTyp:char):FrmlPtr;      {called implicitly}
var ld:LinkDPtr; fd:FileDPtr; z:FrmlPtr;
begin
  if IsForwPoint then begin
    if not IsRoleName(FileVarsAllowed,fd,ld) then Error(9);
    RdLex; RdFldNameFrmlF:=RdFAccess(fd,ld,FTyp); exit end;
  if not FileVarsAllowed then Error(110);
  z:=TryRdFldFrml(CFile,FTyp); if z=nil then Error(8); RdFldNameFrmlF:=z;
end;