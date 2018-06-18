{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L-}

unit rdmerg;

interface

procedure ReadMerge;

implementation

uses base,access,rdrun,compile,rdfildcl;

var WhatToRd:char; {i=Oi output FDs;O=O outp.FDs}
    ReadingOutpBool:boolean;
    Ii,Oi,SumIi:word;
    RD:OutpRDPtr;

function InpFD(I:word):FileDPtr;
begin
  InpFD:=IDA[I]^.Scan^.FD;
end;

function RdIiPrefix:boolean;
  begin if (ForwChar='.') and (Length(LexWord)=2) and (LexWord[1]='I') and
           (LexWord[2] in ['1'..'9']) then
           begin Ii:=ord(LexWord[2])-ord('0');
                 if (Ii>MaxIi) or (WhatToRd='i') and (Ii>Oi) then Error(9);
                 RdLex;RdLex;RdIiPrefix:=True end
        else begin Ii:=0;RdIiPrefix:=False end;
  end;
function FindIiandFldFrml(var FD:FileDPtr;var FTyp:char):FrmlPtr;
var i:integer; z:FrmlPtr;
label 1;
begin
  if not Join and (WhatToRd='i') then begin   { for Oi search first in Ii}
    FD:=InpFD(Oi); z:=TryRdFldFrml(FD,FTyp);
    if z<>nil then begin Ii:=Oi; goto 1 end end;
  for i:=1 to MaxIi do begin     { search in  I1 .. In, for Oi only I1 .. Ii}
    FD:=InpFD(i); z:=TryRdFldFrml(FD,FTyp);
    if z<>nil then begin Ii:=i; goto 1 end;
    if (WhatToRd='i') and (i=Oi) then goto 1 end;
1:FindIiandFldFrml:=z;
end;

function RdFldNameFrmlM(var FTyp:char):FrmlPtr; far;
  var WasIiPrefix:boolean;
procedure RdDirFilVar;
  var LD:LinkDPtr; FD:FileDPtr;
      I:integer;
      Z:FrmlPtr;
  label 1,2;
  begin
      if WasIiPrefix then
         begin CFile:=InpFD(Ii);
               if not IsRoleName(true,FD,LD) then Error(9) end
      else begin
            if not Join and (WhatToRd='i') then
               begin Ii:=Oi;CFile:=InpFD(Ii);
                     if IsRoleName(true,FD,LD) then goto 2 end;
            for I:=1 to MaxIi do
               begin CFile:=InpFD(I);
                     if IsRoleName(true,FD,LD) then begin Ii:=I;goto 2 end;
                     if (WhatToRd='i') and (I=Oi) then goto 1;
               end;
         1:Error(9);
           end;
    2:Accept('.');Z:=RdFAccess(FD,LD,FTyp);
      if LD=nil then Ii:=0 else Z:=FrmlContxt(Z,CFile,CFile^.RecPtr);
      RdFldNameFrmlM:= Z;
  end;
procedure TestSetSumIi;
  begin if (FrmlSumEl<>nil) and (Ii<>0) then
        if FrstSumVar or (SumIi=0) then SumIi:=Ii
        else if SumIi<>Ii then OldError(27);
  end;
procedure RdOutpFldName;
begin
  if RD^.OD=nil {dummy} then Error(85); with RD^.OD^ do
  RdFldNameFrmlM:=FrmlContxt(MakeFldFrml(RdFldName(FD),FTyp),FD,RecPtr);
end;
procedure SetIi;
  begin if not WasIiPrefix then if not Join and (WhatToRd='i') then Ii:=Oi
        else Ii:=1 end;
procedure TestNotSum;
  begin if FrmlSumEl<>nil then OldError(41) end;
procedure Err;
  begin TestNotSum;SetIi; if IDA[Ii]^.ErrTxtFrml=nil then
      begin IDA[Ii]^.ErrTxtFrml:=GetOp(_const,256) end end;
var F:FieldDPtr;                         {RdFldNameFrml - body}
    Z:FrmlPtr;
    LV:LocVarPtr; FD:FileDPtr;
label 1,2;
begin
  WasIiPrefix:=RdIiPrefix;
  if (FrmlSumEl<>nil) and FrstSumVar then SumIi:=0;
  TestIdentif;
  if (LexWord='O') and IsForwPoint and not WasIiPrefix then begin
    RdLex; RdLex; if (FrmlSumEl<>nil) or ReadingOutpBool then Error(99);
    RdOutpFldName; exit end;
  if IsForwPoint then begin RdDirFilVar;TestSetSumIi;exit end;
  if not WasIiPrefix then if FindLocVar(LVBD.Root,LV) then begin
    RdLex; TestNotSum; RdFldNameFrmlM:=FrmlPtr(@LV^.Op); FTyp:=LV^.FTyp; exit end;
  if IsKeyWord('COUNT') then begin
1:  TestNotSum; SetIi; RdFldNameFrmlM:=FrmlPtr(@IDA[Ii]^.Op); FTyp:='R'; exit end;
  if IsKeyWord('GROUP') then begin
2:  TestNotSum; if WasIiPrefix then OldError(41);
    RdFldNameFrmlM:=FrmlPtr(@MergOpGroup); FTyp:='R'; exit end;
  if IsKeyWord('ERROR') then begin
    Err; RdFldNameFrmlM:=FrmlPtr(@IDA[Ii]^.OpErr); FTyp:='B'; exit end;
  if IsKeyWord('WARNING') then begin
    Err; RdFldNameFrmlM:=FrmlPtr(@IDA[Ii]^.OpWarn); FTyp:='B'; exit end;
  if IsKeyWord('ERRORTEXT') then begin
    Err; RdFldNameFrmlM:=IDA[Ii]^.ErrTxtFrml; FTyp:='S'; exit end;
  if WasIiPrefix then begin FD:=InpFD(Ii); z:=TryRdFldFrml(FD,FTyp) end
  else z:=FindIiandFldFrml(FD,FTyp);
  if z=nil then begin
    if IsKeyWord('N') then goto 1; if IsKeyWord('M') then goto 2; Error(8) end;
  TestSetSumIi;
  RdFldNameFrmlM:=FrmlContxt(Z,FD,FD^.RecPtr);
end;

procedure ChainSumElM; far;
  begin if FrstSumVar or (SumIi=0) then SumIi:=1;
        FrmlSumEl^.Chain:=IDA[SumIi]^.Sum;IDA[SumIi]^.Sum:=FrmlSumEl;
  end;


procedure ReadMerge;

procedure CopyPrevMFlds;
  var M,MNew:KeyFldDPtr;
      F:FieldDPtr;
      S:string;
  begin M:=IDA[Ii-1]^.MFld;S:=LexWord;
      while M<>nil do
      begin LexWord:=M^.FldD^.Name; F:=FindFldName(InpFD(Ii));
            if F=nil then OldError(8);
            if not FldTypIdentity(M^.FldD,F) then OldError(12);
            MNew:=GetStore(sizeof(MNew^)); Move(M^,MNew^,sizeof(MNew^));
            MNew^.FldD:=F; ChainLast(IDA[Ii]^.MFld,MNew); M:=M^.Chain end;
      LexWord:=S;
  end;
procedure CheckMFlds(M1,M2:KeyFldDPtr);
  begin while M1<>nil do
      begin if M2=nil then OldError(30);
        if not FldTypIdentity(M1^.FldD,M2^.FldD) or (M1^.Descend<>M2^.Descend)
           or (M1^.CompLex<>M2^.CompLex) then
           OldError(12);
        M1:=M1^.Chain;M2:=M2^.Chain end;
      if M2<>nil then OldError(30);
  end;
procedure MakeOldMFlds;
var M:KeyFldDPtr; n:word;
begin
  M:=IDA[1]^.MFld; OldMFlds:=nil; while M<>nil do begin
    case M^.FldD^.FrmlTyp of 'B':n:=1; 'R':n:=sizeof(float) else n:=256 end;
    ChainLast(OldMFlds,GetStore(sizeof(pointer)+n)); M:=M^.Chain end;
end;
procedure RdAutoSortSK(ID:InpDPtr);
var M,SK:KeyFldDPtr;
begin
  if not ID^.AutoSort then exit; M:=ID^.MFld; while M<>nil do begin
    SK:=GetStore(sizeof(SK^)); Move(M^,SK^,sizeof(SK^));
    ChainLast(ID^.SK,SK); M:=M^.Chain end;
  if Lexem=';' then begin RdLex; RdKFList(ID^.SK,CFile) end;
  if ID^.SK=nil then OldError(60);
end;

procedure ImplAssign(RD:OutpRDPtr;FNew:FieldDPtr);
function AdjustComma(Z1:FrmlPtr;F:FieldDPtr;Op:char):FrmlPtr;
var Z,Z2:FrmlPtr;
begin
  AdjustComma:=Z1; if F^.Typ<>'F' then exit; if F^.Flg and f_Comma=0 then exit;
  Z2:=GetOp(_const,sizeof(float)); Z2^.R:=Power10[F^.M];
  Z:=GetOp(Op,0); Z^.P1:=Z1; Z^.P2:=Z2; AdjustComma:=Z;
end;
procedure FindIiandFldD(var F:FieldDPtr);
var i:integer;
begin
  if not Join and (WhatToRd='i') then begin   { for Oi search first in Ii}
    F:=FindFldName(InpFD(Oi)); if F<>nil then begin Ii:=Oi; exit end end;
  for i:=1 to MaxIi do begin    { search in  I1 .. In, for Oi only I1 .. Ii}
    F:=FindFldName(InpFD(i)); if F<>nil then begin Ii:=i; exit end;
    if (WhatToRd='i') and (i=Oi) then exit end;
end;
var FDNew,FD:FileDPtr; F:FieldDPtr; A,A1:AssignDPtr;
    RPNew,RP:pointer; Z:FrmlPtr; FTyp:char; S:string;
label 1;
begin FDNew:=RD^.OD^.FD;RPNew:=RD^.OD^.RecPtr;S:=LexWord;
  A:=GetZStore(SizeOf(A^)); A1:=RD^.Ass;
  LexWord:=FNew^.Name; FindIiandFldD(F);
  if (F=nil) or (F^.FrmlTyp<>FNew^.FrmlTyp) or
     (F^.FrmlTyp='R') and (F^.Typ<>FNew^.Typ) then begin
    A^.Kind:=_zero;A^.FldD:=FNew end
  else begin
    FD:=InpFD(Ii); RP:=FD^.RecPtr;
    if (FD^.Typ=FDNew^.Typ) and FldTypIdentity(F,FNew) and
       (F^.Typ<>'T') and (F^.Flg and f_Stored<>0) and (FNew^.Flg=F^.Flg) then begin
      A^.Kind:=_move; A^.L:=FNew^.NBytes;
      A^.ToPtr:=Ptr(seg(RPNew^),ofs(RPNew^)+FNew^.Displ);
      A^.FromPtr:=Ptr(seg(RP^),ofs(RP^)+F^.Displ);
      if (A1<>nil) and (A1^.Kind=_move) and
         (seg(A1^.FromPtr^)=seg(A^.FromPtr^)) and
         (ofs(A1^.FromPtr^)+A1^.L=ofs(A^.FromPtr^)) and
         (seg(A1^.ToPtr^)=seg(A1^.ToPtr^)) and
         (ofs(A1^.ToPtr^)+A1^.L=ofs(A^.ToPtr^)) then begin
        A1^.L:=A1^.L+A^.L;
        ReleaseStore(A); goto 1 end
      end
    else begin
      A^.Kind:=_output; A^.OFldD:=FNew; Z:=MakeFldFrml(F,FTyp);
      Z:=AdjustComma(Z,F,_divide); Z:=AdjustComma(Z,FNew,_times);
      A^.Frml:=FrmlContxt(Z,FD,FD^.RecPtr) end;
    end;
  A^.Chain:=A1; RD^.Ass:=A;
1:LexWord:=S;
end;
function FindAssignToF(A:AssignDPtr;F:FieldDPtr):boolean;
begin
  FindAssignToF:=true; while A<>nil do begin
    if (A^.Kind=_output) and (A^.OFldD=F) and not A^.Add then exit;
    A:=A^.Chain end;
  FindAssignToF:=false;
end;
procedure MakeImplAssign;
var FNew:FieldDPtr; AD:AssignDPtr;
begin
  if RD^.OD=nil then exit; FNew:=RD^.OD^.FD^.FldD;
  while FNew<>nil do begin                 {implic.assign   name:=name}
  if (FNew^.Flg and f_Stored<>0) and not FindAssignToF(RD^.Ass,FNew) then ImplAssign(RD,FNew);
  FNew:=FNew^.Chain;
end end;
function RdAssSequ:AssignDPtr;forward;
procedure TestIsOutpFile(FD:FileDPtr);
var OFD:OutpFDPtr;
begin
  OFD:=OutpFDRoot; while OFD<>nil do begin
    if OFD^.FD=FD then OldError(173); OFD:=OFD^.Chain end;
end;
function RdAssign:AssignDPtr;
var F:FieldDPtr; FD:FileDPtr; LV:LocVarPtr; AD:AssignDPtr;
begin
  if IsKeyWord('BEGIN') then begin
    RdAssign:=RdAssSequ; AcceptKeyWord('END'); exit end;
  AD:=GetZStore(SizeOf(AD^)); RdAssign:=AD; TestIdentif;
  if IsKeyWord('IF') then begin
    AD^.Kind:=_ifthenelseM; AD^.Bool:=RdBool;
    AcceptKeyWord('THEN'); AD^.Instr:=RdAssign;
    if IsKeyWord('ELSE') then AD^.ElseInstr:=RdAssign end
  else if ForwChar='.' then begin
    AD^.Kind:=_parfile; FD:=RdFileName; if not FD^.IsParFile then OldError(9);
    TestIsOutpFile(FD);
    Accept('.'); AD^.FD:=FD; F:=RdFldName(FD); AD^.PFldD:=F;
    if F^.Flg and f_Stored=0 then OldError(14);
    RdAssignFrml(F^.FrmlTyp,AD^.Add,AD^.Frml) end
  else if FindLocVar(LVBD.Root,LV) then begin
    RdLex; AD^.Kind:=_locvar; AD^.LV:=LV;
    RdAssignFrml(LV^.FTyp,AD^.Add,AD^.Frml) end
  else begin
    if RD^.OD=nil then Error(72);  {dummy}
    AD^.Kind:=_output; F:=RdFldName(RD^.OD^.FD); AD^.OFldD:=F;
    if F^.Flg and f_Stored=0 then OldError(14);
    RdAssignFrml(F^.FrmlTyp,AD^.Add,AD^.Frml) end;
end;
function RdAssSequ:AssignDPtr;
  var A,ARoot:AssignDPtr;
  label 1;
  begin ARoot:=nil;
      1:A:=AssignDPtr(@ARoot);while A^.Chain<>nil do A:=A^.Chain;
        A^.Chain:=RdAssign;
        if Lexem=';' then
           begin RdLex;
           if not(Lexem in [^z,'#']) and not TestKeyWord('END') then goto 1 end;
        RdAssSequ:=ARoot;
  end;
procedure RdOutpRD(var RDRoot:OutpRDPtr);
  var R:OutpRDPtr; FD:FileDPtr; OD:OutpFDPtr; ID:InpDPtr;
      I:integer;
  label 1;
  begin
     RD:=GetStore(SizeOf(RD^));ChainLast(RDRoot,RD);RD^.Ass:=nil;RD^.Bool:=nil;
     if IsKeyWord('DUMMY') then RD^.OD:=nil
     else begin FD:=RdFileName;
        OD:=OutpFDRoot; while OD<>nil do
        begin if OD^.FD=FD then
              begin if Lexem='+' then if OD^.Append then RdLex else Error(31)
                    else if OD^.Append then Error(31);
                    goto 1 end;
              OD:=OD^.Chain end;
        OD:=GetStore(SizeOf(OD^));OD^.FD:=FD;CFile:=FD;OD^.RecPtr:=GetRecSpace;
        OD^.InplFD:=nil;
        for I:=1 to MaxIi do if InpFD(I)=FD then begin
           OD^.InplFD:=FD; IDA[I]^.IsInplace:=true; if FD^.typSQLFile then Error(172) end;
        if Lexem='+' then
           begin OD^.Append:=true;if OD^.InplFD<>nil then Error(32);RdLex end
        else OD^.Append:=false;
        ChainLast(OutpFDRoot,OD);
        1:RD^.OD:=OD;
      end;
     if Lexem='(' then
        begin RdLex;ReadingOutpBool:=true;RD^.Bool:=RdBool;
              ReadingOutpBool:=false;Accept(')') end;
     if not(Lexem in ['#',^z]) then RD^.Ass:=RdAssSequ;
     MakeImplAssign;
  end;

                                                    { RdMerg - body }
var ID:InpDPtr; F:FieldDPtr; FD:FileDPtr;  KI:KeyInDPtr;
    I:word; WasOi,WasSqlFile,CompLex:boolean;
label 1,3,4;
begin
  ResetCompilePars; RdLex;
  ResetLVBD; if IsKeyWord('VAR') then RdLocDcl(LVBD,false,false,'M');
  WhatToRd:='I'; ReadingOutpBool:=false; WasSqlFile:=false;
  Ii:=0; TestLex('#');
  repeat
    ReadChar; if CurrChar='I' then begin ReadChar;
      if IsDigit(CurrChar) then begin
        I:=ord(CurrChar)-ord('0'); ReadChar; if CurrChar='_' then begin
          RdLex; goto 1 end end end;
    Error(89);
1:  inc(Ii);if I<>Ii then OldError(61);
    ID:=GetZStore(SizeOf(ID^)); IDA[Ii]:=ID;
    FD:=RdFileName; CFile:=FD;
{$ifdef FandSQL} if CFile^.typSQLFile then WasSqlFile:=true; {$endif}
    for I:=1 to Ii-1 do if InpFD(I)=FD then OldError(26);
    CViewKey:=RdViewKey;
    if Lexem='!' then begin RdLex; ID^.AutoSort:=true end;
    ID^.Op:=_const; ID^.OpErr:=_const; ID^.OpWarn:=_const; KI:=nil;
    ID^.ForwRecPtr:=GetRecSpace; FD^.RecPtr:=GetRecSpace;
    if Lexem='(' then begin
      RdLex; ID^.Bool:=RdKeyInBool(KI,false,false,ID^.SQLFilter); Accept(')') end;
    New(ID^.Scan,Init(FD,CViewKey,KI,true));
    if not(Lexem in [';','#',^z]) then RdKFList(ID^.MFld,FD);
    if Ii>1 then
      if IDA[Ii-1]^.MFld=nil then begin
        if ID^.MFld<>nil then OldError(22) end
      else if ID^.MFld=nil then CopyPrevMFlds
      else CheckMFlds(IDA[Ii-1]^.MFld,ID^.MFld);
    RdAutoSortSK(ID); TestLex('#');
  until ForwChar<>'I';

  MaxIi:=Ii; MakeOldMFlds; OldMXStr.Clear;
  OutpFDRoot:=nil; OutpRDs:=nil; Join:=false; WasOi:=false;
  RdFldNameFrml:=RdFldNameFrmlM;

3:ReadChar;
  if CurrChar='O' then begin ReadChar;
    if IsDigit(CurrChar) then begin
      if Join then Error(91); WasOi:=true; Oi:=ord(CurrChar)-ord('0');
      if (Oi=0) or (Oi>MaxIi) then Error(62); goto 4 end
    else if CurrChar='*' then begin
      if WasOi then Error(91); if WasSQLFile then Error(155);
      Join:=true; Oi:=MaxIi;
4:    ReadChar; if CurrChar<>'_' then Error(90); RdLex;
      WhatToRd:='i'; @ChainSumEl:=nil; RdOutpRD(IDA[Oi]^.RD) end
    else if CurrChar='_' then begin
      RdLex; WhatToRd:='O'; ChainSumEl:=ChainSumElM;
      RdOutpRD(OutpRDs) end
    else Error(90) end
  else Error(90);
  if Lexem<>^z then begin TestLex('#'); goto 3 end;

  for i:=1 to MaxIi do begin
    ID:=IDA[i];
    if ID^.ErrTxtFrml<>nil then RdChkDsFromPos(ID^.Scan^.FD,ID^.Chk) end;
 {writeln('maxii=',maxii,'  outprds=',hexa(outprds));
  writeln('oldmflds=',hexa(oldmflds));writeln('ofdroot=',hexa(OutpFDRoot));
  writeln('ida[1]=',hexa(ida[1]),'  ida[2]=',hexa(ida[2]));
  5:goto 5;}
end;

end.