{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit rdrprt;

interface

uses rdrun;

procedure ReadReport(RO:RprtOptPtr);

implementation

uses dos,base,obase,access,compile,rdfildcl,runfrml;

var CBlk:BlkDPtr;
    CZeroLst:FloatPtrList;
    LvToRd:LvDescrPtr;           {all used while translating frml}
var Ii,Oi,SumIi:word;
    WhatToRd:char; {'i'=#XXi 'O'=#XX }
var WasIiPrefix:boolean;
    CBlkSave:BlkDPtr;


function InpFD(I:word):FileDPtr;
begin
  InpFD:=IDA[I]^.Scan^.FD;
end;

function FindInLvBlk(L:LvDescrPtr;var B:BlkDPtr; var RF:RFldDPtr):boolean;
var RF1:RFldDPtr; B1:BlkDPtr; L1:LvDescrPtr; first:boolean;
label 1;
begin
  first:=true; FindInLvBlk:=false;
1:while L<>nil do begin
    B1:=L^.Ft; while B1<>nil do begin
      RF1:=B1^.RFD; while RF1<>nil do begin
        if (Lexem=_identifier) and EquUpcase(RF1^.Name) then begin
          RdLex; FindInLvBlk:=true; RF:=RF1; B:=B1; exit end;
        RF1:=RF1^.Chain end;
      B1:=B1^.Chain end;
    L:=L^.ChainBack end;
  if first then begin first:=false; L:=IDA[1]^.FrstLvS;{DE} goto 1 end;
end;
function RdFldNameFrmlR(var FTyp:char):FrmlPtr; far;
function RdIiPrefix:boolean;
begin
  if (ForwChar='.') and (Length(LexWord)=2) and (LexWord[1]='I') and
     (LexWord[2] in ['1'..'9']) then begin
    Ii:=ord(LexWord[2])-ord('0');
    if (Ii>MaxIi) or (WhatToRd='i') and (Ii>Oi) then Error(9);
    RdLex; RdLex; RdIiPrefix:=true end
  else begin Ii:=0; RdIiPrefix:=false end;
end;
procedure TestSetSumIi;
begin
  if (FrmlSumEl<>nil) and (Ii<>0) then
    if FrstSumVar or (SumIi=0) then SumIi:=Ii
    else if SumIi<>Ii then OldError(27);
end;
function FindIiandFldFrml(var FD:FileDPtr;var FTyp:char):FrmlPtr;
var i:integer; z:FrmlPtr;
label 1;
begin
  if WhatToRd='i' then begin       { search first in Ii}
    FD:=InpFD(Oi); z:=TryRdFldFrml(FD,FTyp);
    if z<>nil then begin Ii:=Oi; goto 1 end end;
  for i:=1 to MaxIi do begin       { search in  I1 .. Imax resp. Oi}
    FD:=InpFD(i); z:=TryRdFldFrml(FD,FTyp);
    if z<>nil then begin Ii:=i; goto 1 end;
    if (WhatToRd='i') and (i=Oi) then goto 1 end;
1:FindIiandFldFrml:=z;
end;
procedure RdDirFilVar;
var LD:LinkDPtr; FD:FileDPtr; I:integer; Z:FrmlPtr;
label 1,2;
begin
  if WasIiPrefix then begin
    CFile:=InpFD(Ii); if not IsRoleName(true,FD,LD) then Error(9) end
  else begin
    if not Join and (WhatToRd='i') then begin
      Ii:=Oi; CFile:=InpFD(Ii); if IsRoleName(true,FD,LD) then goto 2 end;
    for I:=1 to MaxIi do begin
      CFile:=InpFD(I); if IsRoleName(true,FD,LD) then begin Ii:=I;goto 2 end;
      if (WhatToRd='i') and (I=Oi) then goto 1 end;
1:  Error(9) end;
2:RdLex{'.'};Z:=RdFAccess(FD,LD,FTyp);
  if LD=nil then Ii:=0
  else begin
    Z:=FrmlContxt(Z,CFile,CFile^.RecPtr); TestSetSumIi;
    if (FrmlSumEl<>nil) and not FrstSumVar and (CBlk<>nil) then OldError(59) end;
  RdFldNameFrmlR:= Z;
end;
function OwnInBlock:boolean;
  var RF:RFldDPtr;
  begin OwnInBlock:=False;
    RF:=CBlk^.RFD;
    while RF<>nil do
    begin if EquUpcase(RF^.Name) then
            begin RdLex;FTyp:=RF^.FrmlTyp;RdFldNameFrmlR:=RF^.Frml;
                  OwnInBlock:=True;exit end;
          RF:=RF^.Chain;
    end;
  end;
procedure FindInRec;
var FD:FileDPtr; F:FieldDPtr; Z:FrmlPtr;
begin
  if WasIiPrefix then begin
    FD:=InpFD(Ii); z:=TryRdFldFrml(FD,FTyp) end
  else z:=FindIiandFldFrml(FD,FTyp);
  if z=nil then Error(8); TestSetSumIi;
  RdFldNameFrmlR:=FrmlContxt(Z,FD,FD^.RecPtr);
end;
procedure SetIi;
  begin if not WasIiPrefix then if WhatToRd='i' then Ii:=Oi else Ii:=1 end;
procedure TestNotSum;
  begin if FrmlSumEl<>nil then OldError(41) end;
procedure Err;
  begin TestNotSum; SetIi; if IDA[Ii]^.ErrTxtFrml=nil then
      begin IDA[Ii]^.ErrTxtFrml:=GetOp(_const,256) end end;
var LD:LinkDPtr; FD:FileDPtr; RF:RFldDPtr; LV:LocVarPtr; n:word;
    Z:FrmlPtr;
label 1;
begin
  WasIiPrefix:=RdIiPrefix;
  if (FrmlSumEl<>nil) and FrstSumVar and (CBlk<>nil) then begin
    SumIi:=0; CBlkSave:=CBlk; CBlk:=nil; end;
  if IsForwPoint then begin RdDirFilVar; exit end;
  if not WasIiPrefix and FindLocVar(LVBD.Root,LV) then begin
      RdLex; TestNotSum;
      RdFldNameFrmlR:=FrmlPtr(@LV^.Op); FTyp:=LV^.FTyp; exit end;
  if IsKeyWord('COUNT') then begin
    TestNotSum; SetIi; RdFldNameFrmlR:=FrmlPtr(@IDA[Ii]^.Op); FTyp:='R'; exit end;
  if IsKeyWord('GROUP') then begin
    TestNotSum; if WasIiPrefix then OldError(41);
    RdFldNameFrmlR:=FrmlPtr(@MergOpGroup); FTyp:='R'; exit end;
  if IsKeyWord('LINE') then begin n:=0; goto 1 end;
  if IsKeyWord('PAGE') then begin n:=1; goto 1 end;
  if IsKeyWord('PAGELIMIT') then begin n:=2;
1:  if (FrmlSumEl=nil) and not WasIiPrefix then begin
      Z:=GetOp(_getwordvar,1); Z^.N01:=n;
      RdFldNameFrmlR:=Z; FTyp:='R'; exit end end;
  if IsKeyWord('ERROR') then begin
    Err; RdFldNameFrmlR:=FrmlPtr(@IDA[Ii]^.OpErr); FTyp:='B'; exit end;
  if IsKeyWord('WARNING') then begin
    Err; RdFldNameFrmlR:=FrmlPtr(@IDA[Ii]^.OpWarn); FTyp:='B'; exit end;
  if IsKeyWord('ERRORTEXT') then begin
    Err; RdFldNameFrmlR:=IDA[Ii]^.ErrTxtFrml; FTyp:='S'; exit end;
  if FrmlSumEl<>nil then
    if FrstSumVar then begin
      if FindInLvBlk(LvToRd^.ChainBack,CBlk,RF) then begin
        FTyp:=RF^.FrmlTyp; RdFldNameFrmlR:=RF^.Frml; exit end;
      FindInRec; exit end
    else if CBlk=nil then begin FindInRec; exit end
    else if OwnInBlock then exit else OldError(8);
  if OwnInBlock then exit;
  FindInRec;
end;
procedure ChainSumElR; far;
var S:SumElPtr; Z:FloatPtrList;
begin
  if FrstSumVar or (SumIi=0) then SumIi:=1;
  if FrstSumVar or (CBlk=nil) then S:=SumElPtr(@IDA[SumIi]^.Sum)
  else S:=SumElPtr(@CBlk^.Sum);
  FrmlSumEl^.Chain:=S^.Chain; S^.Chain:=FrmlSumEl;
  if CBlkSave<>nil then begin CBlk:=CBlkSave; CBlkSave:=nil end;
  Z:=GetStore(SizeOf(Z^)); Z^.RPtr:=@FrmlSumEl^.R;
  Z^.Chain:= CZeroLst^.Chain; CZeroLst^.Chain:=Z;
end;



procedure ReadReport(RO:RprtOptPtr);

procedure CopyPrevMFlds;
var M,MNew:KeyFldDPtr; F:FieldDPtr; s:string;
begin
  s:=LexWord; M:=IDA[Ii-1]^.MFld; while M<>nil do begin
    LexWord:=M^.FldD^.Name; F:=FindFldName(InpFD(Ii));
    if F=nil then OldError(8);
    if not FldTypIdentity(M^.FldD,F) then OldError(12);
    MNew:=GetStore(sizeof(MNew^)); Move(M^,MNew^,sizeof(MNew^));
    MNew^.FldD:=F; ChainLast(IDA[Ii]^.MFld,MNew); M:=M^.Chain end;
  LexWord:=s;
end;
procedure CheckMFlds(M1,M2:KeyFldDPtr);
begin
  while M1<>nil do begin
    if M2=nil then OldError(30);
    if not FldTypIdentity(M1^.FldD,M2^.FldD) or (M1^.Descend<>M2^.Descend)
       or (M1^.CompLex<>M2^.CompLex) then OldError(12);
    M1:=M1^.Chain;M2:=M2^.Chain end;
  if M2<>nil then OldError(30);
end;
                                                                      
function MakeOldMLvD:LvDescrPtr;
var M:KeyFldDPtr; C:ConstList; L,L1:LvDescrPtr; n:word;
begin
  OldMFlds:=nil; NewMFlds:=nil; L:=GetZStore(sizeof(L^)); LstLvM:=L;
  M:=IDA[1]^.MFld; while M<>nil do begin
    n:=sizeof(pointer)+M^.FldD^.NBytes+1;
    C:=GetStore(n); ChainLast(OldMFlds,C);
    C:=GetStore(n); ChainLast(NewMFlds,C);
    L1:=GetZStore(SizeOf(L1^)); L^.ChainBack:=L1; L1^.Chain:=L; L:=L1;
    L^.Fld:=M^.FldD; M:=M^.Chain end;
  MakeOldMLvD:=L;
end;
procedure RdAutoSortSK(ID:InpDPtr);
function NewLvS(L:LvDescrPtr):LvDescrPtr;
var L1:LvDescrPtr;
begin
  L1:=GetZStore(SizeOf(L1^)); L1^.Chain:=L;
  if L=nil then ID^.LstLvS:=L1 else L^.ChainBack:=L1; NewLvS:=L1;
end;
var M,SK:KeyFldDPtr; L:LvDescrPtr; n:word; C:ConstList; as:boolean;
begin
  if Lexem=';' then begin RdLex; RdKFList(ID^.SFld,CFile) end;
  L:=nil; as:=ID^.AutoSort; if as then begin
    SK:=KeyFldDPtr(@ID^.SK); M:=ID^.MFld; while M<>nil do begin
      SK^.Chain:=GetStore(sizeof(SK^)); SK:=SK^.Chain;
      Move(M^,SK^,sizeof(SK^)); M:=M^.Chain end end;
  M:=ID^.SFld; while M<>nil do begin
    L:=NewLvS(L); L^.Fld:=M^.FldD;
    n:=sizeof(pointer)+M^.FldD^.NBytes+1;
    C:=GetStore(n); ChainLast(ID^.OldSFlds,C);
    if as then begin
      SK^.Chain:=GetStore(sizeof(SK^)); SK:=SK^.Chain;
      Move(M^,SK^,sizeof(SK^)) end;
    M:=M^.Chain end;
  if as and (ID^.SK=nil) then OldError(60);
  ID^.FrstLvS:=NewLvS(L);
end;


procedure RdBlock(var BB:BlkDPtr);
  var LineLen:integer;
      NBytesStored:integer;
      LnL:BytePtr; StrL:WordPtr;
procedure RdCh;
  begin if not IsPrintCtrl(ForwChar) then inc(LineLen); ReadChar end;
procedure StoreCh(C:char);
  var P:^char;
  begin P:=GetStore(1); P^:=C; inc(NBytesStored) end;
function NUnderscores(C:char):integer;
  var N:integer;
  begin N:=0; while ForwChar=C do begin inc(N); RdCh end; NUnderscores:=N end;
procedure EndString;
begin
  StrL^:=NBytesStored;
  inc(CBlk^.NTxtLines); if LineLen=0 then LnL^:=255 else LnL^:=LineLen;
end;
  var I,N,L,M:integer;
      RepeatedGrp:boolean;        {RdBlock - body}
      RF,RF1:RFldDPtr;
      DummyB:BlkDPtr;
      UC:char;
procedure TestSetRFTyp(Typ:char);
  begin if RepeatedGrp then begin if RF^.Typ<>Typ then Error(73) end
        else RF^.Typ:=Typ; if ForwChar in ['.',',',':'] then Error(95) end;
procedure TestSetBlankOrWrap;
  begin if RF^.Typ in ['R','F','S'] then
           if not RepeatedGrp then RF^.BlankOrWrap:=(UC='@')
           else begin if RF^.BlankOrWrap and (UC='_') then Error(73) end
        else if UC='@' then Error(80) end;
procedure RdBeginEnd(var ARoot:AssignDPtr); forward;
function RdAssign:AssignDPtr; forward;
procedure RdAssignBlk(var ARoot:AssignDPtr);
var A:AssignDPtr;
begin
  if IsKeyWord('BEGIN') then RdBeginEnd(ARoot)
  else begin A:=RdAssign; ChainLast(ARoot,A) end;
end;
function RdAssign:AssignDPtr;
var A:AssignDPtr; LV:LocVarPtr; FTyp:char; FD:FileDPtr; F:FieldDPtr;
begin
  A:=GetZStore(sizeof(A^)); RdAssign:=A;
  if IsKeyWord('IF') then begin
    A^.Kind:=_ifthenelseM; A^.Bool:=RdBool;
    AcceptKeyWord('THEN'); RdAssignBlk(A^.Instr);
    if IsKeyWord('ELSE') then RdAssignBlk(A^.ElseInstr) end
  else if ForwChar='.' then begin
    A^.Kind:=_parfile; FD:=RdFileName; if not FD^.IsParFile then OldError(9);
    Accept('.'); A^.FD:=FD; F:=RdFldName(FD); A^.PFldD:=F;
    if F^.Flg and f_Stored=0 then OldError(14);
    RdAssignFrml(F^.FrmlTyp,A^.Add,A^.Frml) end
  else if FindLocVar(LVBD.Root,LV) then begin
    RdLex; A^.Kind:=_locvar; A^.LV:=LV;
    RdAssignFrml(LV^.FTyp,A^.Add,A^.Frml) end
  else Error(147);
end;
procedure RdBeginEnd(var ARoot:AssignDPtr);
label 1;
begin
1:if IsKeyWord('END') then exit;
  RdAssignBlk(ARoot);
  if Lexem=';' then begin RdLex; goto 1 end;
  AcceptKeyWord('END');
end;
procedure RdCond;
begin
  if Lexem='(' then begin RdLex; CBlk^.Bool:=RdBool; Accept(')') end;
end;
var LV:LocVarPtr;
label 0,1,2,3,4,5;
begin
  CBlk:=GetZStore(SizeOf(BlkD)); ChainLast(BB,CBlk);
  RdCond;
  if IsKeyWord('BEGIN') then begin RdBeginEnd(CBlk^.BeforeProc); goto 1 end;
  if Lexem=';' then goto 2;     {read var decl.}
0:if IsKeyWord('BEGIN') then begin RdBeginEnd(CBlk^.AfterProc); goto 2 end;
  if Lexem='.' then begin
    RdLex;
    if IsKeyWord('LINE') then
      if Lexem=_le then begin RdLex;CBlk^.LineBound:=RdRealFrml end
      else begin
        Accept(_assign); CBlk^.AbsLine:=true; CBlk^.LineNo:=RdRealFrml end
    else if IsKeyWord('PAGE') then begin
      Accept(_assign); CBlk^.SetPage:=True; CBlk^.PageNo:=RdRealFrml end
    else if IsKeyWord('NOTATEND') then CBlk^.NotAtEnd:=true
    else if IsKeyWord('NOTSOLO') then CBlk^.DHLevel:=1
    else Error(54) end
  else begin
    RF:=GetStore(SizeOf(RF^)-1); SkipBlank(false);
    if ForwChar=':' then begin
      TestIdentif; GetStore(Length(LexWord));
      Move(LexWord,RF^.Name,length(LexWord)+1);
      if FindInLvBlk(LstLvM,DummyB,RF1) or FindLocVar(LVBD.Root,LV) then Error(26);
      RdLex; Accept(_assign) end
    else RF^.Name:='';
    RF^.Frml:=RdFrml(RF^.FrmlTyp); RF^.BlankOrWrap:=false;
    ChainLast(CBlk^.RFD,RF) end;
1:if Lexem<>';' then begin Accept(',');goto 0 end;
2:RF:=CBlk^.RFD; RepeatedGrp:=False; SkipBlank(true);
  case ForwChar of
    ^z :goto 4;
    '\':begin CBlk^.FF1:=True; ReadChar end end;
3:LnL:=GetZStore(1); StrL:=GetZStore(2); LineLen:=0; NBytesStored:=0;
  if CBlk^.NTxtLines=0 then begin
    CBlk^.Txt:=Pchar(LnL); while ForwChar=' ' do RdCh;
    CBlk^.NBlksFrst:=LineLen end;
  while not(ForwChar in [^m,^z]) do
    case ForwChar of
     '_','@',^p:begin UC:=ForwChar; StoreCh(#$FF);
          if RF=nil then
          begin RF:=CBlk^.RFD;if RF=nil then Error(30);RepeatedGrp:=True end;
          if UC=^p then begin
            if RF^.FrmlTyp<>'S' then Error(12);
            if not RepeatedGrp then RF^.Typ:='P' else
            if RF^.Typ<>'P' then Error(73); ReadChar; goto 5 end;
          L:=NUnderscores(UC);
          case ForwChar of
           ',':begin RdCh;if RF^.FrmlTyp<>'R' then Error(12);M:=NUnderScores(UC);
                     L:=L+M+1; StoreCh(char(L));StoreCh(char(M));
                     TestSetRFTyp('F') end;
           '.':begin RdCh;if RF^.FrmlTyp<>'R' then Error(12);M:=NUnderscores(UC);
                 if ForwChar='.' then
                      begin RdCh;N:=NUnderscores(UC);
                            if (L<>2) or (M<>2) or not (N in [2,4]) then Error(71);
                            TestSetRFTyp('D'); if N=4 then RF^.BlankOrWrap:=true end
                 else begin L:=L+M+1; StoreCh(char(L));StoreCh(char(M));
                            TestSetRFTyp('R') end;
               end;
           ':':begin RdCh;if RF^.FrmlTyp<>'R' then Error(12);M:=NUnderscores(UC);
                 if M<>2 then Error(69); M:=3;
                 if ForwChar=':' then begin
                   RdCh;N:=NUnderscores(UC);
                   if N<>2 then Error(69); M:=6;
                   if ForwChar='.' then begin
                     RdCh;N:=NUnderscores(UC);
                     if N<>2 then Error(69); M:=9 end end;
                 StoreCh(char(L));StoreCh(char(M)); TestSetRFTyp('T') end;
           else StoreCh(char(L)); TestSetRFTyp(RF^.FrmlTyp);
                M:=0; if RF^.Typ='S' then M:=LineLen-L+1; {current column}
                StoreCh(char(M));
          end;
          TestSetBlankOrWrap;
5:        RF:=RF^.Chain;
         end;
     '\':begin CBlk^.FF2:=True; EndString; ReadChar; goto 4 end;
     '{','#':begin ReleaseStore(LnL);goto 4 end;
     else if ForwChar=#$ff then StoreCh(' ') else StoreCh(ForwChar);RdCh;
    end;
  EndString; SkipBlank(true); if ForwChar<>^z then goto 3;
4:if (CBlk^.NTxtLines=1) and (CBlk^.Txt[1]=#0) and CBlk^.FF1 then begin
     CBlk^.NTxtLines:=0; CBlk^.FF1:=false; CBlk^.FF2:=true end;
  if RF<>nil then Error(30); RdLex;
end;
function RdKeyName:LvDescrPtr;
var L:LvDescrPtr; F:FieldDPtr;
begin
  ReadChar; Lexem:=CurrChar; Accept('_');
  if WhatToRd='O' then L:=FrstLvM else L:=IDA[Oi]^.FrstLvS;
  F:=RdFldName(InpFD(Oi));
  while L<>nil do begin
    if L^.Fld=F then begin RdKeyName:=L;exit end;
    L:=L^.Chain end;
  OldError(46);
end;
procedure Rd_Oi;
begin
  Oi:=1; if IsDigit(ForwChar) then begin
    ReadChar; Oi:=ord(CurrChar)-ord('0');
    if (Oi=0) or (Oi>MaxIi) then Error(62); WhatToRd:='i' end;
end;

                              {RdReport - body}
var FD:FileDPtr; KI:KeyInDPtr; B:BlkDPtr; i:word;
    ID:InpDPtr; L:LvDescrPtr; s:string[2]; FDL:RprtFDList;
label 1,2,3,4;
begin
  ResetCompilePars; RdLex;
  CBlkSave:=nil; PgeSizeZ:=nil; PgeLimitZ:=nil;
  FDL:=nil; if (RO<>nil) and (RO^.FDL.FD<>nil) then FDL:=@RO^.FDL;
  ResetLVBD; if IsKeyWord('VAR') then RdLocDcl(LVBD,false,false,'R');
1:if Lexem='.' then begin
    RdLex;TestIdentif;
    if IsKeyWord('PAGESIZE') then begin Accept(_assign);PgeSizeZ:=RdRealFrml end
    else if IsKeyWord('PAGELIMIT') then begin
      Accept(_assign);PgeLimitZ:=RdRealFrml end
    else Error(56);if Lexem=';' then begin RdLex;goto 1 end end;
  SelQuest:=false; Ii:=0; TestLex('#');
  repeat
    inc(Ii); ReadChar; if CurrChar='I' then begin ReadChar;
      if IsDigit(CurrChar) then begin
        if Ii<>ord(CurrChar)-ord('0') then Error(61); ReadChar;
        if CurrChar='_' then goto 2 end
      else if Ii=1 then goto 2 end;
    Error(89);
2:  ID:=GetZStore(SizeOf(ID^)); IDA[Ii]:=ID;
    RdLex; FD:=RdFileName; CFile:=FD;
    for i:=1 to Ii-1 do if InpFD(i)=FD then OldError(26);
    if (FDL<>nil) then begin CFile:=FDL^.FD; FD:=CFile end;
    CViewKey:=RdViewKey;
    if (FDL<>nil) then begin
      if FDL^.ViewKey<>nil then CViewKey:=FDL^.ViewKey end;
    if Lexem='!' then begin RdLex; ID^.AutoSort:=true end;
    ID^.Op:=_const; ID^.OpErr:=_const; ID^.OpWarn:=_const; KI:=nil;
    ID^.ForwRecPtr:=GetRecSpace; FD^.RecPtr:=GetRecSpace;
    if Lexem='(' then begin RdLex;
      if (Ii=1) and (Lexem='?') then begin SelQuest:=true;RdLex end
      else ID^.Bool:=RdKeyInBool(KI,false,false,ID^.SQLFilter); Accept(')') end;
    if (FDL<>nil) and (FDL^.LVRecPtr=nil) and
       ((FDL^.Cond<>nil) or (FDL^.KeyIn<>nil) or (Ii=1) and RO^.UserCondQuest) then begin
      ID^.Bool:=RunEvalFrml(FDL^.Cond);
      KI:=FDL^.KeyIn; ID^.SQLFilter:=FDL^.SQLFilter;
      if Ii=1 then SelQuest:=RO^.UserCondQuest end;
    New(ID^.Scan,Init(FD,CViewKey,KI,true));
    if (FDL<>nil) and (FDL^.LVRecPtr<>nil) then ID^.Scan^.ResetLV(FDL^.LVRecPtr);
    if not(Lexem in [';','#',^z]) then RdKFList(ID^.MFld,FD);
    if Ii>1 then
      if IDA[Ii-1]^.MFld=nil then begin
        if ID^.MFld<>nil then OldError(22) end
      else if ID^.MFld=nil then CopyPrevMFlds
      else CheckMFlds(IDA[Ii-1]^.MFld,ID^.MFld);
    RdAutoSortSK(ID); TestLex('#');
    if FDL<>nil then FDL:=FDL^.Chain;
  until ForwChar<>'I';
  MaxIi:=Ii; FrstLvM:=MakeOldMLvD;

  PageHd:=nil; RprtHd:=nil; PageFt:=nil; PFZeroLst:=nil;
3:s[0]:=#2;ReadChar;s[1]:=upcase(CurrChar);ReadChar;s[2]:=upcase(CurrChar);
  @ChainSumEl:=nil; RdFldNameFrml:=RdFldNameFrmlR; WhatToRd:='O';
  if s='DE' then begin
    Rd_Oi; RdLex; WhatToRd:='i'; RdBlock(IDA[Oi]^.FrstLvS^.Ft); goto 4 end;
  if s='RH' then begin
    RdLex; RdBlock(RprtHd); goto 4 end;
  if s='PH' then begin
    RdLex; RdBlock(PageHd); goto 4 end;
  if s='DH' then begin
    Rd_Oi; RdLex; WhatToRd:='i'; RdBlock(IDA[Oi]^.FrstLvS^.Hd); goto 4 end;
  if s='CH' then begin
    Rd_Oi; L:=RdKeyName; RdBlock(L^.Hd); goto 4 end;
  ChainSumEl:=ChainSumElR;
  if s='RF' then begin
    RdLex; LvToRd:=LstLvM; CZeroLst:=FloatPtrList(@LvToRd^.ZeroLst);
    RdBlock(LvToRd^.Ft); goto 4 end;
  if s='CF' then begin
    Rd_Oi; LvToRd:=RdKeyName; CZeroLst:=FloatPtrList(@LvToRd^.ZeroLst);
    RdBlock(LvToRd^.Ft); goto 4 end;
  if s='PF' then begin
    RdLex; LvToRd:=LstLvM; CZeroLst:=FloatPtrList(@PFZeroLst);
    RdBlock(PageFt);goto 4 end;
  Error(57);
4:if Lexem<>^z then begin TestLex('#'); goto 3 end;

  for i:=1 to MaxIi do begin
    ID:=IDA[i];
    if ID^.ErrTxtFrml<>nil then RdChkDsFromPos(ID^.Scan^.FD,ID^.Chk) end;
end;

end.