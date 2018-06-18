{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L-}

unit runmerg;

interface

uses base,access,rdrun,sort;

procedure RunMerge;

implementation

uses obaseww,oaccess,runfrml;

var NRecsAll:longint;

procedure RunMerge;

function CompMFlds(M:KeyFldDPtr):word;
var x:XString;
begin
  x.PackKF(M);
  CompMFlds:=CompStr(x.S,OldMXStr.S);
end;
procedure SetOldMFlds(M:KeyFldDPtr);
var C:ConstList; F:FieldDPtr;
begin
  OldMXStr.Clear; C:=OldMFlds; while C<>nil do begin
    F:=M^.FldD; case F^.FrmlTyp of
      'S':begin C^.S:=_shorts(F); OldMXStr.StoreStr(C^.S,M) end;
      'R':begin C^.R:=_r(F); OldMXStr.StoreReal(C^.R,M) end;
      else C^.B:=_b(F); OldMXStr.StoreBool(C^.B,M) end;
    C:=C^.Chain;M:=M^.Chain end;
end;
procedure SetMFlds(M:KeyFldDPtr);
  var C:ConstList;
      F:FieldDPtr;
  begin C:=OldMFlds;
      while M<>nil do
      begin F:=M^.FldD;case F^.FrmlTyp of
          'S':s_(F,C^.S);'R':r_(F,C^.R);else b_(F,C^.B) end;
          M:=M^.Chain;C:=C^.Chain;
      end;
  end;

procedure ReadInpFile(ID:InpDPtr);
label 1;
begin with ID^ do begin
  CRecPtr:=ForwRecPtr;
1:Scan^.GetRec; if Scan^.EOF then exit;
  inc(NRecsAll); RunMsgN(NRecsAll); if not RunBool(Bool) then goto 1;
end end;

procedure ZeroSumFlds(Z:SumElPtr);
  begin while Z<>nil do begin Z^.R:=0.0;Z:=Z^.Chain end end;
procedure SumUp(Z:SumElPtr);
  begin while Z<>nil do begin Z^.R:=Z^.R+RunReal(Z^.Frml); Z:=Z^.Chain end end;
procedure RunAssign(A:AssignDPtr);
begin
  while A<>nil do begin
    with A^ do case Kind of
     _move: Move(FromPtr^,ToPtr^,L);
     _zero:case FldD^.FrmlTyp of
            'S':s_(FldD,''); 'R':r_(FldD,0); else b_(FldD,False) end;
     _output: AssgnFrml(OFldD,Frml,false,Add);
     _locvar: LVAssignFrml(LV,MyBP,Add,Frml);
     _parfile: AsgnParFldFrml(FD,PFldD,Frml,Add);
     _ifthenelseM:if RunBool(Bool) then RunAssign(Instr) else RunAssign(ElseInstr);
    end;
    A:=A^.Chain;
end end;
procedure WriteOutp(RD:OutpRDPtr);
var OD:OutpFDPtr;
begin while RD<>nil do begin
  if RunBool(RD^.Bool) then begin OD:=RD^.OD;
    if OD=nil {dummy } then RunAssign(RD^.Ass)
    else begin
      CFile:=OD^.FD; CRecPtr:=OD^.RecPtr; ClearDeletedFlag;
      RunAssign(RD^.Ass);
{$ifdef FandSQL} if CFile^.IsSQLFile then OD^.Strm^.PutRec else {$endif}
      begin PutRec; if OD^.Append and (CFile^.Typ='X') then
        TryInsertAllIndexes(CFile^.IRec) end end end;
  RD:=RD^.Chain;
end end;
procedure OpenInp;
var I:integer;
begin
  NRecsAll:=0; for I:=1 to MaxIi do with IDA[I]^ do begin
    CFile:=Scan^.FD;
    if IsInplace then Md:=NewLMode(ExclMode) else Md:=NewLMode(RdMode);
    Scan^.ResetSort(SK,Bool,Md,SQLFilter);
    inc(NRecsAll,Scan^.NRecs) end;
end;
procedure OpenOutp;
var OD:OutpFDPtr;
begin
  OD:=OutpFDRoot; while OD<>nil do with OD^ do begin
    CFile:=FD;
{$ifdef FandSQL} if CFile^.IsSQLFile then begin
      New(Strm,Init); Strm^.OutpRewrite(Append); CRecPtr:=RecPtr; SetTWorkFlag end else {$endif}
    if InplFD<>nil then FD:=OpenDuplF(true) else Md:=RewriteF(Append);
    OD:=Chain end;
end;
procedure CloseInpOutp;
var i:integer; OD:OutpFDPtr;
begin
  OD:=OutpFDRoot; while OD<>nil do with OD^ do begin
    CFile:=FD; ClearRecSpace(RecPtr);
{$ifdef FandSQL} if CFile^.IsSQLFile then with Strm^ do begin
      OutpClose; Done end else {$endif}
    if InplFD<>nil then begin CFile:=InplFD; SubstDuplF(FD,true) end
    else OldLMode(Md);
    OD:=Chain end;
  for i:=1 to MaxIi do with IDA[i]^ do begin
    Scan^.Close; ClearRecSpace(ForwRecPtr); OldLMode(Md) end;
end;
procedure MoveForwToRec(ID:InpDPtr);
var C:ChkDPtr;
begin
  with ID^ do begin
    CFile:=Scan^.FD; CRecPtr:=CFile^.RecPtr;
    Move(ForwRecPtr^,CRecPtr^,CFile^.RecLen+1); Count:=Count+1;
    C:=Chk; if C<>nil then begin
      Error:=false; Warning:=false; ErrTxtFrml^.S[0]:=#0; while C<>nil do begin
        if not RunBool(C^.Bool) then begin
          Warning:=true; ErrTxtFrml^.S:=RunShortStr(C^.TxtZ);
          if not C^.Warning then begin Error:=true; exit end end;
        C:=C^.Chain end end;
end end;
procedure MergeProc;
var i,res:word; ID:InpDPtr;
begin
  for i:=1 to MaxIi do begin ID:=IDA[i]; with ID^ do
    if Exist then
      repeat
        MoveForwToRec(ID); SumUp(Sum); WriteOutp(RD); ReadInpFile(ID);
        if Scan^.EOF then res:=ord(_gt) else begin
          res:=CompMFlds(ID^.MFld);
          if res=ord(_lt) then CFileError(607) end;
      until res=ord(_gt)
    else begin CFile:=Scan^.FD; CRecPtr:=CFile^.RecPtr;
      ZeroAllFlds; SetMFlds(MFld) end;
end end;
procedure JoinProc(Ii:word;var EmptyGroup:boolean);
var i,res:word; ID:InpDPtr;
begin
  if Ii>MaxIi then begin if not EmptyGroup then begin
    for I:=1 to MaxIi do SumUp(IDA[I]^.Sum); WriteOutp(IDA[MaxIi]^.RD) end end
  else begin ID:=IDA[Ii]; with ID^ do
    if Exist then begin
      Scan^.SeekRec(IRec-1); Count:=0.0;
      CRecPtr:=ForwRecPtr; Scan^.GetRec;
      repeat MoveForwToRec(ID); JoinProc(Ii+1,EmptyGroup);
        ReadInpFile(ID);
        if Scan^.EOF then res:=ord(_gt) else begin
          res:=CompMFlds(MFld); if res=ord(_lt) then CFileError(607) end;
      until res=ord(_gt) end
    else begin CFile:=Scan^.FD; CRecPtr:=CFile^.RecPtr; EmptyGroup:=true;
      ZeroAllFlds; SetMFlds(MFld);
      JoinProc(Ii+1,EmptyGroup) end end;
end;

var I,MinIi,res,NEof:integer;                     {RunMerge - body}
    EmptyGroup,b:boolean;
label 1,2;
begin
  PushProcStk; OpenInp; OpenOutp; MergOpGroup.Group:=1.0;
  RunMsgOn('M',NRecsAll); NRecsAll:=0;
  for I:=1 to MaxIi do ReadInpFile(IDA[I]);
1:MinIi:=0;NEof:=0;
  for I:=1 to MaxIi do with IDA[I]^ do begin
    CFile:=Scan^.FD; IRec:=Scan^.IRec; ZeroSumFlds(Sum);
    if Scan^.EOF then inc(NEof);
    if OldMFlds=nil then begin Exist:=not Scan^.Eof;MinIi:=1 end
    else begin
      CRecPtr:=ForwRecPtr; Exist:=False; Count:=0.0;
      if not Scan^.EOF then begin
        if MinIi=0 then goto 2; res:=CompMFlds(MFld);
        if res<>ord(_gt) then begin
          if res=ord(_lt) then begin 2:SetOldMFlds(MFld); MinIi:=I end;
          Exist:=true end;
    end end end;
  for I:=1 to MinIi-1 do IDA[I]^.Exist:=false;
  if NEof=MaxIi then begin
    b:=SaveCache(0); RunMsgOff; if not b then GoExit;
    CloseInpOutp;
    PopProcStk; exit end;
  EmptyGroup:=false;
  if Join then JoinProc(1,EmptyGroup) else MergeProc;
  if not EmptyGroup then begin
    WriteOutp(OutpRDs); MergOpGroup.Group:=MergOpGroup.Group+1.0 end;
  goto 1;
end;

end.