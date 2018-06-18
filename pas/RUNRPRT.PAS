{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit runrprt;

interface

uses rdrun;

procedure RunReport(RO:RprtOptPtr);

implementation

uses dos,drivers,base,obase,obaseww,access,oaccess,
     runfrml,wwmix,sort;

procedure Ovr; far;
assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push cs; push ax;
end;


type TTDPtr=^TTD;
     TTD=record Chain:TTDPtr;
                SL:StringList;
                Col,Width,Ln:word end;
     YRec=record P:Pchar;
                 I,Ln,TLn,Sz:word;
                 Blk:BlkDPtr;
                 ChkPg:boolean;
                 TD:TTDPtr end;
var  PrintDH:word;
     Y:YRec;
     FrstBlk,NoFF,WasFF2,SetPage,WasOutput:boolean;
     LineLenLst,PageNo,PgeSize:integer;
     Store2Ptr:pointer;

     NRecsAll,RecCount:longint;
     NEof:word;
     MinID:InpDPtr;
     FirstLines,WasDot:boolean;
     NLinesOutp:longint;


procedure RunReport(RO:RprtOptPtr);

procedure PrintPageHd;forward;
procedure PrintPageFt;forward;
procedure TruncLine;forward;
procedure ResetY;
  begin fillchar(Y,sizeof(Y),0) end;
procedure IncPage;
  begin if SetPage then begin SetPage:=false;RprtPage:=PageNo end
        else inc(RprtPage) end;
procedure NewLine;
begin
  writeln(Rprt);
  if WasDot then WasDot:=false else begin
    inc(RprtLine); inc(NLinesOutp); FirstLines:=false end;
  LineLenLst:=0;
  if RprtLine>PgeSize then begin dec(RprtLine,PgeSize);IncPage end;
end;
procedure FormFeed;
  var I:integer;
  begin if NoFF then NoFF:=false
        else begin write(Rprt,^l);RprtLine:=1;IncPage end;
        LineLenLst:=0; WasFF2:=False;
  end;
procedure NewPage;
  begin PrintPageFt; PrintPageHd; TruncLine; FrstBlk:=False end;
function OutOfLineBound(B:BlkDPtr):boolean;
begin
  OutOfLineBound:=(B^.LineBound<>nil) and (RprtLine>RunReal(B^.LineBound))
                   or B^.AbsLine and (RunInt(B^.LineNo)<RprtLine)
end;
procedure Zero(Z:FloatPtrList);
  begin while Z<>nil do begin Z^.RPtr^:=0;Z:=Z^.Chain end end;
procedure WriteNBlks(N:integer);
  begin if N>0 then write(Rprt,' ':N) end;
procedure NewTxtCol(S:LongStrPtr; Col,Width:word; Wrap:boolean);
var Absatz: boolean;
function GetLine(var TA:CharArrPtr;var TLen:word;Width:word;Wrap:boolean):string;
type
  CharArr0 = array[0..10] of char;  CharArr0Ptr = ^CharArr0;
var
  TAOff: word absolute TA;  T: CharArr0Ptr;
  i, i2, j, l2, w, n, n1, n2, nWords, iWrdEnd, i2WrdEnd, wWrdEnd, nWrdEnd: integer;
  WasWrd, Fill: boolean;  s, s1: string;  c: char;
label 1;
begin
  i:= 0;  i2:=0; w:=0;
  T:=CharArr0Ptr(TA);  WasWrd:=false;  nWords:=0; Fill:=false;
  while (i<TLen) and (i2<255) do begin
    c:=T^[i];
    if c=^m then begin Absatz:=true; goto 1 end;
    if (w>=Width) and (c=' ') and Wrap then goto 1;
    if (c<>' ') or WasWrd or not Wrap or Absatz then
      begin inc(i2); s[i2]:=c;  if not IsPrintCtrl(c) then inc(w) end;
    if c=' ' then begin
      if WasWrd then
        begin WasWrd:=false; i2WrdEnd:=i2-1; iWrdEnd:=i;
              wWrdEnd:=w-1; nWrdEnd:=nWords end end
    else if not WasWrd then begin WasWrd:=true; Absatz:=false; inc(nWords) end;
    inc(i) end;
1:if Wrap and (nWords>=2) and (w>Width) then
    begin i2:=i2WrdEnd; i:=iWrdEnd;
          w:=wWrdEnd; nWords:=nWrdEnd; Fill:=true; Absatz:=false end;
  if (i<TLen) and (T^[i]=^m) then
      begin inc(i);  if (i<TLen) and (T^[i]=^j) then inc(i) end;
  inc(TAOff,i); dec(TLen,i);
  l2:=i2; if w<Width then inc(l2, Width-w); n:=l2-i2;
  if (nWords<=1) or (n=0) or not Fill then
    begin FillChar(s[i2+1],n,' '); s[0]:=char(l2) end
  else begin
    n1:=n div (nWords-1); n2:=n mod (nWords-1);
    s[0]:=char(i2); s1:= s; s[0]:=char(l2); i2:=1; WasWrd:=false;
    for i:=1 to length(s1) do begin
      s[i2]:=s1[i];
      if s[i2]<>' ' then WasWrd:= true
      else if WasWrd then begin
        WasWrd:=false; for j:= 1 to n1 do begin inc(i2); s[i2]:=' ' end;
        if n2>0 then begin dec(n2); inc(i2); s[i2]:=' ' end end;
      inc(i2) end;
    end;
   GetLine:= s;
end;

var TA:CharArrPtr; i,LL,Ln:word; TD:TTDPtr;
    ss:string; SL:StringList;
begin
  LL:=S^.LL; TA:=CharArrPtr(@S^.A); Ln:=0; Absatz:=true;
  if Wrap then for i:=1 to LL do
    if (TA^[i]=^m) and ((i=LL) or (TA^[i+1]<>^j)) then TA^[i]:=' ';
  ss:=GetLine(TA,LL,Width,Wrap);write(Rprt,ss);
  while LL>0 do begin
    ss:=GetLine(TA,LL,Width,Wrap); inc(Ln);
    if Ln=1 then begin
      TD:=GetStore2(sizeof(TD^));TD^.SL:=nil;
      TD^.Col:=Col; TD^.Width:=Width end;
    SL:=GetStore2(length(ss)+5); SL^.S:=ss;
    ChainLast(TD^.SL,SL) end;
  if Ln>0 then begin
    TD^.Ln:=Ln; ChainLast(Y.TD,TD); if Ln>Y.TLn then Y.TLn:=Ln end;
end;
procedure CheckPgeLimit;
var p2:pointer; YY:YRec;
begin
  if Y.ChkPg and (RprtLine>PgeLimit) and (PgeLimit<PgeSize) then begin
    p2:=Store2Ptr; MarkStore2(Store2Ptr);
    YY:=Y; ResetY; NewPage; Y:=YY; Store2Ptr:=p2 end;
end;
procedure PendingTT;
var TD:TTDPtr; Col,l,lll:word; SL:StringList;
begin
  lll:=LineLenLst;Col:=LineLenLst+1;
  while Y.TLn>0 do begin
    NewLine; CheckPgeLimit; TD:=Y.TD; Col:=1; while TD<>nil do begin
      if TD^.Ln>0 then begin
        WriteNBlks(TD^.Col-Col); SL:=TD^.SL; write(Rprt,SL^.S);
        l:=LenStyleStr(SL^.S); Col:=TD^.Col+l; dec(TD^.Ln);
        TD^.SL:=SL^.Chain end;
      TD:=TD^.Chain end;
    dec(Y.TLn); LineLenLst:=lll end;
  WriteNBlks(LineLenLst+1-Col);
  if Y.TD<>nil then begin ReleaseStore2(Store2Ptr);Y.TD:=nil;Y.TLn:=0 end;
end;

procedure Print1NTupel(Skip:boolean);
type REditD=record Char:char;L,M:byte end; REditDPtr=^REditD;
var RF:RFldDPtr; RE:REditDPtr;
    L,M,i:word; C:char; R:float; Mask:ScreenStr; S:LongStrPtr;
label 1,2,3;
begin
  if Y.Ln=0 then exit; RF:=RFldDPtr(@Y.Blk^.RFD);
1:WasOutput:=true;
  while Y.I<Y.Sz do begin
    RE:=REditDPtr(@Y.P[Y.I]); C:=RE^.Char;
    if C=#$FF then begin
      RF:=RF^.Chain; if RF=nil then exit; L:=RE^.L; M:=RE^.M;
      if RF^.FrmlTyp='R' then begin
        if not Skip then R:=RunReal(RF^.Frml);
        case RF^.Typ of
         'R','F':begin if Skip then write(Rprt,' ':L)
                 else begin if RF^.Typ='F' then R:=R/Power10[M];
                   if RF^.BlankOrWrap and (R=0) then
                     if M=0 then write(Rprt,' ':L)
                     else write(Rprt,' ':L-M-1,'.',' ':M)
                   else write(Rprt,R:L:M) end;
                 inc(Y.I,2) end;
         'D':begin if RF^.BlankOrWrap then Mask:='DD.MM.YYYY' else
             Mask:='DD.MM.YY'; goto 2 end;
         'T':begin Mask:=copy('hhhhhh',1,L)+copy(':mm:ss.tt',1,M);
                 inc(Y.I,2);
2:               if Skip then write(Rprt,' ':length(Mask))
                 else write(Rprt,StrDate(R,Mask)) end;
        end end
      else begin
        if RF^.Typ='P' then begin
          S:=RunLongStr(RF^.Frml); write(Rprt,^P);
          for i:=0 to S^.LL+1 do write(Rprt,Pchar(S)[i]); ReleaseStore(S);
          goto 3 end;
        inc(Y.I,2);
        if Skip then write(Rprt,' ':L)
        else case RF^.FrmlTyp of
           'S':begin S:=RunLongStr(RF^.Frml);
               while (S^.LL>0) and (S^.A[S^.LL]=' ') do dec(S^.LL);
               NewTxtCol(S,M,L,RF^.BlankOrWrap); ReleaseStore(S) end;
           'B':if RunBool(RF^.Frml) then write(Rprt,AbbrYes)
               else write(Rprt,AbbrNo) end end;
      end
      else begin
        if (C='.') and (Y.I=0) and FirstLines then WasDot:=true;
        write(Rprt,C) end;
3:    inc(Y.I) end;
  PendingTT; dec(Y.Ln); if Y.Ln>0 then begin
    inc(Y.P,Y.Sz); NewLine;
    L:=ord(Y.P^); inc(Y.P); Y.Sz:=WordPtr(Y.P)^; inc(Y.P,2); Y.I:=0;
    CheckPgeLimit; LineLenLst:=L; goto 1 end;
  Y.Blk:=nil;
end;
procedure FinishTuple;
  begin while Y.Blk<>nil do Print1NTupel(True) end;
procedure RunAProc(A:AssignDPtr);
begin
  while A<>nil do begin
    with A^ do case Kind of
     _locvar:LVAssignFrml(LV,MyBP,Add,Frml);
     _parfile:AsgnParFldFrml(FD,PFldD,Frml,Add);
     _ifthenelseM:if RunBool(Bool) then RunAProc(Instr) else RunAProc(ElseInstr);
    end;
    A:=A^.Chain end;
end;
procedure PrintTxt(B:BlkDPtr;ChkPg:boolean);
var I:integer;
begin
  if B=nil then exit;
  if B^.SetPage then begin
    PageNo:=RunInt(B^.PageNo); SetPage:=true end;
  if B<>Y.Blk then begin
    FinishTuple;
    if B^.AbsLine then for I:=RprtLine to RunInt(B^.LineNo)-1 do NewLine;
    if B^.NTxtLines>0 then begin
      if B^.NBlksFrst<LineLenLst then NewLine;
      for I:=1 to B^.NBlksFrst-LineLenLst do write(Rprt,' ') end;
    ResetY; Y.Ln:=B^.NTxtLines; if Y.Ln<>0 then begin
      Y.Blk:=B; Y.P:=B^.Txt; Y.ChkPg:=ChkPg;
      LineLenLst:=ord(Y.P^); inc(Y.P); Y.Sz:=WordPtr(Y.P)^; inc(Y.P,2) end end;
  RunAProc(B^.BeforeProc); Print1NTupel(False); RunAProc(B^.AfterProc);
end;
procedure TruncLine;
  begin FinishTuple;if LineLenLst>0 then NewLine end;
procedure PrintBlkChn(B:BlkDPtr;ChkPg,ChkLine:boolean);
begin while B<>nil do begin
  if RunBool(B^.Bool) then begin
    if ChkLine then begin
      if OutOfLineBound(B) then WasFF2:=true;
      if B^.FF1 or WasFF2 then NewPage end;
    PrintTxt(B,ChkPg); WasFF2:=B^.FF2 end;
  B:=B^.Chain;
end end;
procedure PrintPageFt;
var Ln:word; b:boolean;
begin
  if not FrstBlk then begin
    b:=WasFF2;TruncLine;Ln:=RprtLine;PrintBlkChn(PageFt,false,false);TruncLine;
    NoFF:=RprtLine<Ln; Zero(PFZeroLst); WasFF2:=b end;
end;
procedure PrintPageHd;
var b:boolean;
begin
  b:=FrstBlk; if not b then FormFeed; PrintBlkChn(PageHd,false,false);
  if not b then PrintDH:=2;
end;
procedure SumUp(S:SumElPtr);
  begin while S<>nil do begin
  S^.R:=S^.R+RunReal(S^.Frml); S:=S^.Chain end end;
procedure PrintBlock(B:BlkDPtr;DH:BlkDPtr);
var LAfter:word; B1:BlkDPtr; pdh:boolean;
begin
  pdh:=false;
  while B<>nil do begin
    if RunBool(B^.Bool) then begin
        if B<>Y.Blk then begin
          if (B^.NTxtLines>0) and (B^.NBlksFrst<LineLenLst) then TruncLine;
          if OutOfLineBound(B) then WasFF2:=true;
          LAfter:=RprtLine+maxi(0,B^.NTxtLines-1);
          if (DH<>nil) and (PrintDH>=DH^.DHLevel+1) then begin
            B1:=DH; while B1<>nil do begin
             if RunBool(B1^.Bool) then inc(LAfter,B1^.NTxtLines); B1:=B1^.Chain end end;
          if B^.FF1 or WasFF2 or FrstBlk and (B^.NTxtLines>0) or
             (PgeLimit<PgeSize) and (LAfter>PgeLimit) then  NewPage;
          if (DH<>nil) and (PrintDH>=DH^.DHLevel+1) then begin
            PrintBlkChn(DH,true,false); PrintDH:=0 end;
          end;
        WasOutput:=false;
        PrintTxt(B,true);
        WasFF2:=B^.FF2;
        if (DH=nil) and WasOutput then pdh:=true;
        SumUp(B^.Sum) end;
    B:=B^.Chain end;
  if pdh then PrintDH:=2;
end;
procedure Footings(L,L2:LvDescrPtr);
begin
  while L<>nil do begin PrintBlock(L^.Ft,nil); if L=L2 then exit; L:=L^.Chain end;
end;
procedure Headings(L,L2:LvDescrPtr);
begin
  while (L<>nil) and (L<>L2) do begin PrintBlock(L^.Hd,nil); L:=L^.ChainBack end;
end;
procedure ZeroSumFlds(L:LvDescrPtr);
  begin while(L<>nil) do begin Zero(L^.ZeroLst); L:=L^.ChainBack end end;

procedure ReadInpFile(ID:InpDPtr);
label 1;
begin with ID^ do begin
  CRecPtr:=ForwRecPtr;
1:Scan^.GetRec; if Scan^.EOF then exit;
  if ESCPressed and PromptYN(24) then begin
    WasLPTCancel:=true; GoExit end;
  inc(RecCount); RunMsgN(RecCount); if not RunBool(Bool) then goto 1;
end end;
procedure OpenInp;
var i:integer;
begin
  NRecsAll:=0; for i:=1 to MaxIi do with IDA[i]^ do begin
    CFile:=Scan^.FD; if Scan^.Kind=5 then Scan^.SeekRec(0) else begin
      Md:=NewLMode(RdMode); Scan^.ResetSort(SK,Bool,Md,SQLFilter) end;
    inc(NRecsAll,Scan^.NRecs) end;
end;
procedure CloseInp;
var i:word;
begin
  for i:=1 to MaxIi do with IDA[i]^ do if Scan^.Kind<>5 then begin
    Scan^.Close; ClearRecSpace(ForwRecPtr); OldLMode(Md) end;
end;
function CompMFlds(C:ConstList;M:KeyFldDPtr;var NLv:integer):word;
var res:integer; x:XString;
begin
  NLv:=0; while C<>nil do begin
    inc(NLv); x.Clear; x.StoreKF(M); res:=CompStr(x.S,C^.S);
    if res<>ord(_equ) then begin CompMFlds:=res; exit end;
    C:=C^.Chain; M:=M^.Chain end;
  CompMFlds:=ord(_equ);
end;
procedure GetMFlds(C:ConstList;M:KeyFldDPtr);
var x:XStringPtr;
begin
  while C<>nil do begin
    x:=XStringPtr(@C^.S); x^.Clear; x^.StoreKF(M); C:=C^.Chain; M:=M^.Chain end;
end;
procedure MoveMFlds(C1,C2:ConstList);
begin
  while C2<>nil do begin
    C2^.S:=C1^.S; C1:=C1^.Chain; C2:=C2^.Chain end;
end;
procedure PutMFlds(M:KeyFldDPtr);
var f,f1:FieldDPtr; cf,cf1:FileDPtr; cr,cr1:pointer; m1:KeyFldDPtr;
    s:string; r:float; b:boolean;
begin
  if MinID=nil then exit;
  cf:=CFile; cf1:=MinID^.Scan^.FD; cr:=CRecPtr; cr1:=MinID^.ForwRecPtr;
  m1:=MinID^.MFld; while m<>nil do begin
    f:=m^.FldD; f1:=m1^.FldD; CFile:=cf1; CRecPtr:=cr1; case f^.FrmlTyp of
      'S':begin s:=_shorts(f1); CFile:=cf; CRecPtr:=cr; s_(f,s) end;
      'R':begin r:=_r(f1); CFile:=cf; CRecPtr:=cr; r_(f,r) end;
      else      b:=_b(f1); CFile:=cf; CRecPtr:=cr; b_(f,b) end;
    m:=m^.Chain; m1:=m1^.Chain end;
end;

procedure GetMinKey;
var i,nlv,mini,res:integer;
label 1;
begin
  mini:=0; NEof:=0; 
  for i:=1 to MaxIi do with IDA[i]^ do begin
    CFile:=Scan^.FD; if Scan^.EOF then inc(NEof);
    if OldMFlds=nil then begin Exist:=not Scan^.Eof; mini:=1 end
    else begin
      CRecPtr:=ForwRecPtr; Exist:=false;
      if not Scan^.EOF then begin
        if mini=0 then goto 1; res:=CompMFlds(NewMFlds,MFld,nlv);
        if res<>ord(_gt) then begin
          if res=ord(_lt) then begin 1:GetMFlds(NewMFlds,MFld);mini:=i end;
          Exist:=true end;
  end end end;
  if mini>0 then begin
    for i:=1 to mini-1 do IDA[i]^.Exist:=false; MinID:=IDA[mini] end
  else MinID:=nil;
end;
procedure ZeroCount;
  var i:integer;
  begin for i:=1 to MaxIi do IDA[i]^.Count:=0.0 end;
function GetDifLevel:LvDescrPtr;
var C1,C2:ConstList; M:KeyFldDPtr; L:LvDescrPtr;
begin
  C1:=NewMFlds; C2:=OldMFlds; M:=IDA[1]^.MFld; L:=LstLvM^.ChainBack;
  while M<>nil do begin
    if C1^.S<>C2^.S then begin GetDifLevel:=L; exit end;
    C1:=C1^.Chain; C2:=C2^.Chain; M:=M^.Chain; L:=L^.ChainBack end;
end;
procedure MoveForwToRec(ID:InpDPtr);
var C:ChkDPtr;
begin with ID^ do begin
  CFile:=Scan^.FD; CRecPtr:=CFile^.RecPtr;
  Move(ForwRecPtr^,CRecPtr^,CFile^.RecLen+1); Count:=Count+1;
  C:=Chk; if C<>nil then begin
    Error:=false; Warning:=false; ErrTxtFrml^.S[0]:=#0; while C<>nil do begin
      if not RunBool(C^.Bool) then begin
        Warning:=true; ErrTxtFrml^.S:=RunShortStr(C^.TxtZ);
        if not C^.Warning then begin Error:=true; exit end end;
      C:=C^.Chain end end;
end end;
procedure MoveFrstRecs;
var i:integer;
begin
  for i:=1 to MaxIi do with IDA[i]^ do begin
    if Exist then MoveForwToRec(IDA[i])
    else begin CFile:=Scan^.FD; CRecPtr:=CFile^.RecPtr; ZeroAllFlds;
      PutMFlds(MFld) end end;
end;
procedure MergeProc;
var i,res,nlv:integer; ID:InpDPtr; L:LvDescrPtr;
label 1,2,3,4;
begin
  for i:=1 to MaxIi do begin ID:=IDA[i]; with ID^ do begin if Exist then begin
  CFile:=Scan^.FD; CRecPtr:=CFile^.RecPtr; L:=LstLvS;
1:ZeroSumFlds(L); GetMFlds(OldSFlds,SFld);
  if WasFF2 then PrintPageHd; Headings(L,FrstLvS);
  if PrintDH=0 then PrintDH:=1;
2:PrintBlock(FrstLvS^.Ft,FrstLvS^.Hd); {DE}  SumUp(Sum);
  ReadInpFile(ID); if Scan^.EOF then goto 4;
  res:=CompMFlds(NewMFlds,ID^.MFld,nlv);
  if (res=ord(_lt)) and (MaxIi>1) then begin
    SetMsgPar(Scan^.FD^.Name); RunError(607) end;
  if res<>ord(_equ) then goto 4;
  res:=CompMFlds(OldSFlds,SFld,nlv);
  if res=ord(_equ) then begin MoveForwToRec(ID); goto 2 end;
  L:=LstLvS; while nlv>1 do begin L:=L^.ChainBack; dec(nlv) end;
  Footings(FrstLvS^.Chain,L);
  if WasFF2 then PrintPageFt;
  MoveForwToRec(ID);
  goto 1;
4:Footings(FrstLvS^.Chain,LstLvS);
end end end end;

function RewriteRprt(RO:RprtOptPtr;Pl:word; var Times:word;var IsLPT1:boolean):boolean;
  var PrintCtrl:boolean;
  begin PrintView:=false;WasLPTCancel:=false;IsLPT1:=false;RewriteRprt:=false;
     Times:=1;PrintCtrl:=RO^.PrintCtrl;
     if (RO<>nil) and (RO^.Times<>nil) then Times:=RunInt(RO^.Times);
     if (RO=nil) or (RO^.Path=nil)and(RO^.CatIRec=0) then
        begin SetPrintTxtPath;PrintView:=true;PrintCtrl:=false end
     else begin
        if SEquUpcase(RO^.Path^,'LPT1') then
           begin CPath:='LPT1'; CVol:=''; IsLPT1:=true;
                 RewriteRprt:=ResetPrinter(Pl,0,true,true) and RewriteTxt(Rprt,false);
                 exit end;
        SetTxtPathVol(RO^.Path,RO^.CatIRec) end;
     TestMountVol(CPath[1]);
     if not RewriteTxt(Rprt,PrintCtrl) then
     begin SetMsgPar(CPath);WrLLF10Msg(700+HandleError);
           PrintView:=false;exit end;
     if Times>1 then begin writeln(Rprt,'.ti ',Times:1);Times:=1 end;
     if Pl<>72 then writeln(Rprt,'.pl ',Pl);
     RewriteRprt:=true;
  end;

var L:LvDescrPtr; er:ExitRecord; s:StringPtr; i:word; frst,isLPT1:boolean;
    Times:word; ex,b:boolean;  BD,RFb:BlkDPtr; md:LockMode;
label 0,1,2,3;
begin
  if SelQuest then with IDA[1]^ do begin
    CFile:=Scan^.FD; if not PromptFilter('',Bool,s) then begin
    PrintView:=false; exit end end;
  if PgeLimitZ<>nil then PgeLimit:=RunInt(PgeLimitZ) else PgeLimit:=spec.AutoRprtLimit;
  if PgeSizeZ<>nil then PgeSize:=RunInt(PgeSizeZ) else
    PgeSize:=spec.AutoRprtLimit+spec.CpLines;
  if PgeSize<2 then PgeSize:=2;
  if (PgeLimit>PgeSize) or (PgeLimit=0) then PgeLimit:=PgeSize-1;
  if not RewriteRprt(RO,PgeSize,Times,isLPT1) then exit;
  MarkStore2(Store2Ptr); ex:=true;
  PushProcStk; NewExit(Ovr,er); goto 3;
  OpenInp;
  MergOpGroup.Group:=1.0; frst:=true; NLinesOutp:=0; PrintDH:=2;
0:RunMsgOn('R',NRecsAll); RecCount:=0;
  for i:=1 to MaxIi do begin
    if frst then frst:=false else IDA[i]^.Scan^.SeekRec(0); ReadInpFile(IDA[I]) end;
  RprtPage:=1; RprtLine:=1; SetPage:=false; FirstLines:=true; WasDot:=false;
  WasFF2:=False; NoFF:=false; LineLenLst:=0; FrstBlk:=True; ResetY;
  L:=LstLvM; RFb:=LstLvM^.Ft;
  ZeroSumFlds(L); GetMinKey; ZeroCount; MoveFrstRecs;
  if RprtHd<>nil then begin
    if RprtHd^.FF1 then FormFeed; RprtPage:=1;
    PrintBlkChn(RprtHd,false,false);
    TruncLine; if WasFF2 then FormFeed;
    if SetPage then begin SetPage:=false; RprtPage:=PageNo end
    else RprtPage:=1 end;
  if NEof=MaxIi then goto 2;
1:if WasFF2 then PrintPageHd; Headings(L,nil);
  MergeProc;
  MoveMFlds(NewMFlds,OldMFlds); GetMinKey;
  if NEof=MaxIi then begin
    if FrstLvM<>LstLvM then Footings(FrstLvM,LstLvM^.ChainBack);
2:  WasFF2:=false;TruncLine;PrintBlkChn(RFb,false,true);b:=WasFF2;
    if (PageFt<>nil) and not PageFt^.NotAtEnd then PrintPageFt;
    TruncLine; RunMsgOff;
    if Times>1{only LPT1} then begin
      dec(Times); write(Rprt,^l); goto 0 end;
    if b then FormFeed;
    ex:=false;
3:  RestoreExit(er);
    if PrintView and (NLinesOutp=0) and (LineLenLst=0) then begin
      RdMsg(159); writeln(Rprt); write(Rprt,MsgLine) end;
    Close(Rprt); if isLPT1 then ClosePrinter(0);
    CloseInp; PopProcStk;
    if ex then begin RunMsgOff; if not WasLPTCancel then GoExit end;
    exit end;
  L:=GetDifLevel;
  Footings(FrstLvM,L); if WasFF2 then PrintPageFt;
  ZeroSumFlds(L); ZeroCount;
  MoveFrstRecs; MergOpGroup.Group:=MergOpGroup.Group+1.0;
  goto 1;
end;

end.