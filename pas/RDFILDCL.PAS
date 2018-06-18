{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit rdfildcl;

interface

uses dos,access,rdrun;

function RdFldDescr(const Name:string;Stored:boolean):FieldDPtr;
function RdChkD(Low:word):ChkDPtr;
procedure RdChkDsFromPos(FD:FileDPtr;var C:ChkDPtr);
procedure RdByteList(var s:string);
procedure RdByteListInStore;
procedure RdBegViewDcl(EO:EditOptPtr);
function RdUserView(ViewName:string;EO:EditOptPtr):boolean;
function RdFileD(const FileName:string;FDTyp:char;Ext:ExtStr):pointer;


implementation

uses base,oaccess,compile,runfrml,rdproc,runproj;

 var HasTT:boolean;
function RdFldDescr(const Name:string;Stored:boolean):FieldDPtr;
const TabF:array[0..18]of byte=(0,1,1,2,2,3,3,4,4,4,5,5,6,6,6,7,7,8,8);
var F:FieldDPtr; S:StringPtr; L,M,NBytes:word; Flg:byte; Typ,FrmlTyp,c:char;
    i,n,n1:word; ss:string;
label 1,2;
begin
  F:=GetZStore(pred(SizeOf(F^))+Length(Name)); RdFldDescr:=F;
  Move(Name,F^.Name,length(Name)+1);
  if Stored then Flg:=f_Stored else Flg:=0; Accept(':');
  if (Lexem<>_identifier) or (length(LexWord)>1) then Error(10);
  Typ:=LexWord[1]; RdLex; FrmlTyp:='S'; M:=0;
  if Typ in ['N','F'] then begin Accept(','); L:=RdInteger end;
  case Typ of
    'N':begin NBytes:=(L+1)div 2;
        if CurrChar='L' then begin RdLex; M:=LeftJust end end;
    'F':begin
        if Lexem=',' then begin inc(Flg,f_Comma); RdLex end else Accept('.');
        M:=RdInteger; if (M>15) or (L+M>18) then OldError(3);
        NBytes:=TabF[L+M]; if M=0 then inc(L) else inc(L,M+2);
        FrmlTyp:='R' end;
    'R':begin NBytes:=6; FrmlTyp:='R'; L:=17; M:=5 end;
    'A':begin Accept(','); if not Stored or (Lexem<>_quotedstr) then begin
          L:=RdInteger; if L>255 then Error(3);
          if CurrChar='R' then RdLex else M:=LeftJust end
        else begin S:=RdStrConst; L:=0; c:='?'; n:=0;
          for i:=1 to length(S^) do case S^[i] of
            '[': if c='?' then c:='[' else goto 1;
            ']': if c='[' then c:='?' else goto 1;
            '(': if c='?' then begin c:='('; n1:=0; n:=0 end else goto 1;
            ')': if (c='(') and (n1>0) and (n>0) then begin
                   c:='?'; inc(L,maxw(n1,n)) end else goto 1;
            '|': if (c='(') and (n1>0) then begin n:=maxw(n1,n); n1:=0 end else goto 1;
            else if c='(' then inc(n1) else inc(L) end;
          inc(Flg,f_Mask); M:=LeftJust; if c<>'?' then
1:        Error(171) end;
        NBytes:=L; goto 2 end;
    'D':begin ss[0]:=#0;
        if Lexem=',' then begin RdLex; ss:=LexWord; Accept(_quotedstr) end;
        if ss[0]=#0 then ss:='DD.MM.YY'; S:=StoreStr(ss);
        FrmlTyp:='R'; NBytes:=sizeof(real); L:=length(S^); inc(Flg,f_Mask) end;
    'B':begin L:=1; NBytes:=1; FrmlTyp:='B' end;
    'T':begin if Lexem=',' then begin RdLex; L:=RdInteger+2 end
        else L:=1; NBytes:=sizeof(longint); HasTT:=true;
2:      if Stored and (Lexem='!') then begin RdLex; inc(Flg,f_Encryp) end end;
    else OldError(10) end;
  if NBytes=0 then OldError(113);
  if (L>TxtCols-1) and (Typ<>'A') then OldError(3);
  F^.Typ:=Typ; F^.FrmlTyp:=FrmlTyp; F^.L:=L; F^.M:=M; F^.NBytes:=NBytes;
  F^.Flg:=Flg;
end;


function RdChkD(Low:word):ChkDPtr;
var Upper,N:word; C:ChkDPtr; Z:FrmlPtr;
begin
  C:=GetZStore(sizeof(C^)); RdChkD:=C; C^.Bool:=RdBool; Upper:=OldErrPos;
  if Lexem='?' then begin RdLex; C^.Warning:=true end;
  if Lexem=':' then begin RdLex; C^.TxtZ:=RdStrFrml end
  else begin
    N:=Upper-Low; if N>SizeOf(ScreenStr) then N:=pred(SizeOf(ScreenStr));
    Z:=GetOp(_const,N+1); C^.TxtZ:=Z;
    Z^.S[0]:=char(N); Move(InpArrPtr^[Low],Z^.S[1],N) end;
  if Lexem=',' then begin RdLex; C^.HelpName:=RdHelpName end;
end;
procedure RdChkDChain(var CRoot:ChkDPtr);
var Low:word;
label 1;
begin
  SkipBlank(false); Low:=CurrPos;RdLex;
1:ChainLast(CRoot,RdChkD(Low)); if Lexem=';' then begin
    SkipBlank(false); Low:=CurrPos; RdLex;
    if not(Lexem in ['#',^z]) then goto 1 end;
end;
procedure RdChkDsFromPos(FD:FileDPtr;var C:ChkDPtr);
var cf:FileDPtr;
begin
  if FD^.OrigFD<>nil then RdChkDsFromPos(FD^.OrigFD,C);
  if FD^.ChptPos.R=nil then exit; if FD^.TxtPosUDLI=0 then exit;
  ResetCompilePars; SetInpTTxtPos(FD); RdLex;
  while not(ForwChar in ['L',^z]) do repeat RdLex until Lexem in [^z,'#'];
  if Lexem=^z then exit; RdLex;
  cf:=CFile; CFile:=FD; RdChkDChain(C); CFile:=cf;
end;


procedure RdBegViewDcl(EO:EditOptPtr);
var neg,all:boolean; fl,fl1:FieldList; f:FieldDPtr;
label 1;
begin
  if (Lexem in [_identifier,'[']) then begin
    RdChptName('E',EO^.FormPos,true); exit end;
  neg:=false; all:=false; fl1:=nil; EO^.UserSelFlds:=false;
  if Lexem='^' then begin RdLex; neg:=true end; Accept('(');
  if Lexem=_identifier then RdFldList(fl1) else neg:=true;
1:case Lexem of
   '!':if neg then begin RdLex; all:=true; goto 1 end;
   '?':begin RdLex; EO^.UserSelFlds:=true; goto 1 end end;
  Accept(')');
  if not neg then begin EO^.Flds:=fl1; exit end;
  EO^.Flds:=nil; f:=CFile^.FldD; while f<>nil do begin
    if ((f^.Flg and f_Stored<>0) or all) and not FieldInList(f,fl1) then begin
      fl:=GetStore(SizeOf(fl^)); fl^.FldD:=f; ChainLast(EO^.Flds,fl) end;
    f:=f^.Chain end;
  if EO^.Flds=nil then OldError(117);
end;

procedure RdByteList(var s:string);
var i,i1,i2,l:integer;
label 1;
begin
  Accept('('); l:=0;
1:i1:=RdInteger; i2:=i1; if i1<0 then OldError(133);
  if Lexem=_subrange then begin
    RdLex; i2:=RdInteger; if i2<i1 then OldError(133) end;
  if (i2>255) or (l+i2-i1>=255) then OldError(133);
  for i:=i1 to i2 do begin inc(l); s[l]:=char(i) end;
  if Lexem=',' then begin RdLex; goto 1 end;
  s[0]:=char(l); Accept(')');
end;
procedure RdByteListInStore;
var s:string;
begin
  RdByteList(s); StoreStr(s);
end;
function RdUserView(ViewName:string;EO:EditOptPtr):boolean;
var found,Fin,FVA:boolean; p,p2:pointer; K:KeyDPtr; EOD: EditOpt; fd:FileDPtr;
label 1,2,3;
begin
  found:=false; fd:=CFile;
  MarkStore(p); MarkStore2(p2); move(EO^,EOD,sizeof(EOD));
1:if fd^.TxtPosUDLI=0 then goto 3;
  ResetCompilePars; SetInpTTxtPos(fd); RdLex;
  if (Lexem<>'#') or (ForwChar<>'U') then goto 3;
  RdLex;{#} RdLex;{U}
2:ReleaseStore(p); move(EOD,EO^,sizeof(EO^));
  if EquUpcase(ViewName) then found:=true;
  EO^.ViewName:=StoreStr(LexWord); RdLex; {'('}
  repeat RdLex until Lexem in [')',^z];  RdLex; RdLex;{'):'}
  K:=RdViewKey;
  if K<>nil then begin RdLex;{','} EO^.ViewKey:=K end;
  RdBegViewDcl(EO); while Lexem=',' do begin
   FVA:=FileVarsAllowed; FileVarsAllowed:=false;
   if not RdViewOpt(EO) then Error(44);
   FileVarsAllowed:=FVA end;
  if not found and (Lexem=';') then begin
    RdLex; if not (Lexem in ['#',^z]) then goto 2  end;
3:ReleaseStore2(p2);
  fd:=fd^.OrigFD; if (fd<>nil) and not found then goto 1;
  RdUserView:=found;
end;


var  issql:boolean;

procedure TestUserView;
procedure TestDupl(FD:FileDPtr);
var S:StringList;
begin
  S:=FD^.ViewNames; while S<>nil do begin
    if EquUpcase(S^.S) then Error(26); S:=S^.Chain end;
end;
var p:pointer; K:KeyDPtr; EO:EditOptPtr; S:StringList; FD:FileDPtr;
label 1;
begin
  RdLex;
1:TestIdentif;
  TestDupl(CFile); FD:=FileDRoot;
  while FD<>nil do begin TestDupl(FD);FD:=FD^.Chain end;
  S:=GetStore(length(LexWord)+5); S^.S:=LexWord; ChainLast(CFile^.ViewNames,S);
  RdLex; RdByteListInStore; Accept(':'); MarkStore(p);
  EO:=GetEditOpt;
  K:=RdViewKey; if K<>nil then begin Accept(','); EO^.ViewKey:=K end;
  RdBegViewDcl(EO); while Lexem=',' do if not RdViewOpt(EO) then Error(44);
  ReleaseStore(p);
  if Lexem=';' then begin
    RdLex;if not (Lexem in ['#',^z]) then goto 1 end;
end;

procedure RdFieldDList(Stored:boolean);
var Name:ScreenStr; F:FieldDPtr; FTyp:char; Z:FrmlPtr;
label 1;
begin
1:TestIdentif; Name:=LexWord; F:=FindFldName(CFile); if F<>nil then Error(26);
  RdLex;if not Stored then begin Accept(_assign);Z:=RdFrml(FTyp) end;
  F:=RdFldDescr(Name,Stored);
  if (CFile^.Typ='D') and Stored and (F^.Typ in ['R','N']) then OldError(86);
  ChainLast(CFile^.FldD,F);
  if Stored then begin
    if CFile^.Typ='8' then if (F^.Typ in ['R','B','T']) then OldError(35)
             else if (F^.Typ='F') and (F^.NBytes>5) then OldError(36) end
  else begin F^.Frml:=Z; if FTyp<>F^.FrmlTyp then OldError(12) end;
  if Lexem=';' then begin RdLex;if not(Lexem in ['#',^z]) then goto 1 end;
end;


function RdFileD(const FileName:string;FDTyp:char;Ext:ExtStr):pointer;
procedure RdKeyD;
procedure CheckDuplAlias(var Name:ScreenStr);
procedure LookForK(var Name:ScreenStr;F:FileDPtr);
var K:KeyDPtr;
begin
  if SEquUpcase(F^.Name,Name) then Error(26);
  K:=F^.Keys; while K<>nil do begin
    if SEquUpcase(K^.Alias^,Name) then Error(26);
    K:=K^.Chain end;
end;
var F:FileDPtr;
begin
  if (CFile^.Typ<>'X') {$ifdef FandSQL} and not CFile^.typSQLFile {$endif} then Error(108);
  LookForK(Name,CFile); F:=FileDRoot;
  while F<>nil do begin LookForK(Name,F); F:=F^.Chain end;
end;
function RdFileOrAlias1(F:FileDPtr):KeyDPtr;
var k:KeyDPtr;
label 1;
begin
  k:=F^.Keys; if not EquUpcase(F^.Name) then while k<>nil do begin
    if EquUpcase(k^.Alias^) then goto 1; k:=k^.Chain end;
1: RdFileOrAlias1:=k;
end;
procedure RdFileOrAlias(var FD:FileDPtr;var KD:KeyDPtr);
var f:FileDPtr; r:RdbDPtr; k:KeyDPtr;
label 1;
begin
  TestIdentif; f:=CFile; k:=RdFileOrAlias1(f); if k<>nil then goto 1;
  r:=CRdb; while r<>nil do begin
    f:=r^.FD; while f<>nil do begin
      k:=RdFileOrAlias1(f); if k<>nil then goto 1; f:=f^.Chain end;
    r:=r^.ChainBack end;
  Error(9);
1:if k=nil then Error(24); RdLex; FD:=f; KD:=k;
end;
var F,F2:FieldDPtr; KF,Arg:KeyFldDPtr; FD:FileDPtr; L:LinkDPtr; K,K1:KeyDPtr;
    Name:ScreenStr; N:word;
label 1,2,3,4,5,6;
begin RdLex;
  if Lexem='@' then
       begin if (CFile^.Keys<>nil) or CFile^.IsParFile then Error(26); RdLex;
        if Lexem='@' then begin RdLex;CFile^.IsParFile:=True end
        else begin Name:='';
1:         K:=GetZStore(SizeOf(K^));
           K1:=KeyDPtr(@CFile^.Keys);N:=1;
           while K1^.Chain<>nil do begin K1:=K1^.Chain;inc(N) end;
           K1^.Chain:=K; K^.Alias:=StoreStr(Name);
           K^.Intervaltest:=false; K^.Duplic:=false;
           if Lexem=_le then begin K^.Intervaltest:=true;RdLex end
           else if Lexem='*' then begin
             {$ifdef FandSQL} if CFile^.typSQLFile then Error(155); {$endif}
             K^.Duplic:=true; RdLex end;
           K^.IndexRoot:=N; K^.IndexLen:=RdKFList(K^.KFlds,CFile);
           if K^.IndexLen>MaxIndexLen then OldError(105);
           end;
        goto 6;
       end;
2:   TestIdentif;Name:=LexWord;SkipBlank(false);
     if ForwChar='(' then
        begin RdLex;RdLex;if Lexem='@' then
                begin CheckDuplAlias(Name);RdLex;Accept(')');goto 1 end;
              RdFileOrAlias(FD,K);Accept(')') end
     else RdFileOrAlias(FD,K);
     L:=FindLD(Name);if L<>nil then OldError(26);
     L:=GetZStore(sizeof(L^)-1+length(Name)); L^.Chain:=LinkDRoot;LinkDRoot:=L;
     Move(Name,L^.RoleName,length(Name)+1);
     L^.FromFD:=CFile;L^.ToFD:=FD;L^.ToKey:=K;
     if Lexem='!' then begin
       if (CFile^.Typ<>'X'){$ifdef FandSQL}and not CFile^.typSQLFile{$endif} then Error(108);
       if K^.Duplic then Error(153);  RdLex; L^.MemberRef:=1;
       if Lexem='!' then begin RdLex; L^.MemberRef:=2 end end;
     Arg:=KeyFldDPtr(@L^.Args); KF:=K^.KFlds;
3:   F:=RdFldName(CFile); if F^.Typ='T' then OldError(84);
     Arg^.Chain:=GetZStore(SizeOf(Arg^)); Arg:=Arg^.Chain;
     with Arg^ do begin FldD:=F; CompLex:=KF^.CompLex; Descend:=KF^.Descend end;
     F2:=KF^.FldD;
     if (F^.Typ<>F2^.Typ) or (F^.Typ<>'D') and (F^.L<>F2^.L) or
        (F^.Typ='F') and (F^.M<>F2^.M) then OldError(12);
     KF:=KF^.Chain; if KF<>nil then begin Accept(',');goto 3 end;
6:   if Lexem=';' then
       begin RdLex;if not(Lexem in ['#',^z]) then goto 2 end;
  end;

procedure SetLDIndexRoot(L,L2:LinkDPtr);
var K:KeyDPtr; Arg,KF:KeyFldDPtr; cmptd:boolean;
label 1,2;
begin
  L:=LinkDRoot; while L<>L2 do begin   { find key with equal beginning }
    if CFile^.Typ='X' then begin
      K:=CFile^.Keys; while K<>nil do begin
        KF:=K^.KFlds; Arg:=L^.Args; cmptd:=false;
        while Arg<>nil do begin
          if (KF=nil) or (Arg^.FldD<>KF^.FldD) or (Arg^.CompLex<>KF^.CompLex)
             or (Arg^.Descend<>KF^.Descend) then goto 1;
          if Arg^.FldD^.Flg and f_Stored=0 then cmptd:=true;
          Arg:=Arg^.Chain; KF:=KF^.Chain end;
        L^.IndexRoot:=K^.IndexRoot; goto 2;
1:      K:=K^.Chain end end;
2:  if (L^.MemberRef<>0) and ((L^.IndexRoot=0) or cmptd) then begin
      SetMsgPar(L^.RoleName); OldError(152) end;
    L:=L^.Chain; inc(CFile^.nLDs) end;
end;
procedure TestDepend;
  var ZBool,Z:FrmlPtr; F:FieldDPtr; FTyp:char; p:pointer;
  label 1,2;
  begin RdLex; MarkStore(p);
      1:Accept('(');ZBool:=RdBool;Accept(')');
      2:F:=RdFldName(CFile);if F^.Flg and f_Stored=0 then OldError(14);
        Accept(_assign);Z:=RdFrml(FTyp);if F^.FrmlTyp<>FTyp then Error(12);
        if Lexem=';' then
        begin RdLex;if not(Lexem in ['#',^z]) then
              begin if Lexem='(' then goto 1 else goto 2 end end;
      ReleaseStore(p);
  end;
procedure RdImpl(var IDRoot:ImplDPtr);
  var Z:FrmlPtr; F:FieldDPtr; FTyp:char; ID:ImplDPtr;
  label 1;
  begin RdLex;
      1:F:=RdFldName(CFile);if F^.Flg and f_Stored=0 then OldError(14);
        Accept(_assign);Z:=RdFrml(FTyp);
        if FTyp<>F^.FrmlTyp then OldError(12);
        ID:=GetStore(sizeof(ID^)); ID^.FldD:=F; ID^.Frml:=Z;
        ChainLast(IDRoot,ID);
        if Lexem=';' then
           begin RdLex;if not(Lexem in ['#',^z]) then goto 1 end;
  end;
procedure RdKumul;
procedure RdRoleField(AD:AddDPtr);
  var F:FieldDPtr;
  begin if not IsRoleName(true,AD^.File2,AD^.LD) then Error(9);Accept('.');
        F:=RdFldName(AD^.File2); AD^.Field:=F;
        if F^.Flg and f_Stored=0 then OldError(14);
        if IsKeyArg(F, AD^.File2) then OldError(135) end;
procedure RdImper(AD:AddDPtr);
var KF:KeyFldDPtr;
begin
  if Lexem='!' then begin
    RdLex; AD^.Create:=1; if AD^.LD<>nil then begin
      KF:=AD^.LD^.ToKey^.KFlds; while KF<>nil do begin
        if KF^.FldD^.Flg and f_Stored=0 then OldError(148); KF:=KF^.Chain end end;
    if Lexem='!' then begin RdLex; AD^.Create:=2 end end;
end;
procedure RdAssign(AD:AddDPtr);
  var Z:FrmlPtr; FTyp:char;
  begin {$ifdef FandSQL}if AD^.File2^.typSQLFile then Error(155);{$endif}
        Accept(_assign); AD^.Assign:=true; AD^.Frml:=RdFrml(FTyp);
        if FTyp<>AD^.Field^.FrmlTyp then OldError(12) end;
  var AD:AddDPtr; Low:word; CF:FileDPtr;
  label 1;
  begin RdLex; AD:=AddDPtr(@CFile^.Add);
      1:AD^.Chain:=GetZStore(sizeof(AddD)); AD:=AD^.Chain;
        if IsKeyWord('IF') then
           begin AD^.Bool:=RdBool; AcceptKeyWord('THEN'); RdRoleField(AD);
                 RdImper(AD);RdAssign(AD) end
        else begin RdRoleField(AD);
           if Lexem='(' then
           begin Low:=CurrPos;RdLex;CF:=CFile;CFile:=AD^.File2;
                 AD^.Chk:=RdChkD(Low);CFile:=CF;Accept(')') end;
           RdImper(AD);
           if (AD^.Chk=nil) and (Lexem=_assign) then RdAssign(AD)
           else begin Accept(_addass); AD^.Assign:=false;
              TestReal(AD^.Field^.FrmlTyp); AD^.Frml:=RdRealFrml end end;
        if Lexem=';' then
           begin RdLex;if not(Lexem in ['#',^z]) then goto 1 end;
  end;
procedure SetHCatTyp;
  begin with CFile^ do begin
    Handle:=$FF; Typ:=FDTyp; CatIRec:=GetCatIRec(Name,Typ='0'{multilevel});
    {$ifdef FandSQL} typSQLFile:=issql; SetIsSQLFile; {$endif}
  end end;
procedure GetTFileD;
begin with CFile^ do begin
  if not HasTT and (TF=nil) then exit; if TF=nil then TF:=GetZStore(sizeof(TFile));
  TF^.Handle:=$FF; if FDTyp='D' then TF^.Format:=DbtFormat;
end end;
procedure GetXFileD;
begin with CFile^ do
  if Typ<>'X' then begin if XF<>nil then OldError(104); end
  else begin if XF=nil then XF:=GetZStore(sizeof(XFile)); XF^.Handle:=$FF end;
end;
procedure CallRdFDSegment(FD:FileDPtr);
var rdb,r:RdbDPtr; cr:pointer; cf:FileDPtr; b:boolean; i:word; pos:longint;
begin
  if Lexem<>^z then Accept(';');
  rdb:=CRdb; cr:=CRecPtr; r:=FD^.ChptPos.R;
  if (r=nil) or FD^.IsDynFile then OldError(106);
  CRdb:=r; i:=FD^.ChptPos.IRec;
  CFile:=CRdb^.FD; CRecPtr:=CFile^.RecPtr; ReadRec(i);
  pos:=_t(ChptOldTxt); if pos<=0 then Error(25);
  b:=RdFDSegment(i,pos);
  cf:=CFile; CRdb:=rdb; if InpRdbPos.IRec<>0 then begin
    CFile:=rdb^.FD; ReadRec(InpRdbPos.IRec); CFile:=cf end;
  CRecPtr:=cr; if not b then Error(25);
  CFile^.OrigFD:=FD; CFile^.TxtPosUDLI:=0;
end;
function OrigInp:CompInpDPtr;
var i:CompInpDPtr;
begin
  i:=CompInpDPtr(@PrevCompInp); while i^.ChainBack<>nil do i:=i^.ChainBack;
  OrigInp:=i;
end;

                                   {  RdFileD - body }
const JournalFlds:string[64]=
     'Upd:A,1;RecNr:F,8.0;User:F,4.0;TimeStamp:D,''DD.MM.YYYY hh:mm:ss''';
var FD:FileDPtr; F,F2:FieldDPtr; p:pointer; C:ChkDPtr; LDOld:LinkDPtr;
    n,i:integer; isHlp:boolean; K:KeyDPtr; Prefix,s:string[32];
    li:LiRootsPtr;
label 1,2;
begin
  ResetCompilePars; RdLex;
  issql:=SEquUpcase(ext,'.SQL'); isHlp:=SEquUpcase(ext,'.HLP');
  if IsKeyWord('JOURNALOF') then begin
    FD:=RdFileName; if Lexem=';' then RdLex; SetMsgPar(FileName);
    if FDTyp<>'6' then OldError(103);
    if Lexem<>^z then Error(40); {$ifdef FandSQL}
    if issql or FD^.typSQLFile then OldError(155);                 {$endif}
    LDOld:=LinkDRoot; CallRdFDSegment(FD); LinkDRoot:=LDOld;
    F:=CFile^.FldD; fillchar(CFile^,sizeof(FileD),0); CFile^.Name:=FileName;
    CFile^.IsJournal:=true; SetHCatTyp; CFile^.ChptPos:=OrigInp^.InpRdbPos;
    SetInpStr(JournalFlds); RdLex; RdFieldDList(true);
    F2:=LastInChain(CFile^.FldD); while F<>nil do begin
      if F^.Flg and f_Stored<>0 then begin
        F2^.Chain:=F; F2:=F; if F^.Typ='T' then with F^ do begin
          FrmlTyp:='R'; Typ:='F'; L:=10; Flg:=Flg and not f_Encryp end end;
      F:=F^.Chain end;
    F2^.Chain:=nil; CompileRecLen; ChainLast(FileDRoot,CFile);
    MarkStore(p); goto 1 end;
  if IsKeyWord('LIKE') then begin
    Prefix:=FileName; FD:=RdFileName; if Lexem='(' then begin
      RdLex; TestIdentif; Prefix:=LexWord; RdLex; Accept(')') end;
    CallRdFDSegment(FD); CFile^.IsHlpFile:=false;
    if not(FDTyp in ['6','X']) or not(CFile^.Typ in ['6','X']) then OldError(106);
    K:=CFile^.Keys; while K<>nil do begin
      if K^.Alias^<>'' then begin s:=K^.Alias^; i:=pos('_',s);
        if i<>0 then s:=copy(s,i+1,255); s:=Prefix+'_'+s; K^.Alias:=StoreStr(s) end;
      K:=K^.Chain end end
  else begin
    AlignLongStr; GetStore(2); CFile:=GetZStore(sizeof(FileD)) end;
  CFile^.Name:=FileName; SetHCatTyp; HasTT:=false;
  if (CFile^.OrigFD=nil) or not (Lexem in [^z,'#',']']) then RdFieldDList(True);
  GetTFileD; LDOld:=LinkDRoot;
  CFile^.ChptPos:=OrigInp^.InpRdbPos;
  if isHlp then begin
    F:=CFile^.FldD; F2:=F^.Chain;
    if (F^.Typ<>'A') or (F2=nil) or (F2^.Typ<>'T') or (F2^.Chain<>nil) then OldError(128);
    CFile^.IsHlpFile:=true end;
2:if (Lexem='#') and (ForwChar='C') then begin
    RdLex;RdLex; RdFieldDList(false); goto 2 end;
  if (Lexem='#') and (ForwChar='K') then begin
    RdLex; RdKeyD; goto 2 end;
  if issql and (CFile^.Keys<>nil) then CFile^.Typ:='X';
  GetXFileD; CompileRecLen; SetLDIndexRoot(LinkDRoot,LDOld);
  if (CFile^.Typ='X') and (CFile^.Keys=nil) then Error(107);
  if (Lexem='#') and (ForwChar='A') then begin RdLex;RdKumul end;
  ChainLast(FileDRoot,CFile);
  if Ext='$'{compile from text at run time} then begin
    CFile^.IsDynFile:=true; CFile^.ChptPos.R:=CRdb; MarkStore(p); goto 1 end;
  if Lexem<>^z then CFile^.TxtPosUDLI:=OrigInp^.CurrPos-1;
  if (Lexem='#') and (ForwChar='U') then begin RdLex;TestUserView end;
  MarkStore(p);
  li:=GetZStore(sizeof(LiRoots)); CFile^.LiOfs:=AbsAdr(li)-AbsAdr(CFile);
  if (Lexem='#') and (ForwChar='D') then begin RdLex;TestDepend end;
  if (Lexem='#') and (ForwChar='L') then begin RdLex;RdChkDChain(Li^.Chks) end;
  if (Lexem='#') and (ForwChar='I') then begin RdLex;RdImpl(Li^.Impls) end;
  if PtrRec(InpRdbPos.R).Seg=0{compiled from string} then begin
    CFile^.LiOfs:=0; ReleaseStore(p) end;
  if Lexem<>^z then Error(66);
1:RdFileD:=p;
end;

end.