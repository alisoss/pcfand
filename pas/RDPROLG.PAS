{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit rdprolg;

interface

uses base,access,rdrun,compile;

const
  _IntT=249;_RealT=250;_StrT=251;_LongStrT=252;_ListT=253;_VarT=254;
  _UnderscT=255; {term fun}
  _CioMaskOpt=$80;_PackInpOpt=$40;_FandCallOpt=$20;_BuildInOpt=$10;
  _DbaseOpt=$08;  { predicate Opt }
  _ConcatP=2; _NextLexP=4; _GetLexP=5;
  _FandFieldP=6; _FandFileP=7; _FandKeyP=8; _FandKeyFieldP=9;
  _FandLinkP=10; _FandLinkFieldP=11;
  _MemP=14; _LenP=15; _InvP=16; _AddP=17;_DelP=18;
  _UnionP=20; _MinusP=21; _InterP=22;
  _AbbrevP=25;
  _CallP=32;
type
  PFunDcl   = ^TFunDcl;
  PPTerm    = ^TPTerm;
  TDomainTyp=(_UndefD,_IntD,_RealD,_StrD,_LongStrD,_ListD,_FunD,_RedefD);
  PDomain   = ^TDomain;
  TDomain   = record
    Chain:word; Typ:TDomainTyp; case byte of
      0:(ElemDom:word; Name:string[1]);
      1:(OrigDom:word);
      2:(FunDcl:word);
  end;
  PConst    = ^TConst;
  TConst    = record
    Chain:word; Dom:word;{PDomain} Expr:word;{PPTerm} Name:string[1];
  end;
  TFunDcl   = record
    Chain:word; Name:word; Arity:byte; Arg:array[0..2] of word;
  end;
  TPTerm     = record
    case Fun:byte of
      0     : (Arity:byte; Arg:array [0..0] of word{PPTerm});
      1     : (Op:char; E1,E2,E3:word{PPTerm});  { _IntT.._ListT and Op<>_const }
      2     : (Op0:char; E:array[1..3]of word);
      _IntT : (Op1:char; II:integer);
      _RealT: (Op2:char; RR:float);
      _StrT,_LongStrT : (Op3:char; SS:string);
      _ListT: (Op4:char; Elem,Next:word{PPTerm});
      _VarT : (Idx:word; Bound:boolean);
  end;
  PTermList = ^TTermList;
  TTermList = record Chain:word{PTermList}; Elem:word{PPTerm} end;
  PVarDcl   = ^TVarDcl;
  TVarDcl   = record
    Chain:PVarDcl; Dom:word; Idx:integer; Bound,Used:boolean; Name:string[1];
  end;
  PPredicate= ^TPredicate;
  PDatabase = ^TDatabase;
  PWriteD  = ^TWriteD;
  TWriteD  = record
    Chain: word{PWriteD}; case IsString:boolean of
      true : (SS:string);
      false: (Idx:integer; Dom:word);
  end;
  TCommandTyp=(_PredC,_FailC,_CutC,_WriteC,_CompC,_AssertC,_RetractC,
               _SaveC,_ConsultC,_LoadLexC,_Trace,_SelfC,
               _AppPkC,_AppUnpkC,
               _ErrorC,_WaitC,_NotC,_AllC,_AutoC);
  PCommand  = ^TCommand;
  TCommand  = record
    Chain:word{PCommand}; case Code:TCommandTyp of
      _PredC,_AssertC,_RetractC,_AllC:
               (Arg:word{PTermList};
                Pred:word{PPredicate};
                InpMask,OutpMask,{only _CioMaskOpt}
                Elem{_MemP..:ListDom else PPTerm},Idx,Idx2:word; {_AllC}
                CompMask,KDOfs:word; ArgI:array[0..0] of byte{only FAND-file});
      _WriteC: (WrD:word{PWriteD}; NL:boolean);
      _ErrorC: (WrD1:word; MsgNr:word);
      _Trace : (TrcLevel:integer);
      _CompC : (Typ:TDomainTyp; CompOp:char; E1Idx,E2:word{PPTerm});
      _SaveC,_ConsultC,_LoadLexC:
               (DbPred:word;  { not _LoadLexC }
                FD:FileDPtr; FldD:FieldDPtr; Name:string[1]);
      _AppPkC,_AppUnpkC:
               (apIdx,apDom{Unpk},apTerm:word);
      _AutoC:  (Arg1:word{PTermList};  { like _PredC       autorecursion}
                iWrk,iOutp,nPairs:byte;
                Pair:array[0..5]of record iInp,iOutp:byte end;);
  end;
  PBranch   = ^TBranch;
  TBranch   = record
    Chain:word{PBranch};
    HeadIMask,HeadOMask:word; Head:word{PTermList}; Cmd:word{PCommand};
  end;
  PDbBranch = ^TDbBranch;
  TDbBranch = record
    Chain: PDbBranch;
    LL:word; A:array[1..1{LL}] of byte; {  for all Arguments }
  end;
  PFldList  = ^TFldList;
  TFldList  = record
    Chain:word{PFldList};FldD:FieldDPtr;
  end;
  PScanInf  = ^TScanInf;
  TScanInf  = record
    FD:FileDPtr; FL:word{PFldList}; Name:string[1];
  end;
  TPredicate= record
    Chain,ChainDb:word{PPredicate};
    Name:word{PString};
    Branch:PBranch{offset};{InstrPtr|ofs PScanInf|PDbBranch}
    InstSz,InpMask,LocVarSz{FAND-proc| _xxxP for buildIn}:word;
    Opt:byte; Arity:byte; Arg:array[0..2] of word{PDomain};
  end;
  TDatabase = record
    Chain:word{PDatabase}; Pred:word{PPredicate}; SOfs:word{LongStrPtr/saved/};
    Name:string[1];
  end;
  PProgRoots = ^TProgRoots;
  TProgRoots = record
    Domains,Consts,Predicates,Databases:word;
  end;


function ReadProlog(RecNr:word):word;

var
  _Sg:word;
implementation

uses runprolg, runproj;

const
  MaxPackedPredLen = 4000;

var
  VarDcls:PVarDcl; VarCount:integer;
  IntDom,RealDom,StrDom,LongStrDom,BoolDom,LexDom,LLexDom:word{PDomain};
  MemPred,LenPred,InvPred,AddPred,DelPred,
  UnionPred,MinusPred,InterPred,
  TxtPred:word{PPredicate};
  UnderscoreTerm:word{PPTerm};
  UnbdVarsInTerm,WasUnbd,WasOp:boolean;
  Roots:PProgRoots;
  PackedTermPtr:Pchar; PTPMaxOfs:word;
  ClausePreds:word;


procedure ChainLst(var Root;NewOfs:word); assembler;
asm  les di,Root;
@1:  cmp es:[di].word,0; je @2; mov di,es:[di]; mov es,_Sg; jmp @1;
@2:  mov ax,NewOfs; mov es:[di],ax;
end;
function OOfs(p:pointer):word;
begin
  OOfs:=((PtrRec(p).Seg-_Sg)shl 4) + PtrRec(p).Ofs;
end;
function GetZStor(Sz:word):word;
var p:pointer;
begin
  p:=GetZStore(Sz); GetZStor:=OOfs(p);
end;
function OPtr(Sg:word;p:pointer):pointer;
begin
  OPtr:=ptr(Sg,((PtrRec(p).Seg-Sg)shl 4) + PtrRec(p).Ofs);
end;
function StorStr(S:string):word; assembler;
asm  les di,S; xor ch,ch; mov cl,es:[di]; inc cx; push cx;
     push cx; call GetZStor;
     mov es,_Sg; mov di,ax; pop cx; push ds; lds si,S; cld; rep movsb;
     pop ds end;

{  L E X A N A L  =========================================================}

function IsCharUpper(C:char):boolean;
begin
  IsCharUpper:=(UpcCharTab[C]=C);
end;
function IsUpperIdentif:boolean;
begin
  IsUpperIdentif:=(Lexem=_identifier) and IsCharUpper(LexWord[1]);
end;
procedure RdLex;
var i:word;
label 1;
begin
  OldErrPos:=CurrPos; SkipBlank(false); ReadChar; Lexem:=CurrChar;
  case CurrChar of
    '''': begin Lexem:=_quotedstr; ReadChar; LexWord:='';
         while (CurrChar<>'''') or (ForwChar='''') do begin
           if CurrChar=^z then Error(17);
           if length(LexWord)=pred(sizeof(LexWord)) then Error(6);
           if CurrChar='''' then ReadChar
           else if CurrChar='\' then RdBackSlashCode;
           LexWord:=LexWord+CurrChar; ReadChar end end;
    ':': if ForwChar='-' then begin ReadChar; Lexem:=_assign end;
    '|': if ForwChar='|' then begin ReadChar; Lexem:=_or end;
    '&': if ForwChar='&' then begin ReadChar; Lexem:=_and end;
    else
    if IsLetter(CurrChar) then begin
      Lexem:=_identifier; LexWord[1]:=CurrChar; i:=1;
      while IsLetter(ForwChar) or IsDigit(ForwChar) do begin
        inc(i); if i>32 then Error(2); ReadChar; LexWord[i]:=CurrChar end;
      LexWord[0]:=char(i) end
    else if IsDigit(CurrChar) then begin
1:    Lexem:=_number; LexWord[1]:=CurrChar; i:=1;
      while IsDigit(ForwChar) do begin
        inc(i); if i>9 then Error(3); ReadChar; LexWord[i]:=CurrChar end;
      LexWord[0]:=char(i) end end;
end;
procedure TestIdentif;
begin
  if Lexem<>_identifier then Error(29);
end;
procedure Accept(X:char);
begin
  if Lexem<>X then
    if X=_assign then Error(506) else begin ExpChar:=X; Error(1) end;
  RdLex;
end;
function TestKeyWord(const s:string):boolean;
begin
  TestKeyWord:=(Lexem=_identifier) and (LexWord=s);
end;
function IsKeyWord(const s:string):boolean;
begin
  if TestKeyWord(s) then begin RdLex; IsKeyWord:=true end else IsKeyWord:=false;
end;
procedure AcceptKeyWord(const s:string);
begin
  if not IsKeyWord(s) then begin SetMsgPar(s); Error(33) end;
end;
function RdInteger:integer;
var i,j:integer;
begin
  if Lexem<>_number then Error(525); Val(LexWord,i,j); RdLex; RdInteger:=i;
end;

{  T D O M A I N  =========================================================}

function GetFunDclByName(D:word;var I:byte):PFunDcl;
var fd:word;
begin
  I:=0; fd:=PDomain(ptr(_Sg,d))^.FunDcl;
  while (fd<>0) and
    (StringPtr(ptr(_Sg,PFunDcl(ptr(_Sg,fd))^.Name))^<>LexWord) do begin
    fd:=PFunDcl(ptr(_Sg,fd))^.Chain; inc(I) end;
  if fd=0 then GetFunDclByName:=nil else GetFunDclByName:=ptr(_Sg,fd);
end;
function GetOrigDomain(D:word):word;
begin
  if D<>0 then while PDomain(ptr(_Sg,D))^.Typ=_RedefD do
    D:=PDomain(ptr(_Sg,D))^.OrigDom; GetOrigDomain:=D;
end;

{  T D A T A B A S E  =====================================================}

function FindDataBase(const S:string):word{PDatabase};
var db:PDatabase; dbofs:word absolute db;
label 1;
begin
  db:=ptr(_Sg,Roots^.Databases); while dbofs<>0 do begin
    if db^.Name=S then goto 1; dbofs:=db^.Chain end;
1:FindDataBase:=dbofs;
end;

{  T P R O G R A M  =======================================================}

function FindConst(D:word):word{PPTerm};
var p:PConst; pofs:word absolute p;
begin
  p:=ptr(_Sg,Roots^.Consts); FindConst:=0; while pofs<>0 do begin
    if (p^.Dom=D) and (p^.Name=LexWord) then begin FindConst:=p^.Expr; exit end;
    pofs:=p^.Chain end;
end;
function RdConst(D:word;var RT:word):boolean;
var tofs:word;
begin
  RdConst:=false; if Lexem=_identifier then begin
    tofs:=FindConst(D); if tofs<>0 then begin
      RdLex; RT:=tofs; RdConst:=true end end;
end;
function GetOp1(DOfs:word;Op:char;E1:word):word{PPTerm}; forward;
function FindVarDcl:PVarDcl;
var v:PVarDcl;
label 1;
begin
  v:=VarDcls; while v<>nil do begin
    if v^.Name=LexWord then goto 1; v:=v^.Chain end;
1:FindVarDcl:=v;
end;
function MakeVarDcl(DOfs:word;Idx:integer):PVarDcl;
var v:PVarDcl;
begin
  v:=Mem1.get(sizeof(TVarDcl)+length(LexWord)-1); ChainLast(VarDcls,v);
  v^.Dom:=DOfs; move(LexWord,v^.Name,length(LexWord)+1);
  if Idx<0 then begin v^.Idx:=VarCount; inc(varCount) end else v^.Idx:=Idx;
  MakeVarDcl:=v;
end;
function RdVar(DOfs:word;Kind,Idx:integer;var RT:word{PTerm or idx}):boolean;
var v:PVarDcl; bnd:boolean; { idx=-1 except solo variable in head }
    t:PPTerm; tofs:word absolute t; t1Ofs:word;
label 1;
begin
  if IsKeyWord('_') then begin
    if not (kind in [1,4]) then OldError(508); UnbdVarsInTerm:=true;
    WasUnbd:=true; RT:=UnderscoreTerm; RdVar:=true; exit end;
  RT:=0;
  if not IsUpperIdentif or (Kind=6{const dcl}) then begin
    if Kind=5 then Error(523); RdVar:=false; exit end;
  v:=FindVarDcl; if v=nil then v:=MakeVarDcl(DOfs,Idx)
  else if (v^.Dom<>DOfs) and
    not ((v^.Dom=StrDom) and (DOfs=LongStrDom)) and
    not ((v^.Dom=LongStrDom) and (DOfs=StrDom)) and
    not ((v^.Dom=IntDom) and (DOfs=RealDom)) and
    not ((v^.Dom=RealDom) and (Dofs=IntDom)) then begin RdLex;
1:    Set2MsgPar(PDomain(ptr(_Sg,v^.Dom))^.Name,PDomain(ptr(_Sg,DOfs))^.Name);
      OldError(507) end;
  RdLex; bnd:=v^.Bound; RdVar:=true;
  case Kind of { 1: head-i,call-o  2: call-i  3: head-o  5: unbound-o }
    1: begin v^.Bound:=true; if bnd then v^.Used:=true end;
    2: begin if not bnd then Error(509); v^.Used:=true end;
    3: v^.Used:=true;
    4: if bnd then v^.Used:=true
       else begin v^.Bound:=true; UnbdVarsInTerm:=true; WasUnbd:=true end;
    5: begin v^.Bound:=true; if bnd then OldError(523); RT:=v^.Idx; exit end end;
  if (Idx=-1) or (v^.Idx<>Idx) then begin
    PtrRec(t).Seg:=_Sg; tofs:=GetZStor(5);
    t^.Fun:=_VarT; t^.Idx:=v^.Idx; t^.Bound:=bnd;
    if (v^.Dom<>DOfs) then begin
      if not bnd then goto 1; tofs:=GetOp1(DOfs,_conv,tofs) end;
    RT:=tofs end;
end;

function RdTerm(DOfs:word;Kind:integer):word{PPTerm}; forward;
function RdAddExpr(DOfs:word; Kind:integer):word{PPTerm}; forward;

function DomFun(DOfs:word):word;
begin
  if DOfs=IntDom then DomFun:=_IntT else if DOfs=RealDom then DomFun:=_RealT
  else if DOfs=StrDom then DomFun:=_StrT else DomFun:=_LongStrT;
end;
function GetOp1(DOfs:word;Op:char;E1:word):word{PPTerm};
var t:PPTerm; tofs:word absolute t;
begin
  t:=ptr(_Sg,GetZStor(1+1+2)); t^.Fun:=DomFun(DOfs); t^.Op:=Op; WasOp:=true;
  t^.E1:=E1; GetOp1:=tofs;
end;
function GetOp2(DOfs:word;Op:char;E1,E2:word):word{PPTerm};
var t:PPTerm; tofs:word absolute t;
begin
  t:=ptr(_Sg,GetZStor(1+1+2*2)); t^.Fun:=DomFun(DOfs); t^.Op:=Op; WasOp:=true;
  t^.E1:=E1; t^.E2:=E2; GetOp2:=tofs;
end;
function GetFunOp(DOfs,ResDOfs:word;Op:char;const ArgTyp:string;Kind:integer):word{PPTerm};
var t:PPTerm; tofs:word absolute t; i,l,t1ofs:word;
begin
  if DOfs<>ResDOfs then OldError(510); l:=length(ArgTyp);
  if l>0 then Accept('(');
  t:=ptr(_Sg,GetZStor(1+1+2*l)); t^.Fun:=DomFun(DOfs); t^.Op:=Op; WasOp:=true;
  for i:=1 to l do begin
    if i>1 then Accept(','); case ArgTyp[i] of
      'l':t1Ofs:=RdAddExpr(LongStrDom,Kind);
      's':t1ofs:=RdAddExpr(StrDom,Kind);
      'i':t1Ofs:=RdAddExpr(IntDom,Kind);
      'c':begin if (Lexem<>_quotedstr) or (length(LexWord)<>1) then Error(560);
          t1ofs:=ord(LexWord[1]); RdLex end end;
    t^.E[i]:=t1ofs end;
  if l>0 then Accept(')'); GetFunOp:=tofs;
end;

function RdPrimExpr(DOfs:word;Kind:integer):word{PPTerm};
var t:PPTerm; tofs:word absolute t; op:char;
    minus:boolean; r:float; s:string[20]; i:integer; n:longint;
label 1,2;
begin
  PtrRec(t).Seg:=_Sg; case Lexem of
    '^': begin if DOfs<>IntDom then Error(510); op:=Lexem; RdLex;
         tofs:=GetOp1(DOfs,op,RdPrimExpr(DOfs,Kind)) end;
    '(': begin RdLex; tofs:=RdAddExpr(DOfs,Kind); Accept(')') end;
    _quotedstr: begin if (DOfs<>StrDom) and (DOfs<>LongStrDom) then Error(510);
         tofs:=GetZStor(1+1+1+length(LexWord));
         t^.Fun:=DomFun(DOfs); t^.Op:=_const; t^.SS:=LexWord; RdLex end;
    '$': if DOfs<>IntDom then Error(510) else begin
         i:=0; while ForwChar in ['0'..'9','a'..'f','A'..'F'] do begin
           inc(i); if i>4 then Error(3); ReadChar; s[i]:=CurrChar end;
         if i=0 then Error(504); s[0]:=char(i); n:=HexStrToLong(s); RdLex;
         goto 2 end;
    '-': begin RdLex; if Lexem<>_number then Error(525); minus:=true;
         goto 1 end;
    _number: begin  minus:=false;
1:       s:=LexWord; RdLex;
         if DOfs=IntDom then begin
           val(s,n,i); if minus then n:=-n;
2:         tofs:=GetZStor(1+1+sizeof(integer)); t^.Fun:=_IntT; t^.II:=n end
         else if DOfs=RealDom then begin
           if (Lexem='.') and IsDigit(ForwChar) then begin
             RdLex; s:=s+'.'+LexWord; RdLex end;
           val(s,r,i); if minus then r:=-r;
           tofs:=GetZStor(1+1+sizeof(float)); t^.Fun:=_RealT; t^.RR:=r end
         else OldError(510);
         t^.Op:=_const end;
    else if not RdVar(DOfs,Kind,-1,tofs) and not rdConst(DOfs,tofs) then
    if IsKeyWord('length') then tofs:=GetFunOp(DOfs,IntDom,_length,'s',Kind) else
    if IsKeyWord('pos') then tofs:=GetFunOp(DOfs,IntDom,_pos,'sl',Kind) else
    if IsKeyWord('min') then tofs:=GetFunOp(DOfs,IntDom,_min,'ii',Kind) else
    if IsKeyWord('max') then tofs:=GetFunOp(DOfs,IntDom,_max,'ii',Kind) else
    if IsKeyWord('val') then tofs:=GetFunOp(DOfs,IntDom,_val,'s',Kind) else
    if IsKeyWord('copy') then tofs:=GetFunOp(DOfs,StrDom,_copy,'sii',Kind) else
    if IsKeyWord('str') then tofs:=GetFunOp(DOfs,StrDom,_str,'i',Kind) else
    if IsKeyWord('repeatstr') then tofs:=GetFunOp(DOfs,StrDom,_repeatstr,'si',Kind) else
    if IsKeyword('leadchar') then tofs:=GetFunOp(DOfs,StrDom,_leadchar,'cs',Kind) else
    if IsKeyWord('trailchar') then tofs:=GetFunOp(DOfs,StrDom,_trailchar,'cs',Kind) else
    if IsKeyWord('maxrow') then tofs:=GetFunOp(DOfs,IntDom,_maxrow,'',Kind) else
    if IsKeyWord('maxcol') then tofs:=GetFunOp(DOfs,IntDom,_maxcol,'',Kind)
    else Error(511) end;
  RdPrimExpr:=tofs;
end;
function RdMultExpr(DOfs:word; Kind:integer):word{PPTerm};
var tofs:word; op:char;
begin
  tofs:=RdPrimExpr(DOfs,Kind);
  while (DOfs<>StrDom) and (DOfs<>LongStrDom) and ((Lexem in ['*','/']) or
    (Lexem in [_and,_or]) and (DOfs=IntDom)) do begin
    op:=Lexem; RdLex; tofs:=GetOp2(DOfs,op,tofs,RdPrimExpr(DOfs,Kind)) end;
  RdMultExpr:=tofs;
end;
function RdAddExpr(DOfs:word; Kind:integer):word{PPTerm};
var tofs:word; op:char;
begin
  tofs:=RdMultExpr(DOfs,Kind);
  while (Lexem='+') or (Lexem='-') and ((Dofs=IntDom) or (DOfs=RealDom)) do begin
    op:=Lexem; RdLex; tofs:=GetOp2(DOfs,op,tofs,RdMultExpr(DOfs,Kind)) end;
  RdAddExpr:=tofs;
end;
function RdListTerm(DOfs:word;Kind:integer):word{PPTerm};
var t:PPTerm; tofs:word absolute t; t1,tPrev:word;
label 1;
begin
  PtrRec(t).Seg:=_Sg;
  if not rdVar(DOfs,Kind,-1,tofs) and not rdConst(DOfs,tofs) then begin
    if Lexem<>'[' then Error(510); RdLex; tofs:=0;
    if Lexem=']' then RdLex else begin
1:    t1:=GetZStor(1+1+2*2); with PPTerm(ptr(_Sg,t1))^ do begin
        Fun:=_ListT; Op:=_const;
        Elem:=RdTerm(PDomain(ptr(_Sg,DOfs))^.ElemDom,Kind) end;
      if tofs=0 then tofs:=t1 else PPTerm(ptr(_Sg,tPrev))^.Next:=t1;
      tPrev:=t1; if Lexem=',' then begin RdLex; goto 1 end;
      if Lexem='|' then begin RdLex;
        if not rdVar(DOfs,Kind,-1,PPTerm(ptr(_Sg,tPrev))^.Next) then Error(511) end;
      Accept(']') end end;
  if Lexem='+' then begin
    t1:=tofs; tofs:=GetZStor(1+1+2*2); t^.Op:='+'; RdLex; t^.Fun:=_ListT;
    t^.E1:=t1; t^.E2:=RdListTerm(DOfs,Kind); WasOp:=true end;
  RdListTerm:=tofs;
end;
function RdTerm(DOfs:word;Kind:integer):word{PPTerm};
var t:PPTerm; tofs:word absolute t; f:PFunDcl; i,n:word; idx:byte; wo,wu:boolean;
label 1;
begin
  wo:=WasOp; wu:=WasUnbd; WasOp:=false; WasUnbd:=false;
  case PDomain(ptr(_Sg,DOfs))^.Typ of
    _IntD,_RealD,_StrD,_LongStrD:    tofs:=RdAddExpr(DOfs,Kind);
    _ListD:                          tofs:=RdListTerm(DOfs,Kind);
  else if not rdVar(DOfs,Kind,-1,tofs) and not rdConst(DOfs,tofs) then begin
    TestIdentif; f:=GetFunDclByName(DOfs,idx); if f=nil then Error(512);
    RdLex; n:=f^.Arity; t:=ptr(_Sg,GetZStor(1+1+2*n));
    t^.Fun:=idx; t^.Arity:=n; if n>0 then begin
      Accept('('); for i:=0 to n-1 do begin
        if i>0 then Accept(','); t^.Arg[i]:=RdTerm(f^.Arg[i],Kind) end;
      Accept(')') end end;
    goto 1 end;
  if WasOp then begin
    if WasUnbd then OldError(540); if Kind=6 then OldError(549) end;
1:RdTerm:=tofs; WasOp:=wo; WasUnbd:=wu;
end;

function MakeDomain(DTyp:TDomainTyp;const Nm:string):word;
var dofs:word;
begin
  dofs:=GetZStor(sizeof(TDomain)-1+length(Nm)); ChainLst(Roots^.Domains,dofs);
  with PDomain(ptr(_Sg,dofs))^ do begin Typ:=DTyp; move(Nm,Name,length(Nm)+1) end;
  MakeDomain:=dofs;
end;
function GetDomain(Create:boolean;Nm:string):word;
var d,d1,d2:PDomain; dofs:word absolute d; d1ofs:word absolute d1;
    d2ofs:word absolute d2;
begin
  d:=ptr(_Sg,Roots^.Domains);
  while (dofs<>0) and (d^.Name<>Nm) do dofs:=d^.Chain;
  if dofs=0 then
    if copy(Nm,1,2)='L_' then begin
      d1:=ptr(_Sg,GetOrigDomain(GetDomain(Create,copy(Nm,3,255))));
      if d1ofs=0 then Error(517); Nm:='L_'+d1^.Name;
      d:=ptr(_Sg,Roots^.Domains);
      while (dofs<>0) and (d^.Name<>Nm) do dofs:=d^.Chain;
      if dofs=0 then begin
        dofs:=MakeDomain(_ListD,Nm); d^.ElemDom:=d1ofs end
      end
    else if Create then begin
      if (length(Nm)=0) or not IsCharUpper(Nm[1]) then Error(514);
      dofs:=MakeDomain(_UndefD,Nm) end;
  GetDomain:=dofs;
end;
function RdDomain:word;{PDomain}
var dofs:word;
begin
  TestIdentif; dofs:=GetDomain(false,LexWord); if dofs=0 then Error(517);
  RdLex; RdDomain:=GetOrigDomain(dofs);
end;

procedure RdDomains;
var d:PDomain; dofs:word absolute d; fd:PFunDcl; fdofs:word absolute fd;
    d1:word; a:array[0..31] of word; nm,dofs2:word; n:byte; i:integer;
label 1,2,3,4;
begin
  PtrRec(d).Seg:=_Sg; PtrRec(fd).Seg:=_Sg;
1:TestIdentif; dofs:=GetDomain(true,LexWord);
  if d^.Typ<>_UndefD then Error(505);
  RdLex; while Lexem=',' do begin
    RdLex; d^.Typ:=_RedefD; TestIdentif; d^.OrigDom:=GetDomain(true,LexWord);
    dofs:=d^.OrigDom; if d^.Typ<>_UndefD then OldError(505); RdLex end;
  Accept('='); SkipBlank(false); TestIdentif;
  if IsCharUpper(LexWord[1]) then begin
    d1:=GetDomain(true,LexWord); if d1=dofs then Error(505); RdLex;
    d^.Typ:=_RedefD; d^.OrigDom:=d1; goto 4 end;
  d^.Typ:=_FunD;
2:if GetFunDclByName(dofs,n)<>nil then Error(505);
  if IsCharUpper(LexWord[1]) then Error(515);
  nm:=StorStr(LexWord); RdLex; n:=0;
  if Lexem='(' then begin RdLex;
3:  TestIdentif; a[n]:=GetDomain(true,LexWord); inc(n);
    RdLex; if Lexem=',' then begin RdLex; goto 3 end;
    Accept(')') end;
  fdofs:=GetZStor(sizeof(TFunDcl)-3*2+n*2); ChainLst(d^.FunDcl,fdofs);
  with fd^ do begin Name:=nm; Arity:=n; move(a,Arg,2*n) end;
  if Lexem=';' then begin RdLex; TestIdentif; goto 2 end;
4:if not (Lexem in [^z,'#']) then goto 1;
  dofs:=Roots^.Domains; while dofs<>0 do begin case d^.Typ of
      _UndefD:begin SetMsgPar(d^.Name); OldError(516) end;
      _FunD  :begin fdofs:=d^.FunDcl; while fdofs<>0 do begin
              for i:=1 to fd^.Arity do fd^.Arg[i-1]:=GetOrigDomain(fd^.Arg[i-1]);
              fdofs:=fd^.Chain end end end;
    dofs:=d^.Chain end;
end;

procedure RdConstants;
var p:PConst; pofs:word absolute p; dofs:word;
label 1,2;
begin
1:dofs:=RdDomain; Accept(':');
2:TestIdentif; if IsCharUpper(LexWord[1]) then Error(515);
  if FindConst(dofs)<>0 then Error(505);
  p:=ptr(_Sg,GetZStor(sizeof(TConst)-1+length(LexWord)));
  move(LexWord,p^.Name,length(LexWord)+1); RdLex; p^.Dom:=dofs;
  Accept('='); p^.Expr:=RdTerm(dofs,6); ChainLst(Roots^.Consts,pofs);
  if Lexem=',' then begin RdLex; goto 2 end;
  if not (Lexem in [^z,'#']) then goto 1;
end;

function GetPredicate:word{PPredicate};
var p:PPredicate; pofs:word absolute p;
label 1;
begin
  PtrRec(p).Seg:=_Sg; pofs:=Roots^.Predicates; while pofs<>0 do begin
    if LexWord=PString(ptr(_Sg,p^.Name))^ then begin RdLex; goto 1 end;
    pofs:=p^.Chain end;
  pofs:=ClausePreds; while pofs<>0 do begin
    if LexWord=PString(ptr(_Sg,p^.Name))^ then begin RdLex; goto 1 end;
    pofs:=p^.Chain end;
1:GetPredicate:=pofs;
end;
function RdPredicate:word{PPredicate};
var pofs:word;
begin
  pofs:=GetPredicate; if pofs=0 then Error(513); RdPredicate:=pofs;
end;

function GetOutpMask(P:PPredicate):word;
begin
  GetOutpMask:=($ffff shr (16-P^.Arity)) and not P^.InpMask;
end;

procedure RdPredicateDcl(FromClauses:boolean;Db:PDatabase);
var d:PDomain; dofs:word absolute d; p:PPredicate; pofs:word absolute p;
    si:PScanInf; siofs:word absolute si; fl:PFldList; flofs:word absolute fl;
    ip:InstrPtr; ipofs:word absolute ip;
    nm:word{PString}; i,n:integer;
    w,m:word; a:array[0..31] of word{PDomain};
    pos:RdbPos; typ:char; bpOfs:word;
    isOutp,b:boolean; f:FieldDPtr; o:byte; r:RdbDPtr;
    z:FrmlPtr; zofs:word absolute z;
label 2,3,33,4;
begin
  PtrRec(d).Seg:=_Sg; PtrRec(p).Seg:=_Sg; PtrRec(si).Seg:=_Sg;
  PtrRec(fl).Seg:=_Sg; PtrRec(ip).Seg:=_Sg;
  o:=0; if Db<>nil then o:=_DbaseOpt+_CioMaskOpt;
  if Lexem='@' then begin RdLex; o:=o or _FandCallOpt end
  else if Db<>nil then o:=o or _PackInpOpt;
  TestIdentif; if IsCharUpper(LexWord[1]) then Error(518);
  if GetPredicate<>0 then OldError(505); nm:=StorStr(LexWord);
  if (o and _FandCallOpt)<>0 then if Db<>nil then begin
    siofs:=GetZStor(sizeof(TScanInf)-1+length(LexWord));
    move(LexWord,si^.Name,length(LexWord)+1);
    CFile:=RdFileName; if CFile^.typSQLFile then OldError(155);
    si^.FD:=CFile; goto 2 end
  else begin
    SkipBlank(false);
    if ForwChar='[' then begin
      RdLex; RdLex; TestLex(_quotedstr);
      z:=GetOp(_const,length(LexWord)+1); z^.S:=LexWord;
      pos.R:=ptr(0,OOfs(z)); pos.IRec:=0; Accept(_quotedstr); TestLex(']') end
    else begin
      if not FindChpt('P',LexWord,false,pos) then Error(37);
      pos.R:=ptr(0,StorStr(LexWord)); pos.IRec:=$ffff end end;
  RdLex;
2:n:=0; w:=0; m:=1; if Lexem='(' then begin RdLex;
3:  if Db<>nil then begin
      if (o and _FandCallOpt)<>0 then begin
      f:=RdFldName(CFile); flofs:=GetZStor(sizeof(TFldList));
      fl^.FldD:=OPtr(PtrRec(CFile).Seg,f); ChainLst(si^.FL,flofs); dofs:=0;
      Accept('/'); dofs:=RdDomain;
      case f^.FrmlTyp of
       'B':if dofs<>BoolDom then OldError(510);
       'R':if (dofs<>RealDom) and ((f^.Typ<>'F') or (dofs<>IntDom)) then OldError(510);
       else if f^.Typ='T' then begin
           if (dofs<>LongStrDom) and ((d^.Typ<>_FunD) or (dofs=BoolDom)) then OldError(510) end
       else if dofs<>StrDom then OldError(510) end;
      goto 4 end end
    else if Lexem='&' then RdLex else w:=w or m;
    dofs:=RdDomain;
    if (dofs=LongStrDom) and (Db<>nil) then OldError(541);
    if (o and _FandCallOpt<>0) and (d^.Typ=_FunD) and (dofs<>BoolDom) then
      OldError(528);
4:  a[n]:=dofs; inc(n); m:=m shl 1;
    if Lexem=',' then begin if n=15 then Error(519); RdLex; goto 3 end;
    Accept(')') end;
  pofs:=GetZStor(sizeof(TPredicate)-6+2*n);
  if FromClauses then ChainLst(ClausePreds,pofs)
  else ChainLst(Roots^.Predicates,pofs);
  if (Db<>nil) then begin p^.ChainDb:=Db^.Pred; Db^.Pred:=pofs end;
  with p^ do begin Name:=nm; Arity:=n; move(a,Arg,2*n); Opt:=o;
    InpMask:=w; InstSz:=4*n end;
  if (o and _FandCallOpt)<>0 then
  if (o and _DbaseOpt)<>0 then word(p^.Branch):=siofs
  else begin ipofs:=GetZStor(5+sizeof(RdbPos)+2+n*sizeof(TypAndFrml));
    ip^.Kind:=_proc; ip^.PPos:=pos; ip^.N:=n; bpOfs:=4;
    for i:=1 to n do begin
      dofs:=p^.Arg[i-1];
      if (dofs=RealDom) or (dofs=IntDom) then typ:='R' else
      if (dofs=BoolDom) then typ:='B' else typ:='S';
      isOutp:=(w and 1)=0;
      if isOutp then begin
        z:=GetOp(_getlocvar,2); z^.BPOfs:=bpOfs;
        case typ of
         'S':inc(bpOfs,sizeof(longint)); 'R':inc(bpOfs,sizeof(float));
         else inc(bpOfs,sizeof(boolean)) end end
      else case typ of
          'R':z:=GetOp(_const,sizeof(float));
          'B':z:=GetOp(_const,sizeof(boolean));
          else if dofs=StrDom then z:=GetOp(_const,sizeof(string))
          else begin z:=GetOp(_getlocvar,2);
             z^.BPOfs:=bpOfs; inc(bpOfs,sizeof(longint)) end end;
      with ip^.TArg[i] do begin
        FTyp:=typ; Frml:=OPtr(_Sg,z); FromProlog:=true; IsRetPar:=isOutp end;
      w:=w shr 1 end;
    word(p^.Branch):=ipofs; p^.LocVarSz:=bpOfs end;
end;

function GetCommand(Code:TCommandTyp;N:word):word;
var c:PCommand; cofs:word absolute c;
begin
  c:=ptr(_Sg,GetZStor(3+N)); c^.Code:=Code; GetCommand:=cofs;
end;
procedure RdTermList(C:PCommand;D,Kind:word);
var l:PTermList; lofs:word absolute l;
begin
  l:=ptr(_Sg,GetZStor(sizeof(TTermList))); ChainLst(C^.Arg,lofs);
  l^.Elem:=RdTerm(D,Kind);
end;

function RdCommand:word{PCommand};
var v:PVarDcl; cm:char; {d:PDomain;} op,fTyp:char; code:TCommandTyp;
    w:PWriteD; wofs:word absolute w; nl:boolean;
    p:PPredicate; pofs:word absolute p;  c:PCommand; cofs:word absolute c;
    ld:LinkDPtr; fd:FileDPtr; d:PDomain; dofs:word absolute d;
    l:PTermList; lofs:word absolute l; n:word; s:string; i:integer;
label 1,2,20,21,22,3,4,8,9;
begin
  PtrRec(p).Seg:=_Sg; c:=ptr(_Sg,0); PtrRec(w).Seg:=_Sg; PtrRec(d).Seg:=_Sg;
  PtrRec(l).Seg:=_Sg;
  if Lexem='!' then begin
    RdLex; cofs:=GetCommand(_CutC,0); goto 9 end
  else if Lexem<>_identifier then goto 9;
  if IsUpperIdentif then begin
    v:=FindVarDcl; if v=nil then v:=MakeVarDcl(0,-1); RdLex; dofs:=v^.Dom;
    if v^.bound then begin
      case Lexem of
        '=': op:=_equ;
        '<': case ForwChar of
             '>':begin ReadChar; op:=_ne end;
             '=':begin ReadChar; op:=_le end;
             else op:=_lt end;
        '>': if ForwChar='=' then begin ReadChar;op:=_ge end else op:=_gt;
        else Error(524) end;
      if (dofs<>IntDom) and (dofs<>RealDom) and not(op in [_equ,_ne]) then Error(538);
      RdLex end
    else begin
      if not v^.used then begin Accept(':'); dofs:=RdDomain; v^.Dom:=dofs end;
      Accept('='); op:=_assign end;
    cofs:=GetCommand(_CompC,1+1+2*2); c^.Typ:=d^.Typ;
    c^.E1Idx:=v^.Idx; c^.CompOp:=op; c^.E2:=RdTerm(dofs,2);
    if v^.Bound then v^.Used:=true; v^.Bound:=true; goto 9 end;
  if IsKeyWord('fail') then begin
    cofs:=GetCommand(_FailC,0); goto 9 end;
  if IsKeyWord('wait') then begin
    cofs:=GetCommand(_WaitC,0); goto 9 end;
  if IsKeyWord('trace') then begin
    cofs:=GetCommand(_Trace,2); Accept('('); c^.TrcLevel:=RdInteger;
    goto 8 end;
  if IsKeyWord('error') then begin
    Accept('('); cofs:=GetCommand(_ErrorC,2+2); c^.MsgNr:=RdInteger;
    goto 20 end;
  if IsKeyWord('writeln') then begin nl:=true; goto 1 end;
  if IsKeyWord('write') then begin nl:=false;
1:  cofs:=GetCommand(_WriteC,2+1); c^.NL:=nl; Accept('(');
2:  if Lexem=_quotedstr then begin
      wofs:=GetZStor(3+1+length(LexWord)); w^.IsString:=true; w^.SS:=LexWord end
    else begin wofs:=GetZStor(3+2+2); TestIdentif; v:=FindVarDcl;
      if v=nil then Error(511) else if not v^.Bound then Error(509);
      v^.Used:=true; w^.Dom:=v^.Dom; w^.Idx:=v^.Idx;
      if (c^.Code=_ErrorC) and (v^.Dom<>StrDom) and
         ((c^.WrD<>0) or (v^.Dom<>IntDom)) then Error(558) end;
    RdLex; ChainLst(c^.WrD,wofs);
20: if Lexem=',' then begin RdLex; goto 2 end; goto 8 end;
  if copy(LexWord,1,6)='union_' then begin pofs:=UnionPred; goto 21 end;
  if copy(LexWord,1,6)='minus_' then begin pofs:=MinusPred; goto 21 end;
  if copy(LexWord,1,6)='inter_' then begin pofs:=InterPred;
21: dofs:=GetDomain(false,'L_'+copy(LexWord,7,255));
    if d^.Typ<>_ListD then Error(548); RdLex; Accept('(');
    cofs:=GetCommand(_PredC,5*2); c^.Pred:=pofs; c^.Elem:=dofs;
    RdTermList(c,dofs,2); Accept(','); RdTermList(c,dofs,2); Accept(',');
    RdTermList(c,dofs,1); goto 8 end;
  if copy(LexWord,1,4)='mem_' then begin pofs:=MemPred; goto 22 end;
  if copy(LexWord,1,4)='len_' then begin pofs:=LenPred; goto 22 end;
  if copy(LexWord,1,4)='inv_' then begin pofs:=InvPred; goto 22 end;
  if copy(LexWord,1,4)='add_' then begin pofs:=AddPred; goto 22 end;
  if copy(LexWord,1,4)='del_' then begin pofs:=DelPred;
22: dofs:=GetDomain(false,'L_'+copy(LexWord,5,255));
    if d^.Typ<>_ListD then Error(548); RdLex; Accept('(');
    cofs:=GetCommand(_PredC,5*2); c^.Pred:=pofs; c^.Elem{ListDom}:=dofs;
    if pofs=MemPred then begin
      UnbdVarsInTerm:=false; RdTermList(c,d^.ElemDom,4);
      if UnbdVarsInTerm then n:=2 else n:=3; c^.InpMask:=n; c^.OutpMask:=not n;
      Accept(','); RdTermList(c,dofs,2) end
    else begin
      if pofs=AddPred then begin RdTermList(c,d^.ElemDom,2); Accept(',') end
      else if pofs=DelPred then begin RdTermList(c,d^.ElemDom,1); Accept(',') end;
      RdTermList(c,dofs,2); Accept(',');
      if pofs=LenPred then RdTermList(c,IntDom,1) else RdTermList(c,dofs,1) end;
    goto 8 end;
  if IsKeyWord('loadlex') then begin
    code:=_LoadLexC; Accept('('); pofs:=0; goto 4 end;
  if IsKeyWord('save') then begin code:=_SaveC; goto 3 end;
  if IsKeyWord('consult') then begin code:=_ConsultC;
3:  Accept('('); TestIdentif; pofs:=FindDatabase(LexWord);
    if pofs=0 then Error(531); RdLex; Accept(',');
4:  cofs:=GetCommand(code,2+4+4+1+length(LexWord)); c^.DbPred:=pofs;
    move(LexWord,c^.Name,length(LexWord)+1);
    if not IsRoleName(false,fd,ld) then Error(9);
    if fd^.typSQLFile then OldError(155); Accept('.');
    c^.FldD:=OPtr(PtrRec(fd).Seg,RdFldName(fd));
    if c^.FldD^.Typ<>'T' then OldError(537);
8:  Accept(')') end;
9:RdCommand:=cofs;
end;
function RdPredCommand(Code:TCommandTyp):word{PCommand};
var c:PCommand; cofs:word absolute c; p:PPredicate; pofs:word absolute p;
    i,n,w,m,kind,sz,InpMask,OutpMask,lRoot:word;
    l:PTermList; lofs:word absolute l;  dofs:word;
    t:PPTerm; tofs:word absolute t; si:PScanInf; siofs:word absolute si;
    k:KeyDPtr; kf:KeyFldDPtr; fl:PFldList; flofs:word absolute fl;
    a:array[1..255] of byte; IsFandDb,inOut:boolean; f:FieldDPtr;
label 1,2,3;
begin
  PtrRec(p).Seg:=_Sg; PtrRec(c).Seg:=_Sg; PtrRec(l).Seg:=_Sg;
  PtrRec(t).Seg:=_Sg; PtrRec(si).Seg:=_Sg; PtrRec(fl).Seg:=_Sg;
  pofs:=RdPredicate;
  IsFandDb:=p^.Opt and (_DbaseOpt+_FandCallOpt)=_DbaseOpt+_FandCallOpt;
  if (p^.Opt and _DbaseOpt<>_DBaseOpt) and (Code in [_AssertC,_RetractC])
    then OldError(526);
  kind:=1; m:=1; w:=p^.InpMask; sz:=2+2; InpMask:=0; OutpMask:=0; lRoot:=0;
  if p^.Opt and _CioMaskOpt<>0 then begin
    inc(sz,4); if Code=_AssertC then w:=$ffff else kind:=4 end;
  if p^.Arity>0 then begin
    Accept('('); for i:=0 to p^.Arity-1 do begin
      if i>0 then Accept(','); UnbdVarsInTerm:=false;
      if kind<>4 then if w and 1<>0 then kind:=2 else kind:=1;
      dofs:=p^.Arg[i];
      lofs:=GetZStor(sizeof(TTermList)); ChainLst(lroot,lofs);
      l^.Elem:=RdTerm(dofs,Kind);
      if p^.Opt and _CioMaskOpt<>0 then
        if UnbdVarsInTerm then begin
          if l^.Elem<>UnderscoreTerm then
             OutpMask:=OutpMask or m end
        else InpMask:=InpMask or m;
      m:=m shl 1; w:=w shr 1 end;
    Accept(')') end;
  if p^.Opt and _BuildInOpt<>0 then case p^.LocVarSz of
    _ConcatP : if not (InpMask in [3{iio},4{ooi},5{ioi},6{oii},7{iii}]) then OldError(534);
    _FandFieldP,_FandLinkP:begin if InpMask and 1 = 0{o...} then OldError(555);
           if p^.LocVarSz=_FandLinkP then begin
             InpMask:=InpMask and $7; if InpMask=$7 then InpMask:=5 end
           else InpMask:=InpMask and $3;
           OutpMask:=not InpMask end end;
  { FAND-file: find first key, which is a subset of the input fields }
  i:=0; w:=0;
  if IsFandDb and (Code<>_AssertC) then begin
    inc(sz,10); siofs:=word(p^.Branch);
    CFile:=si^.FD; if CFile^.Typ='X' then begin
    k:=CFile^.Keys; while k<>nil do begin
      kf:=k^.KFlds; inOut:=false; while kf<>nil do begin
        m:=1; n:=0; inc(i); flofs:=si^.FL; while flofs<>0 do begin
          f:=fl^.FldD; flofs:=fl^.Chain;
          if f=OPtr(PtrRec(CFile).Seg,kf^.FldD) then begin
            w:=w or m; a[i]:=n;
            if (flofs<>0) and (f=fl^.FldD) and ((OutpMask and (m shl 1))<>0)
               and (f^.Typ='A') then inOut:=true;
            goto 1 end;
          m:=m shl 1; inc(n) end; goto 2;
1:      kf:=kf^.Chain end;
      if InpMask and w = w then begin
        inc(sz,i); if not inOut then w:=0; goto 3 end;
2:    k:=k^.Chain; i:=0; w:=0 end end end;
3: if Code=_AllC then sz:=maxw(sz,14);
   cofs:=GetCommand(Code,sz); c^.Pred:=pofs; c^.Arg:=lRoot;
   if p^.Opt and _CioMaskOpt<>0 then begin
     c^.InpMask:=InpMask; c^.OutpMask:=OutpMask;
     if IsFandDb then begin c^.CompMask:=InpMask and not w;
       if i>0 then begin
         move(a,c^.ArgI,i); c^.KDOfs:=word(OPtr(PtrRec(CFile).Seg,k)) end end end;
   RdPredCommand:=cofs;
end;
procedure RdDbTerm(DOfs:word);
var p:Pchar absolute PackedTermPtr; i:integer; n:word; wp:WordPtr; d:PDomain;
    s:string[32]; minus:boolean; r:float; f:PFunDcl; idx:byte;
label 1,2,3;
begin
  d:=ptr(_Sg,DOfs); if PtrRec(p).Ofs>=PTPMaxOfs then Error(527);
  case Lexem of
    _quotedstr: begin if d^.Typ<>_StrD then Error(510);
            n:=length(LexWord)+1; if PtrRec(p).Ofs+n>=PTPMaxOfs then Error(527);
            move(LexWord,p^,n); inc(p,n); RdLex end;
    '$': if d^.Typ<>_IntD then Error(510) else begin
         i:=0; while ForwChar in ['0'..'9','a'..'f','A'..'F'] do begin
           inc(i); if i>4 then Error(3); ReadChar; s[i]:=CurrChar end;
         if i=0 then Error(504); s[0]:=char(i); n:=HexStrToLong(s); RdLex;
         goto 2 end;
    '-': begin RdLex; if Lexem<>_number then Error(525); minus:=true;
         goto 1 end;
    _number: begin  minus:=false;
1:       s:=LexWord; RdLex;
         if d^.Typ=_IntD then begin
           val(s,n,i); if minus then n:=-n;
2:         IntegerPtr(p)^:=n; inc(p,2) end
         else begin
           if d^.Typ<>_RealD then Error(510);
           if (Lexem='.') and IsDigit(ForwChar) then begin
             RdLex; s:=s+'.'+LexWord; RdLex end;
           val(s,r,i); FloatPtr(p)^:=r; inc(p,sizeof(float)) end end;
    '['       : begin if d^.Typ<>_ListD then Error(510); RdLex;
            wp:=WordPtr(p); inc(p,2); n:=0;
            if Lexem<>']' then begin
3:            RdDbTerm(d^.ElemDom); inc(n);
              if Lexem=',' then begin RdLex; goto 3 end end;
            Accept(']'); wp^:=n end;
    else TestIdentif; if d^.Typ<>_FunD then Error(510);
         f:=GetFunDclByName(DOfs,idx); if f=nil then Error(512);
         p^:=char(idx); inc(p); RdLex; n:=f^.Arity; if n>0 then begin
           Accept('('); for i:=0 to n-1 do begin
             if i>0 then Accept(','); RdDbTerm(f^.Arg[i]) end;
           Accept(')') end end;
end;
procedure RdDbClause(P:PPredicate);
var i,n:word; t:Pchar absolute PackedTermPtr; wp:WordPtr; b:PDbBranch;
    A:array[1..MaxPackedPredLen] of char;
begin
  Accept('('); t:=Pchar(@A); PTPMaxOfs:=ofs(A)+MaxPackedPredLen-2;
  for i:=0 to P^.Arity-1 do begin
    if i>0 then Accept(','); wp:=WordPtr(t); inc(t,2); RdDbTerm(p^.Arg[i]);
    wp^:=PtrRec(t).Ofs-PtrRec(wp).Ofs-2 end;
  n:=PtrRec(t).Ofs-Ofs(A); b:=Mem3.Alloc(4+n); move(A,b^.LL,n);
  ChainLast(P^.Branch,b); Accept(')'); Accept('.');
end;
procedure CheckPredicates(POff:word);
var p:PPredicate; pofs:word absolute p; si:PScanInf;
begin
  p:=ptr(_Sg,POff); while pofs<>0 do begin
    if (p^.Opt and (_DBaseOpt+_FandCallOpt+_BuildInOpt)=0) and (p^.Branch=nil)
      then begin SetMsgPar(PString(ptr(_Sg,p^.name))^); OldError(522) end;
    if p^.Opt and _DBaseOpt<>0 then
      if p^.Opt and _FandCallOpt<>0 then begin
        si:=ptr(_Sg,word(p^.Branch)); si^.FD:=nil end
      else p^.Branch:=nil;
    pofs:=p^.Chain end;
end;
procedure RdAutoRecursionHead(P:PPredicate;B:PBranch);
var l,l1:PTermList; w:word; c:PCommand; t:PPTerm; d:PDomain; v:PVarDcl;
    lofs:word absolute l; l1ofs:word absolute l1; cofs:word absolute c;
    tofs:word absolute t; dofs:word absolute d;
    isInput:boolean; i,j,k:integer;
label 1;
begin
  if p^.Opt<>0 then Error(550);
  PtrRec(c).Seg:=_Sg; PtrRec(l).Seg:=_Sg; PtrRec(l1).Seg:=_Sg;
  PtrRec(t).Seg:=_Sg; PtrRec(d).Seg:=_Sg;
  cofs:=GetCommand(_AutoC,2+3+6*2); b^.Cmd:=cofs; inc(p^.InstSz,4);
  c^.iWrk:=(p^.InstSz div 4)-1; w:=p^.InpMask;
  for i:=0 to p^.Arity-1 do begin
    if w and 1<>0 then isInput:=true else isInput:=false;
    lofs:=GetZStor(sizeof(TTermList)); ChainLst(c^.Arg,lofs);
    tofs:=GetZStor(1+2+1); t^.Fun:=_VarT; t^.Idx:=i; t^.Bound:=isInput;
    l^.Elem:=tofs;
    l1ofs:=GetZStor(sizeof(TTermList)); ChainLst(b^.Head,l1ofs);
    dofs:=p^.Arg[i];
    if i>0 then Accept(',') else
    if not (d^.Typ in [_FunD,_ListD]) then Error(556);
    if Lexem='!' then begin
      if i>0 then begin
        if isInput or (dofs<>p^.Arg[0]) or (c^.iOutp>0) then Error(551);
        c^.iOutp:=i; end
      else if not isInput then Error(552) end
    else if TestKeyWord('_') then begin
      if not isInput then begin
        if d^.Typ=_FunD then Error(575); j:=0; goto 1 end end
    else begin
      if not IsUpperIdentif then Error(511);
      v:=FindVarDcl; if v=nil then v:=MakeVarDcl(dofs,i);
      if isInput then begin if v^.Bound then Error(553); v^.Bound:=true end
      else begin if v^.Used then Error(553); v^.Used:=true end;
      if v^.Bound and v^.Used then begin j:=v^.Idx;
1:      k:=c^.nPairs; inc(c^.nPairs);
        if isInput then begin c^.Pair[k].iInp:=i; c^.Pair[k].iOutp:=j end
        else begin c^.Pair[k].iInp:=j; c^.Pair[k].iOutp:=i end end end;
    RdLex; w:=w shr 1 end;
  Accept(')');
end;
procedure RdSemicolonClause(P:PPredicate;B:PBranch);
var v:PVarDcl; c:PCommand; cofs:word absolute c; x:char;
    bofs:word absolute B;
label 1,2,3;
begin
  RdLex; PtrRec(c).Seg:=_Sg;
  if IsUpperIdentif then begin x:='a';
    v:=FindVarDcl; if p^.InpMask<>(1 shl (p^.Arity-1))-1 then Error(562);
    if (v=nil) or v^.Bound or (PDomain(ptr(_Sg,v^.Dom))^.Typ<>_ListD) then Error(561);
    RdLex; Accept('+'); Accept('='); v^.Bound:=true;
    cofs:=GetCommand(_AppPkC,6); c^.apIdx:=v^.Idx;
    c^.apTerm:=RdTerm(v^.Dom,2); ChainLst(b^.cmd,cofs);
    if Lexem=',' then begin
      RdLex; AcceptKeyWord('self'); cofs:=GetCommand(_SelfC,0); goto 2 end;
    goto 1 end
  else if TestKeyWord('error') then begin x:='e';
    cofs:=GetCommand(_CutC,0) end
  else if IsKeyWord('self') then begin x:='s'; goto 3 end
  else begin x:='f';
1:  cofs:=GetCommand(_FailC,0) end;
2:ChainLst(b^.cmd,cofs);
3:bofs:=GetZStor(sizeof(TBranch)); ChainLst(p^.Branch,bofs);
  case x of
   'e':b^.cmd:=RdCommand;
   'f':if GetOutpMask(p)<>0 then Error(559);
   's':b^.cmd:=GetCommand(_SelfC,0);
   'a':begin cofs:=GetCommand(_AppUnpkC,4); c^.apIdx:=v^.Idx; c^.apDom:=v^.Dom;
       b^.cmd:=cofs end end;
end;
procedure RdClauses;
var b:PBranch; bofs:word absolute b;
    w,m,mp:word; i,kind:integer; iserr:boolean;
    d,dEl:PDomain; dofs:word absolute d; dElofs:word absolute dEl;
    l:PTermList; lofs:word absolute l; t:PPTerm; tofs:word absolute t;
    p,p1:PPredicate; pofs:word absolute p; p1ofs:word absolute p1;
    c:PCommand; cofs:word absolute c;
    v:PVarDcl; x:pointer; code:TCommandTyp;
type SF=record BP:word; case byte of 0:(Ret:pointer); 1:(RetOfs:word) end;
var z:^SF; zofs:word absolute z; WasNotC:boolean;
label 1,11,2,3,4,6;
begin
  PtrRec(d).Seg:=_Sg; PtrRec(dEl).Seg:=_Sg; PtrRec(p).Seg:=_Sg;
  PtrRec(l).Seg:=_Sg; PtrRec(c).Seg:=_Sg; PtrRec(b).Seg:=_Sg;
  PtrRec(t).Seg:=_Sg; PtrRec(p1).Seg:=_Sg;
1:if Lexem=':' then begin RdLex; RdPredicateDcl(true,nil); goto 6 end;
  TestIdentif; pofs:=RdPredicate;
  if p^.Opt and (_FandCallOpt+_BuildInOpt)<>0 then OldError(529);
  if p^.Opt and _DBaseOpt<>0 then begin RdDbClause(p); goto 6 end;
  VarDcls:=nil; VarCount:=p^.Arity; x:=Mem1.Mark;
  bofs:=GetZStor(sizeof(TBranch)); ChainLst(p^.Branch,bofs);
  if p^.Arity>0 then begin
    Accept('('); if Lexem='!' then begin RdAutoRecursionHead(p,b); goto 4; end;
    w:=p^.InpMask; m:=1; for i:=0 to p^.Arity-1 do begin
      if i>0 then Accept(','); dofs:=p^.Arg[i];
      kind:=1; if (w and 1)=0 then kind:=3;
      lofs:=GetZStor(sizeof(TTermList)); ChainLst(b^.Head,lofs); SkipBlank(false);
      if IsUpperIdentif and (ForwChar in[',',')']) {solo variable} then begin
        RdVar(dofs,kind,i,tofs); if tofs<>0 then goto 11 end
      else begin tofs:=0;
11:	if (w and 1)<>0 then b^.HeadIMask:=b^.HeadIMask or m
        else b^.HeadOMask:=b^.HeadOMask or m;
	if tofs=0 then tofs:=RdTerm(dofs,kind); l^.Elem:=tofs end;
      w:=w shr 1; m:=m shl 1 end;
    Accept(')') end;
  if Lexem<>'.' then begin
    Accept(_assign);
2:  WasNotC:=false;
    if IsKeyWord('self') then begin
      cofs:=GetCommand(_SelfC,0); ChainLst(b^.Cmd,cofs); goto 4 end;
    if IsKeyWord('not') then begin Accept('('); WasNotC:=true end;
    cofs:=RdCommand; if cofs=0 then
    if (Lexem=_identifier) and (copy(LexWord,1,4)='all_') then begin
      dofs:=GetDomain(false,'L_'+copy(LexWord,5,255));
      if d^.Typ<>_ListD then Error(548); RdLex;
      Accept('('); cofs:=RdPredCommand(_AllC); Accept(',');
      c^.Elem:=RdTerm(d^.ElemDom,2); Accept(',');
      c^.Idx:=VarCount; inc(VarCount); RdVar(dofs,5,-1,c^.Idx2);
      Accept(')') end else
    if IsKeyWord('assert') then begin code:=_AssertC; goto 3 end else
    if IsKeyWord('retract') then begin code:=_RetractC;
3:    Accept('('); cofs:=RdPredCommand(code); Accept(')') end
    else cofs:=RdPredCommand(_PredC);
    ChainLst(b^.Cmd,cofs);
    if WasNotC then begin
      if c^.Code<>_PredC then OldError(546); p1ofs:=c^.Pred; lofs:=c^.Arg;
      if (p1^.Opt and _CioMaskOpt)<>0 then w:=c^.InpMask else w:=p1^.InpMask;
      while lofs<>0 do begin
        if w and 1=0 then begin
          tofs:=l^.Elem; if (tofs=0) or (t^.Fun<>_UnderscT) then OldError(547) end;
        lofs:=l^.Chain; w:=w shr 1 end;
      Accept(')'); c^.Code:=_NotC end;
    if Lexem=',' then begin RdLex; goto 2 end;
    if Lexem=';' then RdSemicolonClause(p,b) end;
4:Accept('.');
  v:=VarDcls; while v<>nil do begin
    if not v^.Used or not v^.Bound then begin
      SetMsgPar(v^.Name); if not v^.Used then OldError(521) else OldError(520) end;
    v:=v^.Chain end;
  p^.InstSz:=maxw(p^.InstSz,4*VarCount);
  Mem1.Release(x);
6:if not (Lexem in [^z,'#']) then goto 1;
  CheckPredicates(ClausePreds); ClausePreds:=0;
end;

function MakePred(const PredName,ArgTyp:string; PredKod,PredMask:word):word;
var p:PPredicate; pofs:word absolute p; i,n,dofs:word;
begin
  n:=length(ArgTyp); p:=ptr(_Sg,GetZStor(sizeof(TPredicate)-6+n*2));
  ChainLst(Roots^.Predicates,pofs); MakePred:=pofs;
  with p^ do begin
    Name:=StorStr(PredName); Arity:=n; LocVarSz:=PredKod;
    for i:=1 to n do begin
      case ArgTyp[i] of 's':dofs:=StrDom; 'l':dofs:=LongStrDom;
        'i':dofs:=IntDom; 'r':dofs:=RealDom; 'b':dofs:=BoolDom;
        'x':dofs:=LLexDom end;
      Arg[i-1]:=dofs end;
    if PredMask=$ffff then Opt:=_BuildInOpt+_CioMaskOpt else begin
      Opt:=_BuildInOpt; InpMask:=PredMask end;
    InstSz:=n*4 end;
end;

function ReadProlog(RecNr:word):word;
const Booln:string[7]='Boolean';
      Reell:string[4]='Real';
var db:PDatabase; dbofs:word absolute db; p:PPredicate; pofs:word absolute p;
    d:PDomain; dofs:word absolute d; f:PFunDcl; fofs:word absolute f;
    s:string; pos:RdbPos; ss:LongStrPtr; p1,p2,pp1,pp2,pp3,cr:pointer;
    AA:longint;
begin
  MarkBoth(p1,p2); cr:=CRecPtr;
  if ProlgCallLevel=0 then begin
    FreeMemList:=nil; Mem1.Init; Mem2.Init; Mem3.Init end
  else begin
    pp1:=Mem1.Mark; pp2:=Mem2.Mark; pp3:=Mem3.Mark end;
  AlignLongStr; ss:=GetStore(2); AA:=AbsAdr(@ss^.A);
  _Sg:=PtrRec(HeapPtr).Seg; ReadProlog:=_Sg;
  PtrRec(db).Seg:=_Sg; PtrRec(p).Seg:=_Sg; PtrRec(d).Seg:=_Sg;
  PtrRec(f).Seg:=_Sg; ClausePreds:=0;
  Roots:=GetZStore(sizeof(TProgRoots));
  UnderScoreTerm:=GetZStor(1); PPTerm(ptr(_Sg,UnderScoreTerm))^.Fun:=_UnderscT;
  StrDom:=MakeDomain(_StrD,'String');
  LongStrDom:=MakeDomain(_LongStrD,'LongString');
  IntDom:=MakeDomain(_IntD,'Integer');
  RealDom:=MakeDomain(_RealD,'Real');
  BoolDom:=MakeDomain(_FunD,'Boolean'); dofs:=BoolDom;
    fofs:=GetZStor(sizeof(TFunDcl)-3*2); f^.Name:=StorStr('false'); ChainLst(d^.FunDcl,fofs);
    fofs:=GetZStor(sizeof(TFunDcl)-3*2); f^.Name:=StorStr('true'); ChainLst(d^.FunDcl,fofs);
  pofs:=GetZStor(sizeof(TPredicate)-6); p^.Name:=StorStr('main'); Roots^.Predicates:=pofs;
  LexDom:=MakeDomain(_FunD,'Lexem'); dofs:=LexDom;
    fofs:=GetZStor(sizeof(TFunDcl)); f^.Name:=StorStr('lex'); f^.Arity:=3;
    f^.Arg[0]:=IntDom; f^.Arg[1]:=IntDom; f^.Arg[2]:=StrDom; ChainLst(d^.FunDcl,fofs);
  LLexDom:=MakeDomain(_ListD,'L_Lexem'); dofs:=LLexDom; d^.ElemDom:=LexDom;
  MemPred:=MakePred('mem_?','ii',_MemP,$ffff);
  MakePred('concat','sss',_ConcatP,$ffff);
  MakePred('call','ss',_CallP,3{ii});
  LenPred:=MakePred('len_?','ii',_LenP,1{io});
  InvPred:=MakePred('inv_?','ii',_InvP,1{io});
  AddPred:=MakePred('add_?','iii',_AddP,3{iio});
  DelPred:=MakePred('del_?','iii',_DelP,2{oio});
  UnionPred:=MakePred('union_?','iii',_UnionP,3{iio});
  MinusPred:=MakePred('minus_?','iii',_MinusP,3{iio});
  InterPred:=MakePred('inter_?','iii',_InterP,3{iio});
  MakePred('abbrev','ss',_AbbrevP,1{io});
  MakePred('fandfile','ssss',_FandFileP,0{oooo});
  MakePred('fandfield','sssiiis',_FandFieldP,$ffff);
  MakePred('fandkey','ssbb',_FandKeyP,1{iooo});
  MakePred('fandkeyfield','sssbb',_FandKeyFieldP,3{iiooo});
  MakePred('fandlink','sssssi',_FandLinkP,$ffff);
  MakePred('fandlinkfield','sss',_FandLinkFieldP,3{iio});
  MakePred('nextlex','',_NextLexP,0);
  MakePred('getlex','x',_GetLexP,0{o});
  ResetCompilePars; RdLex;
  while Lexem<>^z do begin
    Accept('#');
    if IsKeyWord('DOMAINS') then RdDomains else
    if IsKeyWord('CONSTANTS') then RdConstants else
    if IsKeyWord('DATABASE') then begin
      s[0]:=#0; if Lexem='-' then begin
        RdLex; TestIdentif; s:=LexWord; RdLex end;
      dbofs:=FindDataBase(s); if dbofs=0 then begin
        dbofs:=GetZStor(sizeof(TDatabase)-1+length(s));
        ChainLst(Roots^.Databases,dbofs); move(s,db^.Name,length(s)+1) end;
      repeat RdPredicateDcl(false,db) until (Lexem in [^z,'#']) end else
    if IsKeyWord('PREDICATES') then
      repeat RdPredicateDcl(false,nil) until (Lexem in [^z,'#'])
    else begin AcceptKeyWord('CLAUSES'); RdClauses end end;
  if AbsAdr(HeapPtr)-AA>MaxLStrLen then OldError(544);
  dbofs:=Roots^.Databases; while dbofs<>0 do begin
    db^.SOfs:=word(OPtr(_Sg,SaveDb(dbofs,AA)));
    dbofs:=db^.Chain end;
  CheckPredicates(Roots^.Predicates);
  ss^.LL:=AbsAdr(HeapPtr)-AA;
  if ProlgCallLevel=0 then ReleaseStore2(p2) else begin
    Mem1.Release(pp1); Mem2.Release(pp2); Mem3.Release(pp3) end;
  if RecNr<>0 then begin
    CFile:=Chpt; CRecPtr:=cr; StoreChptTxt(ChptOldTxt,ss,true);
    WriteRec(RecNr); ReleaseStore(p1) end;
end;

end.