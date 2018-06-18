{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit compile;

interface

uses dos,drivers,base,access,wwmix,rdrun;

function EquUpcase(const S:string):boolean;
procedure Error(N:integer);
procedure SetInpStr(var S:string);
procedure SetInpLongStr(S:LongStrPtr;ShowErr:boolean);
procedure SetInpTTPos(Pos:longint;Decode:boolean);
procedure SetInpTT(RP:RdbPos;FromTxt:boolean);
procedure SetInpTTxtPos(FD:FileDPtr);
procedure ReadChar;
function RdDirective(var b:boolean):word;
procedure SkipLevel(withElse:boolean);
procedure SkipBlank(toNextLine:boolean);
procedure OldError(N:integer);
procedure RdBackSlashCode;
procedure RdLex;
function IsForwPoint:boolean;
procedure TestIdentif;
procedure TestLex(X:char);
procedure Accept(X:char);
function RdInteger:integer;
function RdRealConst:float;
function IsKeyWord(const S:string):boolean;
function TestKeyWord(const S:string):boolean;
function IsOpt(const S:string):boolean;
function IsDigitOpt(const S:string;var N:word):boolean;
function IsIdentifStr(const S:string):boolean;
function RdStrConst:StringPtr;
function Rd1Char:char;
function RdQuotedChar:char;
procedure AcceptKeyWord(const S:string);
procedure RdLocDcl(var LVB:LocVarBlkD;IsParList,WithRecVar:boolean;CTyp:char);
function FindLocVar(LVRoot:LocVarPtr;var LV:LocVarPtr):boolean;
function FindChpt(Typ:char;const Name:string;local:boolean;var RP:RdbPos):boolean;
procedure RdChptName(C:char;var Pos:RdbPos;TxtExpr:boolean);
function AllFldsList(FD:FileDPtr;OnlyStored:boolean):FieldList;
function GetEditOpt:EditOptPtr;
function GetRprtOpt:RprtOptPtr;
function GetOp(Op:char;BytesAfter:integer):FrmlPtr;
function FindFldName(FD:FileDPtr):FieldDPtr;
function RdFldName(FD:FileDPtr):FieldDPtr;
function FindFileD:FileDPtr;
function RdFileName:FileDPtr;
function FindLD(const RoleName:string):LinkDPtr;
function IsRoleName(Both:boolean;var FD:FileDPtr;var LD:LinkDPtr):boolean;
function RdFAccess(FD:FileDPtr;LD:LinkDPtr;var FTyp:char):FrmlPtr;
function TryRdFldFrml(FD:FileDPtr;var FTyp:char):FrmlPtr;
function RdFldNameFrmlF(var FTyp:char):FrmlPtr;
function FrmlContxt(Z:FrmlPtr;FD:FileDPtr;RP:pointer):FrmlPtr;
function MakeFldFrml(F:FieldDPtr;var FTyp:char):FrmlPtr;
procedure TestString(FTyp:char);
procedure TestReal(FTyp:char);
function RdFrml(var FTyp:char):FrmlPtr;
function RdKeyInBool(var KIRoot:KeyInDPtr;NewMyBP,FromRdProc:boolean;var SQLFilter:boolean):FrmlPtr;
function RdBool:FrmlPtr;
function RdRealFrml:FrmlPtr;
function RdStrFrml:FrmlPtr;

function RdViewKey:KeyDPtr;
function RdKF(FD:FileDPtr):KeyFldDPtr;
function RdKFList(var KFRoot:KeyFldDPtr;FD:FileDPtr):word;
function IsKeyArg(F:FieldDPtr; FD:FileDPtr): boolean;
procedure CompileRecLen;
function SaveCompState:pointer;
procedure RestoreCompState(p:pointer);
procedure CFileLikeFD(FD:FileDPtr;MsgNr:word);
function RdHelpName:StringPtr;
function RdAttr:FrmlPtr;
procedure RdW(var W:WRectFrml);
procedure RdFrame(var Z:FrmlPtr;var WFlags:byte);
function PromptSortKeys(FL:FieldList;var SKRoot:KeyFldDPtr):boolean;
procedure RdAssignFrml(FTyp:char;var Add:boolean;var Z:FrmlPtr);
function FldTypIdentity(F1,F2:FieldDPtr):boolean;
procedure RdFldList(var FLRoot:FieldList);
procedure RdNegFldList(var Neg:boolean;var FLRoot:FieldList);
procedure EditModeToFlags(const Mode:string;var Flgs;Err:boolean);

var ChptIPos:RdbPos;   { used in LexAnal and ProjMgr}

implementation

uses  obaseww,oaccess,rdfildcl,
      runfrml,
{$ifdef FAndSQL} channel, {$endif}
      editor,rdproc,runproj;

procedure Ovr; far;
assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push cs; push ax;
end;

function StateLen:word;
begin
  StateLen:=ofs(LstCompileVar)-ofs(CurrChar);
end;
function SaveCompState:pointer;
var p:pointer; n:integer;
begin
  n:=StateLen; p:=GetStore2(n); move(CurrChar,p^,n); SaveCompState:=p;
end;
procedure RestoreCompState(p:pointer);
begin
  Move(p^,CurrChar,StateLen);
  ReleaseStore2(ptr(PtrRec(p).Seg,PtrRec(p).Ofs+StateLen));
end;

{$I lexanal}
{$I rdmix}
{$I rdfrml}
{$I rdfrml1}

end.