{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit runproj;

interface

uses dos,base,access,rdrun;

function IsCurrChpt:boolean;
procedure ReleaseFDLDAfterChpt;
function RdFDSegment(FromI:word;Pos:longint):boolean;
function ChptDel:boolean;
function ChptWriteCRec:word; { 0-O.K., 1-fail, 2-fail and undo}
function PromptHelpName(var N:word):boolean;
procedure EditHelpOrCat(cc,kind:word;txt:string);
procedure StoreChptTxt(F:FieldDPtr;S:LongStrPtr;Del:boolean);
function EditExecRdb(const Nm,ProcNm:string;ProcCall:InstrPtr):boolean;
procedure InstallRdb(n:NameStr);


implementation

uses drivers,obaseww,oaccess,
{$ifdef FandSQL} channel, {$endif}
     runfrml,wwmenu,wwmix,
     compile,rdfildcl,rdmerg,rdrprt,rdedit,rdproc,
     sort,runmerg,runrprt,genrprt,runedi,runproc,expimp,runfand,
{$ifdef FandProlog}
     rdprolg,runprolg,
{$endif}
     editor;


procedure Ovr; far;
assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push cs; push ax;
end;

var E: EditDPtr absolute EditDRoot;

{$I projmgr1}
{$I projmgr}

end.