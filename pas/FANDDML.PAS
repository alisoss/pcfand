{$I Switches}
unit FandDML;
interface
uses Dos,crt;
const
  f_Stored=1; f_Encryp=2; f_Mask=4; f_Comma=8;          {FieldD flags}
type
{$ifdef Coproc} float=double;
{$else} float=real; {$endif}
  ScreenStr=string[80];
  FrmlPtr=pointer;
  ItemPtr=^_FieldDescr;
  _FieldDescr=record
    Chain:ItemPtr;
    Typ,FrmlTyp:char; {Typ='F','A','N','D','R','T','B'  FrmlTyp='S','R','B'}
    L,M,NBytes,Flgs:byte;  {NBytes = length in record
                       M digits after decimal point /Typ='F,n.m'/
                         1 if left(L), 0 if right(R) /Typ='A','N'/
                       L = length in data editor }
    case boolean{Stored} of
    True :(Displ:integer   {displacement in record}
           {..Name} );
    False:(Frml:FrmlPtr;   {formula for computing the item /#C/}
           Name:string);   {after last character of the name
                            follows the mask string /Typ='D'/}
  end;

 FileUseMode=(Closed,RdOnly,RdShared,Shared,Exclusive);
 LockMode=(NullMode,NoExclMode,NoDelMode,NoCrMode,RdMode,
           WrMode,CrMode,DelMode,ExclMode);
 FilePtr=^_FileD;
 _FileD = record
   Chain:FilePtr;
   RecLen:word;
   RecPtr:pointer;
   NRecs:longint;
   WasWrRec,WasRdOnly,EOF:boolean;
   Typ:char; { 8=Fand 8;6=Fand 16;0=RDB; C=CAT }
   Handle:word;
   IRec:longint;
   FrstDispl:word;
   TF:pointer;
   ChptPos:array[1..6]of byte;
   TxtPosUDLI:word;
   OrigFD:FilePtr;    { like orig. or nil}
   Drive:byte;         {1=A:,2=B:,else 0}
   CatIRec:word;
   FldD:ItemPtr;
   IsParFile,IsJournal,IsHlpFile,typSQLFile,IsSQLFile,IsDynFile:boolean;
   UMode:FileUseMode;
   LMode,ExLMode,TaLMode:LockMode;
   ViewNames,XF:pointer;
   Keys:pointer;
   Add:pointer;
   nLDs,DliOfs:word;
   Name:string;
 end;
 RdbDPtr=^_RdbD;
 _RdbD=record
   ChainBack:RdbDPtr;
   FD,HelpFD:FilePtr;  { FD^= _FileRec for this RDB }
   X1,X2:pointer; B:boolean;
   Dir:DirStr;
 end;
 String8=string[8];String11=string[11];String12=string[12];
 CharArr=array[1..10]of char;CharArrPtr=^CharArr;
 StringPtr=^string;
type _Inst_=record userColor:array[0..15] of byte;
                   mNorm,mHili,mFirst,mDisabled, {menu}
                   sNorm,sHili,sMask, {select}
                   pNorm,pTxt, {prompt,verify,password}
                   zNorm, {message}
                   lNorm,lFirst,lSwitch, {last line}
                   fNorm, {first line}
                   tNorm,tCtrl,tBlock, {text edit}
                   tUnderline,tItalic,tDWidth,tDStrike,tEmphasized,tCompressed,tElite,
                   dNorm,dHili,dSubset,dTxt,dDeleted,dSelect, {data edit}
                   uNorm, {user screen}
                   hNorm,hHili,hMenu,hSpec,
                   nNorm,ShadowAttr,DesktopColor:byte;
                   CharOrdTab:array[0..255] of byte;   {abc}
                   UpcCharTab:array[0..255] of byte;
                   TxtCols,TxtRows:word;
            end;
const _prUl1=1;    _prUl2=2;
      _prKv1=3;    _prKv2=4;
      _prBr1=5;    _prBr2=6;
      _prDb1=7;    _prDb2=8;
      _prBd1=9;    _prBd2=10;
      _prKp1=11;   _prKp2=12;
      _prEl1=13;   _prEl2=14;
      _prReset=15;
      _prPageSizeNN=16; _prPageSizeTrail=17;
      _prLMarg=18; _prLMargTrail=19;
      _prUs11=20;  _prUs12=21;
      _prUs21=22;  _prUs22=23;
      _prUs31=24;  _prUs32=25;
      _prLine72=26;_prLine216=27;
      _prDen60=28; _prDen120=29;
      _prDen240=30;_prColor=31;_prClose=32;
var _searchParOfs:word;
    _keyFound:boolean;
var _CallDML:pointer;
    _inst:^_Inst_;

procedure _OpenDML(UserHeapSize:longint);
function _file(Name:String8):FilePtr;
procedure _resetm(F:FilePtr;UseMode:FileUseMode);
procedure _reset(F:FilePtr);
procedure _rewrite(F:FilePtr);
procedure _close(F:FilePtr);
function _item(F:FilePtr;Name:String12):ItemPtr;
procedure _seek(F:FilePtr;N:longint);
procedure _indexseek(F:FilePtr;Alias:string;N:longint);
procedure _deleterec(F:FilePtr);
procedure _recallrec(F:FilePtr);
function _filesize(F:FilePtr):longint;
function _filepos(F:FilePtr):longint;
function _eof(F:FilePtr):boolean;
procedure _read(F:FilePtr);
procedure _write(F:FilePtr);
function _s(F:FilePtr;I:ItemPtr):string;
function _r(F:FilePtr;I:ItemPtr):float;
function _b(F:FilePtr;I:ItemPtr):boolean;
procedure _t(var Arr;var Len:word; MaxLen:word;F:FilePtr;I:ItemPtr);
function _deleted(F:FilePtr):boolean;
procedure s_(F:FilePtr;I:ItemPtr;S:string);
procedure r_(F:FilePtr;I:ItemPtr;R:float);
procedure b_(F:FilePtr;I:ItemPtr;B:boolean);
procedure t_(var Arr;Len:word;F:FilePtr;I:ItemPtr);
procedure _setkey(F:FilePtr;Alias:string);
procedure PushReal(R:float);
procedure PushBoolean(B:boolean);
procedure PushString(S:string);
procedure _searchkey;
function _indexpos:longint;
procedure _keylink(F:FilePtr;Name:String8);
procedure _save;
procedure _newfilesize(F:FilePtr;N:longint);
procedure _fandmsg(N:word;Par1,Par2:StringPtr);
function _strdate(R:float;Mask:ScreenStr):ScreenStr;
function _valdate(S,Mask:ScreenStr):float;
function _menu(Header:ScreenStr;MenuTxt:string):word;
procedure _f10message(Text:ScreenStr);
function _prompts(Text:ScreenStr;Typ:char;L,M:word):string;
function _promptr(Text:ScreenStr;Typ:char;L,M:word;Mask:ScreenStr):float;
function _promptb(Text:ScreenStr):boolean;
procedure _mountvol(Vol:String11;Drive:char);
function _CRdb:RdbDPtr;
function _StoreAvail:longint;
function _GetStore(Size:word):pointer;
procedure _ReleaseStore(P:pointer);
function _RdFrml(S:String;FD:FilePtr;
                 var Z:FrmlPtr;var FTyp:char;
                 var WasError:boolean;var Pos:word):ScreenStr;
function _RunReal(Z:FrmlPtr;FD:FilePtr):float;
function _RunStr(Z:FrmlPtr;FD:FilePtr):string;
function _RunBool(Z:FrmlPtr;FD:FilePtr):boolean;
function _PrTab(N:word):string;


implementation

const DMLVersion=41;
var OldHeapPtr:pointer;

procedure _init(var WMin,WMax,Attr:word;HpOrg,HpEnd:pointer;Ver,PrfxSeg:word;
      var Colr;Prnt:pointer); far; assembler;
  asm  mov bx,0*4; call _CallDML end;
procedure _exit; far; assembler;
  asm  mov bx,1*4; call _CallDML end;

type
  KeyFldDPtr=^KeyFldD;
  KeyFldD=record Chain:KeyFldDPtr;
                 FldD:ItemPtr;
                 CompLex:boolean end;
  KeyDPtr=^KeyD;
  KeyD=record Chain:KeyDPtr;
              KFlds:KeyFldDPtr;
              Intervaltest,Duplic:boolean;
              IndexRoot:word;
              IndexLen:byte;
              Alias:string[1];
       end;                      
function _findalias(F:FilePtr;Alias:String):KeyDPtr; far; assembler;
  asm  mov bx,24*4; call _CallDML end;
function _callsearchkey(F:FilePtr;K:KeyDPtr;XP:pointer):boolean; far; assembler;
  asm  mov bx,23*4; call _CallDML end;
procedure _xstorereal(XP:pointer;R:float;KF:KeyFldDPtr); far; assembler;
  asm  mov bx,48*4; call _CallDML end;
procedure _xstorebool(XP:pointer;B:boolean;KF:KeyFldDPtr); far; assembler;
  asm  mov bx,50*4; call _CallDML end;
procedure _xstorestr(XP:pointer;S:string;KF:KeyFldDPtr); far; assembler;
  asm  mov bx,49*4; call _CallDML end;


var _XXS:string; _XXFD:FilePtr; _XXK:KeyDPtr; _XXF:KeyFldDPtr;

procedure CheckXXF(C:char);
begin if (_XXF=nil) or (_XXF^.FldD^.FrmlTyp<>C) then begin
      _fandmsg(635,nil,nil); halt end end;
procedure _setkey(F:FilePtr; Alias:string);
begin _XXK:=_findalias(F,Alias); _XXFD:=F; _XXF:=_XXK^.KFlds; _XXS[0]:=#0 end;
procedure PushReal(R:float);
begin CheckXXF('R'); _xstorereal(@_XXS,R,_XXF); _XXF:=_XXF^.Chain end;
procedure PushBoolean(B:boolean);
begin CheckXXF('B'); _xstorebool(@_XXS,B,_XXF); _XXF:=_XXF^.Chain end;
procedure PushString(S:string);
begin CheckXXF('S'); _xstorestr(@_XXS,S,_XXF); _XXF:=_XXF^.Chain end;
procedure _searchkey;
begin _keyfound:=_callsearchkey(_XXFD,_XXK,@_XXS) end;


function _file(Name:String8):FilePtr; assembler;
  asm  mov bx,2*4; call _CallDML end;
procedure _resetm(F:FilePtr;UseMode:FileUseMode); assembler;
  asm  mov bx,4*4; call _CallDML end;
procedure _reset(F:FilePtr); assembler;
  asm  mov bx,3*4; call _CallDML end;
procedure _rewrite(F:FilePtr); assembler;
  asm  mov bx,12*4; call _CallDML end;
procedure _close(F:FilePtr); assembler;
  asm  mov bx,5*4; call _CallDML end;
function _item(F:FilePtr;Name:String12):ItemPtr; assembler;
  asm  mov bx,6*4; call _CallDML end;
procedure _seek(F:FilePtr;N:longint); assembler;
  asm  mov bx,7*4; call _CallDML end;
procedure _indexseek(F:FilePtr;Alias:string;N:longint); assembler;
  asm  mov bx,8*4; call _CallDML end;
procedure _deleterec(F:FilePtr); assembler;
  asm  mov bx,46*4; call _CallDML end;
procedure _recallrec(F:FilePtr); assembler;
  asm  mov bx,47*4; call _CallDML end;
function _filesize(F:FilePtr):longint; assembler;
  asm  mov bx,9*4; call _CallDML end;
function _filepos(F:FilePtr):longint; assembler;
  asm  mov bx,10*4; call _CallDML end;
function _eof(F:FilePtr):boolean; assembler;
  asm  mov bx,11*4; call _CallDML end;
procedure _read(F:FilePtr); assembler;
  asm  mov bx,13*4; call _CallDML end;
procedure _write(F:FilePtr); assembler;
  asm  mov bx,14*4; call _CallDML end;
function _s(F:FilePtr;I:ItemPtr):string; assembler;
  asm  mov bx,15*4; call _CallDML end;
function _r(F:FilePtr;I:ItemPtr):float; assembler;
  asm  mov bx,16*4; call _CallDML end;
function _b(F:FilePtr;I:ItemPtr):boolean; assembler;
  asm  mov bx,17*4; call _CallDML end;
function _deleted(F:FilePtr):boolean;
  type BoolPtr=^boolean;
  begin _deleted:=BoolPtr(F^.RecPtr)^ end;
procedure _t(var Arr;var Len:word; MaxLen:word;F:FilePtr;I:ItemPtr); assembler;
  asm  mov bx,18*4; call _CallDML end;
procedure s_(F:FilePtr;I:ItemPtr;S:string); assembler;
  asm  mov bx,19*4; call _CallDML end;
procedure r_(F:FilePtr;I:ItemPtr;R:float); assembler;
  asm  mov bx,20*4; call _CallDML end;
procedure b_(F:FilePtr;I:ItemPtr;B:boolean); assembler;
  asm  mov bx,22*4; call _CallDML end;
procedure t_(var Arr;Len:word;F:FilePtr;I:ItemPtr); assembler;
  asm  mov bx,21*4; call _CallDML end;
function _indexpos:longint; assembler;
  asm  mov bx,37*4; call _CallDML end;
procedure _keylink(F:FilePtr;Name:String8); assembler;
  asm  mov bx,25*4; call _CallDML; mov _keyfound,al end;
procedure _save; assembler;
  asm  mov bx,26*4; call _CallDML end;
procedure _newfilesize(F:FilePtr;N:longint); assembler;
  asm  mov bx,27*4; call _CallDML end;
procedure _fandmsg(N:word;Par1,Par2:StringPtr); assembler;
  asm  mov bx,28*4; call _CallDML end;
function _strdate(R:float;Mask:ScreenStr):ScreenStr; assembler;
  asm  mov bx,29*4; call _CallDML end;
function _valdate(S,Mask:ScreenStr):float; assembler;
  asm  mov bx,30*4; call _CallDML end;
function _menu(Header:ScreenStr;MenuTxt:string):word; assembler;
  asm  mov bx,31*4; call _CallDML end;
procedure _f10message(Text:ScreenStr); assembler;
  asm  mov bx,32*4; call _CallDML end;
function _prompts(Text:ScreenStr;Typ:char;L,M:word):string; assembler;
  asm  mov bx,33*4; call _CallDML end;
function _promptr(Text:ScreenStr;Typ:char;L,M:word;Mask:ScreenStr):float; assembler;
  asm  mov bx,34*4; call _CallDML end;
function _promptb(Text:ScreenStr):boolean; assembler;
  asm  mov bx,35*4; call _CallDML end;
procedure _mountvol(Vol:String11;Drive:char); assembler;
  asm  mov bx,36*4; call _CallDML end;
function _CRdb:RdbDPtr; assembler;
  asm  mov bx,38*4; call _CallDML end;
function _StoreAvail:longint; assembler;
  asm  mov bx,39*4; call _CallDML end;
function _GetStore(Size:word):pointer; assembler;
  asm  mov bx,40*4; call _CallDML end;
procedure _ReleaseStore(P:pointer); assembler;
  asm  mov bx,41*4; call _CallDML end;
     {!!!!!! don't open FAND files
             between p:=_GetStore(size) and ReleaseStore(p) }
function _RdFrml(S:String;FD:FilePtr;
                 var Z:FrmlPtr;var FTyp:char;
                 var WasError:boolean;var Pos:word):ScreenStr; assembler;
  asm  mov bx,42*4; call _CallDML end;
function _RunReal(Z:FrmlPtr;FD:FilePtr):float; assembler;
  asm  mov bx,43*4; call _CallDML end;
function _RunStr(Z:FrmlPtr;FD:FilePtr):string; assembler;
  asm  mov bx,44*4; call _CallDML end;
function _RunBool(Z:FrmlPtr;FD:FilePtr):boolean; assembler;
  asm  mov bx,45*4; call _CallDML end;
function _PrTab(N:word):string; assembler;
  asm  mov bx,51*4; call _CallDML end;

function Normalize(L:longint):pointer; assembler;
asm
  mov dx,L[2].word; mov cl,12; shl dx,cl;
  mov bx,L.word; mov ax,bx; and ax,000FH;
  mov cl,4; shr bx,cl; add dx,bx;
end;

function AbsAdr(P:pointer):longint; assembler;
asm
  mov ax,P[2].word; mov dx,ax; mov cl,12; shr dx,cl;
  mov cl,4; shl ax,cl; add ax,P.word; adc dx,0
end;

procedure _OpenDML(UserHeapSize:longint);
var p:pointer; n:longint; Attr,err:word; s:string[6];
label 1;
begin
  OldHeapPtr:=HeapPtr;
1:n:=maxavail-UserHeapSize;
  if n>0 then begin if n>$FFFF then n:=$FFFF;GetMem(p,n);goto 1 end;
  s:=getenv('DMLADDR'); if s='' then begin
    writeln('start a program using FANDDML only by exec() in a FAND-procedure');
    readln; halt end;
  if s[1]=' ' then s[1]:='0'; val(s,n,err);
  if n=0 then begin
    writeln('program using FANDDML started from FAND version without DML support');
    readln; halt end;
  _CallDML:=Normalize(n);
  _init(WindMin,WindMax,Attr,OldHeapPtr,HeapPtr,DMLVersion,PrefixSeg,_inst,nil);
  halt(1);                         {= 8 bytes, return after halt}
  TextAttr:=Attr;
end;

var ExitSave:pointer;
procedure DMLExit; far;
  begin if _CallDML<>nil then _exit;
        ExitProc:=ExitSave end;


begin
  ExitSave:=ExitProc; ExitProc:=@DMLExit; _CallDML:=nil;
end.