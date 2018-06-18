{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit runprolg;

interface

uses base,access,rdrun;

type
  PMemBlkHd = ^TMemBlkHd;
  TMemBlkHd = record Chain:PMemBlkHd; Sz:word end;
  PMemory   = ^TMemory;
  TMemory   = object
    {Hd: TMemBlkHd;}
    RestSz:word; CurLoc:pointer; CurBlk:PMemBlkHd; FreeList:PMemBlkHd;
    procedure Init;
    function Get(Sz:word):pointer;
    function Mark:pointer;
    procedure Release(P:pointer);
    function StoreStr(const s:string):PString;
    function Alloc(Sz:word):pointer;
    procedure Free(P:pointer;Sz:word);
  end;

var
  FreeMemList:PMemBlkHd;
  Mem1,Mem2,Mem3:TMemory;  { copy-stack, instance-stack, db-heap}
const  ProlgCallLevel:word=0;


procedure RunProlog(Pos:RdbPos;PredName:StringPtr);
function SaveDb(DbOfs:word{PDatabase};AA:longint):LongStrPtr;

implementation

uses drivers,obaseww,oaccess,runfrml,runproc,sort {$ifdef FandSQL}
     ,channel  {$endif}
     ,compile,editor,rdprolg;

procedure Ovr; far;
assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push cs; push ax;
end;

const
  MaxPackedPredLen = 4000;

type
  PTerm    = ^TTerm;
  TTerm     = record
    case Fun:byte of
      0     : (Arity:byte; Arg:array [0..2] of PTerm);
      _IntT : (II:integer);
      _RealT: (RR:float);
      _StrT : (SS:string);
      _LongStrT:(Pos:longint);
      _ListT: (Elem,Next:PTerm);
  end;
  PInstance = ^TInstance;
  TInstance = record
    Pred:word{PPredicate}; PrevInst,RetInst:PInstance; RetCmd:word{PCommand};
    NextBranch{PDbBranch,PFileScan}:PBranch;
    RetBranch:word{PBranch}; StkMark:pointer; WMark:longint; CallLevel:word;
    Vars:array[0..6] of PTerm;
  end;
  PFileScan = ^TFileScan;
  TFileScan = record IRec,Count:longint end;

var CurrInst:PInstance;
    PackedTermPtr:Pchar; PTPMaxOfs:word;
    TrcLevel,CallLevel:integer;
    LexemList:PTerm;

function Trace:boolean; assembler;
asm xor ax,ax; mov bx,TrcLevel; cmp bx,ax; je @1;
    cmp bx,CallLevel; jb @1; mov ax,1; { if TrcLevel<>0 and >=CallLevel }
@1: end;
procedure SetCallLevel(Lv:word); assembler;
asm mov ax,Lv; mov CallLevel,ax; { CallLevel=lv  if =0 then TrcLevel=0 }
    cmp ax,0; jne @1; mov TrcLevel,ax;
@1: end;
procedure WaitC;
var c:word;
begin c:=readkey; if (c=_ESC_) and PromptYN(21) then GoExit  end;

{  T M E M O R Y  =========================================================}

const MemBlockSize = 8192;

procedure TMemory.Init;
begin
  CurBlk:=nil; CurLoc:=nil; RestSz:=0; FreeList:=nil;
end;

function TMemory.Get(Sz:word):pointer;
var p:pointer; p1,p2:PMemBlkHd; n:word;
label 1;
begin
  if Sz>RestSz then begin
    n:=Sz+sizeof(TMemBlkHd); if n<MemBlockSize then n:=MemBlockSize;
    p2:=PMemBlkHd(@FreeMemList); p1:=FreeMemList; while p1<>nil do begin
      if p1^.Sz>=n then begin p2^.Chain:=p1^.Chain; goto 1 end;
      p2:=p1; p1:=p1^.Chain end;
    p1:=GetStore2(n); p1^.Sz:=n;
1:  p1^.Chain:=CurBlk; CurBlk:=p1;
    CurLoc:=Ptr(Seg(p1^),Ofs(p1^)+sizeof(TMemBlkHd));
    RestSz:=p1^.Sz-sizeof(TMemBlkHd) end;
  p:=CurLoc; inc(PtrRec(CurLoc).Ofs,Sz); dec(RestSz,Sz);
  asm les di,p; mov al,0; mov cx,Sz; cld; rep stosb end;
  Get:=p;
end;
function TMemory.Mark:pointer;
begin
  Mark:=CurLoc;
end;
procedure TMemory.Release(P:pointer);   { only for pure stack }
var p1,p2:PMemBlkHd;
begin
  p1:=CurBlk;
  while PtrRec(p1).Seg<>PtrRec(p).Seg do begin
    p2:=p1^.Chain; p1^.Chain:=FreeMemList; FreeMemList:=p1; p1:=p2 end;
  CurBlk:=p1; CurLoc:=p;
  if p=nil then RestSz:=0 else
  RestSz:=p1^.Sz-(PtrRec(p).Ofs-(PtrRec(p1).Ofs+sizeof(TMemBlkHd)));
end;
function TMemory.StoreStr(const s:string):PString;
var p:pointer;
begin
  p:=Get(length(s)+1); move(s,p^,length(s)+1); StoreStr:=p;
end;
function TMemory.Alloc(Sz:word):pointer; { doesn't free once allocated blocks }
var p,p1,p2:PMemBlkHd;
begin
  Sz:=(Sz+7) and $fff8;
  p1:=PMemBlkHd(@FreeList); p:=FreeList; while p<>nil do begin
    if p^.Sz>=Sz then begin
      if p^.Sz>Sz then begin
	p2:=p; inc(PtrRec(p2).Ofs,Sz);
	p1^.Chain:=p2; p2^.Chain:=p^.Chain; p2^.Sz:=p^.Sz-Sz end
      else p1^.Chain:=p^.Chain;
      asm les di,p; mov al,0; mov cx,Sz; cld; rep stosb end;
      Alloc:=p; exit end;
    p1:=p; p:=p^.Chain end;
  Alloc:=Get(Sz);
end;
procedure TMemory.Free(P:pointer;Sz:word);
var p1,p2:PMemBlkHd; pp:PMemBlkHd absolute P;
begin
  Sz:=(Sz+7) and $fff8; p1:=PMemBlkHd(@FreeList); p2:=FreeList;
  while (PtrRec(p2).Seg<>0) and (PtrRec(p2).Seg<>PtrRec(P).Seg) do begin
    p1:=p2; p2:=p2^.Chain end;
  while (PtrRec(p2).Seg=PtrRec(P).Seg) and (PtrRec(p2).Ofs<PtrRec(P).Ofs) do begin
    p1:=p2; p2:=p2^.Chain end;
  if (PtrRec(P).Seg=PtrRec(p2).Seg) and (PtrRec(P).Ofs+Sz=PtrRec(p2).Ofs) then begin
    pp^.Sz:=Sz+p2^.Sz; pp^.Chain:=p2^.Chain end
  else begin pp^.Sz:=Sz; pp^.Chain:=p2 end;
  if (PtrRec(p1).Seg=PtrRec(P).Seg) and (PtrRec(p1).Ofs+p1^.Sz=PtrRec(P).Ofs) then begin
    p1^.Sz:=p1^.Sz+pp^.Sz; p1^.Chain:=pp^.Chain end
  else p1^.Chain:=pp;
end;



{  T D O M A I N  =========================================================}

function GetFunDcl(D:word;I:byte):PFunDcl;
var fd:PFunDcl; fdofs:word absolute fd;
begin
  PtrRec(fd).Seg:=_Sg; fdofs:=PDomain(ptr(_Sg,D))^.FunDcl;
  while i>0 do begin dec(i); fdofs:=fd^.Chain end;
  GetFunDcl:=fd;
end;

{  T T E R M  =============================================================}


function GetIntTerm(I:integer):PTerm;
var t:PTerm;
begin
  t:=Mem1.Get(1+sizeof(integer)); with t^ do begin Fun:=_IntT; II:=I end;
  GetIntTerm:=t;
end;
function GetRealTerm(R:float):PTerm;
var t:PTerm;
begin
  t:=Mem1.Get(1+sizeof(float)); with t^ do begin Fun:=_RealT; RR:=R end;
  GetRealTerm:=t;
end;
function GetBoolTerm(B:boolean):PTerm;
var t:PTerm;
begin
  t:=Mem1.Get(1+1); t^.Fun:=ord(B); GetBoolTerm:=t;
end;
function GetStringTerm(const S:string):PTerm;
var t:PTerm;
begin
  t:=Mem1.Get(1+1+length(S)); with t^ do begin
    Fun:=_StrT; move(S,SS,length(S)+1) end;
  GetStringTerm:=t;
end;
function GetLongStrTerm(N:longint):PTerm;
var t:PTerm;
begin
  t:=Mem1.Get(1+4); with t^ do begin Fun:=_LongStrT; Pos:=N end;
  GetLongStrTerm:=t;
end;
function GetListTerm(aElem:PTerm;aNext:PTerm):PTerm;
var t:PTerm;
begin
  t:=Mem1.Get(1+2*4); GetListTerm:=t; with t^ do begin
    Fun:=_ListT; Elem:=aElem; Next:=aNext end;
end;
function GetFunTerm(aFun:byte;aArity:byte):PTerm;
var t:PTerm;
begin
  t:=Mem1.Get(1+1+aArity*4); GetFunTerm:=t;
  with t^ do begin Fun:=aFun; Arity:=aArity end;
end;
procedure ChainList(var Frst; New:pointer); assembler;
asm  push ds; lds si,Frst; sub si,TTerm.Next;
@1:  cmp [si+2].TTerm.Next.word,0; je @2;
     lds si,[si].TTerm.Next;
     jmp @1;
@2:  les di,New; mov [si].TTerm.Next.word,di; mov [si+2].TTerm.Next.word,es;
     pop ds;
end;

var XXS:string;

function RdLongStr(Pos:longint):LongStrPtr;
var p:LongStrPtr; l:word;
begin
  p:=GetStore(2); SeekH(WorkHandle,Pos); ReadH(WorkHandle,2,l); p^.LL:=l;
  if l>0 then begin GetStore(l); ReadH(WorkHandle,l,p^.A) end; RdLongStr:=p;
end;
function WrLongStrLP(L:word;P:pointer):longint;
begin
  WrLongStrLP:=MaxWSize; SeekH(WorkHandle,MaxWSize); WriteH(WorkHandle,2,L);
  WriteH(WorkHandle,L,P^); inc(MaxWSize,L+2);
end;
function WrLongStr(S:LongStrPtr):longint;
begin
  WrLongStr:=WrLongStrLP(S^.LL,@S^.A);
end;

function RunLSExpr(TOfs:word):LongStrPtr; forward;
procedure RunSExpr(TOfs:word;var s:string); forward;
function RunRExpr(TOfs:word):float; forward;
function RunIExpr1(T:PPTerm):integer;
var s,s2:string; i,err,l:integer; ss:LongStrPtr;
label 1;
begin
  case t^.Op of
   _length:begin RunSExpr(t^.E1,s); i:=length(s) end;
   _val   :begin RunSExpr(t^.E1,s); val(s,i,err) end;
   _pos   :begin RunSExpr(t^.E1,s); ss:=RunLSExpr(t^.E2); l:=ss^.LL;
           i:=FindText(s,'',@ss^.A,l); if i>0 then i:=i-length(s);
           ReleaseStore(ss) end;
  end;
  RunIExpr1:=i;
end;
function RunIExpr(TOfs:word{PPTerm}):integer;
var t:PPTerm;
begin
  t:=ptr(_Sg,TOfs);
  if t^.Fun=_VarT then begin RunIExpr:=CurrInst^.Vars[t^.Idx]^.II; exit end;
  case t^.Op of
   _const:RunIExpr:=t^.II;
   '^'  : RunIExpr:=not RunIExpr(t^.E1);
   _and : RunIExpr:=RunIExpr(t^.E1) and RunIExpr(t^.E2);
   _or  : RunIExpr:=RunIExpr(t^.E1) or RunIExpr(t^.E2);
   '+'  : RunIExpr:=RunIExpr(t^.E1) + RunIExpr(t^.E2);
   '-'  : RunIExpr:=RunIExpr(t^.E1) - RunIExpr(t^.E2);
   '*'  : RunIExpr:=RunIExpr(t^.E1) * RunIExpr(t^.E2);
   '/'  : RunIExpr:=RunIExpr(t^.E1) div RunIExpr(t^.E2);
   _conv: RunIExpr:=trunc(RunRExpr(t^.E1));
   _min : RunIExpr:=mini(RunIExpr(t^.E1),RunIExpr(t^.E2));
   _max : RunIExpr:=maxi(RunIExpr(t^.E1),RunIExpr(t^.E2));
   _maxcol: RunIExpr:=TxtCols;
   _maxrow: RunIExpr:=TxtRows;
   else   RunIExpr:=RunIExpr1(t) end;
end;
function RunRExpr(TOfs:word{PPTerm}):float;
var t:PPTerm;
begin
  t:=ptr(_Sg,TOfs);
  if t^.Fun=_VarT then begin RunRExpr:=CurrInst^.Vars[t^.Idx]^.RR; exit end;
  case t^.Op of
   _const:RunRExpr:=t^.RR;
   '+'  : RunRExpr:=RunRExpr(t^.E1) + RunRExpr(t^.E2);
   '-'  : RunRExpr:=RunRExpr(t^.E1) - RunRExpr(t^.E2);
   '*'  : RunRExpr:=RunRExpr(t^.E1) * RunRExpr(t^.E2);
   '/'  : RunRExpr:=RunRExpr(t^.E1) / RunRExpr(t^.E2);
   _conv: RunRExpr:=RunIExpr(t^.E1) end;
end;
procedure RunSExpr1(T:PPTerm;var s:string);
var s2:string; tofs:word absolute T; t1ofs,l,l2:word; b:boolean;
begin
  l:=0;
  repeat
   b:=(t^.Fun=_StrT) and (t^.Op='+'); if b then t1ofs:=t^.E1 else t1ofs:=tofs;
   RunSExpr(t1ofs,s2); l2:=minw(length(s2),255-l);
   move(s2[1],s[l+1],l2); inc(l,l2); if b then tofs:=t^.E2;
  until not b;
  s[0]:=char(l);
end;
procedure RunSExpr(TOfs:word;var s:string);
var t:PPTerm; i,n,l:word; p:LongStrPtr; q:StringPtr;
label 1,2;
begin
  t:=ptr(_Sg,TOfs);
  if t^.Fun=_VarT then begin q:=@CurrInst^.Vars[t^.Idx]^.SS; goto 1 end else
  case t^.Op of
   _const: begin q:=@t^.SS;
1:         move(q^,s,length(q^)+1) end;
    '+'  : RunSExpr1(t,s);
   _conv : begin p:=RunLSExpr(t^.E1); l:=minw(p^.LL,255); s[0]:=char(l);
                 move(p^.A,s[1],l); ReleaseStore(p) end;
   _copy : begin RunSExpr(t^.E1,s); i:=RunIExpr(t^.E2); n:=RunIExpr(t^.E3);
           l:=length(s); i:=maxw(1,minw(i,l+1));
2:         n:=minw(n,l+1-i); s[0]:=char(n); move(s[i],s[1],n) end;
   _leadchar: begin RunSExpr(t^.E2,s); i:=1; l:=length(s); n:=l;
           while (i<=l) and (s[i]=char(t^.E1)) do inc(i); goto 2 end;
   _trailchar: begin RunSExpr(t^.E2,s); l:=length(s);
           while (l>0) and (s[l]=char(t^.E1)) do dec(l); s[0]:=char(l) end;
   _repeatstr: begin RunSExpr(t^.E1,s); n:=RunIExpr(t^.E2); l:=length(s); i:=0;
           while (n>0) and (i+l<=255) do begin
             dec(n); move(s[1],s[i+1],l); inc(i,l) end;
           s[0]:=char(i) end;
   _str  : str(RunIExpr(t^.E1),s);
  end;
end;
function RunLSExpr(TOfs:word):LongStrPtr;
var t:PPTerm; p,p2:LongStrPtr; l:word; s:StringPtr;
label 1;
begin
  t:=ptr(_Sg,TOfs);
  if t^.Fun=_VarT then p:=RdLongStr(CurrInst^.Vars[t^.Idx]^.Pos)
  else case t^.Op of
   _const: begin l:=length(t^.SS); p:=GetStore(l+2); p^.LL:=l;
           move(t^.SS[1],p^.A,l) end;
   '+':    begin p:=RunLSExpr(t^.E1); p2:=RunLSExpr(t^.E2); l:=p2^.LL;
           p^.LL:=p^.LL+l; move(p2^.A,p2^.LL,l);  ReleaseAfterLongStr(p) end;
   _conv:  begin p:=GetStore(257); s:=ptr(PtrRec(p).Seg,PtrRec(p).Ofs+1);
           RunSExpr(t^.E1,s^); p^.LL:=length(s^); ReleaseAfterLongStr(p) end;
   end;
  RunLSExpr:=p;
end;
function UnifyTermsCC(T1,T2:PTerm):boolean;
var i:integer; p1,p2:LongStrPtr;
begin
  UnifyTermsCC:=true;
  if T2=nil then begin if T1<>nil then UnifyTermsCC:=false; exit end;
  if (T1=nil) or (T1^.Fun<>T2^.Fun) then UnifyTermsCC:=false
  else case T2^.Fun of
   _IntT: UnifyTermsCC:=T1^.II=T2^.II;
   _RealT: UnifyTermsCC:=T1^.RR=T2^.RR;
   _StrT: UnifyTermsCC:=T1^.SS=T2^.SS;
   _LongStrT:begin p1:=RdLongStr(T1^.Pos); p2:=RdLongStr(T2^.Pos);
             UnifyTermsCC:=EquLongStr(p1,p2); ReleaseStore(p1) end;
   _ListT:UnifyTermsCC:=UnifyTermsCC(T1^.Elem,T2^.Elem) and
                      UnifyTermsCC(T1^.Next,T2^.Next);
   else for i:=0 to integer(T1^.Arity)-1 do
  	  if not UnifyTermsCC(T1^.Arg[i],T2^.Arg[i]) then begin
            UnifyTermsCC:=false; exit end end;
end;
function UnifyTermsCV(T1:PTerm; T2Ofs:word{PPTerm}):boolean;
function UnifyVList(var TT1:PTerm; T2:PPTerm):boolean;
var t2ofs:word absolute T2; t,t1:PTerm;
label 1;
begin
  t1:=TT1; UnifyVList:=false; while t2ofs<>0 do
    case T2^.Fun of
     _VarT:begin
           if T2^.Bound then begin
             t:=CurrInst^.Vars[T2^.Idx]; while t<>nil do begin
               if (t1=nil) or not UnifyTermsCC(t1^.Elem,t^.Elem) then exit;
             t:=t^.Next; t1:=t1^.Next end end
           else begin CurrInst^.Vars[T2^.Idx]:=t1; t1:=nil end;
           goto 1 end;
     _UnderscT: begin t1:=nil; goto 1 end;
     else if (t1=nil) or not UnifyTermsCV(t1^.Elem,T2^.Elem) then exit;
          t1:=t1^.Next; t2ofs:=T2^.Next end;
1:UnifyVList:=true; TT1:=t1;
end;
var i:integer; t2:PPTerm; p,p2:LongStrPtr;
label 1;
begin
  UnifyTermsCV:=true;
  if T2Ofs=0 then begin if T1<>nil then UnifyTermsCV:=false; exit end;
  t2:=ptr(_Sg,T2Ofs);
  case t2^.Fun of
    _VarT: if t2^.Bound then
             UnifyTermsCV:=UnifyTermsCC(T1,CurrInst^.Vars[T2^.Idx])
	   else CurrInst^.Vars[T2^.Idx]:=T1;
    _UnderscT:;
    else if (T1=nil) or (T1^.Fun<>T2^.Fun) then goto 1
      else case T2^.Fun of
	_IntT: UnifyTermsCV:=T1^.II=RunIExpr(T2Ofs);
        _RealT: UnifyTermsCV:=T1^.RR=RunRExpr(T2Ofs);
	_StrT: if T2^.Op=_const then UnifyTermsCV:=T1^.SS=T2^.SS else begin
               RunSExpr(T2Ofs,XXS); UnifyTermsCV:=T1^.SS=XXS end;
        _LongStrT:begin p:=RdLongStr(T1^.Pos); p2:=RunLSExpr(T2Ofs);
             UnifyTermsCV:=EquLongStr(p,p2); ReleaseStore(p) end;
	_ListT: begin while T2^.Op='+' do begin
               if not UnifyVList(T1,ptr(_Sg,T2^.E1)) then goto 1; T2Ofs:=T2^.E2 end;
             if not UnifyVList(T1,T2) or (T1<>nil) then goto 1 end;
	else for i:=0 to integer(T1^.Arity)-1 do
  	  if not UnifyTermsCV(T1^.Arg[i],T2^.Arg[i]) then begin
1:          UnifyTermsCV:=false; exit end end end;
end;
function FindInCList(tEl:PTerm;t:PTerm):boolean;
begin
  FindInCList:=true; while t<>nil do begin
    if UnifyTermsCC(tEl,t^.Elem) then exit; t:=t^.Next end;
  FindInCList:=false;
end;
function CopyCList(T:PTerm):PTerm;
var root,t1,prev:PTerm;
begin
  if T=nil then begin CopyCList:=nil; exit end;  root:=nil;
  repeat t1:=Mem1.Get(1+2*4); t1^.Fun:=_ListT; t1^.Elem:=T^.Elem;
    if root=nil then root:=t1 else prev^.Next:=t1;
    prev:=t1; T:=T^.Next until T=nil;
  CopyCList:=root;
end;
function CopyTerm(TOff:word{PPTerm}):PTerm;
function CopyVList(T:PPTerm;Cpy:boolean):PTerm;
var tofs:word absolute T; root,t1,prev:PTerm;
label 1;
begin
  if tofs=0 then begin CopyVList:=nil; exit end; root:=nil;
1:if T^.Fun=_VarT then begin
    t1:=CurrInst^.Vars[T^.Idx]; if Cpy then t1:=CopyCList(t1) end
  else begin
    t1:=Mem1.Get(1+2*4); t1^.Fun:=_ListT; t1^.Elem:=CopyTerm(T^.Elem) end;
  if root=nil then root:=t1 else prev^.Next:=t1;
  if (T^.Fun<>_VarT) then begin
    prev:=t1; tofs:=T^.Next; if tofs<>0 then goto 1 end;
  CopyVList:=root;
end;
var t,t1:PPTerm; tofs:word absolute t; t2:PTerm; i:integer; p:LongStrPtr;
begin
  if TOff=0 then begin CopyTerm:=nil; exit end;
  t:=ptr(_Sg,TOff);
  case t^.Fun of
    _IntT : CopyTerm:=GetIntTerm(RunIExpr(TOfs));
    _RealT: CopyTerm:=GetRealTerm(RunRExpr(TOfs));
    _StrT : if t^.Op=_const then CopyTerm:=GetStringTerm(t^.SS) else
            begin RunSExpr(TOff,XXS); CopyTerm:=GetStringTerm(XXS) end;
    _LongStrT:begin p:=RunLSExpr(TOff); CopyTerm:=GetLongStrTerm(WrLongStr(p));
            ReleaseStore(p) end;
    _VarT : CopyTerm:=CurrInst^.Vars[T^.Idx];
    _ListT: begin t2:=nil; while t^.Op='+' do begin
              ChainList(t2,CopyVList(ptr(_Sg,t^.E1),true)); tofs:=t^.E2 end;
            ChainList(t2,CopyVList(t,false)); CopyTerm:=t2 end;
    else    t2:=GetFunTerm(T^.Fun,T^.Arity);
       for i:=0 to integer(T^.Arity)-1 do t2^.Arg[i]:=CopyTerm(T^.Arg[i]);
       CopyTerm:=t2 end;
end;
procedure PackTermC(T:PTerm);
var p:Pchar absolute PackedTermPtr; i:integer; n:word; wp:WordPtr;
label 1;
begin
  if PtrRec(p).Ofs>=PTPMaxOfs then RunError(1527);
1:if T=nil { [] } then begin WordPtr(p)^:=0; inc(p,2) end else
  case T^.Fun of
    _IntT : begin IntegerPtr(P)^:=T^.II; inc(p,2) end;
    _RealT: begin FloatPtr(p)^:=T^.RR; inc(p,sizeof(float)) end;
    _StrT : begin
            n:=length(t^.SS)+1; if PtrRec(p).Ofs+n>=PTPMaxOfs then RunError(1527);
            move(t^.SS,p^,n); inc(p,n) end;
    _LongStrT: RunError(1543);
    _ListT: begin wp:=WordPtr(p); inc(p,2);
	    n:=0; while T<>nil do begin PackTermC(T^.Elem); T:=T^.Next; inc(n) end;
            wp^:=n end;
    else    p^:=char(T^.Fun); inc(p);
            for i:=0 to integer(T^.Arity)-1 do PackTermC(T^.Arg[i]) end;
end;
procedure PackTermV(TOff:word{PPTerm}); forward;
function PackVList(T:PPTerm):word;
var tofs:word absolute T; n:word; t1:PTerm;
label 1;
begin
  n:=0; repeat
    if t^.Fun=_VarT then begin
      t1:=CurrInst^.Vars[T^.Idx]; while t1<>nil do begin
        PackTermC(t1^.Elem); t1:=t1^.Next; inc(n) end;
      goto 1 end;
    PackTermV(t^.Elem); inc(n); tofs:=T^.Next;
  until tofs=0;
1:PackVList:=n;
end;
procedure PackTermV(TOff:word{PPTerm});
var p:Pchar absolute PackedTermPtr; i:integer; n:word; wp:WordPtr;
    t:PPTerm; tofs:word absolute t; t1:PTerm;
label 1,2;
begin
  if PtrRec(p).Ofs>=PTPMaxOfs then RunError(1527); t:=ptr(_Sg,TOff);
1:if tofs=0 { [] } then begin WordPtr(p)^:=0; inc(p,2) end else
  case T^.Fun of
    _VarT : PackTermC(CurrInst^.Vars[T^.Idx]);
    _IntT : begin IntegerPtr(P)^:=RunIExpr(tofs); inc(p,2) end;
    _RealT: begin FloatPtr(p)^:=RunRExpr(tofs); inc(p,sizeof(float)) end;
    _StrT : begin RunSExpr(tofs,XXS);
            n:=length(XXS)+1; if PtrRec(p).Ofs+n>=PTPMaxOfs then RunError(1527);
            move(XXS,p^,n); inc(p,n) end;
    _LongStrT: RunError(1543);
    _ListT: begin wp:=WordPtr(p); inc(p,2); wp^:=0;
            while t^.Op='+' do begin
              inc(wp^,PackVList(ptr(_Sg,t^.E1))); tofs:=t^.E2 end;
            inc(wp^,PackVList(t)) end;
    else    p^:=char(t^.Fun); inc(p);
            for i:=0 to integer(t^.Arity)-1 do PackTermV(t^.Arg[i]) end;
end;
function GetPackedTerm(T:PTerm):LongStrPtr;
var pt:Pchar absolute PackedTermPtr;  A:array[1..MaxPackedPredLen] of char;
    s:LongStrPtr; n:word;
begin
  pt:=Pchar(@A); PTPMaxOfs:=ofs(A)+MaxPackedPredLen-2;
  PackTermC(T); n:=PtrRec(pt).Ofs-ofs(A);
  s:=GetStore(2+n); s^.LL:=n; move(A,s^.A,n);
  GetPackedTerm:=s;
end;
function UnpackTerm(D:word):PTerm;
var p:Pchar absolute PackedTermPtr;
    i:integer; n:word; q:Pchar; t,tPrev,t1:PTerm; f:PFunDcl;
begin
  case PDomain(ptr(_Sg,D))^.Typ of
   _IntD : begin t:=GetIntTerm(IntegerPtr(p)^); inc(p,2) end;
   _RealD: begin t:=GetRealTerm(FloatPtr(p)^); inc(p,sizeof(float)) end;
   _StrD : begin t:=GetStringTerm(StringPtr(p)^); inc(p,ord(p^)+1) end;
   _LongStrD: RunError(1543);
   _ListD: begin n:=WordPtr(p)^; inc(p,2); T:=nil; while n>0 do begin
             t1:=GetListTerm(UnpackTerm(PDomain(ptr(_Sg,D))^.ElemDom),nil);
             if t=nil then t:=t1 else tPrev^.Next:=t1; tPrev:=t1; dec(n) end end;
   else    n:=ord(p^); inc(p); f:=getFunDcl(D,n); t:=GetFunTerm(n,f^.Arity);
	   for i:=0 to integer(f^.Arity)-1 do t^.arg[i]:=UnpackTerm(f^.Arg[i]) end;
  UnPackTerm:=t;
end;
function PrintPackedTerm(P:Pchar;D:word):Pchar;
var i,n:integer; f:PFunDcl;
begin
  case PDomain(ptr(_Sg,D))^.Typ of
   _IntD : begin write(IntegerPtr(p)^); inc(p,2) end;
   _RealD: begin write(FloatPtr(p)^); inc(p,sizeof(float)) end;
   _StrD : begin write('''',StringPtr(p)^,''''); inc(p,ord(p^)+1) end;
   _ListD: begin write('['); n:=WordPtr(p)^; inc(p,2); for i:=1 to n do begin
             if i>1 then write(',');
             p:=PrintPackedTerm(p,PDomain(ptr(_Sg,D))^.ElemDom) end;
           write(']') end;
   else    f:=getFunDcl(D,ord(p^)); inc(p);
           write(StringPtr(ptr(_Sg,f^.Name))^); if f^.Arity>0 then
           begin write('('); for i:=1 to f^.Arity do begin
	     if i>1 then write(','); p:=PrintPackedTerm(p,f^.Arg[i-1]) end;
  	     write(')') end end;
  PrintPackedTerm:=p;
end;
procedure PrintPackedPred(Q:Pchar;POfs:word{PPredicate});
var i,n:integer; p:PPredicate;
begin
  p:=ptr(_Sg,POfs); write('CALL assert(',PString(ptr(_Sg,p^.Name))^);
  n:=p^.Arity; if n>0 then begin write('(');
    for i:=1 to n do begin
      if i>1 then write(','); inc(q,2); q:=PrintPackedTerm(q,p^.Arg[i-1]) end;
    write(')') end;
  writeln(')'); waitC;
end;
procedure PrintTerm(T:PTerm;DOfs:word);
var fd:PFunDcl; i:word; p:LongStrPtr; d:PDomain;
label 1;
begin
  if T=nil then write('[]') else
  case T^.Fun of
    _IntT : write(T^.II);
    _RealT: write(T^.RR);
    _StrT : write('''',T^.SS,'''');
    _LongStrT: begin p:=RdLongStr(T^.Pos); write('''');
            for i:=1 to p^.LL do write(p^.A[i]); write(''''); ReleaseStore(p) end;
    _ListT: begin write('['); d:=ptr(_Sg,DOfs); i:=0;
1:          PrintTerm(T^.Elem,d^.ElemDom); T:=T^.Next;
   	    if T<>nil then begin write(','); inc(i);
              if (i=3) and (d^.Name='L_Lexem') then write('...') else
              goto 1 end;
  	    write(']') end;
    else    fd:=GetFunDcl(DOfs,T^.Fun);
        write(StringPtr(ptr(_Sg,fd^.Name))^); if T^.Arity=0 then exit;
	write('('); for i:=0 to T^.Arity-1 do begin
	  if i>0 then write(','); PrintTerm(T^.Arg[i],fd^.Arg[i]) end;
	write(')') end;
end;

function LenDbEntry(S:LongStrPtr;Arity:integer):word;
var n:word; i:integer;
begin
  n:=0; for i:=1 to Arity do begin
    inc(n,S^.LL+2); inc(PtrRec(S).Ofs,S^.LL+2) end;
  LenDbEntry:=n;
end;
function SaveDb(DbOfs:word{PDatabase};AA:longint):LongStrPtr;
var n,arity:word; l:longint; s:LongStrPtr; q:Pchar; x:boolean;
    p:PPredicate; pofs:word absolute p; b:PDbBranch; db:PDatabase;
begin
  s:=GetStore(2); db:=ptr(_Sg,DbOfs); p:=ptr(_Sg,Db^.Pred); x:=AA<>0;
  while pofs<>0 do begin
  if (p^.Opt and _FandCallOpt)=0 then begin
    arity:=p^.Arity;
    if not x then begin
      StoreStr(PString(ptr(_Sg,p^.Name))^); BytePtr(GetStore(1))^:=arity end;
    b:=PDbBranch(p^.Branch);
    while b<>nil do begin n:=LenDbEntry(LongStrPtr(@b^.LL),arity);
      q:=GetStore(n+1); if x and (AbsAdr(HeapPtr)-AA>MaxLStrLen) then OldError(544);
      q[0]:=#1; move(b^.LL,q[1],n); b:=b^.Chain end;
    BytePtr(GetStore(1))^:=0 end;
  pofs:=p^.ChainDb end;
  l:=AbsAdr(HeapPtr)-AbsAdr(s)-2; s^.LL:=l; if l>MaxLStrLen then begin
    SetMsgPar(Db^.Name); RunError(1532) end;
  SaveDb:=s;
end;
procedure ConsultDb(S:LongStrPtr;DbOfs:word{PDatabase});
var n:word; q:Pchar; p:PPredicate; pofs:word absolute p;
    b:PDbBranch; db:PDatabase;
label 1;
begin
  q:=Pchar(@S^.A); db:=ptr(_Sg,DbOfs); p:=ptr(_Sg,Db^.Pred);
  while pofs<>0 do begin
  if (p^.Opt and _FandCallOpt)=0 then begin
    if PtrRec(S).Seg<>_Sg then begin
      if StringPtr(q)^<>PString(ptr(_Sg,p^.Name))^ then goto 1; inc(q,ord(q^)+1);
      if ord(q^)<>p^.Arity then goto 1; inc(q) end;
    while q[0]=#1 do begin inc(q); n:=LenDbEntry(LongStrPtr(q),p^.Arity);
      b:=Mem3.Alloc(n+4); ChainLast(p^.Branch,b); move(q[0],b^.LL,n);
      inc(q,n) end;
    if q[0]<>#0 then goto 1; inc(q) end;
  pofs:=p^.ChainDb end;
  if S^.LL<>AbsAdr(q)-AbsAdr(S)-2 then begin
1:  SetMsgPar(Db^.Name); RunError(1533) end;
end;
function Vokal(C:char):boolean;
begin
  Vokal:=CurrToKamen(C) in ['a','†','Ñ','e','Ç','à','i','°','o','¢','ì','î','u','£','ñ','Å','y','ò'];
end;
function IsUpper(C:char):boolean;
begin
  IsUpper:=(C<>'.') and (C=UpcCharTab[C]);
end;
function Abbrev(const S:string):string;
var t:string; i,j:integer;
label 1,2,9;
begin
  t[0]:=#0; i:=length(S); while i>0 do begin
    if S[i]=' ' then begin
      while S[i]=' ' do begin dec(i); if i=0 then goto 9 end;
      if length(t)>0 then t:=' '+t end;
    j:=i; if IsUpper(S[i]) then goto 1;
    while Vokal(S[i]) do begin
      dec(i); if (i=0) or (S[i] in ['.',' ']) then goto 2 end;
    while not Vokal(S[i]) do begin
      dec(i); if (i=0) or (S[i] in ['.',' ']) then goto 2 end;
    while Vokal(S[i]) do begin
      dec(i); if (i=0) or (S[i] in ['.',' ']) then goto 2 end;
    j:=i; t:='.'+t;
1:  while (i>0) and not(S[i] in ['.',' ']) do dec(i);
2:  t:=copy(S,i+1,j-i)+t;
  end;
  if length(t)=length(s) then begin i:=1; while i<length(t)-1 do begin
    if (t[i]='.') and (t[i+1]=' ') then delete(t,i+1,1) else inc(i) end end;
9:Abbrev:=t;
end;
function NextFD(FD:FileDPtr):FileDPtr;
var r:RdbDPtr;
label 1;
begin
  if fd=nil then begin r:=CRdb; fd:=r^.FD end else r:=FD^.ChptPos.R;{not .RDB}
1:FD:=FD^.Chain;
  if FD=nil then begin r:=r^.ChainBack;
    if r<>nil then begin FD:=r^.FD; goto 1 end end
  else if (FD^.Typ='0') or (FD^.ChptPos.R=nil) then goto 1;
  NextFD:=FD;
end;
function FindFD(const FDName:string):FileDPtr;
var fd:FileDPtr;
begin
  fd:=nil; repeat fd:=NextFD(fd) until (fd=nil) or SEquUpcase(fd^.Name,FDName);
  FindFD:=fd;
end;
function Pound(const s:string):string;
begin
  if s='' then Pound:='@' else Pound:=s;
end;
function RunBuildIn:boolean;
var l,l1,l2,l3,n,m,w:word; s:string; p,p1,p2,p3:LongStrPtr;
    r:RdbDptr; fd:FileDPtr; f:FieldDPtr; k:KeyDPtr; kf:KeyFldDPtr;
    t1,t2,tprev,t,root:PTerm; ld:LinkDPtr; mask:StringPtr; i,err:integer;
    c:PCommand; b:boolean; q:PInstance; pos:RdbPos;
label 1,2,3;
begin with CurrInst^ do begin
  c:=ptr(_Sg,RetCmd); w:=c^.InpMask;
  case PPredicate(ptr(_Sg,Pred))^.LocVarSz of
  _NextLexP: if LexemList<>nil then LexemList:=LexemList^.Next;
  _GetLexP: Vars[0]:=LexemList;
  _ConcatP : case w of
    7{iii}: if Vars[0]^.SS+Vars[1]^.SS<>Vars[2]^.SS then goto 1;
    3{iio}: Vars[2]:=GetStringTerm(Vars[0]^.SS+Vars[1]^.SS);
    5{ioi}: begin l:=length(Vars[0]^.SS);
            if Vars[0]^.SS<>copy(Vars[2]^.SS,1,l) then goto 1;
            Vars[1]:=GetStringTerm(copy(Vars[2]^.SS,l+1,255)) end;
    6{oii}: begin l:=length(Vars[1]^.SS); l2:=length(Vars[2]^.SS);
            if Vars[1]^.SS<>copy(Vars[2]^.SS,l2-l+1,l) then begin
1:            NextBranch:=nil; RunBuildIn:=false; exit end;
            Vars[0]:=GetStringTerm(copy(Vars[2]^.SS,1,l2-l)) end;
    4{ooi}: begin n:=word(NextBranch); s:=Vars[2]^.SS;
            if n=0 then n:=length(s); if n=0 then goto 1; dec(n);
            Vars[0]:=GetStringTerm(copy(s,1,length(s)-n));
            Vars[1]:=GetStringTerm(copy(s,length(s)-n+1,n));
            word(NextBranch):=n end end;
  _MemP   : case w of
     3{ii}: begin t1:=Vars[0]; t2:=Vars[1]; while t2<>nil do begin
              if UnifyTermsCC(t1,t2^.Elem) then goto 3; t2:=t2^.Next end;
            goto 1 end;
     2{oi}: begin t2:=PTerm(NextBranch); if t2=nil then begin
              t2:=Vars[1]; if t2=nil then goto 1 end;
            Vars[0]:=t2^.Elem; NextBranch:=PBranch(t2^.Next) end end;
  _FandFileP:begin
           fd:=FileDPtr(NextBranch); if fd=nil then begin
             fd:=NextFD(nil); if fd=nil then goto 1 end;
           Vars[0]:=GetStringTerm(fd^.Name); s[0]:=#0; case fd^.Typ of
            '6':if fd^.IsSQLFile then s:='SQL';
            'X':s:='X'; 'D':s:='DBF'; '8':s:='DTA' end;
           Vars[1]:=GetStringTerm(s);
           Vars[2]:=GetStringTerm(fd^.ChptPos.R^.FD^.Name);
           CFile:=fd; SetCPathVol; Vars[3]:=GetStringTerm(CPath);
           NextBranch:=PBranch(NextFD(fd)) end;
  _FandFieldP:begin
           f:=FieldDPtr(NextBranch); if f=nil then begin
             fd:=FindFD(Vars[0]^.SS); if fd=nil then goto 1;
             f:=fd^.FldD end;
           if w=3{ii} then begin
             while (f<>nil) and not SEquUpcase(f^.Name,Vars[1]^.SS) do f:=f^.Chain;
             if f=nil then goto 1 end
           else Vars[1]:=GetStringTerm(f^.Name);
           Vars[2]:=GetStringTerm(f^.Typ);
           m:=0; l:=f^.L; if f^.Typ='F' then begin m:=f^.M; dec(l);
             if m>0 then dec(l,m+1) end;
           Vars[3]:=GetIntTerm(l); Vars[4]:=GetIntTerm(m);
           m:=f^.Flg; if (f^.Typ in ['N','A']) then m:=m or (f^.M shl 4);
           Vars[5]:=GetIntTerm(m);
           if (f^.Flg and f_Mask<>0) then Mask:=FieldDMask(f)
           else Mask:=StringPtr(@EmptyStr);
           Vars[6]:=GetStringTerm(Mask^);
           if w=3 then NextBranch:=nil else NextBranch:=PBranch(f^.Chain) end;
  _FandKeyP:begin
           k:=KeyDPtr(NextBranch); if k=nil then begin
             fd:=FindFD(Vars[0]^.SS); if fd=nil then goto 1;
             k:=fd^.Keys; if k=nil then goto 1 end;
           Vars[1]:=GetStringTerm(Pound(k^.Alias^));
           Vars[2]:=GetBoolTerm(k^.Intervaltest);
           Vars[3]:=GetBoolTerm(k^.Duplic); NextBranch:=PBranch(k^.Chain) end;
  _FandLinkP:begin
           ld:=LinkDPtr(NextBranch); if ld=nil then begin
             ld:=LinkDRoot; while (ld<>nil) and not
               SEquUpcase(ld^.FromFD^.Name,Vars[0]^.SS) do ld:=ld^.Chain;
             if ld=nil then goto 1 end;
           fd:=ld^.FromFD;
           if w=3{ii} then begin
             while (ld<>nil) and ((fd<>ld^.FromFD) or
             not SEquUpcase(ld^.RoleName,Vars[1]^.SS)) do ld:=ld^.Chain;
             if ld=nil then goto 1;
             Vars[2]:=GetStringTerm(ld^.ToFD^.Name) end
           else if w=5{ioi} then begin
             while (ld<>nil) and ((fd<>ld^.FromFD) or
             not SEquUpcase(ld^.ToFD^.Name,Vars[2]^.SS)) do ld:=ld^.Chain;
             if ld=nil then goto 1;
             Vars[1]:=GetStringTerm(ld^.RoleName) end
           else begin
             Vars[1]:=GetStringTerm(ld^.RoleName);
             Vars[2]:=GetStringTerm(ld^.ToFD^.Name) end;
           Vars[3]:=GetStringTerm(Pound(ld^.ToKey^.Alias^));
           s[0]:=#0; k:=fd^.Keys; while k<>nil do begin
             if k^.IndexRoot=ld^.IndexRoot then s:=k^.Alias^; k:=k^.Chain end;
           Vars[4]:=GetStringTerm(Pound(s));
           n:=ld^.MemberRef; if ld^.IndexRoot<>0 then inc(n,4);
           Vars[5]:=GetIntTerm(n);
           if w=3 then ld:=nil else
           repeat ld:=ld^.Chain until (ld=nil) or (ld^.FromFD=fd);
           NextBranch:=PBranch(ld) end;
  _FandKeyFieldP:begin
           kf:=KeyFldDPtr(NextBranch); if kf=nil then begin
             fd:=FindFD(Vars[0]^.SS); if fd=nil then goto 1;
             k:=fd^.Keys; while (k<>nil) and not SEquUpcase(Pound(k^.Alias^),Vars[1]^.SS) do k:=k^.Chain;
             if k=nil then goto 1; kf:=k^.KFlds end;
           Vars[2]:=GetStringTerm(kf^.FldD^.Name); Vars[3]:=GetBoolTerm(kf^.CompLex);
           Vars[4]:=GetBoolTerm(kf^.Descend); NextBranch:=PBranch(kf^.Chain) end;
   _FandLinkFieldP:begin
           kf:=KeyFldDPtr(NextBranch); if kf=nil then begin
             ld:=LinkDRoot; while (ld<>nil) and (
                not SEquUpcase(ld^.FromFD^.Name,Vars[0]^.SS) or
                not SEquUpcase(ld^.RoleName,Vars[1]^.SS)) do ld:=ld^.Chain;
             if ld=nil then goto 1; kf:=ld^.Args end;
           Vars[2]:=GetStringTerm(kf^.FldD^.Name); NextBranch:=PBranch(kf^.Chain) end;
   _LenP:begin
           t1:=Vars[0]; n:=0; while t1<>nil do begin inc(n); t1:=t1^.Next end;
           Vars[1]:=GetIntTerm(n) end;
   _InvP:begin
           t1:=Vars[0]; t2:=nil; while t1<>nil do begin
              t2:=GetListTerm(t1^.Elem,t2); t1:=t1^.Next end;
           Vars[1]:=t2 end;
   _AddP:begin
           t1:=Vars[0]; t2:=Vars[1]; Vars[2]:=t2; while t2<>nil do begin
             if UnifyTermsCC(t1,t2^.Elem) then goto 3; t2:=t2^.Next end;
           Vars[2]:=GetListTerm(t1,Vars[1]) end;
   _DelP:begin
           q:=CurrInst; t1:=PTerm(NextBranch); if t1=nil then t1:=Vars[1];
2:         if t1=nil then goto 1; Vars[0]:=t1^.Elem; CurrInst:=RetInst;
           b:=UnifyTermsCV(t1^.Elem,PTermList(ptr(_Sg,c^.Arg))^.Elem);
           CurrInst:=q; if b then begin
             root:=nil; t:=Vars[1]; while t<>t1 do begin
               t2:=GetListTerm(t^.Elem,nil);
               if root=nil then root:=t2 else tprev^.Next:=t2;
               tprev:=t2; t:=t^.Next end;
             t1:=t1^.Next; if root=nil then root:=t1 else tprev^.Next:=t1;
             Vars[2]:=root; NextBranch:=PBranch(t1); goto 3 end;
           Mem1.Release(q^.StkMark); t1:=t1^.Next; goto 2 end;
   _UnionP:begin
           t1:=CopyCList(Vars[0]); root:=nil; t2:=Vars[1];
           while t2<>nil do begin
             if not FindInCList(t2^.Elem,t1) then begin
               t:=GetListTerm(t2^.Elem,nil);
               if root=nil then root:=t else tprev^.Next:=t; tprev:=t end;
             t2:=t2^.next end;
           ChainList(t1,root); Vars[2]:=t1 end;
   _MinusP:begin
           root:=nil; t1:=Vars[0]; t2:=Vars[1]; while t1<>nil do begin
             if not FindInCList(t1^.Elem,t2) then begin
               t:=GetListTerm(t1^.Elem,nil);
               if root=nil then root:=t else tprev^.Next:=t; tprev:=t end;
             t1:=t1^.next end;
           Vars[2]:=root end;
   _InterP:begin
           root:=nil; t1:=Vars[0]; t2:=Vars[1]; while t1<>nil do begin
             if FindInCList(t1^.Elem,t2) then begin
               t:=GetListTerm(t1^.Elem,nil);
               if root=nil then root:=t else tprev^.Next:=t; tprev:=t end;
             t1:=t1^.next end;
           Vars[2]:=root end;
   _AbbrevP:Vars[1]:=GetStringTerm(Abbrev(Vars[0]^.SS));
   _CallP:begin if not FindChpt('L',Vars[0]^.SS,false,pos) then begin
             SetMsgPar(Vars[0]^.SS); RunError(1554) end;
          RunProlog(pos,@Vars[1]^.SS); if EdBreak<>0 then goto 1 end;
  end;
3:RunBuildIn:=true;
end end;
procedure SyntxError(N:word;Ex:integer);
begin
  RdMsg(3000+N); EdRecKey:=MsgLine;
  LastExitCode:=Ex;
  GoExit;
end;
procedure AppendLex(var tPrev:PTerm;Pos,Typ:integer;const s:string);
var t:PTerm;
begin
  t:=GetFunTerm(0,3); t^.Arg[2]:=GetStringTerm(s);
  t^.Arg[0]:=GetIntTerm(Pos); t^.Arg[1]:=GetIntTerm(Typ);
  t:=GetListTerm(t,nil);
  if LexemList=nil then LexemList:=t else tPrev^.Next:=t; tPrev:=t;
end;
procedure LoadLex(S:LongStrPtr);
var l,i,n:word; p:Pchar; typ:integer; x:string; t,tPrev:PTerm;
label 1,11,2,3;
begin
  LexemList:=nil; l:=S^.LL; p:=Pchar(@S^.A);
1:if l=0 then begin AppendLex(tPrev,S^.LL,0,''); exit end;
  if p^<=' ' then begin inc(p); dec(l); goto 1 end;
  if p^='{' then begin n:=1;
11:  inc(p); dec(l); if n=0 then goto 1;
     if l=0 then SyntxError(503,S^.LL);
     if p^='{' then inc(n) else if p^='}' then dec(n); goto 11 end;
  i:=0;
  if IsLetter(p^) then begin typ:=1;
2:  inc(i); x[i]:=p^; inc(p); dec(l);
    if (l>0) and (i<255) then
      if IsLetter(p^) then goto 2 else
      if IsDigit(p^) or (p^='_') then begin typ:=2; goto 2 end end
  else if IsDigit(p^) then begin typ:=3;
3:  inc(i); x[i]:=p^; inc(p); dec(l);
    if (l>0) and (i<255) and IsDigit(p^) then goto 3 end
  else begin x[1]:=p^; inc(p); dec(l); typ:=0; i:=1 end;
  x[0]:=char(i); AppendLex(tPrev,S^.LL-l,typ,x); goto 1;
end;
procedure SetCFile(const Name:string);
var r:RdbDPtr;
begin
  r:=CRdb; while r<>nil do begin
    CFile:=r^.FD; while CFile<>nil do begin
      if SEquUpcase(Name,CFile^.Name) then exit; CFile:=CFile^.Chain end;
    r:=r^.ChainBack end;
  if SEquUpcase(Name,'CATALOG') then CFile:=CatFD else RunError(1539);
end;
procedure RetractDbEntry(Q:Pinstance;POfs:word{PPredicate};B:PDbBranch);
var b1:PDbBranch; p:PPredicate;
label 1;
begin
  p:=ptr(_Sg,POfs); b1:=PDbBranch(@P^.Branch);
1:if b1^.Chain<>nil then
    if b1^.Chain=B then begin
      b1^.Chain:=B^.Chain; while (Q<>nil) do begin
        if Q^.NextBranch=pointer(b) then Q^.NextBranch:=pointer(b^.Chain);
        Q:=Q^.PrevInst end end
    else begin b1:=b1^.Chain; goto 1 end;
  Mem3.Free(B,LenDbEntry(LongStrPtr(@B^.LL),P^.Arity)+4);
end;
procedure AppendPackedTerm(C:PCommand);
var pt:Pchar absolute PackedTermPtr; b:PDbBranch;
    A:array[1..MaxPackedPredLen] of char; n:word absolute A;
begin
  pt:=@A[3]; PTPMaxOfs:=ofs(A)+MaxPackedPredLen-2;
  PackTermV(C^.apTerm); n:=PtrRec(pt).Ofs-ofs(A);
  b:=Mem3.Alloc(4+n); move(A,b^.LL,n); ChainLast(CurrInst^.Vars[C^.apIdx],b);
end;
procedure UnpackAppendedTerms(C:PCommand);
var b,b1:PDbBranch; t:PTerm; pt:^PTerm;
begin
  pt:=@CurrInst^.Vars[C^.apIdx]; b:=PDbBranch(pt^); pt^:=nil;
  while b<>nil do begin
    PackedTermPtr:=@b^.A; t:=UnpackTerm(c^.apDom); ChainList(pt^,t);
    b1:=b; b:=b^.Chain; Mem3.Free(b1,b1^.LL+4) end;
end;
function RunCommand(COff:word{PCommand}):boolean;
var i1,i2:integer; r1,r2:float; res:char; i:word; t:PTerm;
    w:PWriteD; wofs:word absolute w; c:PCommand; cofs:word absolute c;
    p1:pointer; n:longint; s:LongStrPtr; md:LockMode;
label 1;
begin
  c:=ptr(_Sg,COff);
  case c^.Code of
    _WriteC: begin
             w:=ptr(_Sg,c^.WrD); while wofs<>0 do begin
             if w^.IsString then write(w^.SS) else PrintTerm(CurrInst^.Vars[w^.Idx],w^.Dom);
             wofs:=w^.Chain end;
             if c^.NL then writeln end;
    _CompC:  begin i:=c^.E1Idx; if c^.CompOp=_assign then
                CurrInst^.Vars[i]:=CopyTerm(c^.E2) else begin
             res:=_equ; case c^.Typ of
             _IntD: begin i1:=CurrInst^.Vars[i]^.II;
                    i2:=RunIExpr(c^.E2);
                    if i1<i2 then res:=_lt else if i1>i2 then res:=_gt end;
             _RealD:begin r1:=CurrInst^.Vars[i]^.RR;
                    r2:=RunRExpr(C^.E2);
                    if r1<r2 then res:=_lt else if r1>r2 then res:=_gt end
             else if not UnifyTermsCV(CurrInst^.Vars[i],c^.E2) then res:=_gt end;
             if (ord(res) and ord(c^.CompOp))=0 then begin RunCommand:=false; exit end;
             end end;
    _SaveC,_ConsultC,_LoadLexC: begin MarkStore(p1);
            CFile:=c^.FD; if CFile=nil then begin
              SetCFile(c^.Name); c^.FD:=CFile; PtrRec(c^.FldD).Seg:=PtrRec(CFile).Seg end;
            if c^.Code=_SaveC then begin
              md:=NewLMode(WrMode); if not LinkLastRec(CFile,n,true) then IncNRecs(1);
              DelTFld(c^.FldD); s:=SaveDb(c^.DbPred,0); longs_(c^.FldD,s);
              WriteRec(CFile^.NRecs) end
            else begin
              md:=NewLMode(RdMode); LinkLastRec(CFile,n,true); s:=_longs(c^.FldD);
              if c^.Code=_ConsultC then ConsultDb(s,c^.DbPred) else LoadLex(s) end;
            OldLMode(md); ReleaseStore(p1) end;
   _ErrorC: begin i1:=-1; i:=1; w:=ptr(_Sg,c^.WrD); while wofs<>0 do begin
             if w^.IsString then begin MsgPar[i]:=w^.SS; inc(i) end
             else begin t:=CurrInst^.Vars[w^.Idx];
               if PDomain(ptr(_Sg,w^.Dom))^.Typ=_IntD then i1:=t^.II
               else begin MsgPar[i]:=t^.SS; inc(i) end end;
             wofs:=w^.Chain end;
            if i1=-1 then begin
              i1:=0; if LexemList<>nil then i1:=LexemList^.Elem^.Arg[0]^.II end;
            SyntxError(c^.MsgNr,i1) end;
    _WaitC: waitC;
    _AppPkC: AppendPackedTerm(c);
    _AppUnpkC: UnpackAppendedTerms(c);
  end;
  RunCommand:=true;
end;
procedure CallFandProc;
var p:PPredicate; pd:InstrPtr; oldBP,ps:ProcStkPtr; i,n,w:word; t:PTerm;
    pp:Pchar absolute ps; s:LongStrPtr; d:PDomain; dofs:word absolute d;
    pt:pointer absolute PackedTermPtr; ss:StringPtr;
begin
  p:=ptr(_Sg,CurrInst^.Pred); PtrRec(d).Seg:=_Sg;
  pd:=ptr(_Sg,word(p^.Branch)); ps:=GetZStore(p^.LocVarSz);
  w:=p^.InpMask; if PtrRec(pd^.Pos.R).Seg=0 then begin
    PtrRec(pd^.Pos.R).Seg:=_Sg; if pd^.Pos.IRec=$ffff then
    if not FindChpt('P',StringPtr(pd^.Pos.R)^,false,pd^.Pos) then RunError(1037) end;
  for i:=1 to p^.Arity do with pd^.TArg[i] do begin
    PtrRec(Frml).Seg:=_Sg; dofs:=p^.Arg[i-1];
    t:=CurrInst^.Vars[i-1]; if (w and 1)<>0 then begin
      case FTyp of
        'R':if t^.Fun=_IntT then Frml^.R:=t^.II else Frml^.R:=t^.RR;
        'B':Frml^.B:=boolean(t^.Fun)
        else if Frml^.Op=_const then Frml^.S:=t^.SS else begin
          if d^.Typ=_LongStrD then s:=RdLongStr(t^.Pos)
          else s:=GetPackedTerm(t);
          LongintPtr(pp+Frml^.BPOfs)^:=TWork.Store(s);
          ReleaseStore(s) end end end;
    w:=w shr 1 end;
  ps^.ChainBack:=MyBP; oldBP:=MyBP; SetMyBP(ps);
  CallProcedure(pd);
  w:=p^.InpMask;
  for i:=1 to p^.Arity do with pd^.TArg[i] do with CurrInst^ do begin
    dofs:=p^.Arg[i-1];
    if Frml^.Op=_getlocvar then case FTyp of
      'S':begin if (w and 1)=0 then
           if d^.Typ=_StrD then Vars[i-1]:=GetStringTerm(RunShortStr(Frml))
           else begin s:=RunLongStr(Frml);
            if d^.Typ=_LongStrD then Vars[i-1]:=GetLongStrTerm(WrLongStr(s))
            else begin PackedTermPtr:=@s^.A; Vars[i-1]:=UnpackTerm(dofs) end;
            ReleaseStore(s) end;
          TWork.Delete(LongintPtr(Ptr(Seg(MyBP^),Ofs(MyBP^)+Frml^.BPOfs))^) end;
      'R':if d^.Typ=_IntD then Vars[i-1]:=GetIntTerm(RunInt(Frml))
          else Vars[i-1]:=GetRealTerm(RunReal(Frml));
      else Vars[i-1]:=GetBoolTerm(RunBool(Frml)) end;
    w:=w shr 1 end;
  SetMyBP(oldBP); ReleaseStore(ps);
end;
function SiCFile(SiOfs:word):PScanInf;
var si:PScanInf; fl:PFldList; flofs:word absolute fl;
begin
  si:=ptr(_sg,SiOfs); SiCFile:=si; CFile:=si^.FD; if CFile<>nil then exit;
  SetCFile(si^.Name); si^.FD:=CFile; fl:=ptr(_sg,si^.FL);
  while flofs<>0 do begin
    PtrRec(fl^.FldD).Seg:=PtrRec(CFile).Seg; flofs:=fl^.Chain end;
end;
procedure AssertFand(P:PPredicate;C:PCommand);
var fl:PFldList; flofs:word absolute fl; l:PTermList; lofs:word absolute l;
    d:PDomain; dofs:word absolute d;
    f:FieldDPtr; md:LockMode; t:PTerm; si:PScanInf; i:word; s:LongStrPtr;
begin
  si:=SiCFile(word(P^.Branch));
  md:=NewLMode(CrMode); CRecPtr:=GetRecSpace; ZeroAllFlds;
  PtrRec(d).Seg:=_Sg; fl:=ptr(_Sg,si^.FL); l:=ptr(_Sg,c^.Arg); i:=0;
  if trace then write('CALL assert(',PString(ptr(_Sg,P^.Name))^,'(');
  while flofs<>0 do begin
    f:=fl^.FldD; if f^.Flg and f_Stored<>0 then begin
    t:=CopyTerm(l^.Elem); dofs:=p^.Arg[i];
    if trace then begin
      if i>0 then write(','); PrintTerm(t,dofs) end;
    case f^.FrmlTyp of
      'B':b_(f,boolean(t^.Fun));
      'R':if t^.Fun=_IntT then r_(f,t^.II) else r_(f,t^.RR);
      else if f^.Typ='T' then begin
        if d^.Typ=_LongStrD then s:=RdLongStr(t^.Pos) else s:=GetPackedTerm(t);
        LongS_(f,s); ReleaseStore(s) end
      else S_(f,t^.SS) end end;
    flofs:=fl^.Chain; lofs:=l^.Chain; inc(i) end;        {$Ifdef FandSQL}
  if trace then begin writeln('))'); waitC end;
  if CFile^.IsSQLFile then Strm1^.InsertRec(false,true) else{$endif}
  begin TestXFExist; IncNRecs(1);
  if CFile^.Typ='X' then RecallRec(CFile^.NRecs) else WriteRec(CFile^.NRecs) end;
  OldLMode(md); ReleaseStore(CRecPtr);
end;
function GetScan(SIOfs:word;C:PCommand;Q:PInstance):PFileScan;
var fs:PFileScan; si:PScanInf; fl:PFldList; flofs:word absolute fl;
    k:KeyDPtr; kofs:word absolute k; kf:KeyFldDPtr; f:FieldDPtr;
    xx:XString; i:word; t:PTerm; r:float; s:LongStrPtr; ss:StringPtr;
    md:LockMode; n:longint; b:boolean;
label 1;
begin
  si:=SiCFile(SIOfs); fs:=Mem1.Get(sizeof(TFileScan)); md:=NewLMode(RdMode);
  k:=nil; if c^.KDOfs<>0 then k:=ptr(PtrRec(CFile).Seg,c^.KDOfs);
  if kofs=0 then begin
    fs^.IRec:=1; fs^.Count:=CFile^.NRecs; goto 1 end;
  TestXFExist; xx.Clear; i:=0;
  kf:=k^.KFlds; while kf<>nil do begin
    f:=kf^.FldD; t:=Q^.Vars[c^.ArgI[i]];
    case f^.FrmlTyp of
     'R': begin if t^.Fun=_IntT then r:=t^.II else r:=t^.RR;
          xx.StoreReal(r,kf) end;
     'B': xx.StoreBool(boolean(t^.Fun),kf);
     else if t^.Fun=_StrT then xx.StoreStr(t^.SS,kf)
     else begin
       if t^.Fun=_LongStrT then s:=RdLongStr(t^.Pos) else s:=GetPackedTerm(t);
       ss:=ptr(PtrRec(s).Seg,PtrRec(s).Ofs+1); ss^[0]:=char(minw(s^.LL,255));
       xx.StoreStr(ss^,kf); ReleaseStore(s) end end;
    kf:=kf^.Chain; inc(i) end;
  k^.FindNr(xx,fs^.IRec);
  if (f^.Typ<>'A') or (xx.S[length(xx.S)]<>#$1f) then inc(xx.S[0]);
  xx.S[length(xx.S)]:=#$FF;
  b:=k^.FindNr(xx,n);
  fs^.Count:=0; if n>=fs^.IRec then fs^.Count:=n-fs^.IRec+ord(b);
1:OldLMode(md);
  if fs^.Count=0 then fs:=nil; GetScan:=fs;
end;
function _MyS(F:FieldDPtr):string;
begin
  if F^.Typ='A' then
    if F^.M=LeftJust then _MyS:=TrailChar(' ',_ShortS(f))
    else _MyS:=LeadChar(' ',_ShortS(f))
  else _MyS:=_ShortS(f);
end;
function ScanFile(Q:PInstance):boolean;
var p:PPredicate; pofs:word absolute p;  c:PCommand; cofs:word absolute c;
    fl:PFldList; flofs:word absolute fl;
    f:FieldDPtr; si:PScanInf; w:word; i:integer; t:PTerm; r:float;
    s:LongStrPtr; ss:string; b:boolean; d:PDomain; dofs:word absolute d;
    pt:pointer absolute PackedTermPtr;
    k:KeyDPtr; kofs:word absolute k;
    md,md1:LockMode; fs,fs1:PFileScan; RecNr:longint; xx:XString;
label 1,2;
begin
  ScanFile:=false;
  fs:=PFileScan(CurrInst^.NextBranch); if fs= nil then exit;
  p:=ptr(_Sg,CurrInst^.Pred); c:=ptr(_Sg,CurrInst^.RetCmd); PtrRec(d).Seg:=_Sg;
  si:=ptr(_Sg,word(p^.Branch)); PtrRec(fl).Seg:=_Sg;
  CFile:=si^.FD; CRecPtr:=GetRecSpace; md:=NewLMode(RdMode);
  k:=nil; if c^.KDOfs<>0 then k:=ptr(PtrRec(CFile).Seg,c^.KDOfs);
1:if k=nil then begin
    repeat RecNr:=fs^.IRec; if RecNr>CFile^.NRecs then begin
        CurrInst^.NextBranch:=nil; goto 2 end;
      ReadRec(RecNr); inc(fs^.IRec);
    until not DeletedFlag;
    if fs^.IRec>CFile^.NRecs then CurrInst^.NextBranch:=nil end
  else begin
    if (fs^.Count=0) or (fs^.IRec>k^.NRecs) then begin
      CurrInst^.NextBranch:=nil; goto 2 end;
    RecNr:=k^.NrToRecNr(fs^.IRec);
    ReadRec(RecNr); inc(fs^.IRec); dec(fs^.Count);
    if (fs^.Count=0) or (fs^.IRec>k^.NRecs) then CurrInst^.NextBranch:=nil end;
  flofs:=si^.FL; w:=c^.CompMask;
  for i:=0 to integer(p^.Arity)-1 do begin        { compare with inp. parameters }
    if (w and 1)<>0 then begin
      t:=CurrInst^.Vars[i]; f:=fl^.FldD; case f^.FrmlTyp of
        'B':if ord(_b(f))<>t^.Fun then goto 1;
        'R':begin r:=_r(f);
            if t^.Fun=_IntT then begin if r<>t^.II then goto 1 end
            else if r<>t^.RR then goto 1 end;
        else if f^.Typ='T' then begin
          dofs:=p^.Arg[i]; if d^.Typ=_LongStrD then s:=RdLongStr(t^.Pos)
          else s:=GetPackedTerm(t);
          b:=EquLongStr(s,_LongS(f)); ReleaseStore(s);
          if not b then goto 1 end
        else if t^.SS<>_MyS(f) then goto 1 end end;
    flofs:=fl^.Chain; w:=w shr 1 end;
  flofs:=si^.FL; w:=c^.OutpMask;
  for i:=0 to integer(p^.Arity)-1 do begin         { create outp. parameters }
    if (w and 1)<>0 then begin
      f:=fl^.FldD; dofs:=p^.Arg[i]; case f^.FrmlTyp of
        'B':CurrInst^.Vars[i]:=GetBoolTerm(_b(f));
        'R':if d^.Typ=_RealD then CurrInst^.Vars[i]:=GetRealTerm(_r(f))
            else CurrInst^.Vars[i]:=GetIntTerm(trunc(_r(f)));
        else if f^.Typ='T' then begin
          s:=_LongS(f);
          if d^.Typ=_LongStrD then CurrInst^.Vars[i]:=GetLongStrTerm(WrLongStr(s))
          else begin pt:=@s^.A; CurrInst^.Vars[i]:=UnpackTerm(dofs) end;
          ReleaseStore(s) end
        else CurrInst^.Vars[i]:=GetStringTerm(_MyS(f)) end end;
    flofs:=fl^.Chain; w:=w shr 1 end;
  ScanFile:=true;
  if c^.Code=_RetractC then begin
    md1:=NewLMode(DelMode);
    while (q<>nil) do begin
      fs1:=PFileScan(q^.NextBranch);
      if (q^.Pred=pofs) and (fs1<>nil) then
        if CFile^.Typ='X' then begin cofs:=q^.RetCmd; kofs:=c^.KDOfs;
          if kofs<>0 then begin
            xx.PackKF(k^.KFlds); k^.RecNrToPath(xx,RecNr);
            if k^.PathToNr<=fs1^.IRec then dec(fs1^.IRec) end end
        else if RecNr<=fs1^.IRec then dec(fs1^.IRec);
      q:=q^.PrevInst end;
    if CFile^.Typ='X' then begin DeleteXRec(RecNr,true); dec(fs^.IRec) end
    else DeleteRec(RecNr);
    OldLMode(md1) end;
2:OldLMode(md); ReleaseStore(CRecPtr);
end;

procedure SaveLMode; far;
  begin with CFile^ do ExLMode:=LMode end;
procedure SetOldLMode; far;
  begin OldLMode(CFile^.ExLMode) end;

procedure TraceCall(Q:PInstance;X:byte);
var i,w:word; p:PPredicate; c:PCommand; d:PDomain; dofs:word absolute d;
begin
  p:=ptr(_Sg,Q^.Pred); c:=ptr(_Sg,Q^.RetCmd); PtrRec(d).Seg:=_Sg;
  if X=1 then write('CALL ') else
  if c^.Code=_AllC then write('MEMBER ') else write('RETURN ');
  case c^.Code of _RetractC:write('retract('); _NotC:write('not(');
                  _AllC:write('all_XX(') end;
  write(PString(ptr(_Sg,p^.Name))^); if p^.Arity>0 then begin
    write('(');
    if (p^.Opt and _CioMaskOpt)<>0 then w:=c^.InpMask else w:=p^.InpMask;
    for i:=0 to p^.Arity-1 do begin if i>0 then write(','); dofs:=p^.Arg[i];
      if (w and 1)=X then
        if (X=1) and ((p^.Opt and _PackInpOpt)<>0) then
          PrintPackedTerm(Pchar(Q^.Vars[i])+2,dofs)
        else begin
          if p^.Opt and _BuildInOpt<>0 then case p^.LocVarSz of
            _MemP,_AddP,_DelP :
                   begin dofs:=c^.Elem; if i=0 then dofs:=d^.ElemDom end;
            _LenP :if i=0 then dofs:=c^.Elem;
            _InvP,_UnionP,_MinusP,_InterP: dofs:=c^.Elem end;
          PrintTerm(Q^.Vars[i],dofs) end
      else write('_');
      w:=w shr 1 end;
    write(')') end;
  if c^.Code in [_RetractC,_NotC,_AllC] then write(')');
  writeln; waitC;
end;


type
  TAutoR=record i:byte; wasCall:boolean; t:PTerm; Arg:array[0..0] of PTerm end;
  PAutoR=^TAutoR;

function AutoRecursion(q:PInstance;p:PPredicate;c:PCommand):boolean;
var i,j,i2,iOutp,sz,arity:integer; w:PAutoR; t,t1:PTerm; f:PFunDcl;
    d:PDomain; dofs:word absolute d;
label 1,2,3;
begin
  d:=ptr(_Sg,p^.Arg[0]); iOutp:=c^.iOutp; t1:=q^.Vars[c^.iWrk];

  if d^.Typ=_ListD then
    if t1=nil then begin
      t:=q^.Vars[0]; if t=nil then begin
        if iOutp>0 then q^.Vars[iOutp]:=t; goto 2 end;
      q^.Vars[c^.iWrk]:=t; q^.Vars[0]:=t^.Next; goto 1 end
    else begin
      if iOutp>0 then begin
        t:=q^.Vars[iOutp]; if t<>t1^.Next then t1:=GetListTerm(t1^.Elem,t);
        q^.Vars[iOutp]:=t1 end;
      goto 3 end;

  w:=PAutoR(t1);
  if w=nil then begin
    t:=q^.Vars[0]; f:=getFunDcl(dofs,t^.Fun);
    w:=Mem2.get(sizeof(TAutoR)+4*f^.Arity);
    w^.t:=t; i:=0; q^.Vars[c^.iWrk]:=PTerm(w) end
  else begin t:=w^.t; f:=getFunDcl(dofs,t^.Fun); i:=w^.i;
    if iOutp>0 then w^.Arg[i]:=q^.Vars[iOutp]; inc(i) end;
  while i<f^.Arity do
    if f^.Arg[i]=dofs then begin
      if w^.wasCall then for j:=0 to c^.nPairs-1 do
        q^.Vars[c^.Pair[j].iInp]:=q^.Vars[c^.Pair[j].iOutp];
      q^.Vars[0]:=t^.Arg[i]; w^.i:=i; w^.wasCall:=true;
1:    AutoRecursion:=true; exit end
    else begin if iOutp>0 then w^.Arg[i]:=t^.Arg[i]; inc(i) end;
  if iOutp>0 then begin sz:=4*f^.Arity;
    if not EquArea(@t^.Arg,@w^.Arg,sz) then begin
      t1:=GetFunTerm(t^.Fun,f^.Arity); move(w^.Arg,t1^.Arg,sz);
      q^.Vars[iOutp]:=t1 end
    else q^.Vars[iOutp]:=t end;
  if not w^.wasCall then
2:for j:=0 to c^.nPairs-1 do begin
    i:=c^.Pair[j].iInp; i2:=c^.Pair[j].iOutp;
    if i>0 then t:=q^.Vars[i] else begin
      dofs:=p^.Arg[i2]; case d^.Typ of
        _ListD: t:=nil; _StrD: t:=GetStringTerm('');
        _LongStrD: t:=GetLongStrTerm(WrLongStrLP(0,nil));
        _IntD: t:=GetIntTerm(0); _RealD: t:=GetRealTerm(0) end end;
    q^.Vars[i2]:=t end;
3:AutoRecursion:=false;
end;

procedure RunProlog(Pos:RdbPos;PredName:StringPtr);
var q,q1,TopInst:PInstance; b1:PDbBranch;
    w,n:word; i:integer; s:LongStrPtr; pt:Pchar absolute PackedTermPtr;
    A:array[1..MaxPackedPredLen] of char; wp:WordPtr;
    pp,pp1,pp2,pp3,pm1,pm2:pointer; ss:LongStrPtr; WMark:longint;
    p,p1:PPredicate; pofs:word absolute p; c:PCommand; cofs:word absolute c;
    b:PBranch; bofs:word absolute b; bd:PDbBranch absolute b;
    l:PTermList; lofs:word absolute l;  db:PDatabase; dbofs:word absolute db;
    t:PTerm; Roots:PProgRoots; ChptLRdb:RdbDPtr;
    oldSg:word; oldCurrInst:PInstance; tl,cl:word; er:ExitRecord;
label 1,2,21,22,23,24,25,3,4,41,5,6,7,8;
begin
  inc(ProlgCallLevel); NewExit(Ovr,er); goto 7; LastExitCode:=1;
  oldSg:=_Sg; oldCurrInst:=CurrInst; ForallFDs(SaveLMode); WMark:=MaxWSize;
  if ProlgCallLevel=1 then begin
    MarkBoth(pm1,pm2); FreeMemList:=nil; Mem1.Init; Mem2.Init; Mem3.Init;
    TrcLevel:=0; CallLevel:=0 end
  else begin
    MarkStore(pp); pp1:=Mem1.Mark; pp2:=Mem2.Mark; pp3:=Mem3.Mark;
    tl:=TrcLevel; cl:=CallLevel end;
  if Pos.IRec=0 then begin
    SetInpLongStr(RunLongStr(FrmlPtr(Pos.R)),true); _Sg:=ReadProlog(0);
    ChptLRdb:=CRdb end
  else begin
    ChptLRdb:=Pos.R; CFile:=ChptLRdb^.FD; CRecPtr:=GetRecSpace;
    ReadRec(Pos.IRec); AlignLongStr;
    _Sg:=PtrRec(HeapPtr).Seg+1; ss:=_longs(ChptOldTxt);
    if ChptLRdb^.Encrypted then CodingLongStr(ss) end;
  Roots:=ptr(_Sg,0);

  db:=ptr(_Sg,Roots^.Databases); while dbofs<>0 do begin
    ConsultDb(ptr(_Sg,db^.SOfs),dbofs); dbofs:=db^.Chain end;
  TopInst:=nil; CurrInst:=nil;
  c:=ptr(_Sg,0); b:=ptr(_Sg,0); l:=ptr(_Sg,0);
  p:=ptr(_Sg,Roots^.Predicates); { main } if PredName<>nil then begin
    while (pofs<>0) and (StringPtr(ptr(_Sg,p^.Name))^<>PredName^) do pofs:=p^.Chain;
    if (pofs=0) or (p^.Arity<>0) then begin
      Set2MsgPar(Pos.R^.FD^.Name,PredName^); RunError(1545) end end;
1:                {    new instance        remember prev. inst,branch,cmd }
  q:=Mem2.Get(p^.InstSz+(sizeof(TInstance)-7*4)); q^.Pred:=pofs;
  q^.PrevInst:=TopInst; TopInst:=q;
  q^.RetInst:=CurrInst; q^.RetBranch:=bofs; q^.RetCmd:=cofs;
  if TrcLevel<>0 then begin
    CallLevel:=CurrInst^.CallLevel+1; q^.CallLevel:=CallLevel end;
				{          copy input parameters  }
  b:=p^.Branch; i:=0;
  if (p^.Opt and _CioMaskOpt)<>0 then w:=c^.InpMask else w:=p^.InpMask;
  while lofs<>0 do begin
    if (w and 1)<>0 then if (p^.Opt and _PackInpOpt)<>0 then begin
      pt:=Pchar(@A); PTPMaxOfs:=ofs(A)+MaxPackedPredLen-2;
      PackTermV(l^.Elem); n:=PtrRec(pt).Ofs-ofs(A);
      s:=Mem1.Get(2+n); q^.Vars[i]:=PTerm(s); s^.LL:=n; move(A,s^.A,n) end
    else q^.Vars[i]:=CopyTerm(l^.Elem);
    inc(i); lofs:=l^.Chain; w:=w shr 1 end;
  if (p^.Opt and (_FandCallOpt+_DbaseOpt))=_FandCallOpt+_DbaseOpt then
    q^.NextBranch:=PBranch(GetScan(bofs,c,q));
  if Trace then TraceCall(q,1);
  q^.StkMark:=Mem1.Mark; q^.WMark:=MaxWSize; CurrInst:=q;
  if (p^.Opt and (_FandCallOpt+_DbaseOpt))=_FandCallOpt then begin
    CallFandProc; goto 4 end;
2:                                {        branch       / redo /        }
  if (p^.Opt and _BuildInOpt)<>0 then             { build-in predicates }
    if RunBuildIn then goto 4 else goto 5;
  if (p^.Opt and _DbaseOpt)<>0 then begin         { database predicates }
    if (p^.Opt and _FandCallOpt)<>0 then
      if ScanFile(TopInst) then goto 4 else goto 5;
    if bd=nil then goto 5; cofs:=q^.RetCmd;
21: s:=LongStrPtr(@bd^.LL); w:=c^.InpMask;
    for i:=0 to integer(p^.Arity)-1 do begin
      if ((w and 1)<>0) and not EquLongStr(LongStrPtr(q^.Vars[i]),s) then begin
        bd:=bd^.Chain; if bd=nil then begin q^.NextBranch:=nil; goto 5 end;
        goto 21 end;
      inc(PtrRec(s).Ofs,s^.LL+2); w:=w shr 1 end;
22: q^.NextBranch:=PBranch(bd^.Chain); s:=LongStrPtr(@bd^.LL); w:=c^.OutpMask;
    for i:=0 to integer(p^.Arity)-1 do begin     { unpack db outp.parameters }
      if (w and 1)<>0 then begin pt:=Pchar(@s^.A); q^.Vars[i]:=UnpackTerm(p^.Arg[i]) end;
      inc(PtrRec(s).Ofs,s^.LL+2); w:=w shr 1 end;
    if c^.Code=_RetractC then RetractDbEntry(TopInst,pofs,bd);
    goto 4 end;
  PtrRec(b).Seg:=_Sg;
23:                                { normal predicates : unify branch head }
  word(q^.NextBranch):=b^.Chain;
  i:=0; lofs:=b^.Head; w:=b^.HeadIMask; while lofs<>0 do begin
    if ((w and 1)<>0) and not UnifyTermsCV(q^.Vars[i],l^.Elem) then begin
      bofs:=b^.Chain; if bofs=0 then goto 5; goto 23 end;
    inc(i); lofs:=l^.Chain; w:=w shr 1 end;
				{           execute all commands       }
  cofs:=b^.Cmd; while cofs<>0 do begin
    case c^.Code of
      _PredC,_RetractC, _NotC:begin
24:          pofs:=c^.Pred; lofs:=c^.Arg; goto 1 end;
      _AllC:begin q^.vars[c^.Idx]:=nil; goto 24 end;
      _CutC : begin q^.NextBranch:=nil; while TopInst<>q do begin
                q1:=TopInst^.PrevInst; Mem2.Release(TopInst); TopInst:=q1 end end;
      _FailC: goto 5;
      _Trace: begin TrcLevel:=c^.TrcLevel;
              if TrcLevel<>0 then begin
                inc(TrcLevel); CallLevel:=1; q^.CallLevel:=1 end
              else begin CallLevel:=0; q^.CallLevel:=0 end end;
      _AssertC: begin p1:=ptr(_Sg,c^.Pred);
              if p1^.Opt and _FandCallOpt<>0 then AssertFand(p1,c) else begin
              lofs:=c^.Arg; pt:=Pchar(@A); PTPMaxOfs:=ofs(A)+MaxPackedPredLen-2;
              while lofs<>0 do begin
                wp:=WordPtr(pt); inc(pt,2); PackTermV(l^.Elem);
                wp^:=PtrRec(pt).Ofs-PtrRec(wp).Ofs-2; lofs:=l^.Chain end;
              n:=PtrRec(pt).Ofs-Ofs(A); b1:=Mem3.Alloc(4+n); move(A,b1^.LL,n);
              ChainLast(p1^.Branch,b1);
              if Trace then PrintPackedPred(Pchar(@A),c^.Pred) end end;
      _AutoC:
25:           if AutoRecursion(q,p,c) then begin lofs:=c^.Arg; goto 1; end;
      _SelfC: begin
              if TopInst<>q then begin q1:=TopInst;
                while q1^.PrevInst<>q do q1:=q1^.PrevInst;
                Mem2.Release(q1); TopInst:=q end;
              Mem1.Release(q^.StkMark); MaxWSize:=q^.WMark; pofs:=q^.Pred;
              b:=p^.Branch; goto 6 end;
      else if not RunCommand(cofs) then goto 5 end;
                                           {       resume command   }
3:  cofs:=c^.Chain end;
                                  {           copy output parameters }
  i:=0; lofs:=b^.Head; w:=b^.HeadOMask; while lofs<>0 do begin
    if (w and 1)<>0 then q^.Vars[i]:=CopyTerm(l^.Elem);
    inc(i); lofs:=l^.Chain; w:=w shr 1 end;
                                    {       called predicate finished   }
4: cofs:=q^.RetCmd;
   if c^.Code=_NotC then begin TopInst:=q^.PrevInst; goto 5 end;
41:if Trace then TraceCall(q,0);
				    {      unify output with caller terms }
  b:=ptr(_Sg,q^.RetBranch); pofs:=q^.Pred; q1:=q; q:=q^.RetInst;
  if q=nil then begin EdBreak:=0; LastExitCode:=0; goto 8 end;
  CurrInst:=q;
  if (p^.Opt and _CioMaskOpt)<>0 then w:=c^.OutpMask else w:=not p^.InpMask;
  i:=0; lofs:=c^.Arg; while lofs<>0 do begin
    if ((w and 1)=1) and not UnifyTermsCV(q1^.Vars[i],l^.Elem) then goto 5;
    inc(i); lofs:=l^.Chain; w:=w shr 1 end;
				{            return to caller;       }
  if c^.Code=_AllC then begin
    ChainList(q^.Vars[c^.Idx],GetListTerm(CopyTerm(c^.Elem),nil));
    q1:=TopInst; while q1<>q do begin
      q1^.StkMark:=Mem1.Mark; q1^.WMark:=MaxWSize; q1:=q1^.PrevInst end;
    goto 5 end;
  if (q1^.NextBranch=nil) and (q1=TopInst) then begin
    TopInst:=q1^.PrevInst; Mem2.Release(q1) end;
  SetCallLevel(q^.CallLevel);
  if c^.Code=_AutoC then goto 25 else goto 3;

{---------------------------------  backtracking  ---------------------------}
5: q1:=nil; q:=TopInst;
  while (q<>nil) and (q^.NextBranch=nil) and
    not (PCommand(ptr(_Sg,q^.RetCmd))^.Code in [_NotC,_AllC]) do
    begin q1:=q; q:=q^.PrevInst end;
  if q=nil then begin
    if Trace then begin writeln('FAIL');waitC end;
    EdBreak:=1; LastExitCode:=0; goto 8 end;
  Mem1.Release(q^.StkMark); MaxWSize:=q^.WMark;
  if q^.NextBranch=nil then begin
    q1:=q; q:=q1^.RetInst; b:=ptr(_Sg,q1^.RetBranch); cofs:=q1^.RetCmd;
    CurrInst:=q; TopInst:=q1^.PrevInst; Mem2.Release(q1);
    if c^.Code=_NotC then begin
      if Trace then begin writeln('FAIL'^m^j'RETURN not()'); waitC end end
    else begin
      q^.Vars[c^.Idx2]:=q^.Vars[c^.Idx]; q^.Vars[c^.Idx]:=nil;
      if Trace then begin writeln('RETURN all_()'); waitC end end;
    SetCallLevel(q^.CallLevel); pofs:=q^.Pred;
    goto 3 end;
  if Trace then begin writeln('FAIL');waitC end;
  TopInst:=q; CurrInst:=TopInst; b:=q^.NextBranch; pofs:=q^.Pred;
  SetCallLevel(q^.CallLevel);
  if q1<>nil then Mem2.Release(q1);
6:if Trace then begin writeln('REDO ',PString(ptr(_Sg,p^.Name))^);waitC end;
  goto 2;

{--------------------------   end of program  ------------------------------}
7:EdBreak:=2;
8:RestoreExit(er);
{writeln(AbsAdr(HeapPtr)-AbsAdr(pm1),'/',AbsAdr(pm2)-AbsAdr(Stack2Ptr)); }
  _Sg:=oldSg; CurrInst:=oldCurrInst; ForallFDs(SetOldLMode); MaxWSize:=WMark;
  if ProlgCallLevel=1 then ReleaseBoth(pm1,pm2) else begin
    ReleaseStore(pp); Mem1.Release(pp1); Mem2.Release(pp2); Mem3.Release(pp3);
    TrcLevel:=tl; CallLevel:=cl end;
  dec(ProlgCallLevel);
end;

end.