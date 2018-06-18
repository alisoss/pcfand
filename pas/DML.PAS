{$I Switches}
{$A-,B-,F+,G-,I+,O-,R-,S+,V+,X+,L-}
unit DML;

interface

uses dos,drivers,base,access;

procedure InitDML;

implementation

uses obaseww,oaccess,runfrml,compile,wwmenu,runedi;

procedure NoOvr; far; assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push dx; push ax;
end;


procedure _opentest;
  begin if CFile^.Handle=$FF then CFileError(612) end;
function FindAlias(Alias:StringPtr):KeyDPtr; far;
var K:KeyDPtr;
label 1;
begin
  K:=CFile^.Keys; while K<>nil do begin
    if SEquUpcase(K^.Alias^,Alias^) then begin FindAlias:=K; exit end;
    K:=K^.Chain end;
1:Set2MsgPar(CFile^.Name,Alias^); RunError(608);
end;

{ parameters are on the stack in reverse order and byte size => word !!!! }

{$S-}function _findalias(qq:pointer):KeyDPtr; far;
type qr=record Alias:StringPtr;FD:FileDPtr end; var q:^qr absolute qq;
  K:KeyDPtr;
label 1;
begin with q^ do begin
  CFile:=FD; _findalias:=FindAlias(Alias);
end end;
function _file(qq:pointer):FileDPtr; far;
type qr=record Name:StringPtr end; var q:^qr absolute qq;
  FD:FileDPtr; R:RdbDPtr;
label 1;
begin with q^ do begin
  R:=CRdb;while R<>nil do begin
    FD:=R^.FD; while FD<>nil do begin
      if SEquUpcase(FD^.Name,Name^) then goto 1; FD:=FD^.Chain end;
    R:=R^.ChainBack end;
  SetMsgPar(Name^); RunError(601);
1:_file:=FD; FD^.RecPtr:=nil;
  {$ifdef FandSQL} if CFile^.typSQLFile then begin
    SetMsgPar(FD^.Name); RunError(651) end; {$endif}
end end;
{$S-}procedure SetFDRecPtr;
begin
  if CFile^.RecPtr=nil then CFile^.RecPtr:=GetZStore2(CFile^.RecLen+1);
end;{$S+}
procedure _resetm(qq:pointer); far;
type qr=record UM:FileUseMode;x:byte;FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; SetFDRecPtr; if UM=Shared then UM:=Exclusive; OpenCreateF(UM);
  SeekRec(0)
end end;
procedure _reset(qq:pointer); far;
type qr=record FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; SetFDRecPtr; OpenCreateF(Exclusive);
  SeekRec(0);
end end;
procedure _rewrite(qq:pointer); far;
type qr=record FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; SetFDRecPtr; RewriteF(false);
  if CFile^.Typ='X' then CFile^.XF^.NoCreate:=false;
end end;
procedure _close(qq:pointer); far;
type qr=record FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; CloseFile;
end end;
function _item(qq:pointer):FieldDPtr; far;
type qr=record Name:StringPtr;FD:FileDPtr end; var q:^qr absolute qq;
  F:FieldDPtr;
label 1;
begin with q^ do begin
  F:=FD^.FldD; while F<>nil do
    if SEquUpcase(F^.Name,Name^) then goto 1 else F:=F^.Chain;
  Set2MsgPar(FD^.Name,Name^);RunError(602);
1:_item:=F;
end end;
procedure _seek(qq:pointer); far;
type qr=record N:longint;FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; _opentest; if (N<0) or (N>CFile^.NRecs) then CFileError(615);
  SeekRec(N);
end end;
function _DMLfilesize(qq:pointer):longint; far;
type qr=record FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; _opentest; _DMLfilesize:=XNRecs(CFile^.Keys);
end end;
function _DMLfilepos(qq:pointer):longint; far;
type qr=record FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; _DMLfilepos:=FD^.IRec;
end end;
function _eof(qq:pointer):boolean; far;
type qr=record FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  _eof:=FD^.IRec>=FD^.NRecs;
end end;
procedure _read(qq:pointer); far;
type qr=record FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; _opentest; CRecPtr:=FD^.RecPtr;
  if FD^.IRec>=FD^.NRecs then CFileError(614); inc(FD^.IRec);
  ReadRec(FD^.IRec); FD^.EOF:=FD^.IRec>=FD^.NRecs;
end end;
procedure _write(qq:pointer); far;
type qr=record FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; _opentest; CRecPtr:=FD^.RecPtr;
  if FD^.IRec=FD^.NRecs then PutRec
  else begin inc(FD^.IRec);WriteRec(FD^.IRec) end;
end end;
procedure _ss(qq:pointer); far;
type qr=record F:FieldDPtr;FD:FileDPtr;Result:StringPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; _opentest; CRecPtr:=FD^.RecPtr;
  if F^.Typ='D' then Result^:=StrDate(_r(F),FieldDMask(F)^)
  else Result^:=_shorts(F);
end end;
function _rr(qq:pointer):float; far;
type qr=record F:FieldDPtr;FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; _opentest; CRecPtr:=FD^.RecPtr; _rr:=_r(F);
end end;
function _bb(qq:pointer):boolean; far;
type qr=record F:FieldDPtr;FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; _opentest; CRecPtr:=FD^.RecPtr; _bb:=_b(F);
end end;
procedure _tt(qq:pointer); far;
type qr=record F:FieldDPtr;FD:FileDPtr;MaxLen:word;Len:WordPtr;Arr:pointer end;
var q:^qr absolute qq;
  l:word; s:LongStrPtr;
begin with q^ do begin
  CFile:=FD; _opentest; CRecPtr:=FD^.RecPtr;
  s:=_longs(F); l:=s^.LL; if l>MaxLen then l:=MaxLen; Len^:=l;
  Move(S^.A,Arr^,l); ReleaseStore(s);
end end;
procedure ss_(qq:pointer); far;
type qr=record S:StringPtr;F:FieldDPtr;FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; _opentest; CRecPtr:=FD^.RecPtr;
  if F^.Typ='D' then r_(F,ValDate(S^,FieldDMask(F)^)) else s_(F,S^);
end end;
procedure rr_(qq:pointer); far;
type qr=record R:float;F:FieldDPtr;FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; _opentest; CRecPtr:=FD^.RecPtr; r_(F,R);
end end;
procedure bb_(qq:pointer); far;
type qr=record B:boolean;x:byte;F:FieldDPtr;FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; _opentest; CRecPtr:=FD^.RecPtr; b_(F,B);
end end;
procedure tt_(qq:pointer); far;
type qr=record F:FieldDPtr;FD:FileDPtr;Len:word;Arr:pointer end; var q:^qr absolute qq;
  s:LongStrPtr;
begin with q^ do begin
  CFile:=FD; _opentest; CRecPtr:=FD^.RecPtr;
  s:=GetStore(Len+2); s^.LL:=Len; Move(Arr^,S^.A,Len);
  longs_(F,s); ReleaseStore(s);
end end;

procedure _deleterec(qq:pointer); far;
type qr=record FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; _opentest; CRecPtr:=FD^.RecPtr;
  if (CFile^.Typ<>'X') or DeletedFlag then CFileError(633);
  DeleteXRec(CFile^.IRec,true);
end end;
procedure _recallrec(qq:pointer); far;
type qr=record FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD;_opentest; CRecPtr:=FD^.RecPtr;
  if (CFile^.Typ<>'X') or not DeletedFlag then CFileError(634);
  RecallRec(CFile^.IRec);
end end;
procedure _indexseek(qq:pointer); far;
type qr=record N:longint;Alias:StringPtr;FD:FileDPtr end; var q:^qr absolute qq;
  k:KeyDPtr;
begin with q^ do begin
  CFile:=FD; _opentest;
  if (CFile^.Typ<>'X') or (N<0) or (N>=CFile^.XF^.NRecs) then CFileError(615);
  k:=FindAlias(Alias); TestXFExist; CFile^.IRec:=k^.NrToRecNr(N+1)-1;
end end;

procedure _xstorereal(qq:pointer); far;
type qr=record KF:KeyFldDPtr;R:float;XP:XStringPtr end; var q:^qr absolute qq;
begin with q^ do begin
  XP^.StoreReal(R,KF);
end end;
procedure _xstorebool(qq:pointer); far;
type qr=record KF:KeyFldDPtr;B:boolean;x:byte;XP:XStringPtr end; var q:^qr absolute qq;
begin with q^ do begin
  XP^.StoreBool(B,KF);
end end;
procedure _xstorestr(qq:pointer); far;
type qr=record KF:KeyFldDPtr;S:StringPtr;XP:XStringPtr end; var q:^qr absolute qq;
begin with q^ do begin
  XP^.StoreStr(S^,KF);
end end;
function _searchkey(qq:pointer):boolean; far;
type qr=record PX:XStringPtr;K:KeyDPtr;FD:FileDPtr end; var q:^qr absolute qq;
  n:longint;
begin with q^ do begin
  CFile:=FD; _opentest; CRecPtr:=FD^.RecPtr;
  if CFile^.Typ='X' then begin
    TestXFExist; _searchkey:=K^.Search(PX^,false,n) end
  else _searchkey:=SearchKey(PX^,K,n);
  SeekRec(n-1);
end end;
function _indexpos(qq:pointer):longint; far;
begin
  TestXFExist; _indexpos:=CFile^.Keys^.PathToNr-1;
end;

function _keylink(qq:pointer):boolean; far;
type qr=record Name:StringPtr;FD:FileDPtr end; var q:^qr absolute qq;
  LD:LinkDPtr;N:longint;B:boolean;
begin with q^ do begin
  CFile:=FD; _opentest; CRecPtr:=FD^.RecPtr; LD:=FindLD(Name^);
  if LD=nil then begin Set2MsgPar(FD^.Name,Name^); RunError(603) end;
  B:=LinkUpw(LD,N,false);  SeekRec(N-1);
  if CFile^.RecPtr=nil then CFile^.RecPtr:=GetZStore2(CFile^.RecLen+1);
  _keylink:=B; Move(CRecPtr^,CFile^.RecPtr^,CFile^.RecLen);
  ReleaseStore(CRecPtr);
end end;
procedure _savefiles(qq:pointer); far;
begin
  SaveFiles;
end;
procedure _newfilesize(qq:pointer); far;
type qr=record N:longint;FD:FileDPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; _opentest; AssignNRecs(false,N);
end end;
procedure _fandmsg(qq:pointer); far;
type qr=record Par2,Par1:StringPtr;N:word end; var q:^qr absolute qq;
begin with q^ do begin
  if Par2<>nil then Set2MsgPar(Par1^,Par2^)
  else if Par1<>nil then SetMsgPar(Par1^);
  WrLLF10Msg(N);
end end;
procedure _DMLStrDate(qq:pointer); far;
type qr=record Mask:StringPtr;R:float;Result:StringPtr end; var q:^qr absolute qq;
begin with q^ do begin
  Result^:=StrDate(R,Mask^);
end end;
function _DMLValDate(qq:pointer):float; far;
type qr=record Mask,S:StringPtr end; var q:^qr absolute qq;
begin with q^ do begin
  _DMLvaldate:=ValDate(S^,Mask^);
end end;
function _DMLmenu(qq:pointer):word; far;
type qr=record MenuTxt,Header:StringPtr end; var q:^qr absolute qq;
var w:PMenuBoxS; s:string;
begin with q^ do begin
  s:='/'+Header^+'/'+MenuTxt^; if s[length(s)]='/' then dec(s[0]);
  New(w,Init(0,0,StringPtr(@s)));
  _DMLMenu:=w^.Exec(1);
  w^.Done; ReleaseStore(w);
end end;
procedure _F10Message(qq:pointer); far;
type qr=record Text:StringPtr end; var q:^qr absolute qq;
begin with q^ do begin
  MsgLine:=Text^; WrLLF10MsgLine;
end end;
function GetFldD(FrmlTyp,Typ:char;L,M:word;Mask:StringPtr):FieldDptr; far;
var F:FieldDPtr;
begin
  F:=GetZStore(sizeof(F^)-1); F^.FrmlTyp:=FrmlTyp; F^.Typ:=Typ;
  F^.L:=L; F^.M:=M;
  if Typ='F' then if M=0 then F^.L:=L+1 else F^.L:=L+2+M;
  if Typ='D' then begin F^.L:=length(Mask^); StoreStr(Mask^) end;
  GetFldD:=F;
end;      
procedure _prompts(qq:pointer); far;
type qr=record M,L:word;Typ:char;x:byte;Text,Result:StringPtr end; var q:^qr absolute qq;
  F:FieldDptr;
begin with q^ do begin
  F:=GetFldD('S',Typ,L,M,nil); Result^:=PromptS(Text^,nil,F); ReleaseStore(F);
end end;
function _promptr(qq:pointer):float; far;
type qr=record Mask:StringPtr;M,L:word;Typ:char;x:byte;Text:StringPtr end; var q:^qr absolute qq;
  F:FieldDptr;
begin with q^ do begin
  F:=GetFldD('R',Typ,L,M,Mask);
  _promptr:=PromptR(Text^,nil,F); ReleaseStore(F);
end end;
function _promptb(qq:pointer):boolean; far;
type qr=record Text:StringPtr end; var q:^qr absolute qq;
  F:FieldDptr;
begin with q^ do begin
  F:=GetFldD('B','B',1,0,nil); _promptb:=PromptB(Text^,nil,F);
  ReleaseStore(F);
end end;
procedure _mountvol(qq:pointer); far;
type qr=record Drive:char;x:byte;Vol:StringPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CVol:=Vol^; TestMountVol(Drive);
end end;
function _DMLCRdb(qq:pointer):RdbDPtr; far;
begin
  _DMLCRdb:=CRdb;
end;
function _DMLStoreAvail(qq:pointer):longint; far;
begin
  _DMLStoreAvail:=StoreAvail;
end;
function _DMLGetStore(qq:pointer):pointer; far;
type qr=record Size:word end; var q:^qr absolute qq;
begin with q^ do begin
  _DMLGetStore:=GetStore(Size);
end end;
procedure _DMLReleaseStore(qq:pointer); far;
type qr=record P:pointer end; var q:^qr absolute qq;
begin with q^ do begin
  ReleaseStore(P);
end end;
function RdFldNameFrmlDML(var FTyp:char):FrmlPtr; far;
begin Error(8) end;
procedure _DMLRdFrml(qq:pointer); far;
type qr=record Pos:WordPtr;WasError:^boolean;FTyp:CharPtr;ZP:^FrmlPtr;FD:FileDPtr;
               S,Result:StringPtr end; var q:^qr absolute qq;
  er:ExitRecord;
label 1,2;
begin with q^ do begin
  ResetCompilePars; CFile:=FD; if FD=nil then RdFldNameFrml:=RdFldNameFrmlDML;
  NewExit(NoOvr,er); goto 1;
  SetInpStr(S^); RdLex; ZP^:=RdFrml(FTyp^); if Lexem<>^z then Error(40);
  WasError^:=false; Result^:=''; goto 2;
1:WasError^:=true; IsCompileErr:=false; Result^:=MsgLine;
2:Pos^:=CurrPos; RestoreExit(er);
end end;
function _DMLRunReal(qq:pointer):float; far;
type qr=record FD:FileDPtr;Z:FrmlPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; CRecPtr:=FD^.RecPtr; _DMLRunReal:=RunReal(Z);
end end;
procedure _DMLRunStr(qq:pointer); far;
type qr=record FD:FileDPtr;Z:FrmlPtr;Result:StringPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; CRecPtr:=FD^.RecPtr; Result^:=RunShortStr(Z);
end end;
function _DMLRunBool(qq:pointer):boolean; far;
type qr=record FD:FileDPtr;Z:FrmlPtr end; var q:^qr absolute qq;
begin with q^ do begin
  CFile:=FD; CRecPtr:=FD^.RecPtr; _DMLRunBool:=RunBool(Z);
end end;
procedure _DMLPrTab(qq:pointer); far;
type qr=record N:word;Result:StringPtr end; var q:^qr absolute qq;
begin with q^ do begin
  if (N=0) or (N>28) then Result^[0]:=#0 else Result^:=PrTab(N);
end end;


procedure NoOvrDML; far; assembler;
asm   pop ax; pop ax; push Fand_bp; call StackOvr;
      pop bp; pop bx;{ds} pop ax;{ip} pop dx;{cs} pop sp; mov ss,DML_ss;
      mov ds,bx; push dx; push ax;
end;

procedure _DMLinit(qq:pointer); far;
type qr=record Prnt{=nil,not used,needed for compatibility with earlier versions},
               Colr:PtrPointer; PrfxSeg,Ver:word;
               HpEnd,HpOrg:pointer; Attr,WMax,WMin:WordPtr end;
type pr=record DS,x1,x2,BP,IP,CS:word end;
       { DML stack: ds, ret/4, old_bp, ret/4, parameter }
var q:^qr absolute qq; p:^pr;
  h:word;
begin with q^ do begin                               
  p:=ptr(DML_ss,DML_sp);
  with ExitBuf do with p^ do begin
    rDS:=DS; rBP:=BP; rSP:=BP; rIP:=IP; rCS:=CS; OvrEx:=NoOvrDML; inc(IP,8) end;
  MyHeapOrg:=HpOrg; MemEnd:=HpEnd; MyHeapEnd:=MemEnd;
  CurPSP:=ptr(PrfxSeg,0);
  ExtendHandles;
  if Ver<>DMLVersion then begin
    write('(DML) error: incorrect version ',Ver);wait;GoExit end;
 {  |fand progr|fand stack|user progr|user free mem }
  HeapPtr:=MyHeapOrg; FormatCache;
  if NCachePages=0 then begin
    write('(DML) error: out of memory');wait;GoExit end;

  OpenOvrFile; OpenResFile; OpenWorkH; OpenFANDFiles(true);
  WMin^:= word(WindMin); WMax^:=word(WindMax); Attr^:=ProcAttr;
  Colr^:=@base.Colors;
end end;

procedure _DMLexit(qq:pointer); far;
begin
  if CacheExist then begin
    CloseFANDFiles(true);
    CloseH(OvrHandle); CloseH(WorkHandle);
    CloseH(ResFile.Handle) end;
  UnExtendHandles;
end; {$S+}



const JmpArr:array[0..51]of pointer=(@_DMLinit{0},@_DMLexit{1},
  @_File{2},@_Reset{3},@_ResetM{4},@_Close{5},@_Item{6},@_Seek{7},
  @_IndexSeek{8},@_DMLFileSize{9},@_DMLFilePos{10},@_Eof{11},@_Rewrite{12},
  @_Read{13},@_Write{14},
  @_ss{15},@_rr{16},@_bb{17},@_tt{18},@ss_{19},@rr_{20},@tt_{21},@bb_{22},
  @_SearchKey{23},@_FindAlias{24},@_KeyLink{25},@_SaveFiles{26},
  @_NewFileSize{27},@_FandMsg{28},@_DMLstrdate{29},@_DMLvaldate{30},
  @_DMLMenu{31},@_F10Message{32},@_prompts{33},@_promptr{34},@_promptb{35},
  @_mountvol{36},@_indexpos{37},@_DMLcrdb{38},@_DMLstoreavail{39},
  @_DMLgetstore{40},@_DMLReleaseStore{41},@_DMLRdFrml{42},@_DMLRunReal{43},
  @_DMLRunStr{44},@_DMLRunBool{45},@_deleterec{46},@_recallrec{47},
  @_xstorereal{48},@_xstorestr{49},@_xstorebool{50},
  @_DMLPrTab{51});

var StackSave:array[1..128] of byte;
    NrFun:word;
    DmlInt3f:pointer;

procedure CallDML; far; assembler;
{ DML stack: ds, ret/4, old_bp, ret/4, parameter }
asm  push ds; mov ax,SEG @DATA; mov ds,ax; mov NrFun,bx;
     cmp bx,0; jne @1;    {_init then get 3f-int-vec and save Fand stack}
     mov es,ax; mov di,OFFSET StackSave;     {_init then save Fand stack}
     mov si,Fand_sp; mov ds,Fand_ss; mov cx,128; sub si,cx; cld; rep movsb;
     mov ds,ax; mov cx,bx; mov ax,353fH; int 21H;
     mov DmlInt3f.word,bx; mov DmlInt3f[2].word,es; mov bx,cx;
@1:  push ds; lds dx,FandInt3f; mov ax,253fH; int 21H; pop ds; { Overlay int }
     mov dx,ss; mov ax,sp; add ax,12; { parameter addr }
     mov DML_ss,ss; mov ss,Fand_ss; mov DML_bp,bp; mov bp,Fand_bp;
     mov DML_sp,sp; mov sp,Fand_sp;
     push dx; push ax;
     call JmpArr.dword[bx];     {dx,bx,ax function result}
     mov ss,DML_ss; mov bp,DML_bp; mov sp,DML_sp;
     cmp NrFun,1*4; jne @2;             {_exit then restore Fand stack}
     mov si,OFFSET StackSave;
     mov es,Fand_ss; mov di,Fand_sp; mov cx,128; sub di,cx; cld; rep movsb;
@2:  push dx; push ax; lds dx,DmlInt3f; mov ax,253fH; int 21H; pop ax; pop dx;
     pop ds;
end;


procedure InitDML;
begin
  _CallDMLAddr:=AbsAdr(@CallDML);
end;

end.