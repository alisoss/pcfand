{$I Switches}
{$A-,B-,F+,G-,I+,O-,R-,S+,V+,X+,L+}
unit runfrml;

interface

uses base,access;

 function Owned(Bool,Sum:FrmlPtr;LD:LinkDPtr):float;
 function CompBool(B1,B2:boolean):integer;
 function CompReal(R1,R2:float;M:integer):integer;
 function CopyToLongStr(const SS:string):LongStrPtr;
 function LeadChar(C:char;S:string):string;
 function TrailChar(C:char;S:string):string;
 function LongTrailChar(C,CNew:char;S:LongStrPtr):LongStrPtr;
 function CopyLine(S:LongStrPtr;N,M:word):LongStrPtr;
 function RunBool(X:FrmlPtr):boolean;
 function RunReal(X:FrmlPtr):float;
 function RunInt(Z:FrmlPtr):longint;
 function RunLongStr(X:FrmlPtr):LongStrPtr;
 function RunShortStr(X:FrmlPtr):string;
procedure AssgnFrml(F:FieldDPtr;X:FrmlPtr;Delete,Add:boolean);
procedure LVAssignFrml(LV:LocVarPtr;OldBP:pointer;Add:boolean;X:FrmlPtr);
procedure DecodeFieldRSB(F:FieldDPtr;LWw:word;R:float;T:string;B:boolean;
                         var Txt:string);
procedure DecodeField(F:FieldDPtr;LWw:word; var Txt:string);
procedure RunWFrml(X:WRectFrml;WFlags:byte;var W:WRect);
function RunWordImpl(Z:FrmlPtr;Impl:word):word;
function FieldInList(F:FieldDPtr; FL:FieldList):boolean;
function GetFromKey(LD:LinkDPtr):KeyDPtr;
function RunEvalFrml(Z:FrmlPtr):FrmlPtr;
function CanCopyT(F:FieldDPtr;Z:FrmlPtr):boolean;
var TFD02:FileDPtr; TF02:TFilePtr; TF02Pos:longint;

implementation

uses dos,obaseww,drivers,oaccess,rdrun,olongstr,
{$ifdef FandGraph} graph, {$endif}
{$ifdef FandSQL} channel, {$endif}
     wwmix,rdproc,runproc,runedi,editor;

function CompReal(R1,R2:float;M:integer):integer;
  begin if M>0 then begin R1:=R1*Power10[M];R2:=R2*Power10[M] end;
       if M>=0 then begin
         if R1>=0 then R1:=int(R1+0.5) else R1:=int(R1-0.5);
         if R2>=0 then R2:=int(R2+0.5) else R2:=int(R2-0.5) end;
       if R1>R2 then CompReal:=ord(_gt)
       else if R1<R2 then CompReal:=ord(_lt)
       else CompReal:=ord(_equ);
  end;
function RoundReal(RR:float; M:integer): float;
var R:float;
begin
  M:=maxi(0,mini(M,10));
  R:=RR*Power10[M]; if R<0 then R:=R-0.50001 else R:=R+0.50001;
  RoundReal:=int(R)/Power10[M];
end;
function CompBool(B1,B2:boolean):integer;
  begin if B1>B2 then CompBool:=ord(_gt)
       else if B1<B2 then CompBool:=ord(_lt)
       else CompBool:=ord(_equ);
  end;
function CopyToLongStr(const SS:string):LongStrPtr;
var l:word; s:LongStrPtr;
begin
  l:=length(SS); s:=GetStore(l+2); s^.LL:=l; move(SS[1],s^.A,l);
  CopyToLongStr:=s;
end;
function LeadChar(C:char;S:string):string;
begin
  while (Length(S)>0) and (S[1]=C) do S:=copy(S,2,255); LeadChar:=S;
end;
function TrailChar(C:char;S:string):string;
begin
  while (Length(S)>0) and (S[Length(S)]=C) do S[0]:=chr(length(S)-1);
  TrailChar:=S;
end;
function LongLeadChar(C,CNew:char;S:LongStrPtr):LongStrPtr;
var i,l:word;
label 1;
begin
  i:=1; l:=S^.LL; while i<=l do begin
    if S^.A[i]<>C then goto 1; if CNew<>#0 then S^.A[i]:=CNew; inc(i) end;
1:if CNew=#0 then begin dec(l,i-1); S^.LL:=l;
    if (i>1) and (l>0) then MyMove(S^.A[i],S^.A[1],l);
    ReleaseAfterLongStr(S) end;
  LongLeadChar:=S;
end;
function LongTrailChar(C,CNew:char;S:LongStrPtr):LongStrPtr;
var l:word;
label 1;
begin
  l:=S^.LL; while l>0 do begin
    if S^.A[l]<>C then goto 1; if CNew<>#0 then S^.A[l]:=CNew; dec(l) end;
1:if CNew=#0 then begin S^.LL:=l; ReleaseAfterLongStr(S) end;
  LongTrailChar:=S;
end;
function CopyLine(S:LongStrPtr;N,M:word):LongStrPtr;
var i,j,l:word;
begin
  i:=1; if N>1 then begin i:=FindCtrlM(S,1,N-1); i:=SkipCtrlMJ(S,i) end;
  j:=FindCtrlM(S,i,M); l:=j-i; if (i>1) and (l>0) then move(S^.A[i],S^.A[1],l);
  S^.LL:=l; ReleaseAfterLongStr(S); CopyLine:=S;
end;
function RepeatStr(S:LongStrPtr;N:integer):LongStrPtr;
var l:word; p:pointer;
begin
  RepeatStr:=S; l:=S^.LL; if l=0 then exit;
  if N<=0 then begin S^.LL:=0; ReleaseAfterLongStr(S); exit end;
  while (N>1) and (longint(S^.LL)+l<=MaxLStrLen) do begin
    p:=GetStore(l); MyMove(S^.A,p^,l); inc(S^.LL,l); dec(N) end;
end;
procedure AccRecNoProc(X:FrmlPtr;Msg:word);
  var N:longint; md:LockMode;
  begin CFile:=X^.RecFD; md:=NewLMode(RdMode); CRecPtr:=GetRecSpace;
      N:=RunInt(X^.P1); if (N<=0) or (N>CFile^.NRecs) then
         begin Set2MsgPar(CFile^.Name,X^.RecFldD^.Name);RunErrorM(md,Msg) end;
      ReadRec(N); OldLMode(md) end;
function RunUserFunc(X:FrmlPtr):pointer;
var fl:FrmlList; lv:LocVarPtr; oldbp,oldprocbp:pointer;
begin
  oldbp:=MyBP; oldprocbp:=ProcMyBP; LVBD:=x^.fc^.LVB; PushProcStk;
  lv:=LVBD.Root; fl:=x^.FrmlL; while fl<>nil do begin
    LVAssignFrml(lv,oldbp,false,fl^.Frml); lv:=lv^.Chain; fl:=fl^.Chain end;
  ProcMyBP:=MyBP;
  RunProcedure(x^.fc^.Instr);
  RunUserFunc:=LocVarAd(lv); ProcMyBP:=oldprocbp;
end;
procedure GetRecNoXString(Z:FrmlPtr;var X:XString);
var kf:KeyFldDPtr; zz:FrmlPtr; i:word;
begin
  i:=0; X.Clear; kf:=Z^.Key^.KFlds;  while kf<>nil do begin
    inc(i); zz:=Z^.Arg[i]; case kf^.FldD^.FrmlTyp of
      'S':X.StoreStr(RunShortStr(zz),kf);
      'R':X.StoreReal(RunReal(zz),kf);
      'B':X.StoreBool(RunBool(zz),kf) end;
    kf:=kf^.Chain end;
end;
function Owned(Bool,Sum:FrmlPtr;LD:LinkDPtr):float;
var Scan:XScanPtr; K:KeyDPtr; x:XString; md:LockMode; n,nBeg:longint;
    cf:FileDPtr; cr:pointer; r:float;
label 1;
begin
  x.PackKF(LD^.ToKey^.KFlds); cf:=CFile; cr:=CRecPtr;
  CFile:=LD^.FromFD; md:=NewLMode(RdMode); TestXFExist; K:=GetFromKey(LD);
  if (Bool=nil) and (Sum=nil) and not CFile^.IsSQLFile then begin
    K^.FindNr(x,nBeg); inc(x.S[0]); x.S[length(x.S)]:=#$FF;
    K^.FindNr(x,n); r:=n-nBeg end
  else begin r:=0; CRecPtr:=GetRecSpace;
      New(Scan,Init(CFile,K,nil,true)); Scan^.ResetOwner(@x,nil);
1:    Scan^.GetRec; if not Scan^.EOF then begin
        if RunBool(Bool) then if Sum=nil then r:=r+1 else r:=r+RunReal(Sum);
        goto 1 end;
      Scan^.Close; ReleaseStore(CRecPtr) end;
  OldLMode(md); CFile:=cf; CRecPtr:=cr; Owned:=r;
end;

function RunReal(X:FrmlPtr):float;
function RunRealStr(X:FrmlPtr):float;
var I,J,L,N:word; R:float; S:LongStrPtr; Mask:string; b:byte;
label 1;
begin
  case X^.Op of
  _valdate:RunRealStr:=ValDate(RunShortStr(X^.P1),X^.Mask);
  _val    :begin val(leadchar(' ',trailchar(' ',RunShortStr(X^.P1))),R,I);
           RunRealStr:=R end;
  _length :begin S:=RunLongStr(X^.P1);RunRealStr:=S^.LL;
           ReleaseStore(S) end;
  _linecnt:begin S:=RunLongStr(X^.P1);
           RunRealStr:=int(CountDLines(S^.A,S^.LL,^m));
           ReleaseStore(S) end;
  _ord    :begin S:=RunLongStr(X^.P1); n:=0; if S^.LL>0 then n:=ord(S^.A[1]);
           RunRealStr:=n; ReleaseStore(S) end;
  _prompt :RunRealStr:=PromptR(RunShortStr(X^.P1),X^.P2,X^.FldD);
  _pos    :begin S:=RunLongStr(X^.P2); Mask:=RunShortStr(X^.P1);
           N:=1; if X^.P3<>nil then N:=RunInt(X^.P3); J:=1;
        1: L:=S^.LL+1-J; I:=0;
           if (N>0) and (L>0) then begin
             I:=FindText(Mask,X^.Options,CharArrPtr(@S^.A[J]),L);
             if I>0 then begin
               J:=J+I-length(Mask); dec(N);
               if N>0 then goto 1; I:=J-1 end end;
           ReleaseStore(S); RunRealStr:=I end;
  _diskfree:begin S:=RunLongStr(X^.P1);
            RunRealStr:=DiskFree(ord(upcase(S^.A[1]))-ord('@'));
            ReleaseStore(S) end;                           {$ifdef FandSQL}
  _sqlfun  :if Strm1=nil then RunRealStr:=0 else begin
            S:=RunLongStr(X^.P1); RunRealStr:=Strm1^.SendTxt(S,false);
            ReleaseStore(S) end;                            {$endif}
  else RunRealStr:=0;
end end;
function RMod(X:FrmlPtr):float;
  var R1,R2:float;
  begin R1:=RunReal(X^.P1); R2:=RunReal(X^.P2); RMod:=int(R1-int(R1/R2)*R2) end;
function LastUpdate(Handle:word):float;
  var dt:DateTime;
  begin UnpackTime(GetDateTimeH(Handle),dt);
        LastUpdate:=RDate(dt.year,dt.month,dt.day,dt.hour,dt.min,dt.sec,0) end;
function TypeDay(R:float):word;
  var i,d:word;
  begin if (R>=WDaysFirst) and (R<=WDaysLast) then
    begin d:=trunc(R-WDaysFirst); for i:=1 to NWDaysTab do
     if WDaysTab^[i].Nr=d then begin TypeDay:=WDaysTab^[i].Typ; exit end end;
    d:=longint(trunc(R)) mod 7; case d of 0:d:=2{Su};6:d:=1{Sa};else d:=0 end;
    TypeDay:=d end;
function AddWDays(R:float;N:integer;d:word):float;
begin
  if N>0 then while (N>0) and (R<=748383.0{2050}) do begin
    R:=R+1;if TypeDay(R)=d then dec(N) end
  else while (N<0) and (R>=1) do begin R:=R-1;if TypeDay(R)=d then inc(N) end;
  AddWDays:=R;
end;
function DifWDays(R1,R2:float;d:word):float;
var N:integer; x1,x2:float; neg:boolean;   r:extended;
begin
  N:=0;
  x1:=R1; x2:=R2; neg:=false;
  if x1>x2 then begin x1:=R2; x2:=R1; neg:=true end;
  x1:=x1+1;
  if (x1>=697248.0 {1910} ) and (x2<=748383.0 {2050}) then
    while x1<=x2 do
    begin if TypeDay(x1)=d then inc(N); x1:=x1+1 end;
  if neg then N:=-N; DifWDays:=int(N);
end;
function GetFileSize:longint;
var h:word; um:FileUseMode;
begin
  TestMountVol(CPath[1]); um:=RdOnly; if IsNetCVol then um:=Shared;
  h:=OpenH(_isoldfile,um); if HandleError<>0 then begin
    GetFileSize:=-1; exit end;
  GetFileSize:=FileSizeH(h); CloseH(h);
end;
function RecNoFun(Z:FrmlPtr):longint;
var k:KeyDPtr; cf:FileDPtr; cr:pointer;
    b:boolean; n:longint; md:LockMode; x:XString;
begin
  GetRecNoXString(Z,x); cf:=CFile; cr:=CRecPtr; k:=Z^.Key;
  CFile:=Z^.FD; md:=NewLMode(RdMode); CRecPtr:=GetRecSpace;
  if CFile^.NRecs>0 then begin
    if CFile^.Typ='X' then begin
      TestXFExist; b:=k^.SearchIntvl(x,false,n) end
    else b:=SearchKey(x,k,n);
    if not b then n:=-n end
  else n:=-1;
  OldLMode(md); ReleaseStore(CRecPtr); CFile:=cf; CRecPtr:=cr; RecNoFun:=n;
end;
function AbsLogRecNoFun(Z:FrmlPtr):longint;
var N:longint; md:LockMode; cf:FileDPtr; cr,p:pointer; k:KeyDPtr;
label 1;
begin
  AbsLogRecNoFun:=0; cf:=CFile; cr:=CRecPtr; MarkStore(p); k:=Z^.Key;
  N:=RunInt(Z^.Arg[1]); if N<=0 then exit;
  CFile:=Z^.FD; md:=NewLMode(RdMode); if N>CFile^.NRecs then goto 1;
  if CFile^.Typ='X' then begin TestXFExist;
    if Z^.Op=_recnolog then begin
      CRecPtr:=GetRecSpace; ReadRec(N); if DeletedFlag then goto 1;
      AbsLogRecNoFun:=k^.RecNrToNr(N) end
    else {_recnoabs} begin
      if N>k^.NRecs then goto 1;
      AbsLogRecNoFun:=k^.NrToRecNr(N) end end
  else AbsLogRecNoFun:=N;
1:OldLMode(md); ReleaseStore(p); CFile:=cf; CRecPtr:=cr;
end;
function LinkProc(X:FrmlPtr):float;
var p:pointer; md:LockMode; cf:FileDPtr; cr:pointer;
    LD:LinkDPtr; N:longint;
begin
  cf:=CFile; cr:=CRecPtr; MarkStore(p); LD:=X^.LinkLD; CFile:=LD^.FromFD;
  if X^.LinkFromRec then CRecPtr:=X^.LinkLV^.RecPtr
  else begin
    N:=RunInt(X^.LinkRecFrml); md:=NewLMode(RdMode);
    if (N<=0) or (N>CFile^.NRecs) then begin
      Set2MsgPar(CFile^.Name,LD^.RoleName); RunErrorM(md,609) end;
    CRecPtr:=GetRecSpace; ReadRec(N); OldLMode(md) end;
  if not LinkUpw(LD,N,false) then N:=-N; LinkProc:=int(N);
  ReleaseStore(p); CFile:=cf; CRecPtr:=cr;
end;
function IntTSR(X:FrmlPtr):word;
var IntNr:byte; FunNr:word; p:pointer; z:FrmlPtr;
     s:string; b:boolean; r:real; ss:LongStrPtr;
begin
  IntNr:=RunInt(X^.P1); FunNr:=RunInt(X^.P2); z:=X^.P3;
  case char(x^.N31) of
    'r':p:=z;
    'S':begin s:=RunShortStr(z); p:=@s end;
    'B':begin b:=RunBool(z); p:=@b end;
    'R':begin r:=RunReal(z); p:=@r end end;
  asm  push ds; mov cl,IntNr; mov ax,FunNr; lds dx,p;
       call @1; int 80H; pop ds; jmp @2;
    @1:pop bx; push cs; push bx;
       mov cs:[bx+1],cl;  ret;  {modify int-instruction; far ret}
    @2:mov @result,ax end;
  if z^.Op=_getlocvar then begin
    p:=ptr(Seg(MyBP^),Ofs(MyBP^)+z^.BPOfs);
    case char(x^.N31) of
    'R': FloatPtr(p)^:=r;
    'S': begin ss:=CopyToLongStr(s); TWork.Delete(LongIntPtr(p)^);
         LongIntPtr(p)^:=TWork.Store(ss); ReleaseStore(ss) end;
    'B': BooleanPtr(p)^:=b end end;
end;
function PortIn(IsWord:boolean;Port:word):word; assembler;
  asm mov dx,Port; xor ax,ax; cmp IsWord,0; je @1; in ax,dx; jmp @2;
@1:   in al,dx;
@2: end;

  var R:float; cf:FileDPtr; md:LockMode; b:boolean;
  RecNo:longint; p:pointer absolute RecNo; cr:pointer;  {$ifdef FandGraph}
  vp:ViewPortType{9 byte} absolute R;                   {$endif}
  h:word absolute R; n:word absolute cf;
  AColors:array[0..53]of byte absolute colors;
  label 1,2;
  begin
    if X=nil then RunReal:=0. else
   1:case X^.Op of
    _field  :RunReal:=_r(X^.Field);
    _getlocvar:RunReal:=FloatPtr(Ptr(Seg(MyBP^),Ofs(MyBP^)+X^.BPOfs))^;
    _const  :RunReal:=X^.R;
    _plus   :RunReal:=RunReal(x^.p1)+RunReal(x^.p2);
    _minus  :RunReal:=RunReal(X^.P1)-RunReal(X^.P2);
    _times  :RunReal:=RunReal(X^.P1)*RunReal(X^.P2);
    _access :begin cf:=CFile;cr:=CRecPtr;
                   if X^.LD<>nil then LinkUpw(X^.LD,RecNo,false)
                   else LinkLastRec(X^.File2,RecNo,false);
                   RunReal:=RunReal(X^.P1);
                   ReleaseStore(CRecPtr); CFile:=cf;CRecPtr:=cr end;
    _recvarfld: begin cf:=CFile; cr:=CRecPtr; CFile:=X^.File2; CRecPtr:=X^.LD;
                   RunReal:=RunReal(X^.P1); CFile:=cf;CRecPtr:=cr end;
    _eval:   begin MarkStore(p); RunReal:=RunReal(GetEvalFrml(X));
                   ReleaseStore(p) end;
    _divide :RunReal:=RunReal(X^.P1)/RunReal(X^.P2);
    _cond:begin 2:if X^.P1<>nil then
                  if not RunBool(X^.P1) then
                     begin if X^.P3=nil then begin RunReal:=0;exit end;
                           X:=X^.P3;goto 2 end;
                X:=X^.P2;goto 1;
          end;
    _newfile:begin cf:=CFile;cr:=CRecPtr;
                   CFile:=X^.NewFile;CRecPtr:=X^.NewRP;
                   RunReal:=RunReal(X^.Frml);
                   CFile:=cf;CRecPtr:=cr end;
    _getwordvar:RunReal:=int(WordVarArr[X^.N01]);
    _div    :RunReal:=int(RunReal(X^.P1)/RunReal(X^.P2));
    _mod    :RunReal:=RMod(X);
    _unminus:RunReal:=-RunReal(X^.P1);
    _today  :RunReal:=Today;
    _pi     :RunReal:=pi;
    _random :RunReal:=random;
    _round  :RunReal:=RoundReal(RunReal(X^.P1),RunInt(X^.P2));
    _abs    :RunReal:=abs(RunReal(X^.P1));
    _int    :RunReal:=int(RunReal(X^.P1));
    _frac   :RunReal:=frac(RunReal(X^.P1));
    _sqr    :RunReal:=sqr(RunReal(X^.P1));
    _sqrt   :RunReal:=sqrt(RunReal(X^.P1));
    _sin    :RunReal:=sin(RunReal(X^.P1));
    _cos    :RunReal:=cos(RunReal(X^.P1));
    _arctan :RunReal:=arctan(RunReal(X^.P1));
    _ln     :RunReal:=ln(RunReal(X^.P1));
    _exp    :begin R:=RunReal(X^.P1);
             if (R<=-50) or (R>88) then RunReal:=0 else RunReal:=exp(R) end;
    _nrecs,_nrecsabs :begin cf:=CFile;CFile:=X^.FD; md:=NewLMode(RdMode);
                   if X^.Op=_nrecs then RecNo:=XNRecs(CFile^.Keys)
                   else RecNo:=CFile^.NRecs;
                   OldLMode(md); RunReal:=int(RecNo); CFile:=cf end;
    _generation:begin cf:=CFile;CFile:=X^.FD;
                   RunReal:=int(Generation);CFile:=cf end;
    _lastupdate:begin cf:=CFile;
                   CFile:=X^.FD; md:=NewLMode(RdMode);
                   RunReal:=LastUpdate(CFile^.Handle);OldLMode(md);CFile:=cf end;
    _catfield:begin RdCatPathVol(X^.CatIRec); TestMountVol(CPath[1]);
              h:=OpenH(_isoldfile,RdOnly); RunReal:=LastUpdate(h); CloseH(h) end;
    _currtime:RunReal:=CurrTime;
    _typeday:RunReal:=TypeDay(RunReal(X^.P1));
    _addwdays:RunReal:=AddWDays(RunReal(X^.P1),RunInt(X^.P2),X^.N21);
    _difwdays:RunReal:=DifWDays(RunReal(X^.P1),RunReal(X^.P2),X^.N21);
    _addmonth:RunReal:=AddMonth(RunReal(X^.P1),RunReal(X^.P2));
    _difmonth:RunReal:=DifMonth(RunReal(X^.P1),RunReal(X^.P2));
    _recno:RunReal:=RecNoFun(X);
    _recnoabs,_recnolog:RunReal:=AbsLogRecNoFun(X);
    _accrecno:begin cf:=CFile;cr:=CRecPtr;AccRecNoProc(X,640);
                    RunReal:=_r(X^.RecFldD); ReleaseStore(CRecPtr);
                    CFile:=cf;CRecPtr:=cr end;
    _link:RunReal:=LinkProc(X);
    _memavail:RunReal:=int(StoreAvail);
    _maxcol:  RunReal:=int(TxtCols);
    _maxrow:  RunReal:=int(TxtRows);    {$ifdef FandGraph}
    _getmaxx: if IsGraphMode then begin
              GetViewSettings(vp); RunReal:=vp.x2-vp.x1 end else RunReal:=0;
    _getmaxy: if IsGraphMode then begin
              GetViewSettings(vp); RunReal:=vp.y2-vp.y1 end else RunReal:=0;
                                        {$endif}
    _exitcode:RunReal:=LastExitCode;
    _edrecno: RunReal:=EdRecNo;
    _txtpos:  RunReal:=LastTxtPos;
    _txtxy:   RunReal:=TxtXY;
    _cprinter: RunReal:=prCurr;
    _mousex:  if IsGraphMode then RunReal:=Event.WhereG.X
              else RunReal:=Event.Where.X+1;
    _mousey:  if IsGraphMode then RunReal:=Event.WhereG.Y
              else RunReal:=Event.Where.Y+1;
    _filesize:begin SetTxtPathVol(X^.TxtPath,X^.TxtCatIRec);
            RunReal:=GetFileSize end;
    _inttsr:RunReal:=IntTSR(X);
    _userfunc:begin RunReal:=FloatPtr(RunUserFunc(X))^;
            cr:=MyBP; PopProcStk; ReleaseStore(cr) end;
    _indexnrecs: RunReal:=X^.WKey^.NRecs;
    _owned: RunReal:=Owned(X^.ownBool,X^.ownSum,X^.ownLD);
    _color: RunReal:=AColors[minw(word(RunInt(X^.P1)),53)];
    _portin: RunReal:=PortIn(RunBool(X^.P1),word(RunInt(X^.P2)));
    _setmybp:begin cr:=MyBP; SetMyBP(ProcMyBP);
            RunReal:=RunReal(X^.P1); SetMyBP(cr) end;
    else RunReal:=RunRealStr(X);
   end;
  end;
function RunInt(Z:FrmlPtr):longint;
  begin RunInt:=trunc(RunReal(Z)) end;

function RunLongStr(X:FrmlPtr):LongStrPtr;
procedure ConcatLongStr(S1,S2:LongStrPtr); assembler;
asm  push ds; cld; lds si,S1; les di,S2; mov ax,MaxLStrLen;
     mov dx,[si]; sub ax,dx;
     mov cx,es:[di]; cmp cx,ax; jna @1; mov cx,ax;
@1:  add dx,cx; mov [si],dx;
     push es; pop ds; lea si,[di+2]; rep movsb; pop ds;
end;
procedure CopyLongStr(S:LongStrPtr;From,Number:word); assembler;
asm  push ds; les si,S; mov ax,es; mov ds,ax; cld;
     mov cx,Number; mov bx,From; cmp bx,0; je @0; dec bx;
@0:  mov ax,[si]; sub ax,bx; ja @1; mov [si].word,0; jmp @3;
@1:  cmp ax,cx; jae @2; mov cx,ax;
@2:  mov [si],cx; lea di,[si+2]; lea si,[si+2+bx]; rep movsb;
@3:  pop ds;
end;
procedure AddToLongStr(S:LongStrPtr;P:pointer;L:word);
var p2:pointer;
begin
  L:=minw(L,MaxLStrLen-S^.LL); p2:=GetStore(L); move(P^,p2^,L); inc(S^.LL,L);
end;
procedure StrMask(R:float;var Mask:string);
var Num:string; i,l,n,sw,pos,pos1:word; minus:boolean;
label 1,2,3;
begin
  sw:=2; l:=length(Mask); n:=0; pos:=l+1; pos1:=pos;
  for i:=l downto 1 do case Mask[i] of
    ',': if sw=2 then sw:=1;
    '0','*': begin pos:=i; goto 1 end;
    '_': begin if sw=1 then pos1:=i;
1:       if sw=1 then sw:=0 else if sw=2 then inc(n) end end;
  if sw=2 then n:=0; R:=R*Power10[n]; R:=RoundReal(R,0); minus:=false;
  if R=0 then Num[0]:=#0 else begin
    if R<0 then begin minus:=true; R:=-R end; str(R:1:0,Num);
    pos:=minw(pos,pos1) end;
  i:=length(Num);
  if (Num='INF') or (Num='NAN') then begin Mask:=Num;
    while (length(Mask)<l) do Mask:=' '+Mask; exit end;
  while l>0 do begin
    case Mask[l] of
      '0','*': if i>0 then goto 3;
      '.',',': if (i=0) and (l<pos) then goto 2;
      '-'    : if minus then minus:=false else Mask[l]:=' ';
      '_':     if i=0 then
                 if l>=pos then Mask[l]:='0' else
2:               if minus then begin minus:=false; Mask[l]:='-' end
                 else Mask[l]:=' '
               else begin
3:               Mask[l]:=Num[i]; dec(i) end end;
    dec(l) end;
  if i>0 then Mask:=copy(Num,1,i)+Mask; if minus then Mask:='-'+Mask;
end;
function RunS(Z:FrmlPtr):LongStrPtr;
var s,snew:string; w:word; cf:FileDPtr; cr:pointer; x:XString absolute s;
    t,tnew:LongStrPtr; l,i,j:word; r:float; m:byte;
label 1;
begin
  case Z^.Op of
  _char    :begin s[0]:=#1; s[1]:=char(trunc(RunReal(Z^.P1))) end;
  _strdate :s:=StrDate(RunReal(Z^.P1),Z^.Mask);
  _str     :if Z^.P3<>nil then begin r:=RunReal(Z^.P1);
              l:=RunInt(Z^.P2); m:=RunInt(Z^.P3);
              if m=255 then str(r:l,s) else str(r:l:m,s) end
            else begin s:=RunShortStr(Z^.P2); StrMask(RunReal(Z^.P1),s) end;
  _replace:begin t:=RunLongStr(Z^.P2); s:=RunShortStr(Z^.P1); j:=1;
           snew:=RunShortStr(Z^.P3); tnew:=GetZStore(2);
1:         l:=t^.LL-(j-1); if l>0 then begin
             i:=FindText(s,Z^.Options,CharArrPtr(@t^.A[j]),l);
             if i>0 then begin
               AddToLongStr(tnew,@t^.A[j],i-length(s)-1);
               AddToLongStr(tnew,@snew[1],length(snew));
               inc(j,i-1); goto 1 end end;
           AddToLongStr(tnew,@t^.A[J],l); MyMove(tnew^,t^,tnew^.LL+2);
           ReleaseAfterLongStr(t); RunS:=t; exit end;
  _prompt  :s:=PromptS(RunShortStr(Z^.P1),Z^.P2,Z^.FldD);
  _getpath :begin s:='.*'; if Z^.P1<>nil then s:=RunShortStr(Z^.P1);
            s:=SelectDiskFile(s,35,false) end;
  _catfield:begin s:=RdCatField(Z^.CatIRec,Z^.CatFld);
            if Z^.CatFld=CatPathName then s:=FExpand(s) end;
  _password:s:=PassWord(false);
  _readkey :begin
            ReadKbd; s[1]:=char(lo(KbdChar)); s[0]:=#1;
            if s[1]=#0 then begin
        asm  mov al,KbdChar[1].byte; cmp al,0; jne @1; mov al,03H; jmp @2;
         @1: cmp al,03H; jne @2; mov al,84H;
         @2: mov s[2].byte,al; mov s.byte,2 end end
            end;
  _username:s:=UserName;
  _accright:s:=AccRight;
  _version :s:=version;
  _edfield :s:=EdField;
  _edfile  :begin s[0]:=#0; if EditDRoot<>nil then s:=EditDRoot^.FD^.Name end;
  _edkey   :s:=EdKey;
  _edreckey:s:=EdRecKey;
  _getenv  :begin s:=RunShortStr(Z^.P1);
            if s='' then s:=paramstr(0) else s:=getenv(s) end;
  _keyof   :begin cf:=CFile; cr:=CRecPtr; with Z^.LV^ do begin
              CFile:=FD; CRecPtr:=RecPtr end;
            x.PackKF(Z^.PackKey^.KFlds); CFile:=cf; CRecPtr:=cr end;
  _keybuf  :begin while KeyPressed do AddToKbdBuf(ReadKey); s:=KbdBuffer end;
  _recno   :GetRecNoXString(Z,x);
  _edbool  :begin s[0]:=#0; if (EditDRoot<>nil) and EditDRoot^.Select
            and (EditDRoot^.BoolTxt<>nil) then s:=EditDRoot^.BoolTxt^ end end;
  RunS:=CopyToLongStr(s);
 end;
function RunSelectStr(Z:FrmlPtr):LongStrPtr;
var s,s2:LongStrPtr; x:string[80]; mode:string[5]; p2,pl:pointer; i,n:word;
begin
  s:=RunLongStr(Z^.P3); n:=CountDLines(s^.A,s^.LL,Z^.Delim);
  for i:=1 to n do begin
    x:=GetDLine(s^.A,s^.LL,Z^.Delim,i);
    if x<>'' then PutSelect(x) end;
  mode:=RunShortStr(Z^.P6);
  for i:=1 to length(mode) do case upcase(mode[i]) of
      'A':ss.Abcd:=true; 'S':ss.Subset:=true; 'I':ss.ImplAll:=true end;
  SetMsgPar(RunShortStr(Z^.P4));
  SelectStr(RunInt(Z^.P1),RunInt(Z^.P2),110,RunShortStr(Z^.P5));
  MarkStore2(p2); s2:=GetStore2(s^.LL+2); n:=1; LastExitCode:=0;
  if KbdChar=_ESC_ then LastExitCode:=1 else
  repeat x:=GetSelect; if x<>'' then begin
    if n>1 then begin s2^.A[n]:=^m; inc(n) end;
    Move(x[1],s2^.A[n],length(x)); inc(n,length(x)) end;
  until not ss.Subset or (x='');
  ReleaseStore(s); s:=GetStore(n+1); s^.LL:=n-1; move(s2^.A,s^.A,n-1);
  ReleaseStore2(p2); RunSelectStr:=s;
end;
procedure LowCase(S:LongStrPtr); assembler;
asm  push ds; mov ax,ds; mov es,ax; lea bx,UpcCharTab[128];
     lds si,S; cld; lodsw; mov cx,ax; jcxz @6;
@1:  mov al,[si]; cmp al,80H; jae @2;
     cmp al,'A'; jb @5; cmp al,'Z'; ja @5; add al,20H; jmp @5;
@2:  push cx; mov cx,128; mov di,bx;
@3:  repne scasb;
     jcxz @4; mov dx,255; sub dx,cx; cmp dl,al; je @3; mov al,dl;
@4:  pop cx;
@5:  mov [si],al; inc si; loop @1;
@6:  pop ds;
end;
  var S:LongStrPtr; b:boolean;
      I:word;  md:LockMode absolute I;   J:integer absolute I;
      RecNo:longint; N:word absolute RecNo; L1:longint absolute RecNo;
      cf:FileDPtr; cr:pointer; L2:longint absolute cr;
      p:pointer absolute RecNo;
  label 1,2;
  begin
    if X=nil then RunLongStr:=GetZStore(2) else
   1:case X^.Op of
    _field  :RunLongStr:=_longs(X^.Field);
    _getlocvar :RunLongStr:=TWork.Read(1,LongintPtr(Ptr(Seg(MyBP^),Ofs(MyBP^)+X^.BPOfs))^);
    _access :begin cf:=CFile;cr:=CRecPtr;
                   CFile:=X^.File2; md:=NewLMode(RdMode);
                   if X^.LD<>nil then begin CFile:=cf; LinkUpw(X^.LD,RecNo,true) end
                   else LinkLastRec(X^.File2,RecNo,true);
                   S:=RunLongStr(X^.P1);
                   OldLMode(md);  {possibly reading .T}
                   ClearRecSpace(CRecPtr); MyMove(S^,CRecPtr^,S^.LL+2);
                   ReleaseAfterLongStr(CRecPtr); RunLongStr:=CRecPtr;
                   CFile:=cf;CRecPtr:=cr end;
    _recvarfld: begin cf:=CFile;cr:=CRecPtr;
                   CFile:=X^.File2;CRecPtr:=X^.LD;
                   RunLongStr:=RunLongStr(X^.P1);
                   CFile:=cf;CRecPtr:=cr end;
    _eval:   begin MarkStore(p);
             S:=RunLongStr(GetEvalFrml(X)); MyMove(S^,p^,S^.LL+2);
             ReleaseAfterLongStr(p); RunLongStr:=p end;
    _newfile:begin cf:=CFile;cr:=CRecPtr;
                   CFile:=X^.NewFile;CRecPtr:=X^.NewRP;
                   RunLongStr:=RunLongStr(X^.Frml);
                   CFile:=cf;CRecPtr:=cr end;
    _cond   :begin 2:if X^.P1<>nil then
                     if not RunBool(X^.P1) then
                        begin if X^.P3=nil then
                          begin S:=GetZStore(2); RunLongStr:=S; exit end;
                          X:=X^.P3; goto 2 end;
                     X:=X^.P2;goto 1;
             end;
    _copy   :begin S:=RunLongStr(X^.P1); L1:=RunInt(X^.P2); L2:=RunInt(X^.P3);
                   if (L1<0) or (L2<0) then S^.LL:=0 else CopyLongStr(S,L1,L2);
                   ReleaseAfterLongStr(S);
                   RunLongStr:=S end;
    _concat :begin S:=RunLongStr(X^.P1);
                   ConcatLongStr(S,RunLongStr(X^.P2));
                   ReleaseAfterLongStr(S); RunLongStr:=S end;
    _const  :RunLongStr:=CopyToLongStr(X^.S);
    _leadchar:RunLongStr:=LongLeadChar(char(X^.N11),char(X^.N12),RunLongStr(X^.P1));
    _trailchar:RunLongStr:=LongTrailChar(char(X^.N11),char(X^.N12),RunLongStr(X^.P1));
    _upcase:begin S:=RunLongStr(X^.P1);
            for i:=1 to S^.LL do S^.A[i]:=UpcCharTab[S^.A[i]];
            RunLongStr:=S end;
    _lowcase:begin S:=RunLongStr(X^.P1); LowCase(S); RunLongStr:=S end;
    _copyline:begin j:=1; if X^.P3<>nil then j:=RunInt(X^.P3);
              RunLongStr:=CopyLine(RunLongStr(X^.P1),RunInt(X^.P2),j) end;
    _repeatstr:RunLongStr:=RepeatStr(RunLongStr(X^.P1),RunInt(X^.P2));
    _accrecno:begin cf:=CFile;cr:=CRecPtr;AccRecNoProc(X,640);
                   S:=_longs(X^.RecFldD);  MyMove(S^,CRecPtr^,S^.LL+2);
                   ReleaseAfterLongStr(CRecPtr); RunLongStr:=CRecPtr;
                   CFile:=cf;CRecPtr:=cr end;
    _gettxt: RunLongStr:=GetTxt(X);
    _nodiakr:begin S:=RunLongStr(X^.P1); ConvToNoDiakr(S^.A,S^.LL,Fonts.VFont);
                  RunLongStr:=S end;
    _userfunc:begin cr:=RunUserFunc(X);
             L1:=LongintPtr(cr)^; LongintPtr(cr)^:=0;
             cr:=MyBP; PopProcStk; ReleaseStore(cr);
             RunLongStr:=TWork.Read(1,L1); TWork.Delete(L1) end;
    _setmybp:begin cr:=MyBP; SetMyBP(ProcMyBP);
            RunLongStr:=RunLongStr(X^.P1); SetMyBP(cr) end;
    _selectstr:RunLongStr:=RunSelectStr(X);
    _clipbd:RunLongStr:=TWork.Read(1,ClpBdPos);
    else RunLongStr:=RunS(X);
   end;
  end;

function RunShortStr(X:FrmlPtr):string;
var s:LongStrPtr;
begin
  s:=RunLongStr(X);
  asm  push ds; cld; les di,@result; lds si,s;
       lodsw; cmp ax,255; jna @1; mov ax,255;
  @1:  mov cx,ax; stosb; rep movsb; pop ds end;
  ReleaseStore(s);
end;


function RunBool(X:FrmlPtr):boolean;
function InReal(R:float;L:BytePtr;M:integer):boolean;
var LOffs:word absolute L; Cr:^float absolute L; I,N:integer;
label 1;
begin
  InReal:=true;
1:N:=L^;inc(LOffs); if N=0 then begin InReal:=false;exit end;
  if N=$FF then
    if CompReal(R,Cr^,M)=ord(_lt) then inc(LOffs,2*sizeof(float))
    else begin inc(LOffs,sizeof(float));
      if CompReal(R,Cr^,M)<>ord(_gt) then exit; inc(LOffs,sizeof(float)) end
  else for I:=1 to N do
    if CompReal(R,Cr^,M)=ord(_equ) then exit else inc(LOffs,sizeof(float));
  goto 1;
end;
function LexInStr(S:LongStrPtr;L:BytePtr):boolean;
var LOffs:word absolute L; Cs:StringPtr absolute L; I,N:integer;
label 1;
begin
  LexInStr:=true;
1:N:=L^;inc(LOffs); if N=0 then begin LexInStr:=false;exit end;
  if N=$FF then
    if CompLexLongShortStr(S,Cs)=ord(_lt) then begin
      inc(LOffs,L^+1); inc(LOffs,L^+1) end
    else begin inc(LOffs,L^+1);
      if CompLexLongShortStr(S,Cs)<>ord(_gt) then exit; inc(LOffs,L^+1) end
  else for I:=1 to N do
    if CompLexLongShortStr(S,Cs)=ord(_equ) then exit else inc(LOffs,L^+1);
  goto 1;
end;
function InStr(S:LongStrPtr;L:BytePtr):boolean;
var LOffs:word absolute L; Cs:StringPtr absolute L; I,N:integer;
label 1;
begin
  InStr:=true;
1:N:=L^;inc(LOffs); if N=0 then begin InStr:=false;exit end;
  if N=$FF then
    if CompLongShortStr(S,Cs)=ord(_lt) then begin
      inc(LOffs,L^+1); inc(LOffs,L^+1) end
    else begin inc(LOffs,L^+1);
      if CompLongShortStr(S,Cs)<>ord(_gt) then exit; inc(LOffs,L^+1) end
  else for I:=1 to N do
    if CompLongShortStr(S,Cs)=ord(_equ) then exit else inc(LOffs,L^+1);
  goto 1;
end;
function RunModulo(X:FrmlPtr):boolean;
var S:string; I,M,N:integer; B1:WordPtr; B1Offs:word absolute B1;
begin
  N:=X^.W11; S:=RunShortStr(X^.P1);
  if Length(S)<>N then begin RunModulo:=false; exit end;
  M:=0; B1:=@X^.W21;
  for I:=1 to N-1 do begin
    M:=M+B1^*(ord(S[I]) and $0F);inc(B1Offs,2) end;
  I:=(X^.W12-(M mod X^.W12)) mod 10;
  if I=(ord(S[N]) and $0F) then RunModulo:=true else RunModulo:=false;
end;
function RunEquMask(X:FrmlPtr):boolean;
var s:LongStrPtr;
begin
  s:=RunLongStr(X^.P1); RunEquMask:=EqualsMask(@s^.A,s^.LL,RunShortStr(X^.P2));
  ReleaseStore(s);
end;

var RecNo:longint; res:word absolute RecNo;
    S,S2:LongStrPtr; b:boolean absolute S;
    w1:word absolute RecNo; w2:word absolute S;
    cf:FileDPtr; cr:pointer; p:pointer absolute RecNo;
label 1,2;
begin
  if X=nil then begin RunBool:=True;exit end;
  case X^.Op of
    _and:if RunBool(X^.P1) then RunBool:=RunBool(X^.P2) else RunBool:=False;
    _or:if RunBool(X^.P1) then RunBool:=True else RunBool:=RunBool(X^.P2);
    _lneg:RunBool:=not RunBool(X^.P1);
    _limpl:if RunBool(X^.P1) then RunBool:=RunBool(X^.P2)
           else RunBool:=True;
    _lequ:if RunBool(X^.P1)=RunBool(X^.P2) then RunBool:=True
           else RunBool:=False;
    _instr:begin S:=RunLongStr(X^.P1);
           if X^.N11=1 then RunBool:=LexInStr(LongTrailChar(' ',#0,S),@X^.N12)
           else RunBool:=InStr(S,@X^.N12);
           ReleaseStore(S) end;
    _inreal:begin RunBool:=InReal(RunReal(X^.P1),@X^.N12,X^.N11) end;
    _compreal:begin RunBool:=
            (CompReal(RunReal(X^.P1),RunReal(X^.P2),X^.N22) and X^.N21)<>0 end;
    _compstr:begin S:=RunLongStr(X^.P1); S2:=RunLongStr(X^.P2);
            if X^.N22=1 then
               res:=CompLexLongStr(LongTrailChar(' ',#0,S),LongTrailChar(' ',#0,S2))
            else res:=CompLongStr(S,S2);
            RunBool:=res and X^.N21<>0; ReleaseStore(S) end;
    _const:RunBool:=X^.B;
    _mouseevent:begin
2:         Event.What:=0; GetMouseEvent; if Event.What=0 then RunBool:=false
           else begin if (Event.What and X^.W01)=0 then goto 2; RunBool:=true end end;
    _ismouse: begin RunBool:=false; if ((Event.What and X^.W01)<>0) and
              ((Event.Buttons and X^.W02)=X^.W02) then RunBool:=true end;
    _mousein: begin w1:=RunInt(X^.P1); w2:=RunInt(X^.P2);
              RunBool:=MouseInRectProc(w1,w2,RunInt(X^.P3)-w1+1,RunInt(X^.P4)-w2+1) end;
    _getlocvar:RunBool:=BooleanPtr(Ptr(Seg(MyBP^),Ofs(MyBP^)+X^.BPOfs))^;
    _modulo:RunBool:=RunModulo(X);
    _field  :RunBool:=_b(X^.Field);
    _access :begin cf:=CFile;cr:=CRecPtr;
                   if X^.LD<>nil then b:=LinkUpw(X^.LD,RecNo,false)
                   else b:=LinkLastRec(X^.File2,RecNo,false);
                   if (X^.P1=nil) then RunBool:=b
                   else RunBool:=RunBool(X^.P1);
                   ReleaseStore(CRecPtr); CFile:=cf; CRecPtr:=cr;
             end;
    _recvarfld: begin cf:=CFile;cr:=CRecPtr;
                   CFile:=X^.File2;CRecPtr:=X^.LD;RunBool:=RunBool(X^.P1);
                   CFile:=cf;CRecPtr:=cr end;
    _eval:   begin MarkStore(p); RunBool:=RunBool(GetEvalFrml(X));
             ReleaseStore(p) end;
    _newfile:begin cf:=CFile;cr:=CRecPtr;
                     CFile:=X^.NewFile;CRecPtr:=X^.NewRP;
                     RunBool:=RunBool(X^.Frml);
                     CFile:=cf;CRecPtr:=cr end;
    _prompt:RunBool:=PromptB(RunShortStr(X^.P1),X^.P2,X^.FldD);
    _promptyn:begin SetMsgPar(RunShortStr(X^.P1)); RunBool:=PromptYN(110) end;
    _accrecno:begin cf:=CFile;cr:=CRecPtr; AccRecNoProc(X,640);
                    RunBool:=_b(X^.RecFldD); ReleaseStore(CRecPtr);
                    CFile:=cf;CRecPtr:=cr end;
    _edupdated:RunBool:=EdUpdated;
    _keypressed:RunBool:=keypressed; {Kbdpressed?}
    _escprompt:RunBool:=EscPrompt;
    _isdeleted:begin cr:=CRecPtr; cf:=CFile; AccRecNoProc(X,642);
                    RunBool:=DeletedFlag; ReleaseStore(CRecPtr);
                    CRecPtr:=cr; CFile:=cf end;
    _lvdeleted:begin cr:=CRecPtr; cf:=CFile; CRecPtr:=X^.LV^.RecPtr; CFile:=X^.LV^.FD;
           RunBool:=DeletedFlag; CRecPtr:=cr; CFile:=cf end;
    _trust:RunBool:=(UserCode=0) or OverlapByteStr(@X^.N01,@AccRight);
    _isnewrec:RunBool:=TestIsNewRec;
    _testmode:RunBool:=IsTestRun;
    _equmask:RunBool:=RunEquMask(X);
    _userfunc:begin RunBool:=BooleanPtr(RunUserFunc(X))^;
            cr:=MyBP; PopProcStk; ReleaseStore(cr) end;
    _setmybp:begin cr:=MyBP; SetMyBP(ProcMyBP);
            RunBool:=RunBool(X^.P1); SetMyBP(cr) end;
  end;
end;
procedure TestTFrml(F:FieldDPtr;Z:FrmlPtr);
var cf:FileDPtr; p:pointer; n:longint; md:LockMode; f1:FieldDPtr;
begin
  case Z^.Op of
  _newfile: begin CFile:=Z^.NewFile; CRecPtr:=Z^.NewRP; TestTFrml(F,Z^.Frml) end;
  _field  :begin f1:=Z^.Field;
          if (f1^.Typ<>'T') or (f1^.Flg and f_Stored=0) then exit;
          if F=nil then begin if f1^.Flg and f_Encryp<>0 then exit end
          else if F^.Flg and f_Encryp<>f1^.Flg and f_Encryp then exit;
          TFD02:=CFile; TF02:=CFile^.TF; if HasTWorkFlag then TF02:=@TWork;
          TF02Pos:=_t(f1) end;
  _getlocvar : begin
           if (F<>nil) and (F^.Flg and f_Encryp<>0) then exit;
           TFD02:=CFile; TF02:=@TWork;
           TF02Pos:=LongintPtr(Ptr(Seg(MyBP^),Ofs(MyBP^)+Z^.BPOfs))^ end;
  _access : begin cf:=CFile; MarkStore(p); CFile:=Z^.File2;
           md:=NewLMode(RdMode);
           if Z^.LD<>nil then begin CFile:=cf; LinkUpw(Z^.LD,n,true) end
           else LinkLastRec(Z^.File2,n,true);
           TestTFrml(F,Z^.P1);
           CFile:=Z^.File2; OldLMode(md); ReleaseStore(p) end;
  _recvarfld: begin CFile:=Z^.File2; CRecPtr:=Z^.LD; TestTFrml(F,Z^.P1) end;
end end;
function CanCopyT(F:FieldDPtr;Z:FrmlPtr):boolean;
var cf:FileDPtr; cr:pointer;
begin
  cf:=CFile; cr:=CRecPtr; TF02:=nil; CanCopyT:=false; TestTFrml(F,Z);
  CFile:=cf; CRecPtr:=cr; CanCopyT:=TF02<>nil;
end;
function TryCopyT(F:FieldDPtr;TF:TFilePtr;var pos:longint;Z:FrmlPtr):boolean;
var md,md2:LockMode;
begin
  TryCopyT:=false; if TF^.Format in [DbtFormat,FptFormat] then exit;
  if Z^.Op=_gettxt then begin pos:=CopyTFFromGetTxt(TF,Z); TryCopyT:=true end
  else if CanCopyT(F,Z) and (TF02^.Format=TF^.Format) then begin
    TryCopyT:=true; pos:=CopyTFString(TF,TFD02,TF02,TF02Pos) end;
end;

procedure AssgnFrml(F:FieldDPtr;X:FrmlPtr;Delete,Add:boolean);
var s:LongStrPtr; pos:longint; tf:TFilePtr;
begin
  case F^.FrmlTyp of
   'S':if F^.Typ='T' then begin
         if HasTWorkFlag then tf:=@TWork else tf:=CFile^.TF;
         if TryCopyT(F,tf,pos,X) then begin
           if Delete then DelTFld(F); t_(F,pos) end
         else begin
           S:=RunLongStr(X); if Delete then DelTFld(F); longs_(F,S);
           ReleaseStore(S) end end
       else s_(F,RunShortStr(X));
   'R':if Add then r_(F,_r(F)+RunReal(X)) else r_(F,RunReal(X));
   'B':b_(F,RunBool(X)) end;
end;
procedure LVAssignFrml(LV:LocVarPtr;OldBP:pointer;Add:boolean;X:FrmlPtr);
var p,bp:pointer; s:LongStrPtr; pos:longint;
begin
  p:=LocVarAd(LV); bp:=MyBP; SetMyBP(OldBP); case LV^.FTyp of
   'S':begin if not TryCopyT(nil,@TWork,pos,X) then begin
         s:=RunLongStr(X); pos:=TWork.Store(s); ReleaseStore(s) end;
       TWork.Delete(LongIntPtr(p)^); LongIntPtr(p)^:=pos end;
   'R':if Add then FloatPtr(p)^:=FloatPtr(p)^+RunReal(X)
       else FloatPtr(p)^:=RunReal(X);
   'B':BooleanPtr(p)^:=RunBool(X) end;
  SetMyBP(bp);
end;

procedure DecodeFieldRSB(F:FieldDPtr;LWw:word;R:float;T:string;B:boolean;
                         var Txt:string);
  var L,M:word; C:char;
  label 1;
  begin L:=F^.L; M:=F^.M; case F^.Typ of
         'D':T:=StrDate(R,FieldDMask(F)^);
         'N':begin C:='0';goto 1 end;
         'A':begin C:=' ';
                 1:if M=LeftJust then
                      while length(T)<L do T:=T+C
                   else while length(T)<L do T:=C+T;
             end;
         'B':if B then T:=AbbrYes else T:=AbbrNo;
         'R':str(R:L,T);
         else {'F'}
             if F^.Flg and f_Comma<>0 then R:=R/Power10[M];
             str(RoundReal(R,M):L:M,T);
        end;
       if length(T)>L then begin T[0]:=char(L); T[L]:='>' end;
       if length(T)>LWw then if M=LeftJust then T[0]:=char(LWw)
          else T:=copy(T,length(T)-LWw+1,LWw);
       Txt:=T;
  end;

procedure DecodeField(F:FieldDPtr;LWw:word;var Txt:string);
var r:float;s:string;b:boolean;
begin
  case F^.FrmlTyp of
    'R': r:=_r(F);
    'S':if F^.Typ='T' then begin
          if (F^.Flg and f_Stored<>0) and (_t(F)=0) then Txt:='.' else Txt:='*'; exit end
        else s:=_shorts(F);
  else b:=_b(F) end;
  DecodeFieldRSB(F,LWw,r,s,b,Txt);
end;

procedure RunWFrml(X:WRectFrml;WFlags:byte;var W:WRect);
  var i:word;
      XA:array[0..3]of FrmlPtr absolute X; WA:array[0..3]of byte absolute W;
begin for i:=0 to 3 do WA[i]:=RunInt(XA[i]);
      CenterWw(W.C1,W.R1,W.C2,W.R2,WFlags) end;
function RunWordImpl(Z:FrmlPtr;Impl:word):word;
var n:word;
begin
  n:=RunInt(Z); if n=0 then n:=Impl; RunWordImpl:=n;
end;

function FieldInList(F:FieldDPtr; FL:FieldList):boolean;
begin
  FieldInList:=false; while FL<>nil do begin
    if FL^.FldD=F then FieldInList:=true; FL:=FL^.Chain end;
end;

function GetFromKey(LD:LinkDPtr):KeyDPtr;
  var K:KeyDPtr;
  begin K:=LD^.FromFD^.Keys; while K^.IndexRoot<>LD^.IndexRoot do K:=K^.Chain;
        GetFromKey:=K end;
function RunEvalFrml(Z:FrmlPtr):FrmlPtr;
begin
  if (Z<>nil) and (Z^.Op=_eval) then Z:=GetEvalFrml(Z);
  RunEvalFrml:=Z;
end;

end.