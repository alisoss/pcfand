{$I Switches}
{$A-,B-,F+,G-,I+,O-,R-,S+,V+,X+,L-}
unit Channel;

interface

uses dos,drivers,base,access;

type
  LANStreamPtr = ^LANStream;
  LANStream = object(TObject)
    constructor Init;
    destructor Done; virtual;
    procedure Write(P:pointer;L:word);
    procedure Flush;
    procedure Read(P:pointer;L:word);
    function  AtEnd:boolean;
    procedure Terminate;
  private
    StreamNr,SequNr:byte;
    Buf,Buf2,Requ:pointer;
    iBuf:word;
    Busy,RequWait,Receiving:boolean;
    EndTime:longint;
    procedure Send;
    procedure Listen(Blk:pointer;L:word;var Err:word);
    procedure ReceiveWait;
  end;
  SQLStreamPtr = ^SQLStream;
  SQLStream = object(LANStream)
    procedure Login(UserName,Password:string);
    function  SelectXRec(K:KeyDPtr;X:XStringPtr;CompOp:char;WithT:boolean):boolean;
    procedure DeleteXRec(K:KeyDPtr;X:XStringPtr;Ad:boolean);
    function  UpdateXRec(K:KeyDPtr;X:XStringPtr;Ad:boolean):boolean;
    procedure UpdateXFld(K:KeyDPtr;X:XStringPtr;F:FieldDPtr);
    function  InsertRec(Ad,Cancel:boolean):boolean;
    procedure ReadTFld(K:KeyDPtr;X:XStringPtr;F:FieldDPtr);
    procedure WriteTFld(K:KeyDPtr;X:XStringPtr;F:FieldDPtr);
    function  SendTxt(S:LongStrPtr;Cancel:boolean):longint;
    procedure DefKeyAcc(K:KeyDPtr);
    function  KeyAcc(K:KeyDPtr;X:XStringPtr):boolean;
    procedure EndKeyAcc(K:KeyDPtr);
    procedure InpResetTxt(S:LongStrPtr);
    procedure InpReset(K:KeyDPtr;SK:KeyFldDPtr;KI:KeyInDPtr;Filter:FrmlPtr;WithT:boolean);
    function  GetRec:boolean;
    procedure InpClose;
    procedure OutpRewrite(Append:boolean);
    procedure PutRec;
    procedure OutpClose;
  private
    procedure WrChar(C:char);
    procedure WrString(S:string);
    procedure WrLString(S:string);
    function  Switch(Cancel:boolean):boolean;
    function  GetTableName:string;
    procedure WrFldDefs;
    procedure WrKeyFldDefs(KF:KeyFldDPtr;CompOp:char);
    procedure WrCompKey(K:KeyDPtr;CompOp:char;X:XStringPtr);
  end;

procedure SQLConnect;
procedure SQLDisconnect;
procedure ShutDownStreams(N:word);

const Strm1:SQLStreamPtr=nil;
const nStreams:word=0;

implementation

uses obaseww,oaccess,runfrml,rdrun;

{$I ipx}

const
  MaxStreams  = 15;
  MaxAckBlks  = 3;
  MaxBlkSz    = 546;    { get from Connect !!!!! }
  LanBlkPrfxSz= 7;
  LanBlkSz    = LanBlkPrfxSz+MaxBlkSz;
  LanBlkHdSz  = 4;
  UsedLanBlkSz= MaxBlkSz-LanBlkHdSz;
  MaxSendCnt  = 3;
  SendDelay   = 18; {ca. 1 sec}
  RequDelay   = 55; {ca. 3 sec}
  AnswerDelay = 109200; {ca. 100 min}
  ReceiveDelay= 55; {ca. 3 sec}

type
  LanBlkHdPtr = ^LanBlkHd;
  LanBlkHd = record
    Chain:LanBlkHdPtr;
    Len:word;
    Busy:boolean;
    Typ:(landata,request,acknowledge,watchdog,shutdown);
    StreamNr,SequNr:byte;
    IsLast:boolean;
    A:array[0..1]of byte;
  end;
  MsgTyp = (SQLInstr,SQLQuery,SQLOutput,SQLLogin,
            SQLDefKeyAcc,SQLKeyAcc,SQLEndKeyAcc,
            SQLInsert,SQLUpdate,SQLRdTFld,SQLWrTFld,
            SQLDelete,SQLSelect,SQLInput);

var
  StreamTab:array[1..MaxStreams] of LANStreamPtr;
  MpxNoAck:boolean;
  MpxTime:longint;
  MpxCnt:word;
  MpxBlk,MpxQueue:LanBlkHdPtr;
  AckBlk:array[0..MaxAckBlks-1] of LanBlkHd;
  NRecsAffected:longint;
  SaveErrNr:word;
const
  WDog:LanBlkHd=(Chain:nil;Len:1;Busy:false;Typ:watchdog);
  LgOut:LanBlkHd=(Chain:nil;Len:1;Busy:false;Typ:shutdown);


{$S-}
procedure MpxExecSend;
begin
  MpxNoAck:=true; MpxTime:=Timer+SendDelay; MpxCnt:=1;
  SendBlock(@MpxBlk^.Typ,MpxBlk^.Len);
end;

procedure MpxTest;
begin
  asm pushf; sti end;
  if MpxNoAck and (Timer>MpxTime) then
    if MpxCnt<MaxSendCnt then begin
      inc(MpxCnt); MpxTime:=Timer+SendDelay; SendBlock(@MpxBlk^.Typ,MpxBlk^.Len) end
    else RunError(2004);
  asm popf end;
end;

procedure MpxSend(B:LanBlkHdPtr);
begin
  asm pushf; sti end;  B^.Busy:=true;
  if MpxNoAck then ChainLast(MpxQueue,B) else begin
    MpxBlk:=B; MpxExecSend end;
  asm popf end;
end;

procedure MpxSendAck(B:LanBlkHdPtr);
var i:word;
begin
  for i:=0 to MaxAckBlks do with AckBlk[i] do if not Busy then begin
    StreamNr:=B^.StreamNr; SequNr:=B^.SequNr; MpxSend(@AckBlk[i]); exit end;
  { ACK lost }
end;

const
  ErrText:array[1..6]of string[24]=(
    'chyba p©i p©ijmu bloku',
    'vadn˜ blok potvrzen¡',
    'vadn˜ request block',
    'vadn˜/misplaced blok dat',
    '‡¡slo streamu neexistuje',
    'inkonzistentni blok');
procedure MpxPost(FromSend:boolean;Blk:pointer;L,Err:word); far; {S- needed}
var B:LanBlkHdPtr absolute Blk; BOfs:word absolute Blk; S:LANStreamPtr;
label 1;
begin                           { interrupts are disabled }
  dec(Bofs,LanBlkPrfxSz);
  if FromSend then begin
    if MpxBlk^.Typ<>landata then begin MpxNoAck:=false; MpxBlk^.Busy:=false end;
    exit end;
  if Err=0 then case B^.Typ of
    watchdog   :if L=1 then begin MpxSend(@WDog); exit end else Err:=6;
    acknowledge:if L<>LanBlkHdSz-1 then Err:=6 else
          if not MpxNoAck or not (MpxBlk^.Typ=landata) or
             (MpxBlk^.StreamNr<>B^.StreamNr) or (MpxBlk^.SequNr<>B^.SequNr) then
            Err:=2
          else begin
            MpxNoAck:=false; MpxBlk^.Busy:=false; if MpxQueue<>nil then begin
              MpxBlk:=MpxQueue; MpxQueue:=MpxBlk^.Chain; MpxExecSend end;
            exit end;
    request    :if L<>LanBlkHdSz-1 then Err:=6 else goto 1;
    else if L<=LanBlkHdSz then Err:=6 else begin
1:    S:=StreamTab[B^.StreamNr];
      if (B^.StreamNr>0) and (B^.StreamNr<=MaxStreams) and (S<>nil) then begin
        S^.Listen(B,L,Err);
        if (B^.Typ=landata) and (err=0) {correct or last repeated} then
          MpxSendAck(B) end
      else Err:=5 end;
     end;
  if Err<>0 then begin
    asm sti end; MsgLine:='SQL server spojen¡: '+ErrText[Err]; WrLLF10MsgLine end;
end;
{$S+}

procedure SQLConnect;
var i,err:word; {srvlan,l:longint; s:string;}
label 1;
begin
  if CatFD^.Handle<>$ff then for i:=1 to CatFD^.NRecs do
    if SEquUpcase(RdCatField(i,CatVolume),'#SQL') then goto 1;
  exit;
1:fillchar(StreamTab,sizeof(StreamTab),0); nStreams:=0;
 { s:=GetEnv('SRVLAN'); if s='' then RunError(659); SaveErrNr:=0;
  l:=HexStrToLong(s); LongRec(srvlan).Hi:=swap(LongRec(l).Lo);
  LongRec(srvlan).Lo:=swap(LongRec(l).Hi);}
  MpxNoAck:=false; MpxQueue:=nil;
  for i:=0 to MaxAckBlks-1 do with AckBlk[i] do begin
    Len:=LanBlkHdSz-1; Busy:=false; Typ:=acknowledge end;
  err:=Connect(MpxPost,{srvlan}0); if err<>0 then RunError(err);
  New(Strm1,Init);
end;
procedure SQLDisconnect;
begin if Strm1<>nil then begin
  MpxSend(@LgOut); delay(100);
  Strm1^.Done; Strm1:=nil; DisConnect;
end end;
procedure ShutDownStreams(N:word);
var i:word;
begin
  for i:=nStreams downto N+1 do StreamTab[i]^.Done;
end;

constructor LANStream.Init;
var i:word; Bf:LanBlkHdPtr absolute Buf; Bf2:LanBlkHdPtr absolute Buf2;
    Rq:LanBlkHdPtr absolute Requ;
begin
  inherited Init;
  Buf:=GetZStore(LanBlkSz); Buf2:=GetZStore(LanBlkSz);
  for i:=1 to MaxStreams do if StreamTab[i]=nil then begin
    StreamNr:=i; StreamTab[i]:=@Self; Bf^.StreamNr:=i; Bf2^.StreamNr:=i;
    Requ:=GetZStore(sizeof(LanBlkHd)-1); nStreams:=maxw(nStreams,i);
    Rq^.Typ:=request; Rq^.StreamNr:=i; Rq^.Len:=LanBlkHdSz-1; exit end;
  RunError(2011);
end;

destructor LANStream.Done;
var i:word;
begin
  StreamTab[StreamNr]:=nil;
  nStreams:=0; for i:=1 to MaxStreams do if StreamTab[i]<>nil then nStreams:=i;
  inherited Done;
end;

procedure LANStream.Send;
var p:pointer; Bf:LanBlkHdPtr absolute Buf;  Bf2:LanBlkHdPtr absolute Buf2;
begin
  while RequWait and (Timer<EndTime) do MpxTest;
  if RequWait then RunError(2007);
  inc(SequNr); Bf^.SequNr:=SequNr; Bf^.Len:=iBuf+LanBlkHdSz; iBuf:=0;
  p:=Buf; Buf:=Buf2; Buf2:=p; Bf^.IsLast:=false;
  if Bf2^.IsLast then begin
    Bf^.Len:=LanBlkHdSz;{rest=0 in Read} Receiving:=true; Busy:=false;
    SequNr:=1; EndTime:=Timer+AnswerDelay end
  else begin EndTime:=Timer+RequDelay; RequWait:=true end;
  MpxSend(Buf2);
end;

procedure LANStream.Write(P:pointer;L:word);
var rest:word; pofs:word absolute P; Bf:LanBlkHdPtr absolute Buf;
label 1;
begin
1:rest:=UsedLanBlkSz-iBuf; if L>rest then begin
    move(P^,Bf^.A[iBuf],rest); inc(iBuf,rest); Send;
    dec(L,rest); inc(pofs,rest); goto 1 end;
  move(P^,Bf^.A[iBuf],L); inc(iBuf,L);
end;

procedure LANStream.Flush;
var Bf:LanBlkHdPtr absolute Buf; Bf2:LanBlkHdPtr absolute Buf2;
begin
  Bf^.IsLast:=true; Send; while Bf2^.Busy do MpxTest;
end;

{$S-}procedure LANStream.Listen(Blk:pointer;L:word;var Err:word);
var B:LanBlkHdPtr absolute Blk; Bf2:LanBlkHdPtr absolute Buf2;
begin  { interrupts are disabled }
  if B^.Typ=request then
    if RequWait and (B^.SequNr=byte(SequNr+1)) then RequWait:=false
    else Err:=3
  else {landata} if Receiving and not Busy then begin
    if B^.SequNr=SequNr then begin
      move(B^.Typ,Bf2^.Typ,L); Bf2^.Len:=L; inc(SequNr); Busy:=true end
    else if B^.SequNr<>SequNr-1 then Err:=4 end
  else Err:=4;
end;{$S+}

procedure LANStream.ReceiveWait;
var p:pointer; Bf:LanBlkHdPtr absolute Buf; Rq:LanBlkHdPtr absolute Requ;
begin
  if Bf^.IsLast then RunError(2010);
  while not Busy and (Timer<EndTime) do MpxTest;
  if not Busy then RunError(2015);
  iBuf:=0;
  p:=Buf; Buf:=Buf2; Buf2:=p;
  EndTime:=Timer+ReceiveDelay; Busy:=false; if not Bf^.IsLast then begin
    Rq^.SequNr:=Bf^.SequNr+1; MpxSend(Rq) end;
end;

procedure LANStream.Read(P:pointer;L:word);
var rest:word; pofs:word absolute P; Bf:LanBlkHdPtr absolute Buf;
label 1;
begin
1:rest:=Bf^.Len-LanBlkHdSz-iBuf; if L>rest then begin
    move(Bf^.A[iBuf],P^,rest); ReceiveWait;
    dec(L,rest); inc(pofs,rest); goto 1 end;
  move(Bf^.A[iBuf],P^,L); inc(iBuf,L);
end;

function LANStream.AtEnd:boolean;
var Bf:LanBlkHdPtr absolute Buf;
begin
  AtEnd:=Bf^.IsLast and (LanBlkHdSz+iBuf=Bf^.Len);
end;

procedure LANStream.Terminate;
var Bf:LanBlkHdPtr absolute Buf;
begin
  if not AtEnd then RunError(2010);
  SequNr:=0; iBuf:=0; Busy:=false; RequWait:=false; Receiving:=false;
  Bf^.IsLast:=false;
  { the stream is now prepared for eventualy sending the next message }
end;


procedure SQLStream.WrChar(C:char);
begin
  Write(@C,1);
end;

procedure SQLStream.WrString(S:string);
begin
  Write(@S[1],length(S));
end;

procedure SQLStream.WrLString(S:string);
begin
  Write(@S,length(S)+1);
end;

procedure SQLStream.Login(UserName,Password:string);
begin
  WrChar(char(SQLLogin)); WrChar(UserName[0]); WrString(UserName);
  WrChar(Password[0]); WrString(Password); Write(@CharOrdTab,256);
  Switch(true);
  Read(@SQLDateMask[0],1); Read(@SQLDateMask[1],length(SQLDateMask));
  Terminate;
end;

function SQLStream.Switch(Cancel:boolean):boolean;
var MsgNr:word; t:string; n:longint;
begin
  Flush; Switch:=true;
  Read(@MsgNr,2);  {valid only for SQLInstr}
  if MsgNr=0 then Read(@NRecsAffected,4)
  else begin
    if MsgNr=110 then begin
        Read(@n,4); Read(@t[0],1); Read(@t[1],length(t)); SetMsgPar(t) end;
    Terminate; if Cancel then RunError(MsgNr); WrLLF10Msg(MsgNr); Switch:=false end;
end;

function SQLStream.GetTableName:string;
begin
  if CFile^.CatIRec=0 then GetTableName:=CFile^.Name
  else GetTableName:=RdCatField(CFile^.CatIRec,CatPathName);
end;

procedure SQLStream.WrFldDefs;
var f:FieldDPtr;
begin
  f:=CFile^.FldD; while f<>nil do begin
    if f^.Flg and f_Stored<>0 then begin Write(@f^.Typ,5); WrLString(f^.Name) end;
    f:=f^.Chain end;
  WrChar(#0);
end;
procedure SQLStream.WrKeyFldDefs(KF:KeyFldDPtr;CompOp:char);
var f:FieldDPtr;
begin
  while kf<>nil do begin
    f:=kf^.FldD; Write(@f^.Typ,5); Write(@kf^.CompLex,2); WrChar(CompOp);
    WrLString(f^.Name);
    kf:=kf^.Chain end;
  WrChar(#0);
end;
procedure SQLStream.WrCompKey(K:KeyDPtr;CompOp:char;X:XStringPtr);
begin
  if K=nil then begin WrChar(#0); WrChar(#0) end
  else begin WrKeyFldDefs(K^.KFlds,CompOp); WrLString(X^.S) end;
end;

function SQLStream.SelectXRec(K:KeyDPtr;X:XStringPtr;CompOp:char;WithT:boolean):boolean;
var MaxRows:word;
begin
  MaxRows:=1;
  WrChar(char(SQLSelect)); Write(@MaxRows,2); Write(@WithT,1);
  WrLString(GetTableName); WrFldDefs; WrCompKey(K,CompOp,X);
  Switch(true); SelectXRec:=GetRec; Terminate;
end;

procedure SQLStream.DeleteXRec(K:KeyDPtr;X:XStringPtr;Ad:boolean);
var b:boolean;
begin
  WrChar(char(SQLDelete)); WrChar(char(Ad)); WrLString(GetTableName);
  if Ad then WrFldDefs; WrCompKey(K,_equ,X);
  Switch(true); b:=GetRec; Terminate;
  if Ad and b then LastExitCode:=ord(not RunAddUpdte('-',nil,nil));
end;

function SQLStream.UpdateXRec(K:KeyDPtr;X:XStringPtr;Ad:boolean):boolean;
var cr,p:pointer; b:boolean;
begin { nejdrive cist starou, pak update T, pak ostatni = transaction }
  WrChar(char(SQLUpdate)); WrChar(char(Ad)); WrLString(GetTableName);
  WrFldDefs; PutRec; WrCompKey(K,_equ,X);
  b:=Switch(false); UpdateXRec:=b; if not b then exit;
  if b and Ad then begin
    cr:=CRecPtr; p:=GetRecSpace; CRecPtr:=p; GetRec; Terminate; CRecPtr:=cr;
    LastExitCode:=ord(not RunAddUpdte('d',p,nil)); ReleaseStore(p) end
  else Terminate;
end;

procedure SQLStream.UpdateXFld(K:KeyDPtr;X:XStringPtr;F:FieldDPtr);
var s:LongStrPtr;
begin
  WrChar(char(SQLUpdate)); WrChar(#0); WrLString(GetTableName);
  Write(@f^.Typ,5); WrLString(f^.Name); WrChar(#0);
  Write(ptr(PtrRec(CRecPtr).Seg,PtrRec(CRecPtr).Ofs+F^.Displ),F^.NBytes);
  if F^.Typ='T' then begin s:=_longs(F); Write(s,s^.LL+2); ReleaseStore(s) end;
  WrCompKey(K,_equ,X);
  Switch(true); Terminate;
end;

function SQLStream.InsertRec(Ad,Cancel:boolean):boolean;
var b:boolean; k:KeyDPtr; x:XString;
begin
  WrChar(char(SQLInsert)); WrLString(GetTableName);
  WrFldDefs; PutRec;
  k:=CFile^.Keys; if (CFile^.TF<>nil) and (k<>nil) then begin
    x.PackKF(k^.KFlds); WrCompKey(K,_equ,@x) end;
  b:=Switch(Cancel); InsertRec:=b; if not b then exit; Terminate;
  if Ad then LastExitCode:=ord(not RunAddUpdte('+',nil,nil));
end;

procedure SQLStream.ReadTFld(K:KeyDPtr;X:XStringPtr;F:FieldDPtr);
var h,l:word; Bf:LanBlkHdPtr absolute Buf;
label 1;
begin
  h:=OpenH(_isoverwritefile,Exclusive); TestCPathError;
  WrChar(char(SQLRdTFld)); WrLString(GetTableName); WrLString(F^.Name);
  WrCompKey(K,_equ,X);
  Switch(true);
1:l:=Bf^.Len-LanBlkHdSz-iBuf; if l>0 then WriteH(h,l,Bf^.A[iBuf]); inc(iBuf,l);
  if not Bf^.IsLast then begin ReceiveWait; goto 1 end;
  Terminate; CloseH(h);
end;

procedure SQLStream.WriteTFld(K:KeyDPtr;X:XStringPtr;F:FieldDPtr);
var h,l:word; sz:longint; Bf:LanBlkHdPtr absolute Buf;
label 1;
begin
  h:=OpenH(_isoldfile,RdOnly); TestCPathError; sz:=FileSizeH(h);
  WrChar(char(SQLWrTFld)); WrLString(GetTableName); WrLString(F^.Name);
  WrCompKey(K,_equ,X); Write(@sz,4);
1:l:=minl(sz,UsedLanBlkSz-iBuf); ReadH(h,l,Bf^.A[iBuf]); inc(iBuf,l);
  dec(sz,l); if sz>0 then begin Send; goto 1 end;
  Switch(true); Terminate; CloseH(h);
end;

function SQLStream.SendTxt(S:LongStrPtr;Cancel:boolean):longint;
var MsgNr:word; t:string; n:longint;
begin
  WrChar(char(SQLInstr)); Write(@S^.A,S^.LL);
  Flush; Read(@MsgNr,2); n:=MsgNr;
  if MsgNr=0 then Read(@NRecsAffected,4)
  else if MsgNr=110 then begin
    Read(@n,4); Read(@t[0],1); Read(@t[1],length(t)); SetMsgPar(t) end;
  Terminate; if Cancel and (MsgNr<>0) then RunError(MsgNr);
  SendTxt:=n;
end;

procedure SQLStream.DefKeyAcc(K:KeyDPtr);
begin
  WrChar(char(SQLDefKeyAcc)); Write(@K,4);
  WrLSTring(GetTableName); WrFldDefs; WrKeyFldDefs(K^.KFlds,_equ); Switch(true); Terminate;
end;

function SQLStream.KeyAcc(K:KeyDPtr;X:XStringPtr):boolean;
var KF:KeyFldDPtr; F:FieldDPtr; Off:word; n:word; b,descend:boolean; p:pointer;
begin
  WrChar(char(SQLKeyAcc)); Write(@K,4); WrString(X^.S); Switch(true);
  b:=GetRec; KeyAcc:=b; Terminate; if b then exit;
  ZeroAllFlds; SetDeletedFlag;
  Off:=1; KF:=K^.KFlds; while (KF<>nil) and (Off<=length(X^.S)) do begin
    F:=KF^.FldD; descend:=KF^.Descend; n:=F^.NBytes;
    p:=Ptr(PtrRec(CRecPtr).Seg,PtrRec(CRecPtr).Ofs+F^.Displ);
    case F^.Typ of
     'F':X^.GetF(Off,n,descend,p^);
     'A':n:=X^.GetA(Off,n,KF^.CompLex,descend,p^);
     'N':X^.GetN(Off,n,descend,p^);
     'D':X^.GetD(Off,descend,p^);
     'B':X^.GetN(Off,1,descend,p^);
    end;
    inc(Off,n); KF:=KF^.Chain end;
end;

procedure SQLStream.EndKeyAcc(K:KeyDPtr);
begin
  WrChar(char(SQLEndKeyAcc)); Write(@K,4); Switch(true); Terminate;
end;

procedure SQLStream.InpResetTxt(S:LongStrPtr);
var MaxRows:word; WithT:boolean; f:FieldDPtr;
begin
  WrChar(char(SQLQuery)); MaxRows:=0; WithT:=true;
  Write(@MaxRows,2); Write(@WithT,1);
  f:=CFile^.FldD; while f<>nil do begin
    if f^.Flg and f_Stored<>0 then Write(@f^.Typ,5); f:=f^.Chain end;
  WrChar(#0);
  Write(@S^.A,S^.LL); Switch(true);
end;

procedure SQLStream.InpReset(K:KeyDPtr;SK:KeyFldDPtr;KI:KeyInDPtr;Filter:FrmlPtr;WithT:boolean);
var d:KeyInDPtr; kf:KeyFldDPtr; s:LongStrPtr; MaxRows:word;
begin
  MaxRows:=0;
  WrChar(char(SQLInput)); Write(@MaxRows,2); Write(@WithT,1);
  WrLString(GetTableName); WrFldDefs;
  s:=RunLongStr(Filter); Write(s,s^.LL+2);
  if K<>nil then WrKeyFldDefs(K^.KFlds,_equ) else WrChar(#0);
  d:=KI; while d<>nil do begin
    WrLString(d^.X1^); if d^.X1^=d^.X2^ then WrChar(#0) else WrLString(d^.X2^);
    d:=d^.Chain end;
  WrChar(#0); WrKeyFldDefs(SK,_equ); Switch(true);
end;

function SQLStream.GetRec:boolean;
var f:FieldDPtr; n:word; s:LongStrPtr;
begin            {!!!!!!!!!!!!!!!!!!!!!}   { pos ve vete vzdy 0 }
  if AtEnd then begin GetRec:=false; exit end;
  GetRec:=true; ClearRecSpace(CRecPtr);
  if CFile^.Typ='X' then begin
    Read(ptr(PtrRec(CRecPtr).Seg,PtrRec(CRecPtr).Ofs+1),CFile^.RecLen-1);
    ClearDeletedFlag end
  else Read(CRecPtr,CFile^.RecLen);
  if CFile^.TF<>nil then begin
    SetTWorkFlag; f:=CFile^.FldD; while f<>nil do begin
      if (f^.Flg and f_Stored<>0) and (f^.Typ='T') then begin
        Read(@n,2); if n=0 then t_(f,0) else begin
          s:=GetStore(n+2); s^.LL:=n; Read(@s^.A,n);
          t_(f,TWork.Store(s)); ReleaseStore(s) end end;
      f:=f^.Chain end end;
end;

procedure SQLStream.InpClose;
var cr:pointer;
begin
  cr:=CRecPtr; CRecPtr:=GetRecSpace; while not AtEnd do GetRec;
  ReleaseStore(CRecPtr); CRecPtr:=cr; Terminate;
end;

procedure SQLStream.OutpRewrite(Append:boolean);
begin
  WrChar(char(SQLOutput)); WrChar(char(Append));
  WrLString(GetTableName); WrFldDefs;
end;

procedure SQLStream.PutRec;
var f:FieldDPtr; s:LongStrPtr;
begin                                { !!!!!!!!!!!!!!!! }
  if CFile^.Typ='X' then
    Write(ptr(PtrRec(CRecPtr).Seg,PtrRec(CRecPtr).Ofs+1),CFile^.RecLen-1)
  else Write(CRecPtr,CFile^.RecLen);
  if CFile^.TF<>nil then begin
    f:=CFile^.FldD; while f<>nil do begin
      if (f^.Flg and f_Stored<>0) and (f^.Typ='T') then begin
        s:=_longs(f); Write(s,s^.LL+2); ReleaseStore(s) end;
      f:=f^.Chain end end;
end;

procedure SQLStream.OutpClose;
begin
  Switch(true); Terminate;
end;

end.
