
type
  TBoot = object(TObject)
    Drive: byte;
    Boot: array [1..3] of byte;     { jump }
    Version: array[1..8] of char;
    SecSize: word;
    SecsPerClust: byte;
    ReservedSecs: word;
    FatCount: byte;
    RootSize: word;
    TotSecs: word;      { 0 for > 32MB }
                      { TotalSecs > 20740 == 2 byte FAT entry }
    MediaCode: byte;
    SecsPerFat: word;
    SecsPerTrack: word;
    HeadCount: word;
    HiddenSecs2: longint;  {  used if >32MB }
    TotSecs2: longint;     {  used if >32MB }
    Progr: array[1..476] of byte;
    constructor Init(ADrive: byte);
    function BytesPerClust: word;
    function FrstDataSec: word;
    function ReadSect(Sect:word; N:word; var Buf): word;
    function RootSec: word;
    function SecsPerCyl: word;
    function SecsPerRoot: word;
    function TotalSecs: longint;
    function WriteSect(Sect:word; N:word; var Buf): word;
  end;

  TDirEntry = object
    Name: array[1..8] of char;
    Ext: array[1..3] of char;
    Attr: byte;
    Reserved: array[1..10] of byte;
    Time: longint;
    Clust: word;
    Size: longint;
    function IsDeleted: boolean;
    function IsNotUsed: boolean;
  end;
  PDirEntryArr = ^TDirEntryArr;
  TDirEntryArr = array[0..1000] of TDirEntry;


constructor TBoot.Init(aDrive:byte);
begin
  TObject.Init; Drive:=aDrive; TotSecs:=1;
end;
function TBoot.BytesPerClust: word;
begin
  BytesPerClust:= SecsPerClust* SecSize;
end;
function TBoot.FrstDataSec: word;
begin
  FrstDataSec:= RootSec + SecsPerRoot;
end;
function TBoot.ReadSect(Sect:word; N: word; var Buf): word; assembler;
asm  push  ds; lds si,Self; mov al,[si].TBoot.Drive; mov cx,N; mov dx,Sect;
     lds bx,Buf; push bp; int 25H;
     pop bp; pop bp; pop ds; mov al,ah; jc @3; xor al,al;
@3:  xor ah,ah;
end;
function TBoot.RootSec: word;
begin
  RootSec:= ReservedSecs + FatCount * SecsPerFat;
end;
function TBoot.SecsPerCyl: word;
begin
  SecsPerCyl:= SecsPerTrack * HeadCount;
end;
function TBoot.SecsPerRoot: word;
begin
  SecsPerRoot:= (RootSize shl 5) div SecSize;
end;
function TBoot.TotalSecs: longint;
begin
  if TotSecs <> 0 then TotalSecs:= TotSecs else TotalSecs:= TotSecs2;
end;
function TBoot.WriteSect(Sect:word; N: word; var Buf): word; assembler;
asm  push  ds; lds si,Self; mov al,[si].TBoot.Drive; mov cx,N; mov dx,Sect;
     lds bx,Buf; push bp; int 26H;
     pop bp; pop bp; pop ds; mov al,ah; jc @3; xor al,al;
@3:  xor ah,ah;
end;


function TDirEntry.IsDeleted: boolean;
begin
  IsDeleted:= Name[1]=#$e5;
end;
function TDirEntry.IsNotUsed: boolean;
begin
  IsNotUsed:= Name[1]=#0;
end;



procedure FillVolDirEntry(var EE; NewName:string);
var e:TDirEntry absolute EE;  x:DateTime; w,n:word;
begin
  FillChar(e,sizeof(TDirEntry),0); e.Attr:=VolumeID;
  SplitDate(Today,x.Day,x.Month,x.Year); GetTime(x.Hour,x.Min,x.Sec,w);
  PackTime(x,e.Time); n:=minw(length(NewName),11);
  FillChar(e.Name,11,' '); Move(NewName[1],e.Name,n);
end;
procedure ResetDisks; assembler;
asm  mov ah,0; mov dl,0; int 13H; mov ah,0dH; int 21H; {reset block driver}
end;
procedure FatPut(Fat:Pchar;I,N:word); assembler;
asm  les bx,Fat; mov ax,I; add bx,ax; shr ax,1; add bx,ax;
     mov ax,es:[bx]; mov dx,N; test I,1; jz @2; mov cl,4; shl dx,cl;
     and ax,000fH; jmp @3;
@2:  and dx,0fffH; and ax,0f000H;
@3:  add ax,dx; mov es:[bx],ax;
end;
const fatBadCluster = $FFF7;
function FatGet(Fat:Pchar;I:word):word; assembler;
asm  les bx,Fat; mov ax,I; add bx,ax; shr ax,1; add bx,ax;
     mov ax,es:[bx]; test I,1; jz @2; mov cl,4; shr ax,cl; jmp @3;
@2:  and ax,0fffH;
@3:  mov dx,ax; and dx,0ff0H; cmp dx,0ff0H; jne @4; add ax,0f000H;
@4:  end;

function FormatOnDrive(DriveC:char): boolean;

procedure SetDasd(Drive: byte; K: byte); assembler;
asm  mov ah,17H; mov dl,Drive; mov  al,K; int 13H;
{ K: 1= 360kB/360kB; 2= 360kB/1,2MB; 3= 1,2MB/1,2MB;
     4= 720kB/720kB; 5= 720kB/1,44MB; 6= 1,44MB/1,44MB }
end;
function FormatTrack(Drive,Track,Head: byte; DrvPar:word; Buf:pointer): word; assembler;
asm  mov dl,Drive; mov dh,Head; mov ch,Track; mov cl,1; les bx,Buf;
     mov al,BYTE PTR DrvPar{ SecPerTrk }; mov ah,5; int 13H;
     mov al,ah; xor ah,ah;
end;
function VerifySect(Drive,Track,Head,FrstSec,NSec: byte): word; assembler;
asm  mov dl,Drive; mov dh,Head; mov ch,Track; mov cl,FrstSec; mov al,NSec;
     mov ah,4; int 13H; mov al,ah; xor ah,ah;
end;
function WrSect(var Boot:TBoot; Frst,NN:word; var Buf): word;
var drive,track,head,frstSec,nSec,errCode:byte; n:word;
label 1;
begin
  drive:=Boot.Drive; track:=Frst div Boot.SecsPerCyl; n:=Frst mod Boot.SecsPerCyl;
  head:=n div Boot.SecsPerTrack; frstSec:=n mod Boot.SecsPerTrack +1;
1:nSec:=Boot.SecsPerTrack-frstSec+1; if nSec>NN then nSec:=NN; dec(NN,nSec);
  asm  mov dl,drive; mov dh,head; mov ch,track; mov cl,frstSec; mov al,nSec;
       les bx,Buf; mov ah,3; int 13H; jb  @1; xor ah, ah;
  @1:  mov errCode,ah
  end;
  if (NN<>0) and (errCode=0) then begin
    inc(head); if head=Boot.HeadCount then begin head:=0; inc(track) end;
    frstSec:=1; goto 1 end;
  WrSect:=errCode;
end;
function GetDrvPar(Drive:byte):word; assembler;
asm  mov ah,8; mov dl,Drive; int $13; mov ax,cx {Lo=SecsPerTrack,Hi=MaxTrack}
end;
function GetDPT(Drive,DrvPar:word):pointer; assembler;
asm  mov ah,18H; mov dx,Drive; mov cx,DrvPar; mov di,0ffffH; stc;
     int 13H; push es; pop dx; mov ax,di;
end;
const
  BootT1: array[1..13] of char =  #$eb#$22#$90'GT  ALIS'#$00#$02;
  BootT2: array[0..3] of string[13] =   {360kB, 1,2MB, 720kB, 1,44MB}
      (#$02#$01#$00#$02#$70#$00#$d0#$02#$fd#$02#$00#$09#$00,
       #$01#$01#$00#$02#$e0#$00#$60#$09#$f9#$07#$00#$0f#$00,
       #$02#$01#$00#$02#$70#$00#$a0#$05#$f9#$03#$00#$09#$00,
       #$01#$01#$00#$02#$e0#$00#$40#$0b#$f0#$09#$00#$12#$00);
  BootT3: array[1..128] of char =
       #$02#0#0#0#0#0#0#0#0#0+
       #$fa#$fc#$33#$c0#$8e#$d0#$bc#$00#$7c#$8e#$d8#$fb#$e8#$52#$00 +
                                                    { $52 = length of text }
       ^m^j'Na diskete neni operacni system.'^m^j +
       'Vymente ji a stisknete libovolnou klavesu !'^m^j#0 +
       #$5e#$ac#$0a#$c0#$74#$09#$bb#$07#$00#$b4#$0e#$cd#$10#$eb#$f2 +
       #$32#$e4#$cd#$16#$cd#$19;
   drvNm:string[1]='A';
var
  MarkerTab:array[1..18] of record track,head,sect,size:byte end;
  secPerTrk,maxTrk,secBeforeTrk,sec,clust,nBad,i,k,drvPar:word;
  s:string[9]; format:word;
  drive,trk,hd:byte; bt:TBoot; fat,buf:Pchar; oldDpt,dpt,p:pointer;
label 2,21,3,4;
begin
  FormatOnDrive:=false; MarkStore(p); GetIntVec($1e,oldDpt);
  drive:=ord(upcase(DriveC))-ord('A'); drvNm[1]:=DriveC; SetMsgPar(drvNm);
  bt.Init(drive);
  drvPar:=GetDrvPar(drive); secPerTrk:=lo(drvPar); maxTrk:=hi(drvPar);
  case drvPar of
    $2709:begin format:=0; k:=1 end;
    $4f0f:begin format:=1; k:=3 end;
    $4f09:begin format:=2; k:=4 end;
    $4f12:begin format:=3; k:=6 end;
    else WrLLF10Msg(658); exit end;
  Move(BootT1,bt.Boot,sizeof(BootT1));
  Move(BootT2[format][1],bt.SecsPerClust,13);
  Move(BootT3,bt.HeadCount,sizeof(BootT3));

  ResetDisks; dpt:=GetDPT(drive,drvPar);
  if ofs(dpt^)=$ffff then begin
     SetDasd(drive,k); dpt:=GetDPT(drive,drvPar) end;
  SetIntVec($1e,dpt); nBad:=0;

  fat:=GetZStore(bt.SecSize*bt.SecsPerFat); buf:=GetStore(bt.SecSize);
  FatPut(fat,0,$ff00+bt.MediaCode); FatPut(fat,1,$ffff);

  RunMsgOn('I',2*(maxTrk+1));
  for trk:=0 to maxTrk do for hd:=0 to 1 do begin
    RunMsgN(2*trk+hd+1);
    for i:=1 to secPerTrk do with MarkerTab[i] do begin
      track:=trk; head:=hd; sect:=i; size:=2 end;
2:  case FormatTrack(drive,trk,hd,drvPar,@MarkerTab) of
      $06{         }: goto 2;
      $80{not ready}: begin F10SpecKey:=_ESC_; Set2MsgPar(drvNm,'');
          WrLLF10Msg(808); if (KbdChar=_ESC_) then goto 21;
          ResetDisks; FormatTrack(drive,trk,hd,drvPar,@MarkerTab); goto 2 end;
      $03{protected}: begin WrLLF10Msg(850);
21:                         RunMsgOff; goto 4 end;
      $00{no error }:
        if VerifySect(drive,trk,hd,1,secPerTrk)<>0 then begin
          ResetDisks;
          if FormatTrack(drive,trk,hd,drvPar,@MarkerTab)<>0 then
                   goto 3;
          secBeforeTrk:=(2*trk+hd)*secPerTrk;
          for i:=1 to secPerTrk do
            if VerifySect(drive,trk,hd,i,1)<>0 then begin
              sec:=secBeforeTrk+i-1; if sec<bt.FrstDataSec then goto 3;
              clust:=(sec-bt.FrstDataSec) div bt.SecsPerClust +2;
              if FatGet(fat,clust)<>fatBadCluster then begin
                FatPut(fat,clust,fatBadCluster); inc(nBad)  end end;
          end;
      else goto 3;
    end;
  end;
  Move(bt.Boot,buf^,512);
  if WrSect(bt,0,1,buf^)<>0 then goto 3;
  with bt do for i:=0 to FatCount-1 do
    if WrSect(bt,ReservedSecs+i*SecsPerFat,SecsPerFat,fat^)<>0 then goto 3;
  FillChar(buf^,512,0); FillVolDirEntry(Buf^,CVol);
  with bt do for i:=1 to SecsPerRoot do begin
    if WrSect(bt,RootSec-1+i,1,buf^)<>0 then goto 3; FillChar(buf^,512,0) end;
  if nBad<>0 then begin
    str(nBad*Bt.SecsPerClust,s); SetMsgPar(s); WrLLF10Msg(657) end;
  FormatOnDrive:= true; RunMsgOff;
  goto 4;
3:RunMsgOff; WrLLF10Msg(656);
4:SetIntVec($1e,oldDpt); ResetDisks; ReleaseStore(p);
end;



const
  RingBufSz=4096; MaxMatchLen=18; MinMatchLen=3; Leer=RingBufSz;
type
  TXBuf=record
    RingBuf:array[0..RingBufSz+MaxMatchLen-2] of byte;
    LSon,Dad:array[0..RingBufSz] of word; {binary search trees; son/dad[iNode]}
    RSon:array[0..RingBufSz+256] of word;{last 256:root for strings beginning with c}
  end;
type
  TcFile = object(TObject)
    Buf,Buf2:Pchar;
    iBuf,lBuf,iBuf2,lBuf2,BufSize,BufSize2:word;
    EOF,EOF2:boolean;
    Compress:byte;
    CodeMask:byte; CodeMaskW,lCode,lInput,nToRead,iRingBuf,jRingBuf:word;
    MatchPos,MatchLen:word;      {set by InsertNode}
    CodeBuf:array[0..16] of byte;{[0]=8 flags:"1"=unencoded byte,"0"=position/length word}
    XBuf:^TXBuf;
    constructor Init(aCompress:byte);
    function MyDiskFree(Floppy:boolean;Drive:byte):longint;
    procedure InsertNode(r:integer);
    procedure DeleteNode(p:integer);
    procedure WriteCodeBuf;
    procedure InitBufOutp;
    procedure WriteBuf(isLast:boolean);
    procedure WriteBuf2; virtual;
    procedure InitBufInp;
    procedure ReadBuf;
    procedure ReadBuf2; virtual;
  end;

constructor TcFile.Init(aCompress:byte);
begin
  inherited Init; Compress:=aCompress;
  if aCompress=0 then begin
    BufSize:=4*RingBufSz; BufSize2:=BufSize; Buf:=GetStore(BufSize); Buf2:=Buf end
  else begin
    BufSize:=RingBufSz; BufSize2:=4*BufSize; XBuf:=GetStore(sizeof(TXBuf));
    Buf:=GetStore(BufSize); Buf2:=GetStore(BufSize2) end;
end;

function TcFile.MyDiskFree(Floppy:boolean;Drive:byte):longint;
begin
  if spec.WithDiskFree or Floppy then MyDiskFree:=DiskFree(Drive)
  else MyDiskFree:=$7fffffff;
end;

procedure TcFile.InsertNode(r:integer); assembler;
var KeyOfs,MLen,MPos:word; res:integer;
{  res:=1; key:=Pchar(@RingBuf[r]); p:=RingBufSz+1+ord(key[0]);
  RSon[r]:=Leer; LSon[r]:=Leer; MatchLen:=0;
1:if res>=0 then
    if RSon[p]<>Leer then p:=RSon[p]
    else begin RSon[p]:=r; Dad[r]:=p; exit end
  else
    if LSon[p]<>Leer then p:=LSon[p]
    else begin LSon[p]:=r; Dad[r]:=p; exit end;
  for i:=1 to MaxMatchLen-1 do begin
    res:=ord(key[i])-RingBuf[p+i]; if res<>0 then goto 2 end;
2:if i<=MatchLen then goto 1;
  MatchPos:=p; MatchLen:=i;
  if i<MaxMatchLen then goto 1;
  Dad[r]:=Dad[p]; LSon[r]:=LSon[p]; RSon[r]:=RSon[p];
  Dad[LSon[p]]:=r; Dad[RSon[p]]:=r;
  if RSon[Dad[p]]=p then RSon[Dad[p]]:=r else LSon[Dad[p]]:=r;
  Dad[p]:=Leer; }
asm push ds; lds si,Self; lds si,[si].TcFile.XBuf; push ds; pop es;
    mov MLen,0; mov res,1;
    mov bx,r; lea ax,[si+bx].TXBuf.RingBuf; mov KeyOfs,ax;
    xor ah,ah; mov al,[si+bx].TXBuf.RingBuf.byte;
    shl bx,1;
    mov [si+bx].TXBuf.RSon.word,Leer; mov [si+bx].TXBuf.LSon.word,Leer;
    mov bx,RingBufSz+1; add bx,ax;
@1: shl bx,1; cmp res,0; jl @3;
    cmp [si+bx].TXBuf.RSon.word,Leer; je @2;
    mov bx,[si+bx].TXBuf.RSon.word; jmp @6;
@2: mov ax,r; mov [si+bx].TXBuf.RSon.word,ax; jmp @4;
@3: cmp [si+bx].TXBuf.LSon.word,Leer; jne @5;
    mov ax,r; mov [si+bx].TXBuf.LSon.word,ax;
@4: shl ax,1; add si,ax; shr bx,1; mov [si].TXBuf.Dad.word,bx; jmp @9;
@5: mov bx,[si+bx].TXBuf.LSon.word;
@6: push si; mov cx,MaxMatchLen;
    lea di,[si+bx].TXBuf.RingBuf; mov dx,di; mov si,KeyOfs;
    dec cx; inc si; inc di; cld; repe cmpsb;
    mov ax,0; je @7; mov ax,-1; jl @71; mov ax,1;
@71:dec di;
@7: mov Res,ax; sub di,dx; {i}
    pop si;
    cmp di,MLen; jle @1;
    mov MPos,bx; mov MLen,di;
    cmp di,MaxMatchLen; jl @1;
    mov dx,bx; shl bx,1; mov cx,bx; {p}  mov di,r; shl di,1; add di,si; {r}
    mov ax,[si+bx].TXBuf.Dad.word; mov [di].TXBuf.Dad.word,ax;
    mov ax,[si+bx].TXBuf.LSon.word; mov [di].TXBuf.LSon.word,ax;
    mov ax,[si+bx].TXBuf.RSon.word; mov [di].TXBuf.RSon.word,ax; mov ax,r;
    mov bx,[si+bx].TXBuf.LSon.word; shl bx,1; mov [si+bx].TXBuf.Dad.word,ax; mov bx,cx;
    mov bx,[si+bx].TXBuf.RSon.word; shl bx,1; mov [si+bx].TXBuf.Dad.word,ax; mov bx,cx;
    mov bx,[si+bx].TXBuf.Dad.word; shl bx,1;
    cmp [si+bx].TXBuf.RSon.word,dx; jne @8; mov [si+bx].TXBuf.RSon.word,ax; jmp @81;
@8: mov [si+bx].TXBuf.LSon.word,ax;
@81:mov bx,cx; mov [si+bx].TXBuf.Dad.word,Leer;
@9: lds si,Self; mov ax,MLen; mov [si].TcFile.MatchLen,ax;
    mov ax,MPos; mov [si].TcFile.MatchPos,ax; pop ds;
end;

procedure TcFile.DeleteNode(p:integer); assembler;
{var q:integer;
begin
 if Dad[p]=Leer then exit;
  if RSon[p]=Leer then q:=LSon[p] else if LSon[p]=Leer then q:=RSon[p]
  else begin
    q:=LSon[p];	if RSon[q]<>Leer then begin
      repeat q:=RSon[q] until RSon[q]=Leer;
      RSon[Dad[q]]:=LSon[q]; Dad[LSon[q]]:=Dad[q]; LSon[q]:=LSon[p];
      Dad[LSon[p]]:=q end;
    RSon[q]:=RSon[p]; Dad[RSon[p]]:=q end;
  Dad[q]:=Dad[p];
  if (RSon[Dad[p]]=p) then RSon[Dad[p]]:=q else LSon[Dad[p]]:=q;
  Dad[p]:=Leer; }
asm push ds; lds si,Self; lds si,[si].TcFile.XBuf;
    mov bx,p; shl bx,1; cmp [si+bx].TXBuf.Dad.word,Leer; je @7;
    cmp [si+bx].TXBuf.RSon.word,Leer; jne @1;
    mov dx,[si+bx].TXBuf.LSon.word; jmp @5;
@1: cmp [si+bx].TXBuf.LSon.word,Leer; jne @2;
    mov dx,[si+bx].TXBuf.RSon.word; jmp @5;
@2: mov dx,[si+bx].TXBuf.LSon.word;{LSon[p]} mov cx,dx;
    mov bx,dx; shl bx,1; cmp [si+bx].TXBuf.RSon.word,Leer; je @4;
@3: mov dx,[si+bx].TXBuf.RSon.word;
    mov bx,dx; shl bx,1; cmp [si+bx].TXBuf.RSon.word,Leer; jne @3;
    mov ax,[si+bx].TXBuf.LSon.word;{LSon[q]} mov [si+bx].TXBuf.LSon.word,cx;
    mov di,[si+bx].TXBuf.Dad.word; {Dad[q]}
    mov bx,di; shl bx,1; mov [si+bx].TXBuf.RSon.word,ax; {RSon[Dad[q]]}
    mov bx,ax; shl bx,1; mov [si+bx].TXBuf.Dad.word,di; {Dad[LSon[q]]}
    mov bx,cx; shl bx,1; mov [si+bx].TXBuf.Dad.word,dx; {Dad[LSon[p]]}
@4: mov bx,p; shl bx,1; mov ax,[si+bx].TXBuf.RSon.word; {RSon[p]}
    mov bx,dx; shl bx,1; mov [si+bx].TXBuf.RSon.word,ax; {RSon[q]}
    mov bx,ax; shl bx,1; mov [si+bx].TXBuf.Dad.word,dx; {Dad[RSon[p]]}
    mov bx,p; shl bx,1;
@5: mov ax,[si+bx].TXBuf.Dad.word; {Dad[p]} mov [si+bx].TXBuf.Dad.word,Leer;
    mov bx,dx; shl bx,1; mov [si+bx].TXBuf.Dad.word,ax; {Dad[q]}
    mov bx,ax; shl bx,1; mov cx,p;
    cmp [si+bx].TXBuf.RSon.word,cx; jne @6;{RSon[Dad[p]] = p? }
    mov [si+bx].TXBuf.RSon.word,dx; jmp @7;
@6: mov [si+bx].TXBuf.LSon.word,dx;
@7: pop ds;
end;

procedure TcFile.WriteCodeBuf;
var i:word;
begin
  for i:=0 to lCode-1 do begin
    if lBuf2>=BufSize2 then WriteBuf2; Buf2[lBuf2]:=char(CodeBuf[i]); inc(lBuf2) end;
  CodeBuf[0]:=0; lCode:=1; CodeMask:=1;
end;

procedure TcFile.InitBufOutp;
begin
  if Compress<>0 then begin
  asm les bx,Self; les bx,es:[bx].TcFile.XBuf; lea di,es:[bx].TXBuf.LSon;
      mov cx,3*(RingBufSz+1)+256; cld; mov ax,Leer; rep stosw;
      lea di,es:[bx].TXBuf.RingBuf; mov cx,RingBufSz; mov ax,0; rep stosb end;
    CodeBuf[0]:=0; lCode:=1; CodeMask:=1;
    jRingBuf:=0; iRingBuf:=RingBufSz-MaxMatchLen; lInput:=0; nToRead:=0 end;
  lBuf:=0; lBuf2:=0;
end;
procedure TcFile.WriteBuf(isLast:boolean);
var i,j:integer; c:byte;
label 1,2;
begin
  if Compress=0 then begin lBuf2:=lBuf; WriteBuf2; lBuf:=0; exit end;
  i:=0; if lInput=0 then begin {initialization phase }
    while (lInput<MaxMatchLen) and (i<lBuf) do begin
      XBuf^.RingBuf[RingBufSz-MaxMatchLen+lInput]:=ord(Buf[i]);
      inc(i); inc(lInput) end;
    for j:=1 to MaxMatchLen do InsertNode(iRingBuf-j); InsertNode(iRingBuf) end;
1:while lInput<>0 do begin
    while nToRead>0 do begin
      if i>=lBuf then if not isLast then goto 2 else begin
        while nToRead>0 do begin DeleteNode(jRingBuf);
          jRingBuf:=(jRingBuf+1)and(RingBufSz-1);
          iRingBuf:=(iRingBuf+1)and(RingBufSz-1);
          dec(lInput); dec(nToRead);
          if (lInput<>0) then InsertNode(iRingBuf) end;
        goto 1 end;
      c:=ord(Buf[i]); inc(i); dec(nToRead);
      DeleteNode(jRingBuf); XBuf^.RingBuf[jRingBuf]:=c;
      if jRingBuf<MaxMatchLen-1 then XBuf^.RingBuf[jRingBuf+RingBufSz]:=c;
      jRingBuf:=(jRingBuf+1) and (RingBufSz-1);
      iRingBuf:=(iRingBuf+1) and (RingBufSz-1);
      InsertNode(iRingBuf) end;
    if MatchLen>lInput then MatchLen:=lInput;
    if MatchLen<MinMatchLen then begin
      nToRead:=1; CodeBuf[0]:=CodeBuf[0] or CodeMask;
      CodeBuf[lCode]:=XBuf^.RingBuf[iRingBuf]; inc(lCode) end
    else begin
      CodeBuf[lCode]:=MatchPos;
      CodeBuf[lCode+1]:=((MatchPos shr 4) and $f0) or (MatchLen-MinMatchLen);
      inc(lCode,2); nToRead:=MatchLen end;
    CodeMask:=CodeMask shl 1; if CodeMask=0 then WriteCodeBuf end;
  WriteCodeBuf;
2:if IsLast then WriteBuf2; lBuf:=0;
end;

procedure TcFile.WriteBuf2;
begin
  { schreiben + lBuf2:=0 }
end;

procedure TcFile.InitBufInp;
var i:word;
begin
  if Compress<>0 then begin
    fillchar(XBuf^.RingBuf,RingBufSz-MaxMatchLen,0);
    iRingBuf:=RingBufSz-MaxMatchLen; CodeMaskW:=0 end;
  iBuf2:=0; lBuf2:=0; EOF:=false; EOF2:=false; ReadBuf;
end;

procedure TcFile.ReadBuf;
var i,j,k,r:integer; c:byte; wLo,wHi:word;
label 1,2;
begin
  lBuf:=0; iBuf:=0; if EOF then exit;
  if Compress=0 then begin
    ReadBuf2; if EOF2 then begin EOF:=true; exit end;
    lBuf:=lBuf2; inc(iBuf2,lBuf); exit end;
1:if lBuf>BufSize-MaxMatchLen then exit;
  CodeMaskW:=CodeMaskW shr 1; if (CodeMaskW and 256)=0 then begin
    if iBuf2>=lBuf2 then begin ReadBuf2; if EOF2 then goto 2 end;
    CodeMaskW:=ord(Buf2[iBuf2]) or $ff00; inc(iBuf2) end;  {hi:count eight}
  if (CodeMaskW and 1)<>0 then begin
    if iBuf2>=lBuf2 then begin ReadBuf2; if EOF2 then goto 2 end;
    c:=ord(Buf2[iBuf2]); inc(iBuf2); Buf[lBuf]:=char(c); inc(lBuf);
    XBuf^.RingBuf[iRingBuf]:=c; iRingBuf:=(iRingBuf+1)and(RingBufSz-1) end
  else begin
    if iBuf2>=lBuf2 then begin ReadBuf2; if EOF2 then goto 2 end;
    wLo:=ord(Buf2[iBuf2]); inc(iBuf2);
    if iBuf2>=lBuf2 then begin ReadBuf2; if EOF2 then goto 2 end;
    wHi:=ord(Buf2[iBuf2]); inc(iBuf2);
    MatchPos:=wLo or ((wHi and $f0)shl 4); MatchLen:=(wHi and $0f)+MinMatchLen;
    for i:=0 to MatchLen-1 do begin
      c:=XBuf^.RingBuf[(MatchPos+i) and (RingBufSz-1)];  Buf[lBuf]:=char(c); inc(lBuf);
      XBuf^.RingBuf[iRingBuf]:=c; iRingBuf:=(iRingBuf+1) and (RingBufSz-1) end end;
  goto 1;
2:if lBuf=0 then EOF:=true;
end;

procedure TcFile.ReadBuf2;
begin
  { lBuf2; iBuf2; EOF2 }
end;