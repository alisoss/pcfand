{ DISK.PAS for FPC: disk access stubbed, compression ported }
type
  TBoot = object(TObject)
    Drive: byte;
    Boot: array [1..3] of byte;
    Version: array[1..8] of char;
    SecSize: word;
    SecsPerClust: byte;
    ReservedSecs: word;
    FatCount: byte;
    RootSize: word;
    TotSecs: word;
    MediaCode: byte;
    SecsPerFat: word;
    SecsPerTrack: word;
    HeadCount: word;
    HiddenSecs2: longint;
    TotSecs2: longint;
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
constructor TBoot.Init(aDrive:byte); begin inherited Init; Drive:=aDrive; TotSecs:=1 end;
function TBoot.BytesPerClust: word;
begin BytesPerClust:= SecsPerClust* SecSize end;
function TBoot.FrstDataSec: word;
begin FrstDataSec:= RootSec + SecsPerRoot end;
function TBoot.ReadSect(Sect:word; N: word; var Buf): word;
begin ReadSect:=1;  end;
function TBoot.RootSec: word;
begin RootSec:= ReservedSecs + FatCount * SecsPerFat end;
function TBoot.SecsPerCyl: word;
begin SecsPerCyl:= SecsPerTrack * HeadCount end;
function TBoot.SecsPerRoot: word;
begin SecsPerRoot:= (RootSize shl 5) div SecSize end;
function TBoot.TotalSecs: longint;
begin if TotSecs <> 0 then TotalSecs:= TotSecs else TotalSecs:= TotSecs2 end;
function TBoot.WriteSect(Sect:word; N: word; var Buf): word;
begin WriteSect:=1;  end;
function TDirEntry.IsDeleted: boolean;
begin IsDeleted:= Name[1]=#$e5 end;
function TDirEntry.IsNotUsed: boolean;
begin IsNotUsed:= Name[1]=#0 end;
procedure FillVolDirEntry(var EE; NewName:string);
var e:TDirEntry absolute EE;  x:DateTime; w,n:word;
begin
  FillChar(e,sizeof(TDirEntry),0); e.Attr:=VolumeID;
  SplitDate(Today,x.Day,x.Month,x.Year); GetTime(x.Hour,x.Min,x.Sec,w);
  PackTime(x,e.Time); n:=minw(length(NewName),11);
  FillChar(e.Name,11,' '); Move(NewName[1],e.Name,n);
end;
procedure ResetDisks;
begin  end;
procedure FatPut(Fat:Pchar;I,N:word);
var ofs:word; v:word;
begin
  ofs:=I + I shr 1;
  v:=ord(Fat[ofs]) or (ord(Fat[ofs+1]) shl 8);
  if odd(I) then begin
    v:=(v and $000F) or (N shl 4);
  end else begin
    v:=(v and $F000) or (N and $0FFF);
  end;
  Fat[ofs]:=chr(v and $FF);
  Fat[ofs+1]:=chr((v shr 8) and $FF);
end;
const fatBadCluster = $FFF7;
function FatGet(Fat:Pchar;I:word):word;
var ofs:word; v:word;
begin
  ofs:=I + I shr 1;
  v:=ord(Fat[ofs]) or (ord(Fat[ofs+1]) shl 8);
  if odd(I) then v:=v shr 4 else v:=v and $0FFF;
  if (v and $0FF0)=$0FF0 then v:=v or $F000;
  FatGet:=v;
end;
function FormatOnDrive(DriveC:char): boolean; begin FormatOnDrive:=false end;
const
  RingBufSz=4096; MaxMatchLen=18; MinMatchLen=3; Leer=RingBufSz;
type
  TXBuf=record
    RingBuf:array[0..RingBufSz+MaxMatchLen-2] of byte;
    LSon,Dad:array[0..RingBufSz] of word;
    RSon:array[0..RingBufSz+256] of word;
  end;
type
  TcFile = object(TObject)
    Buf,Buf2:Pchar;
    iBuf,lBuf,iBuf2,lBuf2,BufSize,BufSize2:word;
    EOF,EOF2:boolean;
    Compress:byte;
    CodeMask:byte; CodeMaskW,lCode,lInput,nToRead,iRingBuf,jRingBuf:word;
    MatchPos,MatchLen:word;
    CodeBuf:array[0..16] of byte;
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
var r:int64;
begin
  { -1 (no such drive: a Unix path's "drive" is its first character) would
    let WriteBuf2 write past its buffer: no limit known, then }
  r:=$7fffffff;
  if spec.WithDiskFree or Floppy then r:=DiskFree(Drive);
  if (r<0) or (r>$7fffffff) then r:=$7fffffff;
  MyDiskFree:=r;
end;
procedure TcFile.InsertNode(r:integer);
var key:integer; p,i:integer; res:integer;
begin
  res:=1;
  key:=r;
  p:=RingBufSz+1+XBuf^.RingBuf[r];
  XBuf^.RSon[r]:=Leer; XBuf^.LSon[r]:=Leer; MatchLen:=0;
  while true do begin
    if res>=0 then begin
      if XBuf^.RSon[p]<>Leer then p:=XBuf^.RSon[p]
      else begin XBuf^.RSon[p]:=r; XBuf^.Dad[r]:=p; exit end
    end else begin
      if XBuf^.LSon[p]<>Leer then p:=XBuf^.LSon[p]
      else begin XBuf^.LSon[p]:=r; XBuf^.Dad[r]:=p; exit end
    end;
    i:=1;
    while i<=MaxMatchLen-1 do begin
      res:=integer(XBuf^.RingBuf[key+i])-integer(XBuf^.RingBuf[p+i]);
      if res<>0 then break;
      inc(i);
    end;
    if i<=MatchLen then continue;
    MatchPos:=p; MatchLen:=i;
    if i>=MaxMatchLen then begin
      XBuf^.Dad[r]:=XBuf^.Dad[p]; XBuf^.LSon[r]:=XBuf^.LSon[p]; XBuf^.RSon[r]:=XBuf^.RSon[p];
      XBuf^.Dad[XBuf^.LSon[p]]:=r; XBuf^.Dad[XBuf^.RSon[p]]:=r;
      if XBuf^.RSon[XBuf^.Dad[p]]=p then XBuf^.RSon[XBuf^.Dad[p]]:=r
      else XBuf^.LSon[XBuf^.Dad[p]]:=r;
      XBuf^.Dad[p]:=Leer;
      exit;
    end;
  end;
end;
procedure TcFile.DeleteNode(p:integer);
var q:integer;
begin
  if XBuf^.Dad[p]=Leer then exit;
  if XBuf^.RSon[p]=Leer then q:=XBuf^.LSon[p]
  else if XBuf^.LSon[p]=Leer then q:=XBuf^.RSon[p]
  else begin
    q:=XBuf^.LSon[p];
    if XBuf^.RSon[q]<>Leer then begin
      repeat q:=XBuf^.RSon[q] until XBuf^.RSon[q]=Leer;
      XBuf^.RSon[XBuf^.Dad[q]]:=XBuf^.LSon[q];
      XBuf^.Dad[XBuf^.LSon[q]]:=XBuf^.Dad[q];
      XBuf^.LSon[q]:=XBuf^.LSon[p];
      XBuf^.Dad[XBuf^.LSon[p]]:=q;
    end;
    XBuf^.RSon[q]:=XBuf^.RSon[p]; XBuf^.Dad[XBuf^.RSon[p]]:=q;
  end;
  XBuf^.Dad[q]:=XBuf^.Dad[p];
  if XBuf^.RSon[XBuf^.Dad[p]]=p then XBuf^.RSon[XBuf^.Dad[p]]:=q
  else XBuf^.LSon[XBuf^.Dad[p]]:=q;
  XBuf^.Dad[p]:=Leer;
end;
procedure TcFile.WriteCodeBuf;
var i:word;
begin
  for i:=0 to lCode-1 do begin
    if lBuf2>=BufSize2 then WriteBuf2; Buf2[lBuf2]:=char(CodeBuf[i]); inc(lBuf2) end;
  CodeBuf[0]:=0; lCode:=1; CodeMask:=1;
end;
procedure TcFile.InitBufOutp;
var i:word;
begin
  if Compress<>0 then begin
    for i:=0 to RingBufSz do begin
      XBuf^.LSon[i]:=Leer; XBuf^.Dad[i]:=Leer; XBuf^.RSon[i]:=Leer;
    end;
    for i:=RingBufSz+1 to RingBufSz+256 do XBuf^.RSon[i]:=Leer;
    FillChar(XBuf^.RingBuf,RingBufSz,0);
    CodeBuf[0]:=0; lCode:=1; CodeMask:=1;
    jRingBuf:=0; iRingBuf:=RingBufSz-MaxMatchLen; lInput:=0; nToRead:=0 end;
  lBuf:=0; lBuf2:=0;
end;
procedure TcFile.WriteBuf(isLast:boolean);
var i,j:integer; c:byte;
label 1,2;
begin
  if Compress=0 then begin lBuf2:=lBuf; WriteBuf2; lBuf:=0; exit end;
  i:=0; if lInput=0 then begin
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
    CodeMaskW:=ord(Buf2[iBuf2]) or $ff00; inc(iBuf2) end;
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
end;
