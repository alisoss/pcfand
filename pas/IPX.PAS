const
     BroadcastWaitTime = 18;
     MaxConnectRetry = 4;
     BLOCK_SIZE = 1514;         {IPX = 576, FTP ODIPKT = 1514}
     BlkHeadSz = 42;            {IPX = 30, UDP = 42}

type
    PROTOpkt = record                   {packet header - enough for ipx and ip}
      eth: array[1..14] of byte;
      ip:  array[1..20] of byte;
      udp: array[1..8]  of byte;
      case byte of
       0: (Data: array[1..BLOCK_SIZE - BlkHeadSz] of byte);
       1: (ConnectErr: word);
    end;

    PROTOcb = record                    {protocol control block}
      Link: pointer;
      ESRAddr: pointer;
      InUse,
      ComplCode: byte;
      Sckt: word;
      Length: word;
      Filler: array[1..20] of byte;
      FragCnt:word;                { must be 1, e.g. single fragment only}
      FragAddr: pointer;
      FragSize: word;
    end;

    TUserPostProc = procedure(FormSend: boolean; Blk:pointer; Len, Err: word);

var
  SndIpxBlk,RcvIpxBlk:PROTOpkt;
  SndEcb,RcvEcb:PROTOcb;
  MySckt:word;
  UsrPostProc:TUserPostProc;
  flag: word;


procedure intrpt; assembler;
asm     int 0;
end;

{procedure Ladeni(i: integer); assembler;
  asm mov ax, i; mov bx, 5; call intrpt;
  end;}

function IPXExists:boolean;
var i, k: integer;
    j :longint; p: pointer absolute j;
const ident: string[8] = 'IPX EMUL';
begin
     for i := $60 to $80 do
     begin
          p := pointer(pointer(4 * i)^);
          j := j + 2;
          for k := 1 to 8 do
          begin
               inc(j);
               if char(p^) <> ident[k] then break;
          end;
          if char(p^) = ident[k] then break;
     end;
     p := @intrpt;
     inc(j);
     move(i, p^, 1);
     if i = $80 then IPXExists := false else IPXExists := true;
end;

{function IPXExists:boolean; assembler;
asm mov ax,7a00H; int 2fH; cmp al,0ffH; jne @1; mov ax,1; jmp @2;
@1: xor ax,ax;
@2: end;}

function OpenSocket:boolean; assembler;
asm  mov bx,0;
     mov al,0; { short lived socket, sufficient for send+receive n blocks }
     mov dx,0; { desired socket, 0 - get any }
     call intrpt;  { al= Status, ff = already open, 0=O.K., else false }
     mov MySckt,dx;
     cmp al,0;
     mov ax,0;
     jne @1;
     mov ax,1;
@1:
end;

procedure CloseSocket; assembler;
asm  mov bx,1; mov dx,MySckt; call intrpt;
end;

{function GetLocalTarget:word; assembler;
asm mov bx, 2;
    mov ax, ds;
    mov es, ax;
    mov si, OFFSET SndIpxBlk.DestLAN;
    mov di, OFFSET SndECB.ImmediateAddr;
    push bp; int 7aH; pop bp;
    mov ax, cx;
end;}

procedure Listen; assembler;   {  BlkSize erneuern ???? !!!!!!  }
asm  mov bx,4; mov ax,ds; mov es,ax; mov si,OFFSET RcvEcb; call intrpt;
end;

procedure SendBlock(Buf:pointer;Len:word);
begin
  SndEcb.FragSize:=Len+BlkHeadSz; Move(Buf^,SndIpxBlk.Data,Len);
  asm mov bx,3; mov ax,ds; mov es,ax; mov si,OFFSET SndEcb; call intrpt;
  end;
end;

{$S-}procedure ListenPostProc; far;
var err:word;
begin { interrupts disabled; es:si points to Ecb }
  asm mov ax,SEG @Data; mov ds,ax end;
  err:=0; if RcvEcb.ComplCode<>0 then err:=1; {transmission error}
  UsrPostProc(false,@RcvIpxBlk.Data,RcvEcb.Length,err);
  Listen;   { at end of Listen interrupts enabled }
end;

procedure SendPostProc; far;
var err:word;
begin { interrupts disabled; es:di points to Ecb }
  asm mov ax,SEG @Data; mov ds,ax end;
  err:=0;
  if SndEcb.ComplCode<>0 then begin
    {correctable error -> SendBlock; exit             !!!!!!!     }
    err:=2005 end;
  UsrPostProc(true,@SndIpxBlk.Data,SndEcb.Length,err);
end;{$S+}

function Connect(aUsrPostProc:TUserPostProc{;SrvLan:longint}):word;
var retry:word; EndTime:longint; Timer:longint absolute 0:$46C;
label 1,2,3;
begin
  UsrPostProc:=aUsrPostProc;
  if not IPXExists then begin Connect:=2000; exit end;
  if not OpenSocket then begin Connect:=2001; exit end;
  fillchar(RcvEcb,sizeof(PROTOcb),0); with RcvEcb do begin
    Sckt:=MySckt; FragCnt:=1; FragAddr:=@RcvIpxBlk; FragSize:=BLOCK_SIZE end;
  Listen;
  fillchar(SndEcb,sizeof(PROTOcb),0); with SndEcb do begin
    Sckt:=MySckt; FragCnt:=1;
    FragAddr:=@SndIpxBlk end;
{  fillchar(SndIpxBlk,BlkHeadSz,0); with SndIpxBlk do begin
    DestLan := SrvLan;
    fillchar(DestNode,sizeof(LANNodeAddr),$ff); DestSckt:=ServerSckt end;
  GetLocalTarget;}
  retry:=0;
1:inc(retry); SendBlock(nil,0);      { try to connect }
  EndTime:=Timer+BroadcastWaitTime;
  while Timer<EndTime do if RcvECB.InUse=0 then
    if(RcvEcb.ComplCode=0) and (RcvEcb.Length=2) then goto 3
    else goto 2;
2:if retry<MaxConnectRetry then goto 1;
  Connect:=2002; exit;
3:Connect:=RcvIpxBlk.ConnectErr;
{  SndIpxBlk.DestNode:=RcvIpxBlk.SrceNode;
  SndEcb.ImmediateAddr:=RcvEcb.ImmediateAddr;}
  SndEcb.ESRAddr:=@SendPostProc; RcvEcb.ESRAddr:=@ListenPostProc;
  Listen;
end;

procedure DisConnect;
begin
  CloseSocket;
end;
