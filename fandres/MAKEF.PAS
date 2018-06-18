{$D+}{$L+}
type
  TResFile = object
    Handle:word;
    procedure Open;
    procedure Close;
    procedure Seek(Pos:longint);
    function GetPos:longint;
    procedure Write(N:word;var Buf);
  end;


const FandResName:string[9]='FAND.RES'#0;

const LToCp1250:array[128..255] of byte=
      (199,252,233,226,228,249,230,231,179,235,213,245,238,143,196,
       198,201,197,229,244,246,188,190,140,156,214,220,141,157,163,
       215,232,225,237,243,250,165,185,142,158,202,234,160,159,200,
       186,171,187,95,95,95,166,166,193,194,204,170,166,166,43,
       43,175,191,43,43,45,45,43,45,43,195,227,43,43,45,45,166,45,
       43,164,240,208,207,203,239,210,205,206,236,43,43,95,95,
       222,217,95,211,223,212,209,241,242,138,154,192,218,224,219,
       253,221,254,180,173,189,178,161,162,167,247,184,176,168,183,
       251,216,248,95,32);
      KToCp1250:array[128..255] of byte=
      (200,252,233,239,228,207,141,232,236,204,197,205,190,229,196,
       193,201,158,142,244,246,211,249,218,253,214,220,138,188,221,
       216,157,225,237,243,250,242,210,217,212,154,248,224,192,200,
       186,171,187,95,95,95,166,166,193,194,204,170,166,166,43,
       43,175,191,43,43,45,45,43,45,43,195,227,43,43,45,45,166,45,
       43,164,240,208,207,203,239,210,205,206,236,43,43,95,95,
       222,217,95,211,223,212,209,241,242,138,154,192,218,224,219,
       253,221,254,180,173,189,178,161,162,167,247,184,176,168,183,
       251,216,248,95,32);
      Cp1250ToL:array[128..255] of byte=
      (128,129,130,131,132,133,134,135,136,137,230,139,151,155,166,
       141,144,145,146,147,148,149,150,151,152,153,231,155,152,156,
       167,171,170,243,244,157,207,164,179,245,249,169,184,174,172,
       240,174,189,248,177,242,136,239,181,182,250,247,165,173,175,
       149,241,150,190,232,181,182,198,142,145,143,128,172,144,168,
       211,183,214,215,210,209,227,213,224,226,138,153,158,252,222,
       233,235,154,237,221,225,234,160,131,199,132,146,134,135,159,
       130,169,137,216,161,140,212,208,228,229,162,147,139,148,246,
       253,133,163,251,129,236,238,255);

procedure TResFile.Open;
var HandleErr:word;
begin
  asm  push ds; mov ax,3c18H; lea dx,FAndResName[1]; mov cx,0; int 21H; pop ds;
       jb @1; les di,Self; mov es:[di].TResFile.Handle,ax; mov ax,0;
@1:    mov HandleErr,ax end;
  if HandleErr<>0 then begin writeln('can''t open FAND.RES err=',handleErr);halt end;
end;

procedure TResFile.Close; assembler;
asm  mov ah,3eH; les di,Self; mov bx,es:[di].TResFile.Handle; int 21H;
end;
          
procedure TResFile.Seek(Pos:longint); assembler;
asm  mov ax,4200H; les di,Self; mov bx,es:[di].TResFile.Handle;
     mov dx,Pos.word; mov cx,Pos[2].word; int 21H;
end;

function TResFile.GetPos:longint; assembler;
asm  mov ax,4201H; les di,Self; mov bx,es:[di].TResFile.Handle;
	 mov cx,0; mov dx,0; int 21H;
end;

procedure TResFile.Write(N:word;var Buf); assembler;
asm push ds; mov ah,40H; lds dx,Buf; mov cx,N;
	les di,Self; mov bx,es:[di].TResFile.Handle; int 21H; pop ds;
end;

const
  ResVersion:word=$0420;
  RMsgIdx=0; BgiEgaVga=1; BgiHerc=2; ChrLittKam=3; ChrTripKam=4;
  Ega8x14K=5; Vga8x16K=6; Vga8x19K=7;
  Ega8x14L=8; Vga8x16L=9; Vga8x19L=10;
  ChrLittLat=11; ChrTripLat=12;
  LatToWinCp=13; KamToWinCp=14; WinCpToLat=15;
  FandFace=16;
  MaxB=100;
  BufSize=64000;

type
  ByteArr = array[0..10] of byte;
  TMsgIdxItem = record Nr,Ofs:word; Count:byte end;
var
  R:TResFile;
  A:array[0..FandFace] of record Pos:longint; Size:word end;
  B:array[1..MaxB] of TMsgIdxItem;
  iB,iNr,oBuf:word;
  PBuf:^ByteArr;

procedure StoreFile(I:word;Name:string);
var F:file; L:longint;
begin
  Assign(F,Name);
  {$I-} Reset(F,1); {$I+}
  if IOResult<>0 then begin writeln(Name+' not found');halt end;
  L:=FileSize(F);
  A[I].Pos:=R.GetPos; A[I].Size:=L;
  BlockRead(F,PBuf^,L); Close(F); R.Write(L,PBuf^);
end;
procedure StoreArray(P:pointer;I,L:word);
begin
  A[I].Pos:=R.GetPos; A[I].Size:=L; R.Write(L,P^);
end;
procedure StoreMsg(I:word;S:string);
begin
  if (iB=0) or (I<>B[iB].Nr+B[iB].Count) or (B[iB].Count=16) then begin
    if iB=MaxB then begin writeln('Msg index overflow');halt end;
    inc(iB); with B[iB] do begin Nr:=I; Ofs:=oBuf; Count:=1 end end
  else inc(B[iB].Count);
  if oBuf>=BufSize-length(S) then begin writeln('Msg buffer overflow');halt end;
  Move(S,PBuf^[oBuf],length(S)+1); inc(oBuf,length(S)+1);
end;
procedure StoreMsgFile;
var F:text; s:string; n,i,err:word;
begin
  if paramstr(1)='D' then Assign(F,'FANDMSGD.TXT')
  else Assign(F,'FANDMSG.TXT');
  {$I-} Reset(F); {$I+}
  if IOResult<>0 then begin writeln('FANDMSG.TXT not found');halt end;
  iB:=0; oBuf:=0;
  while not EOF(F) do begin
    readln(F,s); val(copy(s,1,4),i,err); if err=0 then
	  StoreMsg(i,copy(s,6,255)) end;
  Close(F);
  n:=iB*sizeof(TMsgIdxItem);
  with A[RMsgIdx] do begin Pos:=R.GetPos; Size:=n+2 end;
  R.Write(2,iB); R.Write(n,B); R.Write(oBuf,PBuf^);
end;

begin
  GetMem(PBuf,BufSize);
  R.Open;
  R.Write(2,ResVersion);
  R.Write(sizeof(A),A);
  StoreMsgFile;
  StoreFile(BgiEgaVga,'EGAVGA.BGI');
  StoreFile(BgiHerc,'HERC.BGI');
  StoreFile(ChrLittKam,'LITTkam.CHR');
  StoreFile(ChrTripKam,'TRIPkam.CHR');
  StoreFile(Ega8x14K,'8x14MJK.');
  StoreFile(Vga8x16K,'8x16MJK.');
  StoreFile(Vga8x19K,'8x19MJK.');
  StoreFile(Ega8x14L,'8x14Lat.');
  StoreFile(Vga8x16L,'8x16Lat.');
  StoreFile(Vga8x19L,'8x19Lat.');
  StoreFile(ChrLittLat,'LITTlat.CHR');
  StoreFile(ChrTripLat,'TRIPlat.CHR');
  StoreArray(@LToCp1250,LatToWinCp,128);
  StoreArray(@KToCp1250,KamToWinCp,128);
  StoreArray(@Cp1250ToL,WinCpToLat,128);
  StoreFile(FandFace,'FandFace.Txt');
  R.Seek(2); R.Write(sizeof(A),A);
  R.Close;
  FreeMem(PBuf,BufSize);
end.
