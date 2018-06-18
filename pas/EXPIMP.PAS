{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit expimp;

interface

uses base,access,rdrun;

procedure CopyFile(CD:CopyDPtr);
function FormatOnDrive(DriveC:char):boolean;
function OldToNewCat(var FilSz:longint):boolean;
procedure Backup(IsBackup,NoCompress:boolean;Ir:word;NoCancel:boolean);
procedure BackupM(PD:InstrPtr);
procedure XEncode(S1,S2:LongStrPtr);
{$ifndef FandRunV}
procedure CodingCRdb(Rotate:boolean);
function PromptCodeRdb:boolean;
{$endif}
procedure CheckFile(FD:FileDPtr);

implementation

uses dos,
     drivers,obaseww,oaccess,runfrml,
{$ifdef FandSQL} channel, {$endif}
     wwmix,compile,rdmerg,runmerg,runedi;         {disk  hier TcFile/compress}

procedure Ovr; far;
assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push cs; push ax;
end;

{$I disk}

type
  VolStr = string[11];
  InOutMode = (_inp,_outp,_append);

  PhFile = ^ThFile;
  ThFile = object(TcFile)
    Handle: byte;
    Path: PathStr;
    Vol: VolStr;
    Mode: InOutMode;
    Floppy,IsEOL,Continued: boolean;
    Size,OrigSize,SpaceOnDisk: longint;
    FD:FileDPtr;
    constructor Init(APath: StringPtr; CatIRec:word; AMode:InOutMode; aCompress:byte;
                      F:PhFile);
    destructor Done; virtual;
    procedure Append;
    procedure ClearBuf;
    procedure Delete;
    function ForwChar:char;
    function RdChar:char;
    function RdDM(Delim:char; Max:integer):string;
    function RdDelim(Delim:char):string;
    function RdFix(N:integer):string;
    function RdLongStr:LongStrPtr;
    procedure ReadBuf2; virtual;
    procedure Reset;
    procedure ExtToT;
    procedure ResetT;
    procedure ResetX;
    procedure Rewrite;
    procedure RewriteT;
    procedure RewriteX;
    procedure TestError;
    function TestErr152:boolean;
    procedure WrChar(C:char);
    procedure WriteBuf2; virtual;
    procedure WrString(S:string);
    procedure WrLongStr(S:LongStrPtr; WithDelim:boolean);
  end;

var Nm:NameStr;
procedure CloseEqualFD; far;
var cp:PathStr;
begin with CFile^ do if (Typ<>'0') and SEquUpcase(Name,Nm) then begin
  cp:=CPath; SetCPathVol; if SEquUpcase(cp,CPath) then CloseFile; CPath:=cp;
end end;

constructor ThFile.Init(APath: StringPtr; CatIRec:word; AMode:InOutMode; aCompress:byte;
                        F:PhFile);
var l:byte absolute Path; d:DirStr; e:ExtStr;
begin
  inherited Init(aCompress); Mode:=AMode; Handle:=$FF;
  SetTxtPathVol(APath,CatIRec); Path:=CPath; Vol:=CVol;
  if (F<>nil) and (F^.Path=CPath) then begin SetMsgPar(CPath); RunError(660) end;
  if (CVol<>'#') and (CVol<>'') and (pos('.',CPath)=l-3) and
     (CPath[l] in ['0'..'9']) and (CPath[l-1] in ['0'..'4']) then Floppy:=true;
  FSplit(CPath,d,Nm,e); ForAllFDs(CloseEqualFD);
  case AMode of
   _inp: begin Reset; InitBufInp end;
   _outp: begin Rewrite; InitBufOutp end;
   _append: begin Append; InitBufOutp end;
  end;
end;

destructor ThFile.Done;
begin
  if Mode=_inp then begin CloseH(Handle); RunMsgOff end
  else begin
    WriteBuf(true); CloseH(Handle);
    if not Continued and (Size=0) then DeleteFile(Path) end;
  inherited Done;
end;

procedure ThFile.Append;
var l:byte absolute CPath;
label 1;
begin
  CVol:=Vol; CPath:=Path; TestMountVol(CPath[1]);
  if Floppy then begin
    inc(CPath[l-1],5);
1:  Handle:=OpenH(_isoldfile,RdOnly);
    if HandleError=0 then begin
      CloseH(Handle); Continued:=true;
      inc(Vol[length(Vol)]); CVol:=Vol; TestMountVol(CPath[1]); goto 1 end;
    dec(CPath[l-1],5) end;
  Handle:=OpenH(_isoldnewfile,Exclusive);
  TestCPathError; Size:=FileSizeH(Handle); SeekH(Handle,Size);
  SpaceOnDisk:=MyDiskFree(Floppy,ord(Path[1])-ord('@'));
end;

procedure ThFile.ClearBuf;
begin lBuf:=0; lBuf2:=0 end;

procedure ThFile.Delete;
var l:byte absolute CPath;
begin
  CVol:=Vol; CPath:=Path; TestMountVol(CPath[1]);
  repeat DeleteFile(CPath) until not TestErr152;
  if Floppy then begin inc(CPath[l-1],5); DeleteFile(CPath) end;
end;

procedure ThFile.WriteBuf2;
var
  l:byte absolute CPath; i,n:word;
begin
  i:=0;
  while i<lBuf2 do begin
    while SpaceOnDisk=0 do
    if Floppy then begin
      CloseH(Handle); CPath:=Path; inc(CPath[l-1],5); RenameFile56(Path,CPath,true);
      inc(Vol[length(Vol)]); Rewrite end
    else begin HandleError:=1; TestError end;
    n:=lBuf2-i; if (SpaceOnDisk>=0) and (n>SpaceOnDisk) then n:=SpaceOnDisk;
    WriteH(Handle,n,Buf2[i]); TestError;
    inc(i,n); inc(Size,n); if SpaceOnDisk>0 then dec(SpaceOnDisk,n) end;
  lBuf2:=0;
end;

function ThFile.ForwChar:char;
label 1;
begin
1:if iBuf<lBuf then ForwChar:=Buf[iBuf] else begin
    ReadBuf; if EOF then ForwChar:=^z else goto 1 end;
end;


procedure ThFile.ReadBuf2;
begin
  lBuf2:=0; iBuf2:=0;
  while Size=0 do
    if Continued then begin
      CloseH(Handle); RunMsgOff; inc(Vol[length(Vol)]); Reset end
    else begin EOF2:=true; exit end;
  lBuf2:=BufSize2; if lBuf2>Size then lBuf2:=Size;
  ReadH(Handle,lBuf2,Buf2^); TestError; dec(Size,lBuf2); RunMsgN(OrigSize-Size);
end;

function ThFile.RdChar:char;
label 1;
begin
1:if iBuf<lBuf then begin
    RdChar:=Buf[iBuf]; inc(iBuf) end
  else begin
    ReadBuf; if EOF then RdChar:=^z else goto 1 end;
end;

function ThFile.RdDM(Delim:char; Max:integer):string;
var s:string; c:char; l:byte absolute s;
label 1,2;
begin
  l:=0; IsEOL:=false; c:=RdChar;
1:if EOF then IsEOL:=true else
  if (c=^m) and not (Delim in ['''','"']) then begin
    IsEOL:=true; if ForwChar=^j then RdChar end else
  if c<>Delim then begin
2:  inc(l); s[l]:=c;
    if l<Max then begin c:=RdChar; goto 1 end end
  else if (Delim in ['''','"']) and (ForwChar=c) then begin
    RdChar; goto 2 end;
  RdDM:=s;
end;

function ThFile.RdDelim(Delim:char):string;
begin RdDelim:=RdDM(Delim,255) end;

function ThFile.RdFix(N:integer):string;
begin RdFix:=RdDM(^m,N) end;

function ThFile.RdLongStr:LongStrPtr;
var c:char; x:LongStrPtr; l,n:word;
label 1;
begin
  x:=GetStore(2); RdLongStr:=x; l:=0; n:=0;
  if ForwChar='''' then begin
    RdChar; c:=RdChar;
    while not EOF and (l<MaxLStrLen) do begin
      if c='''' then
        if ForwChar='''' then RdChar else goto 1;
      if n=0 then begin GetStore(1000); n:=1000 end;
      dec(n); inc(l); x^.A[l]:=c; c:=RdChar end;
    end
  else
    while not EOF and (l<MaxLStrLen) and (ForwChar<>^m) do begin
      if n=0 then begin GetStore(1000); n:=1000 end;
      dec(n); inc(l); x^.A[l]:=RdChar end;
1:x^.LL:=l;
end;

procedure ThFile.Reset;
var l:byte absolute CPath;
begin
  CVol:=Vol; CPath:=Path; TestMountVol(CPath[1]);
  repeat Handle:=OpenH(_isoldfile,RdOnly); Continued:=false;
  until not TestErr152;
  if (HandleError=2) and Floppy then begin
    inc(CPath[l-1],5); Handle:=OpenH(_isoldfile,RdOnly);
    Continued:=HandleError=0 end;
  if HandleError=2 then Size:=0
  else begin
    TestError; Size:=FileSizeH(Handle) end;
  RunMsgOn('C',Size); OrigSize:=Size;
end;

procedure ThFile.ExtToT;
begin
  CPath:=Path; FSplit(CPath,CDir,CName,CExt); CExtToT; Path:=CPath;
end;

procedure ThFile.ResetT;
begin
  ExtToT; Reset; InitBufInp;
end;

procedure ThFile.ResetX;
begin
  Path[length(Path)-2]:='X'; Reset; InitBufInp;
end;

procedure ThFile.Rewrite;
begin
  Delete; CPath:=Path;
  CVol:=Vol; Handle:=OpenH(_isoverwritefile,Exclusive);
  TestCPathError; SpaceOnDisk:=MyDiskFree(Floppy,ord(Path[1])-ord('@')); Size:=0;
end;

procedure ThFile.RewriteT;
  begin ExtToT; Rewrite; InitBufOutp; end;

procedure ThFile.RewriteX;
  begin Path[length(Path)-2]:='X'; Rewrite; InitBufOutp; end;

procedure ThFile.TestError;
begin
  CPath:=Path; TestCPathError;
end;

function ThFile.TestErr152:boolean;
var s:string[1];
begin
  TestErr152:=false; if HandleError=152 then begin
    F10SpecKey:=_ESC_; s:=CPath; Set2MsgPar(s,''); WrLLF10Msg(808);
    if (KbdChar=_ESC_) and PromptYN(21) then GoExit; TestErr152:=true end;
end;

procedure ThFile.WrChar(C:char);
begin
  if lBuf=BufSize then WriteBuf(false); Buf[lBuf]:=c; inc(lBuf);
end;

procedure ThFile.WrString(S:string);
var i:integer;
begin
  for i:=1 to length(S) do WrChar(S[i]);
end;

procedure ThFile.WrLongStr(S:LongStrPtr; WithDelim:boolean);
var i:word;
begin
  if WithDelim then WrChar('''');
  for i:=1 to S^.LL do begin
    WrChar(S^.A[i]); if WithDelim and (S^.A[i]='''') then WrChar('''') end;
  if WithDelim then WrChar('''');
end;

procedure VarFixImp(F1:PhFile; Opt:CpOption);
var
  s:string; x:LongStrPtr; pos:longint; F:FieldDPtr; c:char; r:float; err:word;
label 1;
begin
  with F1^ do begin
  F:=CFile^.FldD;  IsEOL:=false;
  while F<>nil do begin if F^.Flg and f_Stored<>0 then
    if IsEOL then
      case F^.FrmlTyp of
        'R': r_(F,0); 'B': b_(F,false); 'S': s_(F,'') end
    else
      case F^.Typ of
       'F':begin
           if Opt=cpFix then s:=RdFix(F^.L) else s:=RdDelim(',');
           val(LeadChar(' ',s),r,err);
           if F^.Flg and f_Comma<>0 then r:=r*Power10[F^.M]; r_(F,r)  end;
       'A':if Opt=cpFix then s_(F,RdFix(F^.L))
           else begin
             c:=ForwChar;
             if c in ['''','"'] then begin
               RdChar; s:=RdDelim(c); RdDelim(',') end
             else s:=RdDelim(',');
             s_(F,s) end;
       'N':if Opt=cpFix then s_(F,RdFix(F^.L)) else s_(F,RdDelim(','));
       'D','R':begin if Opt=cpFix then s:=RdFix(F^.L)
           else begin s:=RdDelim(',');
             if s[1] in ['''','"'] then s:=copy(s,2,length(s)-2) end;
           if s='' then r_(F,0.0) else
           if F^.Typ='R' then begin val(s,r,err); r_(F,r) end
           else r_(F,ValDate(s,FieldDMask(F)^)) end;
       'B':begin s:=RdFix(1); b_(F,s[1]='A');
           if Opt=cpVar then RdDelim(',') end;
       'T':if Opt=cpVar then begin
             x:=RdLongStr; s:=RdDelim(','); longs_(F,x); ReleaseStore(x) end
           else t_(F,0);
      end;
    F:=F^.Chain;
    end;
  if not IsEOL then RdDelim(^m);
  end;
end;

procedure VarFixExp(F2:PhFile; Opt:CpOption);
var
  i,n:word; s,s1:string; x:LongStrPtr; first:boolean; r:float; F:FieldDPtr;
begin
  with F2^ do begin
  F:=CFile^.FldD; first:=true;
  while F<>nil do begin if F^.Flg and f_Stored<>0 then begin
    if first then first:=false else if Opt=cpVar then WrChar(',');
    case F^.Typ of
     'F':begin r:=_r(F);
         if F^.Flg and f_Comma<>0 then r:=r/Power10[F^.M]; str(r:F^.L:F^.M,s);
         if length(s)>F^.L then begin s[1]:='>';s[0]:=chr(F^.L) end;
         if Opt=cpVar then begin
           s:=LeadChar(' ',s);
           if F^.M>0 then begin
             s:=TrailChar('0',s);
             if (length(s)>0)and(s[length(s)]='.') then dec(s[0]) end;
           if s='0' then s:='';
           end;
         end;
     'A':begin s:=_shorts(F);
         if Opt=cpVar then begin
           if F^.M=1 then s:=TrailChar(' ',s)
           else s:=LeadChar(' ',s);
           s1:='';
           for i:=1 to length(s) do begin
             s1:=s1+s[i]; if s[i]='''' then s1:=s1+'''' end;
           s:=''''+s1+''''
         end end;
     'N':begin s:=_shorts(F);
         if Opt=cpVar then
           if F^.M=1 then s:=TrailChar('0',s)
           else s:=LeadChar('0',s);
         end;
     'D','R':begin r:=_r(F); if (r=0) and (Opt=cpVar) then s:=''
         else if F^.Typ='R' then str(r:F^.L,s)
         else begin s:=StrDate(r,FieldDMask(F)^);
              if Opt=cpVar then s:=''''+s+'''' end;
         end;
     'B':if _b(F) then s:='A' else s:='N';
     'T':if Opt=cpVar then begin
           x:=_longs(F); WrLongStr(x,true); ReleaseStore(x) end;
         end;
    if F^.Typ<>'T' then WrString(s);
    end;
    F:=F^.Chain;
  end end;
end;


procedure CopyFile(CD:CopyDPtr);

procedure TxtCtrlJ;
var
  c:char; F1,F2:PhFile; er:ExitRecord; m:InOutMode;
label 1;
begin
  NewExit(Ovr,er); goto 1; F1:=nil; F2:=nil;
  with CD^ do begin
    m:=_outp; if Append then m:=_append;
    New(F1,Init(Path1,CatIRec1,_inp,0,nil));
    New(F2,Init(Path2,CatIRec2,m,0,F2)) end;
  c:=F1^.RdChar;
  while not F1^.EOF do begin
    case c of
      ^m: begin F2^.WrChar(^m);F2^.WrChar(^j) end;
      ^j:;
    else F2^.WrChar(c) end;
    c:=F1^.RdChar end;
  LastExitCode:=0;
1:RestoreExit(er);
  if (F1<>nil) and (F1^.Handle<>$FF) then F1^.Done;
  if (F2<>nil) and (F2^.Handle<>$FF) then begin
    if LastExitCode<>0 then F2^.ClearBuf; F2^.Done end;
end;

procedure ImportTxt;
var
  F1:PhFile; er:ExitRecord; md:LockMode; FE:FrmlElem;
{$ifdef FandSQL}  q:SQLStreamPtr; {$endif}
label 1;
begin
  NewExit(Ovr,er); goto 1; F1:=nil; {$ifdef FandSQL} q:=nil; {$endif}
  with CD^ do begin
  New(F1,Init(Path1,CatIRec1,_inp,0,nil));
  if HdFD<>nil then begin
    FE.Op:=_const; FE.S:=F1^.RdDelim(^m); AsgnParFldFrml(HdFD,HdF,@FE,false) end;
  CFile:=FD2; CRecPtr:=GetRecSpace;
{$ifdef FandSQL} if CFile^.IsSQLFile then begin
    New(q,Init); q^.OutpRewrite(Append) end else {$endif}
  md:=RewriteF(Append);
  while not(F1^.EOF) and (F1^.ForwChar<>^z) do begin
    ZeroAllFlds; ClearDeletedFlag;
    VarFixImp(F1,Opt1); F1^.ForwChar;{set IsEOF at End}
{$ifdef FandSQL} if CFile^.IsSQLFile then q^.PutRec else {$endif}
    begin PutRec;
    if Append and (CFile^.Typ='X') then TryInsertAllIndexes(CFile^.IRec)
    end end;
  LastExitCode:=0;
1:RestoreExit(er);  {$ifdef FandSQL}
  if q<>nil then begin q^.OutpClose; ClearRecSpace(CRecPtr) end; {$endif}
  if (F1<>nil) and (F1^.Handle<>$FF) then begin F1^.Done; OldLMode(md) end;
end end;

procedure ExportTxt;
var
  F2:PhFile; n:longint; i:longint; s:string; er:ExitRecord;
  Scan:XScanPtr; md:LockMode; m:InOutMode;
label 1,2;
begin
  NewExit(Ovr,er); goto 2; F2:=nil; Scan:=nil;
  with CD^ do begin
  m:=_outp; if Append then m:=_append;
  New(F2,Init(Path2,CatIRec2,m,0,nil));
  if HdFD<>nil then begin
    LinkLastRec(HdFD,n,true); s:=_shorts(HdF);
    i:=pos(^m,s); if i>0 then s[0]:=chr(i-1);
    F2^.WrString(s); F2^.WrString(^m^j);
    ClearRecSpace(CRecPtr); ReleaseStore(CRecPtr) end;
  CFile:=FD1; CRecPtr:=GetRecSpace; md:=NewLMode(RdMode);
  New(Scan,Init(CFile,ViewKey,nil,true)); Scan^.Reset(nil,false);
  RunMsgOn('C',Scan^.NRecs);
1:Scan^.GetRec;
  if not Scan^.EOF then begin
    VarFixExp(F2,Opt2); F2^.WrString(^m^j); RunMsgN(Scan^.IRec); goto 1 end;
  LastExitCode:=0; RunMsgOff;
2:RestoreExit(er);
  if Scan<>nil then begin Scan^.Close; ClearRecSpace(CRecPtr); OldLMode(md) end;
  if (F2<>nil) and (F2^.Handle<>$FF) then begin
    if LastExitCode<>0 then F2^.ClearBuf; F2^.Done end;
end end;

procedure MakeMerge;
var er:ExitRecord; s,ali:string;
label 1;
begin
  NewExit(Ovr,er); goto 1;
  with CD^ do begin
    s:='#I1_'+FD1^.Name; if ViewKey<>nil then begin
      ali:=ViewKey^.Alias^; if ali='' then ali:='@'; s:=s+'/'+ali end;
    s:=s+' #O1_'+FD2^.Name; if Append then s:=s+'+' end;
  SetInpStr(s); ReadMerge; RunMerge; LastExitCode:=0;
1:RestoreExit(er);
end;

procedure ExportFD;
procedure Cpy(h:word;sz:longint;F2:PhFile);
var n:word; i:longint;
begin with F2^ do begin
  SeekH(h,0); i:=0; RunMsgOn('C',sz);
  while i<sz do begin
    if sz-i>BufSize then n:=BufSize else n:=sz-i; inc(i,n);
    ReadH(h,n,Buf^); TestCFileError; lBuf:=n; WriteBuf(false);
    RunMsgN(i) end;
  Done; RunMsgOff end end;
var er:ExitRecord; F2:PhFile; h:word; md:LockMode; n:longint;
label 0,1;
begin
  NewExit(Ovr,er); goto 1; F2:=nil;
  with CD^ do begin
    CFile:=FD1; SaveFiles; md:=NewLMode(RdMode);
    New(F2,Init(Path2,CatIRec2,_outp,0,nil)); n:=XNRecs(FD1^.Keys) end;
  with CFile^ do
    if n=0 then F2^.Done else Cpy(Handle,UsedFileSize,F2);
  if CFile^.TF<>nil then begin
    F2^.RewriteT; with CFile^.TF^ do
    if n=0 then F2^.Done else Cpy(Handle,UsedFileSize,F2) end;
  if CD^.WithX1 then begin
    F2^.RewriteX; with CFile^.XF^ do
    if n=0 then F2^.Done else Cpy(Handle,UsedFileSize,F2) end;
0:LastExitCode:=0;
1:RestoreExit(er);
  if (F2<>nil) and (F2^.Handle<>$FF) then begin
    if LastExitCode<>0 then F2^.ClearBuf; F2^.Done; OldLMode(md) end;
end;

procedure ImportFD;
procedure Cpy(h:word;F1:PhFile);
begin with F1^ do begin
  ClearCacheH(h); SeekH(h,0);
  while not EOF do begin WriteH(h,lBuf,Buf^); TestCFileError; ReadBuf end;
  WriteH(h,0,Buf^){trunc}; Done end end;
var er:ExitRecord; F1:PhFile; h:word; md:LockMode;
label 1;
begin
  NewExit(Ovr,er); goto 1; F1:=nil;
  with CD^ do begin
    New(F1,Init(Path1,CatIRec1,_inp,0,nil)); CFile:=FD2; md:=NewLMode(ExclMode) end;
  Cpy(CFile^.Handle,F1);
  if CFile^.TF<>nil then begin
    F1^.ResetT; Cpy(CFile^.TF^.Handle,F1); CloseClearH(CFile^.TF^.Handle) end;
  if CFile^.XF<>nil then begin
    if CD^.WithX2 then begin F1^.ResetX; Cpy(CFile^.XF^.Handle,F1) end
    else begin XFNotValid; SaveCache(0) end;
    CloseClearH(CFile^.XF^.Handle) end;
  LastExitCode:=0; CloseClearH(CFile^.Handle);
  if CFile=CatFD then OpenF(Exclusive);
1:RestoreExit(er);
  if (F1<>nil) and (F1^.Handle<>$FF) then F1^.Done;
end;

procedure ConvWinCp(pBuf,pKod:pointer;L:word); assembler;
asm  push ds; mov cx,L; jcxz @3;
     lds si,pKod; les di,pBuf; xor ah,ah; cld;
@1:  mov al,es:[di]; cmp al,80H; jb @2;
     sub al,80H; mov bx,ax; mov al,ds:[si+bx];
@2:  stosb; loop @1;
@3:  pop ds end;

procedure MakeCopy;
var er:ExitRecord; F1,F2:PhFile; m:InOutMode; kod:word; pKod:pointer;
label 0,1;
begin
  NewExit(Ovr,er); goto 1; F1:=nil; F2:=nil;
  with CD^ do begin
    New(F1,Init(Path1,CatIRec1,_inp,0,nil));
    m:=_outp; if Append then m:=_append;
    New(F2,Init(Path2,CatIRec2,m,0,F1)) end;
  case CD^.Mode of
    5:begin kod:=LatToWinCp; goto 0 end;
    6:begin kod:=KamToWinCp; goto 0 end;
    7:begin kod:=WinCpToLat;
0:          ResFile.Get(kod,pKod) end end;
  while not F1^.EOF do begin
    Move(F1^.Buf^,F2^.Buf^,F1^.lBuf); F2^.lBuf:=F1^.lBuf;
    case CD^.Mode of
      1:ConvKamenLatin(F2^.Buf^,F2^.lBuf,true);
      2:ConvKamenLatin(F2^.Buf^,F2^.lBuf,false);
      3:ConvToNoDiakr(F2^.Buf^,F2^.lBuf,foKamen);
      4:ConvToNoDiakr(F2^.Buf^,F2^.lBuf,foLatin2);
      5,6,7: ConvWinCp(F2^.Buf,pKod,F2^.lBuf) end;
    F2^.WriteBuf(false);
    F1^.ReadBuf end;
  LastExitCode:=0;
1:RestoreExit(er);
  if (F1<>nil) and (F1^.Handle<>$FF) then F1^.Done;
  if (F2<>nil) and (F2^.Handle<>$FF) then begin
    if LastExitCode<>0 then F2^.ClearBuf; F2^.Done end;
end;

var p,p2:pointer;
label 1,2;
begin
  LastExitCode:=2; MarkStore(p);
  with CD^ do begin
    if Opt1 in [cpFix,cpVar] then ImportTxt else
    if Opt2 in [cpFix,cpVar] then ExportTxt else
    if FD1<>nil then if FD2<>nil then MakeMerge else ExportFD
    else if FD2<>nil then ImportFD else
    if Opt1=cpTxt then TxtCtrlJ else MakeCopy;
    SaveFiles; RunMsgOff; ReleaseStore(p);
    if (LastExitCode<>0) and not CD^.NoCancel then GoExit end;
end;

function OldToNewCat(var FilSz:longint):boolean;
var x:record NRecs:longint;RecLen:word end; off,offNew,i:longint;
    a:array[1..90] of byte;
begin with CFile^ do begin
  OldToNewCat:=false; if Typ<>'C' then exit; RdWrCache(true,Handle,NotCached,0,6,x);
  if x.RecLen<>106 then exit;
  x.RecLen:=107; RdWrCache(false,Handle,NotCached,0,6,x);
  for i:=x.NRecs downto 1 do begin
    off:=6+(i-1)*106; offNew:=off+(i-1);
    RdWrCache(true,Handle,NotCached,off+16,90,a); RdWrCache(false,Handle,NotCached,offNew+17,90,a);
    a[17]:=0;
    RdWrCache(true,Handle,NotCached,off,16,a); RdWrCache(false,Handle,NotCached,offNew,17,a) end;
  NRecs:=x.NRecs; FilSz:=x.NRecs*107+6; OldToNewCat:=true;
end end;

type
  PyFile = ^TyFile;
  TyFile = object(TcFile)
    Drive: byte;
    DrvNm:string[1]; Vol:VolStr; Path:PathStr;
    IsBackup,Floppy,Continued: boolean;
    procedure MountVol(IsFirst:boolean);
  end;

procedure TyFile.MountVol(isFirst:boolean);
var bt:TBoot; i,j,nSect,err:word; p:PDirEntryArr; s:SearchRec; fat:pointer;
label 1,2,3;
begin
  if isFirst then begin
    Drive:=ord(upcase(DrvNm[1]))-ord('@');
    Floppy:=(Vol<>'') and (Vol[1]<>'#') and (Drive<FloppyDrives) end;
  if not Floppy then exit;
  if not isFirst then inc(Vol[length(Vol)]);
  CVol:=Vol;
  if not SEquUpcase(MountedVol[Drive],Vol) then begin
    ReleaseDrive(Drive);
1:  F10SpecKey:=_ESC_; Set2MsgPar(drvNm,Vol); WrLLF10Msg(808);
    if KbdChar=_ESC_ then if PromptYN(21) then GoExit else goto 1 end;
2:FindFirst(drvNm+':\*.*',VolumeID,s);
  case DosError of
    31,158,162{hardware failure}:
      if IsBackup and PromptYN(655) and FormatOnDrive(DrvNm[1]) then
        begin MountedVol[Drive]:=Vol; exit end
      else goto 1;
    152{drive not ready}: goto 1;
    18{label missing}:
      if IsBackup then if PromptYN(807) then goto 3 else goto 1
      else begin WrLLF10Msg(809); goto 1 end;
    0: ;
    else WrLLF10Msg(810); goto 1 end;
  i:=pos('.',s.Name); if i<>0 then delete(s.Name,i,1);
  if not SEquUpcase(s.Name,Vol) then begin
    SetMsgPar(s.Name);
    if IsBackup then if spec.OverwrLabeledDisk and PromptYN(816) then goto 3 else goto 1;
    WrLLF10Msg(817); goto 1 end;
3:MountedVol[Drive]:=Vol;
  if not IsBackup then exit;
  SetMsgPar(drvNm);
  ResetDisks; bt.Init(Drive-1);
  i:=bt.ReadSect(0,1,bt.Boot); if i<>0 then RunError(656);
  fat:=GetZStore(bt.SecSize*bt.SecsPerFat);
  bt.ReadSect(bt.ReservedSecs,bt.SecsPerFat,fat^);
  FatPut(fat,0,$ff00+bt.MediaCode); FatPut(fat,1,$ffff);
  for i:=2 to ((2*longint(bt.SecsPerFat)*bt.SecSize)div 3)-1 do begin
    j:=FatGet(fat,i); if j<>fatBadCluster then FatPut(fat,i,0) end;
  for i:=0 to bt.FatCount-1 do begin
    err:=bt.WriteSect(bt.ReservedSecs+i*bt.SecsPerFat,bt.SecsPerFat,fat^);
    if err<>0 then begin
      if err=3 then WrLLF10Msg(850) else WrLLF10Msg(860); goto 1 end end;
  p:=PDirEntryArr(GetZStore(bt.SecsPerRoot*bt.SecSize));
  FillVolDirEntry(p^,Vol); bt.WriteSect(bt.RootSec,bt.SecsPerRoot,p^);
  ResetDisks; ReleaseStore(fat);
end;



type
  PbFile = ^TbFile;
  TbFile = object(TyFile)
    Handle: byte;
    Dir: DirStr; FName: NameStr; Ext: ExtStr;
    Size,OrigSize,SpaceOnDisk: longint;
    constructor Init(NoCompress:boolean);
    procedure TestErr;
    procedure Reset;
    procedure Rewrite;
    procedure ReadBuf2; virtual;
    procedure WriteBuf2; virtual;
    procedure BackupH;
    procedure RestoreH;
    procedure BackupHFD(h:word);
    procedure RestoreHFD(h:word);
    procedure BackupFD;
    procedure RestoreFD;
    procedure Backup(aIsBackup:boolean;Ir:word);
  end;
constructor TbFile.Init(NoCompress:boolean);
begin
  if NoCompress then inherited Init(0) else inherited Init(1);
end;
procedure TbFile.TestErr;
begin
  CPath:=Path; TestCPathError;
end;

procedure TbFile.Reset;
var l:byte absolute CPath;
label 1;
begin
  Path:=Dir+FName+Ext;
1:CVol:=Vol; CPath:=Path; Handle:=OpenH(_isoldfile,RdOnly); Continued:=false;
  if (HandleError=2) and Floppy then begin
    inc(CPath[l-2],5); Handle:=OpenH(_isoldfile,RdOnly); Continued:=true end;
  TestCPathError; Size:=FileSizeH(Handle); OrigSize:=Size; RunMsgOn('C',Size);
end;
procedure TbFile.Rewrite;
begin
  Path:=Dir+FName+Ext;
  CPath:=Path; CVol:=Vol; Handle:=OpenH(_isoverwritefile,Exclusive);
  TestCPathError; SpaceOnDisk:=MyDiskFree(Floppy,Drive);
end;

procedure TbFile.ReadBuf2;
begin
  lBuf2:=0; iBuf2:=0; if (Size=0) and Continued then begin
    CloseH(Handle); RunMsgOff; MountVol(false); Reset end;
  if Size=0 then begin EOF2:=true; exit end;
  lBuf2:=BufSize2; if lBuf2>Size then lBuf2:=Size;
  ReadH(Handle,lBuf2,Buf2^); TestErr; dec(Size,lBuf2); RunMsgN(OrigSize-Size);
end;
procedure TbFile.WriteBuf2;
var l:byte absolute CPath; i,n:word;
begin
  i:=0; while i<lBuf2 do begin
    if SpaceOnDisk=0 then if Floppy then begin
      CloseH(Handle); CPath:=Path; inc(CPath[l-2],5); RenameFile56(Path,CPath,true);
      MountVol(false); Rewrite end
    else begin HandleError:=1; TestErr end;
    n:=lBuf2-i; if (SpaceOnDisk>=0) and (n>SpaceOnDisk) then n:=SpaceOnDisk;
    WriteH(Handle,n,Buf2[i]);
    TestErr; inc(i,n); if SpaceOnDisk>0 then dec(SpaceOnDisk,n) end;
  lBuf2:=0;
end;

procedure TbFile.BackupH;
var h,n:word; i,sz:longint;
label 1;
begin
  h:=OpenH(_isoldfile,RdOnly);
  if HandleError=2 then begin
     Rewrite;
     InitBufOutp; goto 1 end;
  TestCPathError;
  Rewrite;
  InitBufOutp;
  sz:=FileSizeH(h);
  RunMsgOn('C',sz);
  i:=0; while i<sz do begin
    if sz-i>BufSize then n:=BufSize else n:=sz-i; inc(i,n);
    ReadH(h,n,Buf^); lBuf:=n;
    WriteBuf(false); RunMsgN(i) end;
  CloseH(h); RunMsgOff;
1:WriteBuf(true); CloseH(Handle);
end;
procedure TbFile.RestoreH;
var h:word; l:longint; s:string;
begin
  s:=CPath; h:=OpenH(_isoverwritefile,Exclusive); TestCPathError; Reset;
  InitBufInp; while not EOF do begin WriteH(h,lBuf,Buf^); ReadBuf end;
  CloseH(Handle); l:=FileSizeH(h); CloseH(h);
  if (l=0) then DeleteFile(s);  { wegen 6 byte shared files }
  RunMsgOff;
end;
procedure TbFile.BackupHFD(h:word);
var n:word; i,sz:longint;
begin
  Rewrite; InitBufOutp;
  sz:=FileSizeH(h); RunMsgOn('C',sz);
  SeekH(h,0); i:=0; while i<sz do begin
    if sz-i>BufSize then n:=BufSize else n:=sz-i; inc(i,n);
    ReadH(h,n,Buf^); lBuf:=n;
    WriteBuf(false); RunMsgN(i) end;
  WriteBuf(true); CloseH(Handle); RunMsgOff;
end;
procedure TbFile.RestoreHFD(h:word);
begin
  SeekH(h,0);
  while not EOF do begin WriteH(h,lBuf,Buf^); ReadBuf end;
  WriteH(h,0,Buf^); ClearCacheH(h); CloseH(Handle); RunMsgOff;
end;

procedure TbFile.BackupFD;
var md:LockMode;
begin with CFile^ do begin
  md:=NewLMode(RdMode); BackupHFD(Handle);
  if TF<>nil then begin Ext[2]:='T'; BackupHFD(TF^.Handle) end;
  OldLMode(md);
end end;
procedure TbFile.RestoreFD;
var l:word; md:LockMode;
begin with CFile^ do begin
  Reset; InitBufInp;
  if (lBuf>=6) and (Typ in ['6','X']) then begin
    move(buf[4],l,2); if RecLen<>l then begin
      SetMsgPar(Path); RunError(883) end end;
  if Handle=$FF then begin
    SetCPathVol; LMode:=NullMode;
    if IsNetCVol then UMode:=Shared else UMode:=Exclusive;
    Handle:=OpenH(_isoldnewfile,UMode); TestCFileError; NRecs:=0;
    if TF<>nil then begin CExtToT;
      TF^.Handle:=OpenH(_isoldnewfile,UMode); TF^.TestErr end;
    if Typ='X' then begin CExtToX;
      XF^.Handle:=OpenH(_isoldnewfile,UMode); XF^.TestErr end end;
  md:=LMode;
  if IsShared and (ExclMode>LMode) then ChangeLMode(ExclMode,0,false);
  RestoreHFD(Handle);
  if TF<>nil then begin
    Ext[2]:='T'; Reset; InitBufInp; RestoreHFD(TF^.Handle) end;
  RdPrefixes; XFNotValid; SaveCache(0); OldLMode(md);
end end;

  var x_FD:FileDPtr; x_I:word;
  procedure FindFDforI; far;
  begin if (x_FD=nil) and (CFile^.CatIRec=x_I) then x_FD:=CFile end;

procedure TbFile.Backup(aIsBackup:boolean;Ir:word);
var ArNr:string[2]; d:DirStr; n:NameStr; e:ExtStr; i:word;
    h:word; numbers:string[80];
label 1;
begin
  IsBackup:=aIsBackup; SaveFiles;
  ArNr:=RdCatField(Ir,CatArchiv); Vol:=RdCatField(Ir,CatVolume);
  Path:=RdCatField(Ir,CatPathName); i:=pos(' ',Path); numbers:='';
  if i<>0 then begin numbers:=copy(Path,i,255); delete(Path,i,255) end;
  Path:=FExpand(Path); FSplit(Path,Dir,n,e);
  Ext:='.000'; DrvNm:=copy(Dir,1,1);
  MountVol(true); SetInpStr(Numbers); RdLex;
1:for i:=1 to CatFD^.NRecs do
  if not SEquUpcase(RdCatField(i,CatRdbName),'ARCHIVES') then begin
    if (RdCatField(i,CatArchiv)=ArNr) then begin
    FSplit(RdCatField(i,CatPathName),d,FName,e); Ext[2]:='0';
    case Ext[4] of
      '9':Ext[4]:='A';
      'Z':begin Ext[4]:='0';if Ext[3]='9' then Ext[3]:='A' else inc(Ext[3]) end
      else inc(Ext[4]) end;
    x_FD:=nil; x_I:=i; ForAllFDs(FindFDforI); CFile:=x_FD;
    if CFile<>nil then begin
      h:=CFile^.Handle;
      if IsBackup then BackupFD else RestoreFD;
      if h=$ff then CloseFile end
    else begin
      CPath:=FExpand(RdCatField(i,CatPathName)); CVol:=RdCatField(i,CatVolume);
      TestMountVol(CPath[1]);
      if IsBackup then BackupH else RestoreH end;
    end end;
  while not (Lexem in [^z,_number]) do RdLex; if Lexem=_number then begin
    if length(LexWord)=1 then ArNr:='0'+LexWord else ArNr:=LexWord;
    RdLex; goto 1 end;
end;

procedure Backup(IsBackup,NoCompress:boolean;Ir:word;NoCancel:boolean);
var F:PbFile; er:ExitRecord;
label 1;
begin
  new(F,Init(NoCompress)); NewExit(Ovr,er); goto 1; LastExitCode:=1;
  F^.Backup(IsBackup,Ir); LastExitCode:=0;
1:RestoreExit(er); ReleaseStore(F);
  if LastExitCode<>0 then begin RunMsgOff; if not NoCancel then GoExit end;
end;

{==========================================================================}

type
  PzFile = ^TzFile;
  TzFile = object(TyFile)
    WBase,WPos:longint;
    Handle:byte;
    SpaceOnDisk,Size,OrigSize:longint;
    OldDir,Dir: DirStr;
    SubDirOpt,OverwrOpt:boolean;
    constructor Init(BkUp,NoCompr,SubDirO,OverwrO:boolean;Ir:word;const aDir:string);
    procedure Close;
    function  GetWPtr:longint;
    procedure StoreWPtr(Pos,N:longint);
    function  StoreWStr(s:string):longint;
    function  ReadWPtr(Pos:longint):longint;
    function  ReadWStr(var Pos:longint):string;
    function  StoreDirD(const RDir:string):longint;
    procedure SetDir(const RDir:string);
    procedure Get1Dir(Msk:StringList;D:longint;var DLast:longint);
    procedure GetDirs(Mask:LongStrPtr);
    procedure Reset;
    procedure Rewrite;
    procedure ReadBuf2; virtual;
    procedure WriteBuf2; virtual;
    procedure RdH(H:word;Skip:boolean);
    procedure WrH(H:word;Sz:longint);
    procedure ProcFileList;
    procedure Backup(aMsk:LongStrPtr);
    procedure Restore;
  end;

constructor TzFile.Init(BkUp,NoCompr,SubDirO,OverwrO:boolean;
                        Ir:word;const aDir:string);
begin
  SaveFiles; ForAllFDs(ClosePassiveFD);
  if NoCompr then inherited Init(0) else inherited Init(1);
  IsBackup:=BkUp; SubDirOpt:=SubDirO; OverwrOpt:=OverwrO;
  GetDir(0,OldDir);
  Vol:=RdCatField(Ir,CatVolume);
  CPath:=RdCatField(Ir,CatPathName); Path:=FExpand(CPath); DrvNm:=copy(Path,1,1);
  Dir:=aDir; AddBackSlash(Dir);
  WBase:=MaxWSize; WPos:=WBase;
end;
procedure TzFile.Close;
begin if Handle<>0 then begin
  MaxWSize:=WBase; TruncH(WorkHandle,MaxWSize); FlushH(WorkHandle);
  CloseH(Handle); ChDir(OldDir);
end end;
function TzFile.GetWPtr:longint;
begin
  GetWPtr:=WPos-WBase; inc(WPos,4);
end;
procedure TzFile.StoreWPtr(Pos,N:longint);
begin
  SeekH(WorkHandle,WBase+Pos); WriteH(WorkHandle,4,N);
end;
function TzFile.StoreWStr(s:string):longint;
begin
  SeekH(WorkHandle,WPos); WriteH(WorkHandle,length(s)+1,s);
  StoreWStr:=WPos-WBase; inc(WPos,length(s)+1);
end;
function TzFile.ReadWPtr(Pos:longint):longint;
var n:longint;
begin
  SeekH(WorkHandle,WBase+Pos); ReadH(WorkHandle,4,n); ReadWPtr:=n;
end;
function TzFile.ReadWStr(var Pos:longint):string;
var s:string;
begin
  SeekH(WorkHandle,WBase+Pos);
  ReadH(WorkHandle,1,s); ReadH(WorkHandle,length(s),s[1]);
  inc(Pos,length(s)+1); ReadWStr:=s;
end;
function TzFile.StoreDirD(const RDir:string):longint;
begin
  StoreDirD:=GetWPtr; GetWPtr; GetWPtr; StoreWStr(RDir);
end;
procedure TzFile.SetDir(const RDir:string);
var d:DirStr;
label 1;
begin
  d:=Dir+RDir; SetMsgPar(d); DelBackSlash(d);
1:{$I-} ChDir(d);{$I+} if IOResult<>0 then
    if not IsBackup and SubDirOpt then begin
      {$I-} MkDir(d);{$I+} if IOResult<>0 then RunError(644); goto 1 end
    else RunError(703);
end;
procedure TzFile.Get1Dir(Msk:StringList;D:longint;var DLast:longint);
var SR:SearchRec; p:PathStr; i,n:longint; RDir:string; sl:StringList;
label 1;
begin
  i:=D+12; RDir:=ReadWStr(i);
  p:=Dir+RDir+'*.*'; FindFirst(p+#00,0,SR); n:=0;
  if not(DosError in [0,18]) then begin SetMsgPar(p); RunError(904) end;
  while DosError=0 do begin
    if Msk=nil then begin
1:    i:=StoreWStr(SR.name); if n=0 then StoreWPtr(D+4,i); inc(n) end
    else begin sl:=Msk; while sl<>nil do begin
      if EqualsMask(@SR.name[1],length(SR.name),sl^.S) then goto 1;
      sl:=sl^.Chain end end;
    FindNext(SR) end;
  StoreWPtr(D+8,n); StoreWPtr(DLast,0); if not SubDirOpt then exit;
  FindFirst(p+#00,Directory,SR);
  while DosError=0 do begin
    if ((SR.Attr and Directory)=Directory) and (SR.name[1]<>'.') then begin
       i:=StoreDirD(RDir+SR.name+'\'); StoreWPtr(DLast,i); DLast:=i end;
    FindNext(SR) end;
end;
procedure TzFile.GetDirs(Mask:LongStrPtr);
var slRoot,sl:StringList; s:string; d,dLast:longint; l,i,j,n:word;
begin
  slRoot:=nil; l:=Mask^.LL; j:=1;
  repeat
    while (j<=l) and (Mask^.A[j] in [' ',',']) do inc(j);  n:=l-j+1;
    for i:=j to l do if Mask^.A[i] in [' ',','] then begin n:=i-j; break end;
    if n>0 then begin
      n:=minw(n,255); sl:=GetZStore(5+n); ChainLast(slRoot,sl);
      move(Mask^.A[j],sl^.s[1],n); sl^.s[0]:=char(n); inc(j,n) end;
  until n=0;
  d:=StoreDirD(''); dLast:=d;
  repeat Get1Dir(slRoot,d,dLast); d:=ReadWPtr(d) until d=0;
  SeekH(WorkHandle,WBase); WrH(WorkHandle,WPos-WBase);
end;
procedure TzFile.Reset;
var l:byte absolute CPath;
begin
  CVol:=Vol; CPath:=Path; Handle:=OpenH(_isoldfile,RdOnly); Continued:=false;
  if (HandleError=2) and Floppy then begin
    inc(CPath[l-2],5); Handle:=OpenH(_isoldfile,RdOnly); Continued:=true end;
  TestCPathError; Size:=FileSizeH(Handle); OrigSize:=Size; RunMsgOn('C',Size);
end;
procedure TzFile.Rewrite;
begin
  CVol:=Vol; CPath:=Path; Handle:=OpenH(_isoverwritefile,Exclusive);
  TestCPathError; SpaceOnDisk:=MyDiskFree(Floppy,Drive);
end;
procedure TzFile.ReadBuf2;
begin
  lBuf2:=0; iBuf2:=0; if (Size=0) and Continued then begin
    CloseH(Handle); RunMsgOff; MountVol(false); Reset end;
  if Size=0 then begin EOF2:=true; exit end;
  lBuf2:=BufSize2; if lBuf2>Size then lBuf2:=Size;
  ReadH(Handle,lBuf2,Buf2^); CPath:=Path; TestCPathError;
  dec(Size,lBuf2); RunMsgN(OrigSize-Size);
end;
procedure TzFile.WriteBuf2;
var i,n:word; l:byte absolute CPath;
begin
  i:=0; while i<lBuf2 do begin
    if SpaceOnDisk=0 then begin CloseH(Handle);
      CPath:=Path; inc(CPath[l-2],5); RenameFile56(Path,CPath,true);
      MountVol(false); Rewrite end;
    n:=lBuf2-i; if n>SpaceOnDisk then n:=SpaceOnDisk;
    WriteH(Handle,n,Buf2[i]); CPath:=Path; TestCPathError;
    inc(i,n); dec(SpaceOnDisk,n) end;
  lBuf2:=0;
end;
procedure TzFile.RdH(H:word;Skip:boolean);
var sz:longint; a:array[0..3] of char absolute sz; i,n:word;
begin
  for i:=0 to 3 do begin
    if iBuf=lBuf then ReadBuf; a[i]:=Buf[iBuf]; inc(iBuf) end;
  while sz>0 do begin
    if iBuf=lBuf then ReadBuf;
    n:=lBuf-iBuf; if sz<n then n:=sz; if not Skip then WriteH(H,n,Buf[iBuf]);
    inc(iBuf,n); dec(sz,n) end;
end;
procedure TzFile.WrH(H:word;Sz:longint);
var i:longint; n,j,max:word;
begin
  move(Sz,Buf^,4); j:=4; max:=BufSize-4; RunMsgOn('C',Sz);
  i:=0; repeat
    if Sz-i>max then n:=max else n:=Sz-i; inc(i,n);
    if n>0 then ReadH(H,n,Buf[j]); lBuf:=j+n; WriteBuf(false);
    j:=0; max:=BufSize; RunMsgN(i);
    until i=Sz;
  RunMsgOff;
end;
procedure TzFile.ProcFileList;
var d,dNext,i,n:longint; p:pointer; h:word; RDir,FName:string; skp:boolean;
label 1;
begin
  d:=0; MarkStore(p); repeat
    dNext:=ReadWPtr(d); n:=ReadWPtr(d+8); i:=d+12; RDir:=ReadWStr(i);
    SetDir(RDir);
    i:=ReadWPtr(d+4); while n>0 do begin FName:=ReadWStr(i); dec(n);
      CPath:=FExpand(FName); CVol:='';
      if IsBackup then begin
        h:=OpenH(_isoldfile,RdOnly); TestCPathError; WrH(h,FileSizeH(h)) end
      else begin
        skp:=false;
        if not OverwrOpt then begin
          h:=OpenH(_isnewfile,Exclusive); if HandleError=80 then begin
            SetMsgPar(CPath);
            if PromptYN(780) then h:=OpenH(_isoverwritefile,Exclusive)
            else begin skp:=true; goto 1 end end end
        else h:=OpenH(_isoverwritefile,Exclusive);
        TestCPathError;
1:      RdH(h,skp) end;
      CloseH(h);
      end;
    d:=dNext; ReleaseStore(p) until d=0;
end;
procedure TzFile.Backup(aMsk:LongStrPtr);
begin
  MountVol(true);
  Rewrite; InitBufOutp; GetDirs(aMsk); ProcFileList;
  WriteBuf(true);
end;
procedure TzFile.Restore;
begin
  MountVol(true); Reset; if Size=0 then CloseH(Handle) else begin InitBufInp;
    SeekH(WorkHandle,WBase); RdH(WorkHandle,false); ProcFileList; end;
  RunMsgOff;
end;

procedure BackupM(PD:InstrPtr);
var F:PzFile; er:ExitRecord; s:LongStrPtr; p:pointer;
label 1;
begin
  MarkStore(p); if PD^.IsBackup then s:=RunLongStr(PD^.bmMasks);
  new(F,Init(PD^.IsBackup,PD^.NoCompress,PD^.bmSubDir,PD^.bmOverwr,
             PD^.BrCatIRec,RunShortStr(PD^.bmDir)));
  NewExit(Ovr,er); goto 1; LastExitCode:=1;
  if PD^.IsBackup then F^.Backup(s) else F^.Restore;
  LastExitCode:=0;
1:RestoreExit(er); F^.Close; if LastExitCode<>0 then begin
     RunMsgOff; if not PD^.BrNoCancel then GoExit end;
  ReleaseStore(p);
end;

{============================================================================}
var E: EditDPtr absolute EditDRoot;

procedure XEncode(S1,S2:LongStrPtr);
var Flags,Mask,RMask,t:byte; FlagPos,SequPos,SequLen,Len,NewLen,Displ,i:word;
begin
asm  push ds; cld;
     mov cl,Timer.byte; mov t,cl; and cl,3; mov al,9cH; rol al,cl; mov RMask,al;
     lds si,S1; lodsw; mov Len,ax; mov bx,si;
     les di,S2; add di,2; mov Displ,0;
@1:  mov FlagPos,di; inc di; mov Mask,1; mov Flags,0;
@2:  cmp Len,0; je @19;
     push es; push di;
     mov ax,ds; mov es,ax; mov di,bx; mov SequLen,0;
@3:  mov cx,si; sub cx,di; cmp cx,Len; jna @4; mov cx,Len; {min(rest,Len)}
@4:  cmp cx,2; jna @15;
     push si; push di; mov dx,cx;
     repe cmpsb; jz @5; inc cx;
@5:  pop di; pop si;
     sub dx,cx; cmp dx,SequLen; jna @7; mov SequPos,di;
     cmp dx,255; jna @6; mov dx,255;
@6:  mov SequLen,dx;
@7:  inc di; jmp @3;
@15: pop di; pop es;
     cmp SequLen,2; ja @16;
     lodsb; rol RMask,1; xor al,RMask; stosb; dec Len; jmp @17;
@16: mov ax,SequLen; stosb; mov ax,SequPos; sub ax,S1.word; stosw;
     mov al,Mask; or Flags,al;
     mov ax,Len; sub ax,SequLen; mov Len,ax; add si,SequLen;
@17: mov ax,si; sub ax,S1.word; mov dx,di; sub dx,S2.word;
     cmp ax,dx; jna @18; sub ax,dx;
     cmp ax,Displ; jna @18; mov Displ,ax;
@18: shl Mask,1; cmp Mask,0; jne @2;
     push di; mov di,FlagPos; mov al,Flags; stosb; pop di; jmp @1;
@19: cmp Mask,1; jne @20; dec di; jmp @21;
@20: push di; mov di,FlagPos; mov al,Flags; stosb; pop di;
@21: lds si,S2; mov ax,di; sub ax,si; mov cx,ax; sub cx,2; inc ax; {NewLen+3}
     add ax,Displ; mov ds:[si],ax; {LL=Displ+NewLen+3}
     inc si; add si,cx; mov di,si; add di,Displ; std; rep movsb; {Displ nach rechts}
     mov di,S2.word; add di,[di]; dec di; cld; mov al,t; stosb;
     mov ax,Displ; xor ax,0ccccH; stosw; {t+Displ stored at end}
     pop ds end;
   for i:=1 to Displ do S2^.A[i]:=char(Random(255));
end;

{$ifndef FandRunV}

procedure CompressCRdb;
var s:string; p,cr:pointer;
begin
  MarkStore(p); cr:=Chpt^.RecPtr;
  s:='#I1_'+Chpt^.Name+'#O1_'+Chpt^.Name; SetInpStr(s);
  SpecFDNameAllowed:=true; ReadMerge; SpecFDNameAllowed:=false;
  RunMerge; SaveFiles; ReleaseStore(p); Chpt^.RecPtr:=cr;
  CFile:=Chpt; CRecPtr:=E^.NewRecPtr; ReadRec(CRec); with ChptTF^ do begin
    CompileAll:=false; CompileProc:=false; SetUpdHandle(Handle) end;
end;
procedure CodingCRdb(Rotate:boolean);
procedure CompressTxt(IRec:word;s:LongStrPtr;Typ:char);
var l:word; ss:LongStrPtr;
procedure Wr(c:char);
  begin if l>=MaxLStrLen then RunError(661); inc(l); ss^.A[l]:=c end;
var i,n:word; b:boolean; ci:CompInpDPtr; cr:pointer; s2:LongStrPtr;
    p2:pointer;
label 0,1,2;
begin
  InpArrLen:=s^.LL; InpArrPtr:=CharArrPtr(@s^.A); PrevCompInp:=nil;
  if InpArrLen=0 then ForwChar:=^z else ForwChar:=InpArrPtr^[1];
  CurrPos:=1; SwitchLevel:=0; cr:=CRecPtr;
  ss:=GetStore(MaxLStrLen+2); MarkStore2(p2); l:=0;
  if Typ='E' then begin
0:  while not(ForwChar in ['#',^z,^m,'{']) do begin   { read headlines }
      Wr(ForwChar);ReadChar end;
    case ForwChar of
      ^z,'#': goto 1;
      '{':begin SkipBlank(true); Wr('{'); Wr('}') end;
      else ReadChar; if ForwChar=^j then ReadChar end;
    Wr(^m); Wr(^j); goto 0 end;
1:case ForwChar of
   ^z    :if PrevCompInp<>nil then begin
            ci:=PrevCompInp;move(ci^.ChainBack,PrevCompInp,sizeof(CompInpD));
            if CurrPos<=InpArrLen then ForwChar:=InpArrPtr^[CurrPos];
            goto 1 end
          else begin s^.LL:=l; MyMove(ss^.A,s^.A,l); ReleaseAfterLongStr(s);
            ReleaseStore2(p2); CRecPtr:=cr; exit end;
   '{'   : begin ReadChar;
              if ForwChar='$' then begin
                n:=RdDirective(b); case n of
                  0:;
                  1:begin inc(SwitchLevel); if not b then SkipLevel(true) end;
                  5:begin ci:=GetStore2(sizeof(CompInpD));
                      move(PrevCompInp,ci^,sizeof(CompInpD));
                      PrevCompInp:=ci; SetInpTT(ChptIPos,true) end;
                 else if n=3 then SkipLevel(false) else dec(SwitchLevel) end;
                goto 1 end
              else begin n:=1;
2:              case ForwChar of
                 '{': inc(n);
                 '}': begin dec(n); if n=0 then begin
                        Wr(' '); ReadChar; goto 1 end
                       end end;
                ReadChar; goto 2 end end;
   ''''   : begin repeat Wr(ForwChar); ReadChar until ForwChar in ['''',^z];
                  if ForwChar=^z then goto 1 end;
   ^@..' ': if not (Typ in ['R','U','E','H']) then begin
              while (ForwChar<=' ') and (ForwChar<>^z) do ReadChar;
              Wr(' '); goto 1 end;
  end;
  Wr(ForwChar); ReadChar; goto 1;
end;
var posUDLI:word;
procedure CodeF(IRec:word;F:FieldDPtr;Typ:char);
var s,s2:LongStrPtr; sofs:word absolute s;
    pos:longint; p,p2:pointer; i,l:word; b:boolean;
label 1,2;
begin
  pos:=_t(F); if pos=0 then exit; MarkBoth(p,p2);
  s:=ChptTF^.Read(1,pos); l:=s^.LL; ChptTF^.Delete(pos); if l=0 then goto 2;
  if Rotate then begin
    if F=ChptOldTxt then begin
      if Typ='F' then FileDPtr(@s^.A)^.TxtPosUDLI:=posUDLI end
    else begin
      CompressTxt(IRec,s,Typ); l:=s^.LL;
      if l=0 then goto 2; if l>MaxLStrLen then RunError(661);
      if Typ='F' then begin posUDLI:=0; b:=true;
        for i:=1 to l-1 do case s^.A[i] of
          '#':if b and (s^.A[i+1] in ['U','D','L','I']) then begin
                posUDLI:=i; goto 1 end;
          '''': b:= not b end;
1:    end end;
    s2:=GetStore($fffe); XEncode(s,s2); s:=s2 end
  else Code(S^.A,l);
2:longs_(F,s); ReleaseBoth(p,p2);
end;
var i,irec,pos:word; cf:FileDPtr; cr:pointer; s:string[1]; compileAll:boolean;
begin
  cf:=CFile; cr:=CRecPtr; CFile:=Chpt; CRecPtr:=GetRecSpace;
  RunMsgOn('C',CFile^.NRecs);
  irec:=ChptTF^.IRec; compileAll:=ChptTF^.CompileAll;
  for i:=1 to CFile^.NRecs do begin
    ReadRec(i); RunMsgN(i); s:=_shorts(ChptTyp); SetMsgPar(_shorts(ChptName));
    if Rotate and (s[1] in [' ','I']) then else begin
      CodeF(i,ChptTxt,s[1]); CodeF(i,ChptOldTxt,s[1]); WriteRec(i) end end;
  if Rotate then begin i:=1; while i<=CFile^.NRecs do begin
    ReadRec(i); s:=_shorts(ChptTyp);
    if s[1] in [' ','I'] then DeleteRec(i) else inc(i) end end;
  RunMsgOff; ReleaseStore(CRecPtr); CFile:=cf; CRecPtr:=cr;
  CompressCRdb;
  ChptTF^.IRec:=irec; ChptTF^.CompileAll:=compileAll;
end;
function PromptCodeRdb:boolean;
procedure AddLicNr(F:FieldDPtr);
  begin if _t(F)<>0 then t_(F,_t(F)+UserLicNr) end;
procedure CopyH(H:word;const Nm:string);
var sz:longint; buf:pointer; n,h2:word;
begin
  buf:=GetStore(MaxLStrLen); CPath:=Nm; CVol:='';
  h2:=OpenH(_isoverwritefile,Exclusive); sz:=FileSizeH(H); SeekH(H,0);
  while sz>0 do begin
    if sz>MaxLStrLen then n:=MaxLStrLen else n:=sz; dec(sz,n);
    ReadH(H,n,buf^); WriteH(h2,n,buf^) end;
  CloseH(h2); ReleaseStore(buf);
end;
var i:word; b:boolean; cf:FileDPtr; cr:pointer; s:string;
label 1,2;
begin
  SetPassWord(Chpt,1,''); SetPassWord(Chpt,2,''); PromptCodeRdb:=true;
  {$ifndef FandDemo}F10SpecKey:=_AltF10_;{$endif}
  b:=PromptYN(133);                                      {$ifndef FandDemo}
  if KbdChar=_AltF10_ then begin
    F10SpecKey:=_ESC_; b:=PromptYN(147); if KbdChar=_ESC_ then goto 1;
    if b then begin
      CFile:=Chpt; WrPrefixes; SaveCache(0);
      s:=CRdb^.RdbDir; AddBackSlash(s); s:=s+Chpt^.Name;
      CopyH(Chpt^.Handle,s+'.RD$'); CopyH(ChptTF^.Handle,s+'.TT$') end;
    CodingCRdb(true); ChptTF^.LicenseNr:=UserLicNr;
    cf:=CFile; cr:=CRecPtr; CFile:=Chpt; CRecPtr:=GetRecSpace;
    for i:=1 to Chpt^.NRecs do begin
      ReadRec(i);AddLicNr(ChptOldTxt);AddLicNr(ChptTxt);WriteRec(i) end;
    ReleaseStore(CRecPtr); CFile:=cf; CRecPtr:=cr; exit end;  {$endif}
  if b then begin
    SetPassWord(Chpt,1,Password(true));
    if HasPassWord(Chpt,1,'') then goto 1;
    CodingCRdb(false) end
  else begin
1:  CompressCRdb; PromptCodeRdb:=false end;
end;
{$endif}

procedure CheckFile(FD:FileDPtr);
var Prfx:record NRecs:longint;RecLen:word end;
    h:word; d:DirStr; n:NameStr; e:ExtStr; fs:longint;
begin
  TestMountVol(CPath[1]); h:=OpenH(_isoldfile,RdShared);
  LastExitCode:=0; if HandleError<>0 then begin
    if HandleError=2 then LastExitCode:=1 else LastExitCode:=2; exit end;
  ReadH(h,6,Prfx); fs:=FileSizeH(h); CloseH(h);
  if (FD^.RecLen<>Prfx.RecLen) or (Prfx.NRecs<0) and (FD^.Typ<>'X') or
     ((fs-FD^.FrstDispl) div Prfx.RecLen<Prfx.NRecs) or
     (Prfx.NRecs>0) and (FD^.Typ='X') then begin LastExitCode:=3; exit end;
  if FD^.TF=nil then exit;
  FSplit(CPath,d,n,e); if SEquUpcase(e,'.RDB') then e:='.TTT' else e[2]:='T';
  CPath:=d+n+e;
  h:=OpenH(_isoldfile,RdShared);
  if HandleError=0 then CloseH(h) else LastExitCode:=4;
end;

end.