{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+,D+}

unit runfand;

interface

procedure ScrGraphMode(Redraw:boolean;OldScrSeg:word);
function ScrTextMode(Redraw,Switch:boolean):word;

procedure InitRunFand;

implementation

uses dos,graph,
     drivers,base,obase,obaseww,access,oaccess,rdrun,
{$ifdef FandSQL} channel, {$endif}
{$ifdef FandDML}  dml,  {$endif}
     wwmenu,wwmix,compile,rdfildcl,editor,runedi,runproj,runfrml;


procedure Ovr; far;
assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push cs; push ax;
end;


procedure ScrGraphMode(Redraw:boolean;OldScrSeg:word);
var p,p1:pointer; pofs:word absolute p;
    sz,cr,i:word; n,err:integer; b:boolean; s:string[4];
begin
  if (VideoCard=viCga) or (TxtCols<>80) or (TxtRows<>25) then RunError(643);
  DoneMouseEvents; b:=Crs.Enabled; CrsHide; n:=80*25*2;
  if OldScrSeg<>0 then p:=ptr(OldScrSeg,0)
  else begin AlignParagraph; p:=GetStore(n) end;
  Move(ptr(ScrSeg,0)^,p^,n);
  if OldScrSeg<>0 then SetGraphMode(GraphMode)
  else begin
    if (VideoCard=viVga) and (GraphDriver<>EGAMono) then begin
     if BGIReload then
     if Fonts.VFont=foLatin2 then ResFile.Get(Vga8x19L,FontArr) else ResFile.Get(Vga8x19K,FontArr);
     GrBytesPerChar:=19 end
    else begin
      if BGIReload then
      if Fonts.VFont=foLatin2 then ResFile.Get(Ega8x14L,FontArr) else ResFile.Get(Ega8x14K,FontArr);
      GrBytesPerChar:=14 end;
    GrBytesPerLine:=80*GrBytesPerChar;
    if VideoCard=viHercules then n:=BgiHerc else n:=BgiEgaVga;
    if BGIReload then ResFile.Get(n,BGIDriver);
    if RegisterBGIDriver(BGIDriver)<0 then RunError(838);
    GetMem(p1,7000); FreeMem(p1,7000); FreeList:=HeapPtr;
    InitGraph(GraphDriver,GraphMode,''); FreeList:=nil;
    err:=GraphResult;
    if err<>grOK then begin str(err:3,s); SetMsgPar(s); RunError(201) end;
    end;
  IsGraphMode:=true; ScrSeg:=seg(p^); CrsIntrInit;
  if Redraw then ScrPopToGraph(0,0,TxtCols,TxtRows,p,TxtCols);
  InitMouseEvents; if b then CrsShow;
  if BGIReload then
  if Fonts.VFont=foLatin2 then begin
    ResFile.Get(ChrLittLat,BGILittFont); ResFile.Get(ChrTripLat,BGITripFont) end
  else begin
    ResFile.Get(ChrLittKam,BGILittFont); ResFile.Get(ChrTripKam,BGITripFont) end;
  if RegisterBGIFont(BGILittFont)<0 then RunError(838);
  if RegisterBGIFont(BGITripFont)<0 then RunError(838);
end;

function ScrTextMode(Redraw,Switch:boolean):word;
var b:boolean;
begin
  if not IsGraphMode then exit;
  DoneMouseEvents;
  b:=Crs.Enabled; CrsHide; CrsIntrDone;
  if Switch then RestoreCrtMode else CloseGraph;
  IsGraphMode:=false;
  LoadVideoFont;
  if Redraw then Move(ptr(ScrSeg,0)^,ptr(video.Address,0)^,80*25*2);
  ScrTextMode:=ScrSeg; ScrSeg:=video.Address;
  InitMouseEvents;
  CrsShow{is visible}; if not b then CrsHide;
end;


{ ********* Init FAND ***************** }

function IsAT: boolean; assembler;
asm  mov  ax,0f000H
     mov  es,ax
     mov  ax,1
     cmp  Byte Ptr es:[0fffeH],0fcH
     je   @1
     xor  ax,ax
@1:
end;

procedure OpenXMS;
var N:word;
begin
  XMSCachePages:=0; if not IsAT then exit;
  asm mov ax,4300H; int 2fH; cmp al,80H; jne @2;           { XMM present }
      mov ax,4310H; int 2fH;                               { get XMSFun  }
      mov XMSFun.word,bx; mov XMSFun[2].word,es;
      mov ah,8; call [XMSFun]; cmp ax,64; jna @2;
      sub ax,64; mov dx,ax;              { maxavail - 64kB /DOS 4.1 error/}
      cmp dx,spec.XMSMaxKb; jna @1; mov dx,spec.XMSMaxKb; @1:mov N,dx;
      mov ah,9; call [XMSFun]; or ax,ax; je @2; mov XMSHandle,dx; { getmem }
      mov ax,N; mov cl,CachePageShft; sub cl,10; shr ax,cl;
      mov XMSCachePages,ax; mov bl,0;
  @2: mov XMSError,bl end;
  with XMSOut do begin
    Len:=CachePageSize; SHandle:=0; DHandle:=XMSHandle; Dest:=nil end;
  with XMSIn do begin
    Len:=CachePageSize; SHandle:=XMSHandle; Src:=nil; DHandle:=0 end;
end;

procedure OpenCache;
var Z:XMSCacheDPtr; i:word; a,b:longint;
begin
  CachePageSize:=1 shl CachePageShft;
  CachePageSz:=(CachePageSize div 16)+1; { size in paragraphs }
  InitStackSz:=2*CachePageSz; MinStackSz:=4*CachePageSz;
  InitStack2Sz:=CachePageSz; MinStack2Sz:=CachePageSz;
  XMSCacheRoot:=nil;
  OpenXMS;
  if XMSCachePages>0 then begin
    a:=AbsAdr(HeapPtr); b:=AbsAdr(HeapEnd);
    XMSCacheBuf:=HeapPtr; inc(a,CachePageSize);
    for i:=1 to XMSCachePages do begin
      Z:=Normalize(a); inc(a,sizeof(XMSCacheD)); if a>b then RunError(624);
      Z^.Chain:=XMSCacheRoot; XMSCacheRoot:=Z;
      Z^.XMSPage:=i-1; Z^.Handle:=$FF; Z^.Upd:=false end;
    HeapPtr:=Normalize((a+$F) and $FFFFFFF0) end;
  MyHeapOrg:=HeapPtr; MemEnd:=HeapEnd;
  FormatCache;
  HeapError:=@HeapErrFun; if NCachePages=0 then RunError(624);
end;


procedure DetectVideoCard; assembler;
asm  push bp;
     mov ah,8; xor bh,bh; int 10H; mov al,ah; and al,7fH;
     mov StartAttr,al; mov TextAttr,al;            { get attr at cursor pos }
     mov ah,0fH; int 10H; mov StartMode,al;  { get al=video mode, ah=TxtCols }
     mov al,ah; xor ah,ah; mov TxtCols,ax; dec al; mov WindMax.X,al;
     mov cl,14; mov dl,25; mov VideoCard,viCGA;
     cmp GraphDriver,CGA; je @1; cmp GraphDriver,MCGA; je @1;
     mov VideoCard,viHercules; mov ScrGrSeg,0b000H;
     cmp GraphDriver,HercMono; je @1;
     mov VideoCard,viEga; mov ScrGrSeg,0a000H;
     mov ax,1130H; mov bh,2; int 10H; inc dl;  {   cx=bytesperchar,dl=rows }
     cmp cl,16; jne @1; mov VideoCard,viVga;
@1:  mov BytesPerChar,cl;
     xor dh,dh; mov TxtRows,dx; dec dl; mov WindMax.Y,dl;
     xor ax,ax; mov WindMin,ax; mov NrVFont,al; mov IsGraphMode,al;
     mov ah,3; mov bh,0; int 10H;           { get cursor position }
     xor ah,ah; mov al,dl; mov Crs.X,ax; mov al,dh; mov Crs.Y,ax;
     mov cx,2000H; mov ah,1; int 10H;       { cursor off }
     mov ScrSeg,0b000H; mov ChkSnow,0; cmp StartMode,7;{Mono} je @2;
     mov ScrSeg,0b800H; cmp VideoCard,viEga; jnb @2; mov ChkSnow,1;
@2:  pop bp end;

procedure InitDrivers;
var  kl : Char;
begin
  BreakIntrInit;
  DetectGraph(GraphDriver,GraphMode);
  { GraphDriver:=EGAMono; Mark***}
  DetectVideoCard;
  AssignCrt(Output); Rewrite(Output); ClrEvent;
end;
procedure InitAccess;
begin
  ResetCompilePars; SpecFDNameAllowed:=false;
  fillchar(XWork,sizeof(XWork),0);
end;


{ ================================================================== }

procedure RdCFG;
var CfgHandle:word;
procedure RdColors;
var typ:word;
begin
  if StartMode=7 then typ:=1 else if VideoCard>=viEga then typ:=3 else typ:=2;
  SeekH(CfgHandle,PosH(CfgHandle)+(sizeof(video)+sizeof(colors))*(typ-1));
  ReadH(CfgHandle,sizeof(video),video);
  ReadH(CfgHandle,sizeof(colors),colors);
  SeekH(CfgHandle,PosH(CfgHandle)+(sizeof(video)+sizeof(colors))*(3-typ));
end;
procedure RdPrinter;
const NPrintStrg=32;
var l:byte; i,j,n:word; p:BytePtr; off:word absolute p;
    A:array[0..NPrintStrg*256]of byte;
label 1;
begin
  ReadH(CfgHandle,1,prMax);
  for j:=1 to prMax do begin
    p:=BytePtr(@A); n:=0; for i:=0 to NPrintStrg do begin
      ReadH(CfgHandle,1,l); if l=$FF then goto 1; p^:=l; inc(off);
      ReadH(CfgHandle,l,p^); inc(off,l); inc(n,l+1) end;
    ReadH(CfgHandle,1,l); if l<>$FF then begin
  1:  writeln('Invalid FAND.CFG');wait;halt end;
    with printer[j-1] do begin
      GetMem(Strg,n); Move(A,Strg^,n); ReadH(CfgHandle,4,Typ);
      OpCls:=false; ToHandle:=false; ToMgr:=false;
      if TmOut=255 then begin OpCls:=true; TmOut:=0 end
      else if TmOut=254 then begin ToHandle:=true; TmOut:=0 end
      else if TmOut=253 then begin ToMgr:=true; TmOut:=0 end end end;
   SetCurrPrinter(0);
end;
procedure RdWDaysTab;
begin
  ReadH(CfgHandle,sizeof(NWDaysTab),NWDaysTab);
  ReadH(CfgHandle,sizeof(WDaysFirst),WDaysFirst);
  ReadH(CfgHandle,sizeof(WDaysLast),WDaysLast);
  GetMem(WDaysTab,NWDaysTab*3);
  ReadH(CfgHandle,NWDaysTab*3,WDaysTab^);
end;
var ver:array[1..4]of char;
begin
  CVol:=''; CPath:=MyFExpand('FAND.CFG','FANDCFG');
  CfgHandle:=OpenH(_isoldfile,RdOnly);
  if HandleError<>0 then begin write(CPath+' not found');wait;halt end;
  ReadH(CfgHandle,4,ver);
  if ver<>CfgVersion then begin
    write('Invalid version of FAND.CFG');wait;halt end;
  ReadH(CfgHandle,sizeof(spec),spec);
  RdColors;
  ReadH(CfgHandle,sizeof(Fonts),Fonts);
  ReadH(CfgHandle,sizeof(CharOrdTab),CharOrdTab);
  ReadH(CfgHandle,sizeof(UpcCharTab),UpcCharTab);
  RdPrinter;
  RdWDaysTab;
  CloseH(CfgHandle);
end;
procedure CompileHelpCatDcl;
  var s:string; p2:pointer;
  begin FileDRoot:=nil; CRdb:=nil; MarkStore2(p2);
     RdMsg(56); s:=MsgLine; SetInpStr(s);
     {$ifdef FandRunV} RdFileD('UFANDHLP','6','');
     {$else}  RdFileD('FANDHLP','6',''); {$endif}
     HelpFD:=CFile;
     RdMsg(52); s:=MsgLine; SetInpStr(s); RdFileD('Catalog','C','');
     CatFD:=CFile; FileDRoot:=nil;
     CatRdbName:=CatFD^.FldD;CatFileName:=CatRdbName^.Chain;
     CatArchiv:=CatFileName^.Chain;
     CatPathName:=CatArchiv^.Chain;CatVolume:=CatPathName^.Chain;
     MarkStore(AfterCatFD); ReleaseStore2(p2) end;


{ **************  Run FAND  ************************************** }

function SetTopDir(p:PathStr;var n:NameStr):boolean;
var e:ExtStr; er:ExitRecord;
label 1;
begin
  SetTopDir:=false; FSplit(FExpand(p),TopRdbDir,n,e);
  if not IsIdentifStr(n) then begin WrLLF10Msg(881); exit end;
  EditDRoot:=nil; LinkDRoot:=nil; FuncDRoot:=nil;
  TopDataDir:=getenv('FANDDATA');
  DelBackSlash(TopRdbDir); DelBackSlash(TopDataDir);
  if TopDataDir<>'' then TopDataDir:=FExpand(TopDataDir);
  {$I-} ChDir(TopRdbDir);{$I+}
  if IOResult<>0 then begin SetMsgPar(p); WrLLF10Msg(703); exit end;
  CatFDName:=n; NewExit(Ovr,er); goto 1;
  CFile:=CatFD; OpenF(Exclusive); SetTopDir:=true;
1:RestoreExit(er);
end;
procedure RunRdb(p:PathStr);
  var n:NameStr;
  begin if (p<>'') and SetTopDir(p,n) then
        begin EditExecRdb(n,'main',nil); CFile:=CatFD; CloseFile end end;
procedure SelectRunRdb(OnFace:boolean);
  var p:PathStr;
  begin p:=SelectDiskFile('.RDB',34,OnFace); RunRdb(p) end;
procedure CallInstallRdb;
  var p:PathStr; n:NameStr;
  begin p:=SelectDiskFile('.RDB',35,true); if (p<>'') and SetTopDir(p,n) then
        begin InstallRdb(n);CFile:=CatFD;CloseFile end end;

procedure CallEditTxt;
begin CPath:=FExpand(CPath);CVol:='';
      EditTxtFile(nil,'T','',nil,1,0,nil,0,'',0,nil) end;
procedure SelectEditTxt(E:ExtStr;OnFace:boolean);
begin CPath:=SelectDiskFile(E,35,OnFace); if CPath='' then exit; CallEditTxt end;



procedure InitRunFand;
var n,h,l,err,hourmin:word; s:string; nb,sec:byte;
    er:ExitRecord; i,j,MsgNr:integer; mb:PMenuBoxS;  w:longint;
    p:pointer; x:StringPtr; xofs:word absolute x;  txt:string[16];
    r:real;
label 0,1,2;
begin
  InitDrivers; WasInitDrivers:=true; InitAccess;
{$ifdef FandDML}  InitDML;  {$endif}
  NewExit(Ovr,er); exit;
  inc(StackLimit,256);
  OldPrTimeOut:=PrTimeOut;
  CallOpenFandFiles:=OpenFandFiles; CallCloseFandFiles:=CloseFandFiles;
  video.CursOn:=$0607;{if exit before reading .CFG}
  KbdBuffer[0]:=#0; F10SpecKey:=0;
  if getenv('DMLADDR')<>'' then begin
    writeln('type ''exit'' to return to FAND'); wait; halt end;
  WrkDir:=getenv('FANDWORK'); if WrkDir='' then WrkDir:=FandDir;
  AddBackSlash(WrkDir);
  s:=WrkDir+'FANDWORK'; FandWorkName:=s+'.$$$';
  FandWorkXName:=s+'.X$$'; FandWorkTName:=s+'.T$$';
  LANNode:=0; s:=getenv('LANNODE');
    s:=TrailChar(' ',s);
    if s<>'' then begin
    val(s,nb,err); {$ifndef FandRunV} if nb<=3 then {$endif}
    if err=0 then LANNode:=nb end;
  h:=ResFile.Handle; ReadH(h,2,n); if n<>ResVersion then begin
    writeln('FAND.RES incorr. version');wait;halt end;
  OvrHandle:=h-1;
  ReadH(h,sizeof(ResFile.A),ResFile.A); ReadH(h,2,MsgIdxN);
  l:=sizeof(TMsgIdxItem)*MsgIdxN; GetMem(MsgIdx,l); ReadH(h,l,MsgIdx^);
  FrstMsgPos:=PosH(h);
  RdMsg(50); Move(MsgLine[1],AbbrYes,2);
  RdCFG;
  ProcAttr:=colors.uNorm;
  ScrSeg:=video.Address; if Video.TxtRows<>0 then TxtRows:=Video.TxtRows;
  if Fonts.LoadVideoAllowed and (VideoCard>=viEga) then case BytesPerChar of
    14:if Fonts.VFont=foKamen then NrVFont:=Ega8x14K else NrVFont:=Ega8x14L;
    16:if Fonts.VFont=foKamen then NrVFont:=Vga8x16K else NrVFont:=Vga8x16L end;

{}
{Access}
  GetIntVec($3f,FandInt3f);
  FillChar(XWork,sizeof(XWork),0); FillChar(TWork,sizeof(TWork),0);
  CRdb:=nil;
  for i:=1 to FloppyDrives do MountedVol[i]:='';
{Ww}
  ss.Empty:=true; ss.Pointto:=nil;
  TxtEdCtrlUBrk:=false; TxtEdCtrlF4Brk:=false;
  InitMouseEvents;
{Editor}
  InitTxtEditor;
{}
  OpenCache;

  WasInitPgm:=true;
  CompileHelpCatDcl;
  LoadVideoFont;
  if (VideoCard=viHercules) and Fonts.LoadVideoAllowed then begin
    ScrGraphMode(false,0); BGIReload:=false end;


  OpenWorkH; OpenFandFiles(false);


  if (paramstr(1)<>'') and (paramstr(1)<>'?') then begin
{$ifndef FandRunV}
    if SEquUpcase(paramstr(2),'D') then begin IsTestRun:=true; goto 0 end
    else
{$endif}
    if SEquUpcase(paramstr(2),'T') then begin CPath:=paramstr(1);
      if copy(CPath,1,2)='*.' then SelectEditTxt(copy(CPath,2,4),false)
      else CallEditTxt; exit end
    else begin
0:    if copy(paramstr(1),1,2)='*.' then SelectRunRdb(false)
      else RunRdb(paramstr(1));
      if IsTestRun then IsTestRun:=false else exit end end;

  TextAttr:=colors.DeskTopColor; window(1,1,TxtCols,TxtRows-1);
  WriteWFrame(WHasFrame+WDoubleFrame,'','');
  ScrClr(1,1,TxtCols-2,TxtRows-13,#$b1,TextAttr);
  ScrClr(1,TxtRows-12,TxtCols-2,10,#$b2,TextAttr);
  ResFile.Get(FandFace,p); x:=p; inc(xofs);
  for i:=-11 to -6 do begin
    x^[0]:=char(TxtCols-2); ScrWrStr(1,TxtRows+i,x^,TextAttr); inc(xofs,82) end;
  TextAttr:=colors.mHiLi; ScrClr(3,TxtRows-4,TxtCols-6,1,' ',TextAttr);
  {$ifdef Trial} RdMsg(70);
  {$else}{$ifdef FandRunV} RdMsg(42);
  {$else}{$ifdef FandDemo} RdMsg(43);
  {$else} RdMsg(41);
  {$endif} {$endif} {$endif}
  txt[0]:=#0;
  {$ifdef FandNetV} txt:='LAN,'; {$endif}
  {$ifdef FandSQL} txt:='SQL,'; {$endif}
  {$ifndef FandGraph} txt:=txt+'~GRAPH,'; {$endif}
  {$ifndef FandProlog} txt:=txt+'~PRL,'; {$endif}
  {$ifndef FandDML} txt:=txt+'~DML,'; {$endif}
  {$ifdef Coproc} txt:=txt+'COPROC,'; {$endif}
  {$ifdef FandTest} {if txt='' then txt:='test' else} txt:='test,'+txt; {$endif}
  {$ifdef FandAng} txt:=txt+'En '; {$endif}
  if txt<>'' then begin txt[length(txt)]:=')'; MsgLine:=MsgLine+'x ('+txt end
  else MsgLine:=MsgLine+'x';
  gotoxy(5,TxtRows-3); write(MsgLine);
  {$ifdef FandRunV} {$ifndef FandNetV}
  goto 2;
  {$endif} {$endif}
{$ifndef FandDemo}
  if TxtCols>=80 then begin
    RdMsg(40); gotoxy(51,TxtRows-3); write(MsgLine,UserLicNrShow:7) end;
{$endif}
2:ReleaseStore(p);
  MsgNr:=2;

{$ifdef FandRunV} MsgNr:=14; {$endif}

{ ifdef FandDemo
  if today>731215.0 then WrLLF10Msg(47);
 $endif }

{$ifdef FandTest}
  if today>730210.0 then begin WrLLF10Msg(47); {exit;} end;
{$endif}


{ if today>730177.0 then begin WrLLF10Msg(47);  exit; end;}

  RdMsg(MsgNr); New(mb,Init(4,3,StringPtr(@MsgLine))); i:=1;
1:i:=mb^.Exec(i); j:=i;
{$ifdef FandRunV} if j<>0 then inc(j); {$endif}
  w:=PushW(1,1,TxtCols,TxtRows);
  case j of
   1:begin IsTestRun:=true; SelectRunRdb(true); IsTestRun:=false end;
   2:begin SelectRunRdb(true); IsTestRun:=false end;
   3:begin IsInstallRun:=true; CallInstallRdb; IsInstallRun:=false end;
   4:SelectEditTxt('.TXT',true);
   5:OsShell('','',false,true,true,true);
   0,6:begin CloseH(WorkHandle); CloseFandFiles(false); exit end;
  end;
  PopW(w);
  goto 1;
end;

end.
