{$I switches}
{$A-,B-,F+,G-,I+,O-,R-,S+,V+,X+,L+}
unit BASE;

interface

uses dos,graph,overlay,drivers;

                      { COMMON }

const UserLicNrShow:longint=999001 {160188 } ;
      Version:string[4]='4.20';  FDVersion=$0411;
      ResVersion=$0420;  CfgVersion:string[4]='4.20';
      DMLVersion=41;

{$ifdef Coproc} type float=double; const HasCoproc=true;
{$else} type float=real; const HasCoproc=false;{$endif}

type string2=string[2]; string3=string[3]; string4=string[4];
     string8=string[8]; string9=string[9]; string12=string[12];
     string20=string[20]; string127=string[127]; string255=string[255];
     VolStr=string[11]; ScreenStr=string[80]; IdentStr=string[32];
     CharArr=array[1..50]of char; CharArrPtr=^CharArr;
     LongStr=record LL:word; A:CharArr end;
     LongStrPtr=^LongStr;
     StringPtr=^string255; PString=StringPtr;
     StringPtrArr=array[1..10] of StringPtr;
     BytePtr=^byte;CharPtr=^char;BooleanPtr=^boolean;WordPtr=^word;
     IntegerPtr=^integer;LongintPtr=^longint;FloatPtr=^float;
     RealPtr=^real;
     PtrPointer=^pointer;
     PProcedure = procedure;
     WRectPtr = ^WRect;
     WRect = record C1,R1,C2,R2: byte end;
{ Type conversion records }
     WordRec = record Lo, Hi: Byte end;
     LongRec = record Lo, Hi: Word end;
     PtrRec  = record Ofs, Seg: Word end;

const EmptyStr:string[1]='';
      MaxLStrLen = 65000;
  WShadow    = $01;             { window flags }
  WNoClrScr  = $02;
  WPushPixel = $04;
  WNoPop     = $08;
  WHasFrame  = $10;
  WDoubleFrame=$20;


procedure MyMove(var a1,a2; n:word);
function IsLetter(C:char):boolean;
function IsDigit(C:char):boolean;
procedure ChainLast(var Frst;New:pointer);
function LastInChain(var Frst):pointer;
function ListLength(P:pointer):word;
function ListAt(P:pointer;I:word):pointer;
procedure ReplaceChar(S:string;C1,C2:char);
 function SEquUpcase(S1,S2:string):boolean;
 function StrPas(Src:PChar):string;
procedure StrLPCopy(Dest:PChar;s:string;MaxL:word);
function SLeadEqu(S1,S2:string):word;
 function EqualsMask(p:pointer;l:word;Mask:string): boolean;
 function EquLongStr(S1,S2:LongStrPtr):boolean;
 function EquArea(P1,P2:pointer;L:word):boolean;
 function MinI(X, Y: Integer): Integer;
 function MaxI(X, Y: Integer): Integer;
 function MinW(X, Y: Word): Word;
 function MaxW(X, Y: Word): Word;
 function MinL(X, Y: longint): longint;
 function MaxL(X, Y: longint): longint;
function SwapLong(N:longint):longint;
procedure ExChange(X,Y:pointer; L:word);
function OverlapByteStr(p1,p2:pointer):boolean;
function CountDLines(var Buf;L:word;C:char):word;
function GetDLine(var Buf;L:word;C:char;I:word):string;
function FindCtrlM(s:LongStrPtr;i,n:word):word;
function SkipCtrlMJ(s:LongStrPtr;i:word):word;
procedure AddBackSlash(S:string);
procedure DelBackSlash(S:string);
function MouseInRect(X,Y,XSize,YSize:word):boolean;
function MouseInRectProc(X,Y,XSize,YSize:word):boolean;
function HexStrToLong(S:string):longint;
               { *** time , date *** }

procedure SplitDate(R:float;var d,m,y:word);
function RDate (Y,M,D,hh,mm,ss,tt:word) :float;
function AddMonth(R,RM:float):float;
function DifMonth(R1,R2:float):float;
function ValDate(const Txt:string;Mask:string):float;
function StrDate(R:float;Mask:string):string;
function Today :float;
function CurrTime :float;

               { ***  debugging *** }
procedure wait;
{$ifndef FandRunV}
function HexB(b:byte):string2;
function HexW(i:word):string4;
function HexD(i:LongInt):string8;
function HexPtr(p:pointer):string9;
procedure DispH(var ad;NoBytes:integer);
{$endif}


                  { MEMORY MANAGEMENT }

var
  MyHeapOrg,AfterCatFD,{HeapPtr,HeapEnd,} CacheEnd,Stack2Ptr,MemEnd:pointer;
   CachePageSize:word;{4kB}
type
  XMSParam= record
    Len:longint;
    SHandle:word; Src:pointer;
    DHandle:word; Dest:pointer;
  end;
var
  XMSFun:pointer; XMSHandle:word; XMSError:byte;
  XMSOut,XMSIn:XMSParam;

const
  NCachePages:word=0;
  XMSCachePages:word=0;
  CachePageArrOfs=9;
  CachePageShft:byte=12;

type
  CachePagePtr = ^CachePage;
  CachePage = record
    Chain:CachePagePtr;
    case byte of
      1:(Pg3:array[1..3]of byte;
         Handle:byte);
      2:(HPage:longint;
         Upd:boolean;
         Arr:array[0..4095]of byte);
  end;
  XMSCacheDPtr = ^XMSCacheD;
  XMSCacheD = record
    Chain:XMSCacheDPtr;
    case byte of
      1:(Pg3:array[1..3]of byte;
         Handle:byte);
      2:(HPage:longint;
         Upd: boolean;
         XMSPage: word);
  end;
  PSPrefix = record
    Int20H,FreeSeg,Filler1,BytesAvail,MsDosCall:word;
    Terminate,CtrlC,HardError:pointer;
    Filler2:word;
    Handles:array[$18..$2B]of byte;
    EnvrSeg:word;
    Filler3:array[$2E..$31]of byte;
    NumHan:word;
    HanPtr:pointer;
    Filler4:array[$38..$5B]of byte;
    FCB1:array[$5C..$6B]of byte;
    FCB2:array[$6C..$7B]of byte;
    Filler5:array[$7C..$7F]of byte;
    ParmArea:string[$7E]; { $81..$FF }
    PSPlen:byte; { $100 }
  end;
var
  CurPSP:^PSPrefix;
  CacheRoot:CachePagePtr;
  NewHT:array[0..255]of byte;    { at dword boundary, size=256 bytes }
  MyHeapEnd:pointer;       { after NewHT; used in Heap.Asm (System unit)}

  XMSCacheRoot:XMSCacheDPtr;
  XMSCacheBuf:pointer;
  CachePageSz {in paragraphs},
  InitStackSz,MinStackSz,InitStack2Sz,MinStack2Sz:word;


 function HeapErrFun(Size:word):integer;
 function Normalize(L:longint):pointer;
 function AbsAdr(P:pointer):longint;
function Cache(Handle:byte;Page:longint):CachePagePtr;
procedure FormatCache;
procedure AlignParagraph;
 function GetStore(Size:word):pointer;
 function GetZStore(Size:word):pointer;
procedure MarkStore(var p:pointer);
procedure ReleaseStore(p:pointer);
procedure ReleaseAfterLongStr(p:pointer);
 function StoreAvail:longint;
 function GetStore2(Size:word):pointer;
 function GetZStore2(Size:word):pointer;
 function StoreStr(S:string):StringPtr;
procedure MarkStore2(var p:pointer);
procedure ReleaseStore2(p:pointer);
procedure MarkBoth(var p,p2:pointer);
procedure ReleaseBoth(p,p2:pointer);
procedure AlignLongStr;


type
  ProcStkPtr=^ProcStkD;
  ProcStkD=record ChainBack:ProcStkPtr;
                  LVRoot:pointer;  {...values of local variables...}
           end;
  ExitRecord = record
    OvrEx: PProcedure;
    mBP:pointer;
    rBP,rIP,rCS,rSP,rDS: word;
    ExP,BrkP: boolean;
  end;

var
  ExitBuf: ExitRecord;
  MyBP,ProcMyBP:ProcStkPtr;
  BPBound:word;
  ExitP,BreakP:boolean;
const
  LastExitCode:longint=0;

procedure StackOvr(NewBP:word);
procedure NewExit(POvr:PProcedure; var Buf:ExitRecord);
procedure GoExit;
procedure RestoreExit(var Buf:ExitRecord);
function OSshell(Path:PathStr;CmdLine:String127;
   NoCancel,FreeMm,LdFont,TextMd:boolean):boolean;

                   {  VIRTUAL HANDLES  }

type FileOpenMode=
          (_isnewfile,_isoldfile,_isoverwritefile,_isoldnewfile);
     FileUseMode=(Closed,RdOnly,RdShared,Shared,Exclusive);
var HandleError:word;
    OldDir,FandDir,WrkDir:DirStr;
    FandOvrName,FandResName,FandWorkName,FandWorkXName,FandWorkTName:PathStr;
    CPath:PathStr; CDir:DirStr; CName:NameStr; CExt:ExtStr;
    CVol:VolStr;
    WasLPTCancel:boolean;
    WorkHandle:word;
const MaxWSize:longint=0; {currently occupied in FANDWORK.$$$}

 function IsNetCVol:boolean;

 function CacheExist:boolean;
procedure LockCache;
procedure UnLockCache;
 function SaveCache(ErrH:word):boolean;
procedure ClearCacheH(h:word);

procedure ExtendHandles;
procedure UnExtendHandles;
procedure SetUpdHandle(H:word);
procedure ResetUpdHandle(H:word);
 function IsUpdHandle(H:word):boolean;
 function PosH(handle:word):longint;
procedure SeekH(handle:word;pos:longint);
 function FileSizeH(handle:word):longint;
 function TryLockH(Handle:word;Pos:longint;Len:word):boolean;
procedure UnLockH(Handle:word;Pos:longint;Len:word);
 function OpenH(Mode:FileOpenMode;UM:FileUseMode):word;
 function ReadH(handle,bytes:word;var buffer):word;
procedure WriteH(handle,bytes:word;var buffer);
procedure TruncH(handle:word;N:longint);
procedure FlushH(handle:word);
procedure FlushHandles;
procedure CloseH(handle:word);
 function GetDateTimeH(handle:word):longint;
procedure CloseClearH(var h:word);
procedure SubstHandle(h1,h2:word);
procedure SetFileAttr(Attr:word);
function GetFileAttr:word;
procedure RdWrCache(ReadOp:boolean;Handle:word;NotCached:boolean;Pos:longint;
         N:word;var Buf);



procedure DeleteFile(path:PathStr);
procedure RenameFile56(OldPath,NewPath:PathStr;Msg:boolean);

function MyFExpand(Nm:String12;EnvName:String8):PathStr;


                    { DISPLAY }


function LenStyleStr(s:string):word;
function LogToAbsLenStyleStr(s:string;l:word):word;
procedure WrStyleStr(s:string; Attr:word);
procedure WrLongStyleStr(S:LongStrPtr; Attr:word);
procedure RectToPixel(c1,r1,c2,r2:word;var x1,y1,x2,y2:word);



                  {  MESSAGES }


var F10SpecKey:word;
    ProcAttr:byte;   {color.uNorm}
function SetStyleAttr(c:char;var a:byte):boolean;
var MsgLine:string;
var MsgPar:array[1..4]of ScreenStr;
procedure SetMsgPar(const s:string);
procedure Set2MsgPar(const s1,s2:string);
procedure Set3MsgPar(const s1,s2,s3:string);
procedure Set4MsgPar(const s1,s2,s3,s4:string);
procedure RdMsg(N:integer);
procedure WriteMsg(N:word);
procedure ClearLL(attr:byte);



                       {   DML    }


var FandInt3f:pointer;
    OvrHandle,Fand_ss,Fand_sp,Fand_bp,DML_ss,DML_sp,DML_bp:word;
const  _CallDMLAddr:longint=0; {passed to FANDDML by setting "DMLADDR="in env.}

{begin of INSTALL 1}

type  TKbdConv  = (OrigKbd,CsKbd,CaKbd,SlKbd,DtKbd);

var   Spec:record UpdCount:byte;
                  AutoRprtWidth,AutoRprtLimit,CpLines:byte;
                  AutoRprtPrint:boolean;
                  ChoosePrMsg:boolean;
                  TxtInsPg:boolean;
                  TxtCharPg:char;
                  ESCverify:boolean;
                  Prompt158:boolean;
                  F10Enter:boolean;
                  RDBcomment:boolean;
                  CPMdrive:char;
                  RefreshDelay,NetDelay:word;
                  LockDelay, LockRetries:byte;
                  Beep:boolean;
                  LockBeepAllowed:boolean;
                  XMSMaxKb:word;
                  NoCheckBreak:boolean;
                  KbdTyp:TKbdConv;
                  NoMouseSupport,
                  MouseReverse:boolean;
                  DoubleDelay,RepeatDelay,CtrlDelay:byte;
                  OverwrLabeledDisk:boolean;
                  ScreenDelay : Word;
                  OffDefaultYear : byte;
                  WithDiskFree : boolean;
           end;
      Video:record address:word;
                   TxtRows:byte ;
                   ChkSnow:boolean;               {not used }
                   cursOn,cursOff,cursBig:word;
            end;
      Fonts:record VFont:TVideoFont;
                   LoadVideoAllowed,NoDiakrSupported:boolean ;
            end;
      Colors:record userColor:array[0..15] of byte;
                    mNorm,mHili,mFirst,mDisabled, {menu}        {change also FANDDML}
                    sNorm,sHili,sMask, {select}
                    pNorm,pTxt, {prompt,verify,password}
                    zNorm, {message}
                    lNorm,lFirst,lSwitch, {last line}
                    fNorm, {first line}
                    tNorm,tCtrl,tBlock, {text edit}
                    tUnderline,tItalic,tDWidth,tDStrike,tEmphasized,tCompressed,tElite,
                    dNorm,dHili,dSubset,dTxt,dDeleted,dSelect, {data edit}
                    uNorm, {user screen}
                    hNorm,hHili,hMenu,hSpec,
                    nNorm,
                    ShadowAttr,
                    DesktopColor :byte;
             end;
var
  CharOrdTab:array[char] of char;   {after Colors /FANDDML/}
  UpcCharTab:array[char] of char;
  TxtCols,TxtRows:word;
const  LaptopCGA:boolean=false;         { text.mode?, ~graph.}
{const  WithDiskFree:boolean=true;}

{menu: mNorm-text;mHili-vybrana volba;mFirst-prvni pismeno;
 select: sNorm-text;sHili-vybrany prvek;sMask-maska pro vyber souboru(i ^K^R);
 prompt...: pNorm-zadavani textu z klavesnice(prompt,kalkula‡ka,heslo,podmno‘ina,^Q^A,F3,dotazy);
            pTxt-pomocny text pri zadavani(- " -);
 message: zNorm-text hlaseni F10,kompilace,procenta;
 last line: lNorm-napoveda v poslednim radku;lFirst-oznaceni klaves;lSwitch-prepinace,locks;
 first line: fNorm-systemovy radek;
 textovy editor: tNorm-text;tCtrl-nevyznamne ridici znaky;tBlock-blok;
                 tUnderline;tItalic;tDWidth;tDStrike;tEmphasized;tCompressed;tElite-ruzne druhy pisma;
 datovy editor: dNorm-data;dHili-datovy kurzor;dSubset-podmnozina;dTxt-pomocny text;
 user screen: uNorm-zakladni atribut pro uzivatelskou obrazovku;
 help: hNorm-text;hHili-vybrana polozka;hMenu-ostatni polozky;hSpec-zvyrazneny text
 help jednoradkovy: nNorm-text;nFirst-oznaceni klaves }


const prName=0;
      prUl1=1;    prUl2=2;
      prKv1=3;    prKv2=4;
      prBr1=5;    prBr2=6;
      prDb1=7;    prDb2=8;
      prBd1=9;    prBd2=10;
      prKp1=11;   prKp2=12;
      prEl1=13;   prEl2=14;
      prReset=15;
              prMgrFileNm=15; prMgrProg=16; prMgrParam=17;
      prPageSizeNN=16; prPageSizeTrail=17;
      prLMarg=18; prLMargTrail=19;
      prUs11=20;  prUs12=21;
      prUs21=22;  prUs22=23;
      prUs31=24;  prUs32=25;
      prLine72=26;prLine216=27;
      prDen60=28; prDen120=29;
      prDen240=30; prColor=31;
      prClose=32;

function PrTab(N:word):string;
procedure SetCurrPrinter(NewPr:integer);

var prCurr,prMax: integer;
var printer:array[0..9] of
   record Strg:pointer; Typ,Kod:char; Lpti,TmOut:byte;
          OpCls,ToHandle,ToMgr:boolean; Handle:word end;
type TPrTimeOut=array[1..4] of byte;
var OldPrTimeOut:TPrTimeOut;
    PrTimeOut:TPrTimeOut absolute 0:$478;

type WDaysTabType=array[1..1]of record Typ:byte;Nr:word end;
var  NWDaysTab:word; WDaysFirst:real; WDaysLast:real;
     WDaysTab:^WDaysTabType;

{end of INSTALL 1}

const AbbrYes:char='Y';AbbrNo:char='N';
      WasInitDrivers:boolean=false;WasInitPgm:boolean=false;

var LANNode:word;

const
  RMsgIdx=0; BgiEgaVga=1; BgiHerc=2; ChrLittKam=3; ChrTripKam=4;
  Ega8x14K=5; Vga8x16K=6; Vga8x19K=7;
  Ega8x14L=8; Vga8x16L=9; Vga8x19L=10;
  ChrLittLat=11; ChrTripLat=12;
  LatToWinCp=13; KamToWinCp=14; WinCpToLat=15;
  FandFace=16;
type
  TResFile = object
    Handle:word;
    A:array[0..FandFace] of record Pos:longint; Size:word end;
    function Get(Kod:word;var P:pointer):word;
    function GetStr(Kod:word):LongStrPtr;
  end;
  TMsgIdxItem = record Nr,Ofs:word; Count:byte end;
  TMsgIdx = array[1..100] of TMsgIdxItem;
var
  ResFile:TResFile;
  MsgIdx:^TMsgIdx;
  MsgIdxN:word; FrstMsgPos:longint;
  CallOpenFandFiles: procedure(FromDML:boolean);
  CallCloseFandFiles: procedure(FromDML:boolean);
procedure OpenOvrFile;
procedure OpenResFile;
procedure OpenWorkH;

var UserLicNr:integer;
var userToday:float;


implementation      {*******************************************************}

uses obase,obaseww,oaccess,
     access  {SetCPathForH,StoreInTWork,ReadDelINTWork,SQLDisconnect}
{$ifdef FandSQL}  ,channel  {$endif}  {SQLDisconnect}
     ,runfand;


{ PASCAL supports 3 formats of return addresses on the stack
  1) seg:0    points to an area (1 for each overlay segment) with int 3fH
         CD 3F xx xx zz zz 00 00    xxxx is the return offset in the segment
  2) seg:off  seg points to the int-area and off is the return offset
         applicated if there are further returns to a segment not in memory
         on higher stack addresses
  3) real return address for all segments in memory
 Before we can cut the call stack to NewBP, we must find type 1 addresses
 that will be eliminated and search the first type 2 address not eliminated
 for it. This we change to a type 1 address.  }

procedure StackOvr(NewBP:word);
type SF=record BP:word; case byte of 0:(Ret:pointer); 1:(RetOfs:word) end;
var p,q:^SF;   pofs:word absolute p; qofs:word absolute q;
    r,t:^word; rofs:word absolute r; tofs:word absolute t;
label 1;
begin
  asm mov p.word,bp; mov p[2].word,ss end; pofs:=p^.BP;
  while pofs<NewBP do begin r:=p^.ret; pofs:=p^.BP;
    if (rofs=0) and (r^=$3fcd) then begin
    q:=ptr(SSeg,NewBP); inc(rofs,2);
    while qofs<BPBound do begin t:=q^.ret;
    if (seg(t^)=seg(r^)) and (tofs<>0) then begin
       r^:=tofs; q^.retofs:=0; goto 1 end;
    qofs:=q^.BP end end;
1:end;
end;

procedure NoOvr; far; assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push dx; push ax;
end;


{$I common}



const
  CacheLocked:boolean=false;


{$I handle}

{$I memory}

function PrTab(N:word):string;
var p:pointer;
begin p:=printer[prCurr].Strg;
asm  push ds; cld; lds si,p; les di,@result; mov cx,N; inc cx; xor ax,ax;
@1:  add si,ax; lodsb; loop @1;
     stosb; mov cx,ax; rep movsb; pop ds end;
end;
procedure SetCurrPrinter(NewPr:integer);
begin
  if NewPr>=prMax then exit;
  if prCurr>=0 then with printer[prCurr] do if TmOut<>0 then
    PrTimeOut[Lpti]:=OldPrTimeOut[Lpti];
  prCurr:=NewPr;
  if prCurr>=0 then with printer[prCurr] do if TmOut<>0 then begin
    PrTimeOut[Lpti]:=TmOut end;
end;


var ExitSave:pointer;
{$S-} procedure MyExit; far;
procedure WrTurboErr;
var s:string[9];
begin
  str(ExitCode,s); SetMsgPar(s); WrLLF10Msg(626);
  ErrorAddr:=nil; ExitCode:=0;
end;
label 1;
begin
  asm mov ax,SEG @Data; mov ds,ax end;
  ExitProc:=ExitSave;
  if not WasInitPgm then begin UnExtendHandles; goto 1 end;
  if ErrorAddr<>nil then case ExitCode of
    202{stack overflow}:
        begin asm mov sp,ExitBuf.rSP end; WrLLF10Msg(625) end;
    209{overlay read error}: WrLLF10Msg(648);
    else WrTurboErr end;
  {$ifdef FandSQL} SQLDisconnect; {$endif}
  UnExtendHandles;
  DeleteFile(FandWorkName); DeleteFile(FandWorkXName);
  DeleteFile(FandWorkTName);
  CloseXMS;
1:if WasInitDrivers then begin
    DoneMouseEvents; CrsIntrDone; BreakIntrDone;
    if IsGraphMode then begin
      CloseGraph; IsGraphMode:=false; ScrSeg:=video.Address end;
    asm  push bp; mov ah,0fH; int 10H; cmp al,StartMode; je @1;
         mov ah,0; mov al,StartMode; int 10H;
    @1:  pop bp end;
    window(1,1,TxtCols,TxtRows); TextAttr:=StartAttr; ClrScr; CrsNorm;
    ChDir(OldDir);
    SetCurrPrinter(-1) end;
  if ExitCode=202 then halt;
end; {$S+}

procedure OpenResFile;
begin
  CPath:=FandResName; CVol:=''; ResFile.Handle:=OpenH(_isoldfile,RdOnly);
  if HandleError<>0 then begin write('can''t open '+FandResName);wait;halt end;
end;

procedure OpenWorkH;
begin
  CPath:=FandWorkName; CVol:=''; WorkHandle:=OpenH(_isoldnewfile,Exclusive);
  if HandleError<>0 then begin write('can''t open '+FandWorkName);wait;halt end;
end;


function TResFile.Get(Kod:word;var P:pointer):word;
var l:word;
begin
  l:=A[Kod].Size; getmem(P,l); SeekH(Handle,A[Kod].Pos);
  ReadH(Handle,l,P^); Get:=l;
end;
function TResFile.GetStr(Kod:word):LongStrPtr;
var s:LongStrPtr;
begin with A[Kod] do begin
  s:=GetStore(Size+2); s^.LL:=Size;
  SeekH(Handle,Pos); ReadH(Handle,Size,s^.A); GetStr:=s;
end end;
{$S-}procedure RdMsg(N:integer);
var i,j,h,o:word; s:string;
label 1;
begin
  for i:=1 to MsgIdxN do with MsgIdx^[i] do
    if (N>=Nr) and (N<Nr+Count) then begin j:=N-Nr+1; o:=Ofs; goto 1 end;
  o:=0; j:=1; str(N,MsgPar[1]);
1:h:=ResFile.Handle; SeekH(h,FrstMsgPos+o);
  for i:=1 to j do begin ReadH(h,1,s); ReadH(h,length(s),s[1]) end;
  ConvKamenToCurr(s[1],length(s));
  MsgLine:=''; j:=1; s[length(s)+1]:=#0;
  for i:=1 to length(s) do
    if (s[i]='$') and (s[i+1]<>'$') then begin
      MsgLine:=MsgLine+MsgPar[j];inc(j) end
    else begin MsgLine:=MsgLine+s[i]; if (s[i]='$') then inc(i) end;
end;
{$S+}

procedure InitOverlays;
var name:NameStr; ext:ExtStr; sz,err:integer; l:longint; s:string;
const OvrlSz=124;
begin
  GetDir(0,OldDir); FSplit(FExpand(ParamStr(0)),FandDir,name,ext);
  FandOvrName:=MyFExpand(name+'.OVR','FANDOVR');
  OvrInit(FandOvrName); if OvrResult<>0 then begin          {reshandle-1}
    FandOvrName:=ParamStr(0); OvrInit(FandOvrName); if OvrResult<>0 then begin
      write('can''t open FAND.OVR'); wait; halt end end;
  OvrInitEMS;
  s:=GetEnv('FANDOVRB');
  while (Length(s)>0) and (s[Length(s)]=' ') do s[0]:=chr(length(s)-1);
  val( s ,sz,err);
  if (err<>0) or (sz<80) or (sz>OvrlSz+10) then sz:=OvrlSz; l:=longint(sz)*1024;
  OvrSetBuf(l); OvrSetRetry(l div 2);
  FreeList:=nil;
end;

procedure OpenOvrFile;
var h:word;
begin
  CPath:=FandOvrName; CVol:=''; h:=OpenH(_isoldfile,RdOnly);
  if h<>OvrHandle then begin write('can''t open FAND.OVR'); wait; halt end;
end;

begin
  CurPSP:=ptr(PrefixSeg,0); MyHeapEnd:=HeapEnd; ExtendHandles; prCurr:=-1;
  InitOverlays;
  ExitSave:=ExitProc; ExitProc:=@MyExit; MyBP:=nil;
  UserLicNr:=word(UserLicNrShow) and $7FFF;
  FandResName:=MyFExpand('Fand.Res','FANDRES');
  OpenResFile;
end.
