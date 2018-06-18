unit MyDOS;

interface

uses Crt,Dos,Printer;

var  FreePtr:pointer;

const MaxTxtCols=132;

type string1=string[1];   string10=string[10];
     string2=string[2];   string12=string[12];
     string3=string[3];   string20=string[20];
     string4=string[4];   string22=string[22];
     string5=string[5];   string30=string[30];
     string6=string[6];   string31=string[31];
     string7=string[7];   string11=string[11];
     string8=string[8];   string127=string[127];
     string9=string[9];

     ScreenStr=string[MaxTxtCols];
     PathStr=string[64];

var MemSize,NoParagraphs:word;     {memory}

 function AllocMem (size:word):word;
 function AllocMaxMem :word;
procedure ModifyMem (segment,size:word);
procedure ModifyMaxMem (segment:word);
procedure ReleaseMem (segment:word);
procedure AvailMem (var segment,size:word);
 function MemBlockSize (segment:word):LongInt;

 function LegalFileName (n:string8):boolean;     {directory tree}
procedure DivFileName (s:PathStr; var drive,path,name,typ:PathStr);
procedure MyChDir (s:PathStr);

var HeapEnd:pointer;     {heap}

procedure PtrNorm (var p:pointer);
 function PtrToAddr (p:pointer):LongInt;
 function AddrToPtr (adr:LongInt):pointer;
 function HeapUsed :LongInt;

type  CursorType=record on,off,half,big:word end;     {screen}

var   Cursors:CursorType;
      VideoAddr:word;

procedure SetVideoAddr;
procedure SetVideoMode (mode:byte);
 function GetPage :word;
procedure SetPage (page:word);
 function GetCursorMode :word;
procedure SetCursorMode (mode:word);
procedure CursorOn;
procedure CursorOff;
procedure MoveToScreen (var source,dest;length:word);
procedure MoveFromScreen (var source,dest;length:word);
procedure Roll (up:boolean;NoLines,NewAttr,xc,xr,yc,yr:word);
 function EGApresent :boolean;
procedure SetCGABorder (color:byte);

type Locks=(Caps,Num,Scroll,Alt,Ctrl,lShift,rShift);     {keyboard}

procedure Beep;
 function AltCh (alt:char):char;
 function Pressed (lock:Locks):boolean;

var EnvrPtr:pointer;     {environment}

 function EnvrStr :string;
 function EnvrParStr (par:string):string;
 function PgmParStr :string;
 function PgmPathStr :string;
 function BIOSdate: string8;

procedure ResetTimer;     {time}
 function Timer :real;

 function HexN (n:byte):char;     {hex}
 function HexB (b:byte):string2;
 function HexW (i:word):string4;
 function HexD (i:LongInt):string8;
 function HexA (var ad):string9;
procedure DispA (var t:text; var ad;NoBytes:integer);
procedure DispH (var t:text; var ad;NoBytes:integer);
 function HexToN (c:char):byte;
 function HexToB (cc:string2):byte;
 function HexToW (cccc:string4):word;

procedure AnalyzeError (var code,class,action,locus:word);     {dos errors}

procedure DumpBiosVars;     {dumps}

 function CopyFiles (source,dest:PathStr):boolean;     {files}
procedure FlushDisk;
 function TextHandle (var t:text):word;
 function HandleDCB (handle:word):word;

type ExeHeader=record MZ:word;     {control blocks}
                      PartPag:word;
                      PageCnt:word;
                      ReloCnt:word;
                      HdrSize:word;
                      MinMem:word;
                      MaxMem:word;
                      ReloSS:word;
                      ExeSP:word;
                      ChkSum:word;
                      ExeIP:word;
                      ReloCS:word;
                      TablOff:word;
                      Overlay:word;
                      ReloTab:array[1..1]of record ofs,seg:word end;
               end;

type MemCtrlBlock=record typ:char;
                         owner:word;
                         size:word;
                         reserved:array[$05..$0F] of byte;
                  end;

type PSP=record Int20H:word;
                FreeSeg:word;
                Filler1:word;
                BytesAvail:word;
                MsDosCall:word; {?}
                Terminate:pointer;
                CtrlC:pointer;
                HardError:pointer;
                Filler2:array[$16..$17]of byte;
                Handles:array[$18..$2B]of byte;
                EnvrSeg:word;
                Filler3:array[$2E..$31]of byte;
                HandleNum:word;
                HandlePtr:pointer;
                Filler4:array[$38..$5B]of byte;
                FCB1:array[$5C..$6B]of byte;
                FCB2:array[$6C..$7B]of byte;
                Filler5:array[$7C..$7F]of byte;
                ParmArea:string[$7E]; { $81..$FF }
                PSPlen:byte; { $100 }
         end;

type PSPPtr=^PSP;

function PrefixSegPtr :PSPPtr;

const AUXdcb=0; CONdcb=1; PRNdcb=2;
      INPh=1; OUTh=2; ERRh=3; AUXh=4; PRNh=5;

type BPB=record SectSiz:word;  {bytes per sector}
                ClustSiz:byte; {sectors per allocation unit (cluster)}
                ResSecs:word;  {boot and reserved sectors}
                FatCnt:byte;   {number of File Allocation Tables}
                RootSiz:word;  {maximum number of 32-byte directory entries in root}
                TotSecs:word;  {total number of sectors in media (DOS partition)}
                Media:byte;    {media descriptor (same as 1st byte in FAT)}
                FatSize:word;  {number of sectors in one FAT}
         end;

type BootSec=record jmp:array[1..3]of byte;     {near jump to start of boot code}
                    company:array[1..8]of char; {OEM company name and version}
                    B:BPB;                      {BiosParameterBlock}
                    TrkSecs:word;               {sectors per track (cylinder)}
                    HeadCnt:word;               {number of read/write heads (surfaces)}
                    HidnSec:word;               {hidden sectors (may be used in partitioning schemes)}
                    {start of boot code/data}
             end;

var EquipmentFlag:word absolute 0:$0410;     {ROM BIOS}
    VideoMode:byte absolute 0:$0449;
    VideoCols:word absolute 0:$044A;
    VideoPageSize:word absolute 0:$044C;
    VideoPageOffs:word absolute 0:$044E;
    VideoCursLoc:array[1..8,1..2]of byte absolute 0:$0450;
    VideoCursSize:word absolute 0:$0460;
    VideoActPage:byte absolute 0:$0462;
    TimeCounter:LongInt absolute 0:$046C;

const _F1=#$3B;_F2=#$3C;_F3=#$3D;_F4=#$3E;_F5=#$3F;     {extended key codes}
      _F6=#$40;_F7=#$41;_F8=#$42;_F9=#$43;_F10=#$44;
      _ShiftF1=#84; _ShiftF2=#85; _ShiftF3=#86; _ShiftF4=#87; _ShiftF5=#88;
      _ShiftF6=#89; _ShiftF7=#90; _ShiftF8=#91; _ShiftF9=#92; _ShiftF10=#93;
      _CtrlF1=#94;  _CtrlF2=#95;  _CtrlF3=#96;  _CtrlF4=#97;  _CtrlF5=#98;
      _CtrlF6=#99;  _CtrlF7=#100; _CtrlF8=#101; _CtrlF9=#102; _CtrlF10=#103;
      _AltF1=#104;  _AltF2=#105;  _AltF3=#106;  _AltF4=#107;  _AltF5=#108;
      _AltF6=#109;  _AltF7=#110;  _AltF8=#111;  _AltF9=#112;  _AltF10=#113;

      _left=#$4B; _right=#$4D; _up=#$48;   _down=#$50;
      _Home=#$47; _End=#$4F;   _PgUp=#$49; _PgDn=#$51;
      _CtrlLeft=#115; _CtrlRight=#116; _CtrlPgUp=#$84; _CtrlPgDn=#$76;
      _CtrlHome=#$77; _CtrlEnd=#$75;   _ins=#$52;      _del=#$53;

      _AltQ=#16;_AltW=#17;_AltE=#18;_AltR=#19;_AltT=#20;_AltY=#21;_AltU=#22;_AltI=#23;_AltO=#24;_AltP=#25;
      _AltA=#30;_AltS=#31;_AltD=#32;_AltF=#33;_AltG=#34;_AltH=#35;_AltJ=#36;_AltK=#37;_AltL=#38;
      _AltZ=#44;_AltX=#45;_AltC=#46;_AltV=#47;_AltB=#48;_AltN=#49;_AltM=#50;

      _BEL=^g;_BS=^h;_EOF=^z;_NUL=#$00;_CR=#$0D;_LF=#$0A;_ESC=#$1B;     {ASCII ctrl codes}

      upCh=#$18;downCh=#$19;rightCh=#$1A;leftCh=#$1B;    {IBM character set}
      enterCh:array[1..2]of char=(#$11,#$D9);

      __left=^s;__right=^d;__up=^e;__down=^x;     {ctrl keys}
      __CtrlLeft=^a;__CtrlRight=^f;__PgUp=^r;__PgDn=^c;__ins=^n;__del=^g;
      __Home=^s;__End=^d;__CtrlPgUp=^r;__CtrlPgDn=^c; {with ^q prefix}
      __CtrlHome=^e;__CtrlEnd=^x;

const MonoHidd=$00;     { Monochrom colors }
      MonoNorm=$02; MonoUndL=$01; MonoInvr=$70; MonoHiLi=$0A; MonoHiLiUndL=$09;
      MonoNormBl=$82; MonoUndLBl=$81; MonoInvrBl=$F0; MonoHiLiBl=$8A; MonoHiLiUndLBl=$89;

const MonoCursors: CursorType=(on:$0B0C;off:$2000;half:$080C;big:$000C);
      CGACursors:  CursorType=(on:$0607;off:$2000;half:$0407;big:$0007);
      EGACursors:  CursorType=(on:$0607;off:$2000;half:$0407;big:$0007);

implementation

{ *** memory *** }

procedure DispMemCtrlBlock (segment:word);
  var p:^MemCtrlBlock;
  begin p:=ptr(segment-1,0);
        with p^ do begin writeln('typ:',typ);
                         writeln('owner:',hexw(owner));
                         writeln('size:',hexw(size));
                   end;
  end;

procedure MyMsDos (var r:registers);
  begin MsDos(r);
        with r do if (FLAGS and FCarry)=FCarry then DosError:=AX
                                               else DosError:=0;
  end;

function AllocMem(size:word):word;
  var r:Registers;
  begin with r do begin AH:=$48;
                        BX:=size;
                        MyMsDos(r);
                        NoParagraphs:=BX;
                        AllocMem:=AX;
  end             end;

function AllocMaxMem:word;
  begin AllocMaxMem:=AllocMem($FFFF);
        if (DosError=8)and(NoParagraphs>0) then AllocMaxMem:=AllocMem(NoParagraphs);
  end;

procedure ModifyMem(segment,size:word);
  var r:Registers;
  begin with r do begin AH:=$4A;
                        ES:=segment;
                        BX:=size;
                        MyMsDos(r);
                        NoParagraphs:=BX;
  end             end;

procedure ModifyMaxMem(segment:word);
  begin ModifyMem(segment,$FFFF);
        if (DosError=8)and(NoParagraphs>0) then ModifyMem(segment,NoParagraphs);
  end;

procedure ReleaseMem(segment:word);
  var r:Registers;
  begin with r do begin AH:=$49;
                        ES:=segment;
                        MyMsDos(r);
  end             end;

procedure AvailMem(var segment,size:word);
  begin segment:=AllocMem($FFFF);
        if (DosError=8) and (NoParagraphs>0) then begin size:=NoParagraphs;
                                                        segment:=AllocMem(NoParagraphs);
                                                        ReleaseMem(segment);
                                                  end
                                             else size:=0;
  end;

function MemBlockSize(segment:word):LongInt;
  var p:^MemCtrlBlock;
  begin p:=ptr(segment-1,0); MemBlockSize:=LongInt(p^.size) shl 4 end;

{ *** process *** }

(*procedure CallPgm(path,CmdLine:string);
  const DosReserved=$0001;
  var   FreeSeg,AlienSeg,MaxBlock,FreeSize:word; x,y:word;
        p,q:pointer;
  begin AvailMem(FreeSeg,MaxBlock);
        if ofs(FreePtr^)<>0 then begin FreeSeg:=seg(FreePtr^)+(ofs(FreePtr^) shr 4);
                                       FreeSize:=PrefixSegPtr^.FreeSeg-FreeSeg;
{. writeln('before:',freesize,' maxblock:',maxblock); .}
                                       if FreeSize<MaxBlock then FreeSize:=MaxBlock;
                                       ModifyMem(PrefixSeg,PrefixSegPtr^.FreeSeg-PrefixSeg-FreeSize-DosReserved-1);
{. availmem(x,y);writeln('modify1...segment:',hexw(x),' size:',hexw(y)); .}
                                       FreeSeg:=AllocMaxMem;
{. availmem(x,y);writeln('alloc...segment:',hexw(x),' size:',hexw(y)); .}
                                 end;
        ModifyMem(PrefixSeg,seg(HeapPtr^)-PrefixSeg+1);
        availmem(x,y); writeln('free memory ... segment: ',hexw(x),', size: ',hexw(y));
        Exec(path,CmdLine);
        writeln('Dos Error: ',doserror,', press space'); while readkey<>' ' do;
        if ofs(FreePtr^)<>0 then ReleaseMem(FreeSeg);
        ModifyMaxMem(PrefixSeg);
  end;

procedure LoadPgm(path:string);
  var r:Registers;
      StartSeg:word;
      block:LongInt;
  begin path:=path+#$00;
        StartSeg:=seg(HeapOrg^);
        block:=(StartSeg shl 16)+StartSeg;   {???}
        with r do begin AH:=$4B;
                        DS:=seg(path);
                        DX:=succ(ofs(path));
                        ES:=seg(block);
                        BX:=ofs(block);
                        AL:=$03;
                        MsDos(r);
  end             end;

function GetBreakState :boolean;
  var r:registers;
  begin with r do begin AH:=$33;
                        AL:=$00;
                        MsDos(r);
                        GetBreakState:= DL=$01;
  end             end;

procedure SetBreakState (on:boolean);
  var r:registers;
  begin with r do begin AH:=$33;
                        AL:=$01;
                        if on then DL:=$01 else DL:=$00;
                        MsDos(r);
  end             end;*)

{ *** directory tree *** }

function LegalFileName(n:string8):boolean;
  var i:word; lenN:byte absolute n;
  begin LegalFileName:=false; if (lenN=0) or (lenN>8) then exit;
        for i:=1 to lenN do
          if not(n[i] in ['A'..'Z','0'..'9','$','&','#','@','!','%','"','(',')','-','{','}','_']) then exit;
        LegalFileName:=true;
  end;
procedure DivFileName (s:PathStr; var drive,path,name,typ:PathStr);
  var lenS:byte absolute s; i:word;
  begin for i:=1 to lenS do s[i]:=upcase(s[i]);
        drive:=''; path:=''; name:=''; typ:='';
        if (lenS>1) and (s[2]=':') then
          begin drive:=copy(s,1,2); delete(s,1,2) end;
        i:=lenS;
        while (s[i]<>'\') and (s[i]<>'.') and (i>1) do dec(i);
        if s[i]='.' then begin typ:=copy(s,i+1,lenS-i); lenS:=i-1;
                               while (s[i]<>'\') and (i>1) do dec(i);
                         end;
        if s[i]<>'\' then dec(i); name:=copy(s,i+1,lenS-i); delete(s,i+1,lenS-i);
        path:=s;
  end;
procedure MyChDir (s:PathStr);
  var lenS:byte absolute s;
  begin if (lenS>3) and (s[lenS]='\') then dec(lenS); {$I-} ChDir(s) {$I+} end;

{ *** heap *** }

procedure PtrNorm(var p:pointer);
  begin p:=ptr(seg(p^)+(ofs(p^) shr 4),ofs(p^) and $000F) end;

function PtrToAddr (p:pointer):LongInt;
  begin PtrToAddr:=(LongInt(seg(p^)) shl 4)+ofs(p^) end;

function AddrToPtr (adr:LongInt):pointer;
  begin AddrToPtr:=ptr(adr shr 4,adr and $0000000F) end;

function HeapUsed:LongInt;
  begin HeapUsed:=ptrtoaddr(HeapEnd)-ptrtoaddr(HeapOrg)-memavail end;

{ *** screen *** }

var VideoBase:word;

procedure SetVideoAddr;
  begin VideoAddr:=VideoBase+VideoActPage*VideoPageSize end;

procedure SetVideoMode (mode:byte);
  var r:registers;
  begin r.AH:=$00; r.AL:=mode; Intr($10,r) end;

function GetPage:word;
  var r:Registers;
  begin r.AH:=$0F;
        Intr($10,r);
        GetPage:=r.BH;
  end;

procedure SetPage(page:word);
  var r:Registers;
  begin r.AH:=$05;
        r.AL:=page;
        Intr($10,r);
        SetVideoAddr;
  end;


function GetCursorMode:word;
  var r:Registers;
  begin r.AH:=$03;
        Intr($10,r);
        GetCursorMode:=r.CX;
  end;

procedure SetCursorMode(mode:word);
  var r:Registers;
  begin r.AH:=$01;
        r.CX:=mode;
        Intr($10,r);
  end;

procedure CursorOn;
  begin SetCursorMode(Cursors.on) end;
procedure CursorOff;
  begin SetCursorMode(Cursors.off) end;

procedure MoveToScreen(var source,dest; length:word);
  begin if not(CheckSnow) then move(source,dest,length)
                          else begin length:=length shr 1; if length=0 then exit;
                                     inline($1E/$55/$BA/$DA/$03/$C5/$B6/ Source /$C4/$BE/ Dest /$8B/$8E/
                                            Length /$FC/$AD/$89/$C5/$B4/$09/$EC/$D0/$D8/$72/$FB/$FA/$EC/
                                            $20/$E0/$74/$FB/$89/$E8/$AB/$FB/$E2/$EA/$5D/$1F);
  end                          end;

procedure MoveFromScreen(var source,dest; length:word);
  begin if not(CheckSnow) then move(source,dest,length)
                          else begin length:=length shr 1; if length=0 then exit;
                                     Inline($1E/$55/$BA/$DA/$03/$C5/$B6/ Source /$C4/$BE/ Dest /$8B/$8E/
                                            Length /$FC/$EC/$D0/$D8/$72/$FB/$FA/$EC/$D0/$D8/$73/$FB/$AD/
                                            $FB/$AB/$E2/$F0/$5D/$1F);
  end                          end;

procedure Roll (up:boolean;NoLines,NewAttr,xc,xr,yc,yr:word);
  var r:Registers;
  begin with r do begin if up then AH:=$06 else AH:=$07;
                        AL:=NoLines;
                        BH:=NewAttr;
                        CH:=xr-1;
                        CL:=xc-1;
                        DH:=yr-1;
                        DL:=yc-1;
                        Intr($10,r);
  end             end;

function EGApresent :boolean;
  var r:Registers;
  begin with r do begin AH:=$12;
                        BL:=$10;
                        BH:=$FF;
                        Intr($10,r);
                        EGApresent:=BH<>$FF;
  end             end;

procedure SetCGABorder (color:byte);
  var r:Registers;
  begin with r do begin AH:=$0B;
                        BL:=color;
                        BH:=$00;
                        Intr($10,r);
  end             end;

{ *** keyboard *** }

procedure Beep;
  begin write(^g) end;

function AltCh(alt:char):char;
  begin AltCh:=#00;
        case alt of
      _AltQ:AltCh:='Q'; _AltW:AltCh:='W'; _AltE:AltCh:='E'; _AltR:AltCh:='R'; _AltT:AltCh:='T'; _AltY:AltCh:='Y';
      _AltU:AltCh:='U'; _AltI:AltCh:='I'; _AltO:AltCh:='O'; _AltP:AltCh:='P'; _AltA:AltCh:='A'; _AltS:AltCh:='S';
      _AltD:AltCh:='D'; _AltF:AltCh:='F'; _AltG:AltCh:='G'; _AltH:AltCh:='H'; _AltJ:AltCh:='J'; _AltK:AltCh:='K';
      _AltL:AltCh:='L'; _AltZ:AltCh:='Z'; _AltX:AltCh:='X'; _AltC:AltCh:='C'; _AltV:AltCh:='V'; _AltB:AltCh:='B';
      _AltN:AltCh:='N'; _AltM:AltCh:='M';
  end   end;

function Pressed (lock:Locks):boolean;
  var r:Registers; bit:byte;
  begin bit:=$40 shr ord(lock);
        with r do begin AH:=$02;
                        Intr($16,r);
                        pressed:=(AL and bit)=bit;
  end             end;

{ *** environment *** }

function EnvrStr:string;
  var p,q:^char; ofsP:word absolute p; ofsQ:word absolute q; s:string;
  begin q:=EnvrPtr; p:=EnvrPtr; inc(ofsP); move(EnvrPtr^,s[1],255);
        while (q^<>#0) or (p^<>#0) do begin inc(ofsP);inc(ofsQ) end;
        if ofsP>255 then s[0]:=#$FF else s[0]:=char(ofsP);
        EnvrStr:=s;
  end;

function EnvrParStr(par:string):string;
  var s:string; i,j:word;
  begin s:=EnvrStr;i:=pos(par,s);if i=0 then begin EnvrParStr:='';exit end;
        i:=i+length(par)+1;j:=i;while s[j]<>#0 do inc(j);EnvrParStr:=copy(s,i,j-i);
  end;

function PgmParStr:string;
  begin PgmParStr:=PrefixSegPtr^.ParmArea end;

function PgmPathStr:string;
  var p,q:^char; ofsP:word absolute p; ofsQ:word absolute q; s:string;
  begin q:=EnvrPtr; p:=EnvrPtr; inc(ofsP);
        while (q^<>#0) or (p^<>#0) do begin inc(ofsP);inc(ofsQ) end;
        inc(ofsP,3); move(p^,s[1],255); s[0]:=#0;
        while (s[byte(s[0])+1]<>#0) and (s[0]<#255) do inc(s[0]);
        PgmPathStr:=s;
  end;

function BIOSdate:string8;
  var s:string8;
  begin move(ptr($F000,$FFF5)^,s[1],8); s[0]:=#8; BIOSdate:=s end;

{ *** time *** }

var CounterState:LongInt;

procedure ResetTimer;
  begin CounterState:=TimeCounter end;
function Timer:real;
  begin Timer:=(TimeCounter-CounterState)*0.055 end;

{ *** hex *** }

const HexStr:array[0..15]of char='0123456789ABCDEF';

function HexToN(c:char):byte;
  var n:byte;
  begin n:=pos(c,HexStr); if n>0 then dec(n); HexToN:=n end;
function HexToB(cc:string2):byte;
  begin HexToB:=HexToN(cc[1])*16+HexToN(cc[2]) end;
function HexToW(cccc:string4):word;
  begin HexToW:=HexToB(copy(cccc,1,2))*256+HexToB(copy(cccc,3,2)) end;
function HexN(n:byte):char;
  begin HexN:=HexStr[n] end;
function HexB(b:byte):string2;
  begin HexB:=HexStr[b shr 4] + HexStr[b and $0F] end;
function HexW(i:word):string4;
  begin HexW:=HexB(hi(i)) + HexB(lo(i)) end;
function HexD(i:LongInt):string8;
  var a:array[1..4]of byte absolute i;
  begin HexD:=HexB(a[4])+HexB(a[3])+HexB(a[2])+HexB(a[1]) end;
function HexA(var ad):string9;
  var a:array[1..2]of word absolute ad;
  begin HexA:=HexW(a[2])+':'+HexW(a[1]) end;
procedure DispA(var t:text;var ad;NoBytes:integer);
  var i:integer; p:^char;
  begin p:=addr(ad);
        for i:=1 to NoBytes do begin if(p^>#31)and(p^<#126)then write(t,p^)
                                                           else write(t,'.');
                                     p:=ptr(seg(p^),succ(ofs(p^)));
                               end;
        writeln(t);
  end;
procedure DispH(var t:text;var ad;NoBytes:integer);
  var i:integer; p:^byte;
  begin p:=addr(ad);
        for i:=1 to NoBytes do begin write(t,HexB(p^),' ');
                                     p:=Ptr(seg(p^),succ(ofs(p^)));
                               end;
        writeln(t);
  end;

{ *** errors *** }

procedure AnalyzeError (var code,class,action,locus:word);
  var r:registers;
  begin with r do begin AH:=$59;
                        BX:=$0000;
                        MsDos(r);
                        code:=AX;
                        class:=BH;
                        action:=BL;
                        locus:=CH;
  end             end;

{ *** dump *** }

procedure DumpBiosVars;
  begin writeln('EquipmentFlag: ',hexw(EquipmentFlag));
        writeln('    VideoMode: ',hexb(VideoMode));
        writeln('    VideoCols: ',VideoCols);
        writeln('VideoPageSize: ',hexw(VideoPageSize));
        writeln('VideoPageOffs: ',hexw(VideoPageOffs));
        write  (' VideoCursLoc: ');dispH(output,VideoCursLoc,10);
        writeln('VideoCursSize: ',hexw(VideoCursSize));
        writeln(' VideoActPage: ',VideoActPage);
        writeln('  TimeCounter: ',TimeCounter);
  end;

{ *** files *** }

function CopyFiles (source,dest:PathStr):boolean;
  var buffPtr:pointer; buffSize:word;
  function Copy(sName,dName:PathStr):boolean;
    var d,m,i:word; sf,df:file; size:LongInt; ok:boolean; ch:char;
    label 1;
    begin Copy:=false;
          write('-> ',dName);
          assign(sf,sName); assign(df,dName); reset(sf,1); rewrite(df,1);
          size:=FileSize(sf);
          d:=size div buffSize; m:=size mod buffSize; ok:=true;
          {$I-}
          for i:=1 to d do begin BlockRead(sf,buffPtr^,buffSize);
                                 BlockWrite(df,buffPtr^,buffSize);
                                 ok:=IOResult=0; if not(ok) then goto 1;
                           end;
          BlockRead(sf,buffPtr^,m); BlockWrite(df,buffPtr^,m); ok:=IOResult=0;
          {$I+}
        1:Close(sf); Close(df);
          if not(ok) then begin writeln(' !!! chyba !!! '^g);
                                ch:=readkey; Erase(df); exit;
                          end;
          writeln;
          Copy:=true;
    end;
  var d,p,n,t:PathStr; SR:SearchRec;
  begin CopyFiles:=false;
        writeln('Kopiruji soubory: ',source,' do adresare ',dest);
        DivFileName(source,d,p,n,t);
        FindFirst(source,ReadOnly+Archive,SR);
        if maxavail>$F000 then buffSize:=$F000 else buffSize:=maxavail;
        GetMem(buffPtr,buffSize);
        while DosError=0 do begin if not(copy(d+p+SR.name,dest+SR.name)) then exit;
                                  FindNext(SR);
                            end;
        FreeMem(buffPtr,buffSize);
        CopyFiles:=true;
  end;

procedure FlushDisk;
  var r:registers;
  begin r.AH:=$0D; MsDos(r) end;

function TextHandle (var t:text):word;
  var handle:word absolute t;
  begin TextHandle:=handle end;

function HandleDCB (handle:word):word;
  begin HandleDCB:=PrefixSegPtr^.Handles[handle+$17] end;

function PrefixSegPtr :PSPPtr;
  var r:registers;
  begin r.AH:=$62; MsDos(r); PrefixSegPtr:=ptr(r.BX,0) end;

{initialization}

procedure InitVideo;
  var code:word; s:string;
  begin s:=EnvrParStr('VIDEOBASE');
        if s<>'' then val(s,VideoBase,code)
                 else if LastMode=7 then VideoBase:=$B000 else VideoBase:=$B800;
        s:=EnvrParStr('CHECKSNOW');
        if s<>'' then CheckSnow:=s='TRUE'
                 else CheckSnow:=(LastMode<>mono) and not(EGApresent);
                 if LastMode=7 then VideoBase:=$B000 else VideoBase:=$B800;
        if (LastMode=Mono) then Cursors:=MonoCursors
                           else if EGApresent then Cursors:=EGACursors
                                              else Cursors:=CGACursors;
        SetVideoAddr; DirectVideo:=true;
  end;

begin
      EnvrPtr:=ptr(PrefixSegPtr^.EnvrSeg,0);
      HeapEnd:=ptr(seg(FreePtr^)+$1000,0);
      InitVideo;
end.