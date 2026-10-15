{$I Switches}
{$A-,B-,F+,G-,I+,O-,R-,S+,V+,X+,L-}
unit Drivers;

interface

uses dos{$ifdef FandGraph},graph{$endif}{$ifdef FPC},SysUtils{$endif};

type
  TPoint = object
    X,Y: word;
    procedure Assign(XX,YY:word);
  end;

  PObject = ^TObject;
  TObject = object
    constructor Init;
    destructor Done; virtual;
  end;

const
  evMouseDown=$0001; evMouseUp=$0002; evMouseMove=$0004; evMouseAuto=$0008;
  evNothing  =$0000; evMouse  =$000F;
  evKeyDown  =$0010;

  mbLeftButton = $01; mbRightButton = $02; mbDoubleClick = $100;

type
  PEvent = ^TEvent;
  TEvent = record
    case What:word of
     evMouse  : (Buttons:word; Where,WhereG,From:TPoint);
     evKeyDown: (
        case Integer of
	  0: (KeyCode: Word);
          1: (CharCode: Char; ScanCode: Byte));
  end;

var Event:TEvent;
    trialStartFand:longint;
    trialInterval:integer;

const
  _F1=#59;_F6=#64;_F9=#67;_F10=#68;
  _CtrlF1=#94;  _CtrlF2=#95;  _CtrlF3=#96;  _CtrlF4=#97;  _CtrlF5=#98;
  _CtrlF6=#99;  _CtrlF7=#100; _CtrlF8=#101; _CtrlF9=#102; _CtrlF10=#103;
  _AltF1=#104;  _AltF2=#105;  _AltF3=#106;  _AltF4=#107;  _AltF5=#108;
  _AltF6=#109;  _AltF7=#110;  _AltF8=#111;  _AltF9=#112;  _AltF10=#113;
  _ShiftF1=#$54;
  _CtrlHome=#$77; _CtrlEnd=#$75;
  _EOF=^z;_CR=^m;_LF=^j;_ESC=^[;

  _F1_=$3B00;_F2_=$3C00;_F3_=$3D00;_F4_=$3E00;_F5_=$3F00;
  _F6_=$4000;_F7_=$4100;_F8_=$4200;_F9_=$4300;_F10_=$4400;
  _ShiftF1_=$5400;_ShiftF2_=$5500;_ShiftF3_=$5600;_ShiftF4_=$5700;
  _ShiftF5_=$5800;_ShiftF6_=$5900;_ShiftF7_=$5A00;_ShiftF8_=$5B00;
  _ShiftF9_=$5C00;_ShiftF10_=$5D00;
  _CtrlF1_=$5E00;_CtrlF2_=$5F00;_CtrlF3_=$6000;_CtrlF4_=$6100;
  _CtrlF5_=$6200;_CtrlF6_=$6300;_CtrlF7_=$6400;_CtrlF8_=$6500;
  _CtrlF9_=$6600;_CtrlF10_=$6700;
  _AltF1_=$6800;_AltF2_=$6900;_AltF3_=$6A00;_AltF4_=$6B00;_AltF5_=$6C00;
  _AltF6_=$6D00;_AltF7_=$6E00;_AltF8_=$6F00;_AltF9_=$7000;_AltF10_=$7100;

  _left_=$4B00;    _right_=$4D00;    _up_=$4800;      _down_=$5000;
  _Home_=$4700;    _End_=$4F00;      _PgUp_=$4900;    _PgDn_=$5100;
  _CtrlLeft_=$7300;_CtrlRight_=$7400;_CtrlPgUp_=$0300{!differs};_CtrlPgDn_=$7600;
  _CtrlHome_=$7700;_CtrlEnd_=$7500;
  _Ins_=$5200;     _Del_=$5300;  _Tab_=$9;  _ShiftTab_=$0F00;
  _AltEqual_=$8300;

  _ESC_=$1B;
  _A_=1;_B_=2;_C_=3;_D_=4;_E_=5;_F_=6;_G_=7;_H_=8;_I_=9;_J_=10;_K_=11;
  _L_=12;_M_=13;_N_=14;_O_=15;_P_=16;_Q_=17;_R_=18;_S_=19;_T_=20;
  _U_=21;_V_=22;_W_=23;_X_=24;_Y_=25;_Z_=26;

type
  TVideoFont = (foAscii,foLatin2,foKamen);
var
  KbdChar:word;
{$ifdef FPC}
  KbdFlgs:byte;
{$else}
  KbdFlgs:byte absolute 0:$417;
{$endif}
const
  DemoAutoRd:boolean=false;
  KbdBuffer:string='';
  LLKeyFlags:byte=0;

function CurrToKamen(C:char):char;
procedure ConvKamenToCurr(var Buf;L:word);
procedure ConvKamenLatin(var Buf;L:word;ToLatin:boolean);
function ToggleCS(C:char):char;
function NoDiakr(C:char):char;
procedure ConvToNoDiakr(var Buf;L:word;FromFont:TVideoFont);
procedure ClearKeyBuf;                           { Bios }
procedure ClearKbdBuf;                           { buffer + Bios }
function KeyPressed:boolean;                     { Bios }
function ReadKey:word;                           { Bios }
function KbdPressed:boolean;                     { buffer + Bios }
function ESCPressed:boolean;                     { other Bios input lost }
function ReadKbd:word;                           { buffer + Bios /+mouse/}
function KbdTimer(Delta:word;Kind:byte):boolean;
procedure AddToKbdBuf(KeyCode:word);
procedure BreakIntrInit;
procedure BreakIntrDone;

procedure Delay(N:word);
procedure Sound(N:word);
procedure NoSound;


var
  VideoCard:(viCga,viHercules,viEga,viVga);
  GraphDriver,GraphMode:integer;
  ScrSeg,ScrGrSeg:word;
  NrVFont,BytesPerChar:byte;
  ChkSnow:boolean;
  IsGraphMode:boolean;
  GrBytesPerChar:byte;
  GrBytesPerLine:word;       {bytesperchar * 80 bei Umschalten graphic }
procedure LoadVideoFont;
procedure ScrClr(X,Y,SizeX,SizeY:word;C:char;Color:byte);
procedure ScrWrChar(X,Y:word;C:char;Color:byte);
procedure ScrWrStr(X,Y:word;const S:string;Color:byte);
procedure ScrWrFrameLn(X,Y:word;Typ,Width,Color:byte);
procedure ScrWrBuf(X,Y:word;var Buf;L:word);
{$ifdef FPC}
function  TermSize(var Cols,Rows:word):boolean;
procedure ScrRepaintAll;
procedure InstallWinch;
{$endif}
procedure ScrRdBuf(X,Y:word;var Buf;L:word);
function ScrPush(X,Y,SizeX,SizeY:word):pointer;
procedure ScrPop(X,Y:word;P:pointer);
procedure ScrPopToGraph(X,Y,SizeX,SizeY:word;P:pointer;DOfs:word);
procedure ScrMove(X,Y,ToX,ToY,L:word);
procedure ScrColor(X,Y,L:word;Color:byte);

const
  MaxTxtCols=132; {the best adapter}
  MaxTxtRows=100;
  EventQSize = 16;
  BGIReload:boolean=true;
var
  LastWhere,LastWhereG,DownWhere:TPoint;
  WindMin,WindMax:record X,Y:byte end;
  TextAttr,StartAttr,StartMode:byte;
  LastMode:word;
  OldIntr08:pointer;
  FontArr,BGIDriver,BGILittFont,BGITripFont:pointer;
  ButtonCount,
  MouseButtons,LastButtons,DownButtons,
  LastDouble:byte;
  EventCount,EventQHead,EventQTail:word;
  EventQueue:array[0..EventQSize-1] of record
     Time,Buttons,
     X,Y,
     GX,GY :word {pixel}
     end;
  EventQLast:record end;
const
  Crs: record
    X,Y: word;
    Big,On,
    Enabled: boolean;
    Ticks: word;
  end= (X:0;Y:0;Big:false;On:false;Enabled:false;Ticks:0);
  MausExist:boolean=false;
function CrsGet:longint;
procedure CrsSet(S:longint);
procedure CrsShow;
procedure CrsHide;
procedure CrsBig;
procedure CrsNorm;
procedure CrsIntrInit;
procedure CrsIntrDone;
procedure GotoXY(X,Y:byte);
function WhereX:byte;
function WhereY:byte;
procedure Window(X1,Y1,X2,Y2:byte);
procedure ClrScr;
procedure ClrEol;
procedure TextBackGround(Color:byte);
procedure TextColor(Color:byte);
procedure InsLine;
procedure DelLine;
procedure beep;
procedure LockBeep;
procedure ScrBeep;


procedure InitMouseEvents;
procedure DoneMouseEvents;
procedure ShowMouse;
procedure HideMouse;
procedure ShowMaus;
procedure HideMaus;
procedure GetMouseEvent;
procedure SetMouse(X,Y:word;Visible:boolean);

function TestEvent:boolean;
function WaitEvent(Delta:word):word;
procedure GetEvent;
procedure ClrEvent;
function AddCtrlAltShift(Flgs:byte):word;


{$ifdef FPC}
var   Timer:longint;
{$else}
var   Timer:longint absolute 0:$46C;
{$endif}
const ofsTicks=$6C;

const FrameChars:array[0..20] of char=
         (#$da,#$c4,#$bf,#$c0,#$c4,#$d9,#$b3,' ',#$b3,
          #$c9,#$cd,#$bb,#$c8,#$cd,#$bc,#$ba,' ',#$ba,#$c3,#$c4,#$b4);
      MouseWhere:TPoint=(X:0;Y:0);
      MouseWhereG:TPoint=(X:0;Y:0);     {pixel}
      MausVisible:boolean=true;

{$ifdef FPC}
var FandBatch:boolean;
    BatchQuiet:boolean;
{ the terminal is gone - stdin ended, SIGHUP or SIGTERM: WaitEvent calls
  HangupProc (RunFand: save the files) and ends FAND }
var HangupProc:procedure;
    HangupRequested:boolean;
{ around a program FAND runs: the terminal as programs expect it (Msg on a
  cleared screen, if any), then FAND's raw mode and its screen again }
procedure TtyForChild(const Msg:string);
procedure TtyForFand;
{ a loop that waits for a key itself: up to Ms for input, Timer brought
  up to date (WaitEvent alone moves it) }
procedure IdleTick(Ms:word);
{$endif}

procedure AssignCrt(var T:text);


implementation

uses base,obaseww,wwmenu{$ifdef FPC},Crt,fandcp
     {$ifdef UNIX},BaseUnix,TermIO{$endif}
     {$ifdef MSWINDOWS},Windows{$endif}{$endif};

{$ifdef FPC}
procedure TPoint.Assign(XX,YY:word);
begin X:=XX; Y:=YY end;
constructor TObject.Init;
begin end;
destructor TObject.Done;
begin end;
procedure Free(P: PObject); begin if P <> nil then Dispose(P, Done) end;
const RawTtyEnabled: boolean = false;
const RawTtyTried: boolean = false;
procedure RestoreTty; forward;
procedure TermCursorDone; forward;
const TermCrsStale:boolean=true;
const
  TtyRestoreArmed: boolean = false;
  PrevExitProc: pointer = nil;
procedure FandRestoreExit; begin ExitProc := PrevExitProc; TermCursorDone; RestoreTty end;
procedure ArmTtyRestore;
begin
  if TtyRestoreArmed then exit;
  TtyRestoreArmed := true;
  PrevExitProc := ExitProc;
  ExitProc := @FandRestoreExit;
end;
{$ifdef UNIX}
procedure EnsureRawTty;
var rc: longint;
begin
  if RawTtyTried or FandBatch then exit;
  RawTtyTried := true;
  if IsATTY(0) = 0 then exit;
  rc := SysUtils.ExecuteProcess('/bin/stty', '-icanon -echo min 1 time 0');
  RawTtyEnabled := rc = 0; ArmTtyRestore;
end;
procedure RestoreTty; begin if RawTtyEnabled then SysUtils.ExecuteProcess('/bin/stty', 'sane') end;
{ select says "readable" at the end of input too: read a byte ahead, so
  that the end is seen as no key, not as a key that never comes }
const StdinNext:integer=-1; StdinEof:boolean=false;
function StdinReadAhead:boolean;
var b: byte; n: longint;
begin
  repeat n := fpRead(0, b, 1) until (n <> -1) or (fpgeterrno <> ESysEINTR);
  if n = 1 then StdinNext := b
  else if (n = 0) or (fpgeterrno <> ESysEAGAIN) then begin
    StdinEof := true; HangupRequested := true end;
  StdinReadAhead := StdinNext >= 0;
end;
function PollStdinMs(ms:word):boolean;
var fds: TFDSet; tv: TTimeVal; rc: longint;
begin
  PollStdinMs := false; if FandBatch then exit;
  if StdinNext >= 0 then begin PollStdinMs := true; exit end;
  if StdinEof then begin if ms > 0 then SysUtils.Sleep(ms); exit end;
  fpFD_ZERO(fds); fpFD_SET(0, fds);
  tv.tv_sec := ms div 1000; tv.tv_usec := (ms mod 1000) * 1000;
  rc := fpSelect(1, @fds, nil, nil, @tv);
  if rc > 0 then PollStdinMs := StdinReadAhead;
end;
function PollStdin: boolean;
begin PollStdin := PollStdinMs(0) end;
function ReadStdinByte: byte;
begin
  ReadStdinByte := 0;
  if (StdinNext < 0) and not StdinEof then StdinReadAhead;
  if StdinNext >= 0 then begin ReadStdinByte := StdinNext; StdinNext := -1 end;
end;
procedure TermWrite(var Buf; Len:longint);
begin if FandBatch then exit; fpWrite(1, Buf, Len); TermCrsStale:=true end;
{$else}
const
  ENABLE_VT_PROCESSING = $0004;
  ENABLE_EXT_FLAGS     = $0080;
  ENABLE_QUICK_EDIT    = $0040;
const
  OldInMode:  dword = 0;
  OldOutMode: dword = 0;
const StdinEof:boolean=false;
procedure EnsureRawTty;
var hIn,hOut:THandle; m:dword;
begin
  if RawTtyTried or FandBatch then exit;
  RawTtyTried := true;
  hIn  := GetStdHandle(STD_INPUT_HANDLE);
  hOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if GetConsoleMode(hIn,@OldInMode) then begin
    m := OldInMode and not dword(ENABLE_LINE_INPUT or ENABLE_ECHO_INPUT
                                or ENABLE_PROCESSED_INPUT or ENABLE_QUICK_EDIT
                                or ENABLE_MOUSE_INPUT or ENABLE_WINDOW_INPUT);
    m := m or ENABLE_EXT_FLAGS;
    RawTtyEnabled := SetConsoleMode(hIn, m);
  end;
  if GetConsoleMode(hOut,@OldOutMode) then
    SetConsoleMode(hOut, OldOutMode or ENABLE_VT_PROCESSING);
  ArmTtyRestore;
end;
procedure RestoreTty;
begin
  if OldInMode  <> 0 then SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldInMode);
  if OldOutMode <> 0 then SetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE),OldOutMode);
end;
const KeyBytes: string = '';
function KeyRecToBytes(const r:TInputRecord):string;
var vk,md:word; ch:char; ctrl,alt,shift:boolean; cs:dword; ms:string;
  function Nav(fin:char):string; begin if md=1 then Nav:=#27'['+fin else Nav:=#27'[1;'+ms+fin end;
  function Tilde(n:string):string;
  begin if md=1 then Tilde:=#27'['+n+'~' else Tilde:=#27'['+n+';'+ms+'~' end;
begin
  KeyRecToBytes := '';
  if (r.EventType <> KEY_EVENT) or (not r.Event.KeyEvent.bKeyDown) then exit;
  vk := r.Event.KeyEvent.wVirtualKeyCode;
  if vk in [VK_SHIFT,VK_CONTROL,VK_MENU,VK_CAPITAL,
            VK_NUMLOCK,VK_SCROLL,VK_LWIN,VK_RWIN] then exit;
  cs := r.Event.KeyEvent.dwControlKeyState;
  ctrl  := (cs and (LEFT_CTRL_PRESSED or RIGHT_CTRL_PRESSED)) <> 0;
  alt   := (cs and (LEFT_ALT_PRESSED  or RIGHT_ALT_PRESSED )) <> 0;
  shift := (cs and SHIFT_PRESSED) <> 0;
  md := 1; if shift then inc(md); if alt then inc(md,2); if ctrl then inc(md,4);
  str(md,ms);
  case vk of
    VK_UP:     begin KeyRecToBytes:=Nav('A'); exit end;
    VK_DOWN:   begin KeyRecToBytes:=Nav('B'); exit end;
    VK_RIGHT:  begin KeyRecToBytes:=Nav('C'); exit end;
    VK_LEFT:   begin KeyRecToBytes:=Nav('D'); exit end;
    VK_HOME:   begin KeyRecToBytes:=Nav('H'); exit end;
    VK_END:    begin KeyRecToBytes:=Nav('F'); exit end;
    VK_INSERT: begin KeyRecToBytes:=Tilde('2'); exit end;
    VK_DELETE: begin KeyRecToBytes:=Tilde('3'); exit end;
    VK_PRIOR:  begin KeyRecToBytes:=Tilde('5'); exit end;
    VK_NEXT:   begin KeyRecToBytes:=Tilde('6'); exit end;
    VK_F1..VK_F4:
      if md=1 then begin KeyRecToBytes:=#27'O'+chr(ord('P')+vk-VK_F1); exit end
      else begin KeyRecToBytes:=#27'[1;'+ms+chr(ord('P')+vk-VK_F1); exit end;
    VK_F5:  begin KeyRecToBytes:=Tilde('15'); exit end;
    VK_F6:  begin KeyRecToBytes:=Tilde('17'); exit end;
    VK_F7:  begin KeyRecToBytes:=Tilde('18'); exit end;
    VK_F8:  begin KeyRecToBytes:=Tilde('19'); exit end;
    VK_F9:  begin KeyRecToBytes:=Tilde('20'); exit end;
    VK_F10: begin KeyRecToBytes:=Tilde('21'); exit end;
    VK_F11: begin KeyRecToBytes:=Tilde('23'); exit end;
    VK_F12: begin KeyRecToBytes:=Tilde('24'); exit end;
    VK_TAB: if shift then begin KeyRecToBytes:=#27'[Z'; exit end;
  end;
  ch := r.Event.KeyEvent.AsciiChar;
  if ch = #0 then exit;
  (* AltGr comes as Ctrl+Alt with the character it makes (@ # $ [ ] and
     the braces on Czech and other keyboards): that character, not Alt+key *)
  if alt and ctrl and (ch >= ' ') then alt := false;
  if alt and (vk <> VK_ESCAPE) then KeyRecToBytes := #27 + ch
  else KeyRecToBytes := ch;
end;
function PollStdinMs(ms:word):boolean;
var
  h:THandle; avail,got,i:dword; deadline,now:qword; wait:dword;
  rec:array[0..15] of TInputRecord;
begin
  PollStdinMs := false; if FandBatch then exit;
  if KeyBytes <> '' then begin PollStdinMs := true; exit end;
  if StdinEof then begin if ms > 0 then Sleep(ms); exit end;
  h := GetStdHandle(STD_INPUT_HANDLE);
  deadline := SysUtils.GetTickCount64 + ms;
  repeat
    avail := 0;
    if not GetNumberOfConsoleInputEvents(h,avail) then begin
      PollStdinMs := WaitForSingleObject(h,ms) = WAIT_OBJECT_0;
      exit;
    end;
    if avail > 0 then begin
      if avail > 16 then avail := 16;
      if not PeekConsoleInput(h,rec[0],avail,got) then exit;
      for i := 0 to got-1 do
        if KeyRecToBytes(rec[i]) <> '' then begin PollStdinMs := true; exit end;
      if got > 0 then ReadConsoleInput(h,rec[0],got,got);
    end
    else begin
      now := SysUtils.GetTickCount64;
      if now >= deadline then exit;
      wait := deadline - now;
      if WaitForSingleObject(h,wait) <> WAIT_OBJECT_0 then exit;
    end;
  until false;
end;
function PollStdin: boolean;
begin PollStdin := PollStdinMs(0) end;
function ReadStdinByte: byte;
var b: byte; n,avail: dword; h:THandle; rec:TInputRecord; s:string; i:integer;
begin
  ReadStdinByte := 0;
  if KeyBytes = '' then begin
    h := GetStdHandle(STD_INPUT_HANDLE);
    if not GetNumberOfConsoleInputEvents(h,avail) then begin
      if ReadFile(h,b,1,n,nil) and (n=1) then ReadStdinByte := b
      else begin StdinEof := true; HangupRequested := true end;
      exit;
    end;
    for i := 1 to 32 do begin
      if not ReadConsoleInput(h,rec,1,n) or (n<>1) then exit;
      s := KeyRecToBytes(rec);
      if s <> '' then begin KeyBytes := s; break end;
    end;
    if KeyBytes = '' then exit;
  end;
  ReadStdinByte := byte(KeyBytes[1]);
  delete(KeyBytes,1,1);
end;
procedure TermWrite(var Buf; Len:longint);
var n:dword;
begin if FandBatch then exit; WriteFile(GetStdHandle(STD_OUTPUT_HANDLE),Buf,Len,n,nil); TermCrsStale:=true end;
{$endif}
{ The terminal's cursor goes where FAND's is, shown or hidden, underline or
  block (insert/overwrite), whenever input is awaited and it has changed. }
var CrsSent:record X,Y:word; On,Big:boolean end;
procedure TermCursor;
var s:string;
  function N(v:word):string; var r:string; begin str(v,r); N:=r end;
begin
  if FandBatch then exit;
  if not TermCrsStale and (CrsSent.On=Crs.Enabled) and (CrsSent.Big=Crs.Big) and
     (not Crs.Enabled or (CrsSent.X=Crs.X) and (CrsSent.Y=Crs.Y)) then exit;
  if Crs.Enabled then begin
    s:=#27'['+N(Crs.Y+1)+';'+N(Crs.X+1)+'H';
    if Crs.Big then s:=s+#27'[1 q' else s:=s+#27'[3 q';
    s:=s+#27'[?25h' end
  else s:=#27'[?25l';
  TermWrite(s[1],length(s)); TermCrsStale:=false;
  CrsSent.X:=Crs.X; CrsSent.Y:=Crs.Y; CrsSent.On:=Crs.Enabled; CrsSent.Big:=Crs.Big;
end;
procedure TermCursorDone;
const s:string[16]=#27'[0 q'#27'[?25h';
begin if not FandBatch then TermWrite(s[1],length(s)) end;
type TTermEnc=(teAuto,teCp852,teUtf8,teCp1250,teLat2);
const TermEnc:TTermEnc=teAuto;
function EncFromName(s:string):TTermEnc;
begin
  s:=UpperCase(s); EncFromName:=teAuto;
  if (s='CP852') or (s='852') or (s='IBM852') then EncFromName:=teCp852
  else if (s='UTF8') or (s='UTF-8') or (s='65001') then EncFromName:=teUtf8
  else if (s='CP1250') or (s='1250') or (s='WINDOWS-1250') then EncFromName:=teCp1250
  else if (s='LATIN2') or (s='ISO-8859-2') or (s='ISO8859-2') then EncFromName:=teLat2;
end;
function DetectTermEnc:TTermEnc;
var s:string; e:TTermEnc; {$ifdef MSWINDOWS}cp:longint;{$endif}
begin
  e:=EncFromName(GetEnv('FAND_ENCODING'));
  if e<>teAuto then begin DetectTermEnc:=e; exit end;
{$ifdef MSWINDOWS}
  cp:=GetConsoleOutputCP;
  if cp=852 then DetectTermEnc:=teCp852
  else if cp=65001 then DetectTermEnc:=teUtf8
  else if SetConsoleOutputCP(852) then DetectTermEnc:=teCp852
  else if cp=1250 then DetectTermEnc:=teCp1250
  else DetectTermEnc:=teCp852;
{$else}
  s:=GetEnv('LC_ALL'); if s='' then s:=GetEnv('LC_CTYPE');
  if s='' then s:=GetEnv('LANG'); s:=UpperCase(s);
  if pos('UTF',s)>0 then DetectTermEnc:=teUtf8
  else if (pos('8859-2',s)>0) or (pos('LATIN2',s)>0) then DetectTermEnc:=teLat2
  else if pos('852',s)>0 then DetectTermEnc:=teCp852
  else DetectTermEnc:=teUtf8;
{$endif}
end;
function TermEncoding:TTermEnc;
begin if TermEnc=teAuto then TermEnc:=DetectTermEnc; TermEncoding:=TermEnc end;
function Utf8ToCp852(lead:byte):word;
var u:longint; n,i:integer; c:byte;
begin
  Utf8ToCp852:=0;
  if lead<$E0 then begin u:=lead and $1F; n:=1 end
  else if lead<$F0 then begin u:=lead and $0F; n:=2 end
  else begin u:=lead and $07; n:=3 end;
  for i:=1 to n do begin
    if not PollStdinMs(40) then exit;
    c:=ReadStdinByte;
    if (c and $C0)<>$80 then exit;
    u:=(u shl 6) or (c and $3F);
  end;
  if u<$80 then begin Utf8ToCp852:=word(u); exit end;
  for i:=0 to 127 do
    if Cp852Uni[i]=word(u) then begin Utf8ToCp852:=word(i+$80); exit end;
end;
function ReadTermKey:word;
const
  FBase :array[0..3] of byte = ($3B,$54,$5E,$68);
  F11Base:array[0..3] of byte = ($85,$87,$89,$8B);
  AltLetter:array['A'..'Z'] of byte =
    ($1E,$30,$2E,$20,$12,$21,$22,$23,$17,$24,$25,$26,$32,$31,$18,$19,$10,$13,$1F,$14,$16,$2F,$11,$2D,$15,$2C);
  AltDigit:array['0'..'9'] of byte = ($81,$78,$79,$7A,$7B,$7C,$7D,$7E,$7F,$80);
var b,b2,c:byte; K:word; params:string; p1,p2,i,code,modi:integer; final:char;
  function Mod2Idx(m:integer):integer;
  begin
    case m of 2,4,6,8: Mod2Idx:=1; 5,7: Mod2Idx:=2; 3: Mod2Idx:=3; else Mod2Idx:=0 end;
  end;
  function FKey(n,m:integer):word;
  begin
    if n<=10 then FKey:=word(FBase[Mod2Idx(m)]+n-1) shl 8
    else FKey:=word(F11Base[Mod2Idx(m)]+n-11) shl 8;
  end;
  function NavKey(fin:char;m:integer):word;
  var ctrl:boolean; r:word;
  begin
    ctrl:=Mod2Idx(m)=2; r:=$1B;
    case fin of
      'A': if ctrl then r:=$8D00 else r:=$4800;
      'B': if ctrl then r:=$9100 else r:=$5000;
      'C': if ctrl then r:=$7400 else r:=$4D00;
      'D': if ctrl then r:=$7300 else r:=$4B00;
      'H': if ctrl then r:=$7700 else r:=$4700;
      'F': if ctrl then r:=$7500 else r:=$4F00;
    end;
    NavKey:=r;
  end;
begin
  { BP7 read Shift/Ctrl/Alt from the BIOS (KbdFlgs, Shift+arrows mark a
    block): here they come with the key }
  KbdFlgs := 0;
  b := ReadStdinByte; params := ''; final := #0; b2 := 0;
  if b >= $80 then case TermEncoding of
    teUtf8:   if b >= $C0 then begin ReadTermKey := Utf8ToCp852(b); exit end;
    teCp1250: begin
                if From1250[b-$80] <> 0 then b := From1250[b-$80];
                ReadTermKey := b; exit end;
    teLat2:   begin
                if FromLat2[b-$80] <> 0 then b := FromLat2[b-$80];
                ReadTermKey := b; exit end;
  end;
  if b <> $1B then begin
    if b = $0A then b := $0D;
    if b = $7F then b := $08;
    ReadTermKey := b; exit;
  end;
  if not PollStdinMs(40) then begin ReadTermKey := $1B; exit end;
  b2 := ReadStdinByte; K := $1B;
  if b2 = ord('[') then begin
    repeat
      if not PollStdinMs(40) then break;
      c := ReadStdinByte;
      if (chr(c) in ['0'..'9',';']) then begin if length(params)<16 then params:=params+chr(c) end
      else begin final := chr(c); break end;
    until false;
    p1 := 0; p2 := 0; i := pos(';',params);
    if i>0 then begin val(copy(params,1,i-1),p1,code); val(copy(params,i+1,255),p2,code) end
    else val(params,p1,code);
    modi := p2;
    if modi >= 2 then begin                    { xterm: 1 + Shift 1, Alt 2, Ctrl 4 }
      if (modi-1) and 1 <> 0 then KbdFlgs := KbdFlgs or $02;
      if (modi-1) and 4 <> 0 then KbdFlgs := KbdFlgs or $04;
      if (modi-1) and 2 <> 0 then KbdFlgs := KbdFlgs or $08 end;
    if final = 'Z' then KbdFlgs := $02;
    case final of
      'A','B','C','D','H','F': K := NavKey(final,modi);
      'P','Q','R','S': K := FKey(ord(final)-ord('P')+1,modi);
      'Z': K := $0F00;
      '~': case p1 of
             1,7: if Mod2Idx(modi)=2 then K:=$7700 else K:=$4700;
             4,8: if Mod2Idx(modi)=2 then K:=$7500 else K:=$4F00;
             2: K:=$5200; 3: K:=$5300;
             5: if Mod2Idx(modi)=2 then K:=$0300 else K:=$4900;
             6: if Mod2Idx(modi)=2 then K:=$7600 else K:=$5100;
             11..15: K := FKey(p1-10,modi);
             17..21: K := FKey(p1-11,modi);
             23,24:  K := FKey(p1-12,modi);
           end;
    end;
  end else if b2 = ord('O') then begin
    if PollStdinMs(40) then begin
      c := ReadStdinByte;
      case chr(c) of
        'P','Q','R','S': K := FKey(c-ord('P')+1,0);
        'A','B','C','D','H','F': K := NavKey(chr(c),0);
      end;
    end;
  end else if upcase(chr(b2)) in ['A'..'Z'] then begin
    K := word(AltLetter[upcase(chr(b2))]) shl 8; KbdFlgs := $08 end
  else if chr(b2) in ['0'..'9'] then begin K := word(AltDigit[chr(b2)]) shl 8; KbdFlgs := $08 end
  else if b2 = ord('=') then K := $8300
  else if b2 = $1B then K := $1B;
  ReadTermKey := K;
end;
{$I Keybd}
const ShadowCols=255; ShadowRows=MaxTxtRows;
      ShadowReady: boolean = false;
      ScrResized:  boolean = false;
      ResizeTick:  word    = 0;
var Shadow: array[0..ShadowRows-1,0..ShadowCols-1] of word;
procedure ShadowInit;
var y,x:word;
begin
  for y:=0 to ShadowRows-1 do for x:=0 to ShadowCols-1 do Shadow[y,x]:=$0720;
  ShadowReady:=true;
end;
procedure ScrSetAttr(Color:byte); begin Crt.TextAttr := Color end;
procedure ScrGoto(X,Y:word); begin if not FandBatch then Crt.GotoXY(X+1, Y+1) end;
procedure LoadVideoFont;
begin end;
{ the screen's codes below $20 (and $7F) are pictures in the DOS font -
  arrows, triangles, the Enter mark - but controls to a terminal }
const LowGlyph:array[0..31] of word=(
  $0020,$263A,$263B,$2665,$2666,$2663,$2660,$2022,$25D8,$25CB,$25D9,$2642,
  $2640,$266A,$266B,$263C,$25BA,$25C4,$2195,$203C,$00B6,$00A7,$25AC,$21A8,
  $2191,$2193,$2192,$2190,$221F,$2194,$25B2,$25BC);
      LowAscii:string[32]=' ****o*.o0oMF##*><|!PS-|^v><L-^v';
procedure TermEmit(X,Y:word; Attr:byte; const S:string);
const DosToAnsi: array[0..7] of byte = (0,4,2,6,1,5,3,7);
var buf:array[0..2047] of byte; cnt,i:integer; c:byte; u:word;
  procedure PutS(const t:string);
  var k:integer;
  begin for k:=1 to length(t) do begin buf[cnt]:=ord(t[k]); inc(cnt) end end;
  function NumS(v:word):string; var r:string; begin str(v,r); NumS:=r end;
begin
  if length(S)=0 then exit;
  cnt:=0;
  PutS(#27'['+NumS(Y+1)+';'+NumS(X+1)+'H'+#27'[0;');
  if Attr and 8<>0 then PutS(NumS(90+DosToAnsi[Attr and 7]))
                   else PutS(NumS(30+DosToAnsi[Attr and 7]));
  PutS(';'+NumS(40+DosToAnsi[(Attr shr 4) and 7])+'m');
  case TermEncoding of
   teCp852,teCp1250,teLat2:
     for i:=1 to length(S) do begin
       c:=ord(S[i]);
       if c<$20 then c:=ord(LowAscii[c+1])
       else if c=$7F then c:=ord('^')
       else if c>=$80 then
         if TermEncoding=teCp1250 then c:=Cp852To1250[c-$80]
         else if TermEncoding=teLat2 then c:=Cp852ToLat2[c-$80];
       buf[cnt]:=c; inc(cnt);
     end;
   else
     for i:=1 to length(S) do begin
       c:=ord(S[i]);
       if (c>=$20) and (c<$7F) then begin buf[cnt]:=c; inc(cnt) end
       else begin
         if c<$20 then u:=LowGlyph[c] else if c=$7F then u:=$2302
         else u:=Cp852Uni[c-$80];
         if u<$80 then begin buf[cnt]:=u; inc(cnt) end
         else if u<$800 then begin
           buf[cnt]:=$C0 or (u shr 6); buf[cnt+1]:=$80 or (u and $3F); inc(cnt,2)
         end else begin
           buf[cnt]:=$E0 or (u shr 12);
           buf[cnt+1]:=$80 or ((u shr 6) and $3F);
           buf[cnt+2]:=$80 or (u and $3F); inc(cnt,3)
         end;
       end;
       if cnt>2040 then begin TermWrite(buf,cnt); cnt:=0 end;
     end;
  end;
  if cnt>0 then TermWrite(buf,cnt);
end;
{$ifdef UNIX}
var FandTio:Termios;
{$endif}
const FandTtySaved:boolean=false;
procedure TtyForChild(const Msg:string);
var t:string;
begin
  FandTtySaved:=false;
  if FandBatch {$ifdef UNIX}or (IsATTY(0)=0){$endif} then exit;
  {$ifdef UNIX}if TCGetAttr(0,FandTio)<>0 then exit;{$endif}
  t:=#27'[0m'#27'[?25h'#27'[0 q'; if Msg<>'' then t:=t+#27'[H'#27'[2J';
  TermWrite(t[1],length(t));
  if Msg<>'' then begin TermEmit(0,0,$07,Msg); t:=#13#10; TermWrite(t[1],2) end;
  {$ifdef UNIX}SysUtils.ExecuteProcess('/bin/stty','sane');{$else}RestoreTty;{$endif}
  FandTtySaved:=true;
end;
procedure TtyForFand;
begin
  if not FandTtySaved then exit; FandTtySaved:=false;
  {$ifdef UNIX}TCSetAttr(0,TCSANOW,FandTio);{$else}RawTtyTried:=false; EnsureRawTty;{$endif}
  TermCrsStale:=true; ScrRepaintAll;
end;
procedure ScrWrBufRaw(X,Y:word;var Buf;L:word);
type TBufArr = array[0..65534] of word;
var i,j:word; pw:^TBufArr; attr:byte; s:string;
begin
  if L=0 then exit;
  pw := @Buf; i:=0;
  while i<L do begin
    attr:=hi(pw^[i]); j:=i; s:='';
    while (j<L) and (hi(pw^[j])=attr) and (length(s)<200) do begin
      s:=s+char(lo(pw^[j])); inc(j) end;
    TermEmit(X+i, Y, attr, s);
    i:=j;
  end;
end;
procedure ScrWrBuf(X,Y:word;var Buf;L:word);
type TBufArr = array[0..65534] of word;
var i,j:word; pw:^TBufArr; attr:byte; s:string;
begin
  if L=0 then exit;
  if not ShadowReady then ShadowInit;
  if (Y>=TxtRows) or (X>=TxtCols) then exit;
  if X+L>TxtCols then L:=TxtCols-X;
  pw := @Buf; i:=0;
  while i<L do begin
    attr:=hi(pw^[i]); j:=i; s:='';
    while (j<L) and (hi(pw^[j])=attr) and (length(s)<200) do begin
      s:=s+char(lo(pw^[j]));
      if (Y<ShadowRows) and (X+j<ShadowCols) then Shadow[Y,X+j]:=pw^[j];
      inc(j) end;
    TermEmit(X+i, Y, attr, s);
    i:=j;
  end;
end;
procedure ScrRepaintAll;
var y,x:word; row:array[0..ShadowCols-1] of word;
begin
  if not ShadowReady then begin ShadowInit; exit end;
  for y:=0 to TxtRows-1 do begin
    for x:=0 to TxtCols-1 do row[x]:=Shadow[y,x];
    ScrWrBufRaw(0,y,row,TxtCols);
  end;
end;
procedure ScrClr(X,Y,SizeX,SizeY:word;C:char;Color:byte);
var j,i:word; buf:array[0..ShadowCols-1] of word;
begin
  if (SizeX=0) or (SizeY=0) then exit;
  if SizeX>ShadowCols then SizeX:=ShadowCols;
  for i:=0 to SizeX-1 do buf[i]:=word(Color) shl 8 or ord(C);
  for j := 0 to SizeY-1 do ScrWrBuf(X,Y+j,buf,SizeX);
end;
procedure ScrWrChar(X,Y:word;C:char;Color:byte);
var w:word;
begin w:=word(Color) shl 8 or ord(C); ScrWrBuf(X,Y,w,1) end;
procedure ScrWrStr(X,Y:word;const S:string;Color:byte);
var buf:array[0..255] of word; i:word;
begin
  if length(S)=0 then exit;
  for i:=1 to length(S) do buf[i-1]:=word(Color) shl 8 or ord(S[i]);
  ScrWrBuf(X,Y,buf,length(S));
end;
procedure ScrWrFrameLn(X,Y:word;Typ,Width,Color:byte);
var s:string;
begin
  s[0]:=char(Width); s[1]:=FrameChars[Typ]; s[Width]:=FrameChars[Typ+2];
  FillChar(s[2],Width-2,FrameChars[Typ+1]); ScrWrStr(X,Y,s,Color);
end;
procedure ScrRdBuf(X,Y:word;var Buf;L:word);
type TBufArr = array[0..65534] of word;
var i:word; pw:^TBufArr;
begin
  if L=0 then exit;
  if not ShadowReady then ShadowInit;
  pw := @Buf;
  for i := 0 to L-1 do
    if (Y<ShadowRows) and (X+i<ShadowCols) then pw^[i]:=Shadow[Y,X+i]
    else pw^[i] := $0720;
end;
procedure ScrPush1(X,Y,SizeX,SizeY:word;P:pointer);
type TBufArr = array[0..65534] of word;
var pw:^TBufArr; j:word;
begin
  pw := P; pw^[0] := SizeX; pw^[1] := SizeY;
  if (SizeX=0) or (SizeY=0) then exit;
  for j := 0 to SizeY-1 do ScrRdBuf(X,Y+j,pw^[2+j*SizeX],SizeX);
end;
function ScrPush(X,Y,SizeX,SizeY:word):pointer;
var p:pointer;
begin p := GetStore(SizeX*SizeY*2+4); ScrPush := p; ScrPush1(X,Y,SizeX,SizeY,p) end;
procedure ScrPop(X,Y:word;P:pointer);
type TBufArr = array[0..65534] of word;
var pw:^TBufArr; SizeX,SizeY:word; j:word;
begin
  pw := P; SizeX := pw^[0]; SizeY := pw^[1];
  if (SizeX=0) or (SizeY=0) then exit;
  if (SizeX>TxtCols) or (SizeY>TxtRows) or (X>=TxtCols) or (Y>=TxtRows) then exit;
  for j := 0 to SizeY-1 do ScrWrBuf(X,Y+j,pw^[2+j*SizeX],SizeX);
end;
procedure ScrPopToGraph(X,Y,SizeX,SizeY:word;P:pointer;DOfs:word);
begin end;
procedure ScrMove(X,Y,ToX,ToY,L:word);
var buf: array[0..ShadowCols-1] of word;
begin if L > ShadowCols then L := ShadowCols; ScrRdBuf(X,Y,buf,L); ScrWrBuf(ToX,ToY,buf,L) end;
procedure ScrColor(X,Y,L:word;Color:byte);
var buf: array[0..ShadowCols-1] of word; i: word;
begin
  if L > ShadowCols then L := ShadowCols;
  if L=0 then exit;
  ScrRdBuf(X,Y,buf,L);
  for i := 0 to L-1 do buf[i] := word(Color) shl 8 or lo(buf[i]);
  ScrWrBuf(X,Y,buf,L);
end;
procedure CrsShow; begin Crs.Enabled := true end;
procedure CrsHide; begin Crs.Enabled := false end;
procedure CrsBig; begin Crs.Big := true; CrsShow end;
procedure CrsNorm; begin Crs.Big := false; CrsShow end;
{ as in BP7: Big, Enabled, X, Y in the four bytes }
function CrsGet:longint;
begin
  CrsGet := ord(Crs.Big) or (ord(Crs.Enabled) shl 8) or
            (longint(Crs.X and $FF) shl 16) or (longint(Crs.Y and $FF) shl 24);
end;
procedure CrsSet(S:longint);
begin
  CrsHide;
  Crs.Big := (S and $FF)<>0;
  Crs.X := (S shr 16) and $FF;
  Crs.Y := (S shr 24) and $FF;
  if not FandBatch then Crt.GotoXY(Crs.X+1, Crs.Y+1);
  if (S shr 8) and $FF<>0 then CrsShow;
end;
procedure CrsIntrInit; begin end;
procedure CrsIntrDone; begin end;
procedure GotoXY(X,Y:byte);
var AX,AY: word;
begin
  AX := word(X)-1 + WindMin.X;
  AY := word(Y)-1 + WindMin.Y;
  if AX > WindMax.X then AX := WindMin.X;
  if AY > WindMax.Y then AY := WindMin.Y;
  Crs.X := AX; Crs.Y := AY;
  if not FandBatch then Crt.GotoXY(AX+1, AY+1);
end;
function WhereX:byte;
begin WhereX := Crs.X - WindMin.X + 1 end;
function WhereY:byte;
begin WhereY := Crs.Y - WindMin.Y + 1 end;
procedure Window(X1,Y1,X2,Y2:byte);
begin
  if (X1>X2) or (Y1>Y2) then exit;
  if (X1<1) or (Y1<1) then exit;
  if (X2>TxtCols) or (Y2>TxtRows) then exit;
  WindMin.X := X1-1; WindMin.Y := Y1-1;
  WindMax.X := X2-1; WindMax.Y := Y2-1;
  Crs.X := WindMin.X; Crs.Y := WindMin.Y;
  if not FandBatch then Crt.GotoXY(X1, Y1);
end;
procedure ClrScr;
begin
  ScrClr(WindMin.X, WindMin.Y,
         WindMax.X-WindMin.X+1, WindMax.Y-WindMin.Y+1,
         ' ', TextAttr);
  Crs.X := WindMin.X; Crs.Y := WindMin.Y;
  if not FandBatch then Crt.GotoXY(WindMin.X+1, WindMin.Y+1);
end;
procedure ClrEol; begin ScrClr(Crs.X, Crs.Y, WindMax.X-Crs.X+1, 1, ' ', TextAttr) end;
procedure Scroll(X,Y,SizeX,SizeY:word;Up:boolean);
begin
  if Up then
    ScrClr(X, Y+SizeY-1, SizeX, 1, ' ', TextAttr)
  else
    ScrClr(X, Y, SizeX, 1, ' ', TextAttr);
end;
procedure InsLine;
begin Scroll(WindMin.X, Crs.Y, WindMax.X-WindMin.X+1, WindMax.Y-Crs.Y+1, false) end;
procedure DelLine;
begin Scroll(WindMin.X, Crs.Y, WindMax.X-WindMin.X+1, WindMax.Y-Crs.Y+1, true) end;
procedure TextBackGround(Color:byte);
begin TextAttr := (TextAttr and $0F) or ((Color and $07) shl 4); Crt.TextAttr := TextAttr end;
procedure TextColor(Color:byte);
begin TextAttr := (TextAttr and $F0) or (Color and $0F); Crt.TextAttr := TextAttr end;
procedure Delay(N:word); begin SysUtils.Sleep(N * 55) end;
procedure Sound(N:word);
begin end;
procedure NoSound;
begin end;
procedure Beep;
begin system.Write(^G) end;
procedure LockBeep;
begin if spec.LockBeepAllowed then Beep end;
procedure ScrBeep;
const bell:char=#7;
begin if spec.Beep then TermWrite(bell, 1) end;
procedure InitMouseEvents;
begin
  MausExist := false;
  Event.What := 0;
  MouseButtons := 0; LastButtons := 0; DownButtons := 0;
  LastDouble := 0; EventCount := 0;
  EventQHead := 0; EventQTail := 0;
end;
procedure DoneMouseEvents; begin end;
procedure ShowMouse; begin end;
procedure HideMouse; begin end;
procedure ShowMaus; begin end;
procedure HideMaus; begin end;
procedure GetMouseEvent; begin Event.What:=0 end;
procedure SetMouse(X,Y:word;Visible:boolean); begin end;
procedure GetKeyEvent;
var K: word; L: byte;
begin
  Event.What := 0;
  BreakCheck;
  L := ord(KbdBuffer[0]);
  if L > 0 then begin
    K := ord(KbdBuffer[1]);
    if (K = 0) and (L > 1) then begin
      K := word(ord(KbdBuffer[2])) shl 8; Delete(KbdBuffer,1,2) end
    else Delete(KbdBuffer,1,1);
    if K = $0300 then K := 0
    else if K = $8400 then K := $0300;
  end else begin
    EnsureRawTty;
    if not PollStdin then exit;
    K := ReadTermKey;
  end;
  Event.What := evKeyDown;
  Event.KeyCode := K;
  KbdChar := K;
end;
procedure GetMouseKeyEvent; begin GetMouseEvent; if Event.What = 0 then GetKeyEvent end;
procedure TestGlobalKey;
var i:word;
const InMenu6:boolean=false; const InMenu8:boolean=false;
begin
  if Event.What=evKeyDown then case Event.KeyCode of
     _AltF8_:if not InMenu8 then begin ClrEvent; InMenu8:=true;
             i:=Menu(45,ord(spec.KbdTyp)+1);
             if i<>0 then spec.KbdTyp:=TKbdConv(i-1); InMenu8:=false end;
     _AltF6_:if not InMenu6 then begin
             ClrEvent; InMenu6:=true; PrinterMenu(46); InMenu6:=false end;
     _ESC_:  if LLKeyFlags<>0 then begin LLKeyFlags:=0; ClrEvent end;
  end;
end;
function AddCtrlAltShift(Flgs:byte):word;
begin
  if Event.What <> evKeyDown then begin AddCtrlAltShift:=0; exit end;
  if (Flgs and $04)<>0 then begin
    case Event.KeyCode of
      _Home_: Event.KeyCode:=_CtrlHome_;
      _End_:  Event.KeyCode:=_CtrlEnd_;
      ord('Y'): Event.KeyCode:=_Y_;
    end;
  end;
  if Event.KeyCode >= _F1_ then begin
    if (Flgs and $04)<>0 then inc(Event.KeyCode, _CtrlF1_-_F1_)
    else if (Flgs and $08)<>0 then inc(Event.KeyCode, _AltF1_-_F1_)
    else if (Flgs and $03)<>0 then inc(Event.KeyCode, _ShiftF1_-_F1_);
  end;
  TestGlobalKey;
  AddCtrlAltShift := 0;
end;
function TestEvent:boolean;
label 1;
begin
1:if Event.What=0 then GetMouseKeyEvent;
  if Event.What=0 then GetKeyEvent;
  if Event.What=0 then TestEvent:=false
  else begin TestGlobalKey; if Event.What=0 then goto 1; TestEvent:=true end;
end;
function TermSize(var Cols,Rows:word):boolean;
{$ifdef UNIX}
var w:TWinSize;
begin
  TermSize:=false;
  if (fpIOCtl(1,TIOCGWINSZ,@w)<>0) and (fpIOCtl(0,TIOCGWINSZ,@w)<>0)
     and (fpIOCtl(2,TIOCGWINSZ,@w)<>0) then exit;
  if (w.ws_col=0) or (w.ws_row=0) then exit;
  if w.ws_col>Cols then Cols:=w.ws_col;
  if w.ws_row>Rows then Rows:=w.ws_row;
  TermSize:=true;
end;
{$else}
var info:TConsoleScreenBufferInfo;
begin
  TermSize:=false;
  if not GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE),info) then exit;
  if info.srWindow.Right>=info.srWindow.Left+79 then
    Cols:=info.srWindow.Right-info.srWindow.Left+1;
  if info.srWindow.Bottom>=info.srWindow.Top+24 then
    Rows:=info.srWindow.Bottom-info.srWindow.Top+1;
  TermSize:=true;
end;
{$endif}
{$ifdef UNIX}
procedure WinchHandler(sig:longint); cdecl;
begin ScrResized:=true end;
procedure HangupHandler(sig:longint); cdecl;
begin HangupRequested:=true end;
{$endif}
procedure InstallWinch;
begin
{$ifdef UNIX}
  fpSignal(SIGWINCH,SignalHandler(@WinchHandler));
  if not FandBatch then begin    { batch runs never wait for keys: let them die }
    fpSignal(SIGHUP,SignalHandler(@HangupHandler));
    fpSignal(SIGTERM,SignalHandler(@HangupHandler)) end;
{$endif}
end;
procedure PollResize;
{$ifdef MSWINDOWS}
var c,r:word;
begin
  inc(ResizeTick);
  if ResizeTick < 8 then exit;
  ResizeTick := 0;
  c:=80; r:=25;
  if TermSize(c,r) then begin
    if c>MaxTxtCols then c:=MaxTxtCols;
    if r>MaxTxtRows then r:=MaxTxtRows;
    if (c<>TxtCols) or (r<>TxtRows) then ScrResized:=true end;
end;
{$else}
begin end;
{$endif}
procedure ApplyResize;
var c,r:word;
begin
  ScrResized:=false;
  c:=80; r:=25;
  if not TermSize(c,r) then exit;
  if c>MaxTxtCols then c:=MaxTxtCols;
  if r>MaxTxtRows then r:=MaxTxtRows;
  if (c=TxtCols) and (r=TxtRows) then begin ScrRepaintAll; exit end;
  TxtCols:=c; TxtRows:=r;
  WindMax.X:=c-1; WindMax.Y:=r-1;
  ScrRepaintAll;
end;
procedure CheckHangup;
begin
  if HangupRequested then begin
    { no keys and no screen from here on: a message while closing gets Esc }
    HangupRequested := false; FandBatch := true;
    if @HangupProc <> nil then HangupProc;
    halt(1) end;
end;
procedure IdleTick(Ms:word);
begin
  if not FandBatch then PollStdinMs(Ms);
  Timer := longint(SysUtils.GetTickCount64 div 55);
  CheckHangup;
end;
function WaitEvent(Delta:word):word;
var t:longint; Flgs:byte; idle:boolean;
begin
  Flgs := KbdFlgs;
  Timer := longint(SysUtils.GetTickCount64 div 55);
  t := Timer;
  repeat
    Timer := longint(SysUtils.GetTickCount64 div 55);
    CheckHangup;
    if ScrResized then ApplyResize;
    if Event.What=0 then GetMouseKeyEvent;
    if Event.What=0 then GetKeyEvent;
    if (Event.What=0) and FandBatch then begin
      Event.What:=evKeyDown; Event.KeyCode:=_ESC_ end;
    if Event.What <> 0 then begin WaitEvent:=0; TestGlobalKey; exit end;
    { a terminal says nothing while a key is just held: no stale flags }
    if (Delta<>0) and (Timer > t+Delta) then begin KbdFlgs:=0; WaitEvent:=2; exit end;
    TermCursor;
    idle := PollStdinMs(20);
    PollResize;
  until false;
end;
procedure GetEvent;
begin repeat WaitEvent(0) until Event.What<>0 end;
procedure ClrEvent;
begin Event.What:=0 end;
function KbdPressed:boolean;
begin
  KbdPressed:=true;
  if length(KbdBuffer)>0 then exit;
  if PollStdin then exit;
  Event.What:=0; GetMouseKeyEvent;
  if Event.What=evKeyDown then begin
    AddToKbdBuf(Event.KeyCode); ClrEvent; exit end;
  KbdPressed:=false;
end;
function ESCPressed:boolean;
begin
  ESCPressed:=true;
  if PollStdin then begin
    if ReadKey=_ESC_ then exit end
  else begin
    GetMouseKeyEvent;
    if Event.What=evKeyDown then begin
      if Event.KeyCode=_ESC_ then begin ClrEvent; exit end end;
    ClrEvent end;
  ESCPressed:=false;
end;
function ReadKbd:word;
begin
  while Event.What<>evKeyDown do begin ClrEvent; GetEvent end;
  ReadKbd:=Event.KeyCode; KbdChar:=Event.KeyCode; ClrEvent;
end;
{$ifdef Trial}
function getSec:longint;
var h,m,s,ss:word; begin getTime(h,m,s,ss); getSec:=h*3600+m*60+s; end;
procedure TestTrial;
var now:longint;
begin
  if (trialStartFand=0) then begin trialStartFand:=getSec; trialInterval:=900 end
  else begin now:=getSec;
    if now>trialStartFand+trialInterval then begin
      trialStartFand:=now; trialInterval:=trialInterval div 3;
      if trialInterval<10 then trialInterval:=10;
      WrLLF10Msg(71) end end;
end;
{$endif}
procedure BreakIntrInit; begin end;
procedure BreakIntrDone; begin end;
procedure ConLineFeed;
var y,w:word;
begin
  if Crs.Y<WindMax.Y then inc(Crs.Y)
  else if WindMax.Y>WindMin.Y then begin
    w:=WindMax.X-WindMin.X+1;
    for y:=WindMin.Y to WindMax.Y-1 do ScrMove(WindMin.X,y+1,WindMin.X,y,w);
    ScrClr(WindMin.X,WindMax.Y,w,1,' ',TextAttr);
  end;
end;
function WrOutput(var F:TextRec):integer;
var i:longint; c:char; s:string; e:ansistring;
  procedure FlushS;
  begin if length(s)=0 then exit; ScrWrStr(Crs.X,Crs.Y,s,TextAttr); inc(Crs.X,length(s)); s:='' end;
begin
  if FandBatch then begin
    if not BatchQuiet then begin
      e:=''; for i:=0 to longint(F.BufPos)-1 do e:=e+F.BufPtr^[i];
      write(StdErr,S852ToUtf8(e)) end;
    F.BufPos:=0; WrOutput:=0; exit end;
  s:='';
  for i:=0 to longint(F.BufPos)-1 do begin
    c:=F.BufPtr^[i];
    case c of
      #7: begin FlushS; ScrBeep end;
      #8: begin FlushS; if Crs.X>WindMin.X then dec(Crs.X) end;
      #10: begin FlushS; ConLineFeed end;
      #13: begin FlushS; Crs.X:=WindMin.X end;
    else begin
      s:=s+c;
      if Crs.X+length(s)-1>=WindMax.X then begin FlushS; Crs.X:=WindMin.X; ConLineFeed end
      else if length(s)=200 then FlushS;
    end end;
  end;
  FlushS; F.BufPos:=0;
  if not FandBatch then Crt.GotoXY(Crs.X+1,Crs.Y+1);
  WrOutput:=0;
end;
function DummyCrt(var F:TextRec):integer;
begin DummyCrt:=0 end;
function OpenCrt(var F:TextRec):integer;
begin F.InOutFunc:=@WrOutput; F.FlushFunc:=@WrOutput; F.CloseFunc:=@DummyCrt; OpenCrt:=0 end;
procedure AssignCrt(var T:text);
var F:TextRec absolute T;
begin
  FillChar(F,sizeof(F),0);
  F.Mode:=fmClosed; F.BufSize:=sizeof(F.Buffer); F.BufPtr:=@F.Buffer;
  F.OpenFunc:=@OpenCrt; F.Name[0]:=#0;
  F.LineEnd:=#13#10;
end;
begin
  FandBatch:=(copy(ParamStr(2),1,2)='--') or (ParamStr(1)='--help');
  {$ifdef UNIX}
  { the Crt unit made the terminal raw before FAND knew it runs a batch:
    its messages would stair-step; Crt restores the user's setting at exit }
  if FandBatch and (IsATTY(1)=1) then SysUtils.ExecuteProcess('/bin/stty','sane');
  {$endif}
  { the terminal set up before any output (Windows: VT processing, which
    the first screen needs) and its restore armed now, so that it runs
    after Base's MyExit has cleared the screen - exit procedures run last
    armed first }
  if not FandBatch then EnsureRawTty;
{$else}
{$S-}

const
  MausRefresh:boolean=false;
var
  AutoTicks,DownTicks,AutoDelay:word;
  OldBreakIntr,OldKbdIntr:pointer;


const
  Gr640x350=$10; Gr640x480=$12; Txt80x25=$03;
  GDC_port=$3ce;
  SetRes_reg=0; Enable_reg=1; Func_reg=3; Map_reg=4; Mode_reg=5;
  ColorCare_reg=7; Mask_reg=8;
  SEQ_port=$3c4;
  Seq2_reg=2;Seq4_reg=4;


const
  CrsTimeOn=$0003; CrsTimeOff=$0005;
                                            

{$I Keybd}

                    {  S C R E E N  }

procedure GetMonoColor; assembler;
asm  cmp ah,0; je @3; cmp ah,10H; jb @2;
     and ah,0fH; cmp ah,0; jne @1; mov ah,10H; jmp @4;
@1:  mov ah,51H; jmp @4;
@2:  cmp ah,0bH; jb @3; mov ah,05; jmp @4;
@3:  mov ah,1;
@4: end;

procedure EgaWriteArr(X,Y,L:word;From:pointer); assembler;
asm  mov cx,L; jcxz @3;
     mov ax,ScrGrSeg; mov es,ax;
     mov ax,Y; mul GrBytesPerLine; add ax,X; mov di,ax;  { screen addr }
     mov dx,GDC_port; cld;
     mov ax,Mode_reg+0A00H; out dx,ax;        { write mode 2, read mode 1 }
     mov ax,Func_reg+0000H; out dx,ax;        { replace }
     mov ax,ColorCare_reg;  out dx,ax;   { don't care all planes=> get 0fH}
@1:  push di; push cx;
     push ds; lds si,From; lodsw; mov From.word,si; pop ds; { get char + color}                 
     cmp GraphDriver,EGAMono; jne @11; call GetMonoColor
@11: mov bl,ah; and bl,0fH;                    { separate colors    }
     mov cl,4; shr ah,cl; mov bh,ah;
@12: xor ch,ch; mov cl,GrBytesPerChar;         { for all font lines }
     mul GrBytesPerChar.byte; push ds; lds si,FontArr; add si,ax;
     mov al,Mask_reg;
@2:  mov ah,[si]; out dx,ax;                   { font line as mask  }
     and es:[di],bl;                           { in forground color }
     not ah; out dx,ax;                        { invers font line   }
     and es:[di],bh;                           { in background color}
     inc si; add di,80;
     loop @2;
     pop ds; pop cx; pop di; inc di;
     loop @1;                                  { normal values to regs }
     mov ax,Mode_reg; out dx,ax; mov ax,Mask_reg+0FF00H; out dx,ax;
     mov ax,ColorCare_reg+0f00H; out dx,ax;
@3:  end;

procedure EgaScroll(X,Y,SizeX,SizeY:word;Up:boolean); assembler;
asm  mov dx,GDC_port; cld;
     mov ax,Mode_reg+0100H; out dx,ax;        { write mode 1, read mode 0 }
     mov ax,Y; cmp Up,0; jne @1; add ax,SizeY;
@1:  mul GrBytesPerLine; add ax,X; mov di,ax;    { screen addr - destin. }
     cmp Up,0; jne @2;
     sub di,80; mov si,di; sub si,GrBytesPerLine; mov dx,-80; jmp @3;
@2:  mov si,di; add si,GrBytesPerLine; mov dx,80; { screen addr - source }
@3:  mov ax,SizeY; dec ax; mov cl,GrBytesPerChar;
     mul cl; mov cx,ax;                       { no. of scan lines to move }
     mov ax,ScrGrSeg; mov es,ax; jcxz @5;
     push ds; mov ds,ax;
@4:  push cx; push si; push di; mov cx,SizeX;
     rep movsb;
     pop di; add di,dx; pop si; add si,dx; pop cx; loop @4;
     pop ds;
@5:  push dx; mov dx,GDC_port;
     mov ax,Mode_reg+0200H; out dx,ax;         { write mode 2 }
     mov ax,Mask_reg+0ff00H; out dx,ax;        { all pixels }
     mov ax,Func_reg+0000H; out dx,ax;         { overwrite  }
     pop dx;                                                               
     mov al,TextAttr; mov cl,4; shr al,cl;     { background color }
     xor ch,ch; mov cl,GrBytesPerChar;
@6:  push cx; push di; mov cx,SizeX;
     rep stosb;                                { fill with backgr. color }
     pop di; add di,dx; pop cx; loop @6;
     mov dx,GDC_port; mov ax,Mode_reg; out dx,ax;  { impl. register value }
end;

procedure HercGetOfs; assembler; { ax=scan line, dx=X, uses bx -> ax=ofs, cl }
asm  xor bx,bx; shr ax,1; rcr bx,1; shr ax,1; rcr bx,1; shr bx,1;
     mov ah,90; mul ah; add bx,ax;                    { bx=scan line offset }
     mov ax,dx; shl ax,1; shl ax,1; shl ax,1; add ax,dx;   {X*9}
     mov cl,al; and cl,7;                             { cl=bit offset }
     shr ax,1; shr ax,1; shr ax,1; add ax,bx;         { ax=char offset }
end;

procedure HercWriteArr(X,Y,L:word;From:pointer); assembler;
var FontPtr:pointer;
asm  push ds; cld;
     mov ax,ScrGrSeg; mov es,ax;
     lds si,FontArr; mov FontPtr.word,si; mov FontPtr[2].word,ds;
     mov ax,Y; mov cl,4; shl ax,cl; sub ax,Y; sub ax,Y;
     cmp Y,24; jne @1; dec ax;                    { line:= Y*14 [-1] }
@1:  mov dx,X; call HercGetOfs; mov di,ax;
     mov bx,L;
@2:  cmp bx,0; je @10;
     push bx; push di;

     lds si,From; lodsw; mov From.word,si; mov bx,ax; xor ah,ah;
     mov ch,14;                             { for all font lines }
     mul ch; lds si,FontPtr; add si,ax;
     mov dx,7f00H; ror dx,cl;               { 9-bit mask }
@3:  cmp ch,1; jne @4; cmp Y,24; je @8;     { Y=24  only 13 scan lines}
@4:  and es:[di],dx;                        { clear relevant bits }
     lodsb;                                 { load font line }
     cmp ch,2; jne @5; cmp bl,' '; je @5;
     mov ah,bh; and ah,07H; cmp ah,01H; jne @5;   { underlined color ? };
     mov al,0ffH;
@5:  cmp bh,70H; jne @6;                     { invers color ? }
     xor al,0ffH;
@6:  xor ah,ah; ror ax,1; rcl al,1;         { copy lowest to highest bit }
     ror ax,cl; or es:[di],ax;              { store at bit position    }
     add di,2000H;                          { next line }
     jns @7;
     add di,90-8000H;                       { switch to first memory page }
@7:  dec ch; jnz @3;

@8:  pop di; pop bx; dec bx;
     inc di; inc cl; cmp cl,8; jb @2; inc di; xor cl,cl; jmp @2;

@10: pop ds;
end;

procedure CrsDraw; assembler;
var Top:word;
asm  xor ch,ch; mov cl,GrBytesPerChar; mov Top,0;
     cmp Crs.Big,0; jne @0; sub cx,3; mov Top,cx; mov cx,2;
@0:  mov ax,ScrGrSeg; mov es,ax; cld;
     cmp VideoCard,viHercules; jne @4;

     mov ch,cl;                               { line:= Y*14 [-1] }
     mov ax,Crs.Y; mov cl,4; shl ax,cl; sub ax,Crs.Y; sub ax,Crs.Y;
     cmp Crs.Y,24; jne @1; dec ax; cmp ch,14; jne @1; dec ch;
@1:  add ax,Top; mov dx,Crs.X; call HercGetOfs; mov di,ax;
     mov ax,00ffH; ror ax,cl;
@2:  xor es:[di],ax;                          { reverse at bit position    }
     add di,2000H;                            { next scan line }
     jns @3;
     add di,90-8000H;                         { switch to first memory page }
@3:  dec ch; jne @2;
     jmp @6;

@4:  mov ax,Crs.Y; mul GrBytesPerChar; add ax,Top;
     mov dx,80; mul dx; add ax,Crs.X; mov di,ax;
     mov dx,GDC_port;
     mov ax,Mode_reg+0A00H; out dx,ax;        { write mode 2, read mode 1 }
     mov ax,Func_reg+1800H; out dx,ax;        { xor }
     mov ax,ColorCare_reg;  out dx,ax;   { don't care all planes=> get 0fH}
     mov ax,Mask_reg+0ff00H; out dx,ax;        { all pixels }
     mov al,07H;
@5:  and es:[di],al;
     add di,80; loop @5;
     mov ax,Mode_reg; out dx,ax; mov ax,Func_reg; out dx,ax;
     mov ax,ColorCare_reg+0f00H; out dx,ax;              { impl. values }

@6:  end;


procedure LoadVideoFont;
var p:pointer;
begin
  if IsGraphMode or (NrVFont=0) then exit;
  HideMaus;
  ResFile.Get(NrVFont,p);
asm  mov bh,BytesPerChar.byte;
     push bp; mov ax,1110H; {load user font} xor bl,bl; {font nr.0}
     mov cx,256; xor dx,dx; {frst char} les bp,p; int 10H; pop bp end;
  ReleaseStore(p);
  if not Crs.Enabled then
    asm push bp; mov cx,2000H; mov ah,1; int 10H; pop bp end; { cursor off }
  ShowMaus;
end;

procedure ScrGetPtr; assembler;   {ax:Y, di:X,  cx,si unchanged}
asm  mul TxtCols; shl ax,1; shl di,1; add di,ax;
     mov ax,ScrSeg; mov es,ax end;

procedure HideMausIn; assembler; {ax:Y, di:X, cx:SizeX}
asm  cmp MausVisible,0; je @4; cmp IsGraphMode,0; jne @1;
     cmp MouseWhere.Y,ax; jne @4; cmp MouseWhere.X,di; jb @4;
     push ax; push di; push cx;
     add di,cx; cmp MouseWhere.X,di; jae @3; jmp @2;
@1:  push ax; push di; push cx;
@2:  mov ax,2; int 33H; mov MausRefresh,1;
@3:  pop cx; pop di; pop ax;
@4:  end;

procedure ScrWr; assembler; {dx:si=source, es:di=dest, cx, ah=Color}
asm  push ds; jcxz @6; cld; cmp ChkSnow,0; mov ds,dx; jne @2;
@1:  lodsb; stosw; loop @1; jmp @6;      {move to screen }
@2:  mov dx,03daH; mov bh,ah;            {CGA avoid snow}
@3:  lodsb; mov bl,al; mov ah,9;
@4:  in al,dx; rcr al,1; jb @4; cli;
@5:  in al,dx; and al,ah; je @5; mov ax,bx; stosw; sti; loop @3;
@6:  pop ds end;

procedure ScrPopToGraph(X,Y,SizeX,SizeY:word;P:pointer;DOfs:word);
var i:word; pofs:word absolute P;
begin
  for i:=1 to SizeY do begin
    asm cli end;
    if VideoCard=viHercules then HercWriteArr(X,Y,SizeX,p)
    else EgaWriteArr(X,Y,SizeX,p);
    asm sti end; inc(pofs,DOfs shl 1); inc(Y) end;
end;

procedure ScrClr(X,Y,SizeX,SizeY:word;C:char;Color:byte);
var p:pointer;
begin
  HideMaus;
asm  mov ax,Y; mov di,X; call ScrGetPtr; mov p.word,di; mov p[2].word,es;
     mov cx,SizeY; jcxz @7; mov al,C; mov ah,Color; cld;
@1:  push cx; push di; mov cx,SizeX; jcxz @6;     { for all rows   }
     cmp ChkSnow,0; jne @2;
     rep stosw; jmp @6;                           { paint row      }
@2:  push bp; mov bp,ax; mov dx,03daH;  { CGA avoid snow }
@3:  mov ah,9;
@4:  in al,dx; rcr al,1; jb @4; cli;
@5:  in al,dx; and al,ah; je @5; mov ax,bp; stosw; sti; loop @3; pop bp;
@6:  pop di; add di,TxtCols; add di,TxtCols; pop cx; loop @1;
@7:  end;
  if IsGraphMode then ScrPopToGraph(X,Y,SizeX,SizeY,p,TxtCols);
  ShowMaus;
end;

procedure ScrWrChar(X,Y:word;C:char;Color:byte); assembler;
asm  mov ax,Y; mov di,X; mov cx,1; call HideMausIn; call ScrGetPtr;
     mov dx,ss; lea si,C;
     push X; push Y; push cx; push es; push di; {..WriteArr parameters}
     mov ah,Color; call ScrWr;
     cmp IsGraphMode,0; je @2;
     cmp VideoCard,viHercules; jne @1;
     cli; call HercWriteArr; sti; jmp @3;
@1:  cli; call EgaWriteArr; sti; jmp @3;
@2:  add sp,5*2;
@3:  call ShowMaus end;

procedure ScrWrStr(X,Y:word;const S:string;Color:byte); assembler;
asm  mov ax,Y; les si,S; xor ch,ch; mov cl,es:[si]; mov di,X; call HideMausIn;
     call ScrGetPtr; cld; mov dx,S[2].word; inc si;
     push X; push Y; push cx; push es; push di; {..WriteArr parameters}
     mov ah,Color; call ScrWr;
     cmp IsGraphMode,0; je @2;
     cmp VideoCard,viHercules; jne @1;
     cli; call HercWriteArr; sti; jmp @3;
@1:  cli; call EgaWriteArr; sti; jmp @3;
@2:  add sp,5*2;
@3:  call ShowMaus end;

procedure ScrWrFrameLn(X,Y:word;Typ,Width,Color:byte);
var s:string;
begin
  s[0]:=char(Width); s[1]:=FrameChars[Typ]; s[Width]:=FrameChars[Typ+2];
  FillChar(s[2],Width-2,FrameChars[Typ+1]); ScrWrStr(X,Y,s,Color);
end;

procedure ScrWrBuf(X,Y:word;var Buf;L:word); assembler;
asm  mov ax,Y; mov di,X; mov cx,L; jcxz @9; call HideMausIn; call ScrGetPtr;
     push X; push Y; push cx; push es; push di; {..WriteArr parameters}
     push ds; cld; cmp ChkSnow,0; lds si,Buf; jne @1;
     rep movsw; jmp @5;   {move to screen}
@1:  mov dx,03daH;
@2:  lodsw; mov bx,ax; mov ah,9;      {CGA avoid snow}
@3:  in al,dx; rcr al,1; jb @3; cli;
@4:  in al,dx; and al,ah; je @4; mov ax,bx; stosw; sti; loop @2;
@5:  pop ds;
     cmp IsGraphMode,0; je @7;
     cmp VideoCard,viHercules; jne @6;
     cli; call HercWriteArr; sti; jmp @8;
@6:  cli; call EgaWriteArr; sti; jmp @8;
@7:  add sp,5*2;
@8:  call ShowMaus;
@9:  end;

procedure ScrRdBuf(X,Y:word;var Buf;L:word); assembler;
asm  mov ax,Y; mov di,X; mov cx,L; jcxz @5; call HideMausIn; call ScrGetPtr;
     push ds; mov ax,es; mov si,di; les di,Buf; cld;
     cmp ChkSnow,0; mov ds,ax; jne @1;
     rep movsw; jmp @4;   {move from screen}
@1:  mov dx,03daH;        { avoid snow     }
@2:  in al,dx; rcr al,1; jb @2; cli;
@3:  in al,dx; rcr al,1; jnb @3; lodsw; sti; stosw; loop @2;
@4:  pop ds; call ShowMaus;
@5:  end;

procedure ScrPush1(X,Y,SizeX,SizeY:word;P:pointer);
var snow:boolean;
begin
  snow:=ChkSnow; HideMaus;
asm  push ds; cld; mov bx,TxtCols; shl bx,1;
     mov ax,Y; mov di,X; call ScrGetPtr; push es; pop ds; mov si,di;
     les di,P; mov ax,SizeX; stosw; mov ax,SizeY; stosw;
     mov cx,SizeY; mov dx,03daH;
@1:  push si; push cx; mov cx,SizeX; cmp snow,0; jne @2;
     rep movsw; jmp @4;
@2:  in al,dx; rcr al,1; jb @2; cli;
@3:  in al,dx; rcr al,1; jnb @3; lodsw; sti; stosw; loop @2;
@4:  pop cx; pop si; add si,bx; loop @1;
     pop ds end;
  ShowMaus;
end;

function ScrPush(X,Y,SizeX,SizeY:word):pointer;
var p:pointer;
begin
  p:=GetStore(SizeX*SizeY*2+4); ScrPush:=p; ScrPush1(X,Y,SizeX,SizeY,p);
end;


procedure ScrPop(X,Y:word;P:pointer);
var snow:boolean; SizeX,SizeY:word; pofs:word absolute p;
begin
  snow:=ChkSnow; HideMaus;
asm  push ds; cld; mov bx,TxtCols; shl bx,1;
     mov ax,Y; mov di,X; call ScrGetPtr;
     lds si,P; lodsw; mov SizeX,ax; lodsw; mov SizeY,ax;
     mov cx,SizeY; mov dx,03daH;
@1:  push di; push cx; mov cx,SizeX; cmp snow,0; jne @2;
     rep movsw; jmp @6;
@2:  push bp;
@3:  lodsw; mov bp,ax; mov ah,9;
@4:  in al,dx; rcr al,1; jb @4; cli;
@5:  in al,dx; and al,ah; je @5; mov ax,bp; stosw; sti; loop @3; pop bp;
@6:  pop cx; pop di; add di,bx; loop @1;
     pop ds end;
  if IsGraphMode then begin
    inc(pofs,4);
    ScrPopToGraph(X,Y,SizeX,SizeY,P,SizeX) end;
  ShowMaus;
end;

procedure ScrMove(X,Y,ToX,ToY,L:word);
var p:pointer;
begin
asm  call HideMaus; push ds; mov bl,ChkSnow; cld;
     mov ax,Y; mov di,X; call ScrGetPtr; mov si,di;
     mov ax,ToY; mov di,ToX; mov cx,L; call ScrGetPtr;
     mov p.word,di; mov p[2].word,es; push es; pop ds; mov dx,03daH;
     cmp bl,0; jne @1;
     rep movsw; jmp @4;
@1:  mov ah,9;
@2:  in al,dx; rcr al,1; jb @2; cli;
@3:  in al,dx; and al,ah; je @3; lodsw; stosw; sti; loop @1;
@4:  pop ds end;
  if IsGraphMode then begin
    asm cli end;
    if VideoCard=viHercules then HercWriteArr(ToX,ToY,L,p)
    else EgaWriteArr(ToX,ToY,L,p);
    asm sti end end;
  ShowMaus;
end;

procedure ScrColor(X,Y,L:word;Color:byte);
var p:pointer;
begin
asm  mov ax,Y; mov di,X; mov cx,L; call HideMausIn; call ScrGetPtr;
     mov p.word,di; mov p[2].word,es; mov bl,Color; mov dx,03daH; cld;
     cmp ChkSnow,0; jne @2;
     mov al,bl; @1:inc di; stosb; loop @1; jmp @5;
@2:  mov ah,9; inc di;
@3:  in al,dx; rcr al,1; jb @3; cli;
@4:  in al,dx; and al,ah; je @4; mov al,bl; stosb; sti; loop @2;
@5:  end;
  if IsGraphMode then begin
    asm cli end;
    if VideoCard=viHercules then HercWriteArr(X,Y,L,p)
    else EgaWriteArr(X,Y,L,p);
    asm sti end end;
  ShowMaus;
end;

procedure CrsDark; assembler;
asm  mov ax,Crs.Y; mov di,Crs.X; call ScrGetPtr; mov cx,1;
     push Crs.X; push Crs.Y; push cx; push es; push di;  {..WriteArr parameters}
     cmp VideoCard,viHercules; jne @1;
     call HercWriteArr; jmp @3;
@1:  call EgaWriteArr; jmp @3;
@2:  add sp,5*2;
@3:  end;

procedure CrsBlink; assembler;
asm  cmp Crs.Enabled,0; je @2;
     mov ax,Crs.Ticks; inc ax; mov Crs.Ticks,ax;
     cmp Crs.On,0; je @1;
     cmp Crs.Ticks,CrsTimeOn; jb @2;
     call CrsDraw; mov Crs.On,0; mov Crs.Ticks,0; jmp @2;
@1:  cmp Crs.Ticks,CrsTimeOff; jb @2;
     call CrsDraw; mov Crs.On,1; mov Crs.Ticks,0;
@2:  end;

procedure CrsShow;
begin
  if Crs.Enabled then exit;
  if IsGraphMode then begin
    Crs.On:=false; Crs.Ticks:=CrsTimeOff end
  else asm mov cx,video.CursOn; cmp Crs.Big,0; je @1; mov cx,video.CursBig;
@1:        mov ah,1; int 10H end;
  Crs.Enabled:=true;
end;

procedure CrsHide;
begin
  if not Crs.Enabled then exit; Crs.Enabled:=false;
  if IsGraphMode then CrsDark else
  asm mov cx,video.CursOff; mov ah,1; int 10H end;
end;

procedure CrsBig;
begin
  if not Crs.Big then begin CrsHide; Crs.Big:=true end; CrsShow;
end;

procedure CrsNorm;
begin
  if Crs.Big then begin CrsHide; Crs.Big:=false end; CrsShow;
end;

procedure CrsGotoXY(aX,aY:word);
var b:boolean;
begin with Crs do begin
  if IsGraphMode and Enabled then begin
    Enabled:=false; CrsDark;
    X:=aX; Y:=aY; On:=false; Ticks:=CrsTimeOff; Enabled:=true end
  else begin
    X:=aX; Y:=aY; if not IsGraphMode then
    asm  mov dh,Crs.Y.byte; mov dl,Crs.X.byte;
         push bp; mov ah,2; xor bh,bh; int 10H; pop bp end end;
end end;

procedure CrsGotoDX; assembler;
asm  xor ah,ah; mov al,dl; push ax; mov al,dh; push ax; call CrsGotoXY end;

function CrsGet:longint; assembler;
asm  mov dl,Crs.X.byte; mov dh,Crs.Y.byte;
     mov al,Crs.Big; mov ah,Crs.Enabled end;

procedure CrsSet(S:longint); assembler;
asm  call CrsHide; mov al,S.byte; mov Crs.Big,al; xor ah,ah;
     mov al,S[2].byte; push ax; mov al,S[3].byte; push ax; call CrsGotoXY;
     cmp S[1].byte,0; je @1; call CrsShow;
@1:  end;

procedure CrsIntr08; interrupt;
begin
asm  pushf; call dword ptr OldIntr08; mov al,20H; out 20H,al;
     cli; call CrsBlink; sti;
end end;

procedure CrsIntrInit;
begin if IsGraphMode then begin
  GetIntVec($08,OldIntr08); SetIntVec($08,@CrsIntr08);
end end;

procedure CrsIntrDone;
  begin if IsGraphMode then SetIntVec($08,OldIntr08) end;


procedure Beep; begin write(^g) end;
procedure LockBeep; begin if spec.LockBeepAllowed then Beep end;

procedure Window(X1,Y1,X2,Y2:byte); assembler;
asm  mov dl,X1; mov dh,Y1; mov cl,X2; mov ch,Y2;
     cmp dl,cl; ja @1; cmp dh,ch; ja @1;
     dec dl; js @1; dec dh; js @1;
     cmp cl,TxtCols.byte; ja @1; dec cl; cmp ch,TxtRows.byte; ja @1; dec ch;
     mov WindMin,dx; mov WindMax,cx;
     call CrsGotoDX;
@1:  end;

procedure ClrScr;
begin
  ScrClr(WindMin.X,WindMin.Y,WindMax.X-WindMin.X+1,WindMax.Y-WindMin.Y+1,
         ' ',TextAttr);
  asm mov dx,WindMin; call CrsGotoDX end;
end;
procedure ClrEol;
begin
  ScrClr(Crs.X,Crs.Y,WindMax.X-Crs.X+1,1,' ',TextAttr);
end;

procedure Scroll(X,Y,SizeX,SizeY:word;Up:boolean); assembler;
var WasCursor:boolean;
asm  call HideMaus; cld; mov ax,Y; mov di,X; mov cx,SizeY; dec cx;
     cmp Up,0; jne @1; add ax,cx;
@1:  call ScrGetPtr; mov bl,ChkSnow; jcxz @9;
     mov dx,TxtCols; shl dx,1; cmp Up,0; jne @2; neg dx;
@2:  push ds; mov ax,es; mov ds,ax; mov si,di;
@3:  push cx; add si,dx; push si; mov cx,SizeX;
     cmp bl,0; jne @4;
     rep movsw; jmp @8;
@4:  push dx; mov dx,03daH;
@5:  mov ah,9;
@6:  in al,dx; rcr al,1; jb @6; cli;
@7:  in al,dx; and al,ah; je @7; lodsw; stosw; sti; loop @5;
     pop dx;
@8:  pop si; mov di,si; pop cx; loop @3;
     pop ds;
@9:  mov ah,TextAttr; mov al,20H; mov cx,SizeX;
     cmp bl,0; jne @10;
     rep stosw; jmp @14;                         { fill with blanks }
@10: push bp; mov bp,ax; mov dx,03daH;          { CGA avoid snow }
@11: mov ah,9;
@12: in al,dx; rcr al,1; jb @12; cli;
@13: in al,dx; and al,ah; je @13; mov ax,bp; stosw; sti; loop @11; pop bp;
@14: cmp IsGraphMode,0; je @20;
     mov al,Crs.Enabled; mov WasCursor,al; cmp al,0; je @15; call CrsHide;
@15: cmp VideoCard,viHercules; jne @16;
     mov ax,Y; mov di,X; call ScrGetPtr;
     push X; push Y; push SizeX; push SizeY; push es; push di; push TxtCols;
     call ScrPopToGraph; jmp @17;
@16: push X; push Y; push SizeX; push SizeY; xor ah,ah; mov al,Up; push ax;
     call EgaScroll;
@17: cmp WasCursor,0; je @20; call CrsShow;
@20: call ShowMaus end;

procedure InsLine;
begin
  Scroll(WindMin.X,Crs.Y,WindMax.X-WindMin.X+1,WindMax.Y-Crs.Y+1,false);
end;

procedure DelLine;
begin
  Scroll(WindMin.X,Crs.Y,WindMax.X-WindMin.X+1,WindMax.Y-Crs.Y+1,true);
end;

procedure GotoXY(X,Y:byte); assembler;
asm  mov dl,X; dec dl;
     add dl,WindMin.X; jc @1; cmp dl,WindMax.X; ja @1;
     mov dh,Y; dec dh;
     add dh,WindMin.Y; jc @1; cmp dh,WindMax.Y; jna @2;
@1:  mov dx,WindMin;
@2:  call CrsGotoDX end;

function WhereX:byte; assembler;
asm  mov ax,Crs.X; sub al,WindMin.X; inc al;
end;

function WhereY:byte; assembler;
asm  mov ax,Crs.Y; sub al,WindMin.Y; inc al;
end;

procedure TextBackGround(Color:byte); assembler;
asm  mov al,Color; and al,7; mov cl,4; shl al,cl;
     and TextAttr,0fH; or TextAttr,al end;

procedure TextColor(Color:byte); assembler;
asm  mov al,Color; and al,0fH; and TextAttr,0f0H; or TextAttr,al end;

procedure WrDirect; assembler;   {es:si=buffer, di-si=length, bx=cursor}
asm  cmp si,di; je @1; push cx; push dx; push di; push es;
     mov cx,di; sub cx,si; dec si; mov al,es:[si]; push ax; push si;
     mov es:[si],cl; xor ah,ah;
     mov al,bl; push ax; mov al,bh; push ax; push es; push si;
     mov al,TextAttr; push ax;
     call ScrWrStr;
     pop si; pop ax; pop es; pop di; pop dx; pop cx; mov es:[si],al;
@1:  end;

procedure ScrollUp;
begin
  Scroll(WindMin.X,WindMin.Y,WindMax.X-WindMin.X+1,WindMax.Y-WindMin.Y+1,true);
end;

procedure LineFeed; assembler;
asm  inc dh; cmp dh,WindMax.Y; jbe @1;
     dec dh; push cx; push dx; push es; push di;
     call ScrollUp;
     pop di; pop es; pop dx; pop cx;
@1:  end;

procedure ScrBeep; assembler;
asm  cmp spec.Beep,0; je @1;
     push cx; push dx; push si; push di; push es; push bp;
     mov ax,880; push ax; call Sound;
     mov ax,6;{ 6 timer units} push ax; call Delay;
     call NoSound;
     pop bp; pop es; pop di; pop si; pop dx; pop cx;
@1:  end;

function WrOutput(var F:TextRec):word; far; assembler;
asm  call BreakCheck;
     les di,F; mov cx,es:[di].TextRec.BufPos; sub es:[di].TextRec.BufPos,cx;
     jcxz @9; les di,es:[di].TextRec.BufPtr;
     mov dl,Crs.X.byte; mov dh,Crs.Y.byte; mov bx,dx; mov si,di;
@1:  mov al,es:[di]; cmp al,07H{^g}; je @2; cmp al,08H{^h}; je @3;
     cmp al,0aH{^j}; je @4; cmp al,0dH{^m}; je @5;
     inc di; inc dl; cmp dl,WindMax.X; jbe @8;
     call WrDirect; call LineFeed; mov dl,WindMin.X; jmp @7;
@2:  call WrDirect; call ScrBeep; jmp @6;
@3:  call WrDirect; cmp dl,WindMin.X; je @6; dec dl; jmp @6;
@4:  call WrDirect; call LineFeed; jmp @6;
@5:  call WrDirect; mov dl,WindMin.X;
@6:  inc di;
@7:  mov si,di; mov bx,dx;
@8:  loop @1;
     call WrDirect;
     call CrsGotoDX;
@9:  mov ax,0 end;
                                              
function DummyCrt(var F:TextRec):word; far;
  begin DummyCrt:=0 end;

function OpenCrt(var F:TextRec):word; far;
begin with F do begin
  InOutFunc:=@WrOutput; FlushFunc:=@WrOutput; CloseFunc:=@DummyCrt end;
  OpenCrt:=0;
end;

procedure AssignCrt(var T:text);
var F:TextRec absolute T;
begin with F do begin
  Mode:=fmClosed; BufSize:=128; BufPtr:=@Buffer;
  OpenFunc:=@OpenCrt; Name[0]:=#0;
end end;

{$endif}
end.