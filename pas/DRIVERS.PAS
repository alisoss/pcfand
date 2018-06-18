{$I Switches}
{$A-,B-,F+,G-,I+,O-,R-,S+,V+,X+,L-}
unit Drivers;

interface

uses dos,graph;

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
  KbdFlgs:byte absolute 0:$417;
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
procedure ScrRdBuf(X,Y:word;var Buf;L:word);
function ScrPush(X,Y,SizeX,SizeY:word):pointer;
procedure ScrPop(X,Y:word;P:pointer);
procedure ScrPopToGraph(X,Y,SizeX,SizeY:word;P:pointer;DOfs:word);
procedure ScrMove(X,Y,ToX,ToY,L:word);
procedure ScrColor(X,Y,L:word;Color:byte);

const
  MaxTxtCols=132; {the best adapter}
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


var   Timer:longint absolute 0:$46C;
const ofsTicks=$6C;

const FrameChars:array[0..20] of char=
         ('Ú','Ä','¿','À','Ä','Ù','³',' ','³',
          'É','Í','»','È','Í','¼','º',' ','º','Ã','Ä','´');
      MouseWhere:TPoint=(X:0;Y:0);
      MouseWhereG:TPoint=(X:0;Y:0);     {pixel}
      MausVisible:boolean=true;


procedure AssignCrt(var T:text);


implementation

uses base,obaseww,wwmenu;

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

end.