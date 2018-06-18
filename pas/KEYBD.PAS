
                      { T P O I N T , T O B J E C T }


procedure TPoint.Assign(XX,YY:word); assembler;
asm les di,Self; mov ax,XX; mov es:[di].TPoint.X,ax;
    mov ax,YY; mov es:[di].TPoint.Y,ax end;

constructor TObject.Init;
type
  Image = record Link: Word; Data: record end end;
begin
  FillChar(Image(Self).Data, SizeOf(Self) - SizeOf(TObject), 0);
end;

destructor TObject.Done;
begin
end;

procedure Free(P: PObject);
begin
  if P <> nil then Dispose(P, Done);
end;


                     { K E Y B O A R D }


const
  ofsHeadKeyBuf=$1A;
  ofsTailKeyBuf=$1C;    {Bios}
  BreakFlag:boolean=false;
  diHacek=#1; diCarka=#2; diUmlaut=#3;
  Diak:char=#0; {diHacek,diCarka}
  TabKtL:array[$80..$ff] of byte=  { Kamenicky to Latin2 }
      ($ac,$81,$82,$d4,$84,$d2,$9b,$9f,$d8,$b7,$91,$d6,$96,$92,$8e,$b5,
       $90,$a7,$a6,$93,$94,$e0,$85,$e9,$ec,$99,$9a,$e6,$95,$ed,$fc,$9c,
       $a0,$a1,$a2,$a3,$e5,$d5,$de,$e2,$e7,$fd,$ea,$e8,$9e,$f5,$ae,$af,
       $b0,$b1,$b2,$b3,$b4,$8b,$ba,$fb,$eb,$b9,$ba,$bb,$bc,$bd,$be,$bf,
       $c0,$c1,$c2,$c3,$c4,$c5,$c6,$ba,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,
       $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df,
       $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,
       $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);
  TabLtK:array[$80..$ff] of byte=  {Latin2 to Kamenicky }
      ($80,$81,$82,$83,$84,$96,$86,$87,$88,$89,$b6,$b5,$8c,$8d,$8e,$8f,
       $90,$8a,$8d,$93,$94,$9c,$8c,$97,$98,$99,$9a,$86,$9f,$9d,$9e,$87,
       $a0,$a1,$a2,$a3,$a4,$a5,$92,$91,$a8,$a9,$a0,$ab,$80,$ad,$ae,$af,
       $b0,$b1,$b2,$b3,$b4,$8f,$b6,$89,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf,
       $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,
       $d0,$d1,$85,$d3,$83,$a5,$8b,$d7,$88,$d9,$da,$db,$dc,$dd,$a6,$df,
       $95,$e1,$a7,$e3,$e4,$a4,$9b,$a8,$ab,$97,$aa,$55,$98,$9d,$ee,$ef,
       $f0,$f1,$f2,$f3,$f4,$ad,$f6,$f7,$f8,$f9,$fa,$75,$9e,$a9,$fe,$ff);
  TabKtN:array[$80..$ff] of byte=  { Kamenicky to NoDiakr }
      ($43,$75,$65,$64,$61,$44,$54,$63,$65,$45,$4c,$49,$6c,$6c,$41,$41,
       $45,$7a,$5a,$6f,$6f,$4f,$75,$55,$79,$99,$9a,$53,$4c,$59,$52,$74,
       $61,$69,$6f,$75,$6e,$4e,$55,$4f,$73,$72,$72,$52,$ac,$ad,$ae,$af,
       $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf,
       $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,
       $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df,
       $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,
       $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);
  TabLtN:array[$80..$ff] of byte=   { Latin2 to NoDiakr }
      ($43,$75,$65,$61,$61,$75,$63,$63,$6c,$65,$4f,$6f,$69,$5a,$41,$43,
       $45,$4c,$6c,$6f,$6f,$4c,$6c,$53,$73,$4f,$55,$54,$74,$4c,$9e,$63,
       $61,$69,$6f,$75,$41,$61,$5a,$7a,$45,$65,$61,$7a,$43,$73,$ae,$af,
       $b0,$b1,$b2,$b3,$b4,$41,$41,$45,$53,$b9,$ba,$bb,$bc,$5a,$7a,$bf,
       $c0,$c1,$c2,$c3,$c4,$c5,$41,$61,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,
       $64,$44,$44,$45,$64,$4e,$49,$49,$65,$d9,$da,$db,$dc,$54,$55,$df,
       $4f,$e1,$4f,$4e,$6e,$6e,$53,$73,$52,$55,$72,$55,$79,$59,$74,$ef,
       $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$75,$52,$72,$fe,$ff);
  CsKbdSize=67;
  CaKbdSize=15;
  TabCsKbd: array[1..CsKbdSize*3] of char=  { Ascii to CS keyboard }
      ('1',#2,'+','2',#3,'à','3',#4,'®','4',#5,'á','5',#6,'©',
       '6',#7,'ë','7',#8,'ò','8',#9,'†','9',#$a,'°','0',#$b,'Ç',
       ',',#$53,'.',{from german keyboard}
       '=',#$d,diCarka,'#',#$d,diCarka,'+',#$d,diHacek,'''',#$d,diHacek,

       '!',#2,'1','@',#3,'2','"',#3,'2','#',#4,'3','',#4,'3','$',#5,'4',
       '%',#6,'5','^',#7,'6','&',#7,'6','&',#8,'7','/',#8,'7','*',#9,'8',
       '(',#9,'8','(',#$a,'9',')',#$a,'9',')',#$b,'0','=',#$b,'0',
       '-',#$c,'=','·',#$c,'=','_',#$c,'%','?',#$c,'%',
       '[',#$1a,'£','Å',#$1a,'£','{',#$1a,'/','ö',#$1a,'/',
       ']',#$1b,')','+',#$1b,')','}',#$1b,'(','*',#$1b,'(',
       ';',#$27,'ñ','î',#$27,'ñ',':',#$27,'"','ô',#$27,'"',
       '''',#$28,'≠','Ñ',#$28,'≠','"',#$28,'!','é',#$28,'!',
       '~',#$29,';','^',#$29,';', '\',#$2b,diUmlaut,'|',#$2b,'''',
       '<',#$33,'?',';',#$33,'?', '>',#$34,':','/',#$35,'-','?',#$35,'_',
       'Y',#$15,'Z','y',#$15,'z','Z',#$2c,'Y','z',#$2c,'y',
       #25{_Y_},#$15,#26{_Z_},#26{_Z_},#$2c,#25{_Y_});
  SlKbdSize=63;
  TabSlKbd: array[1..SlKbdSize*3] of char=  { Ascii to Slov. keyboard }
      ('1',#2,'+','2',#3,'å','3',#4,'®','4',#5,'á','5',#6,'ü',
       '6',#7,'ë','7',#8,'ò','8',#9,'†','9',#$a,'°','0',#$b,'Ç',
       ',',#$53,'.',{from german keyboard}
       '=',#$d,diCarka,'#',#$d,diCarka,'+',#$d,diHacek,'''',#$d,diHacek,

       '!',#2,'1','@',#3,'2','"',#3,'2','#',#4,'3','',#4,'3','$',#5,'4',
       '%',#6,'5','^',#7,'6','&',#7,'6','&',#8,'7','/',#8,'7','*',#9,'8',
       '(',#9,'8','(',#$a,'9',')',#$a,'9',')',#$b,'0','=',#$b,'0',
       '-',#$c,'=','·',#$c,'=','_',#$c,'%','?',#$c,'%',
       '[',#$1a,'£','Å',#$1a,'£','{',#$1a,'/','ö',#$1a,'/',
       ']',#$1b,'Ñ','+',#$1b,'Ñ','}',#$1b,'(','*',#$1b,'(',
       ';',#$27,'ì','î',#$27,'ì',':',#$27,'"','ô',#$27,'"',
       '''',#$28,')','Ñ',#$28,')','"',#$28,'!','é',#$28,'!',
       '~',#$29,';','^',#$29,';','<',#$33,'?',';',#$33,'?',
       '>',#$34,':','/',#$35,'-','?',#$35,'_',
       'Y',#$15,'Z','y',#$15,'z','Z',#$2c,'Y','z',#$2c,'y');
  DtKbdSize=13;
  TabDtKbd: array[1..DtKbdSize*3] of char=  { Ascii to D keyboard }
      ('@',#3,'"','-',#$c,'·','_',#$c,'/','[',#$1a,'Å','{',#$1a,'ö',
       ';',#$27,'î',':',#$27,'ô','''',#$28,'Ñ','"',#$28,'é',
       '<',#$33,';','>',#$34,':','/',#$35,'-','~',#$29,'''');
  HacekSzSl=29;
  TabHacek: array[1..2*HacekSzSl] of char=      { ^ }
      ('c',#$87,'C',#$80,'d',#$83,'D',#$85,'t',#$9f,'T',#$86,
       'e',#$88,'E',#$89,'z',#$91,'Z',#$92,'o',#$93,'O',#$a7,
       'u',#$96,'U',#$a6,'r',#$a9,'R',#$9e,'l',#$8c,'L',#$9c,
       's',#$a8,'S',#$9b,'n',#$a4,'N',#$a5,diHacek,'+',
       'a','Ñ','A','é','p','î','P','ô','h','Å','H','ö');
  CarkaSzSl=21;
  TabCarka: array[1..2*CarkaSzSl] of char=              { ' }
      ('e',#$82,'E',#$90,'l',#$8d,'L',#$8a,'i',#$a1,'I',#$8b,
       'o',#$a2,'O',#$95,'u',#$a3,'U',#$97,'y',#$98,'Y',#$9d,
       'a',#$a0,'A',#$8f,'r',#$aa,'R',#$ab,diCarka,'=',
       'p',#$b5,'P',#$b6,'h',#$b7,'H',#$b8);
  UmlautSz=7;
  TabUmlaut: array[1..2*UmlautSz] of char=            { : }
      ('a','Ñ','A','é','o','î','O','ô','u','Å','U','ö',diUmlaut,'\');
  ToggleKamen:array[$41..$ab] of byte= (143,66,128,133,144,
    70,71,72,139,74,75,138,77,165,149,
    80,81,158,155,134,151,86,87,88,157,
    146,91,92,93,94,95,96,160,98,135,
    131,130,102,103,104,161,106,107,141,109,
    164,162,112,113,169,168,159,163,118,119,
    120,152,145,123,124,125,126,127,67,117,
    136,100,97,68,84,99,101,69,156,73,
    108,140,65,142,137,122,90,148,111,167,
    129,166,121,79,85,83,76,89,171,116,
    132,105,147,150,110,78,154,153,115,170,
    114,82);

procedure BreakIntHandler; assembler;
asm  push bx; push ax; push ds; mov ax,SEG @Data; mov ds,ax; cmp spec.NoCheckBreak,0; jne @1;
     mov BreakFlag,1;
@1:  pop ds; pop ax; pop bx; iret end;

procedure BreakIntrInit;
  begin GetIntVec($1b,OldBreakIntr); SetIntVec($1b,@BreakIntHandler) end;

procedure BreakIntrDone;
  begin SetIntVec($1b,OldBreakIntr) end;


procedure BreakCheck;
begin
  if BreakFlag then begin BreakFlag:=false; ClearKeyBuf; halt end;
end;

function CurrToKamen(C:char):char; assembler;
asm  mov al,C; cmp Fonts.VFont,foLatin2; jne @1; cmp al,80H; jb @1;
     sub al,80H; xor ah,ah;
     mov bx,ax; mov si,OFFSET TabLtK; mov al,ds:[si+bx];
@1:  end;

procedure ConvKamenToCurr(var Buf;L:word); assembler;
asm  mov si,OFFSET TabKtN; cmp Fonts.NoDiakrSupported,1; je @1;
     mov si,OFFSET TabKtL; cmp Fonts.VFont,foLatin2; jne @4;
@1:  mov cx,L; jcxz @4; les di,Buf; xor ah,ah; cld;
@2:  mov al,es:[di]; cmp al,80H; jb @3; sub al,80H;
     mov bx,ax; mov al,ds:[si+bx];
@3:  stosb; loop @2;
@4:  end;

procedure ConvKamenLatin(var Buf;L:word;ToLatin:boolean); {assembler;}
begin
asm  mov si,OFFSET TabKtL; cmp ToLatin,0; jne @1; mov si,OFFSET TabLtK;
@1:  mov cx,L; jcxz @4; les di,Buf; xor ah,ah; cld;
@2:  mov al,es:[di]; cmp al,80H; jb @3; sub al,80H;
     mov bx,ax; mov al,ds:[si+bx];
@3:  stosb; loop @2;
@4:  end;
end;

function ToggleCS(C:char):char; assembler;
asm  xor ah,ah; mov al,C; cmp al,$41; jb @3;
     cmp Fonts.VFont,foLatin2; jne @1; cmp al,80H; jb @1;
     sub al,80H; mov bx,ax; mov al,TabLtK[bx].byte;
@1:  cmp al,$ab; ja @2; sub al,41H; mov bx,ax; mov al,ToggleKamen[bx].byte;
@2:  cmp Fonts.VFont,foLatin2; jne @3; cmp al,80H; jb @3;
     sub al,80H; mov bx,ax; mov al,TabKtL[bx].byte;
@3:  end;

function NoDiakr(C:char):char; assembler;
asm  xor ah,ah; mov al,C; cmp al,80H; jb @2; mov bx,ax; sub bx,0080H;
     cmp Fonts.VFont,foLatin2; jne @1; mov al,TabLtN[bx].byte; jmp @2;
@1:  cmp al,0abH; ja @2; mov al,TabKtN[bx].byte;
@2:  end;
procedure ConvToNoDiakr(var Buf;L:word;FromFont:TVideoFont); assembler;
asm  les di,Buf; mov cx,L; jcxz @4; xor ah,ah; cld;
     cmp FromFont,foAscii; je @4;
@1:  mov al,es:[di]; cmp al,80H; jb @3; mov bx,ax; sub bx,0080H;
     cmp FromFont,foLatin2; jne @2; mov al,TabLtN[bx].byte; jmp @3;
@2:  mov al,TabKtN[bx].byte;
@3:  stosb; loop @1;
@4:  end;

procedure ClearKeyBuf; assembler;
asm  mov ax,0040H; mov es,ax; mov di,ofsHeadKeyBuf; mov si,ofsTailKeyBuf;
     cli; mov ax,es:[si]; mov es:[di],ax; sti; mov Diak,0; end;


procedure ClearKbdBuf; assembler;
asm  call ClearKeyBuf; mov KbdBuffer.byte,0 end;

procedure AddToKbdBuf(KeyCode:word); assembler;
asm  mov ax,KeyCode; cmp ax,0; jne @1; mov ax,0300H; jmp @2;
@1:  cmp ax,0300H; jne @2; mov ax,8400H;
@2:  xor bh,bh; mov bl,KbdBuffer.byte; cmp bx,253; ja @4;
     inc bx; mov KbdBuffer[bx].byte,al; cmp al,0; jne @3;
     inc bx; mov KbdBuffer[bx].byte,ah;
@3:  mov KbdBuffer.byte,bl;
@4:  end;

function KeyPressed:boolean; assembler;
asm  call BreakCheck; mov ah,1; int 16H; mov ax,0; je @1; mov ax,1; @1: end;

function ReadKey:word; assembler;
asm
@1:  call KeyPressed; cmp al,0; je @1;
     mov ah,0; int 16H; cmp al,0; je @2; xor ah,ah; jmp @4;
@2:  cmp ax,0300H; jne @3; xor ax,ax; jmp @4;
@3:  cmp ax,8400H; jne @4; mov ax,0300H;
@4:  end;


function ConvHCU:word; near; assembler;
asm  @1: cmp al,[si]; je @2; inc si; inc si; loop @1; jmp @3;
@2:  mov al,[si+1];
@3:  end;

procedure GetKeyEvent; assembler;
asm  call BreakCheck; cld;                      { read from buffer }
     mov si,OFFSET KbdBuffer; lodsb; xor ah,ah; mov cx,ax; jcxz @2;
     mov di,si; push ds; pop es;
     lodsb; dec cx; jcxz @0; cmp al,0; jne @0;
     mov ah,[si]; inc si; dec cx;
@0:  mov KbdBuffer.byte,cl; rep movsb;
     cmp ax,0300H; jne @1; xor ax,ax; jmp @11;
@1:  cmp ax,8400H; jne @11; mov ax,0300H;
     jmp @11;
@2:  mov ah,1; int 16H; je @13;  { key pressed ? }
     mov ah,0; int 16H;
     cmp ax,0300H; jne @3; xor ax,ax; jmp @4;
@3:  cmp ax,8400H; jne @4; mov ax,0300H;
@4:  mov si,OFFSET TabCsKbd; mov cx,CsKbdSize; cmp spec.KbdTyp,CsKbd; je @5;
     mov si,OFFSET TabCsKbd; mov cx,CaKbdSize; cmp spec.KbdTyp,CaKbd; je @5;
     mov si,OFFSET TabSlKbd; mov cx,SlKbdSize; cmp spec.KbdTyp,SlKbd; je @5;
     mov si,OFFSET TabDtKbd; mov cx,DtKbdSize; cmp spec.KbdTyp,DtKbd; jne @12;
@5:  jcxz  @8;
@6:  cmp ax,[si]                                 { Cs, Ca or Dt keyboard }
     je @7; add si,3; loop @6; jmp @8;
@7:  mov al,[si+2];
     cmp al,diUmlaut; ja @8;
     cmp al,Diak; je @8;                          { doubly pressed ^ ' }
     mov Diak,al; jmp @13;                        { extra  ^ or ' }
@8:  cmp Diak,diHacek; jne @9; mov si,OFFSET TabHacek; mov cx,HacekSzSl;
     call ConvHCU; jmp @10;
@9:  cmp Diak,diCarka; jne @91; mov si,OFFSET TabCarka; mov cx,CarkaSzSl;
     call ConvHCU; jmp @10;
@91: cmp Diak,diUmlaut; jne @10; mov si,OFFSET TabUmlaut; mov cx,UmlautSz;
     call ConvHCU;
@10: cmp al,0; je @11; xor ah,ah;                 { clear scan code }
     cmp Fonts.VFont,foLatin2; jne @11;
     mov si,OFFSET TabKtL;
     cmp al,80H; jb @11; cmp al,0b8H; ja @11; sub al,80H;
     mov bx,ax; mov al,ds:[si+bx];                { Kamen -> Latin2 }
     jmp @11;
@12: cmp al,0; je @11; xor ah,ah;
@11: mov bx,evKeyDown; mov Diak,0;
     jmp @15;
@13: xor ax,ax; mov bx,ax;                        { evNothing }
@15: mov Event.What,bx; mov Event.KeyCode,ax;
end;

function KbdTimer(Delta:word;Kind:byte):boolean;
var EndTime:longint;
label 1;
begin
  EndTime:=Timer+Delta; KbdTimer:=false;
1:case Kind of          { 0 - wait, 1 - wait or ESC, 2 - wait or any key }
    1: if KeyPressed and (ReadKey=_ESC_) then exit;
    2: if KbdPressed then begin ReadKbd; exit end end;
  if Timer<EndTime then goto 1;
  KbdTimer:=true;
end;


                           { M O U S E }


procedure ShowMouse; assembler;
asm  cmp MausExist,0; je @1; cmp MausVisible,0; jne @1;
     mov ax,1; int 33H; mov MausVisible,1; @1: end;

procedure HideMouse; assembler;
asm  cmp MausExist,0; je @1; cmp MausVisible,0; je @1;
     mov ax,2; int 33H; mov MausVisible,0; @1: end;

procedure ResetMouse; assembler;
asm  cmp MausExist,0; je @1; mov ax,2133H; int 21H; @1: end;

const Acht:byte=8;
procedure MouseEvHandler; far; assembler;
asm  mov si,SEG @DATA; mov ds,si;
     mov MouseWhereG.X,cx; mov MouseWhereG.Y,dx;
     mov si,cx; mov cl,3; shr si,cl;
     cmp IsGraphMode,0; jne @1; shr dx,cl; jmp @2;  {x/8,y/rows pixel->text}
@1:  push ax; mov ax,dx; div GrBytesPerChar; xor ah,ah;
     cmp ax,TxtRows; jb @11; mov ax,TxtRows; dec ax;
@11: mov dx,ax; pop ax;
@2:  mov MouseButtons,bl; mov MouseWhere.X,si; mov MouseWhere.Y,dx;
     test ax,11110B{left/right button pressed/released}; je @4;
     cmp EventCount,EventQSize; jae @4;
     mov ax,0040H; mov es,ax; mov ax,es:ofsTicks;
     mov di,EventQTail; push ds; pop es; cld;
     stosw; mov ax,bx; stosw; mov ax,si; stosw; mov ax,dx; stosw;
     mov ax,MouseWhereG.X; stosw; mov ax,MouseWhereG.Y; stosw;
     cmp di,OFFSET EventQLast; jne @3; mov di,OFFSET EventQueue;
@3:  mov EventQTail,di; inc EventCount;
@4:  end;

procedure InitMouseEvents; assembler;
                            {  MausVisible, MouseWhere.X/Y set before }
asm  mov MausExist,0; mov Event.What,0;
     cmp spec.NoMouseSupport,0; jne @3;
     cmp VideoCard,viEga; jb @3;   { only Ega/Vga supported }
     mov ax,3533H; int 21H; mov ax,es; or ax,bx; je @3; { int-vec 33H = nil ?}
     xor ax,ax; int 33H; or ax,ax; je @3;{ no driver}
     mov ButtonCount,bl; mov MausExist,1;
     cmp IsGraphMode,0; jne @0;         { set max. rows for text mode }
     xor cx,cx; mov ax,TxtRows; mul Acht; dec ax; mov dx,ax; mov ax,0008H; int 33H;
@0:  xor ax,ax; mov MouseButtons,al; mov LastButtons,al; mov DownButtons,al;
     mov LastDouble,al; mov EventCount,ax;
     mov ax,OFFSET ds:EventQueue; mov EventQHead,ax; mov EventQTail,ax;
     mov ax,MouseWhere.X; mov LastWhere.X,ax;
     mul Acht; mov cx,ax;
     mov ax,MouseWhere.Y; mov LastWhere.Y,ax;
     cmp IsGraphMode,0; jne @1; mul Acht; jmp @2;
@1:  mul GrBytesPerChar;
@2:  mov dx,ax;
     mov MouseWhereG.X,cx; mov LastWhereG.X,cx;
     mov MouseWhereG.Y,dx; mov LastWhereG.Y,dx;
     mov ax,4; int 33H;  { set where = (x,y) converted to pixels}
     mov cx,0ffffH{all events}; mov dx,OFFSET cs:MouseEvHandler;
     push cs; pop es; mov ax,12; int 33H;  { set event handler }
     cmp MausVisible,0; je @21;
     mov ax,1; int 33H; { show mouse } jmp @4;
@21: mov ax,2; int 33H; { hide mouse } jmp @4;
@3:  mov MausVisible,0;
@4:  end;

procedure SetMouse(X,Y:word;Visible:boolean); assembler;
asm  cmp MausExist,0; je @4; cmp IsGraphMode,0; jne @1;
     mov ax,X; dec ax; mul Acht; mov cx,ax;
     mov ax,Y; dec ax; mul Acht; mov dx,ax; jmp @2;
@1:  mov cx,X; mov dx,Y;
@2:  mov ax,4; int 33H;  { set where = (x,y) converted to pixels}
     mov al,Visible; mov MausVisible,al; xor ah,ah;
     cmp ax,0; jne @3; mov ax,2;
@3:  int 33H;
@4:  end;

procedure DoneMouseEvents; assembler;
asm  cmp MausExist,0; je @1;
     xor cx,cx; mov dx,cx; mov es,cx; mov ax,12; int 33H; { remove handler }
     mov ax,2; int 33H; { hide mouse }
@1:  end;

procedure HideMaus; assembler;
asm  cmp MausVisible,0; je @1; mov ax,2; int 33H; mov MausRefresh,1;
@1:  end;
procedure ShowMaus; assembler;
asm  cmp MausRefresh,0; je @1; mov ax,1; int 33H; mov MausRefresh,0;
@1:  end;
                  
var RedKeyName:string[8];
procedure GetRedKeyName; assembler;
asm  push ds; mov bh,colors.lFirst;
     mov ax,Event.Where.Y; mul TxtCols; shl ax,1; mov dx,ax;
     mov si,Event.Where.X; inc si; cmp si,TxtCols; jne @1; dec si;
@1:  shl si,1; add si,ax;
     mov ax,ds; mov es,ax; mov di,OFFSET RedKeyName[1];
     mov ax,ScrSeg; mov ds,ax; xor cx,cx; std;
@2:  cmp si,dx; jb @4; lodsw; cmp ah,bh; jne @2; add si,2;
@3:  cmp si,dx; jb @4; lodsw; cmp ah,bh; je @3; add si,2;
@4:  add si,2; cld;
@5:  lodsw; cmp ah,bh; jne @6; inc cx; stosb; cmp cx,8; je @6; jmp @5;
@6:  pop ds; mov RedKeyName.byte,cl;
end;

procedure GetMouseEvent; assembler;
var GX,GY: word;
asm  cmp MausExist,0; je @6;
     xor ax,ax; mov Event.From.X,ax; mov Event.From.Y,ax;
     cli; cmp EventCount,0; jne @1;   { take current time,buttons,x,y }
     xor bh,bh; mov bl,MouseButtons;
     mov cx,MouseWhereG.X; mov GX,cx; mov cx,MouseWhereG.Y; mov GY,cx;
     mov cx,MouseWhere.X; mov dx,MouseWhere.Y;
     mov di,0040H; mov es,di; mov di,ofsTicks; mov di,es:[di]; jmp @3;
@1:  mov si,EventQHead; cld;          { or first queue entry, if exists}
     lodsw; mov di,ax; lodsw; mov bx,ax; lodsw; mov cx,ax; lodsw; mov dx,ax;
     lodsw; mov GX,ax; lodsw; mov GY,ax;
     cmp si,OFFSET EventQLast; jne @2; mov si,OFFSET EventQueue;
@2:  mov EventQHead,si; dec EventCount;
@3:  sti; cmp spec.MouseReverse,0; je @4;  { reverse left/right button }
     mov bh,bl; and bh,3; je @4; cmp bh,3; je @4; xor bl,3;
@4:  mov bh,LastDouble; mov al,LastButtons;
     cmp al,bl; je @5; {buttons equal}
     or al,al; je @7; {released -> pressed}
     or bl,bl; je @9; {pressed -> released};
     mov bl,al; {2.button pressed/released or left<->right; ignore change}
@5:  or bl,bl; je @6; {no button pressed, before and now}
     cmp IsGraphMode,0; jne @51;
     cmp cx,LastWhere.X; jne @10; cmp dx,LastWhere.Y; jne @10; jmp @52;{ moved? }
@51: mov ax,GX; cmp ax,LastWhereG.X; jne @10;
     mov ax,GY; cmp ax,LastWhereG.Y; jne @10;
@52: mov ax,di; sub ax,AutoTicks; cmp ax,AutoDelay; jae @11; { time for auto? }
@6:  xor ax,ax; mov bx,ax; mov cx,ax; mov dx,ax; mov GX,ax; mov GY,ax;
     jmp @13; { evNothing }
@7:  mov bh,0; cmp bl,DownButtons; jne @8;
     cmp cx,DownWhere.X; jne @8; cmp dx,DownWhere.Y; jne @8;
     mov ax,di; sub ax,DownTicks; cmp al,spec.DoubleDelay;{??} jae @8;
     mov bh,1; { same place, same button pressed within time limit}
@8:  mov DownButtons,bl; mov DownWhere.X,cx; mov DownWhere.Y,dx;
     mov DownTicks,di; mov AutoTicks,di; xor ah,ah; mov al,spec.RepeatDelay;
     mov AutoDelay,ax;
     mov ax,evMouseDown; jmp @12;
@9:  mov ax,evMouseUp; jmp @12;
@10: mov ax,LastWhere.X; mov Event.From.X,ax;
     mov ax,LastWhere.Y; mov Event.From.Y,ax;
     mov ax,evMouseMove; jmp @12;
@11: mov AutoTicks,di; mov AutoDelay,1;  {auto after repeatdelay at each tick}
     mov ax,evMouseAuto;
@12: mov LastButtons,bl; mov LastDouble,bh;
     mov LastWhere.X,cx; mov LastWhere.Y,dx; push ax;
     mov ax,GX; mov LastWhereG.X,ax; mov ax,GY; mov LastWhereG.Y,ax; pop ax;
@13: push ds; pop es; mov di,OFFSET Event; cld;
     stosw; mov ax,bx; stosw; mov ax,cx; stosw; mov ax,dx; stosw;
     mov ax,GX; stosw; mov ax,GY; stosw;
@14: end;
procedure GetMouseKeyEvent;
var i,n,x:word;
label 1,2,3,4;
begin
  GetMouseEvent;
  if Event.What=evMouseAuto then goto 3;
  if Event.What=evMouseDown then
    if (Event.Buttons and mbRightButton)<>0 then begin Event.KeyCode:=_ESC_; goto 2 end
    else
3:    if ((Event.Buttons and mbLeftButton)<>0) and (Event.Where.Y=TxtRows-1) then begin
      if not IsGraphMode then HideMaus; GetRedKeyName;
      if not IsGraphMode then ShowMaus;
      if RedKeyName='>' then begin Event.KeyCode:=ord('>'); goto 2 end;
      if RedKeyName='' then begin Event.KeyCode:=_up_; goto 2 end;
      if RedKeyName='' then begin Event.KeyCode:=_down_; goto 2 end;
      if (RedKeyName='Ÿ') or
         (RedKeyName='Enter') then begin Event.KeyCode:=_M_; goto 2 end;
      if RedKeyName='ƒ≥' then begin Event.KeyCode:=_Tab_; goto 2 end;
      if RedKeyName='≥ƒ' then begin Event.KeyCode:=_ShiftTab_; goto 2 end;
      if RedKeyName='Esc' then begin Event.KeyCode:=_ESC_; goto 2 end;
      if RedKeyName='PgUp' then begin Event.KeyCode:=_PgUp_; goto 2 end;
      if RedKeyName='PgDn' then begin Event.KeyCode:=_PgDn_; goto 2 end;
      if RedKeyName='Ctrl' then begin LLKeyFlags:=$04; goto 4 end;
      if RedKeyName='Alt' then begin LLKeyFlags:=$08; goto 4 end;
      if RedKeyName='Shift' then begin LLKeyFlags:=$03;
4:      ClrEvent; exit end;
      if RedKeyName='CtrlHome' then begin Event.KeyCode:=_CtrlHome_; goto 2 end;
      if RedKeyName='CtrlEnd' then begin Event.KeyCode:=_CtrlEnd_; goto 2 end;
      if RedKeyName='Home' then begin Event.KeyCode:=_Home_; goto 2 end;
      if RedKeyName='End' then begin Event.KeyCode:=_End_; goto 2 end;
      if RedKeyName='CtrlY' then begin Event.KeyCode:=_Y_; goto 2 end;
      if copy(RedKeyName,1,6)='ShiftF' then begin
        delete(RedKeyName,1,6); x:=$53; goto 1 end;
      if copy(RedKeyName,1,5)='CtrlF' then begin
        delete(RedKeyName,1,5); x:=$5d; goto 1 end;
      if copy(RedKeyName,1,4)='AltF' then begin
        delete(RedKeyName,1,4); x:=$67; goto 1 end;
      if RedKeyName[1]='F' then begin
        delete(RedKeyName,1,1); x:=$3a;
1:      val(RedKeyName,n,i); if (i=0) and (n>0) and (N<=10) then begin
          Event.KeyCode:=(n+x) shl 8;
2:        Event.What:=evKeyDown end end end;
end;

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
end end;

function AddCtrlAltShift(Flgs:byte{04=Ctrl,08=Alt,03=Shift}):word; assembler;
asm  cmp Event.What,evKeyDown; jne @7; mov ax,Event.KeyCode;
     test Flgs,04H; jz @3;
     cmp ax,_Home_; jne @1; mov ax,_CtrlHome_; jmp @6;
@1:  cmp ax,_End_; jne @2; mov ax,_CtrlEnd_; jmp @6;
@2:  cmp ax,'Y'; jne @3; mov ax,_Y_; jmp @6;
@3:  cmp ax,_F1_; jb @7; cmp ax,_F10_; ja @7;
     test Flgs,04; jz @4; add ax,_CtrlF1_-_F1_; jmp @6;
@4:  test Flgs,08; jz @5; add ax,_AltF1_-_F1_; jmp @6;
@5:  test Flgs,03; jz @6; add ax,_ShiftF1_-_F1_;
@6:  mov Event.KeyCode,ax; call TestGlobalKey;
@7:  end;

function TestEvent:boolean;
label 1;
begin
1:if Event.What=0 then GetMouseKeyEvent; if Event.What=0 then GetKeyEvent;
  if Event.What=0 then TestEvent:=false
  else begin TestGlobalKey; if Event.What=0 then goto 1; TestEvent:=true end;
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
      trialStartFand:=now; trialInterval:=trialInterval div 3 ;
      if trialInterval<10 then trialInterval:=10;
      WrLLF10Msg(71) end;
    end;
  end;
{$endif}



function WaitEvent(Delta:word):word;
var t,t1,pos,l:longint; Flgs:byte; x,y:word; vis,ce:boolean;
const MoveDelay=10;
label 0,1,2;
begin
  Flgs:=KbdFlgs; vis:=false;
0:pos:=0; t:=Timer;
1:if Event.What=0 then GetMouseKeyEvent; if Event.What=0 then GetKeyEvent;
  if Event.What<>0 then begin WaitEvent:=0; goto 2 end;
  if Flgs<>KbdFlgs then begin WaitEvent:=1; goto 2 end;
  if (Delta<>0) and (Timer>t+Delta) then begin WaitEvent:=2; goto 2 end;
  if pos<>0 then begin if Timer>t1+MoveDelay then begin
     ScrWrStr(x,y,'       ',7);
     x:=Random(TxtCols-8); y:=Random(TxtRows-1);
     ScrWrStr(x,y,'PC FAND',7); t1:=Timer end end
  else if (spec.ScreenDelay>0) and (Timer>t+spec.ScreenDelay) then begin
    if IsGraphMode then l:=$08010 else l:=TxtCols*TxtRows*2+50;
    if StoreAvail<l then t:=Timer else begin
      ce:=Crs.Enabled; CrsHide; pos:=PushW1(1,1,TxtCols,TxtRows,true,true);
      TextAttr:=0; ClrScr; vis:=MausVisible; HideMouse; l:=RandSeed;
      t1:=Timer-MoveDelay end end;
  goto 1;
2:if pos<>0 then begin RandSeed:=l; if vis then ShowMouse; PopW(pos);
    if ce then CrsShow;
    if Event.What<>0 then begin Event.What:=0; goto 0 end end;
  TestGlobalKey;
{$ifdef Trial} TestTrial;
{$endif}
end;


procedure GetEvent;
  begin repeat WaitEvent(0) until Event.What<>0 end;
procedure ClrEvent;
  begin Event.What:=0 end;

function KbdPressed:boolean;
begin
  KbdPressed:=true;
  if length(KbdBuffer)>0 then exit; if KeyPressed then exit;
  Event.What:=0; GetMouseKeyEvent; if Event.What=evKeyDown then begin
    AddToKbdBuf(Event.KeyCode); ClrEvent; exit end;
  KbdPressed:=false;
end;

function ESCPressed:boolean;
begin
  ESCPressed:=true;
  if KeyPressed then begin
    if ReadKey=_ESC_ then exit end
  else begin
    GetMouseKeyEvent; if Event.What=evKeyDown then begin
      if Event.KeyCode=_ESC_ then begin ClrEvent; exit end end;
    ClrEvent end;
  ESCPressed:=false;
end;

function ReadKbd:word;
begin
  while Event.What<>evKeyDown do begin
    ClrEvent; GetEvent end;
  ReadKbd:=Event.KeyCode; KbdChar:=Event.KeyCode; ClrEvent;
end;


               {  O T H E R S  }

procedure Delay(N:word); assembler;
var oldH,oldL,boundH,boundL:word;
asm  mov ax,0040H; mov es,ax; mov di,OfsTicks; push ds;
     lds ax,es:[di]; mov dx,ds; mov oldH,dx; mov oldL,ax;
     add ax,N; adc dx,0; mov boundH,dx; mov boundL,ax;
@1:  lds ax,es:[di]; mov dx,ds;
     cmp dx,boundH; ja @3; jb @2;
     cmp ax,boundL; jnb @3;
@2:  cmp dx,oldH; jb @3; ja @1;
     cmp ax,oldL; jnb @1;
@3:  pop ds;
end;

procedure Sound(N:word); assembler;
asm  mov bx,N; mov ax,34ddH; mov dx,0012H; cmp dx,bx; jnc @2;
     div bx; mov bx,ax; in al,61H; test al,3; jnz @1; or al,3;
     out 61H,al; mov al,0b6H; out 43H,al;
@1:  mov al,bl; out 42H,al; mov al,bh; out 42H,al;
@2:  end;

procedure NoSound; assembler;
asm  in al,61H; and al,0fcH; out 61H,al end;
